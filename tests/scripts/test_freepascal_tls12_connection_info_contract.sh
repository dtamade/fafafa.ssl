#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_info_XXXXXX)"
PORT=18466
SERVER_LOG="$WORK_DIR/s_server.log"
CLIENT_LOG="$WORK_DIR/fp_client.log"
SERVER_PID=""

cleanup() {
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    sleep 0.2
    if kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      kill -9 "$SERVER_PID" >/dev/null 2>&1 || true
    fi
  fi
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "$cmd" >/dev/null 2>&1 || fail "missing required command: $cmd"
}

echo "[TEST] pure Pascal TLS1.2 connection-info contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_info_probe.pas" <<'PAS'
program fp_tls12_info_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
  Info: TSSLConnectionInfo;
begin
  Sock := ConnectTCP('127.0.0.1', 18466);
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Ctx.SetProtocolVersions([sslProtocolTLS12]);
  Ctx.SetPreferredVersion(sslProtocolTLS12);
  Ctx.SetVerifyMode([]);
  Ctx.SetCipherList('ECDHE-RSA-CHACHA20-POLY1305');

  Conn := Ctx.CreateConnection(THandle(Sock));
  if Supports(Conn, ISSLClientConnection, Client) then
    Client.SetServerName('localhost');

  if not Conn.Connect then
  begin
    WriteLn('CONNECT_FAIL ', Conn.GetVerifyResultString);
    Halt(1);
  end;

  Info := Conn.GetConnectionInfo;
  WriteLn(
    'PROTO=', Ord(Info.ProtocolVersion),
    ' SUITE=', Info.CipherSuite,
    ' SUITEID=', Info.CipherSuiteId,
    ' KEX=', Ord(Info.KeyExchange),
    ' CIPHER=', Ord(Info.Cipher),
    ' HASH=', Ord(Info.Hash),
    ' KEYSIZE=', Info.KeySize,
    ' MACSIZE=', Info.MacSize
  );
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_info_probe.pas" \
    -o"$WORK_DIR/fp_tls12_info_probe" >/dev/null
)

openssl s_server -quiet -www \
  -accept "$PORT" \
  -tls1_2 \
  -no_ticket \
  -curves X25519 \
  -cipher 'ECDHE-RSA-CHACHA20-POLY1305' \
  -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

sleep 1

(
  cd "$ROOT_DIR"
  "$WORK_DIR/fp_tls12_info_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "TLS1.2 connection-info probe should succeed"
}

rg -F --quiet -- "SUITE=TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "connection info should report the negotiated TLS1.2 cipher suite"
}

rg -F --quiet -- "KEYSIZE=32 MACSIZE=16" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "connection info should report TLS1.2 ChaCha20/Poly1305 sizes"
}

echo "[PASS] pure Pascal TLS1.2 connection-info contract passed"
