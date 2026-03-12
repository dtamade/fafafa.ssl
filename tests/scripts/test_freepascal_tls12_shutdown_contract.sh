#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_shutdown_XXXXXX)"
PORT=18469
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

echo "[TEST] pure Pascal TLS1.2 shutdown contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_shutdown_probe.pas" <<'PAS'
program fp_tls12_shutdown_probe;

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
begin
  Sock := ConnectTCP('127.0.0.1', 18469);
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

  if not Conn.Shutdown then
  begin
    WriteLn('SHUTDOWN_FAIL ERR=', Ord(Conn.GetError(-1)), ' DETAIL=', Conn.GetVerifyResultString);
    Halt(2);
  end;

  WriteLn('SHUTDOWN_OK');
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_shutdown_probe.pas" \
    -o"$WORK_DIR/fp_tls12_shutdown_probe" >/dev/null
)

openssl s_server \
  -accept "$PORT" \
  -tls1_2 \
  -no_ticket \
  -curves X25519 \
  -cipher 'ECDHE-RSA-CHACHA20-POLY1305' \
  -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  -msg -state -quiet \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

sleep 1

set +e
(
  cd "$ROOT_DIR"
  "$WORK_DIR/fp_tls12_shutdown_probe" >"$CLIENT_LOG" 2>&1
)
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal TLS1.2 shutdown probe should succeed"
fi

rg -F --quiet -- "SHUTDOWN_OK" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "shutdown probe should report SHUTDOWN_OK"
}

if ! rg -F --quiet -- "alert read:warning:close notify" "$SERVER_LOG" && \
   ! rg -F --quiet -- "Alert [length 0002], warning close_notify" "$SERVER_LOG"; then
  tail -n 120 "$SERVER_LOG" || true
  fail "OpenSSL server log should observe client close_notify"
fi

echo "[PASS] pure Pascal TLS1.2 shutdown contract passed"
