#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_p256_matrix_XXXXXX)"
BASE_PORT=18500
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

echo "[TEST] pure Pascal TLS1.2 local P-256 matrix contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_p256_matrix_probe.pas" <<'PAS'
program fp_tls12_p256_matrix_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.examples.tcp;

var
  Port: Integer;
  CipherName: string;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
begin
  if ParamCount <> 2 then
  begin
    WriteLn('usage: <port> <cipher>');
    Halt(64);
  end;
  Port := StrToInt(ParamStr(1));
  CipherName := ParamStr(2);

  Sock := ConnectTCP('127.0.0.1', Port);
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Ctx.SetProtocolVersions([sslProtocolTLS12]);
  Ctx.SetPreferredVersion(sslProtocolTLS12);
  Ctx.SetVerifyMode([]);
  Ctx.SetCipherList(CipherName);

  Conn := Ctx.CreateConnection(THandle(Sock));
  if Supports(Conn, ISSLClientConnection, Client) then
    Client.SetServerName('localhost');

  if not Conn.Connect then
  begin
    WriteLn('CONNECT_FAIL ERR=', Ord(Conn.GetError(-1)), ' DETAIL=', Conn.GetVerifyResultString);
    Halt(1);
  end;

  WriteLn('P256_MATRIX_OK CIPHER=', Conn.GetCipherName);
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_p256_matrix_probe.pas" \
    -o"$WORK_DIR/fp_tls12_p256_matrix_probe" >/dev/null
)

run_case() {
  local port="$1"
  local cipher="$2"
  local sigalgs="$3"
  local label="$4"
  local server_log="$WORK_DIR/${label}.server.log"
  local client_log="$WORK_DIR/${label}.client.log"

  openssl s_server -quiet -www \
    -accept "$port" \
    -tls1_2 \
    -no_ticket \
    -curves P-256 \
    -cipher "$cipher" \
    -sigalgs "$sigalgs" \
    -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
    -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
    >"$server_log" 2>&1 &
  SERVER_PID=$!
  sleep 1

  (
    cd "$ROOT_DIR"
    "$WORK_DIR/fp_tls12_p256_matrix_probe" "$port" "$cipher" >"$client_log" 2>&1
  ) || {
    echo "--- $label client log ---"
    cat "$client_log" || true
    echo "--- $label server log ---"
    tail -n 120 "$server_log" || true
    fail "$label should succeed"
  }

  kill "$SERVER_PID" >/dev/null 2>&1 || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""

  rg -F --quiet -- "P256_MATRIX_OK" "$client_log" || {
    cat "$client_log"
    fail "$label should report P256_MATRIX_OK"
  }
}

run_case $((BASE_PORT + 0)) 'ECDHE-RSA-CHACHA20-POLY1305' 'rsa_pss_rsae_sha256' 'p256_chacha_pss'
run_case $((BASE_PORT + 1)) 'ECDHE-RSA-CHACHA20-POLY1305' 'rsa_pkcs1_sha512' 'p256_chacha_pkcs1sha512'
run_case $((BASE_PORT + 2)) 'ECDHE-RSA-AES128-GCM-SHA256' 'rsa_pss_rsae_sha256' 'p256_aes128_pss'
run_case $((BASE_PORT + 3)) 'ECDHE-RSA-AES256-GCM-SHA384' 'rsa_pss_rsae_sha256' 'p256_aes256_pss'

echo "[PASS] pure Pascal TLS1.2 local P-256 matrix contract passed"
