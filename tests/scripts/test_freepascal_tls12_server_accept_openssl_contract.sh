#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_server_accept_XXXXXX)"
PORT=18497
SERVER_LOG="$WORK_DIR/server.log"
CLIENT_LOG="$WORK_DIR/client.log"
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

echo "[TEST] pure Pascal TLS1.2 server accept OpenSSL contract"

require_cmd openssl
require_cmd fpc
require_cmd rg

cat > "$WORK_DIR/server_probe.pas" <<'PAS'
program server_probe;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$DEFINE USE_CTHREADS}{$ENDIF}

uses
  {$IFDEF USE_CTHREADS}
  CThreads,
  {$ENDIF}
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.connection.builder,
  fafafa.examples.tcp;

var
  NetErr: string;
  Ctx: ISSLContext;
  ListenSock, ClientSock: TSocketHandle;
  Conn: ISSLConnection;
begin
  if not InitNetwork(NetErr) then
  begin
    WriteLn('NET_FAIL ', NetErr);
    Halt(1);
  end;

  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS12
    .WithVerifyNone
    .WithCertificate('tests/certificate/test_certs/signer_cert.pem')
    .WithPrivateKey('tests/certificate/test_certs/signer_key.pem')
    .BuildServer;

  ListenSock := ListenTCP(18497, '0.0.0.0');
  try
    WriteLn('READY');
    ClientSock := AcceptConnection(ListenSock);
    try
      Conn := TSSLConnectionBuilder.Create
        .WithContext(Ctx)
        .WithSocket(THandle(ClientSock))
        .WithTimeout(5000)
        .BuildServer;
      WriteLn('ACCEPT_OK PROTO=', Ord(Conn.GetProtocolVersion), ' CIPHER=', Conn.GetCipherName);
      Conn.Shutdown;
    finally
      CloseSocket(ClientSock);
    end;
  finally
    CloseSocket(ListenSock);
    CleanupNetwork;
  end;
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/server_probe.pas" \
    -o"$WORK_DIR/server_probe" >/dev/null
)

(
  cd "$ROOT_DIR"
  "$WORK_DIR/server_probe" >"$SERVER_LOG" 2>&1
) &
SERVER_PID=$!

for _ in {1..40}; do
  if rg -F --quiet -- "READY" "$SERVER_LOG"; then
    break
  fi
  sleep 0.1
done

printf 'GET / HTTP/1.0\r\n\r\n' | \
  timeout 10 openssl s_client \
    -connect 127.0.0.1:$PORT \
    -tls1_2 \
    -cipher 'ECDHE-RSA-CHACHA20-POLY1305' \
    -quiet >"$CLIENT_LOG" 2>&1 || true

wait "$SERVER_PID" || true

rg -F --quiet -- "ACCEPT_OK" "$SERVER_LOG" || {
  echo "--- server log ---"
  cat "$SERVER_LOG" || true
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  fail "pure Pascal TLS1.2 server accept should succeed against local OpenSSL client"
}

echo "[PASS] pure Pascal TLS1.2 server accept OpenSSL contract passed"
