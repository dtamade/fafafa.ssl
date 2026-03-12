#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_shutdown_notify_XXXXXX)"
PORT=18446
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

echo "[TEST] pure Pascal shutdown close_notify contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_shutdown_close_notify_probe.pas" <<'PAS'
program fp_shutdown_close_notify_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Sock: TSocketHandle;
  Req: RawByteString;
  Buf: array[0..1023] of Byte;
  N: Integer;
begin
  Sock := ConnectTCP('127.0.0.1', 18446);
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Ctx.SetPreferredVersion(sslProtocolTLS13);
  Ctx.SetVerifyMode([]);
  Conn := Ctx.CreateConnection(THandle(Sock));
  if not Supports(Conn, ISSLClientConnection, ClientConn) then
  begin
    WriteLn('NO_CLIENT_IFACE');
    Halt(1);
  end;
  ClientConn.SetServerName('localhost');
  if not Conn.Connect then
  begin
    WriteLn('CONNECT_FAIL ', Conn.GetVerifyResultString);
    Halt(2);
  end;

  Req := 'GET / HTTP/1.0'#13#10#13#10;
  Conn.Write(Req[1], Length(Req));
  N := Conn.Read(Buf[0], SizeOf(Buf));
  if N <= 0 then
  begin
    WriteLn('READ_FAIL ', Conn.GetVerifyResultString);
    Halt(4);
  end;

  if not Conn.Shutdown then
  begin
    WriteLn('SHUTDOWN_FAIL ', Conn.GetVerifyResultString);
    Halt(3);
  end;

  Sleep(200);
  CloseSocket(Sock);
  WriteLn('SHUTDOWN_OK');
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_shutdown_close_notify_probe.pas" \
    -o"$WORK_DIR/fp_shutdown_close_notify_probe" >/dev/null
)

openssl s_server -accept "$PORT" -tls1_3 -www -state -msg \
  -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  -cert_chain "$ROOT_DIR/tests/certificate/test_certs/ca_cert.pem" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

for _ in {1..30}; do
  if (echo >"/dev/tcp/127.0.0.1/$PORT") >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

(
  cd "$ROOT_DIR"
  "$WORK_DIR/fp_shutdown_close_notify_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 160 "$SERVER_LOG" || true
  fail "shutdown probe should complete successfully"
}

rg -F --quiet -- "SHUTDOWN_OK" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "shutdown probe should report SHUTDOWN_OK"
}

if ! rg -F --quiet -- "close notify" "$SERVER_LOG"; then
  echo "--- server log ---"
  tail -n 200 "$SERVER_LOG" || true
  fail "OpenSSL server should observe close_notify from pure Pascal client"
fi

if ! rg -F --quiet -- "alert read:warning:close notify" "$SERVER_LOG" && \
   ! rg -F --quiet -- "<<< TLS 1.3, Alert [length 0002], warning close_notify" "$SERVER_LOG"; then
  echo "--- server log ---"
  tail -n 200 "$SERVER_LOG" || true
  fail "server should observe incoming client close_notify, not only its own outgoing alert"
fi

echo "[PASS] pure Pascal shutdown close_notify contract passed"
