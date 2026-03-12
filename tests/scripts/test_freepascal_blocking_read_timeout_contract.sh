#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_read_timeout_XXXXXX)"
PORT=18456
SERVER_LOG="$WORK_DIR/s_server.log"
PROBE_LOG="$WORK_DIR/probe.log"
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

echo "[TEST] pure Pascal blocking read timeout contract"

require_cmd openssl
require_cmd python3
require_cmd fpc
require_cmd timeout

cat > "$WORK_DIR/fp_blocking_read_timeout_probe.pas" <<'PAS'
program fp_blocking_read_timeout_probe;
{$mode ObjFPC}{$H+}

uses
  SysUtils,
  DateUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
  Buf: array[0..255] of Byte;
  Ret: Integer;
  StartedAt, ElapsedMs: Int64;
begin
  Sock := ConnectTCP('127.0.0.1', 18456);
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Ctx.SetPreferredVersion(sslProtocolTLS13);
  Ctx.SetVerifyMode([]);
  Conn := Ctx.CreateConnection(THandle(Sock));
  if Supports(Conn, ISSLClientConnection, Client) then
    Client.SetServerName('localhost');

  if not Conn.Connect then
  begin
    WriteLn('CONNECT_FAIL ', Conn.GetVerifyResultString);
    Halt(1);
  end;

  Conn.SetBlocking(True);
  Conn.SetTimeout(200);

  StartedAt := GetTickCount64;
  Ret := Conn.Read(Buf[0], SizeOf(Buf));
  ElapsedMs := GetTickCount64 - StartedAt;

  WriteLn(
    'READ_RET=', Ret,
    ' ERR=', Ord(Conn.GetError(Ret)),
    ' IS_TIMEOUT=', BoolToStr(Conn.GetError(Ret) = sslErrTimeout, True),
    ' WANTREAD=', BoolToStr(Conn.WantRead, True),
    ' WANTWRITE=', BoolToStr(Conn.WantWrite, True),
    ' ELAPSED_MS=', ElapsedMs,
    ' DETAIL=', Conn.GetVerifyResultString
  );
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_blocking_read_timeout_probe.pas" -o"$WORK_DIR/fp_blocking_read_timeout_probe" >/dev/null
)

cat > "$WORK_DIR/idle_tls_server.py" <<'PY'
import socket
import ssl
import time
import sys

port = int(sys.argv[1])
cert = sys.argv[2]
key = sys.argv[3]

ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ctx.minimum_version = ssl.TLSVersion.TLSv1_3
ctx.maximum_version = ssl.TLSVersion.TLSv1_3
ctx.load_cert_chain(certfile=cert, keyfile=key)

lsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
lsock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
lsock.bind(("127.0.0.1", port))
lsock.listen(5)

conn, _ = lsock.accept()
with ctx.wrap_socket(conn, server_side=True) as tls:
    time.sleep(2.0)
lsock.close()
PY

python3 "$WORK_DIR/idle_tls_server.py" \
  "$PORT" \
  "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

sleep 0.3

(
  cd "$ROOT_DIR"
  timeout 5 "$WORK_DIR/fp_blocking_read_timeout_probe" >"$PROBE_LOG" 2>&1
) || true

if ! rg -F --quiet -- "READ_RET=-1" "$PROBE_LOG" || \
   ! rg -F --quiet -- "IS_TIMEOUT=True" "$PROBE_LOG" || \
   ! rg -F --quiet -- "WANTREAD=False" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "blocking timed read should surface sslErrTimeout instead of WantRead"
fi

if ! rg -i --quiet -- "DETAIL=.*(timeout|timed out)" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "timeout detail should mention timeout explicitly"
fi

echo "[PASS] pure Pascal blocking read timeout contract passed"
