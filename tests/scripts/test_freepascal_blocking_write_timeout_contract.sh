#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_write_timeout_XXXXXX)"
PORT=18458
SERVER_LOG="$WORK_DIR/server.log"
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

echo "[TEST] pure Pascal blocking write timeout contract"

require_cmd python3
require_cmd fpc
require_cmd timeout

cat > "$WORK_DIR/idle_tls_server.py" <<'PY'
import socket
import ssl
import sys
import time

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
lsock.listen(1)

conn, _ = lsock.accept()
conn.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 1024)
with ctx.wrap_socket(conn, server_side=True) as tls:
    time.sleep(1.0)
    end_time = time.time() + 4.0
    while time.time() < end_time:
        try:
            data = tls.recv(4096)
            if not data:
                break
        except OSError:
            break
lsock.close()
PY

cat > "$WORK_DIR/fp_blocking_write_timeout_probe.pas" <<'PAS'
program fp_blocking_write_timeout_probe;
{$mode ObjFPC}{$H+}

uses
  SysUtils,
  BaseUnix,
  Sockets,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
  SendBufSize: LongInt;
  Payload: RawByteString;
  Ret, FillAttempt, RetryAttempt: Integer;
begin
  Sock := ConnectTCP('127.0.0.1', 18458);
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

  SendBufSize := 1024;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDBUF, @SendBufSize, SizeOf(SendBufSize));

  SetLength(Payload, 16384);
  FillChar(Payload[1], Length(Payload), Ord('A'));
  Conn.SetBlocking(True);
  Conn.SetTimeout(200);

  for FillAttempt := 1 to 8192 do
  begin
    Ret := Conn.Write(Payload[1], Length(Payload));
    if Ret <> Length(Payload) then
      Break;
  end;

  if Ret = Length(Payload) then
  begin
    WriteLn('NO_TIMEOUT_REACHED');
    Flush(Output);
    Halt(3);
  end;

  WriteLn(
    'BLOCKED err=', Ord(Conn.GetError(-1)),
    ' IS_TIMEOUT=', BoolToStr(Conn.GetError(-1) = sslErrTimeout, True),
    ' WANTWRITE=', BoolToStr(Conn.WantWrite, True),
    ' DETAIL=', Conn.GetVerifyResultString
  );
  Flush(Output);

  Halt(0);
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_blocking_write_timeout_probe.pas" -o"$WORK_DIR/fp_blocking_write_timeout_probe" >/dev/null
)

python3 "$WORK_DIR/idle_tls_server.py" \
  "$PORT" \
  "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!
sleep 0.2

(
  cd "$ROOT_DIR"
  timeout 20 "$WORK_DIR/fp_blocking_write_timeout_probe" >"$PROBE_LOG" 2>&1
) || true

if ! rg -F --quiet -- "IS_TIMEOUT=True" "$PROBE_LOG" || \
   ! rg -F --quiet -- "WANTWRITE=False" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "blocking write timeout should surface sslErrTimeout instead of WantWrite"
fi

echo "[PASS] pure Pascal blocking write timeout contract passed"
