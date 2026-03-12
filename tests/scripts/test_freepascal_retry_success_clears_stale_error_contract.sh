#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_clear_stale_XXXXXX)"
BACKEND_PORT=18455
PROXY_PORT=18454
SERVER_LOG="$WORK_DIR/s_server.log"
PROXY_LOG="$WORK_DIR/proxy.log"
PROBE_LOG="$WORK_DIR/probe.log"
SERVER_PID=""
PROXY_PID=""

cleanup() {
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    sleep 0.2
    if kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      kill -9 "$SERVER_PID" >/dev/null 2>&1 || true
    fi
  fi
  if [[ -n "$PROXY_PID" ]] && kill -0 "$PROXY_PID" >/dev/null 2>&1; then
    kill "$PROXY_PID" >/dev/null 2>&1 || true
    sleep 0.2
    if kill -0 "$PROXY_PID" >/dev/null 2>&1; then
      kill -9 "$PROXY_PID" >/dev/null 2>&1 || true
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

echo "[TEST] pure Pascal retry success clears stale error contract"

require_cmd openssl
require_cmd python3
require_cmd fpc
require_cmd timeout

cat > "$WORK_DIR/fp_clear_stale_proxy.py" <<'PY'
import socket
import sys
import threading
import time

listen_port = int(sys.argv[1])
target_port = int(sys.argv[2])

listen_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
listen_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listen_sock.bind(("127.0.0.1", listen_port))
listen_sock.listen(1)

client_sock, _ = listen_sock.accept()
server_sock = socket.create_connection(("127.0.0.1", target_port))


def client_to_server():
    total = 0
    try:
        while True:
            chunk = client_sock.recv(256)
            if not chunk:
                try:
                    server_sock.shutdown(socket.SHUT_WR)
                except OSError:
                    pass
                break

            total += len(chunk)
            server_sock.sendall(chunk)
            if total > 8192:
                time.sleep(0.05)
    except OSError:
        pass


def server_to_client():
    try:
        while True:
            chunk = server_sock.recv(4096)
            if not chunk:
                try:
                    client_sock.shutdown(socket.SHUT_WR)
                except OSError:
                    pass
                break
            client_sock.sendall(chunk)
    except OSError:
        pass


threading.Thread(target=client_to_server, daemon=True).start()
threading.Thread(target=server_to_client, daemon=True).start()
time.sleep(30)
PY

cat > "$WORK_DIR/fp_clear_stale_probe.pas" <<'PAS'
program fp_clear_stale_probe;
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
  Sock := ConnectTCP('127.0.0.1', 18454);
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

  SendBufSize := 4096;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDBUF, @SendBufSize, SizeOf(SendBufSize));

  SetLength(Payload, 16384);
  FillChar(Payload[1], Length(Payload), Ord('A'));
  Conn.SetBlocking(False);

  for FillAttempt := 1 to 256 do
  begin
    Ret := Conn.Write(Payload[1], Length(Payload));
    if Ret <> Length(Payload) then
      Break;
  end;

  WriteLn(
    'BLOCKED err=', Ord(Conn.GetError(-1)),
    ' wantwrite=', BoolToStr(Conn.WantWrite, True),
    ' detail=', Conn.GetVerifyResultString
  );

  for RetryAttempt := 1 to 80 do
  begin
    Ret := Conn.Write(Payload[1], Length(Payload));
    if Ret = Length(Payload) then
    begin
      WriteLn(
        'POST_SUCCESS err=', Ord(Conn.GetError(Ret)),
        ' wantwrite=', BoolToStr(Conn.WantWrite, True),
        ' detail=', Conn.GetVerifyResultString
      );
      Halt(0);
    end;
    Sleep(50);
  end;

  WriteLn('POST_SUCCESS_MISSING');
  Halt(2);
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_clear_stale_probe.pas" \
    -o"$WORK_DIR/fp_clear_stale_probe" >/dev/null
)

openssl s_server -quiet \
  -accept "$BACKEND_PORT" \
  -tls1_3 \
  -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  -cert_chain "$ROOT_DIR/tests/certificate/test_certs/ca_cert.pem" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

python3 "$WORK_DIR/fp_clear_stale_proxy.py" "$PROXY_PORT" "$BACKEND_PORT" \
  >"$PROXY_LOG" 2>&1 &
PROXY_PID=$!
sleep 0.5

(
  cd "$ROOT_DIR"
  timeout 12 "$WORK_DIR/fp_clear_stale_probe" >"$PROBE_LOG" 2>&1
) || true

if ! rg -F --quiet -- "POST_SUCCESS err=0 wantwrite=False detail=Verification disabled" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "successful retry should clear stale error/detail state"
fi

echo "[PASS] pure Pascal retry success clears stale error contract passed"
