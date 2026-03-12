#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_hs_state_XXXXXX)"
PORT=18460
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

echo "[TEST] pure Pascal handshake-failed state contract"

require_cmd python3
require_cmd fpc
require_cmd timeout

cat > "$WORK_DIR/idle_tcp_server.py" <<'PY'
import socket
import sys
import time

port = int(sys.argv[1])
lsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
lsock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
lsock.bind(("127.0.0.1", port))
lsock.listen(1)
conn, _ = lsock.accept()
time.sleep(2.0)
conn.close()
lsock.close()
PY

cat > "$WORK_DIR/fp_handshake_failed_state_probe.pas" <<'PAS'
program fp_handshake_failed_state_probe;
{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
begin
  Sock := ConnectTCP('127.0.0.1', 18460);
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Ctx.SetPreferredVersion(sslProtocolTLS13);
  Ctx.SetVerifyMode([]);
  Conn := Ctx.CreateConnection(THandle(Sock));
  if Supports(Conn, ISSLClientConnection, Client) then
    Client.SetServerName('localhost');
  Conn.SetBlocking(True);
  Conn.SetTimeout(200);

  if Conn.Connect then
  begin
    WriteLn('UNEXPECTED_CONNECT_SUCCESS');
    Halt(1);
  end;

  WriteLn(
    'STATE=', Conn.GetState,
    ' STATESTR=', Conn.GetStateString,
    ' ERR=', Ord(Conn.GetError(-1)),
    ' DETAIL=', Conn.GetVerifyResultString
  );
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_handshake_failed_state_probe.pas" -o"$WORK_DIR/fp_handshake_failed_state_probe" >/dev/null
)

python3 "$WORK_DIR/idle_tcp_server.py" "$PORT" >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!
sleep 0.2

(
  cd "$ROOT_DIR"
  timeout 5 "$WORK_DIR/fp_handshake_failed_state_probe" >"$PROBE_LOG" 2>&1
) || true

if ! rg -F --quiet -- "STATE=HANDSHAKE_FAILED" "$PROBE_LOG" || \
   ! rg -F --quiet -- "STATESTR=Handshake failed" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "failed handshake should expose handshake-failed state instead of disconnected"
fi

echo "[PASS] pure Pascal handshake-failed state contract passed"
