#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_info_timeout_XXXXXX)"
PORT=18461
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

echo "[TEST] pure Pascal info callback timeout contract"

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

cat > "$WORK_DIR/fp_info_timeout_probe.pas" <<'PAS'
program fp_info_timeout_probe;
{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.examples.tcp;

type
  TInfoProbe = class
  private
    FStates: array of string;
  public
    procedure HandleInfo(const AWhere: Integer; const ARet: Integer; const AState: string);
    function SawState(const AState: string): Boolean;
    function EventCount: Integer;
  end;

procedure TInfoProbe.HandleInfo(const AWhere: Integer; const ARet: Integer; const AState: string);
var
  LIndex: Integer;
begin
  LIndex := Length(FStates);
  SetLength(FStates, LIndex + 1);
  FStates[LIndex] := AState;
end;

function TInfoProbe.SawState(const AState: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FStates) do
    if SameText(FStates[I], AState) then
      Exit(True);
end;

function TInfoProbe.EventCount: Integer;
begin
  Result := Length(FStates);
end;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
  Probe: TInfoProbe;
begin
  Sock := ConnectTCP('127.0.0.1', 18461);
  Probe := TInfoProbe.Create;
  try
    Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    Ctx.SetPreferredVersion(sslProtocolTLS13);
    Ctx.SetVerifyMode([]);
    Ctx.SetInfoCallback(@Probe.HandleInfo);
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
      'SAW_START=', BoolToStr(Probe.SawState('handshake_start'), True),
      ' SAW_TIMEOUT=', BoolToStr(Probe.SawState('timeout'), True),
      ' SAW_HANDSHAKE_FAILED=', BoolToStr(Probe.SawState('handshake_failed'), True),
      ' EVENTS=', Probe.EventCount,
      ' DETAIL=', Conn.GetVerifyResultString
    );
  finally
    Probe.Free;
  end;
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_info_timeout_probe.pas" -o"$WORK_DIR/fp_info_timeout_probe" >/dev/null
)

python3 "$WORK_DIR/idle_tcp_server.py" "$PORT" >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!
sleep 0.2

(
  cd "$ROOT_DIR"
  timeout 5 "$WORK_DIR/fp_info_timeout_probe" >"$PROBE_LOG" 2>&1
) || true

if ! rg -F --quiet -- "SAW_START=True" "$PROBE_LOG" || \
   ! rg -F --quiet -- "SAW_TIMEOUT=True" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "handshake timeout should emit handshake_start and timeout info states"
fi

echo "[PASS] pure Pascal info callback timeout contract passed"
