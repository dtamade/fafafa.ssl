#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_session_XXXXXX)"
PORT=18473
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

echo "[TEST] pure Pascal TLS1.2 session surface contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_session_probe.pas" <<'PAS'
program fp_tls12_session_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.session,
  fafafa.examples.tcp;

function ConnectOnce(ASessionIn: ISSLSession; out ASessionOut: ISSLSession;
  out AReused: Boolean; out AError: string): Boolean;
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocketHandle;
  Req: RawByteString;
  Buf: array[0..4095] of Byte;
  N: Integer;
begin
  Result := False;
  ASessionOut := nil;
  AReused := False;
  AError := '';

  Sock := ConnectTCP('127.0.0.1', 18473);
  try
    Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    Ctx.SetProtocolVersions([sslProtocolTLS12]);
    Ctx.SetPreferredVersion(sslProtocolTLS12);
    Ctx.SetVerifyMode([]);
    Ctx.SetCipherList('ECDHE-RSA-CHACHA20-POLY1305');

    Conn := Ctx.CreateConnection(THandle(Sock));
    if Supports(Conn, ISSLClientConnection, Client) then
      Client.SetServerName('localhost');

    if ASessionIn <> nil then
      Conn.SetSession(ASessionIn);

    if not Conn.Connect then
    begin
      AError := Conn.GetVerifyResultString;
      Exit;
    end;

    Req := 'GET / HTTP/1.0'#13#10#13#10;
    if Length(Req) > 0 then
      Conn.Write(Req[1], Length(Req));
    N := Conn.Read(Buf[0], SizeOf(Buf));
    if N <= 0 then
    begin
      AError := 'read failed: ' + Conn.GetVerifyResultString;
      Exit;
    end;

    ASessionOut := Conn.GetSession;
    AReused := Conn.IsSessionReused;
    Conn.Close;
    Result := True;
  finally
    CloseSocket(Sock);
  end;
end;

var
  Session1, Session2, Clone: ISSLSession;
  Resumption1: IFreePascalResumptionSession;
  Reused1, Reused2: Boolean;
  Err: string;
begin
  if not ConnectOnce(nil, Session1, Reused1, Err) then
  begin
    WriteLn('FIRST_FAIL ', Err);
    Halt(1);
  end;
  if Session1 = nil then
  begin
    WriteLn('FIRST_SESSION_NIL');
    Halt(2);
  end;
  Clone := Session1.Clone;
  if not Supports(Session1, IFreePascalResumptionSession, Resumption1) then
  begin
    WriteLn('FIRST_NO_INTERNAL_SESSION');
    Halt(5);
  end;
  WriteLn(
    'FIRST_OK PROTO=', Ord(Session1.GetProtocolVersion),
    ' CIPHER=', Session1.GetCipherName,
    ' SUITE=', Resumption1.GetCipherSuite,
    ' RESUMABLE=', BoolToStr(Session1.IsResumable, True),
    ' REUSED=', BoolToStr(Reused1, True),
    ' CLONE_ID_MATCH=', BoolToStr((Clone <> nil) and (Clone.GetID = Session1.GetID), True)
  );

  if not ConnectOnce(Session1, Session2, Reused2, Err) then
  begin
    WriteLn('SECOND_FAIL ', Err);
    Halt(3);
  end;
  if Session2 = nil then
  begin
    WriteLn('SECOND_SESSION_NIL');
    Halt(4);
  end;
  WriteLn(
    'SECOND_OK PROTO=', Ord(Session2.GetProtocolVersion),
    ' RESUMABLE=', BoolToStr(Session2.IsResumable, True),
    ' REUSED=', BoolToStr(Reused2, True)
  );
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_session_probe.pas" \
    -o"$WORK_DIR/fp_tls12_session_probe" >/dev/null
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
  "$WORK_DIR/fp_tls12_session_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal TLS1.2 session surface probe should succeed"
}

rg -F --quiet -- "FIRST_OK PROTO=5" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "first TLS1.2 session should report protocol version TLS1.2"
}

rg -F --quiet -- "RESUMABLE=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "TLS1.2 session should now be resumable"
}

rg -F --quiet -- "SUITE=52392" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "TLS1.2 session snapshot should preserve CHACHA suite id"
}

rg -F --quiet -- "SECOND_OK PROTO=5 RESUMABLE=True REUSED=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "TLS1.2 SetSession path should now reuse the session"
}

rg -F --quiet -- "CLONE_ID_MATCH=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "TLS1.2 session clone should preserve session ID"
}

echo "[PASS] pure Pascal TLS1.2 session surface contract passed"
