#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_resumption_interop_XXXXXX)"
PORT=18492
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

echo "[TEST] pure Pascal TLS1.2 resumption OpenSSL interop contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_resumption_interop_probe.pas" <<'PAS'
program fp_tls12_resumption_interop_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
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

  Sock := ConnectTCP('127.0.0.1', 18492);
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
  Session1, Session2: ISSLSession;
  Reused1, Reused2: Boolean;
  Err: string;
begin
  if not ConnectOnce(nil, Session1, Reused1, Err) then
  begin
    WriteLn('FIRST_FAIL ', Err);
    Halt(1);
  end;
  WriteLn('FIRST_OK reused=', BoolToStr(Reused1, True),
    ' resumable=', BoolToStr((Session1 <> nil) and Session1.IsResumable, True));

  if not ConnectOnce(Session1, Session2, Reused2, Err) then
  begin
    WriteLn('SECOND_FAIL ', Err);
    Halt(2);
  end;
  WriteLn('SECOND_OK reused=', BoolToStr(Reused2, True),
    ' resumable=', BoolToStr((Session2 <> nil) and Session2.IsResumable, True));
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_resumption_interop_probe.pas" \
    -o"$WORK_DIR/fp_tls12_resumption_interop_probe" >/dev/null
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
  "$WORK_DIR/fp_tls12_resumption_interop_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal/OpenSSL TLS1.2 resumption probe should succeed"
}

rg -F --quiet -- "FIRST_OK reused=False resumable=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "first TLS1.2 handshake should produce resumable session"
}

rg -F --quiet -- "SECOND_OK reused=True resumable=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "second TLS1.2 handshake should reuse session against OpenSSL s_server"
}

echo "[PASS] pure Pascal TLS1.2 resumption OpenSSL interop contract passed"
