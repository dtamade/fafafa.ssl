#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_builder_resume_XXXXXX)"
PORT=18493
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

echo "[TEST] pure Pascal TLS1.2 builder/connector resumption contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_builder_resume_probe.pas" <<'PAS'
program fp_tls12_builder_resume_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.tls,
  fafafa.examples.tcp;

function ConnectOnce(ASessionIn: ISSLSession; out ASessionOut: ISSLSession;
  out AReused: Boolean): Boolean;
var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  Sock: TSocketHandle;
  Req: RawByteString;
  Buf: array[0..4095] of Byte;
  N: Integer;
begin
  Result := False;
  ASessionOut := nil;
  AReused := False;

  Sock := ConnectTCP('127.0.0.1', 18493);
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS12
    .WithVerifyNone
    .WithCipherList('ECDHE-RSA-CHACHA20-POLY1305')
    .BuildClient;

  TLS := TSSLConnector.FromContext(Ctx).WithTimeout(5000);
  if ASessionIn <> nil then
    TLS := TLS.WithSession(ASessionIn);

  Stream := TLS.ConnectSocket(THandle(Sock), 'localhost');
  try
    Req := 'GET / HTTP/1.0'#13#10#13#10;
    Stream.Write(Req[1], Length(Req));
    N := Stream.Read(Buf[0], SizeOf(Buf));
    if N <= 0 then
      Halt(11);
    ASessionOut := Stream.Connection.GetSession;
    AReused := Stream.Connection.IsSessionReused;
    Result := True;
  finally
    Stream.Free;
  end;
end;

var
  Session1, Session2: ISSLSession;
  Reused1, Reused2: Boolean;
begin
  if not ConnectOnce(nil, Session1, Reused1) then
  begin
    WriteLn('FIRST_FAIL');
    Halt(1);
  end;
  WriteLn('FIRST_OK REUSED=', BoolToStr(Reused1, True),
    ' RESUMABLE=', BoolToStr((Session1 <> nil) and Session1.IsResumable, True));

  if not ConnectOnce(Session1, Session2, Reused2) then
  begin
    WriteLn('SECOND_FAIL');
    Halt(2);
  end;
  WriteLn('SECOND_OK REUSED=', BoolToStr(Reused2, True),
    ' RESUMABLE=', BoolToStr((Session2 <> nil) and Session2.IsResumable, True));
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_builder_resume_probe.pas" \
    -o"$WORK_DIR/fp_tls12_builder_resume_probe" >/dev/null
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
  "$WORK_DIR/fp_tls12_builder_resume_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal TLS1.2 builder/connector resumption probe should succeed"
}

rg -F --quiet -- "FIRST_OK REUSED=False RESUMABLE=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "first builder/connector TLS1.2 handshake should produce resumable session"
}

rg -F --quiet -- "SECOND_OK REUSED=True RESUMABLE=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "second builder/connector TLS1.2 handshake should reuse session"
}

echo "[PASS] pure Pascal TLS1.2 builder/connector resumption contract passed"
