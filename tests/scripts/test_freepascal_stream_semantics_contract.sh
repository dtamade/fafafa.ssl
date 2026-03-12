#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_stream_semantics_XXXXXX)"
PORT=18445
SERVER_LOG="$WORK_DIR/s_server.log"
EOF_LOG="$WORK_DIR/eof_probe.log"
WANT_LOG="$WORK_DIR/want_probe.log"
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

echo "[TEST] pure Pascal stream semantics contract"

require_cmd openssl
require_cmd fpc
require_cmd timeout

cat > "$WORK_DIR/fp_close_notify_probe.pas" <<'PAS'
program fp_close_notify_probe;
{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.tls,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  Sock: TSocketHandle;
  Req: RawByteString;
  Buf: array[0..1023] of Byte;
  N, Total: Integer;
begin
  Sock := ConnectTCP('127.0.0.1', 18445);
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  Ctx.SetPreferredVersion(sslProtocolTLS13);
  Ctx.SetVerifyMode([]);
  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(THandle(Sock), 'localhost');
  Req := 'GET / HTTP/1.0'#13#10#13#10;
  Stream.Write(Req[1], Length(Req));

  Total := 0;
  try
    repeat
      N := Stream.Read(Buf[0], SizeOf(Buf));
      if N > 0 then
        Inc(Total, N);
    until N = 0;
    WriteLn('EOF_OK total=', Total);
  except
    on E: Exception do
      WriteLn('EXC ', E.ClassName, ': ', E.Message);
  end;

  Stream.Free;
end.
PAS

cat > "$WORK_DIR/fp_wantread_probe.pas" <<'PAS'
program fp_wantread_probe;
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
  Buf: array[0..255] of Byte;
  N: Integer;
begin
  Sock := ConnectTCP('127.0.0.1', 18445);
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

  Conn.SetBlocking(False);
  N := Conn.Read(Buf[0], SizeOf(Buf));
  WriteLn(
    'READ_RET=', N,
    ' ERR=', Ord(Conn.GetError(N)),
    ' WANTREAD=', BoolToStr(Conn.WantRead, True),
    ' WANTWRITE=', BoolToStr(Conn.WantWrite, True),
    ' DETAIL=', Conn.GetVerifyResultString
  );
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_close_notify_probe.pas" -o"$WORK_DIR/fp_close_notify_probe" >/dev/null
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_wantread_probe.pas" -o"$WORK_DIR/fp_wantread_probe" >/dev/null
)

openssl s_server -quiet -www \
  -accept "$PORT" \
  -tls1_3 \
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
  "$WORK_DIR/fp_close_notify_probe" >"$EOF_LOG" 2>&1
) || true

(
  cd "$ROOT_DIR"
  timeout 5 "$WORK_DIR/fp_wantread_probe" >"$WANT_LOG" 2>&1
) || true

if ! rg -F --quiet -- "EOF_OK total=" "$EOF_LOG"; then
  echo "--- eof probe ---"
  cat "$EOF_LOG" || true
  fail "close_notify/EOF probe should finish with EOF_OK"
fi

if ! rg -F --quiet -- "READ_RET=-1" "$WANT_LOG" || \
   ! rg -F --quiet -- "WANTREAD=True" "$WANT_LOG" || \
   ! rg -F --quiet -- "WANTWRITE=False" "$WANT_LOG"; then
  echo "--- wantread probe ---"
  cat "$WANT_LOG" || true
  fail "nonblocking read should surface WANTREAD semantics"
fi

echo "[PASS] pure Pascal stream semantics contract passed"
