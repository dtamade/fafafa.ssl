#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_builder_XXXXXX)"
PORT=18470
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

echo "[TEST] pure Pascal TLS1.2 builder/connector/stream contract"

require_cmd openssl
require_cmd fpc

openssl req -new -nodes -newkey rsa:2048 \
  -keyout "$WORK_DIR/server.key" \
  -out "$WORK_DIR/server.csr" \
  -subj "/C=CN/ST=Beijing/L=Beijing/O=Test Org/CN=localhost" \
  -addext "subjectAltName=DNS:localhost" >/dev/null 2>&1

cat > "$WORK_DIR/ext.cnf" <<'EOF'
subjectAltName=DNS:localhost
extendedKeyUsage=serverAuth
keyUsage=digitalSignature,keyEncipherment
basicConstraints=CA:FALSE
EOF

openssl x509 -req \
  -in "$WORK_DIR/server.csr" \
  -CA "$ROOT_DIR/tests/certificate/test_certs/ca_cert.pem" \
  -CAkey "$ROOT_DIR/tests/certificate/test_certs/ca_key.pem" \
  -CAcreateserial \
  -days 1 \
  -sha256 \
  -extfile "$WORK_DIR/ext.cnf" \
  -out "$WORK_DIR/server.crt" >/dev/null 2>&1

cat > "$WORK_DIR/fp_tls12_builder_probe.pas" <<'PAS'
program fp_tls12_builder_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.tls,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  Sock: TSocketHandle;
  Req: RawByteString;
  Buf: array[0..4095] of Byte;
  N: Integer;
  Resp: RawByteString;
begin
  Sock := ConnectTCP('127.0.0.1', 18470);
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS12
    .WithVerifyPeer
    .WithCAFile('tests/certificate/test_certs/ca_cert.pem')
    .BuildClient;

  TLS := TSSLConnector.FromContext(Ctx).WithTimeout(5000);
  Stream := TLS.ConnectSocket(THandle(Sock), 'localhost');
  try
    Req := 'GET / HTTP/1.0'#13#10#13#10;
    Stream.Write(Req[1], Length(Req));
    N := Stream.Read(Buf[0], SizeOf(Buf));
    if N <= 0 then
    begin
      WriteLn('READ_FAIL');
      Halt(1);
    end;
    SetString(Resp, PAnsiChar(@Buf[0]), N);
    WriteLn(
      'STREAM_OK PROTO=', Ord(Stream.Connection.GetProtocolVersion),
      ' VERIFY=', Stream.Connection.GetVerifyResult,
      ' DETAIL=', Stream.Connection.GetVerifyResultString,
      ' PREFIX=', Copy(string(Resp), 1, 16)
    );
  finally
    Stream.Free;
  end;
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_builder_probe.pas" \
    -o"$WORK_DIR/fp_tls12_builder_probe" >/dev/null
)

openssl s_server -quiet -www \
  -accept "$PORT" \
  -tls1_2 \
  -no_ticket \
  -curves X25519 \
  -cipher 'ECDHE-RSA-CHACHA20-POLY1305' \
  -cert "$WORK_DIR/server.crt" \
  -key "$WORK_DIR/server.key" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

sleep 1

(
  cd "$ROOT_DIR"
  "$WORK_DIR/fp_tls12_builder_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal TLS1.2 builder/connector/stream probe should succeed"
}

rg -F --quiet -- "STREAM_OK" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "builder/connector/stream probe should report STREAM_OK"
}

rg -F --quiet -- "VERIFY=0 DETAIL=Verification passed" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "builder/connector/stream probe should report successful verification"
}

echo "[PASS] pure Pascal TLS1.2 builder/connector/stream contract passed"
