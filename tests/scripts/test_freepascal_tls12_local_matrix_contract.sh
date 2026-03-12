#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_matrix_XXXXXX)"
BASE_PORT=18480
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

echo "[TEST] pure Pascal TLS1.2 local matrix contract"

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

cat > "$WORK_DIR/fp_tls12_matrix_probe.pas" <<'PAS'
program fp_tls12_matrix_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.tls,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

var
  Port: Integer;
  CipherName: string;
  VerifyModeArg: string;
  Builder: ISSLContextBuilder;
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  Sock: TSocketHandle;
  Req: RawByteString;
  Buf: array[0..4095] of Byte;
  N: Integer;
  Resp: RawByteString;
begin
  if ParamCount <> 3 then
  begin
    WriteLn('usage: <port> <cipher> <verify-mode>');
    Halt(64);
  end;

  Port := StrToInt(ParamStr(1));
  CipherName := ParamStr(2);
  VerifyModeArg := LowerCase(ParamStr(3));

  Builder := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS12
    .WithCipherList(CipherName);

  if VerifyModeArg = 'peer' then
    Builder := Builder
      .WithVerifyPeer
      .WithCAFile('tests/certificate/test_certs/ca_cert.pem')
  else
    Builder := Builder.WithVerifyNone;

  Ctx := Builder.BuildClient;
  TLS := TSSLConnector.FromContext(Ctx).WithTimeout(5000);

  Sock := ConnectTCP('127.0.0.1', Port);
  Stream := TLS.ConnectSocket(THandle(Sock), 'localhost');
  try
    Req := 'GET / HTTP/1.0'#13#10#13#10;
    Stream.Write(Req[1], Length(Req));
    N := Stream.Read(Buf[0], SizeOf(Buf));
    if N <= 0 then
    begin
      WriteLn('READ_FAIL');
      Halt(2);
    end;
    SetString(Resp, PAnsiChar(@Buf[0]), N);
    WriteLn(
      'MATRIX_OK CIPHER=', Stream.Connection.GetCipherName,
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
    "$WORK_DIR/fp_tls12_matrix_probe.pas" \
    -o"$WORK_DIR/fp_tls12_matrix_probe" >/dev/null
)

run_case() {
  local port="$1"
  local cipher="$2"
  local verify_mode="$3"
  local label="$4"
  local server_log="$WORK_DIR/${label}.server.log"
  local client_log="$WORK_DIR/${label}.client.log"

  openssl s_server -quiet -www \
    -accept "$port" \
    -tls1_2 \
    -no_ticket \
    -cipher "$cipher" \
    -cert "$WORK_DIR/server.crt" \
    -key "$WORK_DIR/server.key" \
    >"$server_log" 2>&1 &
  SERVER_PID=$!
  sleep 1

  set +e
  (
    cd "$ROOT_DIR"
    "$WORK_DIR/fp_tls12_matrix_probe" "$port" "$cipher" "$verify_mode" >"$client_log" 2>&1
  )
  local status=$?
  set -e

  kill "$SERVER_PID" >/dev/null 2>&1 || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""

  if [[ "$status" -ne 0 ]]; then
    echo "--- $label client log ---"
    cat "$client_log" || true
    echo "--- $label server log ---"
    tail -n 120 "$server_log" || true
    fail "$label should succeed"
  fi

  if [[ "$verify_mode" == "peer" ]]; then
    rg -F --quiet -- "VERIFY=0 DETAIL=Verification passed" "$client_log" || {
      cat "$client_log"
      fail "$label should report successful verification"
    }
  else
    rg -F --quiet -- "DETAIL=Verification disabled" "$client_log" || {
      cat "$client_log"
      fail "$label should report verification disabled"
    }
  fi

  rg -F --quiet -- "MATRIX_OK" "$client_log" || {
    cat "$client_log"
    fail "$label should report MATRIX_OK"
  }
}

run_case $((BASE_PORT + 0)) 'ECDHE-RSA-CHACHA20-POLY1305' 'none' 'chacha_verify_none'
run_case $((BASE_PORT + 1)) 'ECDHE-RSA-CHACHA20-POLY1305' 'peer' 'chacha_verify_peer'
run_case $((BASE_PORT + 2)) 'ECDHE-RSA-AES128-GCM-SHA256' 'none' 'aes_verify_none'
run_case $((BASE_PORT + 3)) 'ECDHE-RSA-AES128-GCM-SHA256' 'peer' 'aes_verify_peer'

echo "[PASS] pure Pascal TLS1.2 local matrix contract passed"
