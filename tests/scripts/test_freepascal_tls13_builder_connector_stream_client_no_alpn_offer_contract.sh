#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls13_client_no_alpn_XXXXXX)"
PORT=18490
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

echo "[TEST] pure Pascal TLS1.3 client-no-offer ALPN contract"

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

cat > "$WORK_DIR/fp_tls13_client_no_alpn_offer_probe.pas" <<'PAS'
program fp_tls13_client_no_alpn_offer_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.tls,
  fafafa.examples.tcp;

function ShowALPN(const AValue: string): string;
begin
  if AValue = '' then
    Result := '<empty>'
  else
    Result := AValue;
end;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  Sock: TSocketHandle;
  Info: TSSLConnectionInfo;
begin
  Sock := ConnectTCP('127.0.0.1', 18490);
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS13
    .WithVerifyPeer
    .WithCAFile('tests/certificate/test_certs/ca_cert.pem')
    .BuildClient;

  TLS := TSSLConnector.FromContext(Ctx).WithTimeout(5000);
  Stream := TLS.ConnectSocket(THandle(Sock), 'localhost');
  try
    Info := Stream.Connection.GetConnectionInfo;
    WriteLn(
      'STREAM_OK PROTO=', ProtocolVersionToString(Stream.Connection.GetProtocolVersion),
      ' VERIFY=', Stream.Connection.GetVerifyResult,
      ' ALPN=', ShowALPN(Stream.Connection.GetSelectedALPNProtocol),
      ' INFO_ALPN=', ShowALPN(Info.ALPNProtocol)
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
    "$WORK_DIR/fp_tls13_client_no_alpn_offer_probe.pas" \
    -o"$WORK_DIR/fp_tls13_client_no_alpn_offer_probe" >/dev/null
)

openssl s_server -quiet \
  -accept "$PORT" \
  -tls1_3 \
  -alpn h2 \
  -cert "$WORK_DIR/server.crt" \
  -key "$WORK_DIR/server.key" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

sleep 1

(
  cd "$ROOT_DIR"
  "$WORK_DIR/fp_tls13_client_no_alpn_offer_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal TLS1.3 client-no-offer ALPN probe should succeed"
}

rg -F --quiet -- "STREAM_OK" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "TLS1.3 client-no-offer probe should report STREAM_OK"
}

rg -F --quiet -- "ALPN=<empty> INFO_ALPN=<empty>" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "TLS1.3 client-no-offer probe should expose empty negotiated ALPN on both APIs"
}

echo "[PASS] pure Pascal TLS1.3 client-no-offer ALPN contract passed"
