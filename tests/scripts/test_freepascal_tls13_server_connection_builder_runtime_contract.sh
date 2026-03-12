#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls13_server_runtime_XXXXXX)"
PORT=18496
SERVER_LOG="$WORK_DIR/server.log"
CLIENT_LOG="$WORK_DIR/client.log"
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

echo "[TEST] pure Pascal TLS1.3 server connection-builder runtime contract"

require_cmd openssl
require_cmd fpc
require_cmd rg

cat > "$WORK_DIR/server_probe.pas" <<'PAS'
program server_probe;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$DEFINE USE_CTHREADS}{$ENDIF}

uses
  {$IFDEF USE_CTHREADS}
  CThreads,
  {$ENDIF}
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.connection.builder,
  fafafa.examples.tcp;

var
  NetErr: string;
  Ctx: ISSLContext;
  ListenSock, ClientSock: TSocketHandle;
  Conn: ISSLConnection;
  ReqBuf: array[0..1023] of Byte;
  N: Integer;
  Resp: RawByteString;
begin
  if not InitNetwork(NetErr) then
  begin
    WriteLn('NET_FAIL ', NetErr);
    Halt(1);
  end;

  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS13
    .WithVerifyNone
    .WithCertificate('tests/certificate/test_certs/signer_cert.pem')
    .WithPrivateKey('tests/certificate/test_certs/signer_key.pem')
    .WithALPN('h2,http/1.1')
    .BuildServer;

  ListenSock := ListenTCP(18496, '0.0.0.0');
  try
    WriteLn('READY');
    ClientSock := AcceptConnection(ListenSock);
    try
      Conn := TSSLConnectionBuilder.Create
        .WithContext(Ctx)
        .WithSocket(THandle(ClientSock))
        .WithTimeout(5000)
        .BuildServer;

      N := Conn.Read(ReqBuf[0], SizeOf(ReqBuf));
      Resp := 'HTTP/1.0 200 OK'#13#10'Content-Length: 2'#13#10#13#10'OK';
      Conn.Write(Resp[1], Length(Resp));
      WriteLn(
        'ACCEPT_OK PROTO=', Ord(Conn.GetProtocolVersion),
        ' CIPHER=', Conn.GetCipherName,
        ' ALPN=', Conn.GetSelectedALPNProtocol,
        ' READ=', N
      );
      Conn.Shutdown;
    finally
      CloseSocket(ClientSock);
    end;
  finally
    CloseSocket(ListenSock);
    CleanupNetwork;
  end;
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/server_probe.pas" \
    -o"$WORK_DIR/server_probe" >/dev/null
)

(
  cd "$ROOT_DIR"
  "$WORK_DIR/server_probe" >"$SERVER_LOG" 2>&1
) &
SERVER_PID=$!

for _ in {1..40}; do
  if rg -F --quiet -- "READY" "$SERVER_LOG"; then
    break
  fi
  sleep 0.1
done

printf 'GET / HTTP/1.0\r\n\r\n' | \
  timeout 10 openssl s_client \
    -connect 127.0.0.1:$PORT \
    -tls1_3 \
    -alpn h2 \
    -quiet >"$CLIENT_LOG" 2>&1 || true

wait "$SERVER_PID" || {
  echo "--- server log ---"
  cat "$SERVER_LOG" || true
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  fail "pure Pascal TLS1.3 server runtime probe should succeed"
}

rg -F --quiet -- "ACCEPT_OK" "$SERVER_LOG" || {
  cat "$SERVER_LOG"
  fail "server runtime probe should report ACCEPT_OK"
}

rg -F --quiet -- "ALPN=h2" "$SERVER_LOG" || {
  cat "$SERVER_LOG"
  fail "server runtime probe should negotiate h2 via ALPN"
}

rg -F --quiet -- "HTTP/1.0 200 OK" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "OpenSSL client should receive HTTP response from pure Pascal server"
}

echo "[PASS] pure Pascal TLS1.3 server connection-builder runtime contract passed"
