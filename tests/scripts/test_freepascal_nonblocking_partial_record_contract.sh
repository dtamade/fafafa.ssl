#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_partial_record_XXXXXX)"
BACKEND_PORT=18447
PROXY_PORT=18446
SERVER_LOG="$WORK_DIR/s_server.log"
PROXY_LOG="$WORK_DIR/proxy.log"
PROBE_LOG="$WORK_DIR/probe.log"
SERVER_PID=""
PROXY_PID=""

cleanup() {
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    sleep 0.2
    if kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      kill -9 "$SERVER_PID" >/dev/null 2>&1 || true
    fi
  fi
  if [[ -n "$PROXY_PID" ]] && kill -0 "$PROXY_PID" >/dev/null 2>&1; then
    kill "$PROXY_PID" >/dev/null 2>&1 || true
    sleep 0.2
    if kill -0 "$PROXY_PID" >/dev/null 2>&1; then
      kill -9 "$PROXY_PID" >/dev/null 2>&1 || true
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

echo "[TEST] pure Pascal nonblocking partial-record contract"

require_cmd openssl
require_cmd python3
require_cmd fpc
require_cmd timeout

cat > "$WORK_DIR/fp_partial_record_proxy.py" <<'PY'
import socket
import sys
import threading
import time

listen_port = int(sys.argv[1])
target_port = int(sys.argv[2])

listen_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
listen_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listen_sock.bind(("127.0.0.1", listen_port))
listen_sock.listen(1)

client_sock, _ = listen_sock.accept()
server_sock = socket.create_connection(("127.0.0.1", target_port))


def pump(src, dst, split=False):
    try:
        while True:
            data = src.recv(65536)
            if not data:
                try:
                    dst.shutdown(socket.SHUT_WR)
                except OSError:
                    pass
                break

            if split and len(data) > 1:
                cut = 3 if len(data) > 3 else 1
                dst.sendall(data[:cut])
                time.sleep(0.35)
                dst.sendall(data[cut:])
            else:
                dst.sendall(data)
    except OSError:
        pass
    finally:
        try:
            dst.close()
        except OSError:
            pass
        try:
            src.close()
        except OSError:
            pass


client_to_server = threading.Thread(target=pump, args=(client_sock, server_sock, False), daemon=True)
server_to_client = threading.Thread(target=pump, args=(server_sock, client_sock, True), daemon=True)
client_to_server.start()
server_to_client.start()
client_to_server.join()
server_to_client.join()
listen_sock.close()
PY

cat > "$WORK_DIR/fp_partial_record_probe.pas" <<'PAS'
program fp_partial_record_probe;
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
  Req: RawByteString;
  Buf: array[0..255] of Byte;
  N, Attempt: Integer;
  Chunk: AnsiString;
begin
  Sock := ConnectTCP('127.0.0.1', 18446);
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

  Req := 'GET / HTTP/1.0'#13#10#13#10;
  if Conn.Write(Req[1], Length(Req)) <> Length(Req) then
  begin
    WriteLn('WRITE_FAIL err=', Ord(Conn.GetError(-1)), ' detail=', Conn.GetVerifyResultString);
    Halt(1);
  end;

  Conn.SetBlocking(False);
  for Attempt := 1 to 8 do
  begin
    N := Conn.Read(Buf[0], SizeOf(Buf));
    WriteLn(
      'READ', Attempt,
      ' n=', N,
      ' err=', Ord(Conn.GetError(N)),
      ' wantread=', BoolToStr(Conn.WantRead, True),
      ' wantwrite=', BoolToStr(Conn.WantWrite, True),
      ' detail=', Conn.GetVerifyResultString
    );

    if N > 0 then
    begin
      SetString(Chunk, PAnsiChar(@Buf[0]), N);
      WriteLn('PAYLOAD_OK=', Copy(Chunk, 1, 32));
      Halt(0);
    end;

    Sleep(300);
  end;

  WriteLn('PAYLOAD_MISSING');
  Halt(2);
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_partial_record_probe.pas" -o"$WORK_DIR/fp_partial_record_probe" >/dev/null
)

openssl s_server -quiet -www \
  -accept "$BACKEND_PORT" \
  -tls1_3 \
  -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
  -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
  -cert_chain "$ROOT_DIR/tests/certificate/test_certs/ca_cert.pem" \
  >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

for _ in {1..30}; do
  if (echo >"/dev/tcp/127.0.0.1/$BACKEND_PORT") >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

python3 "$WORK_DIR/fp_partial_record_proxy.py" "$PROXY_PORT" "$BACKEND_PORT" \
  >"$PROXY_LOG" 2>&1 &
PROXY_PID=$!
sleep 0.2

(
  cd "$ROOT_DIR"
  timeout 8 "$WORK_DIR/fp_partial_record_probe" >"$PROBE_LOG" 2>&1
) || true

if ! rg -F --quiet -- "READ1 n=-1" "$PROBE_LOG" || \
   ! rg -F --quiet -- "wantread=True" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "first nonblocking read should surface WANTREAD on partial record"
fi

if ! rg -F --quiet -- "PAYLOAD_OK=HTTP/1.0" "$PROBE_LOG"; then
  echo "--- probe log ---"
  cat "$PROBE_LOG" || true
  fail "nonblocking continuation should eventually return decrypted payload"
fi

echo "[PASS] pure Pascal nonblocking partial-record contract passed"
