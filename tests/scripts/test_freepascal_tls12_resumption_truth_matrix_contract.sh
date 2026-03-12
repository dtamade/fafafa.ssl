#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_tls12_resumption_matrix_XXXXXX)"
BASE_PORT=18490
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

echo "[TEST] pure Pascal TLS1.2 resumption truth matrix contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls12_resumption_matrix_probe.pas" <<'PAS'
program fp_tls12_resumption_matrix_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.examples.tcp;

function ConnectOnce(APort: Integer; const ACipher: string; ASessionIn: ISSLSession;
  out ASessionOut: ISSLSession; out AReused: Boolean; out AError: string): Boolean;
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

  Sock := ConnectTCP('127.0.0.1', APort);
  try
    Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    Ctx.SetProtocolVersions([sslProtocolTLS12]);
    Ctx.SetPreferredVersion(sslProtocolTLS12);
    Ctx.SetVerifyMode([]);
    Ctx.SetCipherList(ACipher);

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
    Result := True;
  finally
    CloseSocket(Sock);
  end;
end;

procedure RunCase(APort: Integer; const ACipher, ALabel: string);
var
  Session1, Session2: ISSLSession;
  Reused1, Reused2: Boolean;
  Err: string;
begin
  if not ConnectOnce(APort, ACipher, nil, Session1, Reused1, Err) then
  begin
    WriteLn(ALabel, '_FIRST_FAIL ', Err);
    Halt(1);
  end;
  if Session1 = nil then
  begin
    WriteLn(ALabel, '_FIRST_SESSION_NIL');
    Halt(2);
  end;

  if not ConnectOnce(APort, ACipher, Session1, Session2, Reused2, Err) then
  begin
    WriteLn(ALabel, '_SECOND_FAIL ', Err);
    Halt(3);
  end;
  if Session2 = nil then
  begin
    WriteLn(ALabel, '_SECOND_SESSION_NIL');
    Halt(4);
  end;

  WriteLn(
    ALabel,
    '_OK FIRST_RESUMABLE=', BoolToStr(Session1.IsResumable, True),
    ' FIRST_REUSED=', BoolToStr(Reused1, True),
    ' SECOND_RESUMABLE=', BoolToStr(Session2.IsResumable, True),
    ' SECOND_REUSED=', BoolToStr(Reused2, True)
  );
end;

begin
  if ParamCount <> 3 then
  begin
    WriteLn('usage: <port> <cipher> <label>');
    Halt(64);
  end;
  RunCase(StrToInt(ParamStr(1)), ParamStr(2), ParamStr(3));
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls12_resumption_matrix_probe.pas" \
    -o"$WORK_DIR/fp_tls12_resumption_matrix_probe" >/dev/null
)

run_case() {
  local port="$1"
  local cipher="$2"
  local label="$3"
  local server_log="$WORK_DIR/${label}.server.log"
  local client_log="$WORK_DIR/${label}.client.log"

  openssl s_server -quiet -www \
    -accept "$port" \
    -tls1_2 \
    -no_ticket \
    -cipher "$cipher" \
    -cert "$ROOT_DIR/tests/certificate/test_certs/signer_cert.pem" \
    -key "$ROOT_DIR/tests/certificate/test_certs/signer_key.pem" \
    >"$server_log" 2>&1 &
  SERVER_PID=$!
  sleep 1

  (
    cd "$ROOT_DIR"
    "$WORK_DIR/fp_tls12_resumption_matrix_probe" "$port" "$cipher" "$label" >"$client_log" 2>&1
  ) || {
    echo "--- $label client log ---"
    cat "$client_log" || true
    echo "--- $label server log ---"
    tail -n 120 "$server_log" || true
    fail "$label should succeed"
  }

  kill "$SERVER_PID" >/dev/null 2>&1 || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""

  rg -F --quiet -- "${label}_OK FIRST_RESUMABLE=True FIRST_REUSED=False SECOND_RESUMABLE=True SECOND_REUSED=True" "$client_log" || {
    cat "$client_log"
    fail "$label should expose resumable first handshake and reused second handshake"
  }
}

run_case $((BASE_PORT + 0)) 'ECDHE-RSA-CHACHA20-POLY1305' 'chacha'
run_case $((BASE_PORT + 1)) 'ECDHE-RSA-AES128-GCM-SHA256' 'aes128gcm'

echo "[PASS] pure Pascal TLS1.2 resumption truth matrix contract passed"
