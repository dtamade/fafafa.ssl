#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d -t fafafa_fp_resumption_interop_XXXXXX)"
PORT=18443
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

echo "[TEST] pure Pascal TLS13 resumption OpenSSL interop contract"

require_cmd openssl
require_cmd fpc

cat > "$WORK_DIR/fp_tls13_resumption_interop_probe.pas" <<'PAS'
program fp_tls13_resumption_interop_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.examples.tcp;

function CreateTrustedStore: ISSLCertificateStore;
var
  LCert: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  if Result = nil then
    raise Exception.Create('trust store = nil');
  LCert := TSSLFactory.CreateCertificate(sslFreePascal);
  if (LCert = nil) or (not LCert.LoadFromFile('tests/certificate/test_certs/ca_cert.pem')) then
    raise Exception.Create('failed to load trusted CA');
  if not Result.AddCertificate(LCert) then
    raise Exception.Create('failed to add trusted CA');
end;

function ConnectOnce(ASessionIn: ISSLSession; out ASessionOut: ISSLSession;
  out AReused: Boolean; out AError: string): Boolean;
var
  LCtx: ISSLContext;
  LSock: TSocketHandle;
  LConn: ISSLConnection;
  LClient: ISSLClientConnection;
  LReq: RawByteString;
  LBuf: array[0..4095] of Byte;
  LRead: Integer;
  LAttempt: Integer;
begin
  Result := False;
  ASessionOut := nil;
  AReused := False;
  AError := '';

  LSock := ConnectTCP('127.0.0.1', 18443);
  try
    LCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    LCtx.SetPreferredVersion(sslProtocolTLS13);
    LCtx.SetCertificateStore(CreateTrustedStore);
    LCtx.SetCertVerifyFlags([sslCertVerifyDefault, sslCertVerifyIgnoreHostname]);

    LConn := LCtx.CreateConnection(THandle(LSock));
    if LConn = nil then
    begin
      AError := 'connection=nil';
      Exit;
    end;

    if not Supports(LConn, ISSLClientConnection, LClient) then
    begin
      AError := 'client interface missing';
      Exit;
    end;
    LClient.SetServerName('localhost');

    if ASessionIn <> nil then
      LConn.SetSession(ASessionIn);

    if not LConn.Connect then
    begin
      AError := 'connect failed: ' + LConn.GetVerifyResultString;
      Exit;
    end;

    AReused := LConn.IsSessionReused;

    LReq := 'GET / HTTP/1.0'#13#10#13#10;
    if Length(LReq) > 0 then
      LConn.Write(LReq[1], Length(LReq));

    for LAttempt := 1 to 8 do
    begin
      LRead := LConn.Read(LBuf[0], SizeOf(LBuf));
      if LRead < 0 then
      begin
        AError := 'read failed: ' + LConn.GetVerifyResultString;
        Exit;
      end;
      ASessionOut := LConn.GetSession;
      if (ASessionOut <> nil) and ASessionOut.IsResumable then
        Break;
      if LRead = 0 then
        Break;
    end;

    if ASessionOut = nil then
      ASessionOut := LConn.GetSession;
    Result := True;
  finally
    CloseSocket(LSock);
  end;
end;

var
  LSession1, LSession2: ISSLSession;
  LReused1, LReused2: Boolean;
  LErr: string;
begin
  if not ConnectOnce(nil, LSession1, LReused1, LErr) then
  begin
    WriteLn('FIRST_FAIL ', LErr);
    Halt(1);
  end;
  WriteLn('FIRST_OK reused=', BoolToStr(LReused1, True),
    ' resumable=', BoolToStr((LSession1 <> nil) and LSession1.IsResumable, True));

  if not ConnectOnce(LSession1, LSession2, LReused2, LErr) then
  begin
    WriteLn('SECOND_FAIL ', LErr);
    Halt(2);
  end;
  WriteLn('SECOND_OK reused=', BoolToStr(LReused2, True),
    ' resumable=', BoolToStr((LSession2 <> nil) and LSession2.IsResumable, True));
end.
PAS

(
  cd "$ROOT_DIR"
  fpc -B -Mobjfpc -Sh \
    -Fu./src -Fi./src -Fu./examples -FU./lib \
    "$WORK_DIR/fp_tls13_resumption_interop_probe.pas" \
    -o"$WORK_DIR/fp_tls13_resumption_interop_probe" >/dev/null
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
  "$WORK_DIR/fp_tls13_resumption_interop_probe" >"$CLIENT_LOG" 2>&1
) || {
  echo "--- client log ---"
  cat "$CLIENT_LOG" || true
  echo "--- server log (tail) ---"
  tail -n 120 "$SERVER_LOG" || true
  fail "pure Pascal/OpenSSL resumption probe should succeed"
}

rg -F --quiet -- "FIRST_OK reused=False resumable=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "first handshake should produce resumable session"
}

rg -F --quiet -- "SECOND_OK reused=True resumable=True" "$CLIENT_LOG" || {
  cat "$CLIENT_LOG"
  fail "second handshake should reuse session against OpenSSL s_server"
}

echo "[PASS] pure Pascal TLS13 resumption OpenSSL interop contract passed"
