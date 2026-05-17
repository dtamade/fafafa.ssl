program test_sslctxboth_client_capability_clarification;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.lib,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.mbedtls.lib;

var
  GTotal: Integer = 0;
  GPassed: Integer = 0;
  GFailed: Integer = 0;
  GSkipped: Integer = 0;

procedure Pass(const AName: string);
begin
  Inc(GTotal);
  Inc(GPassed);
  WriteLn('[PASS] ', AName);
end;

procedure Fail(const AName, ADetail: string);
begin
  Inc(GTotal);
  Inc(GFailed);
  WriteLn('[FAIL] ', AName);
  if ADetail <> '' then
    WriteLn('       ', ADetail);
end;

procedure Skip(const AName, AReason: string);
begin
  Inc(GTotal);
  Inc(GSkipped);
  WriteLn('[SKIP] ', AName, ' - ', AReason);
end;

procedure CheckTrue(const AName: string; ACondition: Boolean; const ADetail: string = '');
begin
  if ACondition then
    Pass(AName)
  else
    Fail(AName, ADetail);
end;

function BytesOfText(const AText: string): TBytes;
begin
  SetLength(Result, Length(AText));
  if Length(Result) > 0 then
    Move(AText[1], Result[0], Length(Result));
end;

procedure TestStreamConnectionServerNameFallback(ABackend: TSSLLibraryType);
var
  LName: string;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LStream: TMemoryStream;
begin
  LName := SSL_LIBRARY_NAMES[ABackend];

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    Skip(LName + ' dual-context stream ServerName fallback',
      'backend not available on this platform');
    Exit;
  end;

  LCtx := TSSLFactory.CreateContext(sslCtxBoth, ABackend);
  CheckTrue(LName + ' dual-context stream context created',
    LCtx <> nil, 'CreateContext(sslCtxBoth) returned nil');
  if LCtx = nil then
    Exit;

  // INTENTIONAL_COMPAT: legacy context-level SNI coverage. This dual-context
  // regression keeps inherited context ServerName fallback observable on purpose.
  {$PUSH}{$WARN 6058 off}
  LCtx.SetServerName('both.example.com');
  {$POP}

  LStream := TMemoryStream.Create;
  try
    LConn := LCtx.CreateConnection(LStream);
    CheckTrue(LName + ' dual-context stream connection created',
      LConn <> nil, 'CreateConnection(TStream) returned nil');
    CheckTrue(LName + ' dual-context stream connection exposes ISSLClientConnection',
      Supports(LConn, ISSLClientConnection, LClientConn),
      'stream connection should remain client-capable for per-connection ServerName');
    if Supports(LConn, ISSLClientConnection, LClientConn) then
      CheckTrue(LName + ' dual-context stream connection inherits context ServerName fallback',
        LClientConn.GetServerName = 'both.example.com',
        'expected both.example.com, actual="' + LClientConn.GetServerName + '"');
  finally
    LStream.Free;
  end;
end;

procedure TestFreePascalSocketConnectionServerNameFallback;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
begin
  LCtx := TSSLFactory.CreateContext(sslCtxBoth, sslFreePascal);
  CheckTrue('FreePascal dual-context socket context created',
    LCtx <> nil, 'CreateContext(sslCtxBoth, sslFreePascal) returned nil');
  if LCtx = nil then
    Exit;

  // INTENTIONAL_COMPAT: legacy context-level SNI coverage. This dual-context
  // socket path also keeps inherited context ServerName fallback observable.
  {$PUSH}{$WARN 6058 off}
  LCtx.SetServerName('both.example.com');
  {$POP}

  LConn := LCtx.CreateConnection(THandle(-1));
  CheckTrue('FreePascal dual-context socket connection created',
    LConn <> nil, 'CreateConnection(THandle(-1)) returned nil');
  CheckTrue('FreePascal dual-context socket connection exposes ISSLClientConnection',
    Supports(LConn, ISSLClientConnection, LClientConn),
    'socket connection should remain client-capable for per-connection ServerName');
  if Supports(LConn, ISSLClientConnection, LClientConn) then
    CheckTrue('FreePascal dual-context socket connection inherits context ServerName fallback',
      LClientConn.GetServerName = 'both.example.com',
      'expected both.example.com, actual="' + LClientConn.GetServerName + '"');
end;

procedure TestDualContextClientEarlyDataRoleGate(ABackend: TSSLLibraryType);
var
  LName: string;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LEarlyCtx: ISSLEarlyDataContext;
  LEarlyConn: ISSLEarlyDataConnection;
  LStream: TMemoryStream;
  LResult: TSSLOperationResult;
begin
  LName := SSL_LIBRARY_NAMES[ABackend];

  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    Skip(LName + ' dual-context early-data client gate',
      'backend not available on this platform');
    Exit;
  end;

  LCtx := TSSLFactory.CreateContext(sslCtxBoth, ABackend);
  if not Supports(LCtx, ISSLEarlyDataContext, LEarlyCtx) then
  begin
    Skip(LName + ' dual-context early-data client gate',
      'context does not expose ISSLEarlyDataContext');
    Exit;
  end;

  LStream := TMemoryStream.Create;
  try
    LConn := LCtx.CreateConnection(LStream);
    if not Supports(LConn, ISSLEarlyDataConnection, LEarlyConn) then
    begin
      Skip(LName + ' dual-context early-data client gate',
        'connection does not expose ISSLEarlyDataConnection');
      Exit;
    end;

    CheckTrue(LName + ' dual-context ConfigureClientEarlyData succeeds',
      TSSLHelper.ConfigureClientEarlyData(LCtx, True),
      'client-scoped early-data helper should accept sslCtxBoth');
    CheckTrue(LName + ' dual-context ConfigureServerEarlyData succeeds',
      TSSLHelper.ConfigureServerEarlyData(LCtx, sslEarlyDataServerIssueOnly, 16),
      'server-scoped early-data helper should accept sslCtxBoth');

    LResult := LEarlyConn.SetEarlyData(BytesOfText('PING'));
    CheckTrue(LName + ' dual-context SetEarlyData still requires a session',
      not LResult.Success,
      'SetEarlyData unexpectedly succeeded without a configured resumable session');
    CheckTrue(LName + ' dual-context SetEarlyData passes the client-role gate',
      LResult.ErrorMessage = 'Early data requires a configured resumable session',
      'actual error="' + LResult.ErrorMessage + '"');
  finally
    LStream.Free;
  end;
end;

begin
  try
    WriteLn('sslCtxBoth Client Capability Clarification');

    TestStreamConnectionServerNameFallback(sslFreePascal);
    TestStreamConnectionServerNameFallback(sslOpenSSL);
    TestStreamConnectionServerNameFallback(sslWolfSSL);
    TestStreamConnectionServerNameFallback(sslMbedTLS);

    TestFreePascalSocketConnectionServerNameFallback;

    TestDualContextClientEarlyDataRoleGate(sslFreePascal);
    TestDualContextClientEarlyDataRoleGate(sslOpenSSL);
    TestDualContextClientEarlyDataRoleGate(sslWolfSSL);

    WriteLn;
    WriteLn('Total:   ', GTotal);
    WriteLn('Passed:  ', GPassed);
    WriteLn('Failed:  ', GFailed);
    WriteLn('Skipped: ', GSkipped);

    if GFailed > 0 then
      Halt(1);

    WriteLn('All tests passed.');
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
