program test_connection_base_last_error_time_contract;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.connection.base;

type
  TDummyConnection = class(TBaseSSLConnection)
  private
    FForcedError: TSSLErrorCode;
  public
    procedure InjectError(ACode: TSSLErrorCode; const AMessage: string);
    procedure SetForcedError(ACode: TSSLErrorCode);
  protected
    function DoRead(var ABuffer; ACount: Integer): Integer; override;
    function DoWrite(const ABuffer; ACount: Integer): Integer; override;
    function DoConnect: Boolean; override;
    function DoAccept: Boolean; override;
    function DoHandshakeInternal: TSSLHandshakeState; override;
    function DoShutdown: Boolean; override;
    procedure DoClose; override;
    function DoRenegotiate: Boolean; override;
    function DoGetError(ARet: Integer): TSSLErrorCode; override;
    function DoWantRead: Boolean; override;
    function DoWantWrite: Boolean; override;
    function DoGetProtocolVersion: TSSLProtocolVersion; override;
    function DoGetCipherName: string; override;
    function DoGetPeerCertificate: ISSLCertificate; override;
    function DoGetPeerCertificateChain: TSSLCertificateArray; override;
    function DoGetVerifyResult: Integer; override;
    function DoGetVerifyResultString: string; override;
    function DoGetSession: ISSLSession; override;
    procedure DoSetSession(ASession: ISSLSession); override;
    function DoIsSessionReused: Boolean; override;
    function DoGetSelectedALPNProtocol: string; override;
    function DoGetState: string; override;
    function DoGetNativeHandle: Pointer; override;
  end;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure TDummyConnection.InjectError(ACode: TSSLErrorCode; const AMessage: string);
begin
  RecordError(ACode, AMessage);
end;

procedure TDummyConnection.SetForcedError(ACode: TSSLErrorCode);
begin
  FForcedError := ACode;
end;

function TDummyConnection.DoRead(var ABuffer; ACount: Integer): Integer;
begin
  Result := 0;
end;

function TDummyConnection.DoWrite(const ABuffer; ACount: Integer): Integer;
begin
  Result := ACount;
end;

function TDummyConnection.DoConnect: Boolean;
begin
  Result := True;
end;

function TDummyConnection.DoAccept: Boolean;
begin
  Result := True;
end;

function TDummyConnection.DoHandshakeInternal: TSSLHandshakeState;
begin
  Result := sslHsCompleted;
end;

function TDummyConnection.DoShutdown: Boolean;
begin
  Result := True;
end;

procedure TDummyConnection.DoClose;
begin
end;

function TDummyConnection.DoRenegotiate: Boolean;
begin
  Result := True;
end;

function TDummyConnection.DoGetError(ARet: Integer): TSSLErrorCode;
begin
  Result := FForcedError;
end;

function TDummyConnection.DoWantRead: Boolean;
begin
  Result := False;
end;

function TDummyConnection.DoWantWrite: Boolean;
begin
  Result := False;
end;

function TDummyConnection.DoGetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS13;
end;

function TDummyConnection.DoGetCipherName: string;
begin
  Result := 'DUMMY-CIPHER';
end;

function TDummyConnection.DoGetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TDummyConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := nil;
end;

function TDummyConnection.DoGetVerifyResult: Integer;
begin
  Result := 0;
end;

function TDummyConnection.DoGetVerifyResultString: string;
begin
  Result := '';
end;

function TDummyConnection.DoGetSession: ISSLSession;
begin
  Result := nil;
end;

procedure TDummyConnection.DoSetSession(ASession: ISSLSession);
begin
end;

function TDummyConnection.DoIsSessionReused: Boolean;
begin
  Result := False;
end;

function TDummyConnection.DoGetSelectedALPNProtocol: string;
begin
  Result := '';
end;

function TDummyConnection.DoGetState: string;
begin
  Result := 'dummy';
end;

function TDummyConnection.DoGetNativeHandle: Pointer;
begin
  Result := nil;
end;

var
  LConn: TDummyConnection;
  LOverflowConn: TDummyConnection;
  LInitial: TSSLHealthStatus;
  LAfterError: TSSLHealthStatus;
  LAfterPoll: TSSLHealthStatus;
  LDiag: TSSLDiagnosticInfo;
  LDelta: Double;
  I: Integer;
const
  ONE_MILLISECOND_IN_DAYS = 1 / 86400000;
  OVERFLOW_TOTAL = 125;
  DEFAULT_MAX_HISTORY = 100;
begin
  WriteLn('fafafa.ssl - connection base last error time contract');

  LConn := TDummyConnection.Create(nil);
  try
    LConn.SetForcedError(sslErrNone);

    LInitial := LConn.GetHealthStatus;
    AssertTrue(LInitial.LastError = sslErrNone,
      'new connection should have sslErrNone');
    AssertTrue(LInitial.LastErrorTime = 0,
      Format('new connection LastErrorTime should be 0, got %.16f', [LInitial.LastErrorTime]));

    LConn.SetForcedError(sslErrIO);
    AssertTrue(LConn.GetError(-1) = sslErrIO,
      'GetError should return forced backend error');
    LAfterError := LConn.GetHealthStatus;
    AssertTrue(LAfterError.LastError = sslErrIO,
      'GetError should synchronize LastError into health status');
    AssertTrue(LAfterError.LastErrorTime > 0,
      'GetError non-none path should set LastErrorTime');

    LConn.InjectError(sslErrHandshake, 'synthetic error for contract');
    LAfterError := LConn.GetHealthStatus;
    AssertTrue(LAfterError.LastError = sslErrHandshake,
      'after RecordError, LastError should match injected error code');
    AssertTrue(LAfterError.LastErrorTime > 0,
      'after RecordError, LastErrorTime should be populated');

    Sleep(30);
    LAfterPoll := LConn.GetHealthStatus;
    LDelta := Abs(LAfterPoll.LastErrorTime - LAfterError.LastErrorTime);
    AssertTrue(LDelta < ONE_MILLISECOND_IN_DAYS,
      Format('LastErrorTime should remain stable across reads; delta=%.16f', [LDelta]));
  finally
    LConn.Free;
  end;

  LOverflowConn := TDummyConnection.Create(nil);
  try
    for I := 1 to OVERFLOW_TOTAL do
      LOverflowConn.InjectError(sslErrIO, Format('overflow-%.3d', [I]));

    LDiag := LOverflowConn.GetDiagnosticInfo;
    AssertTrue(Length(LDiag.ErrorHistory) = DEFAULT_MAX_HISTORY,
      Format('overflow history size should be %d, got %d',
        [DEFAULT_MAX_HISTORY, Length(LDiag.ErrorHistory)]));

    AssertTrue(LDiag.ErrorHistory[0].ErrorMessage = 'overflow-026',
      'overflow history should retain latest entries in order (first retained = overflow-026)');
    AssertTrue(LDiag.ErrorHistory[High(LDiag.ErrorHistory)].ErrorMessage = 'overflow-125',
      'overflow history should retain last entry (overflow-125)');
  finally
    LOverflowConn.Free;
  end;

  WriteLn('✅ connection base last error time contract passed');
end.
