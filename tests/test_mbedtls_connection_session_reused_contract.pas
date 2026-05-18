program test_mbedtls_connection_session_reused_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api,
  fafafa.ssl.mbedtls.connection;

type
  TMockMbedTLSSession = class(TInterfacedObject, ISSLSession, ISSLNativeHandleAccess)
  private
    FID: string;
    FHandle: Pointer;
    FCreationTime: TDateTime;
    FTimeout: Integer;
  public
    constructor Create(const AID: string; AHandle: Pointer);
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(ATimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function Serialize: TBytes;
    function Deserialize(const AData: TBytes): Boolean;
    function Clone: ISSLSession;
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;
  end;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  GSetSessionCalls: Integer = 0;

procedure AssertTrue(const AName: string; ACondition: Boolean; const ADetail: string = '');
begin
  Inc(TotalTests);
  if ACondition then
  begin
    Inc(PassedTests);
    WriteLn('[PASS] ', AName);
  end
  else
  begin
    Inc(FailedTests);
    WriteLn('[FAIL] ', AName);
    if ADetail <> '' then
      WriteLn('       ', ADetail);
  end;
end;

function FakeMbedTLSSSLSetSession(ssl: Pmbedtls_ssl_context;
  session: Pmbedtls_ssl_session): Integer; cdecl;
begin
  Inc(GSetSessionCalls);
  if (ssl = nil) or (session = nil) then
    Exit(-1);
  Result := 0;
end;

constructor TMockMbedTLSSession.Create(const AID: string; AHandle: Pointer);
begin
  inherited Create;
  FID := AID;
  FHandle := AHandle;
  FCreationTime := Now;
  FTimeout := 3600;
end;

function TMockMbedTLSSession.GetID: string;
begin
  Result := FID;
end;

function TMockMbedTLSSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TMockMbedTLSSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TMockMbedTLSSession.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TMockMbedTLSSession.IsValid: Boolean;
begin
  Result := (FHandle <> nil) and (FID <> '');
end;

function TMockMbedTLSSession.IsResumable: Boolean;
begin
  Result := IsValid;
end;

function TMockMbedTLSSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS12;
end;

function TMockMbedTLSSession.GetCipherName: string;
begin
  Result := 'MOCK-MBEDTLS-SESSION';
end;

function TMockMbedTLSSession.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMockMbedTLSSession.Serialize: TBytes;
begin
  SetLength(Result, 0);
end;

function TMockMbedTLSSession.Deserialize(const AData: TBytes): Boolean;
begin
  Result := Length(AData) > 0;
end;

function TMockMbedTLSSession.Clone: ISSLSession;
begin
  Result := TMockMbedTLSSession.Create(FID, FHandle);
end;

function TMockMbedTLSSession.GetNativeHandle: Pointer;
begin
  Result := FHandle;
end;

function TMockMbedTLSSession.GetBackendType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TMockMbedTLSSession.IsNativeHandleValid: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TestSetSessionMustNotPreclaimResumedHandshake;
var
  LConn: TMbedTLSConnection;
  LStream: TMemoryStream;
  LSession: ISSLSession;
  LOriginalSSLInit: Tmbedtls_ssl_init;
  LOriginalSSLFree: Tmbedtls_ssl_free;
  LOriginalSSLSetup: Tmbedtls_ssl_setup;
  LOriginalSSLSetBio: Tmbedtls_ssl_set_bio;
  LOriginalSSLSetSession: Tmbedtls_ssl_set_session;
begin
  WriteLn;
  WriteLn('=== MbedTLS session reused semantic truth ===');

  LOriginalSSLInit := mbedtls_ssl_init;
  LOriginalSSLFree := mbedtls_ssl_free;
  LOriginalSSLSetup := mbedtls_ssl_setup;
  LOriginalSSLSetBio := mbedtls_ssl_set_bio;
  LOriginalSSLSetSession := mbedtls_ssl_set_session;

  mbedtls_ssl_init := nil;
  mbedtls_ssl_free := nil;
  mbedtls_ssl_setup := nil;
  mbedtls_ssl_set_bio := nil;
  mbedtls_ssl_set_session := @FakeMbedTLSSSLSetSession;
  GSetSessionCalls := 0;

  LStream := TMemoryStream.Create;
  LConn := nil;
  try
    LConn := TMbedTLSConnection.Create(nil, nil, LStream);
    LSession := TMockMbedTLSSession.Create('mock-mbedtls-session', Pointer(PtrUInt($1234)));

    AssertTrue('fresh connection starts with IsSessionReused=False',
      not LConn.IsSessionReused);

    LConn.SetSession(LSession);

    AssertTrue('SetSession still attempts native mbedtls_ssl_set_session when helper exists',
      GSetSessionCalls = 1,
      'expected fake mbedtls_ssl_set_session to be called exactly once');
    AssertTrue('SetSession must not claim a resumed handshake before Connect/DoHandshake',
      not LConn.IsSessionReused,
      'configured session should not be reported as an actually reused handshake');
  finally
    if Assigned(LConn) then
      LConn.Free;
    LStream.Free;
    mbedtls_ssl_init := LOriginalSSLInit;
    mbedtls_ssl_free := LOriginalSSLFree;
    mbedtls_ssl_setup := LOriginalSSLSetup;
    mbedtls_ssl_set_bio := LOriginalSSLSetBio;
    mbedtls_ssl_set_session := LOriginalSSLSetSession;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('MbedTLS Connection Session Reused Contract Test');
  WriteLn('========================================');

  try
    TestSetSessionMustNotPreclaimResumedHandshake;

    WriteLn;
    WriteLn('========================================');
    WriteLn('Summary');
    WriteLn('========================================');
    WriteLn('Total tests: ', TotalTests);
    WriteLn('Passed: ', PassedTests);
    WriteLn('Failed: ', FailedTests);

    if FailedTests > 0 then
      Halt(1);
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ', E.ClassName, ': ', E.Message);
      Halt(2);
    end;
  end;
end.
