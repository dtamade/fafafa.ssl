program test_openssl_connection_info_cipher_contract;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.connection;

var
  GLib: ISSLLibrary = nil;
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  SkippedTests: Integer = 0;

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

procedure MarkSkip(const AName, AReason: string);
begin
  Inc(TotalTests);
  Inc(SkippedTests);
  WriteLn('[SKIP] [capability] ', AName, ' - ', AReason);
end;

function StubSSLGetCurrentCipherNonNil(const ssl: PSSL): PSSL_CIPHER; cdecl;
begin
  Result := PSSL_CIPHER(Pointer(PtrUInt(1)));
end;

function StubSSLCipherGetBits(const cipher: PSSL_CIPHER; alg_bits: PInteger): Integer; cdecl;
begin
  if alg_bits <> nil then
    alg_bits^ := 0;
  Result := 0;
end;

procedure WarmupStreamConnectionConstructor(AContext: ISSLContext);
var
  LStream: TMemoryStream;
  LConn: TOpenSSLConnection;
begin
  LStream := TMemoryStream.Create;
  LConn := nil;
  try
    LConn := TOpenSSLConnection.Create(AContext, LStream);
    if LConn = nil then
      raise Exception.Create('stream connection constructor warmup returned nil');
  finally
    if Assigned(LConn) then
      LConn.Free;
    LStream.Free;
  end;
end;

function CaptureFreshConnectionInfo(AContext: ISSLContext): TSSLConnectionInfo;
var
  LStream: TMemoryStream;
  LConn: TOpenSSLConnection;
begin
  LStream := TMemoryStream.Create;
  LConn := nil;
  try
    LConn := TOpenSSLConnection.Create(AContext, LStream);
    Result := LConn.GetConnectionInfo;
  finally
    if Assigned(LConn) then
      LConn.Free;
    LStream.Free;
  end;
end;

procedure AssertConnectionInfoSafeDegrade(
  const AName: string;
  AContext: ISSLContext;
  const AExpected: TSSLConnectionInfo
);
var
  LStream: TMemoryStream;
  LConn: TOpenSSLConnection;
  LRaised: Boolean;
  LInfo: TSSLConnectionInfo;
  LDetail: string;
begin
  LStream := TMemoryStream.Create;
  LConn := nil;
  try
    LConn := TOpenSSLConnection.Create(AContext, LStream);

    LRaised := False;
    FillChar(LInfo, SizeOf(LInfo), 0);
    LDetail := '';
    try
      LInfo := LConn.GetConnectionInfo;
    except
      on E: Exception do
      begin
        LRaised := True;
        LDetail := E.ClassName + ': ' + E.Message;
      end;
    end;

    AssertTrue(AName + ' should not raise', not LRaised, LDetail);
    AssertTrue(AName + ' should preserve ProtocolVersion baseline',
      LInfo.ProtocolVersion = AExpected.ProtocolVersion,
      'expected GetConnectionInfo to preserve inherited protocol baseline');
    AssertTrue(AName + ' should preserve CipherSuite baseline',
      LInfo.CipherSuite = AExpected.CipherSuite,
      'expected GetConnectionInfo to preserve inherited cipher baseline');
    AssertTrue(AName + ' should preserve KeySize baseline',
      LInfo.KeySize = AExpected.KeySize,
      'expected GetConnectionInfo to preserve inherited key-size baseline');
    AssertTrue(AName + ' should preserve ServerName baseline',
      LInfo.ServerName = AExpected.ServerName,
      'expected GetConnectionInfo to preserve inherited server-name baseline');
  finally
    if Assigned(LConn) then
      LConn.Free;
    LStream.Free;
  end;
end;

procedure TestGetConnectionInfoShouldDegradeSafelyWhenCipherHelpersAreUnavailable;
var
  LContext: ISSLContext;
  LBaselineInfo: TSSLConnectionInfo;
  LOriginalSSLGetCurrentCipher: TSSL_get_current_cipher;
  LOriginalSSLCipherGetName: TSSL_CIPHER_get_name;
  LOriginalSSLCipherGetBits: TSSL_CIPHER_get_bits;
begin
  WriteLn;
  WriteLn('=== OpenSSL connection info cipher guard ===');

  if (not Assigned(SSL_new)) or
     (not Assigned(SSL_set_bio)) or
     (not Assigned(BIO_new)) or
     (not Assigned(BIO_s_mem)) then
  begin
    MarkSkip('openssl connection info cipher contract',
      'required baseline OpenSSL SSL/BIO helpers are unavailable');
    Exit;
  end;

  LContext := GLib.CreateContext(sslCtxClient);
  if LContext = nil then
    raise Exception.Create('failed to create OpenSSL client context');

  WarmupStreamConnectionConstructor(LContext);
  LBaselineInfo := CaptureFreshConnectionInfo(LContext);

  LOriginalSSLGetCurrentCipher := SSL_get_current_cipher;
  LOriginalSSLCipherGetName := SSL_CIPHER_get_name;
  LOriginalSSLCipherGetBits := SSL_CIPHER_get_bits;
  try
    SSL_get_current_cipher := nil;
    SSL_CIPHER_get_name := LOriginalSSLCipherGetName;
    SSL_CIPHER_get_bits := LOriginalSSLCipherGetBits;
    AssertConnectionInfoSafeDegrade(
      'GetConnectionInfo when SSL_get_current_cipher is unavailable',
      LContext,
      LBaselineInfo
    );

    SSL_get_current_cipher := @StubSSLGetCurrentCipherNonNil;
    SSL_CIPHER_get_name := nil;
    SSL_CIPHER_get_bits := @StubSSLCipherGetBits;
    AssertConnectionInfoSafeDegrade(
      'GetConnectionInfo when SSL_CIPHER_get_name is unavailable',
      LContext,
      LBaselineInfo
    );
  finally
    SSL_get_current_cipher := LOriginalSSLGetCurrentCipher;
    SSL_CIPHER_get_name := LOriginalSSLCipherGetName;
    SSL_CIPHER_get_bits := LOriginalSSLCipherGetBits;
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('OpenSSL Connection Info Cipher Contract Test');
  WriteLn('========================================');

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (not Assigned(GLib)) or (not GLib.Initialize) then
      MarkSkip('openssl connection info cipher contract',
        'failed to initialize OpenSSL library');

    if SkippedTests = 0 then
    begin
      LoadOpenSSLCore();
      LoadOpenSSLBIO();
      if not LoadOpenSSLSSL then
        raise Exception.Create('failed to load SSL support');
    end;

    if SkippedTests = 0 then
      TestGetConnectionInfoShouldDegradeSafelyWhenCipherHelpersAreUnavailable;

    WriteLn;
    WriteLn('========================================');
    WriteLn('Summary');
    WriteLn('========================================');
    WriteLn('Total tests: ', TotalTests);
    WriteLn('Passed: ', PassedTests);
    WriteLn('Failed: ', FailedTests);
    WriteLn('Skipped: ', SkippedTests);

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
