program test_ocsp_connection_verification_regression;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Dynlibs, ctypes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.lib,
  fafafa.ssl.openssl.connection,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.native_handle,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.base;

type
  TCRYPTO_malloc_fn = function(num: size_t; const fname: PAnsiChar; line: Integer): Pointer; cdecl;

  TOpenSSLConnectionAccess = class(TOpenSSLConnection)
  protected
    function EnsureOCSPModuleLoaded: Boolean; override;
  public
    function CheckRequiredOCSPStapling(AIsClient: Boolean): Boolean;
  end;

var
  GForceOCSPModuleLoadFailure: Boolean = False;

function TOpenSSLConnectionAccess.CheckRequiredOCSPStapling(AIsClient: Boolean): Boolean;
begin
  Result := ValidateRequiredOCSPStapling(AIsClient);
end;

function TOpenSSLConnectionAccess.EnsureOCSPModuleLoaded: Boolean;
begin
  if GForceOCSPModuleLoadFailure then
    Exit(False);
  Result := inherited EnsureOCSPModuleLoaded;
end;

type
  TSkipCategory = (
    scDependency,
    scVersion,
    scEnvironment,
    scCapability,
    scOther
  );

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  TestsSkipped: Integer = 0;
  SkipDependency: Integer = 0;
  SkipVersion: Integer = 0;
  SkipEnvironment: Integer = 0;
  SkipCapability: Integer = 0;
  SkipOther: Integer = 0;
  GD2IOCSPResponseCalls: Integer = 0;
  GOCSPResponseStatusCalls: Integer = 0;

function CountingD2IOCSPResponse(a: PPOCSP_RESPONSE; const in_: PPByte; len: Integer): POCSP_RESPONSE; cdecl;
begin
  Inc(GD2IOCSPResponseCalls);
  Result := nil;
end;

function CountingOCSPResponseStatus(a: POCSP_RESPONSE): Integer; cdecl;
begin
  Inc(GOCSPResponseStatusCalls);
  Result := OCSP_RESPONSE_STATUS_SUCCESSFUL;
end;

function DummyOCSPResponseStatus(a: POCSP_RESPONSE): Integer; cdecl;
begin
  Result := OCSP_RESPONSE_STATUS_SUCCESSFUL;
end;

procedure NoopOCSPResponseFree(a: POCSP_RESPONSE); cdecl;
begin
end;

function SkipCategoryLabel(ACategory: TSkipCategory): string;
begin
  case ACategory of
    scDependency: Result := 'dependency';
    scVersion: Result := 'version';
    scEnvironment: Result := 'environment';
    scCapability: Result := 'capability';
  else
    Result := 'other';
  end;
end;

procedure LogPass(const AMessage: string);
begin
  Inc(TestsPassed);
  WriteLn('[PASS] ', AMessage);
end;

procedure LogFail(const AMessage: string);
begin
  Inc(TestsFailed);
  WriteLn('[FAIL] ', AMessage);
end;

procedure LogSkip(const AMessage: string; ACategory: TSkipCategory = scOther);
begin
  Inc(TestsSkipped);

  case ACategory of
    scDependency: Inc(SkipDependency);
    scVersion: Inc(SkipVersion);
    scEnvironment: Inc(SkipEnvironment);
    scCapability: Inc(SkipCapability);
  else
    Inc(SkipOther);
  end;

  WriteLn('[SKIP] [', SkipCategoryLabel(ACategory), '] ', AMessage);
end;

procedure CleanupOpenSSLMemory(APtr: Pointer);
begin
  if APtr = nil then
    Exit;

  if Assigned(OPENSSL_free) then
    OPENSSL_free(APtr)
  else if Assigned(CRYPTO_free) then
    CRYPTO_free(APtr, nil, 0);
end;

function LoadSuccessfulBasicOCSPFixture(out ADer: TBytes): Boolean;
const
  FIXTURE_PATH = './tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der';
var
  LStream: TFileStream;
begin
  Result := False;
  SetLength(ADer, 0);

  if not FileExists(FIXTURE_PATH) then
    Exit;

  LStream := TFileStream.Create(FIXTURE_PATH, fmOpenRead or fmShareDenyNone);
  try
    SetLength(ADer, LStream.Size);
    if Length(ADer) > 0 then
      LStream.ReadBuffer(ADer[0], Length(ADer));
  finally
    LStream.Free;
  end;

  Result := Length(ADer) > 0;
end;

procedure TestOCSPLowercaseSymbolAliasLoading;
begin
  WriteLn;
  WriteLn('=== OCSP OpenSSL 3.x lowercase symbol alias loading ===');

  try
    LoadOpenSSLCore;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(OCSP_RESPONSE_create) then
    begin
      LogFail('OCSP_RESPONSE_create unresolved (expected lowercase alias fallback)');
      Exit;
    end;

    if not Assigned(OCSP_RESPONSE_status) then
    begin
      LogFail('OCSP_RESPONSE_status unresolved (expected lowercase alias fallback)');
      Exit;
    end;

    if not Assigned(OCSP_RESPONSE_get1_basic) then
    begin
      LogFail('OCSP_RESPONSE_get1_basic unresolved (expected lowercase alias fallback)');
      Exit;
    end;

    LogPass('OCSP lowercase alias fallback is loaded');

  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;
end;

procedure TestOCSPStatusRequestEnablementFromContextOption;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNativeConn: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LCtxHandle: PSSL_CTX;
  LSSL: PSSL;
  LOptions: TSSLOptions;
  LCtxType: clong;
  LBeforeType: clong;
  LAfterType: clong;
  LState: TSSLHandshakeState;
begin
  WriteLn;
  WriteLn('=== OCSP status_request enablement from context option ===');

  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNativeConn := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;

    if not Assigned(SSL_CTX_get_tlsext_status_type) then
    begin
      if Assigned(SSL_CTX_ctrl) then
        LogFail('SSL_CTX_get_tlsext_status_type should be available via wrapper when SSL_CTX_ctrl exists')
      else
        LogSkip('SSL_CTX_get_tlsext_status_type unavailable', scCapability);
      Exit;
    end;

    if not Assigned(SSL_get_tlsext_status_type) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_get_tlsext_status_type should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_get_tlsext_status_type unavailable', scCapability);
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LCtxHandle := PSSL_CTX(GetNativeHandleSafe(LContext, 'TestOCSPStatusRequestEnablementFromContextOption.Context'));
    LCtxType := SSL_CTX_get_tlsext_status_type(LCtxHandle);
    if LCtxType <> 0 then
    begin
      LogFail(Format('Expected initial context status_type = 0, got %d', [LCtxType]));
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := LContext.CreateConnection(LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNativeConn) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNativeConn.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LBeforeType := SSL_get_tlsext_status_type(LSSL);
    if LBeforeType <> 0 then
    begin
      LogFail(Format('Expected initial connection status_type = 0, got %d', [LBeforeType]));
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    LContext.SetOptions(LOptions);

    LCtxType := SSL_CTX_get_tlsext_status_type(LCtxHandle);
    if LCtxType <> TLSEXT_STATUSTYPE_ocsp then
    begin
      LogFail(Format('Expected context status_type = %d after enabling option, got %d',
        [TLSEXT_STATUSTYPE_ocsp, LCtxType]));
      Exit;
    end;

    // Existing SSL connection should adopt updated option before handshake attempt
    LState := LConn.DoHandshake;
    LAfterType := SSL_get_tlsext_status_type(LSSL);
    if LAfterType <> TLSEXT_STATUSTYPE_ocsp then
    begin
      LogFail(Format('Expected connection status_type = %d after handshake attempt, got %d (state=%d)',
        [TLSEXT_STATUSTYPE_ocsp, LAfterType, Ord(LState)]));
      Exit;
    end;

    LogPass('OCSP status_request is enabled on context and propagated to connection pre-handshake');

  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestRequiredOCSPStaplingFailClosedPolicy;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LVerifyRes: Integer;
begin
  WriteLn;
  WriteLn('=== Required OCSP stapling fail-closed policy ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    if not (ssoRequireOCSPStapling in LContext.GetOptions) then
    begin
      LogFail('ssoRequireOCSPStapling option is not persisted in context');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    // Case 1: required + no stapled response -> fail with VERIFY_NEEDED
    if LConnAccess.CheckRequiredOCSPStapling(True) then
    begin
      LogFail('Expected fail-closed when required stapled response is missing');
      Exit;
    end;

    if Assigned(SSL_get_verify_result) then
    begin
      LVerifyRes := SSL_get_verify_result(LSSL);
      if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_NEEDED then
      begin
        LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_NEEDED (%d), got %d',
          [X509_V_ERR_OCSP_VERIFY_NEEDED, LVerifyRes]));
        Exit;
      end;
    end;

    // Case 2: required + stapled response present but unverifiable -> fail with VERIFY_FAILED
    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    if LConnAccess.CheckRequiredOCSPStapling(True) then
    begin
      LogFail('Expected fail-closed when required stapled response is not verified');
      Exit;
    end;

    if Assigned(SSL_get_verify_result) then
    begin
      LVerifyRes := SSL_get_verify_result(LSSL);
      if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
      begin
        LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
          [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
        Exit;
      end;
    end;

    LogPass('Required OCSP stapling fail-closed policy is enforced (missing/unverified response)');

  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestSuccessfulStapledOCSPFixtureMustNotVerifyWithoutPeerContext;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
begin
  WriteLn;
  WriteLn('=== Regression: successful/basic stapled OCSP fixture must not verify without peer context ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_get_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_get_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_get_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := LContext.CreateConnection(LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // Ownership transferred to SSL instance

    LStatus := LConn.GetOCSPResponseStatus;
    if LStatus <> 'Successful' then
    begin
      LogFail('Expected OCSP status = Successful, got: ' + LStatus);
      Exit;
    end;

    if LConn.IsOCSPResponseVerified then
      LogFail('Successful/basic stapled fixture must NOT be treated as verified without peer cert context')
    else
      LogPass('Successful/basic stapled fixture is correctly rejected without peer cert context');

  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestRequiredOCSPStaplingResponseFreeMissingPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LVerifyRes: Integer;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
begin
  WriteLn;
  WriteLn('=== Required stapling preflight: missing OCSP_RESPONSE_free must block parse ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := nil;
      GD2IOCSPResponseCalls := 0;

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected fail-closed when OCSP_RESPONSE_free is unavailable');
        Exit;
      end;

      if Assigned(SSL_get_verify_result) then
      begin
        LVerifyRes := SSL_get_verify_result(LSSL);
        if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
        begin
          LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
            [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
          Exit;
        end;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected preflight to block d2i parse when OCSP_RESPONSE_free is missing, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Missing OCSP_RESPONSE_free blocks stapled response parse and fails closed');
    finally
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestOCSPResponseStatusResponseFreeMissingPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
begin
  WriteLn;
  WriteLn('=== OCSP status preflight: missing OCSP_RESPONSE_free must block parse ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := LContext.CreateConnection(LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := nil;
      GD2IOCSPResponseCalls := 0;

      LStatus := LConn.GetOCSPResponseStatus;
      if LStatus <> 'OCSP API not available' then
      begin
        LogFail('Expected OCSP status path to fail closed with API-unavailable semantic, got: ' + LStatus);
        Exit;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected status preflight to block d2i parse when OCSP_RESPONSE_free is missing, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Missing OCSP_RESPONSE_free blocks status parse and returns API-unavailable semantic');
    finally
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestOCSPResponseStatusStatusApiMissingPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
begin
  WriteLn;
  WriteLn('=== OCSP status preflight: missing OCSP_RESPONSE_status must block parse ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := LContext.CreateConnection(LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := nil;
      GD2IOCSPResponseCalls := 0;

      LStatus := LConn.GetOCSPResponseStatus;
      if LStatus <> 'OCSP status API not available' then
      begin
        LogFail('Expected OCSP status path to fail closed with status-API semantic, got: ' + LStatus);
        Exit;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected status preflight to block d2i parse when OCSP_RESPONSE_status is missing, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Missing OCSP_RESPONSE_status blocks status parse and returns status-API semantic');
    finally
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestOCSPResponseStatusModuleLoadFailurePreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedModuleLoaded: Boolean;
begin
  WriteLn;
  WriteLn('=== OCSP status preflight: module load failure must block parse ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := TOpenSSLConnectionAccess.Create(LContext, LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    LSavedModuleLoaded := TOpenSSLLoader.IsModuleLoaded(osmOCSP);
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);
      GD2IOCSPResponseCalls := 0;
      GForceOCSPModuleLoadFailure := True;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);

      LStatus := LConn.GetOCSPResponseStatus;
      if LStatus <> 'OCSP API not available' then
      begin
        LogFail('Expected OCSP status path to fail closed when module load fails, got: ' + LStatus);
        Exit;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected module-load-failure preflight to block d2i parse, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Module load failure blocks status parse and returns API-unavailable semantic');
    finally
      if LSavedModuleLoaded then
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, True)
      else
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);
      GForceOCSPModuleLoadFailure := False;
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestOCSPResponseStatusModuleLoadFailureRecoveryPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LRecoveredStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedModuleLoaded: Boolean;
begin
  WriteLn;
  WriteLn('=== OCSP status preflight: module load failure should recover on retry ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := TOpenSSLConnectionAccess.Create(LContext, LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    LSavedModuleLoaded := TOpenSSLLoader.IsModuleLoaded(osmOCSP);
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);

      GD2IOCSPResponseCalls := 0;
      GForceOCSPModuleLoadFailure := True;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);

      LStatus := LConn.GetOCSPResponseStatus;
      if LStatus <> 'OCSP API not available' then
      begin
        LogFail('Expected first status query to fail closed when module load fails, got: ' + LStatus);
        Exit;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected first attempt to block d2i parse on module-load failure, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      GForceOCSPModuleLoadFailure := False;
      GD2IOCSPResponseCalls := 0;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);

      LRecoveredStatus := LConn.GetOCSPResponseStatus;
      if LRecoveredStatus = 'OCSP API not available' then
      begin
        LogFail('Expected retry to recover from prior module-load failure, still got API-unavailable semantic');
        Exit;
      end;

      if not TOpenSSLLoader.IsModuleLoaded(osmOCSP) then
      begin
        LogFail('Expected retry to recover OCSP module loaded state, but module flag is still false');
        Exit;
      end;

      LogPass('Module-load-failure state recovers on retry and status path exits API-unavailable state');
    finally
      if LSavedModuleLoaded then
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, True)
      else
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);
      GForceOCSPModuleLoadFailure := False;
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestOCSPResponseStatusD2IMissingPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
begin
  WriteLn;
  WriteLn('=== OCSP status preflight: missing d2i_OCSP_RESPONSE must fail closed ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := LContext.CreateConnection(LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    try
      d2i_OCSP_RESPONSE := nil;
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@CountingOCSPResponseStatus);
      GOCSPResponseStatusCalls := 0;

      LStatus := LConn.GetOCSPResponseStatus;
      if LStatus <> 'OCSP API not available' then
      begin
        LogFail('Expected OCSP status path to fail closed with API-unavailable semantic when d2i is missing, got: ' + LStatus);
        Exit;
      end;

      if GOCSPResponseStatusCalls <> 0 then
      begin
        LogFail(Format('Expected missing d2i preflight to block status resolver calls, got status calls=%d',
          [GOCSPResponseStatusCalls]));
        Exit;
      end;

      LogPass('Missing d2i_OCSP_RESPONSE blocks status path and returns API-unavailable semantic');
    finally
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestOCSPParseFailureSemanticAlignment;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LVerifyRes: Integer;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
begin
  WriteLn;
  WriteLn('=== OCSP parse-failure semantic alignment: status string vs required-stapling verify_result ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);
      GD2IOCSPResponseCalls := 0;

      LStatus := LConnAccess.GetOCSPResponseStatus;
      if LStatus <> 'Failed to parse OCSP response' then
      begin
        LogFail('Expected status-path parse-failure semantic, got: ' + LStatus);
        Exit;
      end;

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected required-stapling parse-failure path to fail closed');
        Exit;
      end;

      if Assigned(SSL_get_verify_result) then
      begin
        LVerifyRes := SSL_get_verify_result(LSSL);
        if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
        begin
          LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
            [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
          Exit;
        end;
      end;

      if GD2IOCSPResponseCalls < 2 then
      begin
        LogFail(Format('Expected both status and required-stapling paths to hit parse-failure, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Parse-failure semantics are aligned: status string + required-stapling verify_result');
    finally
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestEnsureOCSPModuleLoadedPointerRebindingStability;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LStatus: string;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedModuleLoaded: Boolean;
begin
  WriteLn;
  WriteLn('=== EnsureOCSPModuleLoaded pointer-rebinding stability ===');

  LOpenSSLResp := nil;
  LLibrary := nil;
  LContext := nil;
  LConn := nil;
  LNative := nil;
  LStream := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    LConn := LContext.CreateConnection(LStream);
    if LConn = nil then
    begin
      LogFail('CreateConnection returned nil');
      Exit;
    end;

    if not Supports(LConn, ISSLNativeHandleAccess, LNative) then
    begin
      LogFail('Connection does not support ISSLNativeHandleAccess');
      Exit;
    end;

    LSSL := PSSL(LNative.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    LSavedModuleLoaded := TOpenSSLLoader.IsModuleLoaded(osmOCSP);
    try
      GForceOCSPModuleLoadFailure := False;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);
      GD2IOCSPResponseCalls := 0;

      LStatus := LConn.GetOCSPResponseStatus;

      if not TOpenSSLLoader.IsModuleLoaded(osmOCSP) then
      begin
        LogFail('Expected EnsureOCSPModuleLoaded to recover OCSP module loaded state');
        Exit;
      end;

      if Pointer(d2i_OCSP_RESPONSE) = Pointer(@CountingD2IOCSPResponse) then
      begin
        LogFail('Expected LoadOpenSSLOCSP to rebind d2i_OCSP_RESPONSE away from counting stub');
        Exit;
      end;

      if LStatus = 'OCSP API not available' then
      begin
        LogFail('Expected status path to exit API-unavailable after module reload, got: ' + LStatus);
        Exit;
      end;

      LogPass('EnsureOCSPModuleLoaded reload rebinds OCSP pointers and exits API-unavailable state');
    finally
      if LSavedModuleLoaded then
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, True)
      else
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);
      GForceOCSPModuleLoadFailure := False;
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  LConn := nil;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestRequiredOCSPStaplingResponseStatusMissingPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LVerifyRes: Integer;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
begin
  WriteLn;
  WriteLn('=== Required OCSP stapling preflight: missing OCSP_RESPONSE_status must block parse ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := nil;
      GD2IOCSPResponseCalls := 0;

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected fail-closed when OCSP_RESPONSE_status is unavailable');
        Exit;
      end;

      if Assigned(SSL_get_verify_result) then
      begin
        LVerifyRes := SSL_get_verify_result(LSSL);
        if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
        begin
          LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
            [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
          Exit;
        end;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected preflight to block d2i parse when OCSP_RESPONSE_status is missing, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Missing OCSP_RESPONSE_status blocks stapled response parse and fails closed');
    finally
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestRequiredOCSPStaplingD2IMissingPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LVerifyRes: Integer;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
begin
  WriteLn;
  WriteLn('=== Required OCSP stapling preflight: missing d2i_OCSP_RESPONSE must block status path ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    try
      d2i_OCSP_RESPONSE := nil;
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@CountingOCSPResponseStatus);
      GOCSPResponseStatusCalls := 0;

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected fail-closed when d2i_OCSP_RESPONSE is unavailable');
        Exit;
      end;

      if Assigned(SSL_get_verify_result) then
      begin
        LVerifyRes := SSL_get_verify_result(LSSL);
        if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
        begin
          LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
            [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
          Exit;
        end;
      end;

      if GOCSPResponseStatusCalls <> 0 then
      begin
        LogFail(Format('Expected missing d2i preflight to block status resolver calls in required-stapling path, got status calls=%d',
          [GOCSPResponseStatusCalls]));
        Exit;
      end;

      LogPass('Missing d2i_OCSP_RESPONSE blocks required-stapling path and fails closed');
    finally
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestRequiredOCSPStaplingModuleLoadFailurePreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LVerifyRes: Integer;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedModuleLoaded: Boolean;
begin
  WriteLn;
  WriteLn('=== Required OCSP stapling preflight: module load failure must block parse ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    LSavedModuleLoaded := TOpenSSLLoader.IsModuleLoaded(osmOCSP);
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);
      GD2IOCSPResponseCalls := 0;
      GForceOCSPModuleLoadFailure := True;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected fail-closed when OCSP module loading fails');
        Exit;
      end;

      if Assigned(SSL_get_verify_result) then
      begin
        LVerifyRes := SSL_get_verify_result(LSSL);
        if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
        begin
          LogFail(Format('Expected verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
            [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
          Exit;
        end;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected module-load-failure preflight to block d2i parse, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      LogPass('Module load failure blocks stapled response parse and required stapling fails closed');
    finally
      if LSavedModuleLoaded then
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, True)
      else
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);
      GForceOCSPModuleLoadFailure := False;
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

procedure TestRequiredOCSPStaplingModuleLoadFailureRecoveryPreflight;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LOptions: TSSLOptions;
  LConnAccess: TOpenSSLConnectionAccess;
  LStream: TMemoryStream;
  LSSL: PSSL;
  LResponseDER: TBytes;
  LOpenSSLResp: Pointer;
  LSetResult: clong;
  LVerifyRes: Integer;
  LCryptoMalloc: TCRYPTO_malloc_fn;
  LSavedD2I: Td2i_OCSP_RESPONSE;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedModuleLoaded: Boolean;
begin
  WriteLn;
  WriteLn('=== Required OCSP stapling preflight: module load failure should recover on retry ===');

  LLibrary := nil;
  LContext := nil;
  LConnAccess := nil;
  LStream := nil;
  LOpenSSLResp := nil;

  try
    LoadOpenSSLCore;
    LoadOpenSSLSSL;
    LoadOpenSSLCrypto;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      LogSkip('OCSP module not available', scCapability);
      Exit;
    end;

    if not Assigned(SSL_set_tlsext_status_ocsp_resp) then
    begin
      if Assigned(SSL_ctrl) then
        LogFail('SSL_set_tlsext_status_ocsp_resp should be available via wrapper when SSL_ctrl exists')
      else
        LogSkip('SSL_set_tlsext_status_ocsp_resp unavailable', scCapability);
      Exit;
    end;

    LCryptoMalloc := TCRYPTO_malloc_fn(GetProcedureAddress(GetCryptoLibHandle, 'CRYPTO_malloc'));
    if not Assigned(LCryptoMalloc) then
    begin
      LogSkip('CRYPTO_malloc unavailable', scDependency);
      Exit;
    end;

    if not LoadSuccessfulBasicOCSPFixture(LResponseDER) then
    begin
      LogFail('Missing or empty OCSP fixture: tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der');
      Exit;
    end;

    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library instance unavailable', scDependency);
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed', scDependency);
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('CreateContext returned nil');
      Exit;
    end;

    LOptions := LContext.GetOptions;
    Include(LOptions, ssoEnableOCSPStapling);
    Include(LOptions, ssoRequireOCSPStapling);
    LContext.SetOptions(LOptions);

    LStream := TMemoryStream.Create;
    LConnAccess := TOpenSSLConnectionAccess.Create(LContext, LStream);

    LSSL := PSSL(LConnAccess.GetNativeHandle);
    if LSSL = nil then
    begin
      LogFail('Native SSL handle is nil');
      Exit;
    end;

    LOpenSSLResp := LCryptoMalloc(Length(LResponseDER), 'test_ocsp_connection_verification_regression', 0);
    if LOpenSSLResp = nil then
    begin
      LogFail('CRYPTO_malloc failed for OCSP response payload');
      Exit;
    end;

    Move(LResponseDER[0], LOpenSSLResp^, Length(LResponseDER));
    LSetResult := SSL_set_tlsext_status_ocsp_resp(LSSL, PByte(LOpenSSLResp), Length(LResponseDER));
    if LSetResult <> 1 then
    begin
      CleanupOpenSSLMemory(LOpenSSLResp);
      LOpenSSLResp := nil;
      LogFail('SSL_set_tlsext_status_ocsp_resp failed for fixture payload');
      Exit;
    end;

    LOpenSSLResp := nil; // ownership transferred

    LSavedD2I := d2i_OCSP_RESPONSE;
    LSavedResponseFree := OCSP_RESPONSE_free;
    LSavedResponseStatus := OCSP_RESPONSE_status;
    LSavedModuleLoaded := TOpenSSLLoader.IsModuleLoaded(osmOCSP);
    try
      d2i_OCSP_RESPONSE := Td2i_OCSP_RESPONSE(@CountingD2IOCSPResponse);
      OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@NoopOCSPResponseFree);
      OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);

      GD2IOCSPResponseCalls := 0;
      GForceOCSPModuleLoadFailure := True;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected first required-stapling check to fail closed on module-load failure');
        Exit;
      end;

      if Assigned(SSL_get_verify_result) then
      begin
        LVerifyRes := SSL_get_verify_result(LSSL);
        if LVerifyRes <> X509_V_ERR_OCSP_VERIFY_FAILED then
        begin
          LogFail(Format('Expected first verify_result = X509_V_ERR_OCSP_VERIFY_FAILED (%d), got %d',
            [X509_V_ERR_OCSP_VERIFY_FAILED, LVerifyRes]));
          Exit;
        end;
      end;

      if GD2IOCSPResponseCalls <> 0 then
      begin
        LogFail(Format('Expected first required-stapling attempt to block d2i parse, got d2i calls=%d',
          [GD2IOCSPResponseCalls]));
        Exit;
      end;

      GForceOCSPModuleLoadFailure := False;
      GD2IOCSPResponseCalls := 0;
      TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);

      if LConnAccess.CheckRequiredOCSPStapling(True) then
      begin
        LogFail('Expected retry required-stapling check to remain fail-closed with counting d2i stub');
        Exit;
      end;

      if not TOpenSSLLoader.IsModuleLoaded(osmOCSP) then
      begin
        LogFail('Expected retry required-stapling check to recover OCSP module loaded state, but module flag is still false');
        Exit;
      end;

      LogPass('Module-load-failure state recovers on retry for required-stapling path');
    finally
      if LSavedModuleLoaded then
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, True)
      else
        TOpenSSLLoader.SetModuleLoaded(osmOCSP, False);
      GForceOCSPModuleLoadFailure := False;
      OCSP_RESPONSE_status := LSavedResponseStatus;
      OCSP_RESPONSE_free := LSavedResponseFree;
      d2i_OCSP_RESPONSE := LSavedD2I;
    end;
  except
    on E: Exception do
      LogFail('Exception: ' + E.Message);
  end;

  CleanupOpenSSLMemory(LOpenSSLResp);
  if Assigned(LConnAccess) then
    LConnAccess.Free;
  if Assigned(LStream) then
    LStream.Free;

  if Assigned(LLibrary) then
    LLibrary.Finalize;
end;

begin
  WriteLn('OCSP Connection Verification Regression Test');
  WriteLn('============================================');

  TestOCSPLowercaseSymbolAliasLoading;
  TestOCSPStatusRequestEnablementFromContextOption;
  TestRequiredOCSPStaplingFailClosedPolicy;
  TestSuccessfulStapledOCSPFixtureMustNotVerifyWithoutPeerContext;
  TestRequiredOCSPStaplingResponseFreeMissingPreflight;
  TestRequiredOCSPStaplingResponseStatusMissingPreflight;
  TestRequiredOCSPStaplingD2IMissingPreflight;
  TestRequiredOCSPStaplingModuleLoadFailurePreflight;
  TestRequiredOCSPStaplingModuleLoadFailureRecoveryPreflight;
  TestOCSPResponseStatusResponseFreeMissingPreflight;
  TestOCSPResponseStatusStatusApiMissingPreflight;
  TestOCSPResponseStatusD2IMissingPreflight;
  TestOCSPParseFailureSemanticAlignment;
  TestEnsureOCSPModuleLoadedPointerRebindingStability;
  TestOCSPResponseStatusModuleLoadFailurePreflight;
  TestOCSPResponseStatusModuleLoadFailureRecoveryPreflight;

  WriteLn;
  WriteLn('============================================');
  WriteLn('Passed:  ', TestsPassed);
  WriteLn('Failed:  ', TestsFailed);
  WriteLn('Skipped: ', TestsSkipped);
  WriteLn(Format('Skip breakdown: dependency=%d, version=%d, environment=%d, capability=%d, other=%d',
    [SkipDependency, SkipVersion, SkipEnvironment, SkipCapability, SkipOther]));
  WriteLn('============================================');

  if TestsFailed = 0 then
    ExitCode := 0
  else
    ExitCode := 1;
end.
