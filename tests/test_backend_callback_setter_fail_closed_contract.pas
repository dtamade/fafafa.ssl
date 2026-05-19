program test_backend_callback_setter_fail_closed_contract;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.lib
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.winssl.lib
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  ;

type
  TCallbackKind = (ckVerify, ckPassword, ckInfo);

  TCallbackProbe = class
  public
    function VerifyCallback(const ACertificate: TSSLCertificateInfo;
      const AErrorCode: Integer; const AErrorMessage: string): Boolean;
    function PasswordCallback(var APassword: string; const AIsRetry: Boolean): Boolean;
    procedure InfoCallback(const AWhere: Integer; const ARet: Integer; const AState: string);
  end;

function TCallbackProbe.VerifyCallback(const ACertificate: TSSLCertificateInfo;
  const AErrorCode: Integer; const AErrorMessage: string): Boolean;
begin
  Result := True;
end;

function TCallbackProbe.PasswordCallback(var APassword: string;
  const AIsRetry: Boolean): Boolean;
begin
  APassword := '';
  Result := True;
end;

procedure TCallbackProbe.InfoCallback(const AWhere: Integer;
  const ARet: Integer; const AState: string);
begin
end;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

function CallbackKindName(AKind: TCallbackKind): string;
begin
  case AKind of
    ckVerify: Result := 'Verify callback';
    ckPassword: Result := 'Password callback';
    ckInfo: Result := 'Info callback';
  end;
end;

procedure AssignNonNilCallback(ACtx: ISSLContext; AKind: TCallbackKind; AProbe: TCallbackProbe);
begin
  case AKind of
    ckVerify: ACtx.SetVerifyCallback(@AProbe.VerifyCallback);
    ckPassword: ACtx.SetPasswordCallback(@AProbe.PasswordCallback);
    ckInfo: ACtx.SetInfoCallback(@AProbe.InfoCallback);
  end;
end;

procedure ClearCallback(ACtx: ISSLContext; AKind: TCallbackKind);
begin
  case AKind of
    ckVerify: ACtx.SetVerifyCallback(nil);
    ckPassword: ACtx.SetPasswordCallback(nil);
    ckInfo: ACtx.SetInfoCallback(nil);
  end;
end;

procedure CheckPublishedBackend(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LProbe: TCallbackProbe;
  LKind: TCallbackKind;
begin
  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ABackend], ' backend not available on this platform');
    Exit;
  end;

  LLib := TSSLFactory.GetLibrary(ABackend);
  Require(LLib <> nil, SSL_LIBRARY_NAMES[ABackend] + ' library should be creatable when available');
  Require(LLib.GetCapabilities.SupportsCallbacks,
    SSL_LIBRARY_NAMES[ABackend] + ' must publish SupportsCallbacks=True for this contract');

  LCtx := LLib.CreateContext(sslCtxClient);
  Require(LCtx <> nil, SSL_LIBRARY_NAMES[ABackend] + ' context should be creatable');
  LProbe := TCallbackProbe.Create;
  try
    for LKind := Low(TCallbackKind) to High(TCallbackKind) do
    begin
      try
        AssignNonNilCallback(LCtx, LKind, LProbe);
      except
        on E: Exception do
          raise Exception.CreateFmt('%s should accept non-nil %s when SupportsCallbacks=True: %s',
            [SSL_LIBRARY_NAMES[ABackend], CallbackKindName(LKind), E.Message]);
      end;

      try
        ClearCallback(LCtx, LKind);
      except
        on E: Exception do
          raise Exception.CreateFmt('%s should accept nil clear for %s when SupportsCallbacks=True: %s',
            [SSL_LIBRARY_NAMES[ABackend], CallbackKindName(LKind), E.Message]);
      end;
    end;
  finally
    LProbe.Free;
  end;

  WriteLn('[PASS] ', SSL_LIBRARY_NAMES[ABackend],
    ' published callback setters accept non-nil assignments and nil clears');
end;

procedure CheckUnpublishedBackend(ABackend: TSSLLibraryType);
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LProbe: TCallbackProbe;
  LKind: TCallbackKind;
  LRejected: Boolean;
  LLowerMsg: string;
begin
  if not TSSLFactory.IsLibraryAvailable(ABackend) then
  begin
    WriteLn('[SKIP] ', SSL_LIBRARY_NAMES[ABackend], ' backend not available on this platform');
    Exit;
  end;

  LLib := TSSLFactory.GetLibrary(ABackend);
  Require(LLib <> nil, SSL_LIBRARY_NAMES[ABackend] + ' library should be creatable when available');
  Require(not LLib.GetCapabilities.SupportsCallbacks,
    SSL_LIBRARY_NAMES[ABackend] + ' must publish SupportsCallbacks=False for this contract');

  LCtx := LLib.CreateContext(sslCtxClient);
  Require(LCtx <> nil, SSL_LIBRARY_NAMES[ABackend] + ' context should be creatable');
  LProbe := TCallbackProbe.Create;
  try
    for LKind := Low(TCallbackKind) to High(TCallbackKind) do
    begin
      LRejected := False;
      try
        AssignNonNilCallback(LCtx, LKind, LProbe);
      except
        on E: ESSLException do
        begin
          LLowerMsg := LowerCase(E.Message);
          Require((E.ErrorCode = sslErrUnsupported) or (Pos('unsupported', LLowerMsg) > 0) or
            (Pos('不支持', E.Message) > 0),
            Format('%s non-nil %s rejection must report unsupported semantics: %s',
              [SSL_LIBRARY_NAMES[ABackend], CallbackKindName(LKind), E.Message]));
          LRejected := True;
        end;
      end;

      Require(LRejected,
        Format('%s must reject non-nil %s while SupportsCallbacks=False',
          [SSL_LIBRARY_NAMES[ABackend], CallbackKindName(LKind)]));

      try
        ClearCallback(LCtx, LKind);
      except
        on E: Exception do
          raise Exception.CreateFmt('%s should accept nil clear for %s while SupportsCallbacks=False: %s',
            [SSL_LIBRARY_NAMES[ABackend], CallbackKindName(LKind), E.Message]);
      end;
    end;
  finally
    LProbe.Free;
  end;

  WriteLn('[PASS] ', SSL_LIBRARY_NAMES[ABackend],
    ' unpublished callback setters fail-closed on non-nil assignments and accept nil clears');
end;

begin
  WriteLn('Testing backend callback setter fail-closed contract');
  WriteLn('====================================================');

  CheckPublishedBackend(sslOpenSSL);
  CheckPublishedBackend(sslWinSSL);
  CheckUnpublishedBackend(sslFreePascal);
  CheckUnpublishedBackend(sslWolfSSL);
  CheckUnpublishedBackend(sslMbedTLS);

  WriteLn('====================================================');
  WriteLn('✅ backend callback setter fail-closed contract verified');
end.
