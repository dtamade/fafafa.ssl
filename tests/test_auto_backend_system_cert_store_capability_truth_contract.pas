program test_auto_backend_system_cert_store_capability_truth_contract;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.backend.selector,
  fafafa.ssl.context.builder,
  fafafa.ssl.freepascal.lib
  {$IFDEF UNIX}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.openssl.base
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.mbedtls.base
  , fafafa.ssl.wolfssl.lib
  , fafafa.ssl.wolfssl.base
  {$ENDIF}
  {$IFDEF WINDOWS}
  , fafafa.ssl.openssl.backed
  , fafafa.ssl.winssl.lib
  , fafafa.ssl.mbedtls.lib
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  ;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

function AnyAvailableBackendPublishesSystemCertStore: Boolean;
var
  LType: TSSLLibraryType;
  LLib: ISSLLibrary;
begin
  Result := False;

  for LType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if LType = sslAutoDetect then
      Continue;
    if not TSSLFactory.IsLibraryAvailable(LType) then
      Continue;

    LLib := TSSLFactory.GetLibrary(LType);
    if (LLib <> nil) and LLib.GetCapabilities.SupportsSystemCertStore then
      Exit(True);
  end;
end;

var
  LRequirements: TSSLRequirements;
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LExpectedAvailable: Boolean;
  LSelectedType: TSSLLibraryType;
  LMatchScore: Integer;
  LSelectedLib: ISSLLibrary;
begin
  WriteLn('Testing Auto-Backend System-Cert-Store Capability Truth Contract');
  WriteLn('================================================================');

  LExpectedAvailable := AnyAvailableBackendPublishesSystemCertStore;

  LRequirements := CreateDefaultRequirements(optBalanced);
  LRequirements.MinSecurityScore := 0;
  LRequirements.MinPerformanceScore := 0;
  LRequirements.MinCompatibilityLevel := 0;
  LRequirements.PlatformPreferences.RequireSystemCertStore := True;

  if LExpectedAvailable then
  begin
    Require(SelectBestBackend(LRequirements, LSelectedType, LMatchScore),
      'Selector must succeed when at least one available backend publishes system-cert-store support');
    LSelectedLib := TSSLFactory.GetLibrary(LSelectedType);
    Require((LSelectedLib <> nil) and LSelectedLib.GetCapabilities.SupportsSystemCertStore,
      'Selector must only return a backend that publishes system-cert-store support');
  end
  else
  begin
    Require(not SelectBestBackend(LRequirements, LSelectedType, LMatchScore),
      'Selector must fail when no available backend publishes system-cert-store support');
  end;

  LBuilder := TSSLContextBuilder.Create.WithAutoBackendSelection(LRequirements);
  LResult := LBuilder.TryBuildClient(LContext);

  if LExpectedAvailable then
  begin
    Require(LResult.IsOk,
      'Auto-backend builder must succeed when at least one available backend publishes system-cert-store support');
    Require(LContext <> nil,
      'Context must be created when a published system-cert-store-capable backend exists');
  end
  else
  begin
    Require(LResult.IsErr,
      'Auto-backend builder must fail when no available backend publishes system-cert-store support');
    Require(LContext = nil,
      'Context must remain nil when no published system-cert-store-capable backend exists');
    Require(Pos('No suitable SSL backend found for requirements', LResult.ErrorMessage) > 0,
      'Error should mention that no suitable backend satisfies the system-cert-store requirement');
  end;

  WriteLn('✅ Auto-backend system-cert-store capability truth contract verified');
end.
