program test_facade_capability_native_handle_entry;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl;

var
  Caps: TSSLBackendCapabilities;
  Support: TSSLFeatureSupportLevel;
  Impl: TSSLBackendImplType;
  Native: ISSLNativeHandleAccess;
  Desc: string;
begin
  Native := nil;
  Caps := Default(TSSLBackendCapabilities);
  Caps.BackendType := sslOpenSSL;
  Caps.BackendImplType := sslImplCLibrary;
  Caps.RequiresExternalLibrary := True;
  Caps.SNISupport := sslSupportStable;
  Caps.ALPNSupport := sslSupportExperimental;
  Caps.SupportedCiphers := [sslCipherAES256GCM];
  Caps.SupportedHashes := [sslHashSHA256];
  Caps.SupportedKeyExchanges := [sslKexECDHE_RSA];

  NormalizeLegacyCapabilityBooleans(Caps);

  Support := Caps.SNISupport;
  Impl := Caps.BackendImplType;
  if not IsFeatureStable(Support) then
    Halt(1);
  if not IsFeatureUsable(Caps.ALPNSupport) then
    Halt(2);
  if IsFeatureDeprecated(Caps.ALPNSupport) then
    Halt(3);
  if not IsCLibraryBackend(Caps) then
    Halt(4);
  if not RequiresExternalDependencies(Caps) then
    Halt(5);
  if not IsCipherSupported(Caps, sslCipherAES256GCM) then
    Halt(6);
  if not IsHashSupported(Caps, sslHashSHA256) then
    Halt(7);
  if not IsKeyExchangeSupported(Caps, sslKexECDHE_RSA) then
    Halt(8);

  Desc := GetCapabilitiesDescription(Caps);
  if LibraryTypeToString(sslOpenSSL) = '' then
    Halt(9);
  if GetSecurityScore(Caps) < 0 then
    Halt(10);
  if GetPerformanceScore(Caps) < 0 then
    Halt(11);

  Caps.BackendImplType := sslImplNative;
  Impl := Caps.BackendImplType;
  if (Impl <> sslImplNative) or (not IsNativeBackend(Caps)) then
    Halt(12);

  if Assigned(Native) and (Desc = '') then
    Halt(13);
end.
