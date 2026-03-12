program test_openssl_lib_unit_compatibility;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.openssl.lib;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
  WriteLn('[PASS] ', AMessage);
end;

var
  LLib: ISSLLibrary;
  LConcrete: TOpenSSLLibrary;
  LPaths: TOpenSSLLibraryPaths;
begin
  ClearCustomLibraryPaths;
  Require(not IsUsingCustomPaths, 'custom path mode starts disabled');

  SetCustomLibraryPaths('/tmp/libcrypto.so', '/tmp/libssl.so');
  Require(IsUsingCustomPaths, 'custom path mode enables through canonical unit');

  LPaths := GetCustomLibraryPaths;
  Require(LPaths.CryptoLibPath = '/tmp/libcrypto.so', 'crypto path is forwarded');
  Require(LPaths.SSLLibPath = '/tmp/libssl.so', 'ssl path is forwarded');

  LLib := CreateOpenSSLLibrary;
  Require(LLib <> nil, 'factory function returns library');

  LConcrete := TOpenSSLLibrary.Create;
  try
    Require(LConcrete.GetLibraryType = sslOpenSSL, 'class alias exposes OpenSSL library type');
  finally
    LConcrete.Free;
  end;

  ClearCustomLibraryPaths;
end.
