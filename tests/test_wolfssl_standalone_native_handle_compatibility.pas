program test_wolfssl_standalone_native_handle_compatibility;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.wolfssl.connection;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LNative: ISSLNativeHandleAccess;
  LStream: TMemoryStream;
begin
  WriteLn('fafafa.ssl - WolfSSL standalone shim should preserve native handle access');

  try
    LContext := TSSLFactory.CreateContext(sslCtxClient, sslWolfSSL);
  except
    on E: Exception do
    begin
      WriteLn('[SKIP] WolfSSL unavailable: ', E.Message);
      Halt(0);
    end;
  end;

  LStream := TMemoryStream.Create;
  try
    LConnection := fafafa.ssl.wolfssl.connection.TWolfSSLConnection.Create(LContext, LStream);
    Require(LConnection <> nil, 'standalone WolfSSL connection should not be nil');
    Require(Supports(LConnection, ISSLNativeHandleAccess, LNative),
      'standalone WolfSSL connection should support ISSLNativeHandleAccess');
    Require(LNative.GetBackendType = sslWolfSSL,
      'standalone WolfSSL connection should report sslWolfSSL backend type');
    Require(LNative.IsNativeHandleValid,
      'standalone WolfSSL connection should expose a valid native handle');
    Require(LNative.GetNativeHandle <> nil,
      'standalone WolfSSL connection should expose a non-nil native handle');
  finally
    LStream.Free;
  end;

  WriteLn('[PASS] WolfSSL standalone shim preserves native handle access');
end.
