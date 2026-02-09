program test_freepascal_backend_basic;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.factory,
  fafafa.ssl.base;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('❌ ', AMessage);
    Halt(1);
  end;
end;

var
  LAvailable: Boolean;
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
begin
  WriteLn('Testing FreePascal backend registration and creation...');

  LAvailable := TSSLFactory.IsLibraryAvailable(sslFreePascal);
  AssertTrue(LAvailable, 'sslFreePascal should be available');

  LLib := TSSLFactory.GetLibrary(sslFreePascal);
  AssertTrue(LLib <> nil, 'GetLibrary(sslFreePascal) should return library instance');
  AssertTrue(LLib.GetLibraryType = sslFreePascal, 'Library type mismatch');

  LCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LCtx <> nil, 'CreateContext should return context');
  AssertTrue(LCtx.GetContextType = sslCtxClient, 'Context type mismatch');

  WriteLn('✅ FreePascal backend basic checks passed');
end.
