program test_tls_method;
{$mode objfpc}{$H+}
uses
  SysUtils,
  fafafa.ssl.openssl.lib,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.types;
var
  Lib: ISSLLibrary;
  Method: PSSL_METHOD;
begin
  Lib := CreateOpenSSLLibrary;
  Lib.Initialize;
  
  WriteLn('TLS_client_method assigned: ', Assigned(TLS_client_method));
  WriteLn('TLS_server_method assigned: ', Assigned(TLS_server_method));
  WriteLn('SSLv23_client_method assigned: ', Assigned(SSLv23_client_method));
  
  if Assigned(TLS_client_method) then
  begin
    WriteLn('Calling TLS_client_method...');
    Method := TLS_client_method();
    WriteLn('Result: ', PtrUInt(Method));
  end;
end.
