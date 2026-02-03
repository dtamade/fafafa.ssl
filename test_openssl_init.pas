program test_openssl_init;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

var
  Lib: ISSLLibrary;
begin
  WriteLn('Testing OpenSSL library initialization...');
  WriteLn;
  
  try
    // Create OpenSSL library instance
    Lib := TOpenSSLLibrary.Create;
    WriteLn('SUCCESS: TOpenSSLLibrary.Create');
    
    // Try to initialize
    WriteLn('Calling Initialize...');
    if Lib.Initialize then
    begin
      WriteLn('SUCCESS: OpenSSL library initialized');
      WriteLn('Version: ', Lib.GetVersionString);
    end
    else
    begin
      WriteLn('FAIL: Initialize returned False');
      WriteLn('Error: ', Lib.GetLastErrorString);
    end;
  except
    on E: Exception do
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
  end;
end.
