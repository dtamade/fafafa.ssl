program test_openssl_detailed;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

var
  Lib: TOpenSSLLibrary;
begin
  WriteLn('Testing OpenSSL library initialization with detailed logging...');
  WriteLn;
  
  try
    // Create OpenSSL library instance directly (not through interface)
    Lib := TOpenSSLLibrary.Create;
    WriteLn('SUCCESS: TOpenSSLLibrary.Create');
    
    // Set log level to info to see all messages
    Lib.SetLogLevel(sslLogInfo);
    Lib.SetLogCallback(@WriteLn);
    
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
      WriteLn('Last Error: ', Lib.GetLastErrorString);
    end;
    
    Lib.Free;
  except
    on E: Exception do
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
  end;
end.
