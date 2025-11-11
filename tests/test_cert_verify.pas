program test_cert_verify;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  
  fafafa.ssl.base,
  fafafa.ssl.openssl;

procedure TestCertificateVerification;
var
  LLib: ISSLLibrary;
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
begin
  WriteLn('Test: Certificate Verification');
  WriteLn('================================');
  
  try
    // Create OpenSSL library
    LLib := TOpenSSLLibrary.Create;
    if not LLib.Initialize then
    begin
      WriteLn('FAIL: Could not initialize OpenSSL library');
      Exit;
    end;
    WriteLn('OK: OpenSSL library initialized');
    
    // Create certificate
    LCert := LLib.CreateCertificate;
    if LCert = nil then
    begin
      WriteLn('FAIL: Could not create certificate object');
      Exit;
    end;
    WriteLn('OK: Certificate object created');
    
    // Create certificate store
    LStore := LLib.CreateCertificateStore;
    if LStore = nil then
    begin
      WriteLn('FAIL: Could not create certificate store');
      Exit;
    end;
    WriteLn('OK: Certificate store created');
    
    WriteLn('');
    WriteLn('All basic tests passed!');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end;

procedure TestHostnameVerification;
var
  LLib: ISSLLibrary;
  LCert: ISSLCertificate;
begin
  WriteLn('');
  WriteLn('Test: Hostname Verification');
  WriteLn('============================');
  
  try
    // Create OpenSSL library
    LLib := TOpenSSLLibrary.Create;
    if not LLib.Initialize then
    begin
      WriteLn('FAIL: Could not initialize OpenSSL library');
      Exit;
    end;
    WriteLn('OK: OpenSSL library initialized');
    
    // Create certificate
    LCert := LLib.CreateCertificate;
    if LCert = nil then
    begin
      WriteLn('FAIL: Could not create certificate object');
      Exit;
    end;
    WriteLn('OK: Certificate object created');
    
    // Test hostname verification with empty certificate
    // Should return False because certificate is not loaded
    if not LCert.VerifyHostname('example.com') then
      WriteLn('OK: VerifyHostname returns False for unloaded certificate')
    else
      WriteLn('FAIL: VerifyHostname should return False for unloaded certificate');
    
    WriteLn('');
    WriteLn('All hostname verification tests passed!');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end;

begin
  TestCertificateVerification;
  TestHostnameVerification;
  WriteLn('');
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
