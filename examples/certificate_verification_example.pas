program certificate_verification_example;

{$mode objfpc}{$H+}

{
  证书验证示例
  演示如何加载和验证SSL/TLS证书
}

uses
  SysUtils, Classes,
  fafafa.ssl.factory,
  fafafa.ssl.base;

procedure VerifyCertificateExample;
var
  SSLLib: ISSLLibrary;
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  VerifyResult: TSSLCertVerifyResult;
begin
  WriteLn('=================================');
  WriteLn('Certificate Verification Example');
  WriteLn('=================================');
  WriteLn;
  
  // 1. 获取SSL库实例
  SSLLib := TSSLFactory.GetLibrary(sslAutoDetect);
  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize SSL library');
    Exit;
  end;
  
  WriteLn('✓ SSL Library: ', LibraryTypeToString(SSLLib.GetLibraryType));
  WriteLn('✓ Version: ', SSLLib.GetVersion);
  WriteLn;
  
  // 2. 创建证书存储并加载系统证书
  Store := SSLLib.CreateCertificateStore;
  WriteLn('Loading system certificates...');
  
  if Store.LoadSystemStore then
  begin
    WriteLn('✓ System certificates loaded');
    WriteLn('  Certificate count: ', Store.GetCount);
  end
  else
    WriteLn('✗ Failed to load system certificates');
  
  WriteLn;
  
  // 3. 加载要验证的证书
  Cert := SSLLib.CreateCertificate;
  
  // 假设证书文件存在
  if FileExists('test_cert.pem') then
  begin
    WriteLn('Loading certificate from file...');
    if Cert.LoadFromFile('test_cert.pem') then
    begin
      WriteLn('✓ Certificate loaded');
      WriteLn('  Subject: ', Cert.GetSubject);
      WriteLn('  Issuer: ', Cert.GetIssuer);
      WriteLn('  Serial: ', Cert.GetSerialNumber);
      WriteLn('  Not Before: ', DateTimeToStr(Cert.GetNotBefore));
      WriteLn('  Not After: ', DateTimeToStr(Cert.GetNotAfter));
      WriteLn('  SHA256 Fingerprint: ', Cert.GetFingerprintSHA256);
      WriteLn;
      
      // 4. 基础验证
      WriteLn('Performing basic verification...');
      if Cert.Verify(Store) then
        WriteLn('✓ Certificate is valid')
      else
        WriteLn('✗ Certificate validation failed');
      
      WriteLn;
      
      // 5. 高级验证（带详细结果）
      WriteLn('Performing advanced verification...');
      if Cert.VerifyEx(Store, [sslCertVerifyCheckTime], VerifyResult) then
      begin
        WriteLn('✓ Verification successful');
        WriteLn('  Result: ', VerifyResult.ErrorMessage);
      end
      else
      begin
        WriteLn('✗ Verification failed');
        WriteLn('  Error Code: ', VerifyResult.ErrorCode);
        WriteLn('  Error Message: ', VerifyResult.ErrorMessage);
        WriteLn('  Details: ', VerifyResult.DetailedInfo);
      end;
      
      WriteLn;
      
      // 6. 检查证书状态
      WriteLn('Certificate status:');
      WriteLn('  Is Expired: ', Cert.IsExpired);
      WriteLn('  Is Self-Signed: ', Cert.IsSelfSigned);
      WriteLn('  Is CA: ', Cert.IsCA);
      
      // 7. 主机名验证
      if Cert.VerifyHostname('example.com') then
        WriteLn('  ✓ Hostname matches')
      else
        WriteLn('  ✗ Hostname does not match');
    end
    else
      WriteLn('✗ Failed to load certificate');
  end
  else
    WriteLn('Note: test_cert.pem not found, skipping verification');
  
  WriteLn;
  WriteLn('=================================');
  WriteLn('Example completed');
  WriteLn('=================================');
end;

procedure SearchCertificatesExample;
var
  SSLLib: ISSLLibrary;
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  I: Integer;
begin
  WriteLn;
  WriteLn('=============================');
  WriteLn('Certificate Search Example');
  WriteLn('=============================');
  WriteLn;
  
  SSLLib := TSSLFactory.GetLibrary(sslAutoDetect);
  if not SSLLib.Initialize then Exit;
  
  Store := SSLLib.CreateCertificateStore;
  
  // 从路径加载证书
  if Store.LoadFromPath('/etc/ssl/certs') then
  begin
    WriteLn('✓ Certificates loaded from /etc/ssl/certs');
    WriteLn('  Total certificates: ', Store.GetCount);
    WriteLn;
    
    // 搜索示例
    WriteLn('Searching for certificates...');
    
    // 按主题搜索
    Cert := Store.FindBySubject('DigiCert');
    if Cert <> nil then
      WriteLn('  ✓ Found by subject: ', Cert.GetSubject)
    else
      WriteLn('  - No certificate found with subject containing "DigiCert"');
    
    // 枚举前3个证书
    WriteLn;
    WriteLn('First 3 certificates:');
    for I := 0 to Min(2, Store.GetCount - 1) do
    begin
      Cert := Store.GetCertificate(I);
      if Cert <> nil then
      begin
        WriteLn('  [', I + 1, '] ', Cert.GetSubject);
        WriteLn('      Issuer: ', Cert.GetIssuer);
      end;
    end;
  end
  else
    WriteLn('Note: Could not load certificates from /etc/ssl/certs');
  
  WriteLn;
  WriteLn('=============================');
end;

begin
  try
    VerifyCertificateExample;
    SearchCertificatesExample;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.

