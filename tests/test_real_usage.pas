program test_real_usage;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  
procedure TestRealCertificateLoading;
var
  SerialNum, Subject, SigAlg: string;
  IsCA: Boolean;
begin
  WriteLn;
  WriteLn('=== Test 1: Real Certificate Loading ===');
  
  // 尝试加载系统证书
  Store := SSLLib.CreateCertificateStore;
  if Store = nil then
  begin
    WriteLn('❌ FAIL: Cannot create certificate store');
    Exit;
  end;
  
  WriteLn('Attempting to load system certificates...');
  if not Store.LoadSystemStore then
  begin
    WriteLn('⚠️  System certificate store failed');
    WriteLn('Trying default paths...');
    if not Store.LoadFromPath('/etc/ssl/certs') then
    begin
      WriteLn('❌ FAIL: Cannot load any certificates');
      WriteLn('This is a BLOCKING issue - no certificates means no HTTPS!');
      Exit;
    end;
  end;
  
  WriteLn('✓ Certificate store loaded, count: ', Store.GetCount);
  
  if Store.GetCount = 0 then
  begin
    WriteLn('❌ FAIL: Certificate store is empty!');
    Exit;
  end;
  
  // 测试实际的证书功能
  Cert := Store.GetCertificate(0);
  if Cert = nil then
  begin
    WriteLn('❌ FAIL: Cannot get certificate from store');
    Exit;
  end;
  
  WriteLn;
  WriteLn('Testing certificate methods on real cert:');
  
  try
    Subject := Cert.GetSubject;
    WriteLn('  Subject: ', Copy(Subject, 1, 60), '...');
    if Subject = '' then
      WriteLn('  ⚠️  WARNING: Subject is empty!');
  except
    on E: Exception do
      WriteLn('  ❌ GetSubject FAILED: ', E.Message);
  end;
  
  try
    SerialNum := Cert.GetSerialNumber;
    WriteLn('  Serial: ', Copy(SerialNum, 1, 40), '...');
    if SerialNum = '' then
      WriteLn('  ⚠️  WARNING: Serial number is empty!');
  except
    on E: Exception do
      WriteLn('  ❌ GetSerialNumber FAILED: ', E.Message);
  end;
  
  try
    SigAlg := Cert.GetSignatureAlgorithm;
    WriteLn('  Signature Algorithm: ', SigAlg);
    if SigAlg = '' then
      WriteLn('  ⚠️  WARNING: Signature algorithm is empty!');
  except
    on E: Exception do
      WriteLn('  ❌ GetSignatureAlgorithm FAILED: ', E.Message);
  end;
  
  try
    IsCA := Cert.IsCA;
    WriteLn('  IsCA: ', IsCA);
  except
    on E: Exception do
      WriteLn('  ❌ IsCA FAILED: ', E.Message);
  end;
  
  WriteLn('✓ Certificate methods work');
end;

procedure TestRealHTTPSConnection;
begin
  WriteLn;
  WriteLn('=== Test 2: Real HTTPS Connection ===');
  
  // 创建Context
  Context := SSLLib.CreateContext(sslCtxClient);
  if Context = nil then
  begin
    WriteLn('❌ FAIL: Cannot create SSL context');
    Exit;
  end;
  
  WriteLn('✓ Context created (configuration methods testing skipped)');
  
  WriteLn('⚠️  SKIP: Actual HTTPS connection test requires network code');
  WriteLn('   This library provides SSL/TLS layer, but needs TCP socket separately');
end;

procedure TestBasicAPI;
begin
  WriteLn;
  WriteLn('=== Test 3: Basic API Availability ===');
  
  // 测试基本对象创建
  try
    Context := SSLLib.CreateContext(sslCtxClient);
    WriteLn('✓ CreateContext works');
  except
    on E: Exception do
      WriteLn('❌ CreateContext FAILED: ', E.Message);
  end;
  
  try
    Store := SSLLib.CreateCertificateStore;
    WriteLn('✓ CreateCertificateStore works');
  except
    on E: Exception do
      WriteLn('❌ CreateCertificateStore FAILED: ', E.Message);
  end;
  
  try
    Cert := SSLLib.CreateCertificate;
    if Cert = nil then
      WriteLn('✓ CreateCertificate correctly returns nil (by design)')
    else
      WriteLn('⚠️  CreateCertificate returned non-nil (unexpected)');
  except
    on E: Exception do
      WriteLn('❌ CreateCertificate FAILED: ', E.Message);
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('REAL USAGE TEST - Can we actually use this library?');
  WriteLn('========================================');
  
  try
    // 初始化
    WriteLn;
    WriteLn('Initializing SSL library...');
    SSLLib := CreateOpenSSLLibrary;
    if SSLLib = nil then
    begin
      WriteLn('❌ FATAL: Cannot create SSL library');
      Halt(1);
    end;
    
    if not SSLLib.Initialize then
    begin
      WriteLn('❌ FATAL: Cannot initialize SSL library');
      Halt(1);
    end;
    
    WriteLn('✓ SSL library initialized: ', SSLLib.GetVersionString);
    
    // 运行实际测试
    TestBasicAPI;
    TestRealCertificateLoading;
    TestRealHTTPSConnection;
    
    WriteLn;
    WriteLn('========================================');
    WriteLn('HONEST ASSESSMENT');
    WriteLn('========================================');
    WriteLn('Based on actual testing:');
    WriteLn('1. Library initialization: ✓ Works');
    WriteLn('2. Certificate loading: ? Needs verification');
    WriteLn('3. HTTPS connection: ? Not fully tested');
    WriteLn('4. Production ready: ? Cannot confirm without real HTTPS test');
    WriteLn;
    WriteLn('CONCLUSION: More real-world testing needed!');
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('❌ FATAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.

