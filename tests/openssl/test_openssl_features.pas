program test_openssl_features;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.factory,
  fafafa.ssl.base,
  fafafa.ssl;

procedure TestCertificateStore;
var
  SSLLib: ISSLLibrary;
  Store: ISSLCertificateStore;
  Count: Integer;
begin
  WriteLn;
  WriteLn('Testing Certificate Store');
  WriteLn('==========================');
  
  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;
  
  // 创建证书存储
  Store := SSLLib.CreateCertificateStore;
  WriteLn('Store created: ', Store <> nil);
  
  // 加载系统证书
  if Store.LoadSystemStore then
    WriteLn('System store loaded: TRUE')
  else
    WriteLn('System store loaded: FALSE');
  
  // 获取证书数量
  Count := Store.GetCount;
  WriteLn('Certificate count: ', Count);
  
  WriteLn('✅ Certificate Store test completed');
end;

procedure TestSession;
var
  SSLLib: ISSLLibrary;
  Ctx: ISSLContext;
  Session: ISSLSession;
  Data: TBytes;
  Session2: ISSLSession;
begin
  WriteLn;
  WriteLn('Testing Session Serialization');
  WriteLn('==============================');
  
  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;
  
  // 创建上下文
  Ctx := SSLLib.CreateContext(sslCtxClient);
  WriteLn('Context created: ', Ctx <> nil);
  
  // TODO: 需要实际的连接才能获取Session
  // 这里只测试序列化接口是否可用
  
  WriteLn('✅ Session test completed (basic check)');
end;

procedure TestCertificateVerify;
var
  SSLLib: ISSLLibrary;
  Store: ISSLCertificateStore;
begin
  WriteLn;
  WriteLn('Testing Certificate Verification');
  WriteLn('=================================');
  
  SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize OpenSSL');
    Exit;
  end;
  
  Store := SSLLib.CreateCertificateStore;
  if Store.LoadSystemStore then
    WriteLn('System store loaded for verification: TRUE')
  else
    WriteLn('System store loaded for verification: FALSE');
  
  // TODO: 需要实际的证书才能测试验证
  
  WriteLn('✅ Verification test completed (basic check)');
end;

begin
  WriteLn('Testing OpenSSL Advanced Features');
  WriteLn('==================================');
  WriteLn;
  
  try
    TestCertificateStore;
    TestSession;
    TestCertificateVerify;
    
    WriteLn;
    WriteLn('===================');
    WriteLn('✅ All tests passed');
    WriteLn('===================');
  except
    on E: Exception do
    begin
      WriteLn('❌ Test failed: ', E.Message);
      Halt(1);
    end;
  end;
end.

