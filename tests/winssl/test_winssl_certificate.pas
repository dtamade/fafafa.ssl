program test_winssl_certificate;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

uses
  SysUtils, Classes, Windows,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  
  fafafa.ssl.base;

var
  TestsPassed, TestsFailed: Integer;

procedure WriteTest(const TestName: string);
begin
  Write('  [', TestName, '] ... ');
end;

procedure WritePass;
begin
  WriteLn('[PASS]');
  Inc(TestsPassed);
end;

procedure WriteFail(const Reason: string = '');
begin
  if Reason <> '' then
    WriteLn('[FAIL] - ', Reason)
  else
    WriteLn('[FAIL]');
  Inc(TestsFailed);
end;

procedure TestAPIFunctionsAvailable;
var
  Store: ISSLCertificateStore;
  Count: Integer;
begin
  WriteLn('=== 测试 1: 证书存储访问 ===');
  WriteLn;

  WriteTest('打开 ROOT 系统存储');
  try
    Store := OpenSystemStore(SSL_STORE_ROOT);
    if Store <> nil then
      WritePass
    else
      WriteFail('存储为 nil');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteTest('获取证书计数');
  try
    if Store <> nil then
    begin
      Count := Store.GetCount;
      if Count > 0 then
        WritePass
      else
        WriteFail(Format('计数为 %d', [Count]));
    end
    else
      WriteLn('[SKIP] - 存储未打开');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteLn;
end;

procedure TestCertAPIFunctionsAvailable;
var
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  Subject: string;
begin
  WriteLn('=== 测试 2: 证书枚举 ===');
  WriteLn;

  WriteTest('枚举 ROOT 存储中的证书');
  try
    Store := OpenSystemStore(SSL_STORE_ROOT);
    if (Store <> nil) and (Store.GetCount > 0) then
    begin
      Cert := Store.GetCertificate(0);
      if Cert <> nil then
        WritePass
      else
        WriteFail('获取证书失败');
    end
    else
      WriteFail('ROOT 存储为空');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteTest('读取第一个证书的主题');
  try
    if Cert <> nil then
    begin
      Subject := Cert.GetSubject;
      if Subject <> '' then
        WritePass
      else
        WriteFail('主题为空');
    end
    else
      WriteLn('[SKIP] - 无证书');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteTest('检查证书是否为 CA');
  try
    if Cert <> nil then
    begin
      if Cert.IsCA then
        WritePass
      else
        WriteFail('ROOT 证书应该是 CA');
    end
    else
      WriteLn('[SKIP] - 无证书');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteLn;
end;

procedure TestTypeSizes;
var
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  FP_SHA1, FP_SHA256: string;
begin
  WriteLn('=== 测试 3: 证书指纹 ===');
  WriteLn;

  WriteTest('计算 SHA-1 指纹');
  try
    Store := OpenSystemStore(SSL_STORE_ROOT);
    if (Store <> nil) and (Store.GetCount > 0) then
    begin
      Cert := Store.GetCertificate(0);
      if Cert <> nil then
      begin
        FP_SHA1 := Cert.GetFingerprintSHA1;
        if (FP_SHA1 <> '') and (Length(FP_SHA1) > 20) then
          WritePass
        else
          WriteFail('SHA-1 指纹无效');
      end
      else
        WriteFail('获取证书失败');
    end
    else
      WriteLn('[SKIP] - 无证书');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteTest('计算 SHA-256 指纹');
  try
    if Cert <> nil then
    begin
      FP_SHA256 := Cert.GetFingerprintSHA256;
      if (FP_SHA256 <> '') and (Length(FP_SHA256) > 40) then
        WritePass
      else
        WriteFail('SHA-256 指纹无效');
    end
    else
      WriteLn('[SKIP] - 无证书');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteLn;
end;

procedure TestConstantValues;
var
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  KeyUsage: TStringList;
begin
  WriteLn('=== 测试 4: 证书扩展 ===');
  WriteLn;

  WriteTest('读取 Key Usage 扩展');
  try
    Store := OpenSystemStore(SSL_STORE_ROOT);
    if (Store <> nil) and (Store.GetCount > 0) then
    begin
      Cert := Store.GetCertificate(0);
      if Cert <> nil then
      begin
        KeyUsage := Cert.GetKeyUsage;
        try
          if KeyUsage <> nil then
            WritePass
          else
            WriteFail('KeyUsage 为 nil');
        finally
          KeyUsage.Free;
        end;
      end
      else
        WriteFail('获取证书失败');
    end
    else
      WriteLn('[SKIP] - 无证书');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;

  WriteLn;
end;

procedure TestBasicCredentialAcquisition;
begin
  // No additional test needed for now
end;

procedure PrintSummary;
var
  Total: Integer;
begin
  Total := TestsPassed + TestsFailed;
  WriteLn('==============================================');
  WriteLn('测试摘要:');
  WriteLn('  总计: ', Total);
  WriteLn('  通过: ', TestsPassed, ' (', FormatFloat('0.0', TestsPassed / Total * 100), '%)');
  WriteLn('  失败: ', TestsFailed);
  
  if TestsFailed = 0 then
  begin
    WriteLn;
    WriteLn('✅ 所有测试通过！WinSSL 证书功能正常工作。');
  end
  else
  begin
    WriteLn;
    WriteLn('⚠️ 部分测试失败，需要检查证书功能。');
  end;
  WriteLn('==============================================');
end;

begin
  TestsPassed := 0;
  TestsFailed := 0;

  WriteLn('');
  WriteLn('==============================================');
  WriteLn('  fafafa.ssl - WinSSL 证书功能测试');
  WriteLn('==============================================');
  WriteLn('');
  WriteLn('测试环境:');
  WriteLn('  操作系统: Windows ', GetVersion shr 16, '.', GetVersion and $FFFF);
  WriteLn('  编译器: Free Pascal ', {$I %FPCVERSION%});
  WriteLn('');

  try
    TestAPIFunctionsAvailable;
    TestCertAPIFunctionsAvailable;
    TestTypeSizes;
    TestConstantValues;
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('!!! 严重错误: ', E.Message);
      Inc(TestsFailed);
    end;
  end;
  
  WriteLn('');
  PrintSummary;
  WriteLn('');
  
  // 等待按键
  WriteLn('按回车键退出...');
  ReadLn;
end.
