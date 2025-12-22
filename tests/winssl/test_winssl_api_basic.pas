program test_winssl_api_basic;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

uses
  SysUtils, Windows,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api;

var
  MyCredHandle: CredHandle;
  SchannelCred: SCHANNEL_CRED;
  MyTimeStamp: TimeStamp;
  Status: SECURITY_STATUS;
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
begin
  WriteLn('=== 测试 1: Schannel API 函数可用性 ===');
  WriteLn;
  
  WriteTest('AcquireCredentialsHandleW 可用');
  if Assigned(@AcquireCredentialsHandleW) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('FreeCredentialsHandle 可用');
  if Assigned(@FreeCredentialsHandle) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('InitializeSecurityContextW 可用');
  if Assigned(@InitializeSecurityContextW) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('AcceptSecurityContextW 可用');
  if Assigned(@AcceptSecurityContextW) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('DeleteSecurityContext 可用');
  if Assigned(@DeleteSecurityContext) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('QueryContextAttributesW 可用');
  if Assigned(@QueryContextAttributesW) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('EncryptMessage 可用');
  if Assigned(@EncryptMessage) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('DecryptMessage 可用');
  if Assigned(@DecryptMessage) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteLn;
end;

procedure TestCertAPIFunctionsAvailable;
begin
  WriteLn('=== 测试 2: Windows 证书 API 函数可用性 ===');
  WriteLn;
  
  WriteTest('CertOpenStore 可用');
  if Assigned(@CertOpenStore) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('CertOpenSystemStoreW 可用');
  if Assigned(@CertOpenSystemStoreW) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('CertCloseStore 可用');
  if Assigned(@CertCloseStore) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('CertFindCertificateInStore 可用');
  if Assigned(@CertFindCertificateInStore) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteTest('CertEnumCertificatesInStore 可用');
  if Assigned(@CertEnumCertificatesInStore) then
    WritePass
  else
    WriteFail('函数未找到');
    
  WriteLn;
end;

procedure TestTypeSizes;
begin
  WriteLn('=== 测试 3: 类型大小验证 ===');
  WriteLn;
  
  WriteTest('TSecHandle 大小');
  if SizeOf(TSecHandle) = 16 then
    WritePass
  else
    WriteFail(Format('期望 16 字节, 实际 %d 字节', [SizeOf(TSecHandle)]));
    
  WriteTest('CredHandle 大小');
  if SizeOf(TSecHandle) = 16 then
    WritePass
  else
    WriteFail(Format('期望 16 字节, 实际 %d 字节', [SizeOf(TSecHandle)]));
    
  WriteTest('TSecBuffer 大小');
  if SizeOf(TSecBuffer) >= 12 then  // 至少 12 字节 (ULONG + ULONG + Pointer)
    WritePass
  else
    WriteFail(Format('期望至少 12 字节, 实际 %d 字节', [SizeOf(TSecBuffer)]));
    
  WriteTest('SCHANNEL_CRED 大小');
  if SizeOf(SCHANNEL_CRED) > 0 then
    WritePass
  else
    WriteFail('大小为 0');
    
  WriteLn;
end;

procedure TestConstantValues;
begin
  WriteLn('=== 测试 4: 常量值验证 ===');
  WriteLn;
  
  WriteTest('SCHANNEL_CRED_VERSION');
  if SCHANNEL_CRED_VERSION = 4 then
    WritePass
  else
    WriteFail(Format('期望 4, 实际 %d', [SCHANNEL_CRED_VERSION]));
    
  WriteTest('SECPKG_CRED_OUTBOUND');
  if SECPKG_CRED_OUTBOUND = $00000002 then
    WritePass
  else
    WriteFail(Format('期望 $00000002, 实际 $%.8x', [SECPKG_CRED_OUTBOUND]));
    
  WriteTest('SECBUFFER_VERSION');
  if SECBUFFER_VERSION = 0 then
    WritePass
  else
    WriteFail(Format('期望 0, 实际 %d', [SECBUFFER_VERSION]));
    
  WriteTest('SECBUFFER_DATA');
  if SECBUFFER_DATA = 1 then
    WritePass
  else
    WriteFail(Format('期望 1, 实际 %d', [SECBUFFER_DATA]));
    
  WriteLn;
end;

procedure TestBasicCredentialAcquisition;
const
  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';
begin
  WriteLn('=== 测试 5: 基本凭据获取 ===');
  WriteLn;
  
  WriteTest('初始化 SCHANNEL_CRED');
  try
    FillChar(SchannelCred, SizeOf(SchannelCred), 0);
    SchannelCred.dwVersion := SCHANNEL_CRED_VERSION;
    SchannelCred.grbitEnabledProtocols := 0;  // 使用系统默认
    SchannelCred.dwFlags := $00000004 or $00000080;  // SCH_CRED_MANUAL_CRED_VALIDATION | SCH_CRED_NO_DEFAULT_CREDS
    WritePass;
  except
    on E: Exception do
      WriteFail(E.Message);
  end;
  
  WriteTest('AcquireCredentialsHandleW (客户端)');
  try
    FillChar(MyCredHandle, SizeOf(MyCredHandle), 0);
    FillChar(MyTimeStamp, SizeOf(MyTimeStamp), 0);
    
    Status := AcquireCredentialsHandleW(
      nil,                          // pszPrincipal
      PWideChar(WideString(UNISP_NAME)),  // pszPackage
      SECPKG_CRED_OUTBOUND,         // fCredentialUse
      nil,                          // pvLogonId
      @SchannelCred,                // pAuthData
      nil,                          // pGetKeyFn
      nil,                          // pvGetKeyArgument
      @MyCredHandle,                // phCredential
      @MyTimeStamp                  // ptsExpiry
    );
    
    if Status = 0 then
      WritePass
    else
      WriteFail(Format('返回状态码: 0x%.8x', [Status]));
  except
    on E: Exception do
      WriteFail(E.Message);
  end;
  
  WriteTest('FreeCredentialsHandle');
  try
    if Status = 0 then
    begin
      Status := FreeCredentialsHandle(@MyCredHandle);
      if Status = 0 then
        WritePass
      else
        WriteFail(Format('返回状态码: 0x%.8x', [Status]));
    end
    else
      WriteLn('[SKIP] - 未获取凭据');
  except
    on E: Exception do
      WriteFail(E.Message);
  end;
  
  WriteLn;
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
    WriteLn('✅ 所有测试通过！WinSSL API 绑定正常工作。');
  end
  else
  begin
    WriteLn;
    WriteLn('⚠️ 部分测试失败，需要检查 API 绑定。');
  end;
  WriteLn('==============================================');
end;

begin
  TestsPassed := 0;
  TestsFailed := 0;
  
  WriteLn('');
  WriteLn('==============================================');
  WriteLn('  fafafa.ssl - WinSSL API 基础测试');
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
    TestBasicCredentialAcquisition;
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
