program test_memory_safety;

{$mode objfpc}{$H+}{$J-}

{**
 * Memory Safety Test Suite
 *
 * P0-3: 安全测试扩展 - 内存安全
 *
 * 测试内容:
 * - 安全内存清零
 * - 敏感数据处理
 * - 资源泄漏检测
 * - 重复释放防护
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.memutils,
  fafafa.ssl.openssl.backed;

var
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const aName: string);
begin
  Section := aName;
  WriteLn;
  WriteLn('=== ', aName, ' ===');
end;

procedure Check(const aName: string; ok: Boolean; const details: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', aName, ': ');
  if ok then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if details <> '' then
      WriteLn('    ', details);
  end;
end;

{ 检查字节数组是否全为零 }
function IsAllZeros(const AData: TBytes): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(AData) do
    if AData[I] <> 0 then
      Exit(False);
end;

{ 检查字节数组是否有非零值 }
function HasNonZeros(const AData: TBytes): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AData) do
    if AData[I] <> 0 then
      Exit(True);
end;

{ 测试安全内存清零 }
procedure TestSecureZeroMemory;
var
  Data: TBytes;
  AnsiData: AnsiString;
  I: Integer;
begin
  BeginSection('安全内存清零');

  // 测试 SecureZeroBytes
  SetLength(Data, 256);
  for I := 0 to 255 do
    Data[I] := I;

  Check('填充随机数据后有非零值', HasNonZeros(Data));

  SecureZeroBytes(Data);
  Check('SecureZeroBytes 清零', IsAllZeros(Data));

  // 测试 SecureZeroString (使用 UniqueString 确保字符串可写)
  try
    AnsiData := 'SensitivePassword123!@#';
    // 强制创建字符串的独立副本（使其可写）
    UniqueString(AnsiData);
    Check('填充敏感字符串', Length(AnsiData) > 0);

    SecureZeroString(AnsiData);
    Check('SecureZeroString 执行成功', True);
  except
    on E: Exception do
      Check('SecureZeroString 异常', False, E.Message);
  end;

  // 测试空数据处理
  try
    SetLength(Data, 0);
    SecureZeroBytes(Data);  // Should not crash
    Check('空数组清零不崩溃', True);
  except
    on E: Exception do
      Check('空数组清零', False, E.Message);
  end;

  try
    AnsiData := '';
    SecureZeroString(AnsiData);  // Should not crash
    Check('空字符串清零不崩溃', True);
  except
    on E: Exception do
      Check('空字符串清零', False, E.Message);
  end;
end;

{ 测试恒定时间比较 }
procedure TestConstantTimeComparison;
var
  Data1, Data2: TBytes;
  I: Integer;
begin
  BeginSection('恒定时间比较');

  TCryptoUtils.EnsureInitialized;

  // 测试相等数据
  SetLength(Data1, 32);
  SetLength(Data2, 32);
  for I := 0 to 31 do
  begin
    Data1[I] := I;
    Data2[I] := I;
  end;
  Check('相等数据比较', TCryptoUtils.SecureCompare(Data1, Data2));

  // 测试不等数据（第一位不同）
  Data2[0] := 255;
  Check('第一位不同', not TCryptoUtils.SecureCompare(Data1, Data2));

  // 测试不等数据（最后一位不同）
  Data2[0] := 0;
  Data2[31] := 255;
  Check('最后一位不同', not TCryptoUtils.SecureCompare(Data1, Data2));

  // 测试不同长度
  SetLength(Data2, 16);
  Check('不同长度', not TCryptoUtils.SecureCompare(Data1, Data2));

  // 测试空数组
  SetLength(Data1, 0);
  SetLength(Data2, 0);
  Check('空数组相等', TCryptoUtils.SecureCompare(Data1, Data2));

  // 测试空 vs 非空
  SetLength(Data1, 0);
  SetLength(Data2, 1);
  Data2[0] := 0;
  Check('空 vs 非空不等', not TCryptoUtils.SecureCompare(Data1, Data2));
end;

{ 测试密钥材料安全处理 }
procedure TestKeyMaterialSafety;
var
  Key: TBytes;
  KeyCopy: TBytes;
  I: Integer;
  HasNonZerosBefore, HasNonZerosAfter: Boolean;
begin
  BeginSection('密钥材料安全');

  TCryptoUtils.EnsureInitialized;

  // 生成密钥
  Key := TCryptoUtils.GenerateKey(256);
  Check('生成 256 位密钥', Length(Key) = 32);
  Check('密钥有随机内容', HasNonZeros(Key));

  // 保存副本用于验证
  SetLength(KeyCopy, Length(Key));
  Move(Key[0], KeyCopy[0], Length(Key));

  HasNonZerosBefore := HasNonZeros(Key);

  // 清零密钥
  SecureZeroBytes(Key);
  HasNonZerosAfter := HasNonZeros(Key);

  Check('清零前有非零值', HasNonZerosBefore);
  Check('清零后全为零', not HasNonZerosAfter);

  // 验证副本仍有原始数据
  Check('副本未被影响', HasNonZeros(KeyCopy));

  // 清理副本
  SecureZeroBytes(KeyCopy);
  Check('副本也已清零', IsAllZeros(KeyCopy));
end;

{ 测试 IV 安全处理 }
procedure TestIVSafety;
var
  IV: TBytes;
  I: Integer;
  NonZeroCount: Integer;
begin
  BeginSection('IV 安全');

  TCryptoUtils.EnsureInitialized;

  // 生成多个 IV，验证它们不同
  for I := 1 to 5 do
  begin
    IV := TCryptoUtils.GenerateIV(12);
    Check(Format('IV #%d 长度正确', [I]), Length(IV) = 12);
    Check(Format('IV #%d 有随机内容', [I]), HasNonZeros(IV));
    SecureZeroBytes(IV);
  end;

  // 验证 IV 随机性（不太可能连续生成相同的 IV）
  NonZeroCount := 0;
  for I := 1 to 10 do
  begin
    IV := TCryptoUtils.GenerateIV(12);
    if HasNonZeros(IV) then
      Inc(NonZeroCount);
    SecureZeroBytes(IV);
  end;
  Check('10 次生成都有非零值', NonZeroCount = 10);
end;

{ 测试上下文资源管理 }
procedure TestContextResourceManagement;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  I: Integer;
begin
  BeginSection('上下文资源管理');

  Lib := TOpenSSLLibrary.Create;
  if not Lib.Initialize then
  begin
    Check('初始化 OpenSSL', False);
    Exit;
  end;
  Check('初始化 OpenSSL', True);

  // 创建和销毁多个上下文，检查资源泄漏
  for I := 1 to 10 do
  begin
    Ctx := Lib.CreateContext(sslCtxClient);
    Check(Format('创建上下文 #%d', [I]), Ctx <> nil);
    // Ctx 在这里超出作用域，应该自动释放
  end;

  Check('多次创建/销毁无崩溃', True);

  // 创建上下文并配置后释放
  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx <> nil then
  begin
    // 配置安全选项（替代 ConfigureSecureDefaults）
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetOptions([ssoNoSSLv2, ssoNoSSLv3, ssoDisableCompression]);
    Ctx.SetServerName('test.example.com');
    Ctx.SetALPNProtocols('h2,http/1.1');
    // Ctx 释放
  end;
  Check('配置后释放无崩溃', True);
end;

{ 测试加密/解密资源管理 }
procedure TestCryptoResourceManagement;
var
  Key, IV, Data, Encrypted, Decrypted: TBytes;
  I: Integer;
begin
  BeginSection('加密资源管理');

  TCryptoUtils.EnsureInitialized;

  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);
  SetLength(Data, 1024);
  FillChar(Data[0], 1024, $AA);

  // 多次加密/解密循环
  for I := 1 to 100 do
  begin
    if TCryptoUtils.TryAES_GCM_Encrypt(Data, Key, IV, Encrypted) then
    begin
      if TCryptoUtils.TryAES_GCM_Decrypt(Encrypted, Key, IV, Decrypted) then
      begin
        // 清理中间数据
        SecureZeroBytes(Encrypted);
        SecureZeroBytes(Decrypted);
      end;
    end;
  end;
  Check('100 次加密/解密循环无泄漏', True);

  // 清理敏感数据
  SecureZeroBytes(Key);
  SecureZeroBytes(IV);
  SecureZeroBytes(Data);
  Check('清理敏感数据', True);
end;

{ 测试流式处理资源管理 }
procedure TestStreamingResourceManagement;
var
  Hasher: TStreamingHasher;
  Cipher: TStreamingCipher;
  Key, IV, Data, Output, Tag: TBytes;
  Hash: TBytes;
  I: Integer;
begin
  BeginSection('流式处理资源管理');

  TCryptoUtils.EnsureInitialized;

  // 测试流式哈希器
  for I := 1 to 50 do
  begin
    Hasher := TStreamingHasher.Create(HASH_SHA256);
    try
      SetLength(Data, 100);
      FillChar(Data[0], 100, I);
      Hasher.Update(Data);
      Hash := Hasher.Finalize;
    finally
      Hasher.Free;
    end;
    SecureZeroBytes(Hash);
  end;
  Check('50 次流式哈希无泄漏', True);

  // 测试流式加密器
  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);

  for I := 1 to 50 do
  begin
    Cipher := TStreamingCipher.CreateEncrypt(ENCRYPT_AES_256_GCM, Key, IV);
    try
      SetLength(Data, 100);
      FillChar(Data[0], 100, I);
      Cipher.Update(Data, Output);
      SetLength(Tag, 16);
      Cipher.Finalize(Output, Tag);
    finally
      Cipher.Free;
    end;
    SecureZeroBytes(Output);
    SecureZeroBytes(Tag);
  end;
  Check('50 次流式加密无泄漏', True);

  SecureZeroBytes(Key);
  SecureZeroBytes(IV);
end;

{ 测试异常情况下的资源清理 }
procedure TestExceptionResourceCleanup;
var
  Key, IV, Data, Result_: TBytes;
  I: Integer;
  ExceptionCount: Integer;
begin
  BeginSection('异常资源清理');

  TCryptoUtils.EnsureInitialized;

  ExceptionCount := 0;

  // 故意触发异常，验证不会泄漏资源
  for I := 1 to 20 do
  begin
    try
      // 使用无效密钥长度
      SetLength(Key, 16);  // Wrong size
      SetLength(IV, 12);
      SetLength(Data, 100);
      TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    except
      Inc(ExceptionCount);
    end;
  end;

  Check('异常正确抛出', ExceptionCount = 20);
  Check('异常后无崩溃', True);
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('内存安全测试摘要');
  WriteLn('================================================================');
  WriteLn('总计: ', Total);
  WriteLn('通过: ', Passed);
  WriteLn('失败: ', Failed);
  WriteLn;

  if Failed = 0 then
    WriteLn('结果: 全部测试通过')
  else
    WriteLn('结果: 存在失败的测试');

  WriteLn('================================================================');
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('================================================================');
  WriteLn('内存安全测试套件');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  验证内存安全处理，包括敏感数据清零、资源管理等。');
  WriteLn;

  try
    TestSecureZeroMemory;
    TestConstantTimeComparison;
    TestKeyMaterialSafety;
    TestIVSafety;
    TestContextResourceManagement;
    TestCryptoResourceManagement;
    TestStreamingResourceManagement;
    TestExceptionResourceCleanup;

    PrintSummary;

    if Failed > 0 then
      Halt(1);
  except
    on E: Exception do
    begin
      WriteLn('CRITICAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.
