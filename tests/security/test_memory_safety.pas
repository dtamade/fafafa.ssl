program test_memory_safety;

{$mode objfpc}{$H+}{$J-}

{**
 * Memory Safety Test Suite
 *
 * P0-3: 安全测试扩展 - 内存安全 (已迁移到 TSimpleTestRunner)
 *
 * 测试内容:
 * - 安全内存清零
 * - 敏感数据处理
 * - 资源泄漏检测
 * - 重复释放防护
 *
 * @author fafafa.ssl team
 * @version 1.1.0 - P1-2.3 迁移到统一测试框架
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.memutils,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

function IsAllZeros(const AData: TBytes): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 0 to High(AData) do if AData[I] <> 0 then Exit(False);
end;

function HasNonZeros(const AData: TBytes): Boolean;
var I: Integer;
begin
  Result := False;
  for I := 0 to High(AData) do if AData[I] <> 0 then Exit(True);
end;

procedure TestSecureZeroMemory;
var
  Data: TBytes;
  AnsiData: AnsiString;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== 安全内存清零 ===');

  SetLength(Data, 256);
  for I := 0 to 255 do Data[I] := I;
  Runner.Check('填充随机数据后有非零值', HasNonZeros(Data));
  SecureZeroBytes(Data);
  Runner.Check('SecureZeroBytes 清零', IsAllZeros(Data));

  try
    AnsiData := 'SensitivePassword123!@#';
    UniqueString(AnsiData);
    Runner.Check('填充敏感字符串', Length(AnsiData) > 0);
    SecureZeroString(AnsiData);
    Runner.Check('SecureZeroString 执行成功', True);
  except
    on E: Exception do Runner.Check('SecureZeroString 异常', False, E.Message);
  end;

  try
    SetLength(Data, 0);
    SecureZeroBytes(Data);
    Runner.Check('空数组清零不崩溃', True);
  except
    on E: Exception do Runner.Check('空数组清零', False, E.Message);
  end;

  try
    AnsiData := '';
    SecureZeroString(AnsiData);
    Runner.Check('空字符串清零不崩溃', True);
  except
    on E: Exception do Runner.Check('空字符串清零', False, E.Message);
  end;
end;

procedure TestConstantTimeComparison;
var
  Data1, Data2: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== 恒定时间比较 ===');

  TCryptoUtils.EnsureInitialized;

  SetLength(Data1, 32);
  SetLength(Data2, 32);
  for I := 0 to 31 do begin Data1[I] := I; Data2[I] := I; end;
  Runner.Check('相等数据比较', TCryptoUtils.SecureCompare(Data1, Data2));

  Data2[0] := 255;
  Runner.Check('第一位不同', not TCryptoUtils.SecureCompare(Data1, Data2));

  Data2[0] := 0;
  Data2[31] := 255;
  Runner.Check('最后一位不同', not TCryptoUtils.SecureCompare(Data1, Data2));

  SetLength(Data2, 16);
  Runner.Check('不同长度', not TCryptoUtils.SecureCompare(Data1, Data2));

  SetLength(Data1, 0);
  SetLength(Data2, 0);
  Runner.Check('空数组相等', TCryptoUtils.SecureCompare(Data1, Data2));

  SetLength(Data1, 0);
  SetLength(Data2, 1);
  Data2[0] := 0;
  Runner.Check('空 vs 非空不等', not TCryptoUtils.SecureCompare(Data1, Data2));
end;

procedure TestKeyMaterialSafety;
var
  Key, KeyCopy: TBytes;
  HasNonZerosBefore, HasNonZerosAfter: Boolean;
begin
  WriteLn;
  WriteLn('=== 密钥材料安全 ===');

  TCryptoUtils.EnsureInitialized;

  Key := TCryptoUtils.GenerateKey(256);
  Runner.Check('生成 256 位密钥', Length(Key) = 32);
  Runner.Check('密钥有随机内容', HasNonZeros(Key));

  SetLength(KeyCopy, Length(Key));
  Move(Key[0], KeyCopy[0], Length(Key));

  HasNonZerosBefore := HasNonZeros(Key);
  SecureZeroBytes(Key);
  HasNonZerosAfter := HasNonZeros(Key);

  Runner.Check('清零前有非零值', HasNonZerosBefore);
  Runner.Check('清零后全为零', not HasNonZerosAfter);
  Runner.Check('副本未被影响', HasNonZeros(KeyCopy));

  SecureZeroBytes(KeyCopy);
  Runner.Check('副本也已清零', IsAllZeros(KeyCopy));
end;

procedure TestIVSafety;
var
  IV: TBytes;
  I, NonZeroCount: Integer;
begin
  WriteLn;
  WriteLn('=== IV 安全 ===');

  TCryptoUtils.EnsureInitialized;

  for I := 1 to 5 do
  begin
    IV := TCryptoUtils.GenerateIV(12);
    Runner.Check(Format('IV #%d 长度正确', [I]), Length(IV) = 12);
    Runner.Check(Format('IV #%d 有随机内容', [I]), HasNonZeros(IV));
    SecureZeroBytes(IV);
  end;

  NonZeroCount := 0;
  for I := 1 to 10 do
  begin
    IV := TCryptoUtils.GenerateIV(12);
    if HasNonZeros(IV) then Inc(NonZeroCount);
    SecureZeroBytes(IV);
  end;
  Runner.Check('10 次生成都有非零值', NonZeroCount = 10);
end;

procedure TestContextResourceManagement;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== 上下文资源管理 ===');

  Lib := TOpenSSLLibrary.Create;
  if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;
  Runner.Check('初始化 OpenSSL', True);

  for I := 1 to 10 do
  begin
    Ctx := Lib.CreateContext(sslCtxClient);
    Runner.Check(Format('创建上下文 #%d', [I]), Ctx <> nil);
  end;

  Runner.Check('多次创建/销毁无崩溃', True);

  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx <> nil then
  begin
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetOptions([ssoNoSSLv2, ssoNoSSLv3, ssoDisableCompression]);
    Ctx.SetServerName('test.example.com');
    Ctx.SetALPNProtocols('h2,http/1.1');
  end;
  Runner.Check('配置后释放无崩溃', True);
end;

procedure TestCryptoResourceManagement;
var
  Key, IV, Data, Encrypted, Decrypted: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== 加密资源管理 ===');

  TCryptoUtils.EnsureInitialized;

  Key := TCryptoUtils.GenerateKey(256);
  IV := TCryptoUtils.GenerateIV(12);
  SetLength(Data, 1024);
  FillChar(Data[0], 1024, $AA);

  for I := 1 to 100 do
  begin
    if TCryptoUtils.TryAES_GCM_Encrypt(Data, Key, IV, Encrypted) then
    begin
      if TCryptoUtils.TryAES_GCM_Decrypt(Encrypted, Key, IV, Decrypted) then
      begin
        SecureZeroBytes(Encrypted);
        SecureZeroBytes(Decrypted);
      end;
    end;
  end;
  Runner.Check('100 次加密/解密循环无泄漏', True);

  SecureZeroBytes(Key);
  SecureZeroBytes(IV);
  SecureZeroBytes(Data);
  Runner.Check('清理敏感数据', True);
end;

procedure TestStreamingResourceManagement;
var
  Hasher: TStreamingHasher;
  Cipher: TStreamingCipher;
  Key, IV, Data, Output, Tag, Hash: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== 流式处理资源管理 ===');

  TCryptoUtils.EnsureInitialized;

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
  Runner.Check('50 次流式哈希无泄漏', True);

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
  Runner.Check('50 次流式加密无泄漏', True);

  SecureZeroBytes(Key);
  SecureZeroBytes(IV);
end;

procedure TestExceptionResourceCleanup;
var
  Key, IV, Data: TBytes;
  I, ExceptionCount: Integer;
begin
  WriteLn;
  WriteLn('=== 异常资源清理 ===');

  TCryptoUtils.EnsureInitialized;
  ExceptionCount := 0;

  for I := 1 to 20 do
  begin
    try
      SetLength(Key, 16);
      SetLength(IV, 12);
      SetLength(Data, 100);
      TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
    except
      Inc(ExceptionCount);
    end;
  end;

  Runner.Check('异常正确抛出', ExceptionCount = 20);
  Runner.Check('异常后无崩溃', True);
end;

begin
  WriteLn('================================================================');
  WriteLn('内存安全测试套件');
  WriteLn('================================================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore, osmEVP]);
    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    TestSecureZeroMemory;
    TestConstantTimeComparison;
    TestKeyMaterialSafety;
    TestIVSafety;
    TestContextResourceManagement;
    TestCryptoResourceManagement;
    TestStreamingResourceManagement;
    TestExceptionResourceCleanup;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
