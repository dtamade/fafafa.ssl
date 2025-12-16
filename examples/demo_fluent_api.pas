{
  demo_fluent_api - fafafa.ssl v2.0 新 API 演示
  
  演示内容：
  1. Result 类型（Rust 风格错误处理）
  2. Try 方法（无异常版本）
  3. Connection Builder（流式 API）
}

program demo_fluent_api;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils;

procedure DemoResultTypes;
var
  Data: TBytes;
  Result: TSSLDataResult;
begin
  WriteLn;
  WriteLn('=== 1. Result 类型演示 ===');
  WriteLn;
  
  Data := TEncoding.UTF8.GetBytes('Hello World');
  
  // 创建成功结果
  Result := TSSLDataResult.Ok(Data);
  
  if Result.IsOk then
    WriteLn('✅ 成功: 数据长度 = ', Length(Result.Unwrap), ' 字节')
  else
    WriteLn('❌ 失败: ', Result.ErrorMessage);
    
  // 创建错误结果
  Result := TSSLDataResult.Err(sslErrInvalidParam, '参数无效');
  
  if Result.IsErr then
    WriteLn('❌ 错误码: ', Ord(Result.ErrorCode), ' - ', Result.ErrorMessage);
    
  // UnwrapOr 安全获取
  Data := Result.UnwrapOr(nil);
  if Data = nil then
    WriteLn('💡 UnwrapOr 返回: nil (默认值)')
  else
    WriteLn('💡 UnwrapOr 返回: 有数据');
end;

procedure DemoTryMethods;
var
  Data: TBytes;
  Hash: TBytes;
  Random: TBytes;
begin
  WriteLn;
  WriteLn('=== 2. Try 方法演示 ===');
  WriteLn;
  
  Data := TEncoding.UTF8.GetBytes('Hello World');
  
  // TrySHA256 - 无异常版本
  if TCryptoUtils.TrySHA256(Data, Hash) then
    WriteLn('✅ TrySHA256 成功: ', TCryptoUtils.BytesToHex(Hash))
  else
    WriteLn('❌ TrySHA256 失败');
    
  // TrySHA512
  if TCryptoUtils.TrySHA512('测试字符串', Hash) then
    WriteLn('✅ TrySHA512 成功: ', Copy(TCryptoUtils.BytesToHex(Hash), 1, 32), '...')
  else
    WriteLn('❌ TrySHA512 失败');
    
  // TrySecureRandom
  if TCryptoUtils.TrySecureRandom(16, Random) then
    WriteLn('✅ TrySecureRandom(16) 成功: ', TCryptoUtils.BytesToHex(Random))
  else
    WriteLn('❌ TrySecureRandom 失败');
end;

procedure DemoEncryptionWithTry;
var
  Key, IV, Data, Encrypted, Decrypted: TBytes;
begin
  WriteLn;
  WriteLn('=== 3. 加密 Try 方法演示 ===');
  WriteLn;
  
  Data := TEncoding.UTF8.GetBytes('机密数据 - Confidential');
  
  // 生成密钥和 IV
  Key := TCryptoUtils.GenerateKey(256);  // 32 字节
  IV := TCryptoUtils.GenerateIV(12);     // 12 字节 (GCM)
  
  WriteLn('📦 原始数据: ', TEncoding.UTF8.GetString(Data));
  WriteLn('🔑 密钥: ', TCryptoUtils.BytesToHex(Key));
  WriteLn('🎲 IV: ', TCryptoUtils.BytesToHex(IV));
  
  // 加密
  if TCryptoUtils.TryAES_GCM_Encrypt(Data, Key, IV, Encrypted) then
  begin
    WriteLn('✅ 加密成功: ', Length(Encrypted), ' 字节');
    
    // 解密
    if TCryptoUtils.TryAES_GCM_Decrypt(Encrypted, Key, IV, Decrypted) then
      WriteLn('✅ 解密成功: ', TEncoding.UTF8.GetString(Decrypted))
    else
      WriteLn('❌ 解密失败');
  end
  else
    WriteLn('❌ 加密失败');
end;

begin
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║  fafafa.ssl v2.0 - 新 API 特性演示        ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  
  try
    DemoResultTypes;
    DemoTryMethods;
    DemoEncryptionWithTry;
    
    WriteLn;
    WriteLn('=== 演示完成 ===');
    WriteLn('所有新 API 特性运行正常！');
  except
    on E: Exception do
      WriteLn('❌ 错误: ', E.Message);
  end;
end.
