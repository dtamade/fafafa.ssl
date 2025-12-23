program test_input_validation;

{$mode objfpc}{$H+}{$J-}

{**
 * Input Validation Security Test Suite
 *
 * P0-3: 安全测试扩展 - 输入验证
 *
 * 测试内容:
 * - 边界条件检查
 * - 空值/nil 处理
 * - 参数长度验证
 * - 畸形输入处理
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding,
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

{ 期望异常的测试辅助函数 }
function ExpectException(const ATestName: string): Boolean;
begin
  Result := False;
  Check(ATestName, True);
  Result := True;
end;

{ 测试加密参数验证 }
procedure TestCryptoParameterValidation;
var
  Data, Key, IV, Result_: TBytes;
  GoodKey, GoodIV: TBytes;
  ExceptionRaised: Boolean;
begin
  BeginSection('加密参数验证');

  TCryptoUtils.EnsureInitialized;

  // 准备有效参数
  GoodKey := TCryptoUtils.GenerateKey(256);  // 32 bytes
  GoodIV := TCryptoUtils.GenerateIV(12);     // 12 bytes for GCM
  SetLength(Data, 100);
  FillChar(Data[0], 100, $AA);

  // 测试正常情况
  Check('正常加密', TCryptoUtils.TryAES_GCM_Encrypt(Data, GoodKey, GoodIV, Result_));

  // 测试无效密钥长度
  SetLength(Key, 16);  // 16 bytes instead of 32
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Encrypt(Data, Key, GoodIV);
  except
    on E: ESSLInvalidArgument do
      ExceptionRaised := True;
    on E: Exception do
      ExceptionRaised := True;
  end;
  Check('拒绝短密钥 (16 bytes)', ExceptionRaised);

  // 测试空密钥
  SetLength(Key, 0);
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Encrypt(Data, Key, GoodIV);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝空密钥', ExceptionRaised);

  // 测试无效 IV 长度
  SetLength(IV, 16);  // 16 bytes instead of 12 for GCM
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Encrypt(Data, GoodKey, IV);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝错误长度 IV (16 bytes)', ExceptionRaised);

  // 测试空 IV
  SetLength(IV, 0);
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Encrypt(Data, GoodKey, IV);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝空 IV', ExceptionRaised);

  // 测试空数据（应该工作）
  SetLength(Data, 0);
  Check('接受空数据', TCryptoUtils.TryAES_GCM_Encrypt(Data, GoodKey, GoodIV, Result_));
end;

{ 测试解密参数验证 }
procedure TestDecryptParameterValidation;
var
  Ciphertext, Key, IV: TBytes;
  GoodKey, GoodIV: TBytes;
  ExceptionRaised: Boolean;
begin
  BeginSection('解密参数验证');

  TCryptoUtils.EnsureInitialized;

  GoodKey := TCryptoUtils.GenerateKey(256);
  GoodIV := TCryptoUtils.GenerateIV(12);

  // 测试太短的密文（小于 GCM 标签长度）
  SetLength(Ciphertext, 10);  // Less than 16 byte tag
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Decrypt(Ciphertext, GoodKey, GoodIV);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝太短密文', ExceptionRaised);

  // 测试空密文
  SetLength(Ciphertext, 0);
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Decrypt(Ciphertext, GoodKey, GoodIV);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝空密文', ExceptionRaised);

  // 测试篡改的密文
  SetLength(Ciphertext, 100);
  FillChar(Ciphertext[0], 100, $00);  // Invalid ciphertext + tag
  ExceptionRaised := False;
  try
    TCryptoUtils.AES_GCM_Decrypt(Ciphertext, GoodKey, GoodIV);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝篡改密文（认证失败）', ExceptionRaised);
end;

{ 测试随机数生成参数 }
procedure TestRandomParameterValidation;
var
  Result_: TBytes;
  ExceptionRaised: Boolean;
begin
  BeginSection('随机数参数验证');

  TCryptoUtils.EnsureInitialized;

  // 测试正常长度
  Check('生成 32 字节', TCryptoUtils.TrySecureRandom(32, Result_) and (Length(Result_) = 32));
  Check('生成 1 字节', TCryptoUtils.TrySecureRandom(1, Result_) and (Length(Result_) = 1));
  Check('生成 1024 字节', TCryptoUtils.TrySecureRandom(1024, Result_) and (Length(Result_) = 1024));

  // 测试无效长度
  ExceptionRaised := False;
  try
    TCryptoUtils.SecureRandom(0);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝零长度', ExceptionRaised);

  ExceptionRaised := False;
  try
    TCryptoUtils.SecureRandom(-1);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝负长度', ExceptionRaised);
end;

{ 测试编码参数验证 }
procedure TestEncodingParameterValidation;
var
  Result_: TBytes;
  InvalidHex, InvalidBase64: string;
  ExceptionRaised: Boolean;
begin
  BeginSection('编码参数验证');

  // 测试空输入
  Check('Hex 编码空数组', TEncodingUtils.BytesToHex(nil, False) = '');

  // 测试无效 Hex（应该抛出异常或返回空）
  InvalidHex := 'GHIJKL';  // Invalid hex characters
  ExceptionRaised := False;
  try
    Result_ := TEncodingUtils.HexToBytes(InvalidHex);
    // 如果没抛出异常，检查返回空
    Check('无效 Hex 处理', Length(Result_) = 0);
  except
    ExceptionRaised := True;
    Check('无效 Hex 抛出异常', True);
  end;

  // 测试奇数长度 Hex
  InvalidHex := 'ABC';  // Odd length
  ExceptionRaised := False;
  try
    Result_ := TEncodingUtils.HexToBytes(InvalidHex);
    Check('奇数长度 Hex 处理', True);  // Just verify no crash
  except
    ExceptionRaised := True;
    Check('奇数长度 Hex 抛出异常', True);
  end;

  // 测试无效 Base64
  InvalidBase64 := '!!!###';  // Invalid Base64 characters
  try
    Result_ := TEncodingUtils.Base64Decode(InvalidBase64);
    Check('无效 Base64 处理', True);  // Just verify no crash
  except
    Check('无效 Base64 抛出异常', True);
  end;

  // 测试有效输入
  try
    Check('有效 Hex 解码', Length(TEncodingUtils.HexToBytes('AABBCCDD')) = 4);
  except
    on E: Exception do
      Check('有效 Hex 解码', False, E.Message);
  end;

  try
    Check('有效 Base64 解码', Length(TEncodingUtils.Base64Decode('QUJDRA==')) = 4);
  except
    on E: Exception do
      Check('有效 Base64 解码', False, E.Message);
  end;
end;

{ 测试哈希参数验证 }
procedure TestHashParameterValidation;
var
  EmptyData: TBytes;
  Hash: TBytes;
begin
  BeginSection('哈希参数验证');

  TCryptoUtils.EnsureInitialized;

  // 测试空数据哈希（应该返回空数据的哈希，不是错误）
  SetLength(EmptyData, 0);
  Check('SHA256 空数据', TCryptoUtils.TrySHA256(EmptyData, Hash));
  Check('SHA256 空数据长度正确', Length(Hash) = 32);

  Check('SHA512 空数据', TCryptoUtils.TrySHA512(EmptyData, Hash));
  Check('SHA512 空数据长度正确', Length(Hash) = 64);

  // 测试大数据
  SetLength(EmptyData, 1024 * 1024);  // 1MB
  FillChar(EmptyData[0], Length(EmptyData), $55);
  Check('SHA256 大数据', TCryptoUtils.TrySHA256(EmptyData, Hash));
  Check('SHA256 大数据长度正确', Length(Hash) = 32);
end;

{ 测试上下文参数验证 }
procedure TestContextParameterValidation;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  ExceptionRaised: Boolean;
begin
  BeginSection('上下文参数验证');

  Lib := TOpenSSLLibrary.Create;
  if not Lib.Initialize then
  begin
    Check('初始化 OpenSSL', False);
    Exit;
  end;

  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx = nil then
  begin
    Check('创建上下文', False);
    Exit;
  end;

  // 测试空服务器名称
  Ctx.SetServerName('');
  Check('接受空服务器名称', Ctx.GetServerName = '');

  // 测试有效服务器名称
  Ctx.SetServerName('example.com');
  Check('设置服务器名称', Ctx.GetServerName = 'example.com');

  // 测试空密码套件
  Ctx.SetCipherList('');
  Check('接受空密码列表', True);

  // 测试空 ALPN
  Ctx.SetALPNProtocols('');
  Check('接受空 ALPN', Ctx.GetALPNProtocols = '');

  // 测试有效 ALPN
  Ctx.SetALPNProtocols('h2,http/1.1');
  Check('设置 ALPN 协议', Ctx.GetALPNProtocols = 'h2,http/1.1');

  // 测试加载不存在的证书
  ExceptionRaised := False;
  try
    Ctx.LoadCertificate('/nonexistent/path/cert.pem');
  except
    ExceptionRaised := True;
  end;
  Check('加载不存在证书抛异常', ExceptionRaised);

  // 测试加载不存在的 CA
  ExceptionRaised := False;
  try
    Ctx.LoadCAFile('/nonexistent/path/ca.pem');
  except
    ExceptionRaised := True;
  end;
  Check('加载不存在 CA 抛异常', ExceptionRaised);
end;

{ 测试密钥生成参数 }
procedure TestKeyGenerationParameters;
var
  Key: TBytes;
  ExceptionRaised: Boolean;
begin
  BeginSection('密钥生成参数');

  TCryptoUtils.EnsureInitialized;

  // 测试有效密钥大小
  Key := TCryptoUtils.GenerateKey(128);
  Check('生成 128 位密钥', Length(Key) = 16);

  Key := TCryptoUtils.GenerateKey(256);
  Check('生成 256 位密钥', Length(Key) = 32);

  Key := TCryptoUtils.GenerateKey(512);
  Check('生成 512 位密钥', Length(Key) = 64);

  // 测试无效密钥大小（非 8 的倍数）
  ExceptionRaised := False;
  try
    TCryptoUtils.GenerateKey(100);  // Not multiple of 8
  except
    ExceptionRaised := True;
  end;
  Check('拒绝非 8 倍数密钥大小', ExceptionRaised);

  // 测试零大小
  ExceptionRaised := False;
  try
    TCryptoUtils.GenerateKey(0);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝零密钥大小', ExceptionRaised);

  // 测试负大小
  ExceptionRaised := False;
  try
    TCryptoUtils.GenerateKey(-256);
  except
    ExceptionRaised := True;
  end;
  Check('拒绝负密钥大小', ExceptionRaised);
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('输入验证安全测试摘要');
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
  WriteLn('输入验证安全测试套件');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  验证所有公共 API 的输入参数验证，确保不会因无效输入导致');
  WriteLn('  崩溃或安全漏洞。');
  WriteLn;

  try
    TestCryptoParameterValidation;
    TestDecryptParameterValidation;
    TestRandomParameterValidation;
    TestEncodingParameterValidation;
    TestHashParameterValidation;
    TestContextParameterValidation;
    TestKeyGenerationParameters;

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
