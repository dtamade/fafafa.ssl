program test_input_validation;

{$mode objfpc}{$H+}{$J-}

{**
 * Input Validation Security Test Suite
 *
 * P0-3: 安全测试扩展 - 输入验证 (已迁移到 TSimpleTestRunner)
 *
 * 测试内容:
 * - 边界条件检查
 * - 空值/nil 处理
 * - 参数长度验证
 * - 畸形输入处理
 *
 * @author fafafa.ssl team
 * @version 1.1.0 - P1-2.3 迁移到统一测试框架
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

procedure TestCryptoParameterValidation;
var
  Data, Key, IV, Result_: TBytes;
  GoodKey, GoodIV: TBytes;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== 加密参数验证 ===');

  TCryptoUtils.EnsureInitialized;
  GoodKey := TCryptoUtils.GenerateKey(256);
  GoodIV := TCryptoUtils.GenerateIV(12);
  SetLength(Data, 100);
  FillChar(Data[0], 100, $AA);

  Runner.Check('正常加密', TCryptoUtils.TryAES_GCM_Encrypt(Data, GoodKey, GoodIV, Result_));

  SetLength(Key, 16);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Encrypt(Data, Key, GoodIV); except ExceptionRaised := True; end;
  Runner.Check('拒绝短密钥 (16 bytes)', ExceptionRaised);

  SetLength(Key, 0);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Encrypt(Data, Key, GoodIV); except ExceptionRaised := True; end;
  Runner.Check('拒绝空密钥', ExceptionRaised);

  SetLength(IV, 16);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Encrypt(Data, GoodKey, IV); except ExceptionRaised := True; end;
  Runner.Check('拒绝错误长度 IV (16 bytes)', ExceptionRaised);

  SetLength(IV, 0);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Encrypt(Data, GoodKey, IV); except ExceptionRaised := True; end;
  Runner.Check('拒绝空 IV', ExceptionRaised);

  SetLength(Data, 0);
  Runner.Check('接受空数据', TCryptoUtils.TryAES_GCM_Encrypt(Data, GoodKey, GoodIV, Result_));
end;

procedure TestDecryptParameterValidation;
var
  Ciphertext: TBytes;
  GoodKey, GoodIV: TBytes;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== 解密参数验证 ===');

  TCryptoUtils.EnsureInitialized;
  GoodKey := TCryptoUtils.GenerateKey(256);
  GoodIV := TCryptoUtils.GenerateIV(12);

  SetLength(Ciphertext, 10);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Decrypt(Ciphertext, GoodKey, GoodIV); except ExceptionRaised := True; end;
  Runner.Check('拒绝太短密文', ExceptionRaised);

  SetLength(Ciphertext, 0);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Decrypt(Ciphertext, GoodKey, GoodIV); except ExceptionRaised := True; end;
  Runner.Check('拒绝空密文', ExceptionRaised);

  SetLength(Ciphertext, 100);
  FillChar(Ciphertext[0], 100, $00);
  ExceptionRaised := False;
  try TCryptoUtils.AES_GCM_Decrypt(Ciphertext, GoodKey, GoodIV); except ExceptionRaised := True; end;
  Runner.Check('拒绝篡改密文（认证失败）', ExceptionRaised);
end;

procedure TestRandomParameterValidation;
var
  Result_: TBytes;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== 随机数参数验证 ===');

  TCryptoUtils.EnsureInitialized;

  Runner.Check('生成 32 字节', TCryptoUtils.TrySecureRandom(32, Result_) and (Length(Result_) = 32));
  Runner.Check('生成 1 字节', TCryptoUtils.TrySecureRandom(1, Result_) and (Length(Result_) = 1));
  Runner.Check('生成 1024 字节', TCryptoUtils.TrySecureRandom(1024, Result_) and (Length(Result_) = 1024));

  ExceptionRaised := False;
  try TCryptoUtils.SecureRandom(0); except ExceptionRaised := True; end;
  Runner.Check('拒绝零长度', ExceptionRaised);

  ExceptionRaised := False;
  try TCryptoUtils.SecureRandom(-1); except ExceptionRaised := True; end;
  Runner.Check('拒绝负长度', ExceptionRaised);
end;

procedure TestEncodingParameterValidation;
var
  Result_: TBytes;
  InvalidHex, InvalidBase64: string;
begin
  WriteLn;
  WriteLn('=== 编码参数验证 ===');

  Runner.Check('Hex 编码空数组', TEncodingUtils.BytesToHex(nil, False) = '');

  InvalidHex := 'GHIJKL';
  try
    Result_ := TEncodingUtils.HexToBytes(InvalidHex);
    Runner.Check('无效 Hex 处理', Length(Result_) = 0);
  except
    Runner.Check('无效 Hex 抛出异常', True);
  end;

  InvalidHex := 'ABC';
  try
    Result_ := TEncodingUtils.HexToBytes(InvalidHex);
    Runner.Check('奇数长度 Hex 处理', True);
  except
    Runner.Check('奇数长度 Hex 抛出异常', True);
  end;

  InvalidBase64 := '!!!###';
  try
    Result_ := TEncodingUtils.Base64Decode(InvalidBase64);
    Runner.Check('无效 Base64 处理', True);
  except
    Runner.Check('无效 Base64 抛出异常', True);
  end;

  try
    Runner.Check('有效 Hex 解码', Length(TEncodingUtils.HexToBytes('AABBCCDD')) = 4);
  except
    on E: Exception do Runner.Check('有效 Hex 解码', False, E.Message);
  end;

  try
    Runner.Check('有效 Base64 解码', Length(TEncodingUtils.Base64Decode('QUJDRA==')) = 4);
  except
    on E: Exception do Runner.Check('有效 Base64 解码', False, E.Message);
  end;
end;

procedure TestHashParameterValidation;
var
  EmptyData: TBytes;
  Hash: TBytes;
begin
  WriteLn;
  WriteLn('=== 哈希参数验证 ===');

  TCryptoUtils.EnsureInitialized;

  SetLength(EmptyData, 0);
  Runner.Check('SHA256 空数据', TCryptoUtils.TrySHA256(EmptyData, Hash));
  Runner.Check('SHA256 空数据长度正确', Length(Hash) = 32);

  Runner.Check('SHA512 空数据', TCryptoUtils.TrySHA512(EmptyData, Hash));
  Runner.Check('SHA512 空数据长度正确', Length(Hash) = 64);

  SetLength(EmptyData, 1024 * 1024);
  FillChar(EmptyData[0], Length(EmptyData), $55);
  Runner.Check('SHA256 大数据', TCryptoUtils.TrySHA256(EmptyData, Hash));
  Runner.Check('SHA256 大数据长度正确', Length(Hash) = 32);
end;

procedure TestContextParameterValidation;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== 上下文参数验证 ===');

  Lib := TOpenSSLLibrary.Create;
  if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx = nil then begin Runner.Check('创建上下文', False); Exit; end;

  Ctx.SetServerName('');
  Runner.Check('接受空服务器名称', Ctx.GetServerName = '');

  Ctx.SetServerName('example.com');
  Runner.Check('设置服务器名称', Ctx.GetServerName = 'example.com');

  Ctx.SetCipherList('');
  Runner.Check('接受空密码列表', True);

  Ctx.SetALPNProtocols('');
  Runner.Check('接受空 ALPN', Ctx.GetALPNProtocols = '');

  Ctx.SetALPNProtocols('h2,http/1.1');
  Runner.Check('设置 ALPN 协议', Ctx.GetALPNProtocols = 'h2,http/1.1');

  ExceptionRaised := False;
  try Ctx.LoadCertificate('/nonexistent/path/cert.pem'); except ExceptionRaised := True; end;
  Runner.Check('加载不存在证书抛异常', ExceptionRaised);

  ExceptionRaised := False;
  try Ctx.LoadCAFile('/nonexistent/path/ca.pem'); except ExceptionRaised := True; end;
  Runner.Check('加载不存在 CA 抛异常', ExceptionRaised);
end;

procedure TestKeyGenerationParameters;
var
  Key: TBytes;
  ExceptionRaised: Boolean;
begin
  WriteLn;
  WriteLn('=== 密钥生成参数 ===');

  TCryptoUtils.EnsureInitialized;

  Key := TCryptoUtils.GenerateKey(128);
  Runner.Check('生成 128 位密钥', Length(Key) = 16);

  Key := TCryptoUtils.GenerateKey(256);
  Runner.Check('生成 256 位密钥', Length(Key) = 32);

  Key := TCryptoUtils.GenerateKey(512);
  Runner.Check('生成 512 位密钥', Length(Key) = 64);

  ExceptionRaised := False;
  try TCryptoUtils.GenerateKey(100); except ExceptionRaised := True; end;
  Runner.Check('拒绝非 8 倍数密钥大小', ExceptionRaised);

  ExceptionRaised := False;
  try TCryptoUtils.GenerateKey(0); except ExceptionRaised := True; end;
  Runner.Check('拒绝零密钥大小', ExceptionRaised);

  ExceptionRaised := False;
  try TCryptoUtils.GenerateKey(-256); except ExceptionRaised := True; end;
  Runner.Check('拒绝负密钥大小', ExceptionRaised);
end;

begin
  WriteLn('================================================================');
  WriteLn('输入验证安全测试套件');
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

    TestCryptoParameterValidation;
    TestDecryptParameterValidation;
    TestRandomParameterValidation;
    TestEncodingParameterValidation;
    TestHashParameterValidation;
    TestContextParameterValidation;
    TestKeyGenerationParameters;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
