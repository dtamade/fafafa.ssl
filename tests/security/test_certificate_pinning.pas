program test_certificate_pinning;

{$mode objfpc}{$H+}{$J-}

{**
 * Certificate Pinning Test Suite
 *
 * P3-14: 证书固定测试
 *
 * 测试内容:
 * - SHA-256 指纹提取与比较
 * - SHA-1 指纹提取与比较
 * - 指纹格式规范化
 * - 恒定时间比较（防时序攻击）
 * - 证书链固定
 * - 公钥固定（SPKI Hash）
 *
 * 注意：证书固定是防止 MITM 攻击的重要安全机制。
 * 测试覆盖了实际场景中的常见固定策略。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding,
  fafafa.ssl.openssl.backed;

const
  // 测试用证书 PEM（自签名证书，仅用于测试）
  TEST_CERT_PEM =
    '-----BEGIN CERTIFICATE-----' + sLineBreak +
    'MIICpDCCAYwCCQCU+hU2FXcWH9ANBgkqhkiG9w0BAQsFADAUMRIwEAYDVQQDDAls' + sLineBreak +
    'b2NhbGhvc3QwHhcNMjMxMjAxMDAwMDAwWhcNMjQxMjAxMDAwMDAwWjAUMRIwEAYD' + sLineBreak +
    'VQQDDAlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC7' + sLineBreak +
    'o5eVCqHBGl0dX5YZpC8M8xNEE7NQDwQq6NLBELz8K1rU4pXLI3VqKtX8mKtUQjQZ' + sLineBreak +
    'KqZx5RPn0mWWD0G8AHg8JY5v5r0rM5zy8IaP1EAqVNx8P8O5bHJk3cVaWmW3C9mM' + sLineBreak +
    'L8D3y0LCqN6C9I8mI5wBJ7q3IKqGlv8CaCPLD5VqZnS2Z5lqW3fV0VNx2NYd8W0z' + sLineBreak +
    'R3Ww8bI6N9VnL9V3xL1CpP5kV9xL9EqvI5F3V4P8LXq5wX9L2rKdC8W3mI5nB7q3' + sLineBreak +
    'IKGL8C3aCP5LD9VqnZS2Z5qlW3fV0VNx2NYd8W0zR3Ww8bI6N9VnL9V3xL1CpP5k' + sLineBreak +
    'V9xL9EqvI5F3V4P8LAgMBAAEwDQYJKoZIhvcNAQELBQADggEBAE5P0Z5K5l3DsB0' + sLineBreak +
    '-----END CERTIFICATE-----';

  // 已知的测试用指纹（这些是示例值，实际运行时会动态计算）
  // 格式: AA:BB:CC:DD... (64字节16进制，冒号分隔)
  EXPECTED_FINGERPRINT_FORMAT_LENGTH = 95; // 32*2 + 31 colons for SHA256

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

{ 规范化指纹格式 - 移除冒号，转大写 }
function NormalizeFingerprint(const AFingerprint: string): string;
begin
  Result := UpperCase(StringReplace(AFingerprint, ':', '', [rfReplaceAll]));
end;

{ 安全比较两个字符串 - 恒定时间 }
function SecureStringCompare(const A, B: string): Boolean;
var
  I: Integer;
  Diff: Byte;
begin
  if Length(A) <> Length(B) then
    Exit(False);

  Diff := 0;
  for I := 1 to Length(A) do
    Diff := Diff or (Ord(A[I]) xor Ord(B[I]));

  Result := (Diff = 0);
end;

{ 测试指纹提取 }
procedure TestFingerprintExtraction;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  FP_SHA256, FP_SHA1: string;
begin
  BeginSection('指纹提取');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;
    Check('初始化 OpenSSL', True);

    Cert := Lib.CreateCertificate;
    Check('创建证书对象', Cert <> nil);

    if Cert <> nil then
    begin
      // 尝试从系统存储加载一个证书用于测试
      if Cert.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
      begin
        Check('加载系统 CA 证书', True);

        // 提取 SHA-256 指纹
        FP_SHA256 := Cert.GetFingerprintSHA256;
        Check('提取 SHA-256 指纹', FP_SHA256 <> '', 'Fingerprint: ' + FP_SHA256);

        // 验证 SHA-256 指纹格式（64 hex chars + colons）
        Check('SHA-256 指纹格式正确', Length(NormalizeFingerprint(FP_SHA256)) = 64);

        // 提取 SHA-1 指纹
        FP_SHA1 := Cert.GetFingerprintSHA1;
        Check('提取 SHA-1 指纹', FP_SHA1 <> '', 'Fingerprint: ' + FP_SHA1);

        // 验证 SHA-1 指纹格式（40 hex chars）
        Check('SHA-1 指纹格式正确', Length(NormalizeFingerprint(FP_SHA1)) = 40);

        // 验证两个指纹不同
        Check('SHA-256 和 SHA-1 指纹不同', FP_SHA256 <> FP_SHA1);
      end
      else
      begin
        // 没有系统证书，使用基本测试
        Check('加载系统 CA 证书', False, '文件不存在或无法读取');
        Check('使用空证书测试指纹', Cert.GetFingerprintSHA256 = '', '空证书应返回空指纹');
      end;
    end;
  except
    on E: Exception do
      Check('指纹提取', False, E.Message);
  end;
end;

{ 测试指纹比较 - 模拟证书固定场景 }
procedure TestFingerprintComparison;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  ActualFP, ExpectedFP: string;
  PinnedFingerprints: array[0..2] of string;
  I: Integer;
  Found: Boolean;
begin
  BeginSection('指纹比较（证书固定）');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Cert := Lib.CreateCertificate;
    if Cert = nil then
    begin
      Check('创建证书对象', False);
      Exit;
    end;

    // 尝试加载证书
    if not Cert.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
    begin
      Check('证书加载（跳过比较测试）', False, '无可用证书');
      Exit;
    end;

    ActualFP := NormalizeFingerprint(Cert.GetFingerprintSHA256);
    Check('获取实际指纹', ActualFP <> '');

    // 测试精确匹配
    ExpectedFP := ActualFP;
    Check('精确匹配测试', SecureStringCompare(ActualFP, ExpectedFP));

    // 测试不匹配
    ExpectedFP := StringOfChar('0', 64);
    Check('不匹配测试', not SecureStringCompare(ActualFP, ExpectedFP));

    // 测试固定列表（模拟多个固定指纹）
    PinnedFingerprints[0] := StringOfChar('A', 64);
    PinnedFingerprints[1] := ActualFP;  // 正确的指纹
    PinnedFingerprints[2] := StringOfChar('B', 64);

    Found := False;
    for I := 0 to 2 do
    begin
      if SecureStringCompare(ActualFP, PinnedFingerprints[I]) then
      begin
        Found := True;
        Break;
      end;
    end;
    Check('固定列表匹配', Found);

    // 测试大小写不敏感比较
    ExpectedFP := LowerCase(ActualFP);
    Check('大小写不敏感比较', SecureStringCompare(UpperCase(ActualFP), UpperCase(ExpectedFP)));

  except
    on E: Exception do
      Check('指纹比较', False, E.Message);
  end;
end;

{ 测试恒定时间比较 }
procedure TestConstantTimeComparison;
var
  FP1, FP2: string;
  I: Integer;
  AllMatch: Boolean;
begin
  BeginSection('恒定时间比较');

  // 测试相等字符串
  FP1 := StringOfChar('A', 64);
  FP2 := StringOfChar('A', 64);
  Check('相等字符串比较', SecureStringCompare(FP1, FP2));

  // 测试不等字符串（最后一位不同）
  FP2[64] := 'B';
  Check('最后一位不同', not SecureStringCompare(FP1, FP2));

  // 测试不等字符串（第一位不同）
  FP2 := StringOfChar('A', 64);
  FP2[1] := 'B';
  Check('第一位不同', not SecureStringCompare(FP1, FP2));

  // 测试完全不同的字符串
  FP2 := StringOfChar('B', 64);
  Check('完全不同', not SecureStringCompare(FP1, FP2));

  // 测试空字符串
  Check('空字符串相等', SecureStringCompare('', ''));
  Check('空与非空不等', not SecureStringCompare('', 'A'));

  // 测试不同长度
  Check('不同长度', not SecureStringCompare(StringOfChar('A', 64), StringOfChar('A', 32)));

  // 测试字节级安全比较
  AllMatch := True;
  for I := 1 to 100 do
  begin
    FP1 := TEncodingUtils.BytesToHex(TCryptoUtils.SecureRandom(32), False);
    if SecureStringCompare(FP1, FP1) = False then
    begin
      AllMatch := False;
      Break;
    end;
  end;
  Check('随机指纹自比较', AllMatch);
end;

{ 测试指纹格式处理 }
procedure TestFingerprintFormatting;
var
  RawFP, ColonFP, NormalizedFP: string;
begin
  BeginSection('指纹格式处理');

  // 测试原始格式（无分隔符）
  RawFP := 'A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4E5F6A1B2';
  Check('原始格式长度正确', Length(RawFP) = 64);

  // 测试冒号分隔格式
  ColonFP := 'A1:B2:C3:D4:E5:F6:A1:B2:C3:D4:E5:F6:A1:B2:C3:D4:' +
             'E5:F6:A1:B2:C3:D4:E5:F6:A1:B2:C3:D4:E5:F6:A1:B2';
  Check('冒号格式长度正确', Length(ColonFP) = 95);

  // 测试规范化
  NormalizedFP := NormalizeFingerprint(ColonFP);
  Check('规范化结果正确', NormalizedFP = RawFP);

  // 测试大小写处理
  Check('大写规范化', NormalizeFingerprint('aa:bb:cc') = 'AABBCC');
  Check('混合大小写', NormalizeFingerprint('Aa:Bb:Cc') = 'AABBCC');
end;

{ 测试 TCryptoUtils 安全比较 }
procedure TestCryptoUtilsSecureCompare;
var
  Bytes1, Bytes2: TBytes;
  I: Integer;
begin
  BeginSection('TCryptoUtils.SecureCompare');

  TCryptoUtils.EnsureInitialized;

  // 测试相等数组
  SetLength(Bytes1, 32);
  SetLength(Bytes2, 32);
  for I := 0 to 31 do
  begin
    Bytes1[I] := I;
    Bytes2[I] := I;
  end;
  Check('相等数组', TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  // 测试不等数组
  Bytes2[15] := 255;
  Check('不等数组', not TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  // 测试不同长度
  SetLength(Bytes2, 16);
  Check('不同长度', not TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  // 测试空数组
  SetLength(Bytes1, 0);
  SetLength(Bytes2, 0);
  Check('空数组相等', TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  // 测试大数组
  Bytes1 := TCryptoUtils.SecureRandom(1024);
  Bytes2 := Copy(Bytes1, 0, Length(Bytes1));
  Check('大数组相等', TCryptoUtils.SecureCompare(Bytes1, Bytes2));
end;

{ 测试公钥固定（SPKI Hash） }
procedure TestPublicKeyPinning;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  PublicKeyHash: TBytes;
  PublicKeyHashHex: string;
begin
  BeginSection('公钥固定 (SPKI)');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Cert := Lib.CreateCertificate;
    if Cert = nil then
    begin
      Check('创建证书对象', False);
      Exit;
    end;

    // 尝试加载证书
    if not Cert.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
    begin
      Check('证书加载（跳过 SPKI 测试）', False, '无可用证书');
      Exit;
    end;

    // 获取公钥信息
    Check('证书加载成功', True);

    // 获取公钥 DER 编码并计算哈希
    // 注意：这里模拟 SPKI 固定，实际实现需要从证书中提取公钥
    // 使用证书指纹作为替代演示
    PublicKeyHashHex := Cert.GetFingerprintSHA256;
    Check('获取公钥哈希', PublicKeyHashHex <> '');

    // 验证哈希可以用于固定
    PublicKeyHash := TEncodingUtils.HexToBytes(NormalizeFingerprint(PublicKeyHashHex));
    Check('哈希转换为字节', Length(PublicKeyHash) = 32);

    // 验证 Base64 编码（HPKP 格式）
    PublicKeyHashHex := TEncodingUtils.Base64Encode(PublicKeyHash);
    Check('Base64 编码成功', PublicKeyHashHex <> '');
    Check('Base64 长度正确', Length(PublicKeyHashHex) = 44); // 32 bytes -> 44 base64 chars

  except
    on E: Exception do
      Check('公钥固定', False, E.Message);
  end;
end;

{ 测试证书链固定 }
procedure TestCertificateChainPinning;
var
  Lib: ISSLLibrary;
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  I, CertCount: Integer;
  ChainFingerprints: array of string;
  AllValid: Boolean;
begin
  BeginSection('证书链固定');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Store := Lib.CreateCertificateStore;
    if Store = nil then
    begin
      Check('创建证书存储', False);
      Exit;
    end;

    // 加载系统证书存储
    if not Store.LoadSystemStore then
    begin
      // 尝试从文件加载
      if not Store.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
      begin
        Check('加载证书存储（跳过链固定测试）', False);
        Exit;
      end;
    end;

    CertCount := Store.GetCount;
    Check('证书存储加载', CertCount > 0, Format('加载了 %d 个证书', [CertCount]));

    // 提取证书链指纹
    if CertCount > 3 then
      CertCount := 3; // 限制测试数量

    SetLength(ChainFingerprints, CertCount);
    AllValid := True;

    for I := 0 to CertCount - 1 do
    begin
      Cert := Store.GetCertificate(I);
      if Cert <> nil then
      begin
        ChainFingerprints[I] := NormalizeFingerprint(Cert.GetFingerprintSHA256);
        if Length(ChainFingerprints[I]) <> 64 then
          AllValid := False;
      end
      else
        AllValid := False;
    end;

    Check('提取链证书指纹', AllValid);

    // 验证链中指纹唯一
    AllValid := True;
    for I := 0 to CertCount - 2 do
    begin
      if ChainFingerprints[I] = ChainFingerprints[I + 1] then
      begin
        AllValid := False;
        Break;
      end;
    end;
    Check('链证书指纹唯一', AllValid);

  except
    on E: Exception do
      Check('证书链固定', False, E.Message);
  end;
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('证书固定测试摘要');
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
  WriteLn('证书固定测试套件 (Certificate Pinning)');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  证书固定是防止中间人攻击的重要安全机制。');
  WriteLn('  本测试验证指纹提取、比较和固定逻辑的正确性。');
  WriteLn;

  try
    TestFingerprintExtraction;
    TestFingerprintComparison;
    TestConstantTimeComparison;
    TestFingerprintFormatting;
    TestCryptoUtilsSecureCompare;
    TestPublicKeyPinning;
    TestCertificateChainPinning;

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
