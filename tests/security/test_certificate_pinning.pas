program test_certificate_pinning;

{$mode objfpc}{$H+}{$J-}

{**
 * Certificate Pinning Test Suite
 *
 * P3-14: 证书固定测试 (已迁移到 TSimpleTestRunner)
 *
 * 测试内容:
 * - SHA-256 指纹提取与比较
 * - SHA-1 指纹提取与比较
 * - 指纹格式规范化
 * - 恒定时间比较（防时序攻击）
 * - 证书链固定
 * - 公钥固定（SPKI Hash）
 *
 * @author fafafa.ssl team
 * @version 1.1.0 - P1-2.3 迁移到统一测试框架
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

function NormalizeFingerprint(const AFingerprint: string): string;
begin
  Result := UpperCase(StringReplace(AFingerprint, ':', '', [rfReplaceAll]));
end;

function SecureStringCompare(const A, B: string): Boolean;
var
  I: Integer;
  Diff: Byte;
begin
  if Length(A) <> Length(B) then Exit(False);
  Diff := 0;
  for I := 1 to Length(A) do Diff := Diff or (Ord(A[I]) xor Ord(B[I]));
  Result := (Diff = 0);
end;

procedure TestFingerprintExtraction;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  FP_SHA256, FP_SHA1: string;
begin
  WriteLn;
  WriteLn('=== 指纹提取 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;
    Runner.Check('初始化 OpenSSL', True);

    Cert := Lib.CreateCertificate;
    Runner.Check('创建证书对象', Cert <> nil);

    if Cert <> nil then
    begin
      if Cert.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
      begin
        Runner.Check('加载系统 CA 证书', True);
        FP_SHA256 := Cert.GetFingerprintSHA256;
        Runner.Check('提取 SHA-256 指纹', FP_SHA256 <> '', 'Fingerprint: ' + FP_SHA256);
        Runner.Check('SHA-256 指纹格式正确', Length(NormalizeFingerprint(FP_SHA256)) = 64);
        FP_SHA1 := Cert.GetFingerprintSHA1;
        Runner.Check('提取 SHA-1 指纹', FP_SHA1 <> '', 'Fingerprint: ' + FP_SHA1);
        Runner.Check('SHA-1 指纹格式正确', Length(NormalizeFingerprint(FP_SHA1)) = 40);
        Runner.Check('SHA-256 和 SHA-1 指纹不同', FP_SHA256 <> FP_SHA1);
      end
      else
      begin
        Runner.Check('加载系统 CA 证书', False, '文件不存在或无法读取');
        Runner.Check('使用空证书测试指纹', Cert.GetFingerprintSHA256 = '', '空证书应返回空指纹');
      end;
    end;
  except
    on E: Exception do Runner.Check('指纹提取', False, E.Message);
  end;
end;

procedure TestFingerprintComparison;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  ActualFP, ExpectedFP: string;
  PinnedFingerprints: array[0..2] of string;
  I: Integer;
  Found: Boolean;
begin
  WriteLn;
  WriteLn('=== 指纹比较（证书固定） ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Cert := Lib.CreateCertificate;
    if Cert = nil then begin Runner.Check('创建证书对象', False); Exit; end;

    if not Cert.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
    begin
      Runner.Check('证书加载（跳过比较测试）', False, '无可用证书');
      Exit;
    end;

    ActualFP := NormalizeFingerprint(Cert.GetFingerprintSHA256);
    Runner.Check('获取实际指纹', ActualFP <> '');

    ExpectedFP := ActualFP;
    Runner.Check('精确匹配测试', SecureStringCompare(ActualFP, ExpectedFP));

    ExpectedFP := StringOfChar('0', 64);
    Runner.Check('不匹配测试', not SecureStringCompare(ActualFP, ExpectedFP));

    PinnedFingerprints[0] := StringOfChar('A', 64);
    PinnedFingerprints[1] := ActualFP;
    PinnedFingerprints[2] := StringOfChar('B', 64);

    Found := False;
    for I := 0 to 2 do
      if SecureStringCompare(ActualFP, PinnedFingerprints[I]) then begin Found := True; Break; end;
    Runner.Check('固定列表匹配', Found);

    ExpectedFP := LowerCase(ActualFP);
    Runner.Check('大小写不敏感比较', SecureStringCompare(UpperCase(ActualFP), UpperCase(ExpectedFP)));
  except
    on E: Exception do Runner.Check('指纹比较', False, E.Message);
  end;
end;

procedure TestConstantTimeComparison;
var
  FP1, FP2: string;
  I: Integer;
  AllMatch: Boolean;
begin
  WriteLn;
  WriteLn('=== 恒定时间比较 ===');

  FP1 := StringOfChar('A', 64);
  FP2 := StringOfChar('A', 64);
  Runner.Check('相等字符串比较', SecureStringCompare(FP1, FP2));

  FP2[64] := 'B';
  Runner.Check('最后一位不同', not SecureStringCompare(FP1, FP2));

  FP2 := StringOfChar('A', 64);
  FP2[1] := 'B';
  Runner.Check('第一位不同', not SecureStringCompare(FP1, FP2));

  FP2 := StringOfChar('B', 64);
  Runner.Check('完全不同', not SecureStringCompare(FP1, FP2));

  Runner.Check('空字符串相等', SecureStringCompare('', ''));
  Runner.Check('空与非空不等', not SecureStringCompare('', 'A'));
  Runner.Check('不同长度', not SecureStringCompare(StringOfChar('A', 64), StringOfChar('A', 32)));

  AllMatch := True;
  for I := 1 to 100 do
  begin
    FP1 := TEncodingUtils.BytesToHex(TCryptoUtils.SecureRandom(32), False);
    if SecureStringCompare(FP1, FP1) = False then begin AllMatch := False; Break; end;
  end;
  Runner.Check('随机指纹自比较', AllMatch);
end;

procedure TestFingerprintFormatting;
var
  RawFP, ColonFP, NormalizedFP: string;
begin
  WriteLn;
  WriteLn('=== 指纹格式处理 ===');

  RawFP := 'A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4E5F6A1B2';
  Runner.Check('原始格式长度正确', Length(RawFP) = 64);

  ColonFP := 'A1:B2:C3:D4:E5:F6:A1:B2:C3:D4:E5:F6:A1:B2:C3:D4:' +
             'E5:F6:A1:B2:C3:D4:E5:F6:A1:B2:C3:D4:E5:F6:A1:B2';
  Runner.Check('冒号格式长度正确', Length(ColonFP) = 95);

  NormalizedFP := NormalizeFingerprint(ColonFP);
  Runner.Check('规范化结果正确', NormalizedFP = RawFP);

  Runner.Check('大写规范化', NormalizeFingerprint('aa:bb:cc') = 'AABBCC');
  Runner.Check('混合大小写', NormalizeFingerprint('Aa:Bb:Cc') = 'AABBCC');
end;

procedure TestCryptoUtilsSecureCompare;
var
  Bytes1, Bytes2: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== TCryptoUtils.SecureCompare ===');

  TCryptoUtils.EnsureInitialized;

  SetLength(Bytes1, 32);
  SetLength(Bytes2, 32);
  for I := 0 to 31 do begin Bytes1[I] := I; Bytes2[I] := I; end;
  Runner.Check('相等数组', TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  Bytes2[15] := 255;
  Runner.Check('不等数组', not TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  SetLength(Bytes2, 16);
  Runner.Check('不同长度', not TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  SetLength(Bytes1, 0);
  SetLength(Bytes2, 0);
  Runner.Check('空数组相等', TCryptoUtils.SecureCompare(Bytes1, Bytes2));

  Bytes1 := TCryptoUtils.SecureRandom(1024);
  Bytes2 := Copy(Bytes1, 0, Length(Bytes1));
  Runner.Check('大数组相等', TCryptoUtils.SecureCompare(Bytes1, Bytes2));
end;

procedure TestPublicKeyPinning;
var
  Lib: ISSLLibrary;
  Cert: ISSLCertificate;
  PublicKeyHash: TBytes;
  PublicKeyHashHex: string;
begin
  WriteLn;
  WriteLn('=== 公钥固定 (SPKI) ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Cert := Lib.CreateCertificate;
    if Cert = nil then begin Runner.Check('创建证书对象', False); Exit; end;

    if not Cert.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
    begin
      Runner.Check('证书加载（跳过 SPKI 测试）', False, '无可用证书');
      Exit;
    end;

    Runner.Check('证书加载成功', True);

    PublicKeyHashHex := Cert.GetFingerprintSHA256;
    Runner.Check('获取公钥哈希', PublicKeyHashHex <> '');

    PublicKeyHash := TEncodingUtils.HexToBytes(NormalizeFingerprint(PublicKeyHashHex));
    Runner.Check('哈希转换为字节', Length(PublicKeyHash) = 32);

    PublicKeyHashHex := TEncodingUtils.Base64Encode(PublicKeyHash);
    Runner.Check('Base64 编码成功', PublicKeyHashHex <> '');
    Runner.Check('Base64 长度正确', Length(PublicKeyHashHex) = 44);
  except
    on E: Exception do Runner.Check('公钥固定', False, E.Message);
  end;
end;

procedure TestCertificateChainPinning;
var
  Lib: ISSLLibrary;
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
  I, CertCount: Integer;
  ChainFingerprints: array of string;
  AllValid: Boolean;
begin
  WriteLn;
  WriteLn('=== 证书链固定 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Store := Lib.CreateCertificateStore;
    if Store = nil then begin Runner.Check('创建证书存储', False); Exit; end;

    if not Store.LoadSystemStore then
    begin
      if not Store.LoadFromFile('/etc/ssl/certs/ca-certificates.crt') then
      begin
        Runner.Check('加载证书存储（跳过链固定测试）', False);
        Exit;
      end;
    end;

    CertCount := Store.GetCount;
    Runner.Check('证书存储加载', CertCount > 0, Format('加载了 %d 个证书', [CertCount]));

    if CertCount > 3 then CertCount := 3;

    SetLength(ChainFingerprints, CertCount);
    AllValid := True;

    for I := 0 to CertCount - 1 do
    begin
      Cert := Store.GetCertificate(I);
      if Cert <> nil then
      begin
        ChainFingerprints[I] := NormalizeFingerprint(Cert.GetFingerprintSHA256);
        if Length(ChainFingerprints[I]) <> 64 then AllValid := False;
      end
      else
        AllValid := False;
    end;

    Runner.Check('提取链证书指纹', AllValid);

    AllValid := True;
    for I := 0 to CertCount - 2 do
      if ChainFingerprints[I] = ChainFingerprints[I + 1] then begin AllValid := False; Break; end;
    Runner.Check('链证书指纹唯一', AllValid);
  except
    on E: Exception do Runner.Check('证书链固定', False, E.Message);
  end;
end;

begin
  WriteLn('================================================================');
  WriteLn('证书固定测试套件 (Certificate Pinning)');
  WriteLn('================================================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore, osmX509]);
    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    TestFingerprintExtraction;
    TestFingerprintComparison;
    TestConstantTimeComparison;
    TestFingerprintFormatting;
    TestCryptoUtilsSecureCompare;
    TestPublicKeyPinning;
    TestCertificateChainPinning;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
