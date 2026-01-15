program test_key_derivation_security;

{$mode objfpc}{$H+}{$J-}

{**
 * Key Derivation Security Test Suite
 *
 * P3-8 P0-2: 密钥派生安全测试 (已迁移到 TSimpleTestRunner)
 *
 * 测试内容:
 * - HKDF 参数验证
 * - PBKDF2 迭代次数验证
 * - 盐值长度验证
 * - 密钥派生函数输出长度验证
 *
 * @author fafafa.ssl team
 * @version 1.1.0 - P1-2.3 迁移到统一测试框架
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.hmac,
  fafafa.ssl.openssl.api.kdf,
  fafafa.ssl.openssl.loader,
  test_openssl_base;

const
  // NIST 推荐的最小参数
  MIN_PBKDF2_ITERATIONS = 100000;   // OWASP 2023 推荐
  MIN_SALT_LENGTH = 16;              // 128 bits
  MIN_KEY_LENGTH = 16;               // 128 bits (AES-128)
  RECOMMENDED_KEY_LENGTH = 32;       // 256 bits (AES-256)

  // HKDF 参数
  MIN_HKDF_SALT_LENGTH = 16;
  MIN_HKDF_INFO_LENGTH = 0;  // Info 可以为空

var
  Runner: TSimpleTestRunner;

{ 验证 PBKDF2 迭代次数是否安全 }
function IsPBKDF2IterationsSecure(AIterations: Integer): Boolean;
begin
  Result := AIterations >= MIN_PBKDF2_ITERATIONS;
end;

{ 验证盐值长度是否安全 }
function IsSaltLengthSecure(ASaltLength: Integer): Boolean;
begin
  Result := ASaltLength >= MIN_SALT_LENGTH;
end;

{ 验证派生密钥长度是否安全 }
function IsKeyLengthSecure(AKeyLength: Integer): Boolean;
begin
  Result := AKeyLength >= MIN_KEY_LENGTH;
end;

{ 验证 HKDF 参数是否安全 }
function IsHKDFParametersSecure(ASaltLength, AOutputLength: Integer): Boolean;
begin
  Result := (ASaltLength >= MIN_HKDF_SALT_LENGTH) and
            (AOutputLength >= MIN_KEY_LENGTH);
end;

procedure TestPBKDF2Iterations;
begin
  WriteLn;
  WriteLn('=== PBKDF2 Iterations ===');

  // 测试不安全的迭代次数
  Runner.Check('1000 iterations insecure', not IsPBKDF2IterationsSecure(1000));
  Runner.Check('10000 iterations insecure', not IsPBKDF2IterationsSecure(10000));
  Runner.Check('50000 iterations insecure', not IsPBKDF2IterationsSecure(50000));

  // 测试安全的迭代次数
  Runner.Check('100000 iterations secure', IsPBKDF2IterationsSecure(100000));
  Runner.Check('150000 iterations secure', IsPBKDF2IterationsSecure(150000));
  Runner.Check('310000 iterations secure', IsPBKDF2IterationsSecure(310000));  // OWASP 2023 推荐
end;

procedure TestSaltLength;
begin
  WriteLn;
  WriteLn('=== Salt Length ===');

  // 测试不安全的盐值长度
  Runner.Check('4 bytes salt insecure', not IsSaltLengthSecure(4));
  Runner.Check('8 bytes salt insecure', not IsSaltLengthSecure(8));

  // 测试安全的盐值长度
  Runner.Check('16 bytes salt secure', IsSaltLengthSecure(16));
  Runner.Check('32 bytes salt secure', IsSaltLengthSecure(32));
  Runner.Check('64 bytes salt secure', IsSaltLengthSecure(64));
end;

procedure TestKeyLength;
begin
  WriteLn;
  WriteLn('=== Key Length ===');

  // 测试不安全的密钥长度
  Runner.Check('8 bytes key insecure', not IsKeyLengthSecure(8));

  // 测试安全的密钥长度
  Runner.Check('16 bytes key secure (AES-128)', IsKeyLengthSecure(16));
  Runner.Check('24 bytes key secure (AES-192)', IsKeyLengthSecure(24));
  Runner.Check('32 bytes key secure (AES-256)', IsKeyLengthSecure(32));
end;

procedure TestHKDFParameters;
begin
  WriteLn;
  WriteLn('=== HKDF Parameters ===');

  // 测试不安全的 HKDF 参数
  Runner.Check('Short salt (8) insecure', not IsHKDFParametersSecure(8, 32));
  Runner.Check('Short output (8) insecure', not IsHKDFParametersSecure(16, 8));

  // 测试安全的 HKDF 参数
  Runner.Check('16 salt + 32 output secure', IsHKDFParametersSecure(16, 32));
  Runner.Check('32 salt + 32 output secure', IsHKDFParametersSecure(32, 32));
  Runner.Check('16 salt + 16 output secure', IsHKDFParametersSecure(16, 16));
end;

procedure TestCryptoUtilsKDF;
var
  LPassword: string;
  LSalt: TBytes;
  LDerivedKey: TBytes;
  LEmptyInfo: TBytes;
begin
  WriteLn;
  WriteLn('=== CryptoUtils KDF ===');

  // 准备测试数据
  LPassword := 'test_password_1234';

  SetLength(LSalt, 16);
  FillChar(LSalt[0], 16, $BB);

  // 测试 PBKDF2 派生
  try
    // 使用 DeriveKeyPBKDF2 (MD=nil 默认使用 SHA-256)
    LDerivedKey := DeriveKeyPBKDF2(LPassword, LSalt, 100000, 32, nil);
    Runner.Check('PBKDF2 derives 32-byte key', Length(LDerivedKey) = 32);
    Runner.Check('PBKDF2 output not zero', (Length(LDerivedKey) > 0) and (LDerivedKey[0] <> 0));
  except
    on E: Exception do
      Runner.Check('PBKDF2 execution', False, E.Message);
  end;

  // 测试 HKDF 派生（如果可用）
  SetLength(LEmptyInfo, 0);
  try
    // DeriveKeyHKDF 需要密钥作为 TBytes
    SetLength(LDerivedKey, 0);
    LDerivedKey := DeriveKeyHKDF(LSalt, LSalt, LEmptyInfo, 32, nil);
    Runner.Check('HKDF derives 32-byte key', Length(LDerivedKey) = 32);
  except
    on E: Exception do
    begin
      // HKDF 可能不可用于某些 OpenSSL 版本
      if Pos('not available', LowerCase(E.Message)) > 0 then
        Runner.Check('HKDF not available (expected)', True)
      else
        Runner.Check('HKDF execution', False, E.Message);
    end;
  end;
end;

procedure TestKDFDeterminism;
var
  LPassword: string;
  LSalt: TBytes;
  LKey1, LKey2: TBytes;
  I: Integer;
  AllSame: Boolean;
begin
  WriteLn;
  WriteLn('=== KDF Determinism ===');

  LPassword := 'determinism_test_password';

  SetLength(LSalt, 16);
  FillChar(LSalt[0], 16, $DD);

  try
    // 派生两次，结果应该相同
    LKey1 := DeriveKeyPBKDF2(LPassword, LSalt, 100000, 32, nil);
    LKey2 := DeriveKeyPBKDF2(LPassword, LSalt, 100000, 32, nil);

    Runner.Check('PBKDF2 same length', Length(LKey1) = Length(LKey2));

    if Length(LKey1) = Length(LKey2) then
    begin
      AllSame := True;
      for I := 0 to Length(LKey1) - 1 do
      begin
        if LKey1[I] <> LKey2[I] then
        begin
          AllSame := False;
          Break;
        end;
      end;
      Runner.Check('PBKDF2 deterministic', AllSame);
    end
    else
      Runner.Check('PBKDF2 deterministic', False, 'Length mismatch');

  except
    on E: Exception do
      Runner.Check('KDF determinism test', False, E.Message);
  end;
end;

procedure TestKDFSaltVariation;
var
  LPassword: string;
  LSalt1, LSalt2: TBytes;
  LKey1, LKey2: TBytes;
  I: Integer;
  AllSame: Boolean;
begin
  WriteLn;
  WriteLn('=== KDF Salt Variation ===');

  LPassword := 'salt_variation_test_password';

  SetLength(LSalt1, 16);
  FillChar(LSalt1[0], 16, $11);

  SetLength(LSalt2, 16);
  FillChar(LSalt2[0], 16, $22);

  try
    LKey1 := DeriveKeyPBKDF2(LPassword, LSalt1, 100000, 32, nil);
    LKey2 := DeriveKeyPBKDF2(LPassword, LSalt2, 100000, 32, nil);

    // 不同的盐应该产生不同的密钥
    AllSame := True;
    if Length(LKey1) = Length(LKey2) then
    begin
      for I := 0 to Length(LKey1) - 1 do
      begin
        if LKey1[I] <> LKey2[I] then
        begin
          AllSame := False;
          Break;
        end;
      end;
    end
    else
      AllSame := False;

    Runner.Check('Different salts produce different keys', not AllSame);

  except
    on E: Exception do
      Runner.Check('Salt variation test', False, E.Message);
  end;
end;

begin
  WriteLn('======================================');
  WriteLn('  Key Derivation Security Tests');
  WriteLn('======================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore, osmEVP]);
    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    // 加载 KDF 函数
    LoadKDFFunctions;
    // 加载 HMAC 函数（HKDF 依赖 HMAC）
    LoadOpenSSLHMAC;

    TestPBKDF2Iterations;
    TestSaltLength;
    TestKeyLength;
    TestHKDFParameters;
    TestCryptoUtilsKDF;
    TestKDFDeterminism;
    TestKDFSaltVariation;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
