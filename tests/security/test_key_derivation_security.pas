program test_key_derivation_security;

{$mode objfpc}{$H+}{$J-}

{**
 * Key Derivation Security Test Suite
 *
 * P3-8 P0-2: 密钥派生安全测试
 *
 * 测试内容:
 * - HKDF 参数验证
 * - PBKDF2 迭代次数验证
 * - 盐值长度验证
 * - 密钥派生函数输出长度验证
 *
 * 安全说明:
 * 密钥派生函数的安全性取决于正确的参数选择。
 * 迭代次数过低或盐值过短都会降低安全性。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.kdf;

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
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const AName: string);
begin
  Section := AName;
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

procedure Check(const AName: string; AOk: Boolean; const ADetails: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', AName, ': ');
  if AOk then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if ADetails <> '' then
      WriteLn('    ', ADetails);
  end;
end;

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
  BeginSection('PBKDF2 Iterations');

  // 测试不安全的迭代次数
  Check('1000 iterations insecure', not IsPBKDF2IterationsSecure(1000));
  Check('10000 iterations insecure', not IsPBKDF2IterationsSecure(10000));
  Check('50000 iterations insecure', not IsPBKDF2IterationsSecure(50000));

  // 测试安全的迭代次数
  Check('100000 iterations secure', IsPBKDF2IterationsSecure(100000));
  Check('150000 iterations secure', IsPBKDF2IterationsSecure(150000));
  Check('310000 iterations secure', IsPBKDF2IterationsSecure(310000));  // OWASP 2023 推荐
end;

procedure TestSaltLength;
begin
  BeginSection('Salt Length');

  // 测试不安全的盐值长度
  Check('4 bytes salt insecure', not IsSaltLengthSecure(4));
  Check('8 bytes salt insecure', not IsSaltLengthSecure(8));

  // 测试安全的盐值长度
  Check('16 bytes salt secure', IsSaltLengthSecure(16));
  Check('32 bytes salt secure', IsSaltLengthSecure(32));
  Check('64 bytes salt secure', IsSaltLengthSecure(64));
end;

procedure TestKeyLength;
begin
  BeginSection('Key Length');

  // 测试不安全的密钥长度
  Check('8 bytes key insecure', not IsKeyLengthSecure(8));

  // 测试安全的密钥长度
  Check('16 bytes key secure (AES-128)', IsKeyLengthSecure(16));
  Check('24 bytes key secure (AES-192)', IsKeyLengthSecure(24));
  Check('32 bytes key secure (AES-256)', IsKeyLengthSecure(32));
end;

procedure TestHKDFParameters;
begin
  BeginSection('HKDF Parameters');

  // 测试不安全的 HKDF 参数
  Check('Short salt (8) insecure', not IsHKDFParametersSecure(8, 32));
  Check('Short output (8) insecure', not IsHKDFParametersSecure(16, 8));

  // 测试安全的 HKDF 参数
  Check('16 salt + 32 output secure', IsHKDFParametersSecure(16, 32));
  Check('32 salt + 32 output secure', IsHKDFParametersSecure(32, 32));
  Check('16 salt + 16 output secure', IsHKDFParametersSecure(16, 16));
end;

procedure TestCryptoUtilsKDF;
var
  LPassword, LSalt: TBytes;
  LDerivedKey: TBytes;
begin
  BeginSection('CryptoUtils KDF');

  // 准备测试数据
  SetLength(LPassword, 16);
  FillChar(LPassword[0], 16, $AA);

  SetLength(LSalt, 16);
  FillChar(LSalt[0], 16, $BB);

  // 测试 PBKDF2 派生
  try
    LDerivedKey := TCryptoUtils.PBKDF2(LPassword, LSalt, 100000, 32, HASH_SHA256);
    Check('PBKDF2 derives 32-byte key', Length(LDerivedKey) = 32);
    Check('PBKDF2 output not zero', (Length(LDerivedKey) > 0) and (LDerivedKey[0] <> 0));
  except
    on E: Exception do
      Check('PBKDF2 execution', False, E.Message);
  end;

  // 测试 HKDF 派生（如果可用）
  try
    LDerivedKey := TCryptoUtils.HKDF(LPassword, LSalt, nil, 32, HASH_SHA256);
    Check('HKDF derives 32-byte key', Length(LDerivedKey) = 32);
  except
    on E: Exception do
    begin
      // HKDF 可能不可用于某些 OpenSSL 版本
      if Pos('not available', LowerCase(E.Message)) > 0 then
        Check('HKDF not available (expected)', True)
      else
        Check('HKDF execution', False, E.Message);
    end;
  end;
end;

procedure TestKDFDeterminism;
var
  LPassword, LSalt: TBytes;
  LKey1, LKey2: TBytes;
  I: Integer;
  AllSame: Boolean;
begin
  BeginSection('KDF Determinism');

  SetLength(LPassword, 16);
  FillChar(LPassword[0], 16, $CC);

  SetLength(LSalt, 16);
  FillChar(LSalt[0], 16, $DD);

  try
    // 派生两次，结果应该相同
    LKey1 := TCryptoUtils.PBKDF2(LPassword, LSalt, 100000, 32, HASH_SHA256);
    LKey2 := TCryptoUtils.PBKDF2(LPassword, LSalt, 100000, 32, HASH_SHA256);

    Check('PBKDF2 same length', Length(LKey1) = Length(LKey2));

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
      Check('PBKDF2 deterministic', AllSame);
    end
    else
      Check('PBKDF2 deterministic', False, 'Length mismatch');

  except
    on E: Exception do
      Check('KDF determinism test', False, E.Message);
  end;
end;

procedure TestKDFSaltVariation;
var
  LPassword, LSalt1, LSalt2: TBytes;
  LKey1, LKey2: TBytes;
  I: Integer;
  AllSame: Boolean;
begin
  BeginSection('KDF Salt Variation');

  SetLength(LPassword, 16);
  FillChar(LPassword[0], 16, $EE);

  SetLength(LSalt1, 16);
  FillChar(LSalt1[0], 16, $11);

  SetLength(LSalt2, 16);
  FillChar(LSalt2[0], 16, $22);

  try
    LKey1 := TCryptoUtils.PBKDF2(LPassword, LSalt1, 100000, 32, HASH_SHA256);
    LKey2 := TCryptoUtils.PBKDF2(LPassword, LSalt2, 100000, 32, HASH_SHA256);

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

    Check('Different salts produce different keys', not AllSame);

  except
    on E: Exception do
      Check('Salt variation test', False, E.Message);
  end;
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('======================================');
  WriteLn('  Key Derivation Security Tests');
  WriteLn('======================================');

  try
    // 初始化 OpenSSL
    TOpenSSLLibrary.LoadSSL;

    TestPBKDF2Iterations;
    TestSaltLength;
    TestKeyLength;
    TestHKDFParameters;
    TestCryptoUtilsKDF;
    TestKDFDeterminism;
    TestKDFSaltVariation;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.Message);
      Inc(Failed);
    end;
  end;

  WriteLn;
  WriteLn('======================================');
  WriteLn(Format('Total: %d  Passed: %d  Failed: %d', [Total, Passed, Failed]));
  WriteLn('======================================');

  if Failed > 0 then
    Halt(1);
end.
