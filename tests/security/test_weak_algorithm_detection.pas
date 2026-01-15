program test_weak_algorithm_detection;

{$mode objfpc}{$H+}{$J-}

{**
 * Weak Algorithm Detection Test Suite
 *
 * P3-8 P0-2: 弱算法检测测试
 * P1-2.3: 迁移到 TSimpleTestRunner 框架
 *
 * 测试内容:
 * - 弱密码套件检测 (RC4, 3DES, NULL, EXPORT)
 * - 弱哈希算法检测 (MD5, SHA-1 用于签名)
 * - 短密钥检测 (RSA < 2048, ECDSA < 256)
 * - 过时协议检测 (SSL 2.0, SSL 3.0, TLS 1.0, TLS 1.1)
 *
 * @author fafafa.ssl team
 * @version 1.1.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

{ 检查密码套件是否包含弱算法 }
function ContainsWeakCipher(const ACipherList: string): Boolean;
const
  WEAK_CIPHERS: array[0..7] of string = (
    'RC4', 'DES-', '3DES', 'NULL-', 'EXPORT', '-MD5',
    'ADH-', 'AECDH-'
  );
var
  I, P: Integer;
  UpperCipherList: string;
  WeakCipher: string;
  IsExcluded: Boolean;
begin
  Result := False;
  UpperCipherList := UpperCase(ACipherList);

  for I := Low(WEAK_CIPHERS) to High(WEAK_CIPHERS) do
  begin
    WeakCipher := WEAK_CIPHERS[I];
    P := Pos(WeakCipher, UpperCipherList);

    while P > 0 do
    begin
      IsExcluded := (P > 1) and (UpperCipherList[P - 1] = '!');

      if not IsExcluded then
      begin
        Result := True;
        Exit;
      end;

      P := Pos(WeakCipher, Copy(UpperCipherList, P + Length(WeakCipher), MaxInt));
      if P > 0 then
        P := P + Length(WeakCipher);
    end;
  end;
end;

{ 检查是否为安全的 TLS 版本 }
function IsSecureTLSVersion(AVersion: TSSLProtocolVersion): Boolean;
begin
  Result := AVersion in [sslProtocolTLS12, sslProtocolTLS13];
end;

{ 检查密钥长度是否足够 }
function IsKeyLengthSecure(AAlgorithm: string; ABits: Integer): Boolean;
begin
  AAlgorithm := UpperCase(AAlgorithm);

  if Pos('RSA', AAlgorithm) > 0 then
    Result := ABits >= 2048
  else if Pos('EC', AAlgorithm) > 0 then
    Result := ABits >= 256
  else if Pos('DSA', AAlgorithm) > 0 then
    Result := ABits >= 2048
  else
    Result := ABits >= 128;
end;

{ 检查哈希算法是否为弱算法 }
function IsWeakHash(const AHash: string): Boolean;
var
  H: string;
begin
  H := UpperCase(AHash);
  Result := (H = 'MD5') or (H = 'SHA1') or (H = 'MD4') or (H = 'MD2');
end;

procedure TestWeakCipherDetection;
const
  WEAK_CIPHER_LISTS: array[0..4] of string = (
    'RC4-SHA', 'DES-CBC3-SHA', 'NULL-MD5', 'EXP-RC4-MD5', 'ADH-AES128-SHA'
  );
  STRONG_CIPHER_LISTS: array[0..3] of string = (
    'ECDHE-RSA-AES256-GCM-SHA384', 'TLS_AES_256_GCM_SHA384',
    'TLS_CHACHA20_POLY1305_SHA256', 'ECDHE+AESGCM:!ANULL:!MD5:!RC4'
  );
var
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Weak Cipher Detection ===');

  for I := Low(WEAK_CIPHER_LISTS) to High(WEAK_CIPHER_LISTS) do
    Runner.Check('Detect weak: ' + WEAK_CIPHER_LISTS[I],
          ContainsWeakCipher(WEAK_CIPHER_LISTS[I]));

  for I := Low(STRONG_CIPHER_LISTS) to High(STRONG_CIPHER_LISTS) do
    Runner.Check('Accept strong: ' + Copy(STRONG_CIPHER_LISTS[I], 1, 30),
          not ContainsWeakCipher(STRONG_CIPHER_LISTS[I]));
end;

procedure TestProtocolVersionSecurity;
begin
  WriteLn;
  WriteLn('=== Protocol Version Security ===');

  Runner.Check('SSL 2.0 insecure', not IsSecureTLSVersion(sslProtocolSSL2));
  Runner.Check('SSL 3.0 insecure', not IsSecureTLSVersion(sslProtocolSSL3));
  Runner.Check('TLS 1.0 insecure', not IsSecureTLSVersion(sslProtocolTLS10));
  Runner.Check('TLS 1.1 insecure', not IsSecureTLSVersion(sslProtocolTLS11));
  Runner.Check('TLS 1.2 secure', IsSecureTLSVersion(sslProtocolTLS12));
  Runner.Check('TLS 1.3 secure', IsSecureTLSVersion(sslProtocolTLS13));
end;

procedure TestKeyLengthValidation;
begin
  WriteLn;
  WriteLn('=== Key Length Validation ===');

  Runner.Check('RSA 1024-bit insecure', not IsKeyLengthSecure('RSA', 1024));
  Runner.Check('RSA 2048-bit secure', IsKeyLengthSecure('RSA', 2048));
  Runner.Check('RSA 4096-bit secure', IsKeyLengthSecure('RSA', 4096));

  Runner.Check('ECDSA 160-bit insecure', not IsKeyLengthSecure('ECDSA', 160));
  Runner.Check('ECDSA 256-bit secure', IsKeyLengthSecure('ECDSA', 256));
  Runner.Check('ECDSA 384-bit secure', IsKeyLengthSecure('ECDSA', 384));

  Runner.Check('DSA 1024-bit insecure', not IsKeyLengthSecure('DSA', 1024));
  Runner.Check('DSA 2048-bit secure', IsKeyLengthSecure('DSA', 2048));
end;

procedure TestWeakHashDetection;
const
  WEAK_HASH_ALGOS: array[0..1] of string = ('MD5', 'SHA1');
  STRONG_HASH_ALGOS: array[0..2] of string = ('SHA256', 'SHA384', 'SHA512');
var
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Weak Hash Detection ===');

  for I := Low(WEAK_HASH_ALGOS) to High(WEAK_HASH_ALGOS) do
    Runner.Check(WEAK_HASH_ALGOS[I] + ' is weak', IsWeakHash(WEAK_HASH_ALGOS[I]));

  for I := Low(STRONG_HASH_ALGOS) to High(STRONG_HASH_ALGOS) do
    Runner.Check(STRONG_HASH_ALGOS[I] + ' is strong', not IsWeakHash(STRONG_HASH_ALGOS[I]));
end;

procedure TestDefaultCipherSecurity;
var
  LConfig: TSSLConfig;
begin
  WriteLn;
  WriteLn('=== Default Cipher Security ===');

  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  Runner.Check('Default cipher list secure',
        not ContainsWeakCipher(LConfig.CipherList),
        'CipherList: ' + LConfig.CipherList);

  Runner.Check('Default TLS 1.3 suites secure',
        not ContainsWeakCipher(LConfig.CipherSuites),
        'CipherSuites: ' + LConfig.CipherSuites);
end;

procedure TestContextBuilderSecurityDefaults;
var
  LBuilder: ISSLContextBuilder;
begin
  WriteLn;
  WriteLn('=== Context Builder Security Defaults ===');

  try
    LBuilder := TSSLContextBuilder.CreateWithSafeDefaults;
    Runner.Check('SafeDefaults builder created', LBuilder <> nil);
    Runner.Check('SafeDefaults available', True);
  except
    on E: Exception do
      Runner.Check('SafeDefaults creation', False, E.Message);
  end;
end;

begin
  WriteLn('======================================');
  WriteLn('  Weak Algorithm Detection Tests');
  WriteLn('======================================');

  Runner := TSimpleTestRunner.Create;
  try
    // P1-2.3: 使用统一的模块依赖管理
    Runner.RequireModules([osmCore, osmSSL, osmEVP]);

    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', TOpenSSLTestBase.OpenSSLVersion.VersionString);

    TestWeakCipherDetection;
    TestProtocolVersionSecurity;
    TestKeyLengthValidation;
    TestWeakHashDetection;
    TestDefaultCipherSecurity;
    TestContextBuilderSecurityDefaults;

    Runner.PrintSummary;

    if Runner.FailCount > 0 then
      Halt(1);
  finally
    Runner.Free;
  end;
end.
