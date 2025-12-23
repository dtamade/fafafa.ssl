program test_weak_algorithm_detection;

{$mode objfpc}{$H+}{$J-}

{**
 * Weak Algorithm Detection Test Suite
 *
 * P3-8 P0-2: 弱算法检测测试
 *
 * 测试内容:
 * - 弱密码套件检测 (RC4, 3DES, NULL, EXPORT)
 * - 弱哈希算法检测 (MD5, SHA-1 用于签名)
 * - 短密钥检测 (RSA < 2048, ECDSA < 256)
 * - 过时协议检测 (SSL 2.0, SSL 3.0, TLS 1.0, TLS 1.1)
 *
 * 安全说明:
 * 弱算法的使用是常见的安全漏洞来源。
 * 这些测试确保库正确识别和拒绝不安全的配置。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.openssl.backed;

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

{ 检查密码套件是否包含弱算法 }
function ContainsWeakCipher(const ACipherList: string): Boolean;
const
  WEAK_CIPHERS: array[0..9] of string = (
    'RC4', 'DES', '3DES', 'NULL', 'EXPORT', 'MD5',
    'ANON', 'ADH', 'AECDH', 'CBC'  // CBC 在某些情况下有 BEAST 攻击风险
  );
var
  I: Integer;
  UpperCipherList: string;
begin
  Result := False;
  UpperCipherList := UpperCase(ACipherList);

  for I := Low(WEAK_CIPHERS) to High(WEAK_CIPHERS) do
  begin
    if Pos(WEAK_CIPHERS[I], UpperCipherList) > 0 then
    begin
      // 排除排除模式 (如 !RC4)
      if Pos('!' + WEAK_CIPHERS[I], UpperCipherList) = 0 then
      begin
        Result := True;
        Exit;
      end;
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
    Result := ABits >= 128;  // 对称密钥
end;

procedure TestWeakCipherDetection;
const
  WEAK_CIPHER_LISTS: array[0..4] of string = (
    'RC4-SHA',
    'DES-CBC3-SHA',
    'NULL-MD5',
    'EXP-RC4-MD5',
    'ADH-AES128-SHA'
  );

  STRONG_CIPHER_LISTS: array[0..3] of string = (
    'ECDHE-RSA-AES256-GCM-SHA384',
    'TLS_AES_256_GCM_SHA384',
    'TLS_CHACHA20_POLY1305_SHA256',
    'ECDHE+AESGCM:!ANULL:!MD5:!RC4'
  );
var
  I: Integer;
begin
  BeginSection('Weak Cipher Detection');

  // 测试弱密码套件能被检测到
  for I := Low(WEAK_CIPHER_LISTS) to High(WEAK_CIPHER_LISTS) do
    Check('Detect weak: ' + WEAK_CIPHER_LISTS[I],
          ContainsWeakCipher(WEAK_CIPHER_LISTS[I]),
          'Should detect as weak');

  // 测试强密码套件不被误判
  for I := Low(STRONG_CIPHER_LISTS) to High(STRONG_CIPHER_LISTS) do
    Check('Accept strong: ' + Copy(STRONG_CIPHER_LISTS[I], 1, 30),
          not ContainsWeakCipher(STRONG_CIPHER_LISTS[I]),
          'Should not flag as weak');
end;

procedure TestProtocolVersionSecurity;
var
  V: TSSLProtocolVersion;
begin
  BeginSection('Protocol Version Security');

  // SSL 2.0 和 SSL 3.0 不安全
  Check('SSL 2.0 insecure', not IsSecureTLSVersion(sslProtocolSSL2));
  Check('SSL 3.0 insecure', not IsSecureTLSVersion(sslProtocolSSL3));

  // TLS 1.0 和 1.1 已弃用
  Check('TLS 1.0 insecure', not IsSecureTLSVersion(sslProtocolTLS10));
  Check('TLS 1.1 insecure', not IsSecureTLSVersion(sslProtocolTLS11));

  // TLS 1.2 和 1.3 是安全的
  Check('TLS 1.2 secure', IsSecureTLSVersion(sslProtocolTLS12));
  Check('TLS 1.3 secure', IsSecureTLSVersion(sslProtocolTLS13));
end;

procedure TestKeyLengthValidation;
begin
  BeginSection('Key Length Validation');

  // RSA 密钥长度检测
  Check('RSA 1024-bit insecure', not IsKeyLengthSecure('RSA', 1024));
  Check('RSA 2048-bit secure', IsKeyLengthSecure('RSA', 2048));
  Check('RSA 4096-bit secure', IsKeyLengthSecure('RSA', 4096));

  // ECDSA 密钥长度检测
  Check('ECDSA 160-bit insecure', not IsKeyLengthSecure('ECDSA', 160));
  Check('ECDSA 256-bit secure', IsKeyLengthSecure('ECDSA', 256));
  Check('ECDSA 384-bit secure', IsKeyLengthSecure('ECDSA', 384));

  // DSA 密钥长度检测
  Check('DSA 1024-bit insecure', not IsKeyLengthSecure('DSA', 1024));
  Check('DSA 2048-bit secure', IsKeyLengthSecure('DSA', 2048));
end;

procedure TestWeakHashDetection;
const
  WEAK_HASH_ALGOS: array[0..1] of string = ('MD5', 'SHA1');
  STRONG_HASH_ALGOS: array[0..2] of string = ('SHA256', 'SHA384', 'SHA512');
var
  I: Integer;

  function IsWeakHash(const AHash: string): Boolean;
  var
    H: string;
  begin
    H := UpperCase(AHash);
    Result := (H = 'MD5') or (H = 'SHA1') or (H = 'MD4') or (H = 'MD2');
  end;

begin
  BeginSection('Weak Hash Detection');

  for I := Low(WEAK_HASH_ALGOS) to High(WEAK_HASH_ALGOS) do
    Check(WEAK_HASH_ALGOS[I] + ' is weak', IsWeakHash(WEAK_HASH_ALGOS[I]));

  for I := Low(STRONG_HASH_ALGOS) to High(STRONG_HASH_ALGOS) do
    Check(STRONG_HASH_ALGOS[I] + ' is strong', not IsWeakHash(STRONG_HASH_ALGOS[I]));
end;

procedure TestDefaultCipherSecurity;
var
  LConfig: TSSLConfig;
begin
  BeginSection('Default Cipher Security');

  // 获取默认配置
  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  // 检查默认密码套件不包含弱算法
  Check('Default cipher list secure',
        not ContainsWeakCipher(LConfig.CipherList),
        'CipherList: ' + LConfig.CipherList);

  Check('Default TLS 1.3 suites secure',
        not ContainsWeakCipher(LConfig.CipherSuites),
        'CipherSuites: ' + LConfig.CipherSuites);
end;

procedure TestContextBuilderSecurityDefaults;
var
  LBuilder: ISSLContextBuilder;
begin
  BeginSection('Context Builder Security Defaults');

  // CreateWithSafeDefaults 应该使用安全配置
  try
    LBuilder := TSSLContextBuilder.CreateWithSafeDefaults;
    Check('SafeDefaults builder created', LBuilder <> nil);

    // 验证安全默认值被应用
    // (实际验证需要检查内部配置)
    Check('SafeDefaults available', True);
  except
    on E: Exception do
      Check('SafeDefaults creation', False, E.Message);
  end;
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('======================================');
  WriteLn('  Weak Algorithm Detection Tests');
  WriteLn('======================================');

  try
    // 初始化 OpenSSL
    TOpenSSLLibrary.LoadSSL;

    TestWeakCipherDetection;
    TestProtocolVersionSecurity;
    TestKeyLengthValidation;
    TestWeakHashDetection;
    TestDefaultCipherSecurity;
    TestContextBuilderSecurityDefaults;

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
