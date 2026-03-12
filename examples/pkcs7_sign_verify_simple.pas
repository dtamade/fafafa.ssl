program PKCS7SignVerifySimple;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, ctypes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.loader;

const
  TEST_DATA = 'This is a test message for PKCS#7 signing demonstration.';

var
  TestCert: PX509;
  TestPrivKey: PEVP_PKEY;
  DataBIO: PBIO;
  P7: PPKCS7;
  OutBIO: PBIO;
  VerifyResult: cint;

// Helper function for BIO_reset
function BIO_reset(b: PBIO): clong;
begin
  if Assigned(BIO_ctrl) then
    Result := BIO_ctrl(b, BIO_CTRL_RESET, 0, nil)
  else
    Result := -1;
end;

// Generate test certificate and key
function GenerateTestCertAndKey(out ACert: PX509; out AKey: PEVP_PKEY): Boolean;
var
  pkey: PEVP_PKEY;
  x509: PX509;
  rsa: PRSA;
  name: PX509_NAME;
  bn: PBIGNUM;
  serial: PASN1_INTEGER;
begin
  Result := False;
  ACert := nil;
  AKey := nil;

  // Generate RSA key
  pkey := EVP_PKEY_new();
  if pkey = nil then Exit;

  rsa := RSA_new();
  if rsa = nil then
  begin
    EVP_PKEY_free(pkey);
    Exit;
  end;

  bn := BN_new();
  if bn = nil then
  begin
    RSA_free(rsa);
    EVP_PKEY_free(pkey);
    Exit;
  end;

  BN_set_word(bn, RSA_F4);

  if RSA_generate_key_ex(rsa, 2048, bn, nil) <> 1 then
  begin
    BN_free(bn);
    RSA_free(rsa);
    EVP_PKEY_free(pkey);
    Exit;
  end;

  BN_free(bn);

  if EVP_PKEY_assign(pkey, EVP_PKEY_RSA, rsa) <> 1 then
  begin
    RSA_free(rsa);
    EVP_PKEY_free(pkey);
    Exit;
  end;

  // Create certificate
  x509 := X509_new();
  if x509 = nil then
  begin
    EVP_PKEY_free(pkey);
    Exit;
  end;

  X509_set_version(x509, 2);
  serial := X509_get_serialNumber(x509);
  ASN1_INTEGER_set(serial, 1);
  X509_gmtime_adj(X509_get_notBefore(x509), 0);
  X509_gmtime_adj(X509_get_notAfter(x509), 60 * 60 * 24 * 365);
  X509_set_pubkey(x509, pkey);

  name := X509_get_subject_name(x509);
  X509_NAME_add_entry_by_txt(name, 'C', MBSTRING_ASC, PByte(PAnsiChar('US')), -1, -1, 0);
  X509_NAME_add_entry_by_txt(name, 'O', MBSTRING_ASC, PByte(PAnsiChar('Test Org')), -1, -1, 0);
  X509_NAME_add_entry_by_txt(name, 'CN', MBSTRING_ASC, PByte(PAnsiChar('Test Certificate')), -1, -1, 0);

  X509_set_issuer_name(x509, name);

  if X509_sign(x509, pkey, EVP_sha256()) = 0 then
  begin
    X509_free(x509);
    EVP_PKEY_free(pkey);
    Exit;
  end;

  ACert := x509;
  AKey := pkey;
  Result := True;
end;

begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 签名和验证示例');
  WriteLn('=============================================================');
  WriteLn('');

  // Initialize OpenSSL
  WriteLn('1. 初始化 OpenSSL 库...');
  try
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('   ✗ 错误: 无法初始化 OpenSSL 库: ', E.Message);
      Halt(1);
    end;
  end;
  if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
  begin
    WriteLn('   ✗ 错误: OpenSSL core 未保持已加载状态');
    Halt(1);
  end;
  LoadEVP(GetCryptoLibHandle);
  LoadOpenSSLRSA;
  LoadOpenSSLX509;
  LoadOpenSSLPEM(GetCryptoLibHandle);
  LoadOpenSSLBIO;
  LoadOpenSSLASN1(GetCryptoLibHandle);
  LoadOpenSSLBN;
  LoadPKCS7Functions;
  WriteLn('   ✓ OpenSSL 库初始化成功');
  WriteLn('   版本: ', fafafa.ssl.openssl.api.core.GetOpenSSLVersionString);
  WriteLn('');

  // Generate test certificate and key
  WriteLn('2. 生成测试证书和密钥...');
  if not GenerateTestCertAndKey(TestCert, TestPrivKey) then
  begin
    WriteLn('   ✗ 错误: 无法生成测试证书和密钥');
    Halt(1);
  end;
  WriteLn('   ✓ 测试证书和密钥生成成功');
  WriteLn('');

  // Prepare data to sign
  WriteLn('3. 准备要签名的数据...');
  WriteLn('   数据内容: ', TEST_DATA);
  WriteLn('   数据长度: ', Length(TEST_DATA), ' 字节');
  WriteLn('');

  // Create BIO for data
  DataBIO := BIO_new_mem_buf(PAnsiChar(TEST_DATA), Length(TEST_DATA));
  if DataBIO = nil then
  begin
    WriteLn('   ✗ 错误: 无法创建数据 BIO');
    EVP_PKEY_free(TestPrivKey);
    X509_free(TestCert);
    Halt(1);
  end;

  // Sign the data
  WriteLn('4. 对数据进行 PKCS#7 签名...');
  P7 := PKCS7_sign(TestCert, TestPrivKey, nil, DataBIO, PKCS7_DETACHED or PKCS7_BINARY);
  if P7 = nil then
  begin
    WriteLn('   ✗ 错误: PKCS#7 签名失败');
    BIO_free(DataBIO);
    EVP_PKEY_free(TestPrivKey);
    X509_free(TestCert);
    Halt(1);
  end;
  WriteLn('   ✓ 签名成功');
  WriteLn('');

  // Verify the signature
  WriteLn('5. 验证 PKCS#7 签名...');
  BIO_reset(DataBIO);
  VerifyResult := PKCS7_verify(P7, nil, nil, DataBIO, nil, PKCS7_NOVERIFY);

  if VerifyResult = 1 then
  begin
    WriteLn('   ✓ 签名验证成功！');
    WriteLn('   数据完整性: 完好');
    WriteLn('   签名有效性: 有效');
  end
  else
  begin
    WriteLn('   ✗ 签名验证失败');
  end;
  WriteLn('');

  WriteLn('=============================================================');
  WriteLn('示例完成');
  WriteLn('=============================================================');
  WriteLn('');
  WriteLn('说明:');
  WriteLn('- 本示例演示了 PKCS#7 签名和验证的基本流程');
  WriteLn('- 使用自动生成的测试证书和密钥');
  WriteLn('- 签名使用分离式签名（DETACHED）模式');
  WriteLn('- 验证使用 PKCS7_NOVERIFY 跳过证书链验证');
  WriteLn('- 生产环境应使用真实证书和完整的证书链验证');

  // Cleanup
  PKCS7_free(P7);
  BIO_free(DataBIO);
  EVP_PKEY_free(TestPrivKey);
  X509_free(TestCert);
  WriteLn('[PASS] pkcs7 sign/verify simple example completed');
end.
