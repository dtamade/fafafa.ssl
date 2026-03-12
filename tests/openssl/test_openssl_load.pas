program test_openssl_load;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

uses
  SysUtils,
  {$IFDEF UNIX}cthreads,{$ENDIF}
  // OpenSSL 类型和常量
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  // 核心模块
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.x509,
  // 加密算法模块
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.dsa,
  fafafa.ssl.openssl.api.dh,
  fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.ecdsa,
  fafafa.ssl.openssl.api.ecdh,
  // 哈希算法模块
  fafafa.ssl.openssl.api.sha,
  fafafa.ssl.openssl.api.sha3,
  fafafa.ssl.openssl.api.md,
  fafafa.ssl.openssl.api.blake2,
  fafafa.ssl.openssl.api.hmac,
  // 对称加密模块
  fafafa.ssl.openssl.api.aes,
  fafafa.ssl.openssl.api.des,
  fafafa.ssl.openssl.api.chacha,
  // 证书和编码模块
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.x509v3,
  // 辅助模块
  fafafa.ssl.openssl.api.rand,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.obj,
  fafafa.ssl.openssl.api.buffer,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.api.lhash,
  // 扩展模块
  fafafa.ssl.openssl.api.cms,
  fafafa.ssl.openssl.api.ts,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.kdf,
  fafafa.ssl.openssl.api.store,
  fafafa.ssl.openssl.api.provider,
  fafafa.ssl.openssl.api.conf,
  fafafa.ssl.openssl.api.ui,
  fafafa.ssl.openssl.api.engine,
  fafafa.ssl.openssl.api.comp,
  fafafa.ssl.openssl.api.modes,
  fafafa.ssl.openssl.api.cmac.evp,
  fafafa.ssl.openssl.api.sm;

type
  TModuleLoadProc = procedure;

  TModuleTest = record
    Name: string;
    LoadProc: TModuleLoadProc;
    UnloadProc: TModuleLoadProc;
    TestFunc: Pointer;
  end;

var
  Modules: array of TModuleTest;
  SuccessCount, FailCount: Integer;
  ModuleIndex: Integer;

procedure AddModule(const AName: string; ALoadProc, AUnloadProc: TModuleLoadProc; ATestFunc: Pointer = nil);
var
  LIndex: Integer;
begin
  SetLength(Modules, Length(Modules) + 1);
  LIndex := High(Modules);
  Modules[LIndex].Name := AName;
  Modules[LIndex].LoadProc := ALoadProc;
  Modules[LIndex].UnloadProc := AUnloadProc;
  Modules[LIndex].TestFunc := ATestFunc;
end;

procedure LoadERRCompat;
begin
  LoadOpenSSLERR;
end;

procedure UnloadERRCompat;
begin
  UnloadOpenSSLERR;
end;

procedure LoadBIOCompat;
begin
  LoadOpenSSLBIO;
end;

procedure UnloadBIOCompat;
begin
  UnloadOpenSSLBIO;
end;

procedure LoadSSLCompat;
begin
  LoadOpenSSLSSL;
end;

procedure UnloadSSLCompat;
begin
  UnloadOpenSSLSSL;
end;

procedure LoadEVPCompat;
begin
  LoadEVP(GetCryptoLibHandle);
end;

procedure UnloadEVPCompat;
begin
  UnloadEVP;
end;

procedure LoadX509Compat;
begin
  LoadOpenSSLX509;
end;

procedure UnloadX509Compat;
begin
  UnloadOpenSSLX509;
end;

procedure LoadRSACompat;
begin
  LoadOpenSSLRSA;
end;

procedure UnloadRSACompat;
begin
  UnloadOpenSSLRSA;
end;

procedure LoadDSACompat;
begin
  LoadOpenSSLDSA;
end;

procedure UnloadDSACompat;
begin
  UnloadOpenSSLDSA;
end;

procedure LoadDHCompat;
begin
  LoadOpenSSLDH;
end;

procedure UnloadDHCompat;
begin
  UnloadOpenSSLDH;
end;

procedure LoadECCompat;
begin
  LoadECFunctions(GetCryptoLibHandle);
end;

procedure UnloadECCompat;
begin
  UnloadECFunctions;
end;

procedure LoadECDSACompat;
begin
  LoadOpenSSLECDSA;
end;

procedure UnloadECDSACompat;
begin
  UnloadOpenSSLECDSA;
end;

procedure LoadECDHCompat;
begin
  LoadOpenSSLECDH;
end;

procedure UnloadECDHCompat;
begin
  UnloadOpenSSLECDH;
end;

procedure LoadSHACompat;
begin
  LoadSHAFunctions(GetCryptoLibHandle);
end;

procedure UnloadSHACompat;
begin
  UnloadSHAFunctions;
end;

procedure LoadSHA3Compat;
begin
  LoadSHA3Functions(GetCryptoLibHandle);
end;

procedure UnloadSHA3Compat;
begin
  UnloadSHA3Functions;
end;

procedure LoadMDCompat;
begin
  LoadMDFunctions(GetCryptoLibHandle);
end;

procedure UnloadMDCompat;
begin
  UnloadMDFunctions;
end;

procedure LoadBLAKE2Compat;
begin
  LoadBLAKE2Functions(GetCryptoLibHandle);
end;

procedure UnloadBLAKE2Compat;
begin
  UnloadBLAKE2Functions;
end;

procedure LoadHMACCompat;
begin
  LoadOpenSSLHMAC;
end;

procedure UnloadHMACCompat;
begin
  UnloadOpenSSLHMAC;
end;

procedure LoadAESCompat;
begin
  LoadAESFunctions(GetCryptoLibHandle);
end;

procedure UnloadAESCompat;
begin
  UnloadAESFunctions;
end;

procedure LoadDESCompat;
begin
  LoadDESFunctions(GetCryptoLibHandle);
end;

procedure UnloadDESCompat;
begin
  UnloadDESFunctions;
end;

procedure LoadChaChaCompat;
begin
  LoadChaChaFunctions;
end;

procedure UnloadChaChaCompat;
begin
  UnloadChaChaFunctions;
end;

procedure LoadPEMCompat;
begin
  LoadOpenSSLPEM(GetCryptoLibHandle);
end;

procedure UnloadPEMCompat;
begin
  UnloadOpenSSLPEM;
end;

procedure LoadASN1Compat;
begin
  LoadOpenSSLASN1(GetCryptoLibHandle);
end;

procedure UnloadASN1Compat;
begin
  UnloadOpenSSLASN1;
end;

procedure LoadPKCS7Compat;
begin
  LoadPKCS7Functions;
end;

procedure UnloadPKCS7Compat;
begin
  UnloadPKCS7Functions;
end;

procedure LoadPKCS12Compat;
begin
  LoadPKCS12Module(GetCryptoLibHandle);
end;

procedure UnloadPKCS12Compat;
begin
  UnloadPKCS12Module;
end;

procedure LoadX509V3Compat;
begin
  LoadX509V3Functions(GetCryptoLibHandle);
end;

procedure UnloadX509V3Compat;
begin
  UnloadX509V3Functions;
end;

procedure LoadRANDCompat;
begin
  LoadOpenSSLRAND;
end;

procedure UnloadRANDCompat;
begin
  UnloadOpenSSLRAND;
end;

procedure LoadBNCompat;
begin
  LoadOpenSSLBN;
end;

procedure UnloadBNCompat;
begin
  UnloadOpenSSLBN;
end;

procedure LoadOBJCompat;
begin
  LoadOBJModule(GetCryptoLibHandle);
end;

procedure UnloadOBJCompat;
begin
  UnloadOBJModule;
end;

procedure LoadBufferCompat;
begin
  LoadBufferModule(GetCryptoLibHandle);
end;

procedure UnloadBufferCompat;
begin
  UnloadBufferModule;
end;

procedure LoadStackCompat;
begin
  LoadStackFunctions;
end;

procedure UnloadStackCompat;
begin
  UnloadStackFunctions;
end;

procedure LoadLHashCompat;
begin
  LoadLHashFunctions;
end;

procedure UnloadLHashCompat;
begin
  UnloadLHashFunctions;
end;

procedure LoadCMSCompat;
begin
  LoadOpenSSLCMS(GetCryptoLibHandle);
end;

procedure UnloadCMSCompat;
begin
  UnloadOpenSSLCMS;
end;

procedure LoadTSCompat;
begin
  LoadTSFunctions;
end;

procedure UnloadTSCompat;
begin
  UnloadTSFunctions;
end;

procedure LoadCTCompat;
begin
  LoadCTFunctions;
end;

procedure UnloadCTCompat;
begin
  UnloadCTFunctions;
end;

procedure LoadOCSPCompat;
begin
  LoadOpenSSLOCSP(GetCryptoLibHandle);
end;

procedure UnloadOCSPCompat;
begin
  UnloadOpenSSLOCSP;
end;

procedure LoadKDFCompat;
begin
  LoadKDFFunctions;
end;

procedure UnloadKDFCompat;
begin
  UnloadKDFFunctions;
end;

procedure LoadSTORECompat;
begin
  LoadSTOREFunctions;
end;

procedure UnloadSTORECompat;
begin
  UnloadSTOREFunctions;
end;

procedure LoadProviderCompat;
begin
  LoadProviderModule(GetCryptoLibHandle);
end;

procedure UnloadProviderCompat;
begin
  UnloadProviderModule;
end;

procedure LoadCONFCompat;
begin
  LoadOpenSSLConf;
end;

procedure UnloadCONFCompat;
begin
  UnloadOpenSSLConf;
end;

procedure LoadUICompat;
begin
  LoadUIFunctions;
end;

procedure UnloadUICompat;
begin
  UnloadUIFunctions;
end;

procedure LoadEngineCompat;
begin
  LoadOpenSSLEngine(GetCryptoLibHandle);
end;

procedure UnloadEngineCompat;
begin
  UnloadOpenSSLEngine;
end;

procedure LoadCOMPCompat;
begin
  LoadCOMPFunctions;
end;

procedure UnloadCOMPCompat;
begin
  UnloadCOMPFunctions;
end;

procedure LoadModesCompat;
begin
  LoadModesFunctions;
end;

procedure UnloadModesCompat;
begin
  UnloadModesFunctions;
end;

procedure LoadCMACCompat;
begin
  // fafafa.ssl.openssl.api.cmac.evp uses internal lazy loading.
  // No exported loader exists, so the smoke test only verifies the module can be referenced.
end;

procedure UnloadCMACCompat;
begin
end;

procedure LoadSMCompat;
begin
  LoadOpenSSLSM;
end;

procedure UnloadSMCompat;
begin
  UnloadOpenSSLSM;
end;

procedure InitializeModules;
begin
  AddModule('ERR (错误处理)', @LoadERRCompat, @UnloadERRCompat, @ERR_get_error);
  AddModule('BIO (I/O抽象)', @LoadBIOCompat, @UnloadBIOCompat, @BIO_new);
  AddModule('SSL/TLS', @LoadSSLCompat, @UnloadSSLCompat, @SSL_new);
  AddModule('EVP (加密接口)', @LoadEVPCompat, @UnloadEVPCompat, @EVP_MD_CTX_new);
  AddModule('X509 (证书)', @LoadX509Compat, @UnloadX509Compat, @X509_new);

  AddModule('RSA', @LoadRSACompat, @UnloadRSACompat, @RSA_new);
  AddModule('DSA', @LoadDSACompat, @UnloadDSACompat, @DSA_new);
  AddModule('DH (Diffie-Hellman)', @LoadDHCompat, @UnloadDHCompat, @DH_new);
  AddModule('EC (椭圆曲线)', @LoadECCompat, @UnloadECCompat, @EC_KEY_new);
  AddModule('ECDSA', @LoadECDSACompat, @UnloadECDSACompat, @ECDSA_sign);
  AddModule('ECDH', @LoadECDHCompat, @UnloadECDHCompat, nil);

  AddModule('SHA', @LoadSHACompat, @UnloadSHACompat, @SHA1);
  AddModule('SHA3', @LoadSHA3Compat, @UnloadSHA3Compat, @SHA3_256);
  AddModule('MD (MD5/MD4)', @LoadMDCompat, @UnloadMDCompat, @MD5);
  AddModule('BLAKE2', @LoadBLAKE2Compat, @UnloadBLAKE2Compat, nil);
  AddModule('HMAC', @LoadHMACCompat, @UnloadHMACCompat, @HMAC);

  AddModule('AES', @LoadAESCompat, @UnloadAESCompat, @AES_encrypt);
  AddModule('DES', @LoadDESCompat, @UnloadDESCompat, @DES_ecb_encrypt);
  AddModule('ChaCha20-Poly1305', @LoadChaChaCompat, @UnloadChaChaCompat, nil);

  AddModule('PEM', @LoadPEMCompat, @UnloadPEMCompat, @PEM_read_bio_X509);
  AddModule('ASN.1', @LoadASN1Compat, @UnloadASN1Compat, @ASN1_STRING_new);
  AddModule('PKCS7', @LoadPKCS7Compat, @UnloadPKCS7Compat, @PKCS7_new);
  AddModule('PKCS12', @LoadPKCS12Compat, @UnloadPKCS12Compat, @PKCS12_new);
  AddModule('X509v3 (扩展)', @LoadX509V3Compat, @UnloadX509V3Compat, @X509V3_EXT_conf);

  AddModule('RAND (随机数)', @LoadRANDCompat, @UnloadRANDCompat, @RAND_bytes);
  AddModule('BN (大数)', @LoadBNCompat, @UnloadBNCompat, @BN_new);
  AddModule('OBJ (对象标识符)', @LoadOBJCompat, @UnloadOBJCompat, @OBJ_nid2obj);
  AddModule('BUFFER', @LoadBufferCompat, @UnloadBufferCompat, @BUF_MEM_new);
  AddModule('STACK', @LoadStackCompat, @UnloadStackCompat, @OPENSSL_sk_new);
  AddModule('LHASH', @LoadLHashCompat, @UnloadLHashCompat, @OPENSSL_LH_new);

  AddModule('CMS (加密消息)', @LoadCMSCompat, @UnloadCMSCompat, @CMS_ContentInfo_new);
  AddModule('TS (时间戳)', @LoadTSCompat, @UnloadTSCompat, @TS_REQ_new);
  AddModule('CT (证书透明)', @LoadCTCompat, @UnloadCTCompat, @SCT_new);
  AddModule('OCSP', @LoadOCSPCompat, @UnloadOCSPCompat, @OCSP_REQUEST_new);
  AddModule('KDF (密钥派生)', @LoadKDFCompat, @UnloadKDFCompat, nil);
  AddModule('STORE', @LoadSTORECompat, @UnloadSTORECompat, @OSSL_STORE_open);
  AddModule('PROVIDER (3.0+)', @LoadProviderCompat, @UnloadProviderCompat, @OSSL_PROVIDER_load);
  AddModule('CONF (配置)', @LoadCONFCompat, @UnloadCONFCompat, @NCONF_new);
  AddModule('UI (用户接口)', @LoadUICompat, @UnloadUICompat, @UI_new);
  AddModule('ENGINE', @LoadEngineCompat, @UnloadEngineCompat, @ENGINE_new);
  AddModule('COMP (压缩)', @LoadCOMPCompat, @UnloadCOMPCompat, @COMP_CTX_new);
  AddModule('MODES (加密模式)', @LoadModesCompat, @UnloadModesCompat, nil);
  AddModule('CMAC', @LoadCMACCompat, @UnloadCMACCompat, nil);
  AddModule('SM (国密)', @LoadSMCompat, @UnloadSMCompat, @SM3);
end;

procedure TestModule(const Module: TModuleTest);
begin
  Write(Format('  %-30s: ', [Module.Name]));

  try
    Module.LoadProc;

    if Assigned(Module.TestFunc) and not Assigned(PPointer(Module.TestFunc)^) then
    begin
      WriteLn('[失败] - 函数未加载');
      Inc(FailCount);
      Exit;
    end;

    WriteLn('[成功]');
    Inc(SuccessCount);
  except
    on E: Exception do
    begin
      WriteLn('[失败] - ', E.Message);
      Inc(FailCount);
    end;
  end;
end;

procedure PrintTestResults;
var
  Total: Integer;
  SuccessRate: Double;
begin
  Total := SuccessCount + FailCount;
  if Total > 0 then
    SuccessRate := (SuccessCount / Total) * 100
  else
    SuccessRate := 0;

  WriteLn;
  WriteLn('========================================');
  WriteLn('           测试结果汇总');
  WriteLn('========================================');
  WriteLn(Format('  总模块数: %d', [Total]));
  WriteLn(Format('  成功加载: %d', [SuccessCount]));
  WriteLn(Format('  加载失败: %d', [FailCount]));
  WriteLn(Format('  成功率:   %.1f%%', [SuccessRate]));
  WriteLn('========================================');

  if FailCount > 0 then
    WriteLn('注意: 某些模块可能依赖当前运行时不提供的 OpenSSL 功能');
end;

procedure PrintOpenSSLVersionInfo;
var
  Version: LongWord;
  VersionStr: PAnsiChar;
begin
  if Assigned(OpenSSL_version_num) then
  begin
    Version := OpenSSL_version_num();
    WriteLn(Format('OpenSSL 版本号: 0x%x', [Version]));
  end;

  if Assigned(OpenSSL_version) then
  begin
    VersionStr := OpenSSL_version(0);
    if Assigned(VersionStr) then
      WriteLn('OpenSSL 版本: ', string(VersionStr));
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('     OpenSSL Pascal 绑定模块加载测试');
  WriteLn('========================================');
  WriteLn;

  SuccessCount := 0;
  FailCount := 0;

  WriteLn('1. 加载 OpenSSL 动态库...');
  try
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('错误: 无法加载 OpenSSL 库');
      WriteLn(E.Message);
      Halt(1);
    end;
  end;

  WriteLn('   OpenSSL 库加载成功');
  WriteLn('   Crypto handle: ', PtrUInt(GetCryptoLibHandle));
  WriteLn('   SSL handle: ', PtrUInt(GetSSLLibHandle));
  WriteLn;

  InitializeModules;

  WriteLn('2. 获取 OpenSSL 版本信息...');
  LoadERRCompat;
  PrintOpenSSLVersionInfo;
  WriteLn;

  WriteLn('3. 测试模块加载...');
  WriteLn('----------------------------------------');
  for ModuleIndex := Low(Modules) to High(Modules) do
    TestModule(Modules[ModuleIndex]);

  PrintTestResults;

  WriteLn;
  WriteLn('4. 清理资源...');
  for ModuleIndex := Low(Modules) to High(Modules) do
    Modules[ModuleIndex].UnloadProc;
  UnloadOpenSSLCore;
  WriteLn('   清理完成');
  WriteLn('[PASS] OpenSSL loader smoke completed');
end.
