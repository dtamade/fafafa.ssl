program PKCS7BasicExample;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, ctypes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.consts;

begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 基础功能演示');
  WriteLn('=============================================================');
  WriteLn('');

  // Initialize OpenSSL
  WriteLn('1. 初始化 OpenSSL 库...');
  if not LoadOpenSSLLibrary then
  begin
    WriteLn('   ✗ 错误: 无法初始化 OpenSSL 库');
    Halt(1);
  end;
  WriteLn('   ✓ OpenSSL 库初始化成功');
  WriteLn('   版本: ', GetOpenSSLVersion);
  WriteLn('');

  // Test PKCS7 function availability
  WriteLn('2. 检查 PKCS#7 函数可用性...');
  WriteLn('   PKCS7_new: ', BoolToStr(Assigned(PKCS7_new), '可用', '不可用'));
  WriteLn('   PKCS7_free: ', BoolToStr(Assigned(PKCS7_free), '可用', '不可用'));
  WriteLn('   PKCS7_sign: ', BoolToStr(Assigned(PKCS7_sign), '可用', '不可用'));
  WriteLn('   PKCS7_verify: ', BoolToStr(Assigned(PKCS7_verify), '可用', '不可用'));
  WriteLn('   PKCS7_encrypt: ', BoolToStr(Assigned(PKCS7_encrypt), '可用', '不可用'));
  WriteLn('   PKCS7_decrypt: ', BoolToStr(Assigned(PKCS7_decrypt), '可用', '不可用'));
  WriteLn('');

  // Display PKCS7 constants
  WriteLn('3. PKCS#7 常量定义...');
  WriteLn('   NID_pkcs7_data: ', NID_pkcs7_data);
  WriteLn('   NID_pkcs7_signed: ', NID_pkcs7_signed);
  WriteLn('   NID_pkcs7_enveloped: ', NID_pkcs7_enveloped);
  WriteLn('   NID_pkcs7_signedAndEnveloped: ', NID_pkcs7_signedAndEnveloped);
  WriteLn('   NID_pkcs7_digest: ', NID_pkcs7_digest);
  WriteLn('   NID_pkcs7_encrypted: ', NID_pkcs7_encrypted);
  WriteLn('');

  WriteLn('   PKCS7_TEXT: $', IntToHex(PKCS7_TEXT, 2));
  WriteLn('   PKCS7_NOCERTS: $', IntToHex(PKCS7_NOCERTS, 2));
  WriteLn('   PKCS7_NOSIGS: $', IntToHex(PKCS7_NOSIGS, 2));
  WriteLn('   PKCS7_NOCHAIN: $', IntToHex(PKCS7_NOCHAIN, 2));
  WriteLn('   PKCS7_NOINTERN: $', IntToHex(PKCS7_NOINTERN, 2));
  WriteLn('   PKCS7_NOVERIFY: $', IntToHex(PKCS7_NOVERIFY, 2));
  WriteLn('   PKCS7_DETACHED: $', IntToHex(PKCS7_DETACHED, 2));
  WriteLn('   PKCS7_BINARY: $', IntToHex(PKCS7_BINARY, 2));
  WriteLn('   PKCS7_NOATTR: $', IntToHex(PKCS7_NOATTR, 2));
  WriteLn('');

  WriteLn('=============================================================');
  WriteLn('示例完成');
  WriteLn('=============================================================');
  WriteLn('');
  WriteLn('说明:');
  WriteLn('- 本示例演示了 PKCS#7 模块的基本功能检查');
  WriteLn('- 验证了 OpenSSL 库的加载和初始化');
  WriteLn('- 显示了 PKCS#7 相关的函数和常量');
  WriteLn('');
  WriteLn('PKCS#7 主要用途:');
  WriteLn('- 数字签名: 确保数据完整性和来源真实性');
  WriteLn('- 数据加密: 保护敏感数据的机密性');
  WriteLn('- 数据封装: 提供标准的数据容器格式');
  WriteLn('');
  WriteLn('更多示例请参考:');
  WriteLn('- tests/certificate/test_pkcs7_sign_verify_workflow.pas');
  WriteLn('- tests/certificate/test_p2_pkcs7_*.pas');
end.
