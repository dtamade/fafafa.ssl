program PKCS7EncryptDecryptExample;

{$mode ObjFPC}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  PKCS#7 加密/解密示例（兼容当前 API）

  说明：
  - 展示 PKCS#7 Encrypt/Decrypt API 可用性与推荐工作流
  - 该示例保持“可编译 + 可读”，详细端到端流程请用 tests 中夹具
}

uses
  SysUtils,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.consts;

procedure Main;
var
  LCipher: PEVP_CIPHER;
begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 加密/解密 API 可用性示例');
  WriteLn('=============================================================');
  WriteLn;

  if not LoadOpenSSLLibrary then
    raise Exception.Create('无法加载 OpenSSL');

  WriteLn('OpenSSL: ', GetOpenSSLVersion);
  WriteLn;

  WriteLn('核心函数可用性：');
  WriteLn('  PKCS7_encrypt: ', BoolToStr(Assigned(PKCS7_encrypt), '可用', '不可用'));
  WriteLn('  PKCS7_decrypt: ', BoolToStr(Assigned(PKCS7_decrypt), '可用', '不可用'));
  WriteLn('  EncryptData helper: 见 fafafa.ssl.openssl.api.pkcs7.SignData/EncryptData');
  WriteLn('  DecryptData helper: 见 fafafa.ssl.openssl.api.pkcs7.DecryptData');
  WriteLn;

  LCipher := EVP_aes_256_cbc();
  WriteLn('推荐对称算法 EVP_aes_256_cbc: ', BoolToStr(LCipher <> nil, '可用', '不可用'));
  WriteLn('建议 Flags: PKCS7_BINARY = $', IntToHex(PKCS7_BINARY, 2));
  WriteLn;

  WriteLn('推荐工作流：');
  WriteLn('1) 准备接收者证书栈（STACK_OF(X509)）');
  WriteLn('2) 调用 EncryptData(...) 或 PKCS7_encrypt(...)');
  WriteLn('3) 使用 i2d_PKCS7_bio 导出 DER');
  WriteLn('4) 通过 DecryptData(...) 或 PKCS7_decrypt(...) 恢复明文');
  WriteLn;

  WriteLn('验证入口：');
  WriteLn('- tests/certificate/test_p2_pkcs7_comprehensive.pas');
  WriteLn('- tests/certificate/test_pkcs7_sign_verify_workflow.pas');
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
