program PKCS7SignVerifyExample;

{$mode ObjFPC}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  PKCS#7 签名/验签示例（兼容当前 API）

  说明：
  - 展示 Sign/Verify 相关函数与 helper 的可用性
  - 真实签名证书链验证建议使用 tests 中的完整工作流
}

uses
  SysUtils,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.consts;

procedure Main;
begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 签名/验签 API 可用性示例');
  WriteLn('=============================================================');
  WriteLn;

  if not LoadOpenSSLLibrary then
    raise Exception.Create('无法加载 OpenSSL');

  WriteLn('OpenSSL: ', GetOpenSSLVersion);
  WriteLn;

  WriteLn('核心函数可用性：');
  WriteLn('  PKCS7_sign: ', BoolToStr(Assigned(PKCS7_sign), '可用', '不可用'));
  WriteLn('  PKCS7_verify: ', BoolToStr(Assigned(PKCS7_verify), '可用', '不可用'));
  WriteLn('  SignData helper: 见 fafafa.ssl.openssl.api.pkcs7.SignData');
  WriteLn('  VerifySignedData helper: 见 fafafa.ssl.openssl.api.pkcs7.VerifySignedData');
  WriteLn;

  WriteLn('常用标志：');
  WriteLn('  PKCS7_DETACHED = $', IntToHex(PKCS7_DETACHED, 2));
  WriteLn('  PKCS7_BINARY   = $', IntToHex(PKCS7_BINARY, 2));
  WriteLn('  PKCS7_NOVERIFY = $', IntToHex(PKCS7_NOVERIFY, 2));
  WriteLn;

  WriteLn('推荐工作流：');
  WriteLn('1) 使用签名证书 + 私钥调用 SignData(...)');
  WriteLn('2) 通过 VerifySignedData(...) 验证并恢复原文');
  WriteLn('3) 生产环境启用完整证书链验证（避免 PKCS7_NOVERIFY）');
  WriteLn;

  WriteLn('验证入口：');
  WriteLn('- examples/pkcs7_sign_verify_simple.pas');
  WriteLn('- tests/certificate/test_p2_pkcs7_comprehensive.pas');
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
