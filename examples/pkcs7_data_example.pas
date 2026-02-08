program PKCS7DataExample;

{$mode ObjFPC}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  PKCS#7 Data 容器示例（兼容当前 API）

  说明：
  - 演示 PKCS#7 基础对象生命周期
  - 验证 Data 类型对象可创建并序列化到 DER
}

uses
  SysUtils,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.api.consts;

procedure Main;
var
  LP7: PPKCS7;
  LOutBIO: PBIO;
  LSerializedSize: Integer;
begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 数据容器示例（Data）');
  WriteLn('=============================================================');
  WriteLn;

  if not LoadOpenSSLLibrary then
    raise Exception.Create('无法加载 OpenSSL');

  WriteLn('OpenSSL: ', GetOpenSSLVersion);
  WriteLn('PKCS7_new: ', BoolToStr(Assigned(PKCS7_new), '可用', '不可用'));
  WriteLn('PKCS7_set_type: ', BoolToStr(Assigned(PKCS7_set_type), '可用', '不可用'));
  WriteLn('i2d_PKCS7_bio: ', BoolToStr(Assigned(i2d_PKCS7_bio), '可用', '不可用'));
  WriteLn;

  LP7 := PKCS7_new();
  if LP7 = nil then
    raise Exception.Create('PKCS7_new 失败');

  try
    if PKCS7_set_type(LP7, NID_pkcs7_data) <> 1 then
      raise Exception.Create('PKCS7_set_type(NID_pkcs7_data) 失败');

    LOutBIO := BIO_new(BIO_s_mem());
    if LOutBIO = nil then
      raise Exception.Create('BIO_new(BIO_s_mem) 失败');

    try
      if i2d_PKCS7_bio(LOutBIO, LP7) <= 0 then
        raise Exception.Create('PKCS7 DER 序列化失败');

      LSerializedSize := BIO_pending(LOutBIO);
      WriteLn('✓ PKCS#7 Data 对象创建成功');
      WriteLn('✓ DER 序列化成功，长度: ', LSerializedSize, ' 字节');
    finally
      BIO_free(LOutBIO);
    end;
  finally
    PKCS7_free(LP7);
  end;

  WriteLn;
  WriteLn('提示：完整签名/验签、加解密工作流请参考：');
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
