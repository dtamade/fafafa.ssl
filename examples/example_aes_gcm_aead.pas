program ExampleAESGCMAEAD;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  AES-GCM AEAD 示例（当前推荐实现）

  说明：
  - 使用高层 API `TCryptoUtils.TryAES_GCM_Encrypt/Decrypt`
  - 由库内部处理 OpenSSL 细节，避免低层签名漂移导致的示例失效
}

uses
  SysUtils,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding;

const
  PLAINTEXT = 'Hello, this is a secret message!';
  AAD_TEXT = 'Additional authenticated data';

procedure RunDemo;
var
  LPlainBytes: TBytes;
  LAAD: TBytes;
  LKey: TBytes;
  LIV: TBytes;
  LCipher: TBytes;
  LDecrypted: TBytes;
begin
  WriteLn('========================================');
  WriteLn('  AES-256-GCM AEAD 加密示例');
  WriteLn('========================================');
  WriteLn;

  LPlainBytes := TEncoding.UTF8.GetBytes(PLAINTEXT);
  LAAD := TEncoding.UTF8.GetBytes(AAD_TEXT);

  // 32 字节 key + 12 字节 IV 是 GCM 推荐配置
  LKey := TCryptoUtils.GenerateKey(256);
  LIV := TCryptoUtils.GenerateIV(12);

  WriteLn('明文: ', PLAINTEXT);
  WriteLn('AAD: ', AAD_TEXT);
  WriteLn('Key: ', TEncodingUtils.BytesToHex(LKey, False));
  WriteLn('IV : ', TEncodingUtils.BytesToHex(LIV, False));
  WriteLn;

  if not TCryptoUtils.TryAES_GCM_Encrypt(LPlainBytes, LKey, LIV, LCipher, LAAD) then
    raise Exception.Create('AES-GCM 加密失败');

  WriteLn('✓ 加密成功');
  WriteLn('密文长度: ', Length(LCipher), ' 字节');
  WriteLn('密文(hex): ', TEncodingUtils.BytesToHex(LCipher, False));
  WriteLn;

  if not TCryptoUtils.TryAES_GCM_Decrypt(LCipher, LKey, LIV, LDecrypted, LAAD) then
    raise Exception.Create('AES-GCM 解密或认证失败');

  WriteLn('✓ 解密与认证成功');
  WriteLn('解密结果: ', TEncoding.UTF8.GetString(LDecrypted));
  WriteLn;

  WriteLn('关键要点：');
  WriteLn('1) GCM 提供机密性 + 完整性（AEAD）');
  WriteLn('2) AAD 会参与认证但不加密');
  WriteLn('3) IV 必须避免重复');
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
