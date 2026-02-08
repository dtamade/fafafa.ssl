program practical_examples;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  fafafa.ssl 实用示例集（精简兼容版）

  目标：
  - 保持示例“可编译、可运行、可理解”
  - 使用当前稳定高层 API，避免低层符号漂移
}

uses
  SysUtils,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.encoding;

var
  TotalExamples: Integer = 0;
  SuccessExamples: Integer = 0;

procedure MarkResult(const AName: string; AOK: Boolean);
begin
  Inc(TotalExamples);
  if AOK then
  begin
    Inc(SuccessExamples);
    WriteLn('✅ ', AName, ' - 成功');
  end
  else
    WriteLn('❌ ', AName, ' - 失败');
end;

function ExampleSHA256: Boolean;
var
  LData: string;
  LHashHex: string;
begin
  LData := 'Hello, fafafa.ssl practical examples';
  LHashHex := TCryptoUtils.SHA256Hex(LData);

  WriteLn('  输入: ', LData);
  WriteLn('  SHA-256: ', LHashHex);
  Result := Length(LHashHex) = 64;
end;

function ExampleSHA512: Boolean;
var
  LData: TBytes;
  LHash: TBytes;
  LHex: string;
begin
  LData := TEncoding.UTF8.GetBytes('SHA-512 example');
  LHash := TCryptoUtils.SHA512(LData);
  LHex := TEncodingUtils.BytesToHex(LHash, False);

  WriteLn('  SHA-512 长度: ', Length(LHash), ' 字节');
  WriteLn('  SHA-512 (前32 chars): ', Copy(LHex, 1, 32), '...');
  Result := Length(LHash) = 64;
end;

function ExampleAESGCM: Boolean;
var
  LPlain: TBytes;
  LKey: TBytes;
  LIV: TBytes;
  LAAD: TBytes;
  LCipher: TBytes;
  LRecovered: TBytes;
begin
  LPlain := TEncoding.UTF8.GetBytes('Practical AES-GCM payload');
  LKey := TCryptoUtils.GenerateKey(256);
  LIV := TCryptoUtils.GenerateIV(12);
  LAAD := TEncoding.UTF8.GetBytes('practical-aad');

  if not TCryptoUtils.TryAES_GCM_Encrypt(LPlain, LKey, LIV, LCipher, LAAD) then
    Exit(False);

  if not TCryptoUtils.TryAES_GCM_Decrypt(LCipher, LKey, LIV, LRecovered, LAAD) then
    Exit(False);

  WriteLn('  密文长度: ', Length(LCipher), ' 字节');
  WriteLn('  解密结果: ', TEncoding.UTF8.GetString(LRecovered));
  Result := TEncoding.UTF8.GetString(LRecovered) = TEncoding.UTF8.GetString(LPlain);
end;

function ExampleRandomAndKey: Boolean;
var
  LRandom: TBytes;
  LKey: TBytes;
begin
  if not TCryptoUtils.TrySecureRandom(16, LRandom) then
    Exit(False);

  LKey := TCryptoUtils.GenerateKey(256);

  WriteLn('  Random(16): ', TEncodingUtils.BytesToHex(LRandom, False));
  WriteLn('  Key(256bit): ', TEncodingUtils.BytesToHex(LKey, False));

  Result := (Length(LRandom) = 16) and (Length(LKey) = 32);
end;

begin
  WriteLn('==============================================================');
  WriteLn('fafafa.ssl 实用示例集（精简兼容版）');
  WriteLn('==============================================================');
  WriteLn;

  try
    WriteLn('[示例 1] SHA-256 哈希');
    MarkResult('SHA-256', ExampleSHA256);
    WriteLn;

    WriteLn('[示例 2] SHA-512 哈希');
    MarkResult('SHA-512', ExampleSHA512);
    WriteLn;

    WriteLn('[示例 3] AES-256-GCM 认证加密');
    MarkResult('AES-GCM', ExampleAESGCM);
    WriteLn;

    WriteLn('[示例 4] 安全随机数与密钥生成');
    MarkResult('Random+Key', ExampleRandomAndKey);
    WriteLn;

    WriteLn('--------------------------------------------------------------');
    WriteLn('完成统计: ', SuccessExamples, '/', TotalExamples, ' 成功');
    if SuccessExamples = TotalExamples then
      WriteLn('🎉 所有示例执行成功')
    else
      WriteLn('⚠️ 存在失败示例，请检查运行环境');
    WriteLn('--------------------------------------------------------------');
  except
    on E: Exception do
    begin
      WriteLn('❌ 异常: ', E.Message);
      Halt(1);
    end;
  end;
end.
