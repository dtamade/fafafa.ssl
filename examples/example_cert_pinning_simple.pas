program example_cert_pinning_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.cert.pinning,
  fafafa.ssl.base;

const
  // 证书固定类型常量
  PIN_TYPE_CERTIFICATE = 0;  // ptCertificate
  PIN_TYPE_PUBLICKEY = 1;    // ptPublicKey

var
  Validator: TPinValidator;
begin
  WriteLn('=== Certificate Pinning Simple Example ===');
  WriteLn;

  try
    // 创建 Pin 验证器
    Validator := TPinValidator.Create;
    try
      WriteLn('✓ Pin Validator created');

      // 添加主要 Pin（示例：Google 的公钥 Pin）
      // 注意：这些是示例 Pin，实际使用时需要从真实证书中提取
      Validator.AddPinBase64(
        'X3pGTSOuJeEVw989IJ/cEtXUEmy52zs1TZQrU06KUKg=',
        ptPublicKey,
        'Google Primary Pin',
        False
      );

      WriteLn('✓ Added primary certificate pin');

      // 添加备用 Pin（OWASP 最佳实践：至少 2 个 Pin）
      Validator.AddPinBase64(
        'YLh1dUR9y6Kja30RrAn7JKnbQG/uEtLMkBgFF2Fuihg=',
        ptPublicKey,
        'Google Backup Pin',
        True
      );

      WriteLn('✓ Added backup certificate pin');
      WriteLn;

      // 检查配置
      WriteLn('Pin Configuration:');
      WriteLn('  Valid pins: ', Validator.GetValidPinCount);
      WriteLn('  Secure configuration: ', Validator.IsSecureConfiguration);
      WriteLn('  Require valid pin: ', Validator.RequireValidPin);
      WriteLn;

      // 显示 Pin 信息
      WriteLn(Validator.GetPinInfo);

      WriteLn('=== Certificate Pinning Configuration Complete ===');
      WriteLn;
      WriteLn('Note: This example demonstrates certificate pinning configuration.');
      WriteLn('To test actual pin validation, you would need to:');
      WriteLn('1. Extract real certificate pins from your target server');
      WriteLn('2. Create a TLS connection to that server');
      WriteLn('3. The pin validator will automatically verify the certificate');
      WriteLn;
      WriteLn('Example pin extraction command:');
      WriteLn('  openssl s_client -connect example.com:443 | \');
      WriteLn('  openssl x509 -pubkey -noout | \');
      WriteLn('  openssl pkey -pubin -outform der | \');
      WriteLn('  openssl dgst -sha256 -binary | \');
      WriteLn('  openssl enc -base64');

    finally
      Validator.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
