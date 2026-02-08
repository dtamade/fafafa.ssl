program example_cert_pinning;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.ssl.base,
  fafafa.ssl.cert.pinning;

var
  Ctx: ISSLContext;
begin
  WriteLn('=== Certificate Pinning Example ===');
  WriteLn;

  try
    // 创建客户端上下文
    Ctx := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;

    WriteLn('✓ SSL Context created successfully');

    // 添加证书固定（示例：Google 的公钥 Pin）
    // 注意：这些是示例 Pin，实际使用时需要从真实证书中提取
    Ctx.AddCertificatePinBase64(
      'X3pGTSOuJeEVw989IJ/cEtXUEmy52zs1TZQrU06KUKg=',
      Ord(ptPublicKey),
      'Google Primary Pin',
      False
    );

    WriteLn('✓ Added primary certificate pin');

    // 添加备用 Pin（OWASP 最佳实践：至少 2 个 Pin）
    Ctx.AddCertificatePinBase64(
      'YLh1dUR9y6Kja30RrAn7JKnbQG/uEtLMkBgFF2Fuihg=',
      Ord(ptPublicKey),
      'Google Backup Pin',
      True
    );

    WriteLn('✓ Added backup certificate pin');

    // 启用证书固定
    Ctx.SetCertificatePinningEnabled(True);

    WriteLn('✓ Certificate pinning enabled');
    WriteLn;

    // 检查证书固定状态
    if Ctx.GetCertificatePinningEnabled then
      WriteLn('Certificate pinning is ACTIVE')
    else
      WriteLn('Certificate pinning is INACTIVE');

    WriteLn;
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

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
