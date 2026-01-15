{
  test_quick_cert - 测试 TSSLQuick.GetCertificateInfo 方法
}

program test_quick_cert;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.quick;

var
  Info: TQuickCertInfo;
  Host: string;
begin
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║  TSSLQuick.GetCertificateInfo Test        ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn;
  
  Host := 'www.google.com';
  WriteLn('正在获取 ', Host, ' 的证书信息...');
  WriteLn;
  
  try
    Info := TSSLQuick.GetCertificateInfo(Host);
    
    WriteLn('证书信息:');
    WriteLn('  主题: ', Info.Subject);
    WriteLn('  颁发者: ', Info.Issuer);
    WriteLn('  有效期开始: ', DateTimeToStr(Info.ValidFrom));
    WriteLn('  有效期结束: ', DateTimeToStr(Info.ValidUntil));
    WriteLn('  是否有效: ', BoolToStr(Info.IsValid, 'Yes', 'No'));
    WriteLn('  是否过期: ', BoolToStr(Info.IsExpired, 'Yes', 'No'));
    WriteLn('  距离过期: ', Info.DaysUntilExpiry, ' 天');
    WriteLn('  SHA256指纹: ', Info.Fingerprint);
    
    if Info.ErrorMessage <> '' then
      WriteLn('  错误: ', Info.ErrorMessage);
    
    WriteLn;
    if Info.IsValid then
    begin
      WriteLn('✅ 证书验证通过！');
      Halt(0);
    end
    else
    begin
      WriteLn('❌ 证书无效！');
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      WriteLn('❌ 发生错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
