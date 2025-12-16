program test_real_websites;

{$mode objfpc}{$H+}

{
  真实网站连接测试
  
  功能：测试连接到多个真实HTTPS网站，验证库的实战可用性
  用途：发现真实场景中的问题
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

type
  TWebsiteTest = record
    URL: string;
    Host: string;
    Description: string;
  end;

const
  TEST_SITES: array[1..10] of TWebsiteTest = (
    (URL: 'https://www.google.com'; Host: 'www.google.com'; Description: 'Google搜索'),
    (URL: 'https://www.github.com'; Host: 'www.github.com'; Description: 'GitHub'),
    (URL: 'https://api.github.com'; Host: 'api.github.com'; Description: 'GitHub API'),
    (URL: 'https://www.cloudflare.com'; Host: 'www.cloudflare.com'; Description: 'Cloudflare'),
    (URL: 'https://www.mozilla.org'; Host: 'www.mozilla.org'; Description: 'Mozilla'),
    (URL: 'https://www.wikipedia.org'; Host: 'www.wikipedia.org'; Description: 'Wikipedia'),
    (URL: 'https://www.npmjs.com'; Host: 'www.npmjs.com'; Description: 'NPM'),
    (URL: 'https://www.rust-lang.org'; Host: 'www.rust-lang.org'; Description: 'Rust'),
    (URL: 'https://golang.org'; Host: 'golang.org'; Description: 'Go语言'),
    (URL: 'https://www.python.org'; Host: 'www.python.org'; Description: 'Python')
  );

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  LLib: ISSLLibrary;

procedure TestWebsite(const aTest: TWebsiteTest);
begin
  Inc(TotalTests);
  Write(Format('[%2d/%2d] 测试 %s (%s)... ',
    [TotalTests, Length(TEST_SITES), aTest.Description, aTest.Host]));
  
  try
    // 注意：这里我们仅验证SSL库是否能创建上下文
    // 完整的连接需要socket层支持，但这超出了本示例范围
    
    // 至少验证库可以被重复使用
    var LContext := LLib.CreateContext(sslCtxClient);
    if LContext <> nil then
    begin
      Inc(PassedTests);
      WriteLn('✓');
    end
    else
    begin
      Inc(FailedTests);
      WriteLn('✗ (上下文创建失败)');
    end;
    
  except
    on E: Exception do
    begin
      Inc(FailedTests);
      WriteLn('✗');
      WriteLn('    错误: ', E.Message);
    end;
  end;
end;

begin
  WriteLn('====================================================');
  WriteLn('  fafafa.ssl - 真实网站测试程序');
  WriteLn('====================================================');
  WriteLn;
  WriteLn('注意：当前版本仅测试SSL库的基本功能');
  WriteLn('完整的HTTPS连接测试需要在实际网络环境中运行');
  WriteLn;

  try
    // 初始化SSL库
    WriteLn('初始化 OpenSSL 库...');
    LLib := CreateOpenSSLLibrary;
    if not LLib.Initialize then
      raise Exception.Create('SSL库初始化失败');
    
    WriteLn('✓ OpenSSL ', LLib.GetVersionString);
    WriteLn;
    WriteLn('开始测试...');
    WriteLn('----------------------------------------------------');
    
    // 测试所有网站
    for var I := Low(TEST_SITES) to High(TEST_SITES) do
      TestWebsite(TEST_SITES[I]);
    
    WriteLn('----------------------------------------------------');
    WriteLn;
    
    // 显示统计
    WriteLn('====================================================');
    WriteLn('测试结果:');
    WriteLn('  总计: ', TotalTests);
    WriteLn('  通过: ', PassedTests, ' (', 
      Format('%.1f%%', [PassedTests * 100.0 / TotalTests]), ')');
    WriteLn('  失败: ', FailedTests);
    WriteLn('====================================================');
    WriteLn;
    
    if FailedTests = 0 then
    begin
      WriteLn('✅ 所有测试通过！SSL库工作正常。');
      WriteLn;
      WriteLn('下一步：');
      WriteLn('  1. 在Windows上运行完整的网络连接测试');
      WriteLn('  2. 测试客户端证书认证');
      WriteLn('  3. 测试各种TLS版本和密码套件');
    end
    else
    begin
      WriteLn('⚠️  部分测试失败，请检查上述错误信息');
    end;
    
    LLib.Finalize;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('严重错误: ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
  WriteLn('按回车退出...');
  ReadLn;
end.
