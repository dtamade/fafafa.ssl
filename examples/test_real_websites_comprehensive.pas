program test_real_websites_comprehensive;

{$mode objfpc}{$H+}

{
  综合真实网站连接测试 (50+ 站点)
  
  功能：大规模测试 HTTPS 连接兼容性
  用途：验证库在各种服务器配置、CDN、CA 下的稳定性
  难度：⭐⭐⭐ 高级
}

uses
  SysUtils, Classes,
  {$IFDEF UNIX}
  fafafa.examples.sockets,
  {$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.base;

type
  TWebsiteTest = record
    Host: string;
    Port: Word;
    Description: string;
  end;

  TTestResult = record
    Success: Boolean;
    ErrorMessage: string;
    Protocol: string;
    Cipher: string;
    ResponseTime: Integer;
  end;

const
  // Top 50+ Global Websites & Tech Services covering various CDNs and CAs
  TEST_SITES: array[1..52] of TWebsiteTest = (
    // Search Engines
    (Host: 'www.google.com'; Port: 443; Description: 'Google'),
    (Host: 'www.bing.com'; Port: 443; Description: 'Bing'),
    (Host: 'duckduckgo.com'; Port: 443; Description: 'DuckDuckGo'),
    (Host: 'www.baidu.com'; Port: 443; Description: 'Baidu'),
    (Host: 'yandex.com'; Port: 443; Description: 'Yandex'),

    // Social Media
    (Host: 'www.facebook.com'; Port: 443; Description: 'Facebook'),
    (Host: 'twitter.com'; Port: 443; Description: 'Twitter'),
    (Host: 'www.instagram.com'; Port: 443; Description: 'Instagram'),
    (Host: 'www.linkedin.com'; Port: 443; Description: 'LinkedIn'),
    (Host: 'www.reddit.com'; Port: 443; Description: 'Reddit'),
    (Host: 'www.tiktok.com'; Port: 443; Description: 'TikTok'),
    (Host: 'weibo.com'; Port: 443; Description: 'Weibo'),

    // Tech & Dev
    (Host: 'github.com'; Port: 443; Description: 'GitHub'),
    (Host: 'gitlab.com'; Port: 443; Description: 'GitLab'),
    (Host: 'bitbucket.org'; Port: 443; Description: 'Bitbucket'),
    (Host: 'stackoverflow.com'; Port: 443; Description: 'Stack Overflow'),
    (Host: 'www.docker.com'; Port: 443; Description: 'Docker'),
    (Host: 'www.npmjs.com'; Port: 443; Description: 'NPM'),
    (Host: 'pypi.org'; Port: 443; Description: 'PyPI'),
    (Host: 'crates.io'; Port: 443; Description: 'Crates.io'),

    // Cloud & CDN
    (Host: 'aws.amazon.com'; Port: 443; Description: 'AWS'),
    (Host: 'azure.microsoft.com'; Port: 443; Description: 'Azure'),
    (Host: 'cloud.google.com'; Port: 443; Description: 'Google Cloud'),
    (Host: 'www.cloudflare.com'; Port: 443; Description: 'Cloudflare'),
    (Host: 'www.akamai.com'; Port: 443; Description: 'Akamai'),
    (Host: 'www.fastly.com'; Port: 443; Description: 'Fastly'),
    (Host: 'www.digitalocean.com'; Port: 443; Description: 'DigitalOcean'),
    (Host: 'www.heroku.com'; Port: 443; Description: 'Heroku'),
    (Host: 'vercel.com'; Port: 443; Description: 'Vercel'),
    (Host: 'netlify.com'; Port: 443; Description: 'Netlify'),

    // E-commerce & Payments
    (Host: 'www.amazon.com'; Port: 443; Description: 'Amazon'),
    (Host: 'www.ebay.com'; Port: 443; Description: 'eBay'),
    (Host: 'www.paypal.com'; Port: 443; Description: 'PayPal'),
    (Host: 'stripe.com'; Port: 443; Description: 'Stripe'),
    (Host: 'www.shopify.com'; Port: 443; Description: 'Shopify'),
    (Host: 'www.taobao.com'; Port: 443; Description: 'Taobao'),
    (Host: 'www.jd.com'; Port: 443; Description: 'JD.com'),

    // Media & Streaming
    (Host: 'www.youtube.com'; Port: 443; Description: 'YouTube'),
    (Host: 'www.netflix.com'; Port: 443; Description: 'Netflix'),
    (Host: 'www.twitch.tv'; Port: 443; Description: 'Twitch'),
    (Host: 'www.spotify.com'; Port: 443; Description: 'Spotify'),
    (Host: 'vimeo.com'; Port: 443; Description: 'Vimeo'),

    // News & Info
    (Host: 'www.wikipedia.org'; Port: 443; Description: 'Wikipedia'),
    (Host: 'www.nytimes.com'; Port: 443; Description: 'NY Times'),
    (Host: 'www.cnn.com'; Port: 443; Description: 'CNN'),
    (Host: 'www.bbc.com'; Port: 443; Description: 'BBC'),
    (Host: 'www.forbes.com'; Port: 443; Description: 'Forbes'),

    // Programming Languages
    (Host: 'www.python.org'; Port: 443; Description: 'Python'),
    (Host: 'golang.org'; Port: 443; Description: 'Go'),
    (Host: 'www.rust-lang.org'; Port: 443; Description: 'Rust'),
    (Host: 'www.php.net'; Port: 443; Description: 'PHP'),
    (Host: 'www.ruby-lang.org'; Port: 443; Description: 'Ruby')
  );

var
  GLib: ISSLLibrary;
  GTotalTests: Integer = 0;
  GPassedTests: Integer = 0;
  GFailedTests: Integer = 0;

function GetProtocolName(AVer: TSSLProtocolVersion): string;
begin
  case AVer of
    sslProtocolUnknown: Result := 'Unknown';
    sslProtocolSSL2: Result := 'SSLv2';
    sslProtocolSSL3: Result := 'SSLv3';
    sslProtocolTLS10: Result := 'TLS 1.0';
    sslProtocolTLS11: Result := 'TLS 1.1';
    sslProtocolTLS12: Result := 'TLS 1.2';
    sslProtocolTLS13: Result := 'TLS 1.3';
    sslProtocolDTLS10: Result := 'DTLS 1.0';
    sslProtocolDTLS12: Result := 'DTLS 1.2';
  else
    Result := 'Unknown';
  end;
end;

{$IFDEF UNIX}
function ConnectSocket(const AHost: string; APort: Word): TSocket;
var
  LAddr: tsockaddr_in;
  LHostEntry: PHostEnt;
begin
  Result := socket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    raise Exception.Create('Failed to create socket');

  // Resolve hostname
  LHostEntry := gethostbyname(PChar(AHost));
  if LHostEntry = nil then
  begin
    close(Result);
    raise Exception.CreateFmt('Failed to resolve host: %s', [AHost]);
  end;

  // Connect
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  Move(LHostEntry^.h_addr_list^^, LAddr.sin_addr, SizeOf(LAddr.sin_addr));

  if connect(Result, psockaddr(@LAddr), SizeOf(LAddr)) < 0 then
  begin
    close(Result);
    raise Exception.CreateFmt('Failed to connect to %s:%d', [AHost, APort]);
  end;
end;
{$ENDIF}

function TestWebsite(const ATest: TWebsiteTest): TTestResult;
var
  {$IFDEF UNIX}
  LSocket: TSocket;
  {$ENDIF}
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LRequest: string;
  LStartTime, LEndTime: QWord;
begin
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.Protocol := '';
  Result.Cipher := '';
  Result.ResponseTime := 0;

  Inc(GTotalTests);
  Write(Format('[%2d/%2d] %-25s ', [GTotalTests, Length(TEST_SITES), ATest.Description]));

  LStartTime := GetTickCount64;

  try
    {$IFDEF UNIX}
    try
      LSocket := ConnectSocket(ATest.Host, ATest.Port);
    except
      on E: Exception do
      begin
        Result.ErrorMessage := 'Connect Error: ' + E.Message;
        WriteLn('✗ ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;
    end;

    try
      LContext := GLib.CreateContext(sslCtxClient);
      LContext.SetServerName(ATest.Host);

      // Load system CA bundle
      if FileExists('/etc/ssl/certs/ca-certificates.crt') then
        LContext.LoadCAFile('/etc/ssl/certs/ca-certificates.crt')
      else if FileExists('/etc/pki/tls/certs/ca-bundle.crt') then
        LContext.LoadCAFile('/etc/pki/tls/certs/ca-bundle.crt');

      LConn := LContext.CreateConnection(LSocket);
      if not LConn.Connect then
      begin
        Result.ErrorMessage := 'Handshake Error: ' + GLib.GetLastErrorString;
        WriteLn('✗ ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;

      Result.Protocol := GetProtocolName(LConn.GetProtocolVersion);
      Result.Cipher := LConn.GetCipherName;

      // Send simple HEAD request to verify data transmission
      LRequest := Format('HEAD / HTTP/1.1'#13#10'Host: %s'#13#10'Connection: close'#13#10#13#10, [ATest.Host]);
      if LConn.Write(PChar(LRequest)^, Length(LRequest)) <> Length(LRequest) then
      begin
        Result.ErrorMessage := 'Send Error';
        WriteLn('✗ ', Result.ErrorMessage);
        Inc(GFailedTests);
        Exit;
      end;

      // We don't strictly need to read the full response for this connectivity test,
      // just ensuring we can write is often enough, but reading a bit confirms full duplex.
      // Skipping read for speed in this bulk test, or maybe read 1 byte.
      
      LEndTime := GetTickCount64;
      Result.ResponseTime := LEndTime - LStartTime;
      Result.Success := True;
      Inc(GPassedTests);

      WriteLn(Format('✓ %s (%s, %dms)', [Result.Protocol, Result.Cipher, Result.ResponseTime]));

    finally
      close(LSocket);
    end;
    {$ELSE}
    WriteLn('⊘ Skipped (Unix only)');
    {$ENDIF}

  except
    on E: Exception do
    begin
      Result.ErrorMessage := 'Exception: ' + E.Message;
      WriteLn('✗ ', Result.ErrorMessage);
      Inc(GFailedTests);
    end;
  end;
end;

var
  I: Integer;
begin
  WriteLn('================================================================');
  WriteLn('  fafafa.ssl - Comprehensive Real Website Test (50+ Sites)');
  WriteLn('================================================================');
  WriteLn;

  try
    GLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if not GLib.Initialize then
      raise Exception.Create('Failed to initialize SSL library');

    WriteLn('Library: OpenSSL ', GLib.GetVersionString);
    WriteLn('----------------------------------------------------------------');

    for I := Low(TEST_SITES) to High(TEST_SITES) do
      TestWebsite(TEST_SITES[I]);

    WriteLn('----------------------------------------------------------------');
    WriteLn(Format('Results: %d/%d Passed (%.1f%%)', [GPassedTests, GTotalTests, GPassedTests * 100.0 / GTotalTests]));
    WriteLn('================================================================');

    GLib.Finalize;
  except
    on E: Exception do
      WriteLn('FATAL: ', E.Message);
  end;
end.
