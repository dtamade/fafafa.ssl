program test_winssl_certificate;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.winssl.lib,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.connection;

procedure TestCertificateInfo(const aHost: string; aPort: Word);
var
  SSLLibrary: ISSLLibrary;
  SSLContext: ISSLContext;
  SSLConnection: ISSLConnection;
  Socket: TSocket;
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  HttpRequest: string;
  Cert: ISSLCertificate;
  CertChain: TSSLCertificateArray;
  CertInfo: TSSLCertificateInfo;
  i: Integer;
  SANs: TStringList;
  WSAData: TWSAData;
begin
  WriteLn('=== WinSSL Certificate Test ===');
  WriteLn;
  
  // 初始化 Winsock
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
  begin
    WriteLn('Failed to initialize Winsock');
    Exit;
  end;
  
  try
    // 创建 WinSSL 库
    WriteLn('1. Creating WinSSL library...');
    SSLLibrary := CreateWinSSLLibrary;
    if not SSLLibrary.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize WinSSL library');
      Exit;
    end;
    WriteLn('   Library type: ', SSLLibrary.GetVersionString);
    WriteLn;
    
    // 创建上下文
    WriteLn('2. Creating SSL context...');
    SSLContext := SSLLibrary.CreateContext(sslCtxClient);
    SSLContext.SetServerName(aHost);
    WriteLn('   Context created');
    WriteLn;
    
    // 创建 socket 并连接
    WriteLn('3. Connecting to ', aHost, ':', aPort, '...');
    Socket := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if Socket = INVALID_SOCKET then
    begin
      WriteLn('ERROR: Failed to create socket');
      Exit;
    end;
    
    // 解析主机名
    HostEnt := gethostbyname(PChar(aHost));
    if HostEnt = nil then
    begin
      WriteLn('ERROR: Failed to resolve hostname');
      closesocket(Socket);
      Exit;
    end;
    
    // 连接到服务器
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(aPort);
    Addr.sin_addr.S_addr := PLongint(HostEnt^.h_addr_list^)^;
    
    if WinSock2.connect(Socket, @Addr, SizeOf(Addr)) <> 0 then
    begin
      WriteLn('ERROR: Failed to connect to server');
      closesocket(Socket);
      Exit;
    end;
    WriteLn('   Connected to server');
    WriteLn;
    
    // 创建 SSL 连接
    WriteLn('4. Performing SSL handshake...');
    SSLConnection := SSLContext.CreateConnection(Socket);
    if not SSLConnection.Connect then
    begin
      WriteLn('ERROR: SSL handshake failed');
      closesocket(Socket);
      Exit;
    end;
    WriteLn('   SSL handshake completed');
    WriteLn('   Protocol: ', ProtocolVersionToString(SSLConnection.GetProtocolVersion));
    WriteLn('   Cipher: ', SSLConnection.GetCipherName);
    WriteLn('   ALPN: ', SSLConnection.GetSelectedALPNProtocol);
    WriteLn;
    
    // 获取对端证书
    WriteLn('5. Retrieving peer certificate...');
    Cert := SSLConnection.GetPeerCertificate;
    if Cert = nil then
    begin
      WriteLn('ERROR: Failed to retrieve peer certificate');
    end
    else
    begin
      WriteLn('   Certificate retrieved successfully');
      WriteLn;
      
      // 显示证书信息
      WriteLn('6. Certificate Information:');
      WriteLn('   Subject: ', Cert.GetSubject);
      WriteLn('   Issuer: ', Cert.GetIssuer);
      WriteLn('   Serial Number: ', Cert.GetSerialNumber);
      WriteLn('   Version: ', Cert.GetVersion);
      WriteLn('   Not Before: ', DateTimeToStr(Cert.GetNotBefore));
      WriteLn('   Not After: ', DateTimeToStr(Cert.GetNotAfter));
      WriteLn('   Public Key Algorithm: ', Cert.GetPublicKeyAlgorithm);
      WriteLn('   Signature Algorithm: ', Cert.GetSignatureAlgorithm);
      WriteLn('   SHA-1 Fingerprint: ', Cert.GetFingerprintSHA1);
      WriteLn('   SHA-256 Fingerprint: ', Cert.GetFingerprintSHA256);
      WriteLn('   Is Self-Signed: ', BoolToStr(Cert.IsSelfSigned, True));
      WriteLn('   Is Expired: ', BoolToStr(Cert.IsExpired, True));
      
      // 显示 Subject Alternative Names
      WriteLn;
      WriteLn('   Subject Alternative Names:');
      SANs := Cert.GetSubjectAltNames;
      try
        if SANs.Count > 0 then
        begin
          for i := 0 to SANs.Count - 1 do
            WriteLn('     - ', SANs[i]);
        end
        else
          WriteLn('     (none)');
      finally
        SANs.Free;
      end;
      WriteLn;
    end;
    
    // 获取证书链
    WriteLn('7. Retrieving certificate chain...');
    CertChain := SSLConnection.GetPeerCertificateChain;
    WriteLn('   Chain length: ', Length(CertChain));
    for i := 0 to Length(CertChain) - 1 do
    begin
      WriteLn('   [', i, '] ', CertChain[i].GetSubject);
      WriteLn('       Issuer: ', CertChain[i].GetIssuer);
    end;
    WriteLn;
    
    // 验证证书
    WriteLn('8. Certificate Verification:');
    WriteLn('   Result: ', SSLConnection.GetVerifyResultString);
    WriteLn('   Code: ', SSLConnection.GetVerifyResult);
    WriteLn;
    
    // 发送 HTTP 请求测试
    WriteLn('9. Sending HTTP request...');
    HttpRequest := 'GET / HTTP/1.1' + #13#10 +
                   'Host: ' + aHost + #13#10 +
                   'Connection: close' + #13#10 +
                   'User-Agent: WinSSL-Test/1.0' + #13#10 +
                   #13#10;
    
    if SSLConnection.WriteString(HttpRequest) then
      WriteLn('   Request sent successfully')
    else
      WriteLn('   ERROR: Failed to send request');
    WriteLn;
    
    // 关闭连接
    WriteLn('10. Closing connection...');
    SSLConnection.Shutdown;
    closesocket(Socket);
    WriteLn('    Connection closed');
    WriteLn;
    
    WriteLn('=== Test Completed Successfully ===');
    
  finally
    WSACleanup;
  end;
end;

begin
  try
    if ParamCount >= 1 then
    begin
      if ParamCount >= 2 then
        TestCertificateInfo(ParamStr(1), StrToIntDef(ParamStr(2), 443))
      else
        TestCertificateInfo(ParamStr(1), 443);
    end
    else
    begin
      // 默认测试 www.google.com
      WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <hostname> [port]');
      WriteLn('Testing with default: www.google.com:443');
      WriteLn;
      TestCertificateInfo('www.google.com', 443);
    end;
    
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
      WriteLn;
      WriteLn('Press Enter to exit...');
      ReadLn;
    end;
  end;
end.
