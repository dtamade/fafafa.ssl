program test_openssl;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, WinSock2,
  {$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.base;

procedure TestOpenSSLConnection;
var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  Request: AnsiString;
  Response: array[0..4095] of Byte;
  BytesRead: Integer;
  TotalBytes: Integer;
  Cert: ISSLCertificate;
  WSAData: TWSAData;
begin
  WriteLn('=== OpenSSL Backend Test ===');
  WriteLn;
  
  {$IFDEF MSWINDOWS}
  // 初始化 Winsock
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
  begin
    WriteLn('Failed to initialize Winsock');
    Exit;
  end;
  {$ENDIF}
  
  try
    // 创建 OpenSSL 库实例
    WriteLn('Creating OpenSSL library instance...');
    Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if not Assigned(Lib) then
    begin
      WriteLn('Failed to create OpenSSL library instance');
      Exit;
    end;
    
    WriteLn('Library Name: ', Lib.GetName);
    WriteLn('Library Version: ', Lib.GetVersion);
    WriteLn;
    
    // 创建 SSL 上下文
    WriteLn('Creating SSL context...');
    Context := Lib.CreateContext;
    if not Assigned(Context) then
    begin
      WriteLn('Failed to create SSL context');
      Exit;
    end;
    
    // 设置验证模式
    Context.SetVerifyMode(svmPeer);
    
    // 创建连接
    WriteLn('Creating SSL connection...');
    Connection := Context.CreateConnection;
    if not Assigned(Connection) then
    begin
      WriteLn('Failed to create SSL connection');
      Exit;
    end;
    
    // 连接到服务器
    WriteLn('Connecting to www.google.com:443...');
    if not Connection.Connect('www.google.com', 443) then
    begin
      WriteLn('Failed to connect: ', Connection.GetLastError);
      Exit;
    end;
    
    WriteLn('Connected successfully!');
    WriteLn('Protocol Version: ', Connection.GetProtocolVersion);
    WriteLn('Cipher: ', Connection.GetCipherName);
    WriteLn;
    
    // 获取服务器证书
    Cert := Connection.GetPeerCertificate;
    if Assigned(Cert) then
    begin
      WriteLn('Server Certificate:');
      WriteLn('  Subject: ', Cert.GetSubject);
      WriteLn('  Issuer: ', Cert.GetIssuer);
      WriteLn('  Valid From: ', DateTimeToStr(Cert.GetNotBefore));
      WriteLn('  Valid To: ', DateTimeToStr(Cert.GetNotAfter));
      WriteLn('  Public Key: ', Cert.GetPublicKeyAlgorithm, ' (', Cert.GetPublicKeyBits, ' bits)');
      WriteLn;
    end;
    
    // 发送 HTTP 请求
    Request := 'GET / HTTP/1.1' + #13#10 +
               'Host: www.google.com' + #13#10 +
               'User-Agent: OpenSSL-Test/1.0' + #13#10 +
               'Accept: */*' + #13#10 +
               'Connection: close' + #13#10 +
               #13#10;
    
    WriteLn('Sending HTTP request...');
    if Connection.Write(Request[1], Length(Request)) <> Length(Request) then
    begin
      WriteLn('Failed to send request');
      Exit;
    end;
    
    // 读取响应
    WriteLn('Reading response...');
    WriteLn('----------------------------------------');
    
    TotalBytes := 0;
    repeat
      BytesRead := Connection.Read(Response, SizeOf(Response));
      if BytesRead > 0 then
      begin
        Write(Copy(PAnsiChar(@Response[0]), 1, BytesRead));
        Inc(TotalBytes, BytesRead);
        
        // 只显示前1000字节
        if TotalBytes > 1000 then
        begin
          WriteLn;
          WriteLn('... (truncated, total ', TotalBytes, ' bytes received)');
          Break;
        end;
      end;
    until BytesRead <= 0;
    
    WriteLn;
    WriteLn('----------------------------------------');
    
    // 断开连接
    Connection.Disconnect;
    WriteLn;
    WriteLn('Connection closed');
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.ClassName, ': ', E.Message);
    end;
  end;
  
  {$IFDEF MSWINDOWS}
  WSACleanup;
  {$ENDIF}
end;

procedure TestOpenSSLAvailability;
var
  Factory: TSSLFactory;
begin
  WriteLn('=== OpenSSL Availability Test ===');
  WriteLn;
  
  WriteLn('Checking if OpenSSL is available...');
  if TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
  begin
    WriteLn('OpenSSL is available!');
    WriteLn('Description: ', TSSLFactory.GetLibraryDescription(sslOpenSSL));
  end
  else
  begin
    WriteLn('OpenSSL is NOT available');
    WriteLn('Please make sure OpenSSL DLLs are in the system path or application directory');
    WriteLn('Required files:');
    {$IFDEF MSWINDOWS}
    WriteLn('  - libssl-3.dll');
    WriteLn('  - libcrypto-3.dll');
    {$ELSE}
    WriteLn('  - libssl.so');
    WriteLn('  - libcrypto.so');
    {$ENDIF}
  end;
  
  WriteLn;
end;

begin
  try
    TestOpenSSLAvailability;
    WriteLn;
    
    if TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
      TestOpenSSLConnection
    else
      WriteLn('Skipping connection test as OpenSSL is not available');
      
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.ClassName, ': ', E.Message);
      WriteLn('Press Enter to exit...');
      ReadLn;
    end;
  end;
end.