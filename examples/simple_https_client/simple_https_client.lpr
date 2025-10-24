program simple_https_client;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

procedure PrintHeader;
begin
  WriteLn;
  WriteLn('================================================================================');
  WriteLn('              fafafa.ssl - Simple HTTPS Client Example');
  WriteLn('================================================================================');
  WriteLn;
end;

procedure Example1_QuickHTTPSGet;
var
  LResponse: string;
begin
  WriteLn('[Example 1] Quick HTTPS GET Request');
  WriteLn('------------------------------------');
  
  try
    // 最简单的方式：一行代码实现 HTTPS GET
    LResponse := TSSLFactory.QuickHTTPSGet('https://www.example.com');
    
    WriteLn('✅ Success!');
    WriteLn('Response length: ', Length(LResponse), ' bytes');
    WriteLn;
    WriteLn('First 200 characters:');
    WriteLn(Copy(LResponse, 1, 200), '...');
    WriteLn;
  except
    on E: Exception do
    begin
      WriteLn('❌ Error: ', E.Message);
      WriteLn;
    end;
  end;
end;

procedure Example2_ManualConnection;
var
  LConn: ISSLConnection;
  LRequest, LResponse: string;
begin
  WriteLn('[Example 2] Manual HTTPS Connection');
  WriteLn('------------------------------------');
  
  try
    // 创建连接
    WriteLn('Creating connection to www.google.com:443...');
    LConn := TSSLFactory.QuickClientConnection('www.google.com', 443);
    
    WriteLn('✅ Connected successfully');
    WriteLn('Using backend: ', LConn.GetBackendName);
    WriteLn;
    
    // 构造 HTTP 请求
    LRequest := 
      'GET / HTTP/1.1' + #13#10 +
      'Host: www.google.com' + #13#10 +
      'User-Agent: fafafa.ssl/1.0' + #13#10 +
      'Accept: text/html' + #13#10 +
      'Connection: close' + #13#10 +
      #13#10;
    
    // 发送请求
    WriteLn('Sending HTTP request...');
    LConn.Write(LRequest);
    
    // 读取响应
    WriteLn('Reading response...');
    LResponse := LConn.ReadAll;
    
    WriteLn('✅ Response received');
    WriteLn('Response length: ', Length(LResponse), ' bytes');
    WriteLn;
    
    // 显示响应头
    WriteLn('Response headers:');
    var LLines := TStringList.Create;
    try
      LLines.Text := LResponse;
      for var i := 0 to Min(15, LLines.Count - 1) do
      begin
        WriteLn('  ', LLines[i]);
        if Trim(LLines[i]) = '' then
          Break;  // 空行表示头部结束
      end;
    finally
      LLines.Free;
    end;
    WriteLn;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Error: ', E.Message);
      WriteLn;
    end;
  end;
end;

procedure Example3_WithContext;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LRequest, LResponse: string;
begin
  WriteLn('[Example 3] HTTPS Connection with Custom Context');
  WriteLn('-------------------------------------------------');
  
  try
    // 获取最佳SSL库
    LLib := TSSLFactory.CreateBest;
    WriteLn('Using SSL library: ', LLib.GetLibraryName);
    WriteLn('Version: ', LLib.GetVersion);
    WriteLn;
    
    // 创建客户端上下文
    LContext := LLib.CreateContext(sslRoleClient);
    
    // 配置上下文
    WriteLn('Configuring SSL context...');
    LContext.SetMinProtocolVersion(sslProtoTLS12);  // 至少 TLS 1.2
    LContext.SetVerifyMode(sslVerifyPeer);  // 验证服务器证书
    
    WriteLn('✅ Context configured');
    WriteLn;
    
    // 创建连接
    WriteLn('Connecting to httpbin.org:443...');
    LConn := LContext.CreateConnection('httpbin.org', 443);
    LConn.Connect;
    
    WriteLn('✅ Connected and SSL handshake completed');
    WriteLn;
    
    // 发送请求
    LRequest := 
      'GET /get HTTP/1.1' + #13#10 +
      'Host: httpbin.org' + #13#10 +
      'User-Agent: fafafa.ssl/1.0' + #13#10 +
      'Accept: application/json' + #13#10 +
      'Connection: close' + #13#10 +
      #13#10;
    
    WriteLn('Sending request to /get endpoint...');
    LConn.Write(LRequest);
    
    // 读取响应
    LResponse := LConn.ReadAll;
    
    WriteLn('✅ Response received');
    WriteLn('Response length: ', Length(LResponse), ' bytes');
    WriteLn;
    
    // 查找 JSON 内容
    var LJsonStart := Pos('{', LResponse);
    if LJsonStart > 0 then
    begin
      WriteLn('JSON Response:');
      WriteLn(Copy(LResponse, LJsonStart, 500), '...');
    end;
    WriteLn;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Error: ', E.Message);
      WriteLn;
    end;
  end;
end;

procedure Example4_ErrorHandling;
var
  LConn: ISSLConnection;
begin
  WriteLn('[Example 4] Error Handling');
  WriteLn('--------------------------');
  
  WriteLn('Attempting to connect to invalid hostname...');
  try
    LConn := TSSLFactory.QuickClientConnection('invalid-hostname-12345.com', 443);
    WriteLn('Connection created (unexpected!)');
  except
    on E: Exception do
    begin
      WriteLn('✅ Exception caught as expected:');
      WriteLn('   ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn;
  
  WriteLn('Attempting to connect to wrong port...');
  try
    LConn := TSSLFactory.QuickClientConnection('www.google.com', 12345);
    WriteLn('Connection created (unexpected!)');
  except
    on E: Exception do
    begin
      WriteLn('✅ Exception caught as expected:');
      WriteLn('   ', E.ClassName, ': ', E.Message);
    end;
  end;
  WriteLn;
end;

begin
  PrintHeader;
  
  // 运行示例
  Example1_QuickHTTPSGet;
  WriteLn;
  WriteLn('Press ENTER to continue to next example...');
  ReadLn;
  WriteLn;
  
  Example2_ManualConnection;
  WriteLn;
  WriteLn('Press ENTER to continue to next example...');
  ReadLn;
  WriteLn;
  
  Example3_WithContext;
  WriteLn;
  WriteLn('Press ENTER to continue to next example...');
  ReadLn;
  WriteLn;
  
  Example4_ErrorHandling;
  
  WriteLn('================================================================================');
  WriteLn('All examples completed! Press ENTER to exit...');
  ReadLn;
end.

