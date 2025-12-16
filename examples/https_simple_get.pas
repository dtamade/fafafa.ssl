program https_simple_get;

{$mode objfpc}{$H+}

{
  超简单 HTTPS GET 请求示例
  
  功能：最简化的HTTPS请求示例，适合快速入门
  用途：验证库是否可用，学习基本用法
  难度：⭐ 入门级
}

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}WinSock2{$ELSE}Sockets{$ENDIF},
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

const
  TARGET_HOST = 'www.example.com';
  TARGET_PORT = 443;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LRequest, LResponse: string;

// 辅助函数：建立TCP连接
function ConnectTCP(const aHost: string; aPort: Word): TSocket;
var
  LAddr: TSockAddr;
  LHostEnt: PHostEnt;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
    raise Exception.Create('无法创建socket');
  
  LHostEnt := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if LHostEnt = nil then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('无法解析主机: %s', [aHost]);
  end;
  
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(aPort);
  LAddr.sin_addr := PInAddr(LHostEnt^.h_addr_list^)^;
  
  if connect(Result, LAddr, SizeOf(LAddr)) <> 0 then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('无法连接到 %s:%d', [aHost, aPort]);
  end;
end;

begin
  WriteLn('==========================================');
  WriteLn('   fafafa.ssl - 超简单 HTTPS 示例');
  WriteLn('==========================================');
  WriteLn;

  try
    {$IFDEF MSWINDOWS}
    var LWSAData: TWSAData;
    WSAStartup(MAKEWORD(2, 2), LWSAData);
    {$ENDIF}

    // 步骤1: 创建并初始化SSL库
    WriteLn('步骤 1: 初始化SSL库...');
    LLib := CreateOpenSSLLibrary;
    if not LLib.Initialize then
      raise Exception.Create('SSL库初始化失败');
    WriteLn('  ✓ 成功 (OpenSSL ', LLib.GetVersionString, ')');
    WriteLn;

    // 步骤2: 创建客户端上下文
    WriteLn('步骤 2: 创建SSL上下文...');
    LContext := LLib.CreateContext(sslCtxClient);
    if LContext = nil then
      raise Exception.Create('创建上下文失败');
    WriteLn('  ✓ 成功');
    WriteLn;

    // 步骤3: 建立TCP连接
    WriteLn('步骤 3: 连接到 ', TARGET_HOST, ':', TARGET_PORT, '...');
    LSocket := ConnectTCP(TARGET_HOST, TARGET_PORT);
    WriteLn('  ✓ TCP连接成功');
    WriteLn;

    // 步骤4: TLS握手
    WriteLn('步骤 4: 执行TLS握手...');
    LConn := LContext.CreateConnection(LSocket);
    if not LConn.Connect then
      raise Exception.Create('TLS握手失败');
    WriteLn('  ✓ 握手成功');
    WriteLn('    协议: ', GetProtocolName(LConn.GetProtocolVersion));
    WriteLn('    密码套件: ', LConn.GetCipherName);
    WriteLn;

    // 步骤5: 发送HTTP请求
    WriteLn('步骤 5: 发送HTTP请求...');
    LRequest := 'GET / HTTP/1.1'#13#10 +
                'Host: ' + TARGET_HOST + #13#10 +
                'Connection: close'#13#10 +
                #13#10;
    LConn.WriteString(LRequest);
    WriteLn('  ✓ 请求已发送 (', Length(LRequest), ' 字节)');
    WriteLn;

    // 步骤6: 接收响应
    WriteLn('步骤 6: 接收响应...');
    LResponse := LConn.ReadString;
    WriteLn('  ✓ 收到 ', Length(LResponse), ' 字节');
    WriteLn;

    // 显示响应（前500字符）
    WriteLn('==========================================');
    WriteLn('响应内容 (前500字符):');
    WriteLn('------------------------------------------');
    if Length(LResponse) > 500 then
      WriteLn(Copy(LResponse, 1, 500), '...')
    else
      WriteLn(LResponse);
    WriteLn('------------------------------------------');
    WriteLn;

    // 清理
    LConn.Shutdown;
    closesocket(LSocket);
    LLib.Finalize;

    WriteLn('==========================================');
    WriteLn('✅ 测试成功！');
    WriteLn('==========================================');
    WriteLn;
    WriteLn('💡 接下来你可以：');
    WriteLn('  1. 修改 TARGET_HOST 测试其他网站');
    WriteLn('  2. 查看 examples/01_tls_client.pas 了解更多细节');
    WriteLn('  3. 阅读文档学习证书验证等高级功能');
    WriteLn;

    {$IFDEF MSWINDOWS}
    WSACleanup;
    {$ENDIF}

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('==========================================');
      WriteLn('❌ 错误: ', E.Message);
      WriteLn('==========================================');
      Halt(1);
    end;
  end;

  WriteLn('按回车退出...');
  ReadLn;
end.


{$mode objfpc}{$H+}

{
  简单HTTPS GET请求示例
  
  功能：演示如何使用 fafafa.ssl 库进行简单的 HTTPS GET 请求
  用途：连接到 HTTPS 网站并获取响应
  难度：⭐ 入门级
}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LRequest: AnsiString;
  LResponse: TBytes;
  LBytesRead: Integer;
  I: Integer;
  LResponseStr: string;

begin
  WriteLn('===============================================');
  WriteLn('fafafa.ssl - 简单 HTTPS GET 请求示例');
  WriteLn('===============================================');
  WriteLn;

  try
    // 1. 创建SSL库实例 (自动选择 OpenSSL 或 WinSSL)
    WriteLn('[步骤 1/5] 创建 SSL 库实例...');
    LLib := TSSLFactory.CreateLibrary(sslOpenSSL);
    if not LLib.Initialize then
    begin
      WriteLn('错误: SSL 库初始化失败');
      WriteLn('原因: ', LLib.GetLastErrorString);
      Halt(1);
    end;
    WriteLn('✓ SSL 库初始化成功');
    WriteLn('  库类型: ', LLib.GetName);
    WriteLn('  版本: ', LLib.GetVersion);
    WriteLn;

    // 2. 创建客户端上下文
    WriteLn('[步骤 2/5] 创建客户端上下文...');
    LContext := LLib.CreateContext(sslCtxClient);
    if LContext = nil then
    begin
      WriteLn('错误: 创建上下文失败');
      Halt(1);
    end;
    WriteLn('✓ 上下文创建成功');
    WriteLn;

    // 3. 配置上下文（可选）
    WriteLn('[步骤 3/5] 配置连接参数...');
    LContext.SetServerName('www.example.com');
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    WriteLn('✓ 配置完成');
    WriteLn('  服务器: www.example.com');
    WriteLn('  端口: 443');
    WriteLn('  协议: TLS 1.2, TLS 1.3');
    WriteLn;

    // 4. 创建连接
    WriteLn('[步骤 4/5] 建立 HTTPS 连接...');
    LConnection := LContext.CreateConnection;
    if LConnection = nil then
    begin
      WriteLn('错误: 创建连接失败');
      Halt(1);
    end;

    // 连接到服务器
    if not LConnection.Connect('www.example.com', 443) then
    begin
      WriteLn('错误: 连接失败');
      Halt(1);
    end;
    WriteLn('✓ TLS 握手成功');
    WriteLn('  协议版本: TLS 1.2+');
    WriteLn('  加密套件: 已协商');
    WriteLn;

    // 5. 发送HTTP GET请求
    WriteLn('[步骤 5/5] 发送 HTTP 请求...');
    LRequest := 
      'GET / HTTP/1.1'#13#10 +
      'Host: www.example.com'#13#10 +
      'User-Agent: fafafa.ssl/1.0'#13#10 +
      'Accept: */*'#13#10 +
      'Connection: close'#13#10 +
      #13#10;

    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
    begin
      WriteLn('错误: 发送请求失败');
      Halt(1);
    end;
    WriteLn('✓ 请求已发送 (', Length(LRequest), ' 字节)');
    WriteLn;

    // 6. 接收响应
    WriteLn('===============================================');
    WriteLn('接收响应:');
    WriteLn('===============================================');
    
    SetLength(LResponse, 4096);
    LBytesRead := LConnection.Read(LResponse[0], Length(LResponse));
    
    if LBytesRead > 0 then
    begin
      WriteLn('✓ 收到响应 (', LBytesRead, ' 字节)');
      WriteLn;
      
      // 转换为字符串并显示前500字符
      SetLength(LResponse, LBytesRead);
      SetLength(LResponseStr, LBytesRead);
      Move(LResponse[0], LResponseStr[1], LBytesRead);
      
      WriteLn('响应内容 (前 500 字符):');
      WriteLn('-----------------------------------------------');
      if Length(LResponseStr) > 500 then
        WriteLn(Copy(LResponseStr, 1, 500), '...')
      else
        WriteLn(LResponseStr);
      WriteLn('-----------------------------------------------');
    end
    else
    begin
      WriteLn('警告: 未收到响应数据');
    end;

    WriteLn;
    WriteLn('===============================================');
    WriteLn('✅ 测试完成！');
    WriteLn('===============================================');
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('===============================================');
      WriteLn('❌ 发生错误:');
      WriteLn('  ', E.Message);
      WriteLn('===============================================');
      Halt(1);
    end;
  end;

  WriteLn;
  WriteLn('按回车键退出...');
  ReadLn;
end.
