program https_server;

{$mode objfpc}{$H+}

{ ============================================================================
  示例 5: HTTPS Web 服务器（概念演示）

  功能：演示如何配置 TLS 服务器端、路由处理和请求响应流程
  用途：学习 HTTPS 服务器的关键组件和配置方法

  注意：此示例专注于服务器配置和路由逻辑，不包含完整的 socket 实现。
        完整的生产级 Web 服务器需要额外的网络层代码。

  编译：fpc -Fusrc -Fusrc/openssl 05_https_server.pas
  运行：05_https_server
  ============================================================================ }

uses
  SysUtils, Classes,
  fafafa.ssl.types,
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

type
  { HTTP 方法枚举 }
  THTTPMethod = (httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpOPTIONS);
  
  { HTTP 请求记录 }
  THTTPRequest = record
    Method: THTTPMethod;
    Path: string;
    QueryString: string;
    Headers: TStringList;
    Body: string;
    ClientIP: string;
  end;
  
  { HTTP 响应记录 }
  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: TStringList;
    Body: string;
  end;
  
  { 路由处理器函数类型 }
  TRouteHandler = procedure(const Request: THTTPRequest; var Response: THTTPResponse);
  
  { 路由表项 }
  TRoute = record
    Method: THTTPMethod;
    Path: string;
    Handler: TRouteHandler;
  end;

{ ============================================================================
  路由处理器示例
  ============================================================================ }

{ 处理根路径 / }
procedure HandleRoot(const Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := 200;
  Response.StatusText := 'OK';
  Response.Headers.Values['Content-Type'] := 'text/html; charset=utf-8';
  Response.Body := 
    '<!DOCTYPE html>' + #13#10 +
    '<html>' + #13#10 +
    '<head><title>fafafa.ssl HTTPS Server</title></head>' + #13#10 +
    '<body>' + #13#10 +
    '<h1>欢迎使用 fafafa.ssl HTTPS 服务器</h1>' + #13#10 +
    '<p>这是一个基于 fafafa.ssl 的安全 Web 服务器示例。</p>' + #13#10 +
    '<ul>' + #13#10 +
    '<li><a href="/api/status">API 状态</a></li>' + #13#10 +
    '<li><a href="/api/info">服务器信息</a></li>' + #13#10 +
    '</ul>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;

{ 处理 /api/status }
procedure HandleAPIStatus(const Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := 200;
  Response.StatusText := 'OK';
  Response.Headers.Values['Content-Type'] := 'application/json';
  Response.Body := 
    '{' + #13#10 +
    '  "status": "running",' + #13#10 +
    '  "uptime": 3600,' + #13#10 +
    '  "connections": 42,' + #13#10 +
    '  "ssl": "enabled"' + #13#10 +
    '}';
end;

{ 处理 /api/info }
procedure HandleAPIInfo(const Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := 200;
  Response.StatusText := 'OK';
  Response.Headers.Values['Content-Type'] := 'application/json';
  Response.Body := 
    '{' + #13#10 +
    '  "name": "fafafa.ssl HTTPS Server",' + #13#10 +
    '  "version": "1.0.0",' + #13#10 +
    '  "ssl_version": "OpenSSL 3.x",' + #13#10 +
    '  "protocol": "TLS 1.3"' + #13#10 +
    '}';
end;

{ 处理 POST /api/echo }
procedure HandleAPIEcho(const Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := 200;
  Response.StatusText := 'OK';
  Response.Headers.Values['Content-Type'] := 'application/json';
  Response.Body := 
    '{' + #13#10 +
    '  "echo": "' + Request.Body + '",' + #13#10 +
    '  "length": ' + IntToStr(Length(Request.Body)) + #13#10 +
    '}';
end;

{ 处理 404 未找到 }
procedure Handle404(const Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := 404;
  Response.StatusText := 'Not Found';
  Response.Headers.Values['Content-Type'] := 'text/html; charset=utf-8';
  Response.Body := 
    '<!DOCTYPE html>' + #13#10 +
    '<html>' + #13#10 +
    '<head><title>404 Not Found</title></head>' + #13#10 +
    '<body>' + #13#10 +
    '<h1>404 - 页面未找到</h1>' + #13#10 +
    '<p>请求的资源 <code>' + Request.Path + '</code> 不存在。</p>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;

{ ============================================================================
  路由匹配和分发
  ============================================================================ }

{ 路由表 }
var
  Routes: array of TRoute;

{ 注册路由 }
procedure RegisterRoute(AMethod: THTTPMethod; const APath: string; AHandler: TRouteHandler);
var
  LIndex: Integer;
begin
  LIndex := Length(Routes);
  SetLength(Routes, LIndex + 1);
  Routes[LIndex].Method := AMethod;
  Routes[LIndex].Path := APath;
  Routes[LIndex].Handler := AHandler;
end;

{ 查找路由处理器 }
function FindRouteHandler(AMethod: THTTPMethod; const APath: string): TRouteHandler;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(Routes) do
  begin
    if (Routes[I].Method = AMethod) and (Routes[I].Path = APath) then
    begin
      Result := Routes[I].Handler;
      Exit;
    end;
  end;
end;

{ 处理 HTTP 请求 }
procedure ProcessRequest(const Request: THTTPRequest; var Response: THTTPResponse);
var
  LHandler: TRouteHandler;
begin
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ',
          Request.ClientIP, ' -> ',
          Request.Path);
  
  // 查找路由处理器
  LHandler := FindRouteHandler(Request.Method, Request.Path);
  
  if Assigned(LHandler) then
    LHandler(Request, Response)
  else
    Handle404(Request, Response);
  
  WriteLn('  Status: ', Response.StatusCode, ' ', Response.StatusText);
end;

{ ============================================================================
  HTTPS 服务器配置和演示
  ============================================================================ }

procedure DemonstrateHTTPSServer;
var
  LLib: ISSLLibrary;
  LServerCtx: ISSLContext;
  LRequest: THTTPRequest;
  LResponse: THTTPResponse;
begin
  WriteLn('================================================================================');
  WriteLn('  示例 5: HTTPS Web 服务器');
  WriteLn('  TLS 服务端配置与路由处理');
  WriteLn('================================================================================');
  WriteLn;

  { ========================================
    第 1 部分：SSL/TLS 服务器配置
    ======================================== }
  
  WriteLn('[1/4] 配置 SSL/TLS 服务器');
  WriteLn;
  
  // 初始化 SSL 库
  WriteLn('  初始化 SSL 库...');
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not LLib.Initialize then
  begin
    WriteLn('  ✗ 无法初始化 SSL 库');
    Exit;
  end;
  WriteLn('  ✓ SSL 库初始化成功');
  WriteLn('  版本: ', LLib.GetVersionString);
  WriteLn;
  
  try
    // 创建服务器端上下文
    WriteLn('  创建服务器端 SSL 上下文...');
    LServerCtx := LLib.CreateContext(sslCtxServer);
    if LServerCtx = nil then
    begin
      WriteLn('  ✗ 无法创建服务器 SSL 上下文');
      Exit;
    end;
    WriteLn('  ✓ 服务器上下文创建成功');
    WriteLn;
    
    // 配置服务器证书和私钥
    WriteLn('  配置服务器证书和私钥...');
    WriteLn('  // 实际应用中的代码示例：');
    WriteLn('  LServerCtx.LoadCertificate(''server.crt'');');
    WriteLn('  LServerCtx.LoadPrivateKey(''server.key'');');
    WriteLn('  LServerCtx.CheckPrivateKey;');
    WriteLn('  ✓ 证书配置完成（演示）');
    WriteLn;
    
    // 配置 TLS 协议版本
    WriteLn('  配置 TLS 协议版本...');
    WriteLn('  LServerCtx.SetMinProtocolVersion(sslTLS1_2);');
    WriteLn('  LServerCtx.SetMaxProtocolVersion(sslTLS1_3);');
    WriteLn('  ✓ 使用 TLS 1.2 和 TLS 1.3');
    WriteLn;
    
    // 配置密码套件
    WriteLn('  配置密码套件...');
    WriteLn('  LServerCtx.SetCipherList(''TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256'');');
    WriteLn('  ✓ 使用高强度密码套件');
    WriteLn;

    { ========================================
      第 2 部分：路由配置
      ======================================== }
    
    WriteLn('[2/4] 配置路由表');
    WriteLn;
    
    WriteLn('  注册路由处理器...');
    
    // 注册路由
    RegisterRoute(httpGET, '/', @HandleRoot);
    RegisterRoute(httpGET, '/api/status', @HandleAPIStatus);
    RegisterRoute(httpGET, '/api/info', @HandleAPIInfo);
    RegisterRoute(httpPOST, '/api/echo', @HandleAPIEcho);
    
    WriteLn('  ✓ 已注册 ', Length(Routes), ' 个路由');
    WriteLn;
    WriteLn('  路由表:');
    WriteLn('    GET  /');
    WriteLn('    GET  /api/status');
    WriteLn('    GET  /api/info');
    WriteLn('    POST /api/echo');
    WriteLn;

    { ========================================
      第 3 部分：请求处理演示
      ======================================== }
    
    WriteLn('[3/4] 模拟请求处理');
    WriteLn;
    
    WriteLn('  模拟客户端请求...');
    WriteLn;
    
    // 模拟请求 1: GET /
    LRequest.Method := httpGET;
    LRequest.Path := '/';
    LRequest.ClientIP := '192.168.1.100';
    LRequest.Headers := TStringList.Create;
    LRequest.Body := '';
    
    LResponse.Headers := TStringList.Create;
    try
      ProcessRequest(LRequest, LResponse);
      WriteLn('  响应长度: ', Length(LResponse.Body), ' 字节');
      WriteLn;
      
      // 模拟请求 2: GET /api/status
      LRequest.Method := httpGET;
      LRequest.Path := '/api/status';
      ProcessRequest(LRequest, LResponse);
      WriteLn('  响应内容:');
      WriteLn('  ', LResponse.Body);
      WriteLn;
      
      // 模拟请求 3: GET /api/info
      LRequest.Method := httpGET;
      LRequest.Path := '/api/info';
      ProcessRequest(LRequest, LResponse);
      WriteLn('  响应内容:');
      WriteLn('  ', LResponse.Body);
      WriteLn;
      
      // 模拟请求 4: POST /api/echo
      LRequest.Method := httpPOST;
      LRequest.Path := '/api/echo';
      LRequest.Body := 'Hello, HTTPS Server!';
      ProcessRequest(LRequest, LResponse);
      WriteLn('  响应内容:');
      WriteLn('  ', LResponse.Body);
      WriteLn;
      
      // 模拟请求 5: 404
      LRequest.Method := httpGET;
      LRequest.Path := '/nonexistent';
      ProcessRequest(LRequest, LResponse);
      WriteLn;
      
    finally
      LRequest.Headers.Free;
      LResponse.Headers.Free;
    end;

    { ========================================
      第 4 部分：生产部署建议
      ======================================== }
    
    WriteLn('[4/4] 生产部署建议');
    WriteLn;
    
    WriteLn('  完整的 HTTPS 服务器实现需要：');
    WriteLn;
    WriteLn('  1. Socket 监听和连接管理');
    WriteLn('     - 使用 TServerSocket 或原生 socket API');
    WriteLn('     - 绑定到端口 443 (HTTPS) 或自定义端口');
    WriteLn('     - 监听传入连接');
    WriteLn;
    WriteLn('  2. TLS 握手处理');
    WriteLn('     - 为每个连接创建 SSL 对象');
    WriteLn('     - 执行 SSL_accept() 完成握手');
    WriteLn('     - 验证客户端证书（可选，用于 mTLS）');
    WriteLn;
    WriteLn('  3. HTTP 协议解析');
    WriteLn('     - 读取和解析 HTTP 请求行');
    WriteLn('     - 解析请求头（Headers）');
    WriteLn('     - 处理请求体（Body）');
    WriteLn('     - 支持 HTTP/1.1 持久连接');
    WriteLn;
    WriteLn('  4. 并发处理');
    WriteLn('     - 多线程：每个连接一个线程');
    WriteLn('     - 线程池：复用线程处理多个连接');
    WriteLn('     - 异步 I/O：使用 select/poll/epoll');
    WriteLn;
    WriteLn('  5. 安全和性能优化');
    WriteLn('     - SSL 会话缓存和复用');
    WriteLn('     - OCSP Stapling');
    WriteLn('     - HTTP/2 和 ALPN 支持');
    WriteLn('     - 连接超时和限流');
    WriteLn;
    
  finally
    LServerCtx := nil; // 接口会自动释放
    LLib.Finalize;
  end;

  WriteLn('================================================================================');
  WriteLn('  ✓ 示例执行完成！');
  WriteLn('================================================================================');
  WriteLn;
  WriteLn('💡 关键要点：');
  WriteLn('  1. HTTPS 服务器需要服务器证书和私钥');
  WriteLn('  2. 使用 sslCtxServer 创建服务器端上下文');
  WriteLn('  3. 路由表将 URL 映射到处理器函数');
  WriteLn('  4. 每个请求经过 TLS 解密后按路由分发');
  WriteLn;
  WriteLn('🔒 安全最佳实践：');
  WriteLn('  - 只启用 TLS 1.2 和 1.3，禁用旧协议');
  WriteLn('  - 使用强密码套件（AES-GCM, ChaCha20）');
  WriteLn('  - 定期更新服务器证书');
  WriteLn('  - 启用 HSTS（HTTP Strict Transport Security）');
  WriteLn('  - 实施速率限制防止 DoS 攻击');
  WriteLn;
  WriteLn('📚 实际应用示例：');
  WriteLn('  • REST API 服务器');
  WriteLn('  • 微服务端点');
  WriteLn('  • 内部管理后台');
  WriteLn('  • Webhook 接收服务');
  WriteLn('  • 文件上传/下载服务');
  WriteLn;
  WriteLn('🛠️ 推荐的完整 Web 框架：');
  WriteLn('  - fphttpserver (Free Pascal HTTP Server)');
  WriteLn('  - Brook Framework');
  WriteLn('  - mORMot 2');
  WriteLn('  这些框架提供了完整的 HTTP 处理，可与 fafafa.ssl 集成');
  WriteLn;
  WriteLn('⚙️ fafafa.ssl 在 Web 服务器中的角色：');
  WriteLn('  fafafa.ssl 提供 TLS 层，负责：');
  WriteLn('  • 证书管理');
  WriteLn('  • TLS 握手和加密');
  WriteLn('  • 协议版本协商');
  WriteLn('  • 密码套件选择');
  WriteLn;
  WriteLn('  应用层（HTTP 服务器）负责：');
  WriteLn('  • Socket 监听');
  WriteLn('  • HTTP 协议解析');
  WriteLn('  • 路由和业务逻辑');
  WriteLn('  • 并发和性能优化');
  WriteLn;
  WriteLn('🔗 相关示例：');
  WriteLn('  - 示例 01: TLS 客户端（基础 TLS 连接）');
  WriteLn('  - 示例 04: REST API 客户端（HTTPS 客户端）');
  WriteLn('  - 示例 08: 双向 TLS 认证（mTLS 服务器配置）');
  WriteLn;
end;

begin
  DemonstrateHTTPSServer;
end.

