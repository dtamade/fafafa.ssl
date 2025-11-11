program https_rest_client;

{$mode objfpc}{$H+}

{ ============================================================================
  示例 4: HTTPS REST API 客户端（概念演示）
  
  功能：演示如何使用 fafafa.ssl 构建 REST API 请求
  用途：学习 HTTP 方法、请求头构建和响应处理的概念
  
  注意：本示例专注于展示 API 用法概念，不包含实际的网络连接代码。
        实际的网络连接需要根据目标平台选择合适的 socket 库。
  
  支持的 HTTP 方法：
    - GET: 获取资源
    - POST: 创建资源  
    - PUT: 更新资源
    - DELETE: 删除资源
  
  编译：fpc -Fusrc -Fusrc/openssl 04_https_rest_client.pas
  运行：04_https_rest_client
  ============================================================================ }

uses
  SysUtils, Classes,
  fafafa.ssl.types,
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

type
  { HTTP 方法枚举 }
  THTTPMethod = (httpGET, httpPOST, httpPUT, httpDELETE);
  
  { REST API 请求构建器 }
  TRESTRequestBuilder = class
  private
    FMethod: THTTPMethod;
    FPath: string;
    FHost: string;
    FHeaders: TStringList;
    FBody: string;
    
    function GetMethodName: string;
  public
    constructor Create(aMethod: THTTPMethod; const aHost, aPath: string);
    destructor Destroy; override;
    
    procedure AddHeader(const aName, aValue: string);
    procedure SetBody(const aBody: string);
    function BuildRequest: string;
    procedure DisplayRequest;
  end;

{ TRESTRequestBuilder }

constructor TRESTRequestBuilder.Create(aMethod: THTTPMethod; const aHost, aPath: string);
begin
  FMethod := aMethod;
  FHost := aHost;
  FPath := aPath;
  FHeaders := TStringList.Create;
  FBody := '';
  
  // 添加默认请求头
  AddHeader('Host', FHost);
  AddHeader('User-Agent', 'fafafa.ssl-rest-client/1.0');
  AddHeader('Accept', 'application/json');
  AddHeader('Connection', 'close');
end;

destructor TRESTRequestBuilder.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TRESTRequestBuilder.GetMethodName: string;
begin
  case FMethod of
    httpGET: Result := 'GET';
    httpPOST: Result := 'POST';
    httpPUT: Result := 'PUT';
    httpDELETE: Result := 'DELETE';
  end;
end;

procedure TRESTRequestBuilder.AddHeader(const aName, aValue: string);
begin
  FHeaders.Add(aName + ': ' + aValue);
end;

procedure TRESTRequestBuilder.SetBody(const aBody: string);
begin
  FBody := aBody;
  if FBody <> '' then
  begin
    AddHeader('Content-Type', 'application/json');
    AddHeader('Content-Length', IntToStr(Length(FBody)));
  end;
end;

function TRESTRequestBuilder.BuildRequest: string;
var
  i: Integer;
begin
  // 构建 HTTP 请求
  Result := GetMethodName + ' ' + FPath + ' HTTP/1.1'#13#10;
  
  // 添加所有请求头
  for i := 0 to FHeaders.Count - 1 do
    Result := Result + FHeaders[i] + #13#10;
  
  // 结束请求头
  Result := Result + #13#10;
  
  // 添加请求体（如果有）
  if FBody <> '' then
    Result := Result + FBody;
end;

procedure TRESTRequestBuilder.DisplayRequest;
var
  LRequest: string;
begin
  LRequest := BuildRequest;
  WriteLn('  请求内容:');
  WriteLn('  ', StringOfChar('-', 70));
  
  // 显示请求（如果太长则截断）
  if Length(LRequest) > 500 then
    WriteLn(Copy(LRequest, 1, 500), #13#10'  ... (', Length(LRequest) - 500, ' 字节已省略)')
  else
    Write(LRequest);
    
  WriteLn('  ', StringOfChar('-', 70));
  WriteLn('  请求大小: ', Length(LRequest), ' 字节');
end;

{ 演示函数 }

procedure DemonstrateGET;
var
  LBuilder: TRESTRequestBuilder;
begin
  WriteLn('[1/4] GET 请求示例');
  WriteLn('      用途：获取资源');
  WriteLn('      目标：GET https://api.example.com/users/123');
  WriteLn;
  
  LBuilder := TRESTRequestBuilder.Create(httpGET, 'api.example.com', '/users/123');
  try
    LBuilder.DisplayRequest;
  finally
    LBuilder.Free;
  end;
  WriteLn;
end;

procedure DemonstratePOST;
var
  LBuilder: TRESTRequestBuilder;
  LBody: string;
begin
  WriteLn('[2/4] POST 请求示例');
  WriteLn('      用途：创建新资源');
  WriteLn('      目标：POST https://api.example.com/users');
  WriteLn;
  
  LBuilder := TRESTRequestBuilder.Create(httpPOST, 'api.example.com', '/users');
  try
    LBody := '{"name":"张三","email":"zhangsan@example.com","age":30}';
    LBuilder.SetBody(LBody);
    LBuilder.DisplayRequest;
  finally
    LBuilder.Free;
  end;
  WriteLn;
end;

procedure DemonstratePUT;
var
  LBuilder: TRESTRequestBuilder;
  LBody: string;
begin
  WriteLn('[3/4] PUT 请求示例');
  WriteLn('      用途：更新现有资源');
  WriteLn('      目标：PUT https://api.example.com/users/123');
  WriteLn;
  
  LBuilder := TRESTRequestBuilder.Create(httpPUT, 'api.example.com', '/users/123');
  try
    LBody := '{"id":123,"name":"张三","email":"newemail@example.com","age":31}';
    LBuilder.SetBody(LBody);
    LBuilder.DisplayRequest;
  finally
    LBuilder.Free;
  end;
  WriteLn;
end;

procedure DemonstrateDELETE;
var
  LBuilder: TRESTRequestBuilder;
begin
  WriteLn('[4/4] DELETE 请求示例');
  WriteLn('      用途：删除资源');
  WriteLn('      目标：DELETE https://api.example.com/users/123');
  WriteLn;
  
  LBuilder := TRESTRequestBuilder.Create(httpDELETE, 'api.example.com', '/users/123');
  try
    LBuilder.DisplayRequest;
  finally
    LBuilder.Free;
  end;
  WriteLn;
end;

procedure DemonstrateSSLUsage;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
begin
  WriteLn('[SSL/TLS] fafafa.ssl 使用示例');
  WriteLn('          展示如何初始化 SSL 库并创建安全上下文');
  WriteLn;
  
  // 初始化 SSL 库
  WriteLn('  1. 初始化 SSL 库...');
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not LLib.Initialize then
  begin
    WriteLn('     ✗ 无法初始化 SSL 库');
    Exit;
  end;
  
  WriteLn('     ✓ SSL 库初始化成功');
  WriteLn('     版本: ', LLib.GetVersionString);
  WriteLn;
  
  try
    // 创建 SSL 上下文
    WriteLn('  2. 创建 SSL 上下文...');
    LContext := LLib.CreateContext(sslCtxClient);
    WriteLn('     ✓ 上下文创建成功');
    WriteLn;
    
    // 配置 TLS 参数
    WriteLn('  3. 配置 TLS 参数...');
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LContext.SetVerifyMode([sslVerifyPeer]);
    WriteLn('     ✓ 协议版本: TLS 1.2 / 1.3');
    WriteLn('     ✓ 证书验证: 已启用');
    WriteLn;
    
    WriteLn('  4. 准备就绪！');
    WriteLn('     此 SSL 上下文可用于创建安全的 HTTPS 连接');
    WriteLn('     使用 LContext.CreateConnection(socket) 创建 SSL 连接');
    WriteLn;
    
  finally
    LLib.Finalize;
  end;
end;

begin
  WriteLn('================================================================================');
  WriteLn('  示例 4: HTTPS REST API 客户端（概念演示）');
  WriteLn('  演示 GET/POST/PUT/DELETE 请求的构建方法');
  WriteLn('================================================================================');
  WriteLn;
  
  try
    // 演示各种 HTTP 方法
    DemonstrateGET;
    DemonstratePOST;
    DemonstratePUT;
    DemonstrateDELETE;
    
    WriteLn('================================================================================');
    WriteLn;
    
    // 演示 SSL/TLS 使用
    DemonstrateSSLUsage;
    
    WriteLn('================================================================================');
    WriteLn('  ✓ 示例执行完成！');
    WriteLn('================================================================================');
    WriteLn;
    WriteLn('💡 学到的知识：');
    WriteLn('  1. 如何构建 RESTful API 请求（GET/POST/PUT/DELETE）');
    WriteLn('  2. 如何添加和管理 HTTP 请求头');
    WriteLn('  3. 如何处理 JSON 请求体');
    WriteLn('  4. 如何初始化和配置 SSL/TLS 上下文');
    WriteLn('  5. HTTP 请求的完整结构');
    WriteLn;
    WriteLn('🔒 安全提示：');
    WriteLn('  - 始终使用 HTTPS（TLS加密）访问 API');
    WriteLn('  - 启用证书验证（sslVerifyPeer）防止中间人攻击');
    WriteLn('  - 使用 TLS 1.2 或更高版本');
    WriteLn('  - 不要在 URL 或日志中暴露敏感信息');
    WriteLn;
    WriteLn('📝 实现完整的 REST 客户端需要：');
    WriteLn('  1. Socket 连接库（根据平台选择）:');
    WriteLn('     - Windows: WinSock2');
    WriteLn('     - Linux/Unix: BaseUnix + Sockets');
    WriteLn('     - 跨平台: Synapse, lNet, Indy 等');
    WriteLn('  2. 使用 fafafa.ssl 包装 socket:');
    WriteLn('     Connection := Context.CreateConnection(YourSocket);');
    WriteLn('  3. 执行 TLS 握手:');
    WriteLn('     Connection.Connect;');
    WriteLn('  4. 发送/接收数据:');
    WriteLn('     Connection.WriteString(Request);');
    WriteLn('     Response := Connection.ReadString;');
    WriteLn;
    WriteLn('📚 下一步：');
    WriteLn('  - 查看示例 1: TLS 客户端 (01_tls_client.pas) - 包含完整网络代码');
    WriteLn('  - 查看示例 6: 数字签名与验证 (06_digital_signature.pas)');
    WriteLn('  - 阅读 docs/USER_GUIDE.md 了解完整的 API 使用');
    WriteLn;
    
    ExitCode := 0;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('================================================================================');
      WriteLn('  ✗ 错误: ', E.Message);
      WriteLn('================================================================================');
      WriteLn;
      ExitCode := 1;
    end;
  end;
end.
