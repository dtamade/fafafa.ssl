{
  https_client - HTTPS 客户端示例
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    演示如何使用 fafafa.ssl 库创建 HTTPS 客户端
    包括发送 GET 和 POST 请求
}

program https_client;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes, Sockets,
  fafafa.ssl;

type
  { TSimpleHTTPSClient - 简单的 HTTPS 客户端实现 }
  TSimpleHTTPSClient = class
  private
    FHost: string;
    FPort: Integer;
    FSocket: TSocket;
    FSSLContext: ISSLContext;
    FSSLConnection: ISSLConnection;
    FConnected: Boolean;
    FResponseHeaders: TStringList;
    FResponseBody: string;
    FStatusCode: Integer;
    FStatusText: string;
    
    function CreateSocket: Boolean;
    function ConnectSocket: Boolean;
    procedure CloseSocket;
    function InitializeSSL: Boolean;
    function PerformSSLHandshake: Boolean;
    function SendData(const aData: string): Boolean;
    function ReceiveData: string;
    function ParseResponse(const aResponse: string): Boolean;
    function BuildHTTPRequest(const aMethod, aPath: string; 
                            const aHeaders: TStringList; 
                            const aBody: string = ''): string;
  public
    constructor Create(const aHost: string; aPort: Integer = 443);
    destructor Destroy; override;
    
    function Connect: Boolean;
    procedure Disconnect;
    
    function Get(const aPath: string; aHeaders: TStringList = nil): Boolean;
    function Post(const aPath: string; const aBody: string; 
                 aHeaders: TStringList = nil): Boolean;
    
    property Connected: Boolean read FConnected;
    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property ResponseHeaders: TStringList read FResponseHeaders;
    property ResponseBody: string read FResponseBody;
  end;

{ TSimpleHTTPSClient }

constructor TSimpleHTTPSClient.Create(const aHost: string; aPort: Integer);
begin
  inherited Create;
  FHost := aHost;
  FPort := aPort;
  FSocket := INVALID_SOCKET;
  FConnected := False;
  FResponseHeaders := TStringList.Create;
  FStatusCode := 0;
end;

destructor TSimpleHTTPSClient.Destroy;
begin
  if FConnected then
    Disconnect;
  FResponseHeaders.Free;
  inherited;
end;

function TSimpleHTTPSClient.CreateSocket: Boolean;
begin
  FSocket := fpSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  Result := FSocket <> INVALID_SOCKET;
  if not Result then
    WriteLn('错误: 创建 socket 失败');
end;

function TSimpleHTTPSClient.ConnectSocket: Boolean;
var
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
begin
  Result := False;
  
  // 解析主机名
  LHostEnt := GetHostByName(PChar(FHost));
  if LHostEnt = nil then
  begin
    WriteLn('错误: 无法解析主机名 ', FHost);
    Exit;
  end;
  
  // 设置地址结构
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(FPort);
  LAddr.sin_addr.s_addr := PInAddr(LHostEnt^.h_addr_list^)^.s_addr;
  
  // 连接
  Result := fpConnect(FSocket, @LAddr, SizeOf(LAddr)) = 0;
  if not Result then
    WriteLn('错误: 连接到 ', FHost, ':', FPort, ' 失败');
end;

procedure TSimpleHTTPSClient.CloseSocket;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

function TSimpleHTTPSClient.InitializeSSL: Boolean;
begin
  Result := False;
  
  try
    // 创建 SSL 上下文
    FSSLContext := CreateSSLContext(sslCtxClient);
    if not Assigned(FSSLContext) then
    begin
      WriteLn('错误: 创建 SSL 上下文失败');
      Exit;
    end;
    
    // 配置 SSL
    FSSLContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    FSSLContext.SetVerifyMode([sslVerifyPeer]);
    FSSLContext.SetServerName(FHost); // SNI
    
    // 创建 SSL 连接
    FSSLConnection := FSSLContext.CreateConnection(THandle(FSocket));
    if not Assigned(FSSLConnection) then
    begin
      WriteLn('错误: 创建 SSL 连接失败');
      Exit;
    end;
    
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('SSL 初始化异常: ', E.Message);
      Result := False;
    end;
  end;
end;

function TSimpleHTTPSClient.PerformSSLHandshake: Boolean;
begin
  Result := False;
  
  try
    WriteLn('正在进行 SSL 握手...');
    Result := FSSLConnection.Connect;
    
    if Result then
    begin
      WriteLn('SSL 握手成功!');
      WriteLn('  协议版本: ', ProtocolVersionToString(FSSLConnection.GetProtocolVersion));
      WriteLn('  密码套件: ', FSSLConnection.GetCipherName);
      
      // 获取服务器证书信息
      var LCert := FSSLConnection.GetPeerCertificate;
      if Assigned(LCert) then
      begin
        WriteLn('  服务器证书:');
        WriteLn('    主题: ', LCert.GetSubject);
        WriteLn('    颁发者: ', LCert.GetIssuer);
        WriteLn('    有效期: ', DateTimeToStr(LCert.GetNotBefore), 
                ' 至 ', DateTimeToStr(LCert.GetNotAfter));
      end;
    end
    else
      WriteLn('SSL 握手失败!');
      
  except
    on E: Exception do
    begin
      WriteLn('SSL 握手异常: ', E.Message);
      Result := False;
    end;
  end;
end;

function TSimpleHTTPSClient.Connect: Boolean;
begin
  Result := False;
  
  if FConnected then
  begin
    Result := True;
    Exit;
  end;
  
  WriteLn('连接到 ', FHost, ':', FPort, '...');
  
  // 创建并连接 socket
  if not CreateSocket then Exit;
  if not ConnectSocket then
  begin
    CloseSocket;
    Exit;
  end;
  
  // 初始化 SSL
  if not InitializeSSL then
  begin
    CloseSocket;
    Exit;
  end;
  
  // SSL 握手
  if not PerformSSLHandshake then
  begin
    FSSLConnection := nil;
    FSSLContext := nil;
    CloseSocket;
    Exit;
  end;
  
  FConnected := True;
  Result := True;
  WriteLn('连接成功!');
end;

procedure TSimpleHTTPSClient.Disconnect;
begin
  if not FConnected then Exit;
  
  WriteLn('断开连接...');
  
  // 关闭 SSL 连接
  if Assigned(FSSLConnection) then
  begin
    FSSLConnection.Shutdown;
    FSSLConnection := nil;
  end;
  
  FSSLContext := nil;
  
  // 关闭 socket
  CloseSocket;
  
  FConnected := False;
  WriteLn('已断开连接');
end;

function TSimpleHTTPSClient.SendData(const aData: string): Boolean;
var
  LBytes: TBytes;
  LSent: Integer;
begin
  Result := False;
  
  if not FConnected then
  begin
    WriteLn('错误: 未连接');
    Exit;
  end;
  
  LBytes := TEncoding.UTF8.GetBytes(aData);
  LSent := FSSLConnection.Write(LBytes[0], Length(LBytes));
  Result := LSent = Length(LBytes);
  
  if Result then
    WriteLn('发送 ', LSent, ' 字节')
  else
    WriteLn('发送失败');
end;

function TSimpleHTTPSClient.ReceiveData: string;
var
  LBuffer: array[0..4095] of Byte;
  LReceived: Integer;
  LResponse: TStringStream;
begin
  Result := '';
  
  if not FConnected then
  begin
    WriteLn('错误: 未连接');
    Exit;
  end;
  
  LResponse := TStringStream.Create('', TEncoding.UTF8);
  try
    repeat
      FillChar(LBuffer, SizeOf(LBuffer), 0);
      LReceived := FSSLConnection.Read(LBuffer, SizeOf(LBuffer));
      
      if LReceived > 0 then
      begin
        LResponse.Write(LBuffer, LReceived);
        WriteLn('接收 ', LReceived, ' 字节');
      end;
    until LReceived <= 0;
    
    Result := LResponse.DataString;
  finally
    LResponse.Free;
  end;
end;

function TSimpleHTTPSClient.BuildHTTPRequest(const aMethod, aPath: string;
  const aHeaders: TStringList; const aBody: string): string;
var
  LRequest: TStringList;
  I: Integer;
begin
  LRequest := TStringList.Create;
  try
    // 请求行
    LRequest.Add(Format('%s %s HTTP/1.1', [aMethod, aPath]));
    
    // 必需的头部
    LRequest.Add(Format('Host: %s', [FHost]));
    LRequest.Add('Connection: close');
    LRequest.Add('User-Agent: fafafa.ssl/1.0');
    
    // 自定义头部
    if Assigned(aHeaders) then
    begin
      for I := 0 to aHeaders.Count - 1 do
        LRequest.Add(aHeaders[I]);
    end;
    
    // Content-Length (对于 POST)
    if aBody <> '' then
      LRequest.Add(Format('Content-Length: %d', [Length(aBody)]));
    
    // 空行分隔头部和正文
    LRequest.Add('');
    
    // 请求正文
    if aBody <> '' then
      LRequest.Add(aBody);
    
    Result := LRequest.Text;
  finally
    LRequest.Free;
  end;
end;

function TSimpleHTTPSClient.ParseResponse(const aResponse: string): Boolean;
var
  LLines: TStringList;
  I, LHeaderEnd: Integer;
  LStatusLine: string;
  LParts: TStringList;
begin
  Result := False;
  FResponseHeaders.Clear;
  FResponseBody := '';
  FStatusCode := 0;
  FStatusText := '';
  
  if aResponse = '' then Exit;
  
  LLines := TStringList.Create;
  LParts := TStringList.Create;
  try
    LLines.Text := aResponse;
    if LLines.Count = 0 then Exit;
    
    // 解析状态行
    LStatusLine := LLines[0];
    LParts.Delimiter := ' ';
    LParts.DelimitedText := LStatusLine;
    if LParts.Count >= 3 then
    begin
      FStatusCode := StrToIntDef(LParts[1], 0);
      FStatusText := LParts[2];
      for I := 3 to LParts.Count - 1 do
        FStatusText := FStatusText + ' ' + LParts[I];
    end;
    
    // 查找头部结束位置
    LHeaderEnd := -1;
    for I := 1 to LLines.Count - 1 do
    begin
      if LLines[I] = '' then
      begin
        LHeaderEnd := I;
        Break;
      end;
      FResponseHeaders.Add(LLines[I]);
    end;
    
    // 提取正文
    if (LHeaderEnd >= 0) and (LHeaderEnd < LLines.Count - 1) then
    begin
      for I := LHeaderEnd + 1 to LLines.Count - 1 do
      begin
        if FResponseBody <> '' then
          FResponseBody := FResponseBody + #13#10;
        FResponseBody := FResponseBody + LLines[I];
      end;
    end;
    
    Result := FStatusCode > 0;
  finally
    LParts.Free;
    LLines.Free;
  end;
end;

function TSimpleHTTPSClient.Get(const aPath: string; aHeaders: TStringList): Boolean;
var
  LRequest, LResponse: string;
begin
  Result := False;
  
  if not FConnected then
  begin
    if not Connect then Exit;
  end;
  
  WriteLn('发送 GET 请求: ', aPath);
  
  // 构建请求
  LRequest := BuildHTTPRequest('GET', aPath, aHeaders);
  
  // 发送请求
  if not SendData(LRequest) then Exit;
  
  // 接收响应
  LResponse := ReceiveData;
  
  // 解析响应
  Result := ParseResponse(LResponse);
  
  if Result then
    WriteLn('响应: ', FStatusCode, ' ', FStatusText)
  else
    WriteLn('解析响应失败');
end;

function TSimpleHTTPSClient.Post(const aPath: string; const aBody: string;
  aHeaders: TStringList): Boolean;
var
  LRequest, LResponse: string;
  LHeaders: TStringList;
begin
  Result := False;
  
  if not FConnected then
  begin
    if not Connect then Exit;
  end;
  
  WriteLn('发送 POST 请求: ', aPath);
  
  // 准备头部
  LHeaders := TStringList.Create;
  try
    if Assigned(aHeaders) then
      LHeaders.Assign(aHeaders);
    
    // 添加 Content-Type 如果没有
    if LHeaders.IndexOfName('Content-Type') = -1 then
      LHeaders.Add('Content-Type: application/x-www-form-urlencoded');
    
    // 构建请求
    LRequest := BuildHTTPRequest('POST', aPath, LHeaders, aBody);
    
    // 发送请求
    if not SendData(LRequest) then Exit;
    
    // 接收响应
    LResponse := ReceiveData;
    
    // 解析响应
    Result := ParseResponse(LResponse);
    
    if Result then
      WriteLn('响应: ', FStatusCode, ' ', FStatusText)
    else
      WriteLn('解析响应失败');
  finally
    LHeaders.Free;
  end;
end;

{ 主程序 }

procedure TestHTTPSGet;
var
  LClient: TSimpleHTTPSClient;
begin
  WriteLn('========================================');
  WriteLn('测试 HTTPS GET 请求');
  WriteLn('========================================');
  
  // 注意：这是一个示例，实际的 SSL 握手实现还未完成
  // 所以这个程序目前只能演示框架结构
  
  LClient := TSimpleHTTPSClient.Create('www.example.com', 443);
  try
    if LClient.Connect then
    begin
      if LClient.Get('/', nil) then
      begin
        WriteLn('请求成功!');
        WriteLn('状态码: ', LClient.StatusCode);
        WriteLn('状态文本: ', LClient.StatusText);
        WriteLn('响应头部:');
        WriteLn(LClient.ResponseHeaders.Text);
        WriteLn('响应正文长度: ', Length(LClient.ResponseBody), ' 字节');
        
        // 显示前 200 个字符
        if Length(LClient.ResponseBody) > 0 then
        begin
          WriteLn('响应正文预览:');
          WriteLn(Copy(LClient.ResponseBody, 1, 200), '...');
        end;
      end
      else
        WriteLn('请求失败!');
    end
    else
      WriteLn('连接失败!');
  finally
    LClient.Free;
  end;
end;

procedure TestHTTPSPost;
var
  LClient: TSimpleHTTPSClient;
  LHeaders: TStringList;
begin
  WriteLn('========================================');
  WriteLn('测试 HTTPS POST 请求');
  WriteLn('========================================');
  
  LClient := TSimpleHTTPSClient.Create('httpbin.org', 443);
  LHeaders := TStringList.Create;
  try
    LHeaders.Add('Content-Type: application/json');
    
    if LClient.Connect then
    begin
      if LClient.Post('/post', '{"test": "data"}', LHeaders) then
      begin
        WriteLn('请求成功!');
        WriteLn('状态码: ', LClient.StatusCode);
        WriteLn('响应正文:');
        WriteLn(LClient.ResponseBody);
      end
      else
        WriteLn('请求失败!');
    end
    else
      WriteLn('连接失败!');
  finally
    LHeaders.Free;
    LClient.Free;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('fafafa.ssl HTTPS 客户端示例');
  WriteLn('========================================');
  WriteLn;
  WriteLn('用法: https_client [选项]');
  WriteLn;
  WriteLn('选项:');
  WriteLn('  get   - 测试 GET 请求');
  WriteLn('  post  - 测试 POST 请求');
  WriteLn('  all   - 运行所有测试');
  WriteLn;
  WriteLn('注意: 当前 SSL 握手功能尚未完全实现，');
  WriteLn('      此程序主要演示 API 使用方法。');
end;

begin
  try
    if ParamCount = 0 then
    begin
      ShowUsage;
      TestHTTPSGet;
    end
    else
    begin
      case LowerCase(ParamStr(1)) of
        'get': TestHTTPSGet;
        'post': TestHTTPSPost;
        'all': begin
          TestHTTPSGet;
          WriteLn;
          TestHTTPSPost;
        end;
      else
        ShowUsage;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn('错误: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  WriteLn;
  WriteLn('按 Enter 键退出...');
  ReadLn;
end.