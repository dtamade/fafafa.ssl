unit fafafa.ssl.http.client;

{$mode ObjFPC}{$H+}

{
  简单的 HTTP 客户端
  
  为 OCSP Stapling 提供基础的 HTTP POST 支持。
  这是一个简化的实现,实际生产环境应使用更完整的 HTTP 库。
  
  @author fafafa.ssl team
  @version 1.0.0
}

interface

uses
  SysUtils, Classes, Sockets;

type
  // ========================================================================
  // 简单 HTTP 客户端
  // ========================================================================
  TSimpleHTTPClient = class
  private
    FTimeout: Integer;           // 超时时间 (毫秒)
    FContentType: string;        // Content-Type 头
    FUserAgent: string;          // User-Agent 头

    function ParseURL(const AURL: string; out AHost, APath: string;
      out APort: Word; out AUseSSL: Boolean): Boolean;
    function SendRequest(const AHost: string; APort: Word;
      const ARequest: string; AUseSSL: Boolean): TBytes;
    function SendRequestWithBody(const AHost: string; APort: Word;
      const AHeaders: string; const ABody: TBytes; AUseSSL: Boolean): TBytes;
    function FindResponseBodyStart(const AResponse: TBytes): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // HTTP POST 请求
    function Post(const AURL: string; const AData: TBytes): TBytes;

    property Timeout: Integer read FTimeout write FTimeout;
    property ContentType: string read FContentType write FContentType;
    property UserAgent: string read FUserAgent write FUserAgent;
  end;

implementation

uses
  fafafa.ssl.factory,
  fafafa.ssl.base;

// ========================================================================
// TSimpleHTTPClient
// ========================================================================

constructor TSimpleHTTPClient.Create;
begin
  inherited Create;
  FTimeout := 10000;  // 10 秒
  FContentType := 'application/ocsp-request';
  FUserAgent := 'fafafa.ssl/1.0';
end;

destructor TSimpleHTTPClient.Destroy;
begin
  inherited Destroy;
end;

function TSimpleHTTPClient.ParseURL(const AURL: string; out AHost, APath: string;
  out APort: Word; out AUseSSL: Boolean): Boolean;
var
  URL: string;
  Pos1, Pos2: Integer;
begin
  Result := False;
  AHost := '';
  APath := '/';
  APort := 80;
  AUseSSL := False;
  
  URL := AURL;
  
  // 检查协议
  if Copy(URL, 1, 8) = 'https://' then
  begin
    AUseSSL := True;
    APort := 443;
    Delete(URL, 1, 8);
  end
  else if Copy(URL, 1, 7) = 'http://' then
  begin
    AUseSSL := False;
    APort := 80;
    Delete(URL, 1, 7);
  end
  else
    Exit;
  
  // 查找路径分隔符
  Pos1 := Pos('/', URL);
  if Pos1 > 0 then
  begin
    AHost := Copy(URL, 1, Pos1 - 1);
    APath := Copy(URL, Pos1, Length(URL));
  end
  else
  begin
    AHost := URL;
    APath := '/';
  end;
  
  // 检查端口
  Pos2 := Pos(':', AHost);
  if Pos2 > 0 then
  begin
    try
      APort := StrToInt(Copy(AHost, Pos2 + 1, Length(AHost)));
      AHost := Copy(AHost, 1, Pos2 - 1);
    except
      Exit;
    end;
  end;
  
  Result := AHost <> '';
end;

function TSimpleHTTPClient.SendRequest(const AHost: string; APort: Word;
  const ARequest: string; AUseSSL: Boolean): TBytes;
var
  Socket: TSocket;
  Addr: TInetSockAddr;
  RequestBytes: TBytes;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  Response: TMemoryStream;
  SSLCtx: ISSLContext;
  SSLConn: ISSLConnection;
begin
  SetLength(Result, 0);
  
  // 创建 socket
  Socket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Socket < 0 then
    raise Exception.Create('Failed to create socket');
  
  try
    // 解析主机地址
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(APort);
    
    // 简化: 假设主机名是 IP 地址或可以直接解析
    // 实际实现应使用 DNS 解析
    Addr.sin_addr.s_addr := StrToNetAddr(AHost).s_addr;
    
    // 连接
    if fpConnect(Socket, @Addr, SizeOf(Addr)) < 0 then
      raise Exception.Create('Failed to connect to ' + AHost);
    
    RequestBytes := TEncoding.ASCII.GetBytes(ARequest);
    
    if AUseSSL then
    begin
      // 使用 SSL/TLS
      SSLCtx := TSSLFactory.CreateContext(sslCtxClient);
      SSLConn := SSLCtx.CreateConnection(Socket);
      
      SSLConn.Connect;
      SSLConn.Write(RequestBytes[0], Length(RequestBytes));
      
      Response := TMemoryStream.Create;
      try
        repeat
          BytesRead := SSLConn.Read(Buffer[0], SizeOf(Buffer));
          if BytesRead > 0 then
            Response.Write(Buffer[0], BytesRead);
        until BytesRead <= 0;
        
        SetLength(Result, Response.Size);
        if Response.Size > 0 then
          Move(Response.Memory^, Result[0], Response.Size);
      finally
        Response.Free;
      end;
    end
    else
    begin
      // 普通 HTTP
      fpSend(Socket, @RequestBytes[0], Length(RequestBytes), 0);
      
      Response := TMemoryStream.Create;
      try
        repeat
          BytesRead := fpRecv(Socket, @Buffer[0], SizeOf(Buffer), 0);
          if BytesRead > 0 then
            Response.Write(Buffer[0], BytesRead);
        until BytesRead <= 0;
        
        SetLength(Result, Response.Size);
        if Response.Size > 0 then
          Move(Response.Memory^, Result[0], Response.Size);
      finally
        Response.Free;
      end;
    end;
    
  finally
    CloseSocket(Socket);
  end;
  
  // 简化: 直接返回整个响应
  // 实际实现应解析 HTTP 响应头,提取 body
end;

function TSimpleHTTPClient.Post(const AURL: string; const AData: TBytes): TBytes;
var
  Host, Path: string;
  Port: Word;
  UseSSL: Boolean;
  Request: string;
  ContentLength: Integer;
  RawResponse: TBytes;
  BodyStart: Integer;
begin
  SetLength(Result, 0);

  if not ParseURL(AURL, Host, Path, Port, UseSSL) then
    raise Exception.Create('Invalid URL: ' + AURL);

  ContentLength := Length(AData);

  // 构建 HTTP POST 请求（包含请求头和请求体）
  Request := Format(
    'POST %s HTTP/1.1'#13#10 +
    'Host: %s'#13#10 +
    'User-Agent: %s'#13#10 +
    'Content-Type: %s'#13#10 +
    'Content-Length: %d'#13#10 +
    'Connection: close'#13#10 +
    #13#10,
    [Path, Host, FUserAgent, FContentType, ContentLength]
  );

  // 将请求体追加到请求字符串（需要转换为二进制形式发送）
  // 注意：SendRequest 需要修改以支持二进制请求体
  RawResponse := SendRequestWithBody(Host, Port, Request, AData, UseSSL);

  // 解析 HTTP 响应，提取 body
  BodyStart := FindResponseBodyStart(RawResponse);
  if BodyStart >= 0 then
  begin
    // 返回响应体
    Result := Copy(RawResponse, BodyStart, Length(RawResponse) - BodyStart);
  end
  else
  begin
    // 无法解析响应，返回整个响应（向后兼容）
    Result := RawResponse;
  end;
end;

function TSimpleHTTPClient.SendRequestWithBody(const AHost: string; APort: Word;
  const AHeaders: string; const ABody: TBytes; AUseSSL: Boolean): TBytes;
var
  Socket: TSocket;
  Addr: TInetSockAddr;
  HeaderBytes: TBytes;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  Response: TMemoryStream;
  SSLCtx: ISSLContext;
  SSLConn: ISSLConnection;
begin
  SetLength(Result, 0);

  // 创建 socket
  Socket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Socket < 0 then
    raise Exception.Create('Failed to create socket');

  try
    // 解析主机地址
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(APort);

    // 简化: 假设主机名是 IP 地址或可以直接解析
    // 实际实现应使用 DNS 解析
    Addr.sin_addr.s_addr := StrToNetAddr(AHost).s_addr;

    // 连接
    if fpConnect(Socket, @Addr, SizeOf(Addr)) < 0 then
      raise Exception.Create('Failed to connect to ' + AHost);

    HeaderBytes := TEncoding.ASCII.GetBytes(AHeaders);

    if AUseSSL then
    begin
      // 使用 SSL/TLS
      SSLCtx := TSSLFactory.CreateContext(sslCtxClient);
      SSLConn := SSLCtx.CreateConnection(Socket);

      SSLConn.Connect;
      // 发送请求头
      SSLConn.Write(HeaderBytes[0], Length(HeaderBytes));
      // 发送请求体
      if Length(ABody) > 0 then
        SSLConn.Write(ABody[0], Length(ABody));

      Response := TMemoryStream.Create;
      try
        repeat
          BytesRead := SSLConn.Read(Buffer[0], SizeOf(Buffer));
          if BytesRead > 0 then
            Response.Write(Buffer[0], BytesRead);
        until BytesRead <= 0;

        SetLength(Result, Response.Size);
        if Response.Size > 0 then
          Move(Response.Memory^, Result[0], Response.Size);
      finally
        Response.Free;
      end;
    end
    else
    begin
      // 普通 HTTP
      // 发送请求头
      fpSend(Socket, @HeaderBytes[0], Length(HeaderBytes), 0);
      // 发送请求体
      if Length(ABody) > 0 then
        fpSend(Socket, @ABody[0], Length(ABody), 0);

      Response := TMemoryStream.Create;
      try
        repeat
          BytesRead := fpRecv(Socket, @Buffer[0], SizeOf(Buffer), 0);
          if BytesRead > 0 then
            Response.Write(Buffer[0], BytesRead);
        until BytesRead <= 0;

        SetLength(Result, Response.Size);
        if Response.Size > 0 then
          Move(Response.Memory^, Result[0], Response.Size);
      finally
        Response.Free;
      end;
    end;

  finally
    CloseSocket(Socket);
  end;
end;

function TSimpleHTTPClient.FindResponseBodyStart(const AResponse: TBytes): Integer;
var
  I: Integer;
  HeaderEnd: Integer;
begin
  Result := -1;
  HeaderEnd := -1;

  // 查找 HTTP 响应头结束标记 (CRLFCRLF)
  // 头部以 #13#10#13#10 结束
  if Length(AResponse) < 4 then
    Exit;

  for I := 0 to Length(AResponse) - 4 do
  begin
    if (AResponse[I] = 13) and (AResponse[I + 1] = 10) and
       (AResponse[I + 2] = 13) and (AResponse[I + 3] = 10) then
    begin
      HeaderEnd := I;
      Break;
    end;
  end;

  if HeaderEnd >= 0 then
    Result := HeaderEnd + 4;  // 跳过 CRLFCRLF
end;

end.
