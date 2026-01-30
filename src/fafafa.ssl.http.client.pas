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
begin
  SetLength(Result, 0);
  
  if not ParseURL(AURL, Host, Path, Port, UseSSL) then
    raise Exception.Create('Invalid URL: ' + AURL);
  
  ContentLength := Length(AData);
  
  // 构建 HTTP POST 请求
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
  
  // 发送请求
  Result := SendRequest(Host, Port, Request, UseSSL);
  
  // TODO: 添加请求体 (AData)
  // TODO: 解析响应,提取 body
end;

end.
