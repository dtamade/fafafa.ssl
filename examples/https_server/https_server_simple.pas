program https_server_simple;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 基础 HTTPS 服务器示例
  - 监听指定端口 (默认 8443)
  - 使用 OpenSSL 后端
  - 响应最小 HTTP 请求并返回 JSON

  准备：
    1. 生成或准备 server.crt / server.key (PEM)
       可使用 examples/02_generate_certificate.pas 或 scripts/local_tls_server.sh
    2. 编译: fpc -Fu../../src -Fu../../src/openssl https_server_simple.pas
    3. 运行: ./https_server_simple [port] [cert] [key]
}

uses
  SysUtils,
  fafafa.ssl, fafafa.ssl.base,
  https_server_common;

const
  DEFAULT_PORT = 8443;
  DEFAULT_CERT_FILE = 'server.crt';
  DEFAULT_KEY_FILE = 'server.key';

procedure Log(const AMsg: string);
begin
  WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', AMsg);
end;

procedure HandleClient(AContext: ISSLContext; var AClientSocket: TSocketHandle);
var
  Connection: ISSLConnection;
  Request, Path, Body: string;
begin
  try
    Connection := AContext.CreateConnection(THandle(AClientSocket));
    if Connection = nil then
      raise Exception.Create('无法创建 SSL 连接');
    Connection.SetTimeout(5000);
    if not Connection.Accept then
      raise Exception.Create('TLS 握手失败');

    if not ReadHTTPRequest(Connection, Request) then
      raise Exception.Create('客户端未发送有效请求');

    Path := ExtractPath(Request);
    if Path = '' then
      Path := '/';

    if Path = '/health' then
      Body := '{"status":"ok"}'
    else
      Body := '{"message":"Hello from fafafa.ssl","path":"' + Path + '"}';

    SendHTTPResponse(Connection, Body, 'application/json');
    Connection.Shutdown;
    Log('完成请求: ' + Path);
  except
    on E: Exception do
      Log('处理连接失败: ' + E.Message);
  end;
  CloseSocket(AClientSocket);
end;

procedure RunServer(APort: Word; const ACertFile, AKeyFile: string);
var
  Context: ISSLContext;
  ServerSocket, ClientSocket: TSocketHandle;
  Err: string;
begin
  if not FileExists(ACertFile) then
    raise Exception.Create('找不到证书文件: ' + ACertFile);
  if not FileExists(AKeyFile) then
    raise Exception.Create('找不到私钥文件: ' + AKeyFile);

  Context := TSSLFactory.CreateContext(sslCtxServer, sslOpenSSL);
  if Context = nil then
    raise Exception.Create('无法创建 SSL 上下文');

  Context.LoadCertificate(ACertFile);
  Context.LoadPrivateKey(AKeyFile);
  Context.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  ServerSocket := CreateListeningSocket(APort, Err);
  if ServerSocket = INVALID_SOCKET_HANDLE then
    raise Exception.Create('创建监听套接字失败: ' + Err);

  Log(Format('监听 https://0.0.0.0:%d (Ctrl+C 退出)', [APort]));

  try
    while True do
    begin
      ClientSocket := AcceptClient(ServerSocket, Err);
      if ClientSocket = INVALID_SOCKET_HANDLE then
      begin
        Log('接受连接失败: ' + Err);
        Continue;
      end;
      HandleClient(Context, ClientSocket);
    end;
  finally
    CloseSocket(ServerSocket);
  end;
end;

var
  Port: Word;
  CertFile, KeyFile: string;
  NetErr: string;
begin
  if not InitNetwork(NetErr) then
  begin
    WriteLn('网络初始化失败: ', NetErr);
    Halt(1);
  end;
  try
    if ParamCount >= 1 then
      Port := StrToIntDef(ParamStr(1), DEFAULT_PORT)
    else
      Port := DEFAULT_PORT;
    if ParamCount >= 2 then
      CertFile := ParamStr(2)
    else
      CertFile := DEFAULT_CERT_FILE;
    if ParamCount >= 3 then
      KeyFile := ParamStr(3)
    else
      KeyFile := DEFAULT_KEY_FILE;

    try
      RunServer(Port, CertFile, KeyFile);
    except
      on E: Exception do
      begin
        WriteLn('服务器启动失败: ', E.Message);
        Halt(1);
      end;
    end;
  finally
    CleanupNetwork;
  end;
end.
