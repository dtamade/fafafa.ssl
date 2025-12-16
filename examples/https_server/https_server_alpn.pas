program https_server_alpn;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ ALPN 演示服务器
  - 启用 ALPN 协商 (默认 h2,http/1.1)
  - 返回服务器选择的协议 }

uses
  SysUtils,
  fafafa.ssl, fafafa.ssl.base,
  https_server_common;

const
  DEFAULT_PROTOCOLS = 'h2,http/1.1';

function JsonEscape(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

procedure Log(const AMsg: string);
begin
  WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', AMsg);
end;

procedure HandleClient(AContext: ISSLContext; var AClientSocket: TSocketHandle);
var
  Connection: ISSLConnection;
  Request, SelectedProto, Body: string;
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

    try
      SelectedProto := Connection.GetSelectedALPNProtocol;
    except
      SelectedProto := '';
    end;

    Body := '{"selected_protocol":"' + JsonEscape(SelectedProto) + '"}';
    SendHTTPResponse(Connection, Body, 'application/json');
    Connection.Shutdown;
    Log('完成请求，ALPN=' + SelectedProto);
  except
    on E: Exception do
      Log('处理连接失败: ' + E.Message);
  end;
  CloseSocket(AClientSocket);
end;

procedure RunServer(APort: Word; const ACertFile, AKeyFile, AProtocols: string);
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
  Context.SetALPNProtocols(AProtocols);
  Context.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  ServerSocket := CreateListeningSocket(APort, Err);
  if ServerSocket = INVALID_SOCKET_HANDLE then
    raise Exception.Create('创建监听套接字失败: ' + Err);

  Log(Format('ALPN 服务器监听 https://0.0.0.0:%d (候选: %s)', [APort, AProtocols]));
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
  CertFile, KeyFile, Protocols, NetErr: string;
begin
  if ParamCount < 3 then
  begin
    WriteLn('用法: ./https_server_alpn <port> <server.crt> <server.key> [protocols]');
    Halt(1);
  end;

  if not InitNetwork(NetErr) then
  begin
    WriteLn('网络初始化失败: ', NetErr);
    Halt(1);
  end;
  try
    Port := StrToIntDef(ParamStr(1), 8443);
    CertFile := ParamStr(2);
    KeyFile := ParamStr(3);
    if ParamCount >= 4 then
      Protocols := ParamStr(4)
    else
      Protocols := DEFAULT_PROTOCOLS;

    try
      RunServer(Port, CertFile, KeyFile, Protocols);
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
