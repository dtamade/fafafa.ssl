program https_server_mtls;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 双向 TLS (mTLS) 服务器示例
  - 要求客户端提供证书
  - 返回客户端证书信息

  用法:
    ./https_server_mtls <port> <server.crt> <server.key> <ca.pem>
 }

uses
  SysUtils,
  fafafa.ssl, fafafa.ssl.base,
  https_server_common;

procedure Log(const AMsg: string);
begin
  WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', AMsg);
end;

function JsonEscape(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

procedure HandleClient(AContext: ISSLContext; var AClientSocket: TSocketHandle);
var
  Connection: ISSLConnection;
  Request: string;
  PeerCert: ISSLCertificate;
  Body: string;
begin
  try
    Connection := AContext.CreateConnection(THandle(AClientSocket));
    if Connection = nil then
      raise Exception.Create('无法创建 SSL 连接');
    Connection.SetTimeout(5000);
    if not Connection.Accept then
      raise Exception.Create('TLS 握手失败（客户端证书无效？）');

    if not ReadHTTPRequest(Connection, Request) then
      raise Exception.Create('客户端未发送有效请求');

    PeerCert := Connection.GetPeerCertificate;
    if PeerCert <> nil then
      Body := Format('{"authenticated":true,"subject":"%s","issuer":"%s"}',
        [JsonEscape(PeerCert.GetSubject), JsonEscape(PeerCert.GetIssuer)])
    else
      Body := '{"authenticated":false}';

    SendHTTPResponse(Connection, Body, 'application/json');
    Connection.Shutdown;
    Log('完成 mTLS 请求');
  except
    on E: Exception do
      Log('处理连接失败: ' + E.Message);
  end;
  CloseSocket(AClientSocket);
end;

procedure RunServer(APort: Word; const ACertFile, AKeyFile, ACAFile: string);
var
  Context: ISSLContext;
  ServerSocket, ClientSocket: TSocketHandle;
  Err: string;
begin
  if (ACertFile = '') or (AKeyFile = '') or (ACAFile = '') then
    raise Exception.Create('必须提供证书、私钥和 CA 文件');
  if not FileExists(ACertFile) then
    raise Exception.Create('找不到证书文件: ' + ACertFile);
  if not FileExists(AKeyFile) then
    raise Exception.Create('找不到私钥文件: ' + AKeyFile);
  if not FileExists(ACAFile) then
    raise Exception.Create('找不到 CA 文件: ' + ACAFile);

  Context := TSSLFactory.CreateContext(sslCtxServer, sslOpenSSL);
  if Context = nil then
    raise Exception.Create('无法创建 SSL 上下文');

  Context.LoadCertificate(ACertFile);
  Context.LoadPrivateKey(AKeyFile);
  Context.LoadCAFile(ACAFile);
  Context.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
  Context.SetVerifyDepth(3);

  ServerSocket := CreateListeningSocket(APort, Err);
  if ServerSocket = INVALID_SOCKET_HANDLE then
    raise Exception.Create('创建监听套接字失败: ' + Err);

  Log(Format('mTLS 服务器监听 https://0.0.0.0:%d', [APort]));
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
  CertFile, KeyFile, CAFile: string;
  NetErr: string;
begin
  if ParamCount < 4 then
  begin
    WriteLn('用法: ./https_server_mtls <port> <server.crt> <server.key> <ca.pem>');
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
    CAFile := ParamStr(4);

    try
      RunServer(Port, CertFile, KeyFile, CAFile);
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
