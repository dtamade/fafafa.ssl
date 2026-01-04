program https_client_auth;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ HTTPS 客户端证书示例（mTLS）

  说明：
  - 本示例不再依赖历史上的 fafafa.ssl.http.simple（已移除）。
  - 通过 Context Builder 配置客户端证书/私钥，并使用 TSSLConnector 建立 TLS。
}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  BUFFER_SIZE = 16384;

procedure PrintHeader;
begin
  WriteLn('=====================================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #3 (客户端证书)');
  WriteLn('=====================================================');
  WriteLn('用法: ./https_client_auth <URL> <client_cert.pem> <client_key.pem> [ca_bundle.pem]');
  WriteLn('例如: ./https_client_auth https://localhost:8443/secure certs/client.crt certs/client.key ca.pem');
  WriteLn('提示: 可通过 scripts/local_tls_server.sh 启动本地测试服务。');
  WriteLn;
end;

procedure ValidateFile(const APath, ALabel: string);
begin
  if not FileExists(APath) then
  begin
    WriteLn('✗ 找不到 ', ALabel, ': ', APath);
    Halt(1);
  end;
end;

function ParseURL(const AURL: string; out AHost, APath: string; out APort: Word): Boolean;
var
  LTemp, LHostPart: string;
  LPos, LPortPos: Integer;
begin
  Result := False;
  AHost := '';
  APath := '/';
  APort := 443;

  LTemp := Trim(AURL);
  if Pos('https://', LowerCase(LTemp)) = 1 then
    Delete(LTemp, 1, 8)
  else if Pos('http://', LowerCase(LTemp)) = 1 then
  begin
    Delete(LTemp, 1, 7);
    APort := 80;
  end;

  LPos := Pos('/', LTemp);
  if LPos > 0 then
  begin
    LHostPart := Copy(LTemp, 1, LPos - 1);
    APath := Copy(LTemp, LPos, Length(LTemp));
  end
  else
    LHostPart := LTemp;

  LPortPos := Pos(':', LHostPart);
  if LPortPos > 0 then
  begin
    APort := StrToIntDef(Copy(LHostPart, LPortPos + 1, Length(LHostPart)), APort);
    AHost := Copy(LHostPart, 1, LPortPos - 1);
  end
  else
    AHost := LHostPart;

  Result := (AHost <> '');
end;

function ReadAll(AStream: TStream): string;
var
  Buffer: array[0..BUFFER_SIZE - 1] of Byte;
  N: Longint;
  Mem: TMemoryStream;
begin
  Result := '';
  Mem := TMemoryStream.Create;
  try
    repeat
      N := AStream.Read(Buffer[0], SizeOf(Buffer));
      if N > 0 then
        Mem.WriteBuffer(Buffer[0], N);
    until N = 0;

    if Mem.Size > 0 then
    begin
      SetLength(Result, Mem.Size);
      Mem.Position := 0;
      Mem.ReadBuffer(Result[1], Mem.Size);
    end;
  finally
    Mem.Free;
  end;
end;

procedure ExecuteRequest(const AURL, ACertFile, AKeyFile, ACAFile: string);
var
  Host, Path, NetErr: string;
  Port: Word;
  Sock: TSocketHandle;
  Builder: ISSLContextBuilder;
  Ctx: ISSLContext;
  Connector: TSSLConnector;
  TLS: TSSLStream;
  Request: RawByteString;
  Response: string;
begin
  if not ParseURL(AURL, Host, Path, Port) then
    raise Exception.Create('无法解析 URL: ' + AURL);

  if not InitNetwork(NetErr) then
    raise Exception.Create('网络初始化失败: ' + NetErr);

  Sock := INVALID_SOCKET;
  TLS := nil;
  try
    Sock := ConnectTCP(Host, Port);

    Builder := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithCertificate(ACertFile)
      .WithPrivateKey(AKeyFile);

    if ACAFile <> '' then
      Builder := Builder.WithCAFile(ACAFile)
    else
      Builder := Builder.WithSystemRoots;

    Ctx := Builder.BuildClient;

    Connector := TSSLConnector.FromContext(Ctx).WithTimeout(15000);
    TLS := Connector.ConnectSocket(THandle(Sock), Host);

    Request := 'GET ' + Path + ' HTTP/1.1'#13#10 +
               'Host: ' + Host + #13#10 +
               'User-Agent: fafafa.ssl-mtls-example/1.0'#13#10 +
               'Connection: close'#13#10 +
               #13#10;

    if Length(Request) > 0 then
      TLS.WriteBuffer(Request[1], Length(Request));

    Response := ReadAll(TLS);

    WriteLn('✓ 请求完成');
    WriteLn('响应长度: ', Length(Response), ' 字节');
    WriteLn;
    WriteLn(Response);
  finally
    if TLS <> nil then
      TLS.Free;
    CloseSocket(Sock);
    CleanupNetwork;
  end;
end;

var
  URL, CertFile, KeyFile, CAFile: string;
begin
  PrintHeader;
  if ParamCount < 3 then
  begin
    WriteLn('缺少参数，至少需要 URL、客户端证书和私钥。');
    Halt(1);
  end;

  URL := ParamStr(1);
  CertFile := ParamStr(2);
  KeyFile := ParamStr(3);
  if ParamCount >= 4 then
    CAFile := ParamStr(4)
  else
    CAFile := '';

  ValidateFile(CertFile, '客户端证书');
  ValidateFile(KeyFile, '客户端私钥');
  if CAFile <> '' then
    ValidateFile(CAFile, 'CA 文件');

  try
    ExecuteRequest(URL, CertFile, KeyFile, CAFile);
  except
    on E: Exception do
    begin
      WriteLn('✗ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
