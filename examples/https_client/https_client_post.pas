program https_client_post;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ HTTPS POST 示例

  说明：
  - 本示例不再依赖历史上的 fafafa.ssl.http.simple（已移除）。
  - 改为使用 Rust 风格门面：TSSLConnector + TSSLStream。
}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  DEFAULT_URL = 'https://httpbin.org/post';
  DEFAULT_BODY = '{"message":"hello from fafafa.ssl"}';
  DEFAULT_CONTENT_TYPE = 'application/json';
  BUFFER_SIZE = 16384;

procedure PrintHeader;
begin
  WriteLn('============================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #2 (POST)');
  WriteLn('============================================');
  WriteLn('用法: ./https_client_post [URL] [BODY] [CONTENT-TYPE]');
  WriteLn('缺省 URL: ', DEFAULT_URL);
  WriteLn('缺省内容: ', DEFAULT_BODY);
  WriteLn('缺省类型: ', DEFAULT_CONTENT_TYPE);
  WriteLn;
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

procedure ExecuteRequest(const AURL, ABody, AContentType: string);
var
  Host, Path, NetErr: string;
  Port: Word;
  Sock: TSocketHandle;
  Ctx: ISSLContext;
  Connector: TSSLConnector;
  TLS: TSSLStream;
  Request: RawByteString;
  Response: string;
  ContentLen: string;
begin
  if not ParseURL(AURL, Host, Path, Port) then
    raise Exception.Create('无法解析 URL: ' + AURL);

  if Pos('https://', LowerCase(Trim(AURL))) <> 1 then
    WriteLn('⚠ 提示: URL 不是 https://，本示例仍会尝试建立 TLS 连接。');

  if not InitNetwork(NetErr) then
    raise Exception.Create('网络初始化失败: ' + NetErr);

  Sock := INVALID_SOCKET;
  TLS := nil;
  try
    Sock := ConnectTCP(Host, Port);

    // 启用证书验证并加载系统根证书
    Ctx := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;

    Connector := TSSLConnector.FromContext(Ctx).WithTimeout(15000);
    TLS := Connector.ConnectSocket(THandle(Sock), Host);

    ContentLen := IntToStr(Length(ABody));

    Request := 'POST ' + Path + ' HTTP/1.1'#13#10 +
               'Host: ' + Host + #13#10 +
               'User-Agent: fafafa.ssl-example/1.0'#13#10 +
               'Accept: application/json'#13#10 +
               'Content-Type: ' + AContentType + #13#10 +
               'Content-Length: ' + ContentLen + #13#10 +
               'Connection: close'#13#10 +
               #13#10 +
               RawByteString(ABody);

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
  URL, Body, ContentType: string;
begin
  PrintHeader;
  if ParamCount >= 1 then
    URL := ParamStr(1)
  else
    URL := DEFAULT_URL;

  if ParamCount >= 2 then
    Body := ParamStr(2)
  else
    Body := DEFAULT_BODY;

  if ParamCount >= 3 then
    ContentType := ParamStr(3)
  else
    ContentType := DEFAULT_CONTENT_TYPE;

  try
    ExecuteRequest(URL, Body, ContentType);
  except
    on E: Exception do
    begin
      WriteLn('✗ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
