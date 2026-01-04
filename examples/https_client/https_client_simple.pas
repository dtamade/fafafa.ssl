program https_client_simple;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 简单 HTTPS GET 示例

  说明：
  - 本示例不再依赖历史上的 fafafa.ssl.http.simple（已移除）。
  - 改为使用 Rust 风格门面：TSSLConnector + TSSLStream。
  - SNI/hostname 通过 per-connection 的 ISSLClientConnection.SetServerName 自动设置。
}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  DEFAULT_URL = 'https://httpbin.org/get';
  PREVIEW_LENGTH = 200;
  BUFFER_SIZE = 16384;

procedure PrintHeader;
begin
  WriteLn('=============================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #1 (GET)');
  WriteLn('=============================================');
  WriteLn('用法: ./https_client_simple [URL]');
  WriteLn('缺省 URL: ', DEFAULT_URL);
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
    // 本示例仍然会走 TLS（主要用于演示），但会提示你 URL 不是 https://
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

procedure PrintPreview(const ABody: string);
var
  LPreview: string;
begin
  if ABody = '' then
  begin
    WriteLn('响应体为空。');
    Exit;
  end;
  if Length(ABody) > PREVIEW_LENGTH then
    LPreview := Copy(ABody, 1, PREVIEW_LENGTH) + '... (截断)'
  else
    LPreview := ABody;
  WriteLn('内容预览:');
  WriteLn(LPreview);
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

procedure ExecuteRequest(const AURL: string);
var
  Host, Path, NetErr: string;
  Port: Word;
  Sock: TSocketHandle;
  Ctx: ISSLContext;
  Connector: TSSLConnector;
  TLS: TSSLStream;
  Request: RawByteString;
  Response: string;
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
    // 1) 建立 TCP 连接
    Sock := ConnectTCP(Host, Port);

    // 2) 构建 TLS Context（启用证书验证 + 使用系统根证书）
    Ctx := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;

    // 3) 执行 TLS 握手（per-connection 设置 SNI/hostname）
    Connector := TSSLConnector.FromContext(Ctx).WithTimeout(15000);
    TLS := Connector.ConnectSocket(THandle(Sock), Host);

    // 4) 发送 HTTP 请求并读取响应
    Request := 'GET ' + Path + ' HTTP/1.1'#13#10 +
               'Host: ' + Host + #13#10 +
               'User-Agent: fafafa.ssl-example/1.0'#13#10 +
               'Connection: close'#13#10 +
               #13#10;

    if Length(Request) > 0 then
      TLS.WriteBuffer(Request[1], Length(Request));

    Response := ReadAll(TLS);

    WriteLn('✓ 请求完成');
    WriteLn('响应长度: ', Length(Response), ' 字节');
    PrintPreview(Response);
  finally
    if TLS <> nil then
      TLS.Free;
    CloseSocket(Sock);
    CleanupNetwork;
  end;
end;

var
  URL: string;
begin
  PrintHeader;
  if ParamCount >= 1 then
    URL := ParamStr(1)
  else
    URL := DEFAULT_URL;

  try
    ExecuteRequest(URL);
  except
    on E: Exception do
    begin
      WriteLn('✗ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
