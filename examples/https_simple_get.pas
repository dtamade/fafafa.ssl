program https_simple_get;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  超简单 HTTPS GET 请求示例

  说明：
  - 使用当前推荐 API：TSSLContextBuilder + TSSLConnector + TSSLStream
  - 使用 fafafa.examples.tcp 处理跨平台 TCP 连接
}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  TARGET_URL = 'https://www.example.com/';
  PREVIEW_LIMIT = 500;
  BUFFER_SIZE = 16384;

function ParseURL(const AURL: string; out AHost, APath: string; out APort: Word): Boolean;
var
  LTemp, LHostPart: string;
  LSlashPos, LPortPos: Integer;
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

  LSlashPos := Pos('/', LTemp);
  if LSlashPos > 0 then
  begin
    LHostPart := Copy(LTemp, 1, LSlashPos - 1);
    APath := Copy(LTemp, LSlashPos, Length(LTemp));
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

function ReadAll(AStream: TStream): RawByteString;
var
  LBuffer: array[0..BUFFER_SIZE - 1] of Byte;
  LRead: Longint;
  LMem: TMemoryStream;
begin
  Result := '';
  LMem := TMemoryStream.Create;
  try
    repeat
      LRead := AStream.Read(LBuffer[0], SizeOf(LBuffer));
      if LRead > 0 then
        LMem.WriteBuffer(LBuffer[0], LRead);
    until LRead = 0;

    if LMem.Size > 0 then
    begin
      SetLength(Result, LMem.Size);
      LMem.Position := 0;
      LMem.ReadBuffer(Result[1], LMem.Size);
    end;
  finally
    LMem.Free;
  end;
end;

procedure ExecuteGET(const AURL: string);
var
  LHost, LPath, LNetErr: string;
  LPort: Word;
  LSocket: TSocketHandle;
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LTLS: TSSLStream;
  LRequest: RawByteString;
  LResponse: RawByteString;
  LPreview: string;
begin
  if not ParseURL(AURL, LHost, LPath, LPort) then
    raise Exception.Create('无法解析 URL: ' + AURL);

  if not InitNetwork(LNetErr) then
    raise Exception.Create('网络初始化失败: ' + LNetErr);

  LSocket := INVALID_SOCKET;
  LTLS := nil;
  try
    LSocket := ConnectTCP(LHost, LPort);

    LContext := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;

    LConnector := TSSLConnector.FromContext(LContext).WithTimeout(15000);
    LTLS := LConnector.ConnectSocket(THandle(LSocket), LHost);

    LRequest := 'GET ' + LPath + ' HTTP/1.1'#13#10 +
                'Host: ' + LHost + #13#10 +
                'User-Agent: fafafa.ssl-https_simple_get/1.0'#13#10 +
                'Connection: close'#13#10 +
                #13#10;

    if Length(LRequest) > 0 then
      LTLS.WriteBuffer(LRequest[1], Length(LRequest));

    LResponse := ReadAll(LTLS);

    WriteLn('✓ 请求完成');
    WriteLn('响应长度: ', Length(LResponse), ' 字节');
    WriteLn;

    if Length(LResponse) > PREVIEW_LIMIT then
      LPreview := Copy(string(LResponse), 1, PREVIEW_LIMIT) + '...'
    else
      LPreview := string(LResponse);

    WriteLn('响应预览（前 ', PREVIEW_LIMIT, ' 字符）:');
    WriteLn('------------------------------------------');
    WriteLn(LPreview);
    WriteLn('------------------------------------------');
  finally
    if LTLS <> nil then
      LTLS.Free;
    CloseSocket(LSocket);
    CleanupNetwork;
  end;
end;

var
  LURL: string;
begin
  WriteLn('==========================================');
  WriteLn('   fafafa.ssl - 超简单 HTTPS 示例');
  WriteLn('==========================================');
  WriteLn;

  if ParamCount >= 1 then
    LURL := ParamStr(1)
  else
    LURL := TARGET_URL;

  try
    ExecuteGET(LURL);
  except
    on E: Exception do
    begin
      WriteLn('✗ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
