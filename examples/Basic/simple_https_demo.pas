program simple_https_demo;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 演示最小依赖的 HTTPS 请求
  - 不再依赖已移除的 fafafa.ssl.http.simple
  - 使用 Rust 风格门面：TSSLConnector + TSSLStream
  - SNI/hostname 使用 per-connection 的 ISSLClientConnection.SetServerName
}

uses
  SysUtils, Classes, StrUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  BUFFER_SIZE = 16384;

type
  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: TStringList;
    Body: RawByteString;
    Success: Boolean;
    ErrorMessage: string;
  end;

procedure FreeHTTPResponse(var AResp: THTTPResponse);
begin
  if AResp.Headers <> nil then
    AResp.Headers.Free;
  AResp.Headers := nil;
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

function ReadAll(AStream: TStream): RawByteString;
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

function FindHeaderEnd(const S: RawByteString; out ADelimLen: Integer): Integer;
begin
  Result := Pos(#13#10#13#10, S);
  if Result > 0 then
  begin
    ADelimLen := 4;
    Exit;
  end;

  Result := Pos(#10#10, S);
  if Result > 0 then
    ADelimLen := 2
  else
    ADelimLen := 0;
end;

procedure SplitHTTPResponse(const ARaw: RawByteString; out AStatusLine: string;
  AHeaders: TStringList; out ABody: RawByteString);
var
  LHeaderEnd, LDelimLen: Integer;
  LHeaderBlock: RawByteString;
  LText: string;
  LLines: TStringList;
  I: Integer;
begin
  AStatusLine := '';
  if AHeaders <> nil then
    AHeaders.Clear;
  ABody := '';

  LHeaderEnd := FindHeaderEnd(ARaw, LDelimLen);
  if (LHeaderEnd = 0) or (LDelimLen = 0) then
  begin
    ABody := ARaw;
    Exit;
  end;

  LHeaderBlock := Copy(ARaw, 1, LHeaderEnd - 1);
  ABody := Copy(ARaw, LHeaderEnd + LDelimLen, Length(ARaw));

  if AHeaders = nil then
    Exit;

  LText := string(LHeaderBlock);
  LText := StringReplace(LText, #13#10, #10, [rfReplaceAll]);
  LText := StringReplace(LText, #13, #10, [rfReplaceAll]);

  LLines := TStringList.Create;
  try
    LLines.Text := LText;
    if LLines.Count > 0 then
      AStatusLine := LLines[0];

    for I := 1 to LLines.Count - 1 do
      if Trim(LLines[I]) <> '' then
        AHeaders.Add(LLines[I]);
  finally
    LLines.Free;
  end;
end;

function ParseStatusLine(const ALine: string; out AStatusCode: Integer; out AStatusText: string): Boolean;
var
  P1, P2: Integer;
  CodeStr: string;
begin
  Result := False;
  AStatusCode := 0;
  AStatusText := '';

  P1 := Pos(' ', ALine);
  if P1 = 0 then
    Exit;

  P2 := PosEx(' ', ALine, P1 + 1);
  if P2 = 0 then
    P2 := Length(ALine) + 1;

  CodeStr := Trim(Copy(ALine, P1 + 1, P2 - P1 - 1));
  AStatusCode := StrToIntDef(CodeStr, 0);
  AStatusText := Trim(Copy(ALine, P2 + 1, Length(ALine)));

  Result := AStatusCode > 0;
end;

function HasChunkedTransferEncoding(AHeaders: TStringList): Boolean;
var
  I: Integer;
  L: string;
begin
  Result := False;
  if AHeaders = nil then
    Exit;

  for I := 0 to AHeaders.Count - 1 do
  begin
    L := LowerCase(Trim(AHeaders[I]));
    if Pos('transfer-encoding:', L) = 1 then
    begin
      if Pos('chunked', L) > 0 then
        Exit(True);
    end;
  end;
end;

function DecodeChunkedBody(const ABody: RawByteString): RawByteString;
var
  P, LineEnd, SemiPos: Integer;
  SizeLine: string;
  ChunkSize: Integer;
  DataStart: Integer;
begin
  Result := '';
  P := 1;

  while True do
  begin
    LineEnd := PosEx(#13#10, ABody, P);
    if LineEnd = 0 then
      Break;

    SizeLine := Trim(string(Copy(ABody, P, LineEnd - P)));
    SemiPos := Pos(';', SizeLine);
    if SemiPos > 0 then
      SizeLine := Copy(SizeLine, 1, SemiPos - 1);

    if SizeLine = '' then
      Break;

    ChunkSize := StrToIntDef('$' + SizeLine, -1);
    if ChunkSize < 0 then
      Break;

    P := LineEnd + 2; // skip CRLF

    if ChunkSize = 0 then
      Break;

    DataStart := P;
    if DataStart + ChunkSize - 1 > Length(ABody) then
      Break;

    Result := Result + Copy(ABody, DataStart, ChunkSize);
    P := DataStart + ChunkSize;

    // skip trailing CRLF after chunk data
    if Copy(ABody, P, 2) = #13#10 then
      Inc(P, 2)
    else if (P <= Length(ABody)) and (ABody[P] = #10) then
      Inc(P);
  end;
end;

function DoRequest(const AMethod, AURL: string; const ABody: RawByteString;
  const AContentType: string; const AExtraHeaders: array of string;
  ATimeoutMs: Integer; AVerifyPeer: Boolean): THTTPResponse;
var
  Host, Path: string;
  Port: Word;
  Sock: TSocketHandle;
  Ctx: ISSLContext;
  Connector: TSSLConnector;
  TLS: TSSLStream;
  Request: RawByteString;
  RawResp: RawByteString;
  StatusLine: string;
  I: Integer;
begin
  // Avoid FillChar on records with managed fields (strings/RawByteString).
  Result.Headers := TStringList.Create;
  Result.StatusCode := 0;
  Result.StatusText := '';
  Result.Body := '';
  Result.Success := False;
  Result.ErrorMessage := '';

  if not ParseURL(AURL, Host, Path, Port) then
  begin
    Result.ErrorMessage := 'URL 解析失败: ' + AURL;
    Exit;
  end;

  Sock := INVALID_SOCKET;
  TLS := nil;
  try
    try
      Sock := ConnectTCP(Host, Port);

      if AVerifyPeer then
        Ctx := TSSLContextBuilder.Create
          .WithTLS12And13
          .WithVerifyPeer
          .WithSystemRoots
          .BuildClient
      else
        Ctx := TSSLContextBuilder.Create
          .WithTLS12And13
          .WithVerifyNone
          .BuildClient;

      Connector := TSSLConnector.FromContext(Ctx).WithTimeout(ATimeoutMs);
      TLS := Connector.ConnectSocket(THandle(Sock), Host);

      Request := AMethod + ' ' + Path + ' HTTP/1.1'#13#10 +
                 'Host: ' + Host + #13#10 +
                 'User-Agent: simple_https_demo/1.0' + #13#10 +
                 'Accept: */*' + #13#10;

      for I := Low(AExtraHeaders) to High(AExtraHeaders) do
        if Trim(AExtraHeaders[I]) <> '' then
          Request := Request + Trim(AExtraHeaders[I]) + #13#10;

      if ABody <> '' then
      begin
        if AContentType <> '' then
          Request := Request + 'Content-Type: ' + AContentType + #13#10;
        Request := Request +
                   'Content-Length: ' + IntToStr(Length(ABody)) + #13#10 +
                   'Connection: close' + #13#10 +
                   #13#10 +
                   ABody;
      end
      else
        Request := Request + 'Connection: close' + #13#10 + #13#10;

      if Length(Request) > 0 then
        TLS.WriteBuffer(Request[1], Length(Request));

      RawResp := ReadAll(TLS);

      SplitHTTPResponse(RawResp, StatusLine, Result.Headers, Result.Body);
      if ParseStatusLine(StatusLine, Result.StatusCode, Result.StatusText) then
        Result.Success := (Result.StatusCode >= 200) and (Result.StatusCode < 300)
      else
        Result.Success := Length(Result.Body) > 0;

      if HasChunkedTransferEncoding(Result.Headers) then
        Result.Body := DecodeChunkedBody(Result.Body);
    except
      on E: Exception do
      begin
        Result.Success := False;
        Result.ErrorMessage := E.Message;
      end;
    end;
  finally
    if TLS <> nil then
      TLS.Free;
    CloseSocket(Sock);
  end;
end;

procedure PrintPreview(const ABody: RawByteString; AMaxLen: Integer);
var
  S: string;
begin
  S := string(ABody);
  if Length(S) > AMaxLen then
    S := Copy(S, 1, AMaxLen) + '... (截断)';
  WriteLn(S);
end;

procedure Demo1_SimpleGet;
var
  R: THTTPResponse;
begin
  WriteLn('=== 示例1: 简单GET请求 ===');
  WriteLn;

  R := DoRequest('GET', 'https://www.google.com', '', '', [], 15000, True);
  try
    if R.Success then
    begin
      WriteLn('请求成功！状态码: ', R.StatusCode, ' ', R.StatusText);
      WriteLn('响应体前 100 字符:');
      PrintPreview(R.Body, 100);
    end
    else
    begin
      if R.StatusCode > 0 then
        WriteLn('请求失败，状态码: ', R.StatusCode, ' ', R.StatusText);
      if R.ErrorMessage <> '' then
        WriteLn('错误: ', R.ErrorMessage);
    end;
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Demo2_SimplePost;
var
  R: THTTPResponse;
  JSON: RawByteString;
begin
  WriteLn('=== 示例2: 简单POST请求 ===');
  WriteLn;

  JSON := '{"name":"test","value":"hello"}';
  R := DoRequest('POST', 'https://httpbin.org/post', JSON, 'application/json', [], 15000, True);
  try
    if R.Success then
    begin
      WriteLn('请求成功！状态码: ', R.StatusCode, ' ', R.StatusText);
      WriteLn('响应体前 200 字符:');
      PrintPreview(R.Body, 200);
    end
    else
    begin
      if R.StatusCode > 0 then
        WriteLn('请求失败，状态码: ', R.StatusCode, ' ', R.StatusText);
      if R.ErrorMessage <> '' then
        WriteLn('错误: ', R.ErrorMessage);
    end;
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Demo3_AdvancedGet;
var
  R: THTTPResponse;
  I: Integer;
begin
  WriteLn('=== 示例3: 高级GET请求（自定义请求头/超时）===');
  WriteLn;

  R := DoRequest(
    'GET',
    'https://httpbin.org/get',
    '',
    '',
    ['Accept: application/json', 'X-Custom-Header: CustomValue'],
    10000,
    True
  );

  try
    WriteLn('状态码: ', R.StatusCode, ' ', R.StatusText);
    if R.Headers <> nil then
    begin
      WriteLn('响应头(前 10 行):');
      for I := 0 to R.Headers.Count - 1 do
      begin
        if I >= 10 then Break;
        WriteLn('  ', R.Headers[I]);
      end;
      WriteLn;
    end;

    if R.Success then
    begin
      WriteLn('响应体前 400 字符:');
      PrintPreview(R.Body, 400);
    end
    else
    begin
      if R.ErrorMessage <> '' then
        WriteLn('错误: ', R.ErrorMessage);
    end;
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Demo4_DownloadFile;
var
  R: THTTPResponse;
  FS: TFileStream;
begin
  WriteLn('=== 示例4: 下载文件(保存响应体到 robots.txt) ===');
  WriteLn;

  R := DoRequest('GET', 'https://www.google.com/robots.txt', '', '', [], 15000, True);
  try
    if not R.Success then
    begin
      WriteLn('下载失败: ', R.ErrorMessage);
      Exit;
    end;

    FS := TFileStream.Create('robots.txt', fmCreate);
    try
      if Length(R.Body) > 0 then
        FS.WriteBuffer(R.Body[1], Length(R.Body));
    finally
      FS.Free;
    end;

    WriteLn('文件下载成功: robots.txt (', Length(R.Body), ' bytes)');
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure ShowComparison;
begin
  WriteLn('=== 代码量对比（示意） ===');
  WriteLn;
  WriteLn('传统方式（更底层，需要自己管理 socket + SSL connection）：');
  WriteLn('  Sock := ConnectTCP(Host, 443);');
  WriteLn('  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslAutoDetect);');
  WriteLn('  Conn := Ctx.CreateConnection(THandle(Sock));');
  WriteLn('  (Conn as ISSLClientConnection).SetServerName(Host);');
  WriteLn('  Conn.Connect;');
  WriteLn('  Conn.Write(...); Conn.Read(...);');
  WriteLn;
  WriteLn('Rust 风格门面（更少样板代码）：');
  WriteLn('  TLS := TSSLConnector.FromContext(Ctx).ConnectSocket(THandle(Sock), Host);');
  WriteLn('  TLS.WriteBuffer(...); TLS.Read(...);');
  WriteLn;
end;

var
  NetErr: string;
begin
  WriteLn('fafafa.ssl - 简化 HTTPS 客户端演示');
  WriteLn('==================================');
  WriteLn;

  if not InitNetwork(NetErr) then
  begin
    WriteLn('网络初始化失败: ', NetErr);
    Halt(1);
  end;

  try
    ShowComparison;

    Demo1_SimpleGet;
    Demo2_SimplePost;
    Demo3_AdvancedGet;
    Demo4_DownloadFile;

    WriteLn('演示完成！');
    WriteLn;
    WriteLn('提示:');
    WriteLn('  - 生产环境务必开启 VerifyPeer，并加载系统根证书（WithSystemRoots）');
    WriteLn('  - SNI/hostname 建议使用 per-connection 的 ISSLClientConnection.SetServerName');
    WriteLn;
  finally
    CleanupNetwork;
  end;
end.

