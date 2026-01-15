program example_https_api;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 实用 HTTPS API 示例

  - 不再依赖历史的 fafafa.ssl.http.simple（已移除）
  - 直接用 TLS 门面 (TSSLConnector + TSSLStream) 发送原始 HTTP/1.1 请求
  - 不依赖 JSON 库，展示原始响应与最小解析
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
    TLSVersion: string;
    CipherSuite: string;
    VerifyResult: Integer;
    VerifyResultString: string;
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
  FillChar(Result, SizeOf(Result), 0);
  Result.Headers := TStringList.Create;
  Result.StatusCode := 0;
  Result.StatusText := '';
  Result.Body := '';
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.TLSVersion := '';
  Result.CipherSuite := '';
  Result.VerifyResult := 0;
  Result.VerifyResultString := '';

  if not ParseURL(AURL, Host, Path, Port) then
  begin
    Result.ErrorMessage := 'URL 解析失败: ' + AURL;
    Exit;
  end;

  Sock := INVALID_SOCKET;
  TLS := nil;
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

    // collect TLS info
    Result.TLSVersion := ProtocolVersionToString(TLS.Connection.GetProtocolVersion);
    Result.CipherSuite := TLS.Connection.GetCipherName;
    Result.VerifyResult := TLS.Connection.GetVerifyResult;
    Result.VerifyResultString := TLS.Connection.GetVerifyResultString;

    Request := AMethod + ' ' + Path + ' HTTP/1.1'#13#10 +
               'Host: ' + Host + #13#10 +
               'User-Agent: fafafa.ssl-example_https_api/1.0' + #13#10 +
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

procedure Example1_SimpleGET;
var
  R: THTTPResponse;
begin
  WriteLn('=== Example 1: Simple GET Request ===');
  WriteLn('Fetching from example.com...');
  WriteLn;

  R := DoRequest('GET', 'https://www.example.com/', '', '', [], 15000, True);
  try
    WriteLn('TLS: ', R.TLSVersion, ' / ', R.CipherSuite);
    WriteLn('Status: ', R.StatusCode, ' ', R.StatusText);
    WriteLn('Response length: ', Length(R.Body), ' bytes');
    WriteLn('First 200 characters:');
    PrintPreview(R.Body, 200);
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Example2_APIWithHeaders;
var
  R: THTTPResponse;
  I: Integer;
begin
  WriteLn('=== Example 2: API Call with Custom Headers ===');
  WriteLn('Calling JSONPlaceholder API...');
  WriteLn;

  R := DoRequest(
    'GET',
    'https://jsonplaceholder.typicode.com/posts/1',
    '',
    '',
    ['Accept: application/json', 'User-Agent: fafafa.ssl Example/1.0'],
    15000,
    True
  );

  try
    WriteLn('Status: ', R.StatusCode, ' ', R.StatusText);
    WriteLn('Success: ', BoolToStr(R.Success, True));
    WriteLn('TLS: ', R.TLSVersion, ' / ', R.CipherSuite);
    WriteLn;

    WriteLn('Response Headers (first 10):');
    for I := 0 to R.Headers.Count - 1 do
    begin
      if I >= 10 then Break;
      WriteLn('  ', R.Headers[I]);
    end;
    WriteLn;

    WriteLn('Response Body (first 500 chars):');
    PrintPreview(R.Body, 500);
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Example3_POSTData;
var
  R: THTTPResponse;
  Data: RawByteString;
begin
  WriteLn('=== Example 3: POST Data ===');
  WriteLn('Sending JSON data to API...');
  WriteLn;

  Data := '{"title":"Test Post","body":"This is a test","userId":1}';
  R := DoRequest(
    'POST',
    'https://jsonplaceholder.typicode.com/posts',
    Data,
    'application/json',
    ['Accept: application/json'],
    15000,
    True
  );

  try
    WriteLn('Status: ', R.StatusCode, ' ', R.StatusText);
    WriteLn('Success: ', BoolToStr(R.Success, True));
    WriteLn('Response Body (first 500 chars):');
    PrintPreview(R.Body, 500);
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Example4_DownloadFile;
var
  R: THTTPResponse;
  FilePath: string;
  FS: TFileStream;
begin
  WriteLn('=== Example 4: Download File ===');
  FilePath := 'example_download.html';
  WriteLn('Downloading to: ', FilePath);
  WriteLn;

  R := DoRequest('GET', 'https://www.example.com/', '', '', [], 15000, True);
  try
    if not R.Success then
    begin
      WriteLn('✗ Download failed: ', R.ErrorMessage);
      Exit;
    end;

    FS := TFileStream.Create(FilePath, fmCreate);
    try
      if Length(R.Body) > 0 then
        FS.WriteBuffer(R.Body[1], Length(R.Body));
    finally
      FS.Free;
    end;

    WriteLn('✓ File downloaded successfully');
    WriteLn('File size: ', Length(R.Body), ' bytes');
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Example5_ErrorHandling;
var
  R: THTTPResponse;
begin
  WriteLn('=== Example 5: Error Handling ===');
  WriteLn('Trying a 404 URL...');
  WriteLn;

  R := DoRequest(
    'GET',
    'https://jsonplaceholder.typicode.com/invalid-endpoint',
    '',
    '',
    [],
    15000,
    True
  );

  try
    WriteLn('Status: ', R.StatusCode, ' ', R.StatusText);
    WriteLn('Success (2xx): ', BoolToStr(R.Success, True));

    if R.Success then
      WriteLn('✗ Should have reported failure')
    else
      WriteLn('✓ Correctly reported failure');

    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

procedure Example6_HTTPSVerification;
var
  R: THTTPResponse;
begin
  WriteLn('=== Example 6: HTTPS Certificate Verification ===');
  WriteLn('Testing with proper HTTPS site...');
  WriteLn;

  R := DoRequest('GET', 'https://www.google.com', '', '', [], 15000, True);
  try
    WriteLn('Status: ', R.StatusCode, ' ', R.StatusText);
    WriteLn('VerifyResult: ', R.VerifyResult);
    WriteLn('VerifyResultString: ', R.VerifyResultString);
    if R.VerifyResult = 0 then
      WriteLn('Certificate verification: ✓ Passed')
    else
      WriteLn('Certificate verification: ✗ Failed');
    WriteLn;
  finally
    FreeHTTPResponse(R);
  end;
end;

var
  Lib: ISSLLibrary;
  NetErr: string;
begin
  WriteLn('==========================================');
  WriteLn('  HTTPS API Client Examples');
  WriteLn('  Practical demonstrations');
  WriteLn('==========================================');
  WriteLn;

  if not InitNetwork(NetErr) then
  begin
    WriteLn('Network init failed: ', NetErr);
    Halt(1);
  end;

  try
    Lib := TSSLFactory.GetLibraryInstance(sslAutoDetect);
    WriteLn('Library: ', LibraryTypeToString(Lib.GetLibraryType));
    WriteLn('Version: ', Lib.GetVersionString);
    WriteLn;

    // Run all examples
    Example1_SimpleGET;
    Example2_APIWithHeaders;
    Example3_POSTData;
    Example4_DownloadFile;
    Example5_ErrorHandling;
    Example6_HTTPSVerification;

    WriteLn('==========================================');
    WriteLn('✓ All examples completed');
    WriteLn('==========================================');
    WriteLn;
  finally
    CleanupNetwork;
  end;

  WriteLn('Press Enter to exit...');
  ReadLn;
end.
