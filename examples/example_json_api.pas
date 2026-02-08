program example_json_api;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  JSON API 示例（当前可用实现）

  说明：
  - 历史单元 fafafa.ssl.http.json 已移除
  - 本示例使用 TSSLContextBuilder + TSSLConnector + TSSLStream 发起 HTTPS 请求
  - 使用 fpjson/jsonparser 解析 JSON
}

uses
  SysUtils, Classes, StrUtils,
  fpjson, jsonparser,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  BUFFER_SIZE = 16384;

type
  TSimpleHTTPResult = record
    StatusCode: Integer;
    Body: RawByteString;
    ErrorMessage: string;
  end;

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

function ParseStatusCode(const AStatusLine: string): Integer;
var
  LSpace1, LSpace2: Integer;
  LCodeStr: string;
begin
  Result := 0;
  LSpace1 := Pos(' ', AStatusLine);
  if LSpace1 = 0 then
    Exit;

  LSpace2 := PosEx(' ', AStatusLine, LSpace1 + 1);
  if LSpace2 = 0 then
    LSpace2 := Length(AStatusLine) + 1;

  LCodeStr := Trim(Copy(AStatusLine, LSpace1 + 1, LSpace2 - LSpace1 - 1));
  Result := StrToIntDef(LCodeStr, 0);
end;

function SplitHTTPResponse(const ARaw: RawByteString; out AStatusLine: string;
  out ABody: RawByteString): Boolean;
var
  LHeaderEnd, LDelimLen, LLineEnd: Integer;
begin
  Result := False;
  AStatusLine := '';
  ABody := '';

  LHeaderEnd := Pos(#13#10#13#10, ARaw);
  if LHeaderEnd > 0 then
    LDelimLen := 4
  else
  begin
    LHeaderEnd := Pos(#10#10, ARaw);
    if LHeaderEnd > 0 then
      LDelimLen := 2
    else
      Exit;
  end;

  ABody := Copy(ARaw, LHeaderEnd + LDelimLen, Length(ARaw));

  LLineEnd := Pos(#13#10, ARaw);
  if LLineEnd = 0 then
    LLineEnd := Pos(#10, ARaw);

  if LLineEnd > 0 then
    AStatusLine := Trim(Copy(string(ARaw), 1, LLineEnd - 1));

  Result := True;
end;

function DoJSONRequest(const AMethod, AURL: string; const ABody: RawByteString;
  const AContentType: string): TSimpleHTTPResult;
var
  LHost, LPath, LNetErr: string;
  LPort: Word;
  LSocket: TSocketHandle;
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LTLS: TSSLStream;
  LRequest: RawByteString;
  LRawResp: RawByteString;
  LStatusLine: string;
begin
  Result.StatusCode := 0;
  Result.Body := '';
  Result.ErrorMessage := '';

  if not ParseURL(AURL, LHost, LPath, LPort) then
  begin
    Result.ErrorMessage := '无法解析 URL: ' + AURL;
    Exit;
  end;

  if not InitNetwork(LNetErr) then
  begin
    Result.ErrorMessage := '网络初始化失败: ' + LNetErr;
    Exit;
  end;

  LSocket := INVALID_SOCKET;
  LTLS := nil;
  try
    LSocket := ConnectTCP(LHost, LPort);

    LContext := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;

    LConnector := TSSLConnector.FromContext(LContext).WithTimeout(20000);
    LTLS := LConnector.ConnectSocket(THandle(LSocket), LHost);

    LRequest := AMethod + ' ' + LPath + ' HTTP/1.1'#13#10 +
                'Host: ' + LHost + #13#10 +
                'User-Agent: fafafa.ssl-example_json_api/1.0'#13#10 +
                'Accept: application/json'#13#10;

    if ABody <> '' then
    begin
      if AContentType <> '' then
        LRequest := LRequest + 'Content-Type: ' + AContentType + #13#10;
      LRequest := LRequest +
                  'Content-Length: ' + IntToStr(Length(ABody)) + #13#10 +
                  'Connection: close'#13#10 +
                  #13#10 +
                  ABody;
    end
    else
      LRequest := LRequest + 'Connection: close'#13#10 + #13#10;

    if Length(LRequest) > 0 then
      LTLS.WriteBuffer(LRequest[1], Length(LRequest));

    LRawResp := ReadAll(LTLS);

    if SplitHTTPResponse(LRawResp, LStatusLine, Result.Body) then
      Result.StatusCode := ParseStatusCode(LStatusLine)
    else
    begin
      Result.StatusCode := 0;
      Result.Body := LRawResp;
    end;
  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;

  if LTLS <> nil then
    LTLS.Free;
  CloseSocket(LSocket);
  CleanupNetwork;
end;

procedure Example1_GetJSON;
var
  LResp: TSimpleHTTPResult;
  LJSON: TJSONData;
  LObj: TJSONObject;
begin
  WriteLn('=== Example 1: GET JSON ===');
  WriteLn('Fetching JSON from JSONPlaceholder API...');
  WriteLn;

  LResp := DoJSONRequest('GET', 'https://jsonplaceholder.typicode.com/posts/1', '', '');
  if LResp.ErrorMessage <> '' then
  begin
    WriteLn('✗ Error: ', LResp.ErrorMessage);
    WriteLn;
    Exit;
  end;

  if (LResp.StatusCode < 200) or (LResp.StatusCode >= 300) then
  begin
    WriteLn('✗ HTTP status: ', LResp.StatusCode);
    WriteLn;
    Exit;
  end;

  LJSON := GetJSON(string(LResp.Body));
  try
    if LJSON.JSONType <> jtObject then
      raise Exception.Create('Response is not a JSON object');

    LObj := TJSONObject(LJSON);
    WriteLn('Response:');
    WriteLn('  User ID: ', LObj.Get('userId', 0));
    WriteLn('  ID: ', LObj.Get('id', 0));
    WriteLn('  Title: ', LObj.Get('title', ''));
    WriteLn('  Body: ', Copy(LObj.Get('body', ''), 1, 50), '...');
    WriteLn('✓ Success');
  finally
    LJSON.Free;
  end;
  WriteLn;
end;

procedure Example2_PostJSON;
var
  LRequestObj: TJSONObject;
  LResp: TSimpleHTTPResult;
  LJSON: TJSONData;
  LObj: TJSONObject;
begin
  WriteLn('=== Example 2: POST JSON ===');
  WriteLn('Creating a new post...');
  WriteLn;

  LRequestObj := TJSONObject.Create;
  try
    LRequestObj.Add('title', 'Test Post from fafafa.ssl');
    LRequestObj.Add('body', 'This is a test post created using JSON + TLS stream');
    LRequestObj.Add('userId', 1);

    WriteLn('Sending:');
    WriteLn(LRequestObj.AsJSON);
    WriteLn;

    LResp := DoJSONRequest(
      'POST',
      'https://jsonplaceholder.typicode.com/posts',
      RawByteString(LRequestObj.AsJSON),
      'application/json'
    );

    if LResp.ErrorMessage <> '' then
      raise Exception.Create(LResp.ErrorMessage);

    LJSON := GetJSON(string(LResp.Body));
    try
      if LJSON.JSONType <> jtObject then
        raise Exception.Create('Response is not a JSON object');

      LObj := TJSONObject(LJSON);
      WriteLn('Response:');
      WriteLn('  HTTP Status: ', LResp.StatusCode);
      WriteLn('  ID: ', LObj.Get('id', 0));
      WriteLn('  Title: ', LObj.Get('title', ''));
      WriteLn('✓ Success');
    finally
      LJSON.Free;
    end;
  except
    on E: Exception do
      WriteLn('✗ Error: ', E.Message);
  end;

  LRequestObj.Free;
  WriteLn;
end;

procedure Example3_GetArray;
var
  LResp: TSimpleHTTPResult;
  LJSON: TJSONData;
  LArray: TJSONArray;
  I: Integer;
  LObj: TJSONObject;
begin
  WriteLn('=== Example 3: GET JSON Array ===');
  WriteLn('Fetching list of posts...');
  WriteLn;

  LResp := DoJSONRequest('GET', 'https://jsonplaceholder.typicode.com/posts?_limit=5', '', '');
  if LResp.ErrorMessage <> '' then
  begin
    WriteLn('✗ Error: ', LResp.ErrorMessage);
    WriteLn;
    Exit;
  end;

  if (LResp.StatusCode < 200) or (LResp.StatusCode >= 300) then
  begin
    WriteLn('✗ HTTP status: ', LResp.StatusCode);
    WriteLn;
    Exit;
  end;

  LJSON := GetJSON(string(LResp.Body));
  try
    if LJSON.JSONType <> jtArray then
      raise Exception.Create('Response is not a JSON array');

    LArray := TJSONArray(LJSON);
    WriteLn('Received ', LArray.Count, ' posts:');
    for I := 0 to LArray.Count - 1 do
    begin
      if LArray.Items[I].JSONType = jtObject then
      begin
        LObj := TJSONObject(LArray.Items[I]);
        WriteLn('  [', LObj.Get('id', 0), '] ', LObj.Get('title', ''));
      end;
    end;
    WriteLn('✓ Success');
  finally
    LJSON.Free;
  end;
  WriteLn;
end;

procedure Example4_ErrorHandling;
var
  LResp: TSimpleHTTPResult;
begin
  WriteLn('=== Example 4: Error Handling ===');
  WriteLn('Trying invalid URL...');
  WriteLn;

  LResp := DoJSONRequest('GET', 'https://jsonplaceholder.typicode.com/invalid', '', '');

  if LResp.ErrorMessage <> '' then
  begin
    WriteLn('✓ Caught exception:');
    WriteLn('  ', LResp.ErrorMessage);
  end
  else if (LResp.StatusCode >= 400) then
  begin
    WriteLn('✓ Caught HTTP error correctly:');
    WriteLn('  Status Code: ', LResp.StatusCode);
  end
  else
    WriteLn('✗ Expected non-success response, got status: ', LResp.StatusCode);

  WriteLn;
end;

begin
  WriteLn('==========================================');
  WriteLn('  JSON API Client Examples');
  WriteLn('==========================================');
  WriteLn;

  try
    Example1_GetJSON;
    Example2_PostJSON;
    Example3_GetArray;
    Example4_ErrorHandling;

    WriteLn('==========================================');
    WriteLn('✓ All examples completed');
    WriteLn('==========================================');
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
