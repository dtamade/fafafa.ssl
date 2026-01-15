program https_client_production;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ Production-Ready HTTPS Client Example

  Demonstrates recommended usage patterns in current fafafa.ssl:
  - TSSLContextBuilder for secure defaults + system roots
  - TSSLConnector/TSSLStream for ergonomic TLS over an existing TCP socket
  - per-connection SNI via ConnectSocket(..., serverName)

  NOTE: This is a minimal demo; real production code needs robust HTTP parsing,
  redirect handling, retries, streaming, etc.
}

uses
  SysUtils, Classes, StrUtils, DateUtils,
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

    if Copy(ABody, P, 2) = #13#10 then
      Inc(P, 2)
    else if (P <= Length(ABody)) and (ABody[P] = #10) then
      Inc(P);
  end;
end;

type
  THTTPSClient = class
  private
    FContext: ISSLContext;
    FTimeoutMs: Integer;
    FLastError: string;
  public
    constructor Create;
    function Get(const AURL: string; out AResp: THTTPResponse): Boolean;

    property TimeoutMs: Integer read FTimeoutMs write FTimeoutMs;
    property LastError: string read FLastError;
  end;

{ THTTPSClient }

constructor THTTPSClient.Create;
begin
  inherited Create;
  FTimeoutMs := 30000;
  FLastError := '';

  // Create a reusable client context with safe defaults
  FContext := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // Enable session cache for better performance
  if FContext <> nil then
  begin
    FContext.SetSessionCacheMode(True);
    FContext.SetSessionCacheSize(256);
    FContext.SetSessionTimeout(600);
  end;
end;

function THTTPSClient.Get(const AURL: string; out AResp: THTTPResponse): Boolean;
var
  Host, Path: string;
  Port: Word;
  Sock: TSocketHandle;
  TLS: TSSLStream;
  Connector: TSSLConnector;
  Request: RawByteString;
  RawResp: RawByteString;
  StatusLine: string;
begin
  FillChar(AResp, SizeOf(AResp), 0);
  AResp.Headers := TStringList.Create;
  AResp.StatusCode := 0;
  AResp.StatusText := '';
  AResp.Body := '';
  AResp.Success := False;
  AResp.ErrorMessage := '';

  Result := False;
  FLastError := '';

  if not ParseURL(AURL, Host, Path, Port) then
  begin
    FLastError := 'Invalid URL: ' + AURL;
    AResp.ErrorMessage := FLastError;
    Exit;
  end;

  Sock := INVALID_SOCKET;
  TLS := nil;
  try
    Sock := ConnectTCP(Host, Port);

    Connector := TSSLConnector.FromContext(FContext).WithTimeout(FTimeoutMs);
    TLS := Connector.ConnectSocket(THandle(Sock), Host);

    Request := 'GET ' + Path + ' HTTP/1.1'#13#10 +
               'Host: ' + Host + #13#10 +
               'User-Agent: fafafa.ssl-production-example/1.0' + #13#10 +
               'Accept: */*' + #13#10 +
               'Connection: close' + #13#10 +
               #13#10;

    if Length(Request) > 0 then
      TLS.WriteBuffer(Request[1], Length(Request));

    RawResp := ReadAll(TLS);

    SplitHTTPResponse(RawResp, StatusLine, AResp.Headers, AResp.Body);
    if ParseStatusLine(StatusLine, AResp.StatusCode, AResp.StatusText) then
      AResp.Success := (AResp.StatusCode >= 200) and (AResp.StatusCode < 300)
    else
      AResp.Success := Length(AResp.Body) > 0;

    if HasChunkedTransferEncoding(AResp.Headers) then
      AResp.Body := DecodeChunkedBody(AResp.Body);

    Result := AResp.Success;
    if not Result then
      FLastError := Format('HTTP %d %s', [AResp.StatusCode, AResp.StatusText]);
  except
    on E: Exception do
    begin
      FLastError := E.Message;
      AResp.ErrorMessage := E.Message;
      Result := False;
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
    S := Copy(S, 1, AMaxLen) + '... (truncated)';
  WriteLn(S);
end;

procedure RunProductionExample;
var
  Client: THTTPSClient;
  R: THTTPResponse;
  OK: Boolean;
  StartTime: TDateTime;
  DurationMs: Integer;
  NetErr: string;
begin
  WriteLn('================================================================');
  WriteLn('  Production-Ready HTTPS Client Example (updated API)');
  WriteLn('================================================================');
  WriteLn;

  if not InitNetwork(NetErr) then
    raise Exception.Create('Network init failed: ' + NetErr);

  try
    Client := THTTPSClient.Create;
    try
      // Example 1
      WriteLn('--- Test 1: GET https://www.example.com/ ---');
      StartTime := Now;
      OK := Client.Get('https://www.example.com/', R);
      DurationMs := MilliSecondsBetween(Now, StartTime);
      try
        if OK then
        begin
          WriteLn('✓ SUCCESS in ', DurationMs, ' ms, bytes=', Length(R.Body));
          WriteLn;
          WriteLn('Body preview (first 200 chars):');
          PrintPreview(R.Body, 200);
        end
        else
          WriteLn('✗ FAILED: ', Client.LastError);
      finally
        FreeHTTPResponse(R);
      end;

      WriteLn;
      WriteLn('--- Test 2: GET https://www.google.com/ ---');
      StartTime := Now;
      OK := Client.Get('https://www.google.com/', R);
      DurationMs := MilliSecondsBetween(Now, StartTime);
      try
        if OK then
          WriteLn('✓ SUCCESS in ', DurationMs, ' ms, bytes=', Length(R.Body))
        else
          WriteLn('✗ FAILED: ', Client.LastError);
      finally
        FreeHTTPResponse(R);
      end;

      WriteLn;
      WriteLn('Best Practices Demonstrated:');
      WriteLn('  ✓ VerifyPeer + System roots');
      WriteLn('  ✓ per-connection SNI via TSSLConnector.ConnectSocket');
      WriteLn('  ✓ Timeouts + resource cleanup');
      WriteLn;
    finally
      Client.Free;
    end;
  finally
    CleanupNetwork;
  end;
end;

begin
  try
    RunProductionExample;
  except
    on E: Exception do
    begin
      WriteLn('CRITICAL ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
