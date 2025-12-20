program winssl_health_checker;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ENDIF}
  SysUtils, Classes, DateUtils,
  fafafa.ssl.factory,
  fafafa.ssl.base;

type
  TURLComponents = record
    Protocol: string;
    Host: string;
    Port: Word;
    Path: string;
  end;

  THealthCheckResult = record
    URL: string;
    Success: Boolean;
    StatusCode: Integer;
    ResponseTime: Double;  // in seconds
    ErrorMessage: string;
  end;

{ Parse URL into components }
function ParseURL(const aURL: string; out aComponents: TURLComponents): Boolean;
var
  LRest, LHostPort: string;
  LPos: Integer;
begin
  Result := False;
  FillChar(aComponents, SizeOf(aComponents), 0);

  // Check protocol
  if Pos('https://', LowerCase(aURL)) <> 1 then
  begin
    WriteLn('Error: Only HTTPS URLs are supported');
    Exit;
  end;

  aComponents.Protocol := 'https';
  LRest := Copy(aURL, 9, Length(aURL)); // Skip "https://"

  // Find path separator
  LPos := Pos('/', LRest);
  if LPos > 0 then
  begin
    LHostPort := Copy(LRest, 1, LPos - 1);
    aComponents.Path := Copy(LRest, LPos, Length(LRest));
  end
  else
  begin
    LHostPort := LRest;
    aComponents.Path := '/';
  end;

  // Parse host:port
  LPos := Pos(':', LHostPort);
  if LPos > 0 then
  begin
    aComponents.Host := Copy(LHostPort, 1, LPos - 1);
    try
      aComponents.Port := StrToInt(Copy(LHostPort, LPos + 1, Length(LHostPort)));
    except
      Exit;
    end;
  end
  else
  begin
    aComponents.Host := LHostPort;
    aComponents.Port := 443; // Default HTTPS port
  end;

  Result := (aComponents.Host <> '');
end;

{ Check health of a single endpoint }
function CheckHealth(const aURL: string): THealthCheckResult;
var
  LComponents: TURLComponents;
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
  LResponse: string;
  LWSAData: TWSAData;
  LStartTime, LEndTime: TDateTime;
  LPos: Integer;
  LStatusLine: string;
  LTimeout: DWORD;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.URL := aURL;
  Result.Success := False;
  Result.StatusCode := 0;
  Result.ResponseTime := 0;

  // Parse URL
  if not ParseURL(aURL, LComponents) then
  begin
    Result.ErrorMessage := 'Invalid URL';
    Exit;
  end;

  // Initialize Winsock
  if WSAStartup(MAKEWORD(2, 2), LWSAData) <> 0 then
  begin
    Result.ErrorMessage := 'Winsock initialization failed';
    Exit;
  end;

  try
    // Create WinSSL library
    LLib := CreateSSLLibrary(sslWinSSL);
    if not LLib.Initialize then
    begin
      Result.ErrorMessage := 'SSL library initialization failed';
      Exit;
    end;

    // Create client context
    LCtx := LLib.CreateContext(sslCtxClient);
    LCtx.SetServerName(LComponents.Host);
    LCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

    // Create TCP socket
    LSocket := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if LSocket = INVALID_SOCKET then
    begin
      Result.ErrorMessage := 'Socket creation failed';
      Exit;
    end;

    try
      // Set socket timeout (5 seconds)
      LTimeout := 5000;
      setsockopt(LSocket, SOL_SOCKET, SO_RCVTIMEO, @LTimeout, SizeOf(LTimeout));
      setsockopt(LSocket, SOL_SOCKET, SO_SNDTIMEO, @LTimeout, SizeOf(LTimeout));

      // Resolve hostname
      LHostEnt := gethostbyname(PAnsiChar(AnsiString(LComponents.Host)));
      if LHostEnt = nil then
      begin
        Result.ErrorMessage := 'DNS resolution failed';
        Exit;
      end;

      // Connect to server
      FillChar(LAddr, SizeOf(LAddr), 0);
      LAddr.sin_family := AF_INET;
      LAddr.sin_port := htons(LComponents.Port);
      LAddr.sin_addr := PInAddr(LHostEnt^.h_addr_list^)^;

      LStartTime := Now;

      if WinSock2.connect(LSocket, LAddr, SizeOf(LAddr)) = SOCKET_ERROR then
      begin
        Result.ErrorMessage := 'Connection failed';
        Exit;
      end;

      // Create SSL connection
      LConn := LCtx.CreateConnection(LSocket);

      if not LConn.Connect then
      begin
        Result.ErrorMessage := 'TLS handshake failed';
        Exit;
      end;

      // Send HTTP GET request
      LRequest := Format('GET %s HTTP/1.1'#13#10 +
                        'Host: %s'#13#10 +
                        'User-Agent: WinSSL-HealthChecker/1.0'#13#10 +
                        'Connection: close'#13#10 +
                        #13#10, [LComponents.Path, LComponents.Host]);

      if not LConn.WriteString(LRequest) then
      begin
        Result.ErrorMessage := 'Failed to send request';
        Exit;
      end;

      // Receive response (only first chunk to get status)
      LResponse := '';
      LBytesRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
      if LBytesRead > 0 then
      begin
        SetLength(LResponse, LBytesRead);
        Move(LBuffer[0], LResponse[1], LBytesRead);
      end;

      LEndTime := Now;
      Result.ResponseTime := (LEndTime - LStartTime) * 86400; // Convert to seconds

      // Parse status code
      LPos := Pos(#13#10, LResponse);
      if LPos > 0 then
      begin
        LStatusLine := Copy(LResponse, 1, LPos - 1);

        // Extract status code (HTTP/1.1 200 OK)
        if Pos('HTTP/', LStatusLine) = 1 then
        begin
          Delete(LStatusLine, 1, 9); // Remove "HTTP/1.1 "
          LPos := Pos(' ', LStatusLine);
          if LPos > 0 then
            LStatusLine := Copy(LStatusLine, 1, LPos - 1);

          try
            Result.StatusCode := StrToInt(Trim(LStatusLine));
            Result.Success := (Result.StatusCode >= 200) and (Result.StatusCode < 300);

            if not Result.Success then
              Result.ErrorMessage := 'HTTP ' + IntToStr(Result.StatusCode);
          except
            Result.ErrorMessage := 'Invalid status code';
          end;
        end
        else
          Result.ErrorMessage := 'Invalid HTTP response';
      end
      else
        Result.ErrorMessage := 'No response received';

      // Close SSL connection
      LConn.Shutdown;

    finally
      closesocket(LSocket);
    end;

  finally
    WSACleanup;
  end;
end;

{ Display health check result with color }
procedure DisplayResult(const aResult: THealthCheckResult);
const
  {$IFDEF WINDOWS}
  GREEN = FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  RED = FOREGROUND_RED or FOREGROUND_INTENSITY;
  YELLOW = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  {$ENDIF}
var
  {$IFDEF WINDOWS}
  LConsoleHandle: THandle;
  LOriginalAttrs: WORD;
  LConsoleInfo: CONSOLE_SCREEN_BUFFER_INFO;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  LConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(LConsoleHandle, LConsoleInfo);
  LOriginalAttrs := LConsoleInfo.wAttributes;
  {$ENDIF}

  Write('  ');

  // Status indicator
  if aResult.Success then
  begin
    {$IFDEF WINDOWS}
    SetConsoleTextAttribute(LConsoleHandle, GREEN);
    {$ENDIF}
    Write('[  OK  ]');
  end
  else
  begin
    {$IFDEF WINDOWS}
    SetConsoleTextAttribute(LConsoleHandle, RED);
    {$ENDIF}
    Write('[ FAIL ]');
  end;

  {$IFDEF WINDOWS}
  SetConsoleTextAttribute(LConsoleHandle, LOriginalAttrs);
  {$ENDIF}

  Write(' ');

  // URL
  Write(aResult.URL);

  // Response time
  if aResult.ResponseTime > 0 then
  begin
    if aResult.ResponseTime < 1.0 then
      Write(' (', Format('%.0f', [aResult.ResponseTime * 1000]), ' ms)')
    else
      Write(' (', Format('%.2f', [aResult.ResponseTime]), ' s)');
  end;

  // Status code or error
  if aResult.Success then
    WriteLn(' - HTTP ', aResult.StatusCode)
  else if aResult.ErrorMessage <> '' then
    WriteLn(' - ', aResult.ErrorMessage)
  else
    WriteLn;
end;

{ Load URLs from file }
function LoadURLsFromFile(const aFileName: string; aURLs: TStringList): Boolean;
var
  LFile: TextFile;
  LLine: string;
begin
  Result := False;

  if not FileExists(aFileName) then
  begin
    WriteLn('Error: File not found: ', aFileName);
    Exit;
  end;

  try
    AssignFile(LFile, aFileName);
    Reset(LFile);
    try
      while not Eof(LFile) do
      begin
        ReadLn(LFile, LLine);
        LLine := Trim(LLine);

        // Skip empty lines and comments
        if (LLine <> '') and (LLine[1] <> '#') then
          aURLs.Add(LLine);
      end;

      Result := True;
    finally
      CloseFile(LFile);
    end;
  except
    on E: Exception do
      WriteLn('Error reading file: ', E.Message);
  end;
end;

{ Display usage information }
procedure ShowUsage;
begin
  WriteLn('WinSSL Health Checker v1.0');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  winssl_health_checker <URL> [URL2] [URL3] ...');
  WriteLn('  winssl_health_checker -f <file>');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -f <file>    Load URLs from file (one URL per line, # for comments)');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  winssl_health_checker https://api.example.com/health');
  WriteLn('  winssl_health_checker https://api1.com/health https://api2.com/health');
  WriteLn('  winssl_health_checker -f endpoints.txt');
  WriteLn;
  WriteLn('Features:');
  WriteLn('  - Batch health checking of HTTPS endpoints');
  WriteLn('  - Response time measurement');
  WriteLn('  - HTTP status code validation');
  WriteLn('  - Color-coded output (green=healthy, red=unhealthy)');
  WriteLn('  - 5-second timeout per endpoint');
end;

// Main program
var
  LURLs: TStringList;
  i: Integer;
  LResult: THealthCheckResult;
  LSuccessCount, LFailCount: Integer;
begin
  WriteLn('=== WinSSL Health Checker ===');
  WriteLn;

  LURLs := TStringList.Create;
  try
    try
      // Parse command line arguments
      if ParamCount = 0 then
      begin
        ShowUsage;
        Halt(1);
      end;

      // Check for file input
      if (ParamCount >= 2) and (ParamStr(1) = '-f') then
      begin
        if not LoadURLsFromFile(ParamStr(2), LURLs) then
          Halt(1);
      end
      else
      begin
        // Add URLs from command line
        for i := 1 to ParamCount do
          LURLs.Add(ParamStr(i));
      end;

      if LURLs.Count = 0 then
      begin
        WriteLn('Error: No URLs provided');
        Halt(1);
      end;

      WriteLn('Checking ', LURLs.Count, ' endpoint(s)...');
      WriteLn;

      // Check each URL
      LSuccessCount := 0;
      LFailCount := 0;

      for i := 0 to LURLs.Count - 1 do
      begin
        LResult := CheckHealth(LURLs[i]);
        DisplayResult(LResult);

        if LResult.Success then
          Inc(LSuccessCount)
        else
          Inc(LFailCount);
      end;

      // Summary
      WriteLn;
      WriteLn('Results: ', LSuccessCount, ' healthy, ', LFailCount, ' unhealthy');

      if LFailCount = 0 then
      begin
        WriteLn('All endpoints are healthy!');
        ExitCode := 0;
      end
      else
      begin
        WriteLn('Some endpoints are unhealthy!');
        ExitCode := 1;
      end;

    except
      on E: Exception do
      begin
        WriteLn;
        WriteLn('EXCEPTION: ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    LURLs.Free;
  end;
end.
