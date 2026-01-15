program winssl_rest_client;

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
  THTTPMethod = (hmGET, hmPOST, hmPUT, hmDELETE, hmPATCH);

  TURLComponents = record
    Protocol: string;
    Host: string;
    Port: Word;
    Path: string;
  end;

  TRequestOptions = record
    Method: THTTPMethod;
    URL: string;
    RequestBody: string;
    ContentType: string;
    CustomHeaders: TStringList;
  end;

{ Convert HTTP method to string }
function HTTPMethodToString(aMethod: THTTPMethod): string;
begin
  case aMethod of
    hmGET: Result := 'GET';
    hmPOST: Result := 'POST';
    hmPUT: Result := 'PUT';
    hmDELETE: Result := 'DELETE';
    hmPATCH: Result := 'PATCH';
  end;
end;

{ Parse HTTP method from string }
function ParseHTTPMethod(const aMethod: string; out aResult: THTTPMethod): Boolean;
var
  LMethod: string;
begin
  Result := True;
  LMethod := UpperCase(aMethod);

  if LMethod = 'GET' then
    aResult := hmGET
  else if LMethod = 'POST' then
    aResult := hmPOST
  else if LMethod = 'PUT' then
    aResult := hmPUT
  else if LMethod = 'DELETE' then
    aResult := hmDELETE
  else if LMethod = 'PATCH' then
    aResult := hmPATCH
  else
    Result := False;
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
      WriteLn('Error: Invalid port number');
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

{ Simple JSON pretty-print }
function PrettyPrintJSON(const aJSON: string): string;
var
  i, LIndent: Integer;
  LChar: Char;
  LInString: Boolean;
  LResult: string;
begin
  LResult := '';
  LIndent := 0;
  LInString := False;

  for i := 1 to Length(aJSON) do
  begin
    LChar := aJSON[i];

    case LChar of
      '"':
        begin
          LResult := LResult + LChar;
          if (i > 1) and (aJSON[i - 1] <> '\') then
            LInString := not LInString;
        end;

      '{', '[':
        begin
          LResult := LResult + LChar + #13#10;
          if not LInString then
          begin
            Inc(LIndent);
            LResult := LResult + StringOfChar(' ', LIndent * 2);
          end;
        end;

      '}', ']':
        begin
          if not LInString then
          begin
            Dec(LIndent);
            LResult := LResult + #13#10 + StringOfChar(' ', LIndent * 2);
          end;
          LResult := LResult + LChar;
        end;

      ',':
        begin
          LResult := LResult + LChar;
          if not LInString then
            LResult := LResult + #13#10 + StringOfChar(' ', LIndent * 2);
        end;

      ':':
        begin
          LResult := LResult + LChar;
          if not LInString then
            LResult := LResult + ' ';
        end;

      else
        LResult := LResult + LChar;
    end;
  end;

  Result := LResult;
end;

{ Make HTTP/HTTPS request }
function MakeRequest(const aOptions: TRequestOptions): Boolean;
var
  LComponents: TURLComponents;
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
  LRequest: string;
  LBuffer: array[0..8191] of Byte;
  LBytesRead: Integer;
  LResponse: string;
  LWSAData: TWSAData;
  LStartTime, LEndTime: TDateTime;
  LHeaderPos, LStatusPos: Integer;
  LStatusCode: string;
  i: Integer;
  LHeaders, LBody: string;
begin
  Result := False;

  // Parse URL
  if not ParseURL(aOptions.URL, LComponents) then
    Exit;

  WriteLn('Method: ', HTTPMethodToString(aOptions.Method));
  WriteLn('URL: ', aOptions.URL);
  WriteLn('Host: ', LComponents.Host, ':', LComponents.Port);
  WriteLn('Path: ', LComponents.Path);
  if aOptions.RequestBody <> '' then
    WriteLn('Request Body: ', Length(aOptions.RequestBody), ' bytes');
  WriteLn;

  // Initialize Winsock
  if WSAStartup(MAKEWORD(2, 2), LWSAData) <> 0 then
  begin
    WriteLn('Error: Failed to initialize Winsock');
    Exit;
  end;

  try
    // Create WinSSL library
    LLib := CreateSSLLibrary(sslWinSSL);
    if not LLib.Initialize then
    begin
      WriteLn('Error: Failed to initialize WinSSL library');
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
      WriteLn('Error: Failed to create socket');
      Exit;
    end;

    try
      // Resolve hostname
      WriteLn('Resolving hostname...');
      LHostEnt := gethostbyname(PAnsiChar(AnsiString(LComponents.Host)));
      if LHostEnt = nil then
      begin
        WriteLn('Error: Failed to resolve hostname');
        Exit;
      end;

      // Connect to server
      FillChar(LAddr, SizeOf(LAddr), 0);
      LAddr.sin_family := AF_INET;
      LAddr.sin_port := htons(LComponents.Port);
      LAddr.sin_addr := PInAddr(LHostEnt^.h_addr_list^)^;

      WriteLn('Connecting to server...');
      if WinSock2.connect(LSocket, LAddr, SizeOf(LAddr)) = SOCKET_ERROR then
      begin
        WriteLn('Error: Failed to connect to server');
        Exit;
      end;

      // Create SSL connection
      LConn := LCtx.CreateConnection(LSocket);

      WriteLn('Performing TLS handshake...');
      if not LConn.Connect then
      begin
        WriteLn('Error: TLS handshake failed');
        Exit;
      end;

      WriteLn('Connection established');
      WriteLn;

      // Build HTTP request
      LRequest := HTTPMethodToString(aOptions.Method) + ' ' + LComponents.Path + ' HTTP/1.1'#13#10 +
                  'Host: ' + LComponents.Host + #13#10 +
                  'User-Agent: WinSSL-RestClient/1.0'#13#10 +
                  'Accept: application/json, */*'#13#10;

      // Add custom headers
      if aOptions.CustomHeaders <> nil then
      begin
        for i := 0 to aOptions.CustomHeaders.Count - 1 do
          LRequest := LRequest + aOptions.CustomHeaders[i] + #13#10;
      end;

      // Add content-related headers if there's a request body
      if aOptions.RequestBody <> '' then
      begin
        LRequest := LRequest +
                    'Content-Type: ' + aOptions.ContentType + #13#10 +
                    'Content-Length: ' + IntToStr(Length(aOptions.RequestBody)) + #13#10;
      end;

      LRequest := LRequest + 'Connection: close'#13#10#13#10;

      // Add request body if present
      if aOptions.RequestBody <> '' then
        LRequest := LRequest + aOptions.RequestBody;

      // Send request
      LStartTime := Now;

      if not LConn.WriteString(LRequest) then
      begin
        WriteLn('Error: Failed to send HTTP request');
        Exit;
      end;

      // Receive response
      LResponse := '';
      repeat
        LBytesRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
        if LBytesRead > 0 then
        begin
          SetLength(LResponse, Length(LResponse) + LBytesRead);
          Move(LBuffer[0], LResponse[Length(LResponse) - LBytesRead + 1], LBytesRead);
        end;
      until LBytesRead <= 0;

      LEndTime := Now;

      // Parse response
      LHeaderPos := Pos(#13#10#13#10, LResponse);
      if LHeaderPos = 0 then
      begin
        WriteLn('Error: Invalid HTTP response');
        Exit;
      end;

      // Extract status code
      LStatusPos := Pos('HTTP/1.1 ', LResponse);
      if LStatusPos > 0 then
      begin
        LStatusCode := Copy(LResponse, LStatusPos + 9, 3);
        WriteLn('Status: HTTP/1.1 ', LStatusCode);
      end;

      WriteLn('Response Time: ', FormatDateTime('s.zzz', LEndTime - LStartTime), ' seconds');
      WriteLn('Response Size: ', Length(LResponse) - LHeaderPos - 3, ' bytes');
      WriteLn;

      // Display response headers (first line only)
      WriteLn('Response Headers:');
      WriteLn('---');
      LHeaders := Copy(LResponse, 1, LHeaderPos - 1);
      WriteLn(LHeaders);
      WriteLn('---');
      WriteLn;

      // Display response body
      LBody := Copy(LResponse, LHeaderPos + 4, Length(LResponse));
      WriteLn('Response Body:');
      WriteLn('---');

      // Try to pretty-print JSON
      if (Pos('application/json', LowerCase(LResponse)) > 0) or
         ((Length(LBody) > 0) and ((LBody[1] = '{') or (LBody[1] = '['))) then
      begin
        WriteLn(PrettyPrintJSON(LBody));
      end
      else
      begin
        WriteLn(LBody);
      end;

      WriteLn('---');

      Result := True;

      // Close SSL connection
      LConn.Shutdown;

    finally
      closesocket(LSocket);
    end;

  finally
    WSACleanup;
  end;
end;

{ Display usage information }
procedure ShowUsage;
begin
  WriteLn('WinSSL REST Client v1.0');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  winssl_rest_client <METHOD> <URL> [OPTIONS]');
  WriteLn;
  WriteLn('Methods:');
  WriteLn('  GET, POST, PUT, DELETE, PATCH');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -d <data>          Request body (for POST/PUT/PATCH)');
  WriteLn('  -H <header>        Custom HTTP header (can be used multiple times)');
  WriteLn('  -t <content-type>  Content-Type header (default: application/json)');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  winssl_rest_client GET https://api.github.com/users/octocat');
  WriteLn('  winssl_rest_client POST https://httpbin.org/post -d ''{"key":"value"}''');
  WriteLn('  winssl_rest_client PUT https://api.example.com/item/1 -d ''{"name":"test"}''');
  WriteLn('  winssl_rest_client DELETE https://api.example.com/item/1');
  WriteLn;
  WriteLn('Features:');
  WriteLn('  - Zero-dependency HTTPS using Windows Schannel');
  WriteLn('  - JSON request/response support with pretty-printing');
  WriteLn('  - Custom headers support');
  WriteLn('  - Response time measurement');
end;

// Main program
var
  LOptions: TRequestOptions;
  i: Integer;
  LParam: string;
begin
  WriteLn('=== WinSSL REST Client ===');
  WriteLn;

  // Initialize options
  FillChar(LOptions, SizeOf(LOptions), 0);
  LOptions.ContentType := 'application/json';
  LOptions.CustomHeaders := TStringList.Create;

  try
    // Parse command line arguments
    if ParamCount < 2 then
    begin
      ShowUsage;
      Halt(1);
    end;

    // Parse method
    if not ParseHTTPMethod(ParamStr(1), LOptions.Method) then
    begin
      WriteLn('Error: Invalid HTTP method: ', ParamStr(1));
      WriteLn;
      ShowUsage;
      Halt(1);
    end;

    // Parse URL
    LOptions.URL := ParamStr(2);

    // Parse options
    i := 3;
    while i <= ParamCount do
    begin
      LParam := ParamStr(i);

      if LParam = '-d' then
      begin
        Inc(i);
        if i <= ParamCount then
          LOptions.RequestBody := ParamStr(i)
        else
        begin
          WriteLn('Error: Missing argument for -d');
          Halt(1);
        end;
      end
      else if LParam = '-H' then
      begin
        Inc(i);
        if i <= ParamCount then
          LOptions.CustomHeaders.Add(ParamStr(i))
        else
        begin
          WriteLn('Error: Missing argument for -H');
          Halt(1);
        end;
      end
      else if LParam = '-t' then
      begin
        Inc(i);
        if i <= ParamCount then
          LOptions.ContentType := ParamStr(i)
        else
        begin
          WriteLn('Error: Missing argument for -t');
          Halt(1);
        end;
      end
      else
      begin
        WriteLn('Error: Unknown option: ', LParam);
        Halt(1);
      end;

      Inc(i);
    end;

    // Make request
    try
      if MakeRequest(LOptions) then
      begin
        WriteLn;
        WriteLn('SUCCESS!');
        ExitCode := 0;
      end
      else
      begin
        WriteLn;
        WriteLn('FAILED!');
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
    LOptions.CustomHeaders.Free;
  end;
end.
