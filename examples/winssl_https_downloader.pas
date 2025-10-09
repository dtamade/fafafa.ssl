program winssl_https_downloader;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ENDIF}
  SysUtils, Classes, DateUtils,
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.abstract.types;

type
  TURLComponents = record
    Protocol: string;
    Host: string;
    Port: Word;
    Path: string;
  end;

  TDownloadProgress = record
    TotalBytes: Int64;
    StartTime: TDateTime;
    LastUpdateTime: TDateTime;
    BytesPerSecond: Double;
  end;

var
  GProgress: TDownloadProgress;

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

{ Format byte size for display }
function FormatByteSize(aBytes: Int64): string;
begin
  if aBytes < 1024 then
    Result := Format('%d B', [aBytes])
  else if aBytes < 1024 * 1024 then
    Result := Format('%.2f KB', [aBytes / 1024])
  else if aBytes < 1024 * 1024 * 1024 then
    Result := Format('%.2f MB', [aBytes / (1024 * 1024)])
  else
    Result := Format('%.2f GB', [aBytes / (1024 * 1024 * 1024)]);
end;

{ Format speed for display }
function FormatSpeed(aBytesPerSecond: Double): string;
begin
  if aBytesPerSecond < 1024 then
    Result := Format('%.0f B/s', [aBytesPerSecond])
  else if aBytesPerSecond < 1024 * 1024 then
    Result := Format('%.2f KB/s', [aBytesPerSecond / 1024])
  else
    Result := Format('%.2f MB/s', [aBytesPerSecond / (1024 * 1024)]);
end;

{ Update download progress display }
procedure UpdateProgress(aBytesReceived: Integer);
var
  LNow: TDateTime;
  LElapsed: Double;
begin
  GProgress.TotalBytes := GProgress.TotalBytes + aBytesReceived;
  LNow := Now;

  // Update every 0.5 seconds
  if MilliSecondsBetween(LNow, GProgress.LastUpdateTime) >= 500 then
  begin
    LElapsed := (LNow - GProgress.StartTime) * 86400; // Convert to seconds
    if LElapsed > 0 then
      GProgress.BytesPerSecond := GProgress.TotalBytes / LElapsed;

    Write(#13'Downloaded: ', FormatByteSize(GProgress.TotalBytes),
          '  Speed: ', FormatSpeed(GProgress.BytesPerSecond), '  ');

    GProgress.LastUpdateTime := LNow;
  end;
end;

{ Download file from HTTPS URL }
function DownloadFile(const aURL: string; const aOutputFile: string): Boolean;
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
  LFile: TFileStream;
  LWSAData: TWSAData;
  LHeaderEnd: Boolean;
  LResponse: string;
  LPos: Integer;
begin
  Result := False;

  // Parse URL
  if not ParseURL(aURL, LComponents) then
    Exit;

  WriteLn('Downloading from: ', aURL);
  WriteLn('Host: ', LComponents.Host, ':', LComponents.Port);
  WriteLn('Path: ', LComponents.Path);
  WriteLn('Output: ', aOutputFile);
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

    WriteLn('Using: Windows Schannel (WinSSL)');

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

      WriteLn('TLS handshake completed');
      WriteLn('Protocol: ', ProtocolVersionToString(LConn.GetProtocolVersion));
      WriteLn;

      // Send HTTP GET request
      LRequest := Format('GET %s HTTP/1.1'#13#10 +
                        'Host: %s'#13#10 +
                        'User-Agent: WinSSL-Downloader/1.0'#13#10 +
                        'Connection: close'#13#10 +
                        #13#10, [LComponents.Path, LComponents.Host]);

      if not LConn.WriteString(LRequest) then
      begin
        WriteLn('Error: Failed to send HTTP request');
        Exit;
      end;

      // Initialize progress tracking
      FillChar(GProgress, SizeOf(GProgress), 0);
      GProgress.StartTime := Now;
      GProgress.LastUpdateTime := GProgress.StartTime;

      // Create output file
      try
        LFile := TFileStream.Create(aOutputFile, fmCreate);
        try
          WriteLn('Downloading...');

          // Skip HTTP headers
          LHeaderEnd := False;
          LResponse := '';

          while not LHeaderEnd do
          begin
            LBytesRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
            if LBytesRead <= 0 then
              Break;

            SetLength(LResponse, Length(LResponse) + LBytesRead);
            Move(LBuffer[0], LResponse[Length(LResponse) - LBytesRead + 1], LBytesRead);

            // Look for header end (double CRLF)
            LPos := Pos(#13#10#13#10, LResponse);
            if LPos > 0 then
            begin
              // Check HTTP status
              if Pos('HTTP/1.1 200', LResponse) = 0 then
              begin
                WriteLn;
                WriteLn('Error: Server returned non-200 status');
                WriteLn(Copy(LResponse, 1, Pos(#13#10, LResponse) - 1));
                Exit;
              end;

              LHeaderEnd := True;

              // Write body part (after headers)
              LPos := LPos + 4; // Skip the double CRLF
              if LPos <= Length(LResponse) then
              begin
                LFile.WriteBuffer(LResponse[LPos], Length(LResponse) - LPos + 1);
                UpdateProgress(Length(LResponse) - LPos + 1);
              end;
            end;
          end;

          // Download remaining data
          repeat
            LBytesRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));
            if LBytesRead > 0 then
            begin
              LFile.WriteBuffer(LBuffer, LBytesRead);
              UpdateProgress(LBytesRead);
            end;
          until LBytesRead <= 0;

          WriteLn;
          WriteLn;
          WriteLn('Download completed successfully!');
          WriteLn('Total size: ', FormatByteSize(GProgress.TotalBytes));
          WriteLn('Average speed: ', FormatSpeed(GProgress.BytesPerSecond));
          WriteLn('Saved to: ', aOutputFile);

          Result := True;

        finally
          LFile.Free;
        end;
      except
        on E: Exception do
        begin
          WriteLn;
          WriteLn('Error: Failed to create output file: ', E.Message);
        end;
      end;

      // Close SSL connection
      LConn.Shutdown;

    finally
      closesocket(LSocket);
    end;

  finally
    WSACleanup;
  end;
end;

{ Convert protocol version to string }
function ProtocolVersionToString(aVer: TSSLProtocolVersion): string;
begin
  case aVer of
    sslProtocolTLS10: Result := 'TLS 1.0';
    sslProtocolTLS11: Result := 'TLS 1.1';
    sslProtocolTLS12: Result := 'TLS 1.2';
    sslProtocolTLS13: Result := 'TLS 1.3';
    else Result := 'Unknown';
  end;
end;

{ Display usage information }
procedure ShowUsage;
begin
  WriteLn('WinSSL HTTPS Downloader v1.0');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  winssl_https_downloader <URL> <output_file>');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  winssl_https_downloader https://example.com/file.zip download.zip');
  WriteLn('  winssl_https_downloader https://api.github.com/repos/user/repo/zipball output.zip');
  WriteLn;
  WriteLn('Features:');
  WriteLn('  - Zero-dependency HTTPS downloads using Windows Schannel');
  WriteLn('  - Real-time progress display with speed indicator');
  WriteLn('  - TLS 1.2/1.3 support');
  WriteLn('  - Streaming download for large files');
end;

// Main program
var
  LURL, LOutputFile: string;
  LResponse: string;
begin
  WriteLn('=== WinSSL HTTPS Downloader ===');
  WriteLn;

  // Parse command line arguments
  if ParamCount < 2 then
  begin
    ShowUsage;
    Halt(1);
  end;

  LURL := ParamStr(1);
  LOutputFile := ParamStr(2);

  // Check if output file already exists
  if FileExists(LOutputFile) then
  begin
    Write('Output file already exists. Overwrite? (y/n): ');
    ReadLn(LResponse);
    if LowerCase(Trim(LResponse)) <> 'y' then
    begin
      WriteLn('Download cancelled.');
      Halt(0);
    end;
  end;

  try
    if DownloadFile(LURL, LOutputFile) then
    begin
      WriteLn;
      WriteLn('SUCCESS!');
      ExitCode := 0;
    end
    else
    begin
      WriteLn;
      WriteLn('FAILED!');

      // Clean up partial download
      if FileExists(LOutputFile) then
      begin
        try
          DeleteFile(LOutputFile);
          WriteLn('Partial download deleted.');
        except
          // Ignore
        end;
      end;

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
end.
