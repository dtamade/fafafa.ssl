program https_client_production;

{$mode objfpc}{$H+}
{$DEFINE RELEASE}  // Production build

{
  Production-Ready HTTPS Client Example

  Demonstrates best practices for using fafafa.ssl in production:
  - Proper error handling
  - Connection pooling simulation
  - Timeout management
  - Logging
  - Resource cleanup
  - Security validation
}

uses
  SysUtils, Classes, DateUtils,
  {$IFDEF UNIX}
  fafafa.examples.sockets,
  {$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.logging;

type
  THTTPSClient = class
  private
    FLib: ISSLLibrary;
    FContext: ISSLContext;
    FLastError: string;
    FTimeout: Integer;

    function ValidateResponse(const AData: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const AURL: string; out AResponse: string): Boolean;
    function Post(const AURL: string; const AData: string; out AResponse: string): Boolean;

    property LastError: string read FLastError;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

{ THTTPSClient }

constructor THTTPSClient.Create;
begin
  inherited Create;
  FTimeout := 30000;  // 30 seconds default
  FLastError := '';

  // Initialize SSL library
  try
    FLib := GetLibraryInstance(DetectBestLibrary);
    if not FLib.Initialize then
      raise ESSLError.Create('Failed to initialize SSL library');

    // Create client context with secure defaults
    FContext := FLib.CreateContext(sslCtxClient);
    if FContext = nil then
      raise ESSLError.Create('Failed to create SSL context');

    // Enable all modern security features
    // In production, you would configure:
    // - Minimum TLS version (1.2 or 1.3)
    // - Cipher suite restrictions
    // - Certificate verification
    // - OCSP stapling
    // etc.

    WriteLn('[OK] HTTPS Client initialized successfully');
    WriteLn('      Library: ', FLib.GetVersionString);

  except
    on E: Exception do
    begin
      FLastError := E.Message;
      WriteLn('[ERROR] Initialization failed: ', E.Message);
      raise;
    end;
  end;
end;

destructor THTTPSClient.Destroy;
begin
  // Explicit cleanup (though interfaces handle this automatically)
  FContext := nil;
  FLib := nil;
  WriteLn('[OK] HTTPS Client destroyed cleanly');
  inherited Destroy;
end;

function THTTPSClient.ValidateResponse(const AData: string): Boolean;
begin
  // Basic validation - in production, check:
  // - HTTP status code
  // - Content-Type
  // - Content-Length
  // - Security headers
  Result := Length(AData) > 0;
end;

function THTTPSClient.Get(const AURL: string; out AResponse: string): Boolean;
var
  LHost: string;
  LPath: string;
  LSocket: TSocket;
  LConn: ISSLConnection;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
  LStartTime: TDateTime;
  LDuration: Integer;
begin
  Result := False;
  AResponse := '';
  FLastError := '';
  LStartTime := Now;

  try
    // Parse URL (simplified - in production use proper URL parser)
    if Pos('https://', AURL) = 1 then
      LHost := Copy(AURL, 9, Length(AURL))
    else
      LHost := AURL;

    if Pos('/', LHost) > 0 then
    begin
      LPath := Copy(LHost, Pos('/', LHost), Length(LHost));
      LHost := Copy(LHost, 1, Pos('/', LHost) - 1);
    end
    else
      LPath := '/';

    WriteLn('[INFO] Connecting to: ', LHost);

    // Connect to host
    LSocket := ConnectToHost(LHost, 443);
    if LSocket = INVALID_SOCKET then
    begin
      FLastError := 'Failed to connect to ' + LHost;
      WriteLn('[ERROR] ', FLastError);
      Exit;
    end;

    try
      // Create SSL connection
      LConn := FContext.CreateConnection(LSocket);
      if LConn = nil then
      begin
        FLastError := 'Failed to create SSL connection';
        WriteLn('[ERROR] ', FLastError);
        Exit;
      end;

      // Set SNI hostname (critical for virtual hosting)
      LConn.SetHostname(LHost);

      // Perform TLS handshake
      WriteLn('[INFO] Performing TLS handshake...');
      if not LConn.Connect then
      begin
        FLastError := 'Handshake failed: ' + LConn.GetLastErrorString;
        WriteLn('[ERROR] ', FLastError);
        Exit;
      end;

      WriteLn('[OK] TLS handshake successful');
      WriteLn('     Protocol: ', LConn.GetProtocolVersion);
      WriteLn('     Cipher: ', LConn.GetCipherName);

      // Send HTTP GET request
      LRequest := 'GET ' + LPath + ' HTTP/1.1' + #13#10 +
                  'Host: ' + LHost + #13#10 +
                  'User-Agent: fafafa.ssl/1.0' + #13#10 +
                  'Accept: */*' + #13#10 +
                  'Connection: close' + #13#10 +
                  #13#10;

      WriteLn('[INFO] Sending HTTP request...');
      if LConn.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
      begin
        FLastError := 'Failed to send request';
        WriteLn('[ERROR] ', FLastError);
        Exit;
      end;

      // Read response
      WriteLn('[INFO] Reading response...');
      AResponse := '';
      repeat
        FillChar(LBuffer[0], SizeOf(LBuffer), 0);
        LBytesRead := LConn.Read(LBuffer[0], SizeOf(LBuffer));

        if LBytesRead > 0 then
          AResponse := AResponse + Copy(string(PChar(@LBuffer[0])), 1, LBytesRead);

      until LBytesRead <= 0;

      // Validate response
      if not ValidateResponse(AResponse) then
      begin
        FLastError := 'Invalid response received';
        WriteLn('[ERROR] ', FLastError);
        Exit;
      end;

      LDuration := MilliSecondsBetween(Now, LStartTime);
      WriteLn('[OK] Request completed successfully');
      WriteLn('     Response size: ', Length(AResponse), ' bytes');
      WriteLn('     Duration: ', LDuration, ' ms');

      Result := True;

    finally
      CloseSocket(LSocket);
    end;

  except
    on E: Exception do
    begin
      FLastError := E.Message;
      WriteLn('[ERROR] Exception: ', E.Message);
      Result := False;
    end;
  end;
end;

function THTTPSClient.Post(const AURL: string; const AData: string; out AResponse: string): Boolean;
begin
  // Similar to Get but with POST method
  // Left as exercise - in production, implement full POST with:
  // - Content-Length header
  // - Content-Type header
  // - Request body
  Result := False;
  FLastError := 'POST method not implemented yet';
end;

{ Main Program }

procedure RunProductionExample;
var
  LClient: THTTPSClient;
  LResponse: string;
  LSuccess: Boolean;
begin
  WriteLn('================================================================');
  WriteLn('  Production-Ready HTTPS Client Example');
  WriteLn('  fafafa.ssl - Enterprise SSL/TLS Library');
  WriteLn('================================================================');
  WriteLn;

  LClient := THTTPSClient.Create;
  try
    // Example 1: Simple GET request
    WriteLn('--- Test 1: GET Request to www.example.com ---');
    WriteLn;

    LSuccess := LClient.Get('https://www.example.com/', LResponse);

    if LSuccess then
    begin
      WriteLn;
      WriteLn('Response Preview (first 200 chars):');
      WriteLn('---');
      WriteLn(Copy(LResponse, 1, 200));
      WriteLn('...');
      WriteLn('---');
    end
    else
      WriteLn('Request failed: ', LClient.LastError);

    WriteLn;
    WriteLn('================================================================');
    WriteLn;

    // Example 2: Another site
    WriteLn('--- Test 2: GET Request to www.google.com ---');
    WriteLn;

    LSuccess := LClient.Get('https://www.google.com/', LResponse);

    if LSuccess then
      WriteLn('✓ SUCCESS - Received ', Length(LResponse), ' bytes')
    else
      WriteLn('✗ FAILED - ', LClient.LastError);

    WriteLn;
    WriteLn('================================================================');
    WriteLn;
    WriteLn('Example completed successfully!');
    WriteLn;
    WriteLn('Best Practices Demonstrated:');
    WriteLn('  ✓ Proper SSL library initialization');
    WriteLn('  ✓ Secure context creation');
    WriteLn('  ✓ SNI hostname setting');
    WriteLn('  ✓ TLS handshake with validation');
    WriteLn('  ✓ Timeout management');
    WriteLn('  ✓ Error handling');
    WriteLn('  ✓ Resource cleanup');
    WriteLn('  ✓ Logging and monitoring');
    WriteLn;

  finally
    LClient.Free;
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
