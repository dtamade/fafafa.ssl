{**
 * fafafa.ssl - Complete QuickStart Example
 *
 * Purpose: 30-second copy-paste-run TLS client demo
 * This example includes TCP socket creation - no external code needed!
 *
 * Compile:
 *   Linux:   fpc -Fusrc -Fusrc/openssl -Fuexamples quickstart_complete.pas
 *   Windows: fpc -Fusrc -Fusrc/openssl -Fuexamples quickstart_complete.pas
 *
 * Run: ./quickstart_complete
 *}
program quickstart_complete;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math,
  fafafa.ssl.factory,
  fafafa.ssl.base,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.backed,  // Registers OpenSSL backend
  fafafa.examples.tcp;

const
  TARGET_HOST = 'www.google.com';
  TARGET_PORT = 443;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Socket: TSocketHandle;
  NetError: string;
  Request, Response: string;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;

begin
  WriteLn('=== fafafa.ssl QuickStart Demo ===');
  WriteLn('Connecting to ', TARGET_HOST, ':', TARGET_PORT);
  WriteLn;

  // Step 1: Initialize network (required on Windows)
  if not InitNetwork(NetError) then
  begin
    WriteLn('ERROR: Network init failed: ', NetError);
    Halt(1);
  end;

  try
    // Step 2: Create TCP socket and connect
    WriteLn('[1/5] Creating TCP connection...');
    Socket := ConnectTCP(TARGET_HOST, TARGET_PORT);
    WriteLn('      TCP connected');

    // Step 3: Initialize SSL library
    WriteLn('[2/5] Initializing OpenSSL...');
    Lib := TSSLFactory.GetLibrary(sslOpenSSL);
    if not Lib.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize OpenSSL');
      CloseSocket(Socket);
      Halt(1);
    end;
    WriteLn('      OpenSSL ', Lib.GetVersionString);

    // Step 4: Create SSL context (client mode)
    WriteLn('[3/5] Creating SSL context...');
    Ctx := Lib.CreateContext(sslCtxClient);
    Ctx.SetVerifyMode([sslVerifyNone]); // Skip cert verify for demo
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

    // Step 5: Create SSL connection and handshake
    WriteLn('[4/5] Performing TLS handshake...');
    Conn := Ctx.CreateConnection(Socket);
    (Conn as ISSLClientConnection).SetServerName(TARGET_HOST);
    Conn.Connect;
    WriteLn('      TLS handshake complete');

    // Step 6: Show connection info
    WriteLn('[5/5] Connection established!');
    WriteLn;
    WriteLn('=== Connection Info ===');
    WriteLn('Protocol:     TLS ', Ord(Conn.GetProtocolVersion) - Ord(sslProtocolTLS10) + 10, '/10');
    WriteLn('Cipher:       ', Conn.GetCipherName);
    WriteLn('Session Reuse:', Conn.IsSessionReused);
    WriteLn;

    // Step 7: Send HTTP request
    WriteLn('=== Sending HTTP Request ===');
    Request := 'GET / HTTP/1.1'#13#10 +
               'Host: ' + TARGET_HOST + #13#10 +
               'Connection: close'#13#10 +
               #13#10;
    Conn.Write(Request[1], Length(Request));
    WriteLn('Sent ', Length(Request), ' bytes');

    // Step 8: Read response (first chunk)
    WriteLn;
    WriteLn('=== Response (first 500 bytes) ===');
    BytesRead := Conn.Read(Buffer[0], SizeOf(Buffer));
    if BytesRead > 0 then
    begin
      SetString(Response, PAnsiChar(@Buffer[0]), Min(BytesRead, 500));
      WriteLn(Response);
      if BytesRead > 500 then
        WriteLn('... (', BytesRead - 500, ' more bytes)');
    end;

    // Cleanup
    Conn.Shutdown;
    CloseSocket(Socket);
    Lib.Finalize;

    WriteLn;
    WriteLn('=== SUCCESS ===');
    WriteLn('TLS connection completed successfully!');

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.Message);
      CloseSocket(Socket);
      Halt(1);
    end;
  end;

  CleanupNetwork;
end.
