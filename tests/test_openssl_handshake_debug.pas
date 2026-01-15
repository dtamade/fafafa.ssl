{**
 * Test: OpenSSL Handshake Debug
 * Purpose: Diagnose why OpenSSL TLS handshake is failing
 *}

program test_openssl_handshake_debug;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets, BaseUnix,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed;

const
  TEST_HOST = '142.250.185.68';  // Google IP
  TEST_PORT = 443;

function CreateTCPSocket(const AHost: string; APort: Integer): TSocket;
var
  LSockAddr: TInetSockAddr;
  LAddr: in_addr;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then Exit(-1);

  FillChar(LSockAddr, SizeOf(LSockAddr), 0);
  LSockAddr.sin_family := AF_INET;
  LSockAddr.sin_port := htons(APort);
  LAddr := StrToNetAddr(AHost);
  LSockAddr.sin_addr := LAddr;

  if fpConnect(Result, @LSockAddr, SizeOf(LSockAddr)) < 0 then
  begin
    CloseSocket(Result);
    Exit(-1);
  end;
end;

var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSocket: TSocket;

begin
  WriteLn('OpenSSL Handshake Debug Test');
  WriteLn('============================');
  WriteLn('');

  // Create OpenSSL library
  WriteLn('1. Creating OpenSSL library...');
  LLib := CreateOpenSSLLibrary;
  if LLib = nil then
  begin
    WriteLn('   FAILED: Could not create OpenSSL library');
    Halt(1);
  end;
  WriteLn('   OK');

  // Initialize
  WriteLn('2. Initializing library...');
  if not LLib.Initialize then
  begin
    WriteLn('   FAILED: Could not initialize OpenSSL');
    Halt(1);
  end;
  WriteLn('   OK - Version: ', LLib.GetVersionString);

  // Create context
  WriteLn('3. Creating SSL context...');
  LCtx := LLib.CreateContext(sslCtxClient);
  if LCtx = nil then
  begin
    WriteLn('   FAILED: Could not create context');
    LLib.Finalize;
    Halt(1);
  end;
  WriteLn('   OK');

  // Disable peer verification for testing
  WriteLn('4. Configuring context (disable peer verification)...');
  LCtx.SetVerifyMode([]);  // No verification
  WriteLn('   OK');

  // Create TCP socket
  WriteLn('5. Creating TCP socket to ', TEST_HOST, ':', TEST_PORT, '...');
  LSocket := CreateTCPSocket(TEST_HOST, TEST_PORT);
  if LSocket <= 0 then
  begin
    WriteLn('   FAILED: Could not connect TCP socket');
    LLib.Finalize;
    Halt(1);
  end;
  WriteLn('   OK - Socket: ', LSocket);

  // Create SSL connection
  WriteLn('6. Creating SSL connection...');
  LConn := LCtx.CreateConnection(LSocket);
  if LConn = nil then
  begin
    WriteLn('   FAILED: Could not create SSL connection');
    CloseSocket(LSocket);
    LLib.Finalize;
    Halt(1);
  end;
  WriteLn('   OK');

  // Perform handshake
  WriteLn('7. Performing TLS handshake...');
  if LConn.Connect then
  begin
    WriteLn('   SUCCESS!');
    WriteLn('   Protocol: ', Ord(LConn.GetProtocolVersion));
    WriteLn('   Cipher: ', LConn.GetCipherName);
    LConn.Shutdown;
  end
  else
  begin
    WriteLn('   FAILED: Handshake failed');
    WriteLn('   State: ', LConn.GetStateString);
    WriteLn('   Verify Result: ', LConn.GetVerifyResultString);
  end;

  CloseSocket(LSocket);
  LLib.Finalize;

  WriteLn('');
  WriteLn('Test complete.');
end.
