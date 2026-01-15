{**
 * Test: PEM Export Functionality
 * Purpose: Test DER to PEM conversion for certificates
 *}

program test_pem_export;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets, BaseUnix,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.connection,
  fafafa.ssl.mbedtls.certificate;

const
  TEST_HOST = '142.250.185.68';  // Google IP
  TEST_PORT = 443;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;

procedure Test(const AName: string; ACondition: Boolean);
begin
  Inc(GTestCount);
  Write(AName, ': ');
  if ACondition then
  begin
    WriteLn('PASS');
    Inc(GPassCount);
  end
  else
    WriteLn('FAIL');
end;

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
  LPeerCert: ISSLCertificate;
  LPEM: string;
  LDER: TBytes;

begin
  WriteLn('PEM Export Functionality Test');
  WriteLn('==============================');
  WriteLn('');

  // Create MbedTLS library
  LLib := CreateMbedTLSLibrary;
  if (LLib = nil) or not LLib.Initialize then
  begin
    WriteLn('MbedTLS not available');
    Halt(1);
  end;

  WriteLn('MbedTLS Version: ', LLib.GetVersionString);
  WriteLn('');

  // Create context and connect
  LCtx := LLib.CreateContext(sslCtxClient);
  Test('Context created', LCtx <> nil);

  LSocket := CreateTCPSocket(TEST_HOST, TEST_PORT);
  Test('TCP connected', LSocket > 0);

  if LSocket > 0 then
  begin
    LConn := LCtx.CreateConnection(LSocket);
    Test('SSL connection created', LConn <> nil);

    if LConn.Connect then
    begin
      Test('TLS handshake', True);

      // Get peer certificate
      LPeerCert := LConn.GetPeerCertificate;
      Test('Peer certificate retrieved', LPeerCert <> nil);

      if LPeerCert <> nil then
      begin
        // Test DER export
        LDER := LPeerCert.SaveToDER;
        Test('DER export', Length(LDER) > 0);
        WriteLn('  DER size: ', Length(LDER), ' bytes');

        // Test PEM export
        LPEM := LPeerCert.SaveToPEM;
        Test('PEM export', Length(LPEM) > 0);
        Test('PEM has BEGIN marker', Pos('-----BEGIN CERTIFICATE-----', LPEM) > 0);
        Test('PEM has END marker', Pos('-----END CERTIFICATE-----', LPEM) > 0);
        WriteLn('  PEM size: ', Length(LPEM), ' characters');

        // Show first few lines of PEM
        WriteLn('');
        WriteLn('PEM Output (first 200 chars):');
        WriteLn(Copy(LPEM, 1, 200), '...');
      end;

      LConn.Shutdown;
    end
    else
      Test('TLS handshake', False);

    CloseSocket(LSocket);
  end;

  LLib.Finalize;

  WriteLn('');
  WriteLn('========================================');
  WriteLn('PEM Export Test Summary');
  WriteLn('========================================');
  WriteLn('Total:  ', GTestCount);
  WriteLn('Passed: ', GPassCount);
  WriteLn('Failed: ', GTestCount - GPassCount);
  WriteLn('========================================');

  if GPassCount < GTestCount then
    Halt(1);
end.
