{**
 * Test: Certificate Chain Building
 * Purpose: Test certificate chain construction from peer certificates
 *}

program test_certificate_chain;

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
  LStore: ISSLCertificateStore;
  LChain: TSSLCertificateArray;
  I: Integer;

begin
  WriteLn('Certificate Chain Building Test');
  WriteLn('================================');
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
        WriteLn('');
        WriteLn('Peer Certificate Info:');
        WriteLn('  Subject: ', LPeerCert.GetSubject);
        WriteLn('  Issuer: ', LPeerCert.GetIssuer);
        WriteLn('  Self-signed: ', BoolToStr(LPeerCert.IsSelfSigned, 'Yes', 'No'));
        WriteLn('  Expired: ', BoolToStr(LPeerCert.IsExpired, 'Yes', 'No'));
        WriteLn('  Days until expiry: ', LPeerCert.GetDaysUntilExpiry);

        // Create certificate store and add peer cert
        LStore := TMbedTLSCertificateStore.Create;
        Test('Certificate store created', LStore <> nil);

        if LStore.AddCertificate(LPeerCert) then
        begin
          Test('Certificate added to store', True);
          Test('Store count is 1', LStore.GetCount = 1);

          // Build certificate chain
          LChain := LStore.BuildCertificateChain(LPeerCert);
          Test('Certificate chain built', Length(LChain) > 0);
          WriteLn('  Chain length: ', Length(LChain));

          // Display chain
          WriteLn('');
          WriteLn('Certificate Chain:');
          for I := 0 to High(LChain) do
          begin
            WriteLn('  [', I, '] Subject: ', LChain[I].GetSubject);
            WriteLn('      Issuer: ', LChain[I].GetIssuer);
          end;

          // Test find functions
          WriteLn('');
          WriteLn('Testing Find Functions:');
          Test('FindBySubject works', LStore.FindBySubject('invalid') <> nil);
        end
        else
          Test('Certificate added to store', False);
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
  WriteLn('Certificate Chain Test Summary');
  WriteLn('========================================');
  WriteLn('Total:  ', GTestCount);
  WriteLn('Passed: ', GPassCount);
  WriteLn('Failed: ', GTestCount - GPassCount);
  WriteLn('========================================');

  if GPassCount < GTestCount then
    Halt(1);
end.
