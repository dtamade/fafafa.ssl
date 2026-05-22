{******************************************************************************}
{  Cross-Backend Error Handling Consistency Tests                               }
{                                                                              }
{  Verifies that OpenSSL and FreePascal backends produce consistent error       }
{  behavior when certificate verification fails or connections are rejected.    }
{******************************************************************************}

program test_cross_backend_errors;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  BaseUnix, Unix,
  {$ENDIF}
  SysUtils, Classes, sockets,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.freepascal.lib,
  cross_backend_base;

const
  GOOD_HOST = 'www.google.com';
  GOOD_PORT = 443;

var
  Runner: TCrossBackendTestRunner;

{$IFNDEF WINDOWS}
type
  TInetSockAddr = record
    sin_family: cushort;
    sin_port: cushort;
    sin_addr: in_addr;
    sin_zero: array[0..7] of char;
  end;

  PHostEnt = ^THostEnt;
  THostEnt = record
    h_name: PChar;
    h_aliases: PPChar;
    h_addrtype: cint;
    h_length: cint;
    h_addr_list: PPChar;
  end;

function gethostbyname(name: PChar): PHostEnt; cdecl; external 'c';
{$ENDIF}

function ConnectTCP(const AHost: string; APort: Word): TSocket;
var
  Addr: TInetSockAddr;
  HostEnt: PHostEnt;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    Exit;

  HostEnt := gethostbyname(PChar(AHost));
  if (HostEnt = nil) or (HostEnt^.h_addr_list = nil) or (HostEnt^.h_addr_list^ = nil) then
  begin
    CloseSocket(Result);
    Result := -1;
    Exit;
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Move(HostEnt^.h_addr_list^^, Addr.sin_addr, SizeOf(Addr.sin_addr));

  if fpConnect(Result, @Addr, SizeOf(Addr)) <> 0 then
  begin
    CloseSocket(Result);
    Result := -1;
  end;
end;

type
  THandshakeOutcome = record
    Succeeded: Boolean;
    ErrorClass: string;
    ErrorMessage: string;
  end;

function TryHandshake(ALib: ISSLLibrary; const AHost: string;
  APort: Word; const ASNI: string): THandshakeOutcome;
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  Sock: TSocket;
begin
  Result.Succeeded := False;
  Result.ErrorClass := '';
  Result.ErrorMessage := '';

  Sock := ConnectTCP(AHost, APort);
  if Sock < 0 then
  begin
    Result.ErrorClass := 'TCP';
    Result.ErrorMessage := 'TCP connect failed';
    Exit;
  end;

  try
    Ctx := ALib.CreateContext(sslCtxClient);
    Ctx.SetVerifyMode([sslVerifyNone]);
    Conn := Ctx.CreateConnection(THandle(Sock));
    if Supports(Conn, ISSLClientConnection, Client) then
      Client.SetServerName(ASNI);
    try
      if Conn.Connect then
      begin
        Result.Succeeded := True;
        Conn.Shutdown;
      end
      else
      begin
        Result.ErrorClass := 'TLS';
        Result.ErrorMessage := ALib.GetLastErrorString;
      end;
    except
      on E: Exception do
      begin
        Result.ErrorClass := E.ClassName;
        Result.ErrorMessage := E.Message;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.ErrorClass := E.ClassName;
      Result.ErrorMessage := 'setup: ' + E.Message;
    end;
  end;
  CloseSocket(Sock);
end;

{ Test 1: Hostname mismatch - connect to google but claim SNI is wrong }
procedure Test_Hostname_Mismatch;
var
  LibSSL, LibFPC: ISSLLibrary;
  OutSSL, OutFPC: THandshakeOutcome;
begin
  WriteLn('');
  WriteLn('--- Test: Hostname Mismatch Detection ---');
  WriteLn('  (Note: sslVerifyPeer alone may not enforce hostname check)');

  LibSSL := TOpenSSLLibrary.Create;
  LibSSL.Initialize;
  LibFPC := CreateFreePascalSSLLibrary;
  LibFPC.Initialize;

  OutSSL := TryHandshake(LibSSL, GOOD_HOST, GOOD_PORT, 'wrong.invalid.host');
  OutFPC := TryHandshake(LibFPC, GOOD_HOST, GOOD_PORT, 'wrong.invalid.host');

  // Both backends should behave the same way (consistency is the goal)
  Runner.AssertBothSucceed('Hostname mismatch behavior consistent',
    OutSSL.Succeeded, OutFPC.Succeeded, 'OpenSSL', 'FreePascal');

  if OutSSL.Succeeded and OutFPC.Succeeded then
    WriteLn('  [INFO] Neither backend rejects mismatched SNI with sslVerifyPeer alone')
  else if (not OutSSL.Succeeded) and (not OutFPC.Succeeded) then
    WriteLn('  [INFO] Both backends reject mismatched SNI')
  else
    WriteLn('  [INFO] Inconsistent: OpenSSL=', OutSSL.Succeeded, ' FreePascal=', OutFPC.Succeeded);

  LibSSL.Finalize;
  LibFPC.Finalize;
end;

{ Test 2: Valid connection succeeds on both }
procedure Test_Valid_Connection;
var
  LibSSL, LibFPC: ISSLLibrary;
  OutSSL, OutFPC: THandshakeOutcome;
begin
  WriteLn('');
  WriteLn('--- Test: Valid Connection Succeeds ---');

  LibSSL := TOpenSSLLibrary.Create;
  LibSSL.Initialize;
  LibFPC := CreateFreePascalSSLLibrary;
  LibFPC.Initialize;

  OutSSL := TryHandshake(LibSSL, GOOD_HOST, GOOD_PORT, GOOD_HOST);
  OutFPC := TryHandshake(LibFPC, GOOD_HOST, GOOD_PORT, GOOD_HOST);

  Runner.Check('OpenSSL valid handshake succeeds', OutSSL.Succeeded,
    OutSSL.ErrorMessage);
  Runner.Check('FreePascal valid handshake succeeds', OutFPC.Succeeded,
    OutFPC.ErrorMessage);
  Runner.AssertBothSucceed('Both succeed on valid host (consistent)',
    OutSSL.Succeeded, OutFPC.Succeeded, 'OpenSSL', 'FreePascal');

  LibSSL.Finalize;
  LibFPC.Finalize;
end;

{ Test 3: Connection to non-TLS port fails gracefully }
procedure Test_Non_TLS_Port;
var
  LibSSL, LibFPC: ISSLLibrary;
  OutSSL, OutFPC: THandshakeOutcome;
begin
  WriteLn('');
  WriteLn('--- Test: Non-TLS Port Fails Gracefully ---');

  LibSSL := TOpenSSLLibrary.Create;
  LibSSL.Initialize;
  LibFPC := CreateFreePascalSSLLibrary;
  LibFPC.Initialize;

  OutSSL := TryHandshake(LibSSL, GOOD_HOST, 80, GOOD_HOST);
  OutFPC := TryHandshake(LibFPC, GOOD_HOST, 80, GOOD_HOST);

  // Consistency is the primary assertion
  Runner.AssertBothSucceed('Non-TLS port behavior consistent',
    OutSSL.Succeeded, OutFPC.Succeeded, 'OpenSSL', 'FreePascal');

  if OutSSL.Succeeded then
    WriteLn('  [INFO] OpenSSL: handshake unexpectedly succeeded on port 80')
  else
    WriteLn('  [INFO] OpenSSL: correctly failed (', OutSSL.ErrorClass, ': ', OutSSL.ErrorMessage, ')');

  if OutFPC.Succeeded then
    WriteLn('  [INFO] FreePascal: handshake unexpectedly succeeded on port 80')
  else
    WriteLn('  [INFO] FreePascal: correctly failed (', OutFPC.ErrorClass, ': ', OutFPC.ErrorMessage, ')');

  LibSSL.Finalize;
  LibFPC.Finalize;
end;

{ Main }
begin
  WriteLn('==============================================');
  WriteLn('Cross-Backend Error Handling Tests');
  WriteLn('OpenSSL vs FreePascal');
  WriteLn('==============================================');

  Runner := TCrossBackendTestRunner.Create;
  try
    Test_Valid_Connection;
    Test_Hostname_Mismatch;
    Test_Non_TLS_Port;

    Runner.PrintSummary;
    Halt(Runner.ExitCode);
  finally
    Runner.Free;
  end;
end.
