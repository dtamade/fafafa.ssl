{******************************************************************************}
{  FreePascal TLS 1.3 Backend - Multi-Server Compatibility Test               }
{                                                                              }
{  Connects to multiple real-world HTTPS servers to verify the pure Pascal     }
{  TLS 1.3 implementation handles diverse certificate chains, cipher           }
{  selections, and server configurations correctly.                            }
{******************************************************************************}

program test_freepascal_multiserver;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}BaseUnix, Unix,{$ENDIF}
  SysUtils, Classes, sockets,
  fafafa.ssl.base,
  fafafa.ssl.freepascal.lib,
  fafafa.ssl.freepascal.context;

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

var
  Lib: ISSLLibrary;
  Passed, Failed, Skipped: Integer;

function ConnectTCP(const AHost: string; APort: Word): TSocket;
var
  Addr: TInetSockAddr;
  HostEnt: PHostEnt;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then Exit;

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

procedure TestServer(const AHost: string; AVerify: Boolean);
var
  Ctx: ISSLContext;
  FPCtx: TFreePascalContext;
  Conn: ISSLConnection;
  Client: ISSLClientConnection;
  ConnInfo: ISSLConnectionInfo;
  Info: TSSLConnectionInfo;
  Sock: TSocket;
  Request: RawByteString;
  Buffer: array[0..4095] of Byte;
  N: Integer;
begin
  Write('  ', AHost, ' ... ');

  Sock := ConnectTCP(AHost, 443);
  if Sock < 0 then
  begin
    WriteLn('[SKIP] DNS/TCP failed');
    Inc(Skipped);
    Exit;
  end;

  Ctx := Lib.CreateContext(sslCtxClient);
  if AVerify then
  begin
    FPCtx := (Ctx as TObject) as TFreePascalContext;
    FPCtx.LoadSystemCertificates;
  end
  else
    Ctx.SetVerifyMode([sslVerifyNone]);

  Conn := Ctx.CreateConnection(THandle(Sock));
  if Supports(Conn, ISSLClientConnection, Client) then
    Client.SetServerName(AHost);

  if not Conn.Connect then
  begin
    WriteLn('[FAIL] handshake: ', Lib.GetLastErrorString);
    Inc(Failed);
    CloseSocket(Sock);
    Exit;
  end;

  if Supports(Conn, ISSLConnectionInfo, ConnInfo) then
  begin
    Info := ConnInfo.GetConnectionInfo;
    Write('[OK] ', Info.CipherSuite);
    if Info.PeerCertificate.Subject <> '' then
      Write(' | ', Info.PeerCertificate.Subject);
  end
  else
    Write('[OK]');

  Request := 'GET / HTTP/1.0'#13#10'Host: ' + AHost + #13#10#13#10;
  Conn.Write(Request[1], Length(Request));

  N := Conn.Read(Buffer[0], SizeOf(Buffer) - 1);
  if N > 0 then
    Write(' | ', N, 'B')
  else
    Write(' | no-data');

  WriteLn;
  Inc(Passed);
  Conn.Shutdown;
  CloseSocket(Sock);
end;

begin
  WriteLn('==============================================');
  WriteLn('FreePascal TLS 1.3 Multi-Server Compatibility');
  WriteLn('==============================================');
  WriteLn;

  Lib := CreateFreePascalSSLLibrary;
  if not Lib.Initialize then
  begin
    WriteLn('FATAL: FreePascal SSL init failed');
    Halt(1);
  end;

  Passed := 0;
  Failed := 0;
  Skipped := 0;

  WriteLn('--- Without certificate verification ---');
  TestServer('www.google.com', False);
  TestServer('www.cloudflare.com', False);
  TestServer('github.com', False);
  TestServer('www.example.com', False);
  TestServer('one.one.one.one', False);

  WriteLn;
  WriteLn('--- With certificate verification (system CA) ---');
  TestServer('www.google.com', True);
  TestServer('www.cloudflare.com', True);
  TestServer('www.example.com', True);

  Lib.Finalize;

  WriteLn;
  WriteLn('==============================================');
  WriteLn(Format('Results: %d passed, %d failed, %d skipped', [Passed, Failed, Skipped]));
  WriteLn('==============================================');

  if Failed > 0 then
    Halt(Failed);
end.
