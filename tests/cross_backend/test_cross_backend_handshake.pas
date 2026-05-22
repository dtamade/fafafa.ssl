{******************************************************************************}
{  Cross-Backend Capability and Handshake Consistency Tests                    }
{                                                                              }
{  Verifies that OpenSSL and FreePascal backends produce consistent behavior   }
{  for core TLS operations: initialization, capability reporting, and          }
{  TLS 1.3 handshake against a real server.                                    }
{******************************************************************************}

program test_cross_backend_handshake;

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
  TEST_HOST = 'www.google.com';
  TEST_PORT = 443;

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

{ Test 1: Both backends initialize successfully }
procedure Test_Initialization;
var
  LibSSL, LibFPC: ISSLLibrary;
  InitSSL, InitFPC: Boolean;
begin
  WriteLn('');
  WriteLn('--- Test: Backend Initialization ---');

  LibSSL := TOpenSSLLibrary.Create;
  InitSSL := LibSSL.Initialize;
  Runner.Check('OpenSSL initializes', InitSSL);

  LibFPC := CreateFreePascalSSLLibrary;
  InitFPC := LibFPC.Initialize;
  Runner.Check('FreePascal initializes', InitFPC);

  Runner.AssertBothSucceed('Both backends initialize',
    InitSSL, InitFPC, 'OpenSSL', 'FreePascal');

  if InitSSL then LibSSL.Finalize;
  if InitFPC then LibFPC.Finalize;
end;

{ Test 2: TLS 1.3 support consistency }
procedure Test_TLS13_Support;
var
  LibSSL, LibFPC: ISSLLibrary;
  CapSSL, CapFPC: TSSLBackendCapabilities;
begin
  WriteLn('');
  WriteLn('--- Test: TLS 1.3 Capability Consistency ---');

  LibSSL := TOpenSSLLibrary.Create;
  LibSSL.Initialize;
  LibFPC := CreateFreePascalSSLLibrary;
  LibFPC.Initialize;

  CapSSL := LibSSL.GetCapabilities;
  CapFPC := LibFPC.GetCapabilities;

  Runner.Check('OpenSSL supports TLS 1.3',
    LibSSL.IsProtocolSupported(sslProtocolTLS13));
  Runner.Check('FreePascal supports TLS 1.3',
    LibFPC.IsProtocolSupported(sslProtocolTLS13));

  Runner.Check('Both support AES-128-GCM',
    LibSSL.IsCipherSupported('TLS_AES_128_GCM_SHA256') and
    LibFPC.IsCipherSupported('TLS_AES_128_GCM_SHA256'));

  Runner.Check('Both support AES-256-GCM',
    LibSSL.IsCipherSupported('TLS_AES_256_GCM_SHA384') and
    LibFPC.IsCipherSupported('TLS_AES_256_GCM_SHA384'));

  Runner.Check('Both support CHACHA20-POLY1305',
    LibSSL.IsCipherSupported('TLS_CHACHA20_POLY1305_SHA256') and
    LibFPC.IsCipherSupported('TLS_CHACHA20_POLY1305_SHA256'));

  Runner.Check('Both report session ticket support',
    (CapSSL.SessionTicketsSupport >= sslSupportStable) and
    (CapFPC.SessionTicketsSupport >= sslSupportExperimental));

  LibSSL.Finalize;
  LibFPC.Finalize;
end;

{ Test 3: Both backends can complete TLS 1.3 handshake to a real server }
procedure Test_Handshake_Consistency;
var
  LibSSL, LibFPC: ISSLLibrary;
  CtxSSL, CtxFPC: ISSLContext;
  ConnSSL, ConnFPC: ISSLConnection;
  ClientSSL, ClientFPC: ISSLClientConnection;
  SockSSL, SockFPC: TSocket;
  HandshakeSSL, HandshakeFPC: Boolean;
begin
  WriteLn('');
  WriteLn('--- Test: TLS 1.3 Handshake Consistency (', TEST_HOST, ') ---');

  LibSSL := TOpenSSLLibrary.Create;
  if not LibSSL.Initialize then
  begin
    Runner.Skip('OpenSSL handshake', 'OpenSSL init failed');
    Exit;
  end;

  LibFPC := CreateFreePascalSSLLibrary;
  if not LibFPC.Initialize then
  begin
    Runner.Skip('FreePascal handshake', 'FreePascal init failed');
    LibSSL.Finalize;
    Exit;
  end;

  HandshakeSSL := False;
  HandshakeFPC := False;

  // OpenSSL handshake
  SockSSL := ConnectTCP(TEST_HOST, TEST_PORT);
  if SockSSL < 0 then
  begin
    Runner.Skip('OpenSSL handshake', 'TCP connect failed');
  end
  else
  begin
    try
      CtxSSL := LibSSL.CreateContext(sslCtxClient);
      CtxSSL.SetVerifyMode([sslVerifyNone]);
      ConnSSL := CtxSSL.CreateConnection(THandle(SockSSL));
      if Supports(ConnSSL, ISSLClientConnection, ClientSSL) then
        ClientSSL.SetServerName(TEST_HOST);
      try
        HandshakeSSL := ConnSSL.Connect;
        Runner.Check('OpenSSL handshake succeeds', HandshakeSSL,
          LibSSL.GetLastErrorString);
      except
        on E: Exception do
          Runner.Check('OpenSSL handshake succeeds', False, E.Message);
      end;
      if HandshakeSSL then
        ConnSSL.Shutdown;
    except
      on E: Exception do
        Runner.Check('OpenSSL handshake succeeds', False, 'setup: ' + E.Message);
    end;
    CloseSocket(SockSSL);
  end;

  // FreePascal handshake
  SockFPC := ConnectTCP(TEST_HOST, TEST_PORT);
  if SockFPC < 0 then
  begin
    Runner.Skip('FreePascal handshake', 'TCP connect failed');
  end
  else
  begin
    try
      CtxFPC := LibFPC.CreateContext(sslCtxClient);
      CtxFPC.SetVerifyMode([sslVerifyNone]);
      ConnFPC := CtxFPC.CreateConnection(THandle(SockFPC));
      if Supports(ConnFPC, ISSLClientConnection, ClientFPC) then
        ClientFPC.SetServerName(TEST_HOST);
      try
        HandshakeFPC := ConnFPC.Connect;
        Runner.Check('FreePascal handshake succeeds', HandshakeFPC,
          LibFPC.GetLastErrorString);
      except
        on E: Exception do
          Runner.Check('FreePascal handshake succeeds', False, E.Message);
      end;
      if HandshakeFPC then
        ConnFPC.Shutdown;
    except
      on E: Exception do
        Runner.Check('FreePascal handshake succeeds', False, 'setup: ' + E.Message);
    end;
    CloseSocket(SockFPC);
  end;

  Runner.AssertBothSucceed('Handshake outcome consistent',
    HandshakeSSL, HandshakeFPC, 'OpenSSL', 'FreePascal');

  LibSSL.Finalize;
  LibFPC.Finalize;
end;

{ Test 4: Protocol version negotiated consistently }
procedure Test_Protocol_Version_Consistency;
var
  LibSSL, LibFPC: ISSLLibrary;
  CtxSSL, CtxFPC: ISSLContext;
  ConnSSL, ConnFPC: ISSLConnection;
  ClientSSL, ClientFPC: ISSLClientConnection;
  InfoSSL, InfoFPC: ISSLConnectionInfo;
  SockSSL, SockFPC: TSocket;
  ProtoSSL, ProtoFPC: TSSLProtocolVersion;
  GotSSL, GotFPC: Boolean;
begin
  WriteLn('');
  WriteLn('--- Test: Negotiated Protocol Version Consistency ---');

  LibSSL := TOpenSSLLibrary.Create;
  LibSSL.Initialize;
  LibFPC := CreateFreePascalSSLLibrary;
  LibFPC.Initialize;

  ProtoSSL := sslProtocolUnknown;
  ProtoFPC := sslProtocolUnknown;
  GotSSL := False;
  GotFPC := False;

  // OpenSSL
  SockSSL := ConnectTCP(TEST_HOST, TEST_PORT);
  if SockSSL >= 0 then
  begin
    try
      CtxSSL := LibSSL.CreateContext(sslCtxClient);
      CtxSSL.SetVerifyMode([sslVerifyNone]);
      ConnSSL := CtxSSL.CreateConnection(THandle(SockSSL));
      if Supports(ConnSSL, ISSLClientConnection, ClientSSL) then
        ClientSSL.SetServerName(TEST_HOST);
      if ConnSSL.Connect then
      begin
        if Supports(ConnSSL, ISSLConnectionInfo, InfoSSL) then
        begin
          ProtoSSL := InfoSSL.GetConnectionInfo.ProtocolVersion;
          GotSSL := True;
        end;
        ConnSSL.Shutdown;
      end;
    except
      on E: Exception do
        Runner.Skip('OpenSSL protocol version', E.Message);
    end;
    CloseSocket(SockSSL);
  end;

  // FreePascal
  SockFPC := ConnectTCP(TEST_HOST, TEST_PORT);
  if SockFPC >= 0 then
  begin
    try
      CtxFPC := LibFPC.CreateContext(sslCtxClient);
      CtxFPC.SetVerifyMode([sslVerifyNone]);
      ConnFPC := CtxFPC.CreateConnection(THandle(SockFPC));
      if Supports(ConnFPC, ISSLClientConnection, ClientFPC) then
        ClientFPC.SetServerName(TEST_HOST);
      if ConnFPC.Connect then
      begin
        if Supports(ConnFPC, ISSLConnectionInfo, InfoFPC) then
        begin
          ProtoFPC := InfoFPC.GetConnectionInfo.ProtocolVersion;
          GotFPC := True;
        end;
        ConnFPC.Shutdown;
      end;
    except
      on E: Exception do
        Runner.Skip('FreePascal protocol version', E.Message);
    end;
    CloseSocket(SockFPC);
  end;

  if GotSSL and GotFPC then
  begin
    Runner.Check('OpenSSL negotiates TLS 1.3', ProtoSSL = sslProtocolTLS13,
      'got ord=' + IntToStr(Ord(ProtoSSL)));
    Runner.Check('FreePascal negotiates TLS 1.3', ProtoFPC = sslProtocolTLS13,
      'got ord=' + IntToStr(Ord(ProtoFPC)));
    Runner.Check('Same protocol version', ProtoSSL = ProtoFPC,
      Format('OpenSSL=%d, FreePascal=%d', [Ord(ProtoSSL), Ord(ProtoFPC)]));
  end
  else
    Runner.Skip('Protocol version comparison', 'one or both handshakes failed');

  LibSSL.Finalize;
  LibFPC.Finalize;
end;

{ Main }
begin
  WriteLn('==============================================');
  WriteLn('Cross-Backend Consistency Tests');
  WriteLn('OpenSSL vs FreePascal');
  WriteLn('==============================================');

  Runner := TCrossBackendTestRunner.Create;
  try
    Test_Initialization;
    Test_TLS13_Support;
    Test_Handshake_Consistency;
    Test_Protocol_Version_Consistency;

    Runner.PrintSummary;
    Halt(Runner.ExitCode);
  finally
    Runner.Free;
  end;
end.
