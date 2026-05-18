unit fafafa.examples.tcp;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  WinSock2,
  {$ELSE}
  BaseUnix, Unix, Sockets, NetDB,
  {$ENDIF}
  Classes,
  fafafa.ssl.base;

type
  TSocketHandle = {$IFDEF MSWINDOWS}TSocket{$ELSE}TSocket{$ENDIF};

const
  // FPC's Sockets unit doesn't always provide INVALID_SOCKET; normalize it here.
  INVALID_SOCKET: TSocketHandle = {$IFDEF MSWINDOWS}WinSock2.INVALID_SOCKET{$ELSE}TSocketHandle(-1){$ENDIF};

function InitNetwork(out AError: string): Boolean;
procedure CleanupNetwork;

function ConnectTCP(const AHost: string; APort: Word): TSocketHandle;
function ListenTCP(APort: Word; const AAddress: string = '0.0.0.0'): TSocketHandle;
function AcceptConnection(AListenSocket: TSocketHandle): TSocketHandle;
procedure CloseSocket(var ASocket: TSocketHandle);
procedure GetCertificateVerificationInfo(AConnection: ISSLConnection;
  out AVerifyResult: Integer; out AVerifyResultString: string);

implementation

{$IFDEF MSWINDOWS}
function InitNetwork(out AError: string): Boolean;
var
  WSAData: TWSAData;
  Code: Integer;
begin
  Code := WSAStartup($0202, WSAData);
  Result := (Code = 0);
  if Result then
    AError := ''
  else
    AError := SysErrorMessage(Code);
end;

procedure CleanupNetwork;
begin
  WSACleanup;
end;
{$ELSE}
function InitNetwork(out AError: string): Boolean;
begin
  AError := '';
  Result := True;
end;

procedure CleanupNetwork;
begin
  // no-op on Unix
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ConnectTCP(const AHost: string; APort: Word): TSocketHandle;
var
  Addr: TSockAddr;
  HostEnt: PHostEnt;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
    raise Exception.Create('Unable to create socket');

  HostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
  if HostEnt = nil then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('Unable to resolve host: %s', [AHost]);
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Addr.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;

  if WinSock2.connect(Result, Addr, SizeOf(Addr)) <> 0 then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('Unable to connect to %s:%d', [AHost, APort]);
  end;
end;

function ListenTCP(APort: Word; const AAddress: string = '0.0.0.0'): TSocketHandle;
var
  Addr: TSockAddr;
  HostEnt: PHostEnt;
  OptVal: Integer;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
    raise Exception.Create('Unable to create listen socket');

  // Set SO_REUSEADDR
  OptVal := 1;
  if setsockopt(Result, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal)) <> 0 then
  begin
    closesocket(Result);
    raise Exception.Create('Unable to set SO_REUSEADDR');
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);

  if AAddress = '0.0.0.0' then
    Addr.sin_addr.S_addr := INADDR_ANY
  else
  begin
    HostEnt := gethostbyname(PAnsiChar(AnsiString(AAddress)));
    if HostEnt = nil then
    begin
      closesocket(Result);
      raise Exception.CreateFmt('Unable to resolve bind address: %s', [AAddress]);
    end;
    Addr.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;
  end;

  if bind(Result, Addr, SizeOf(Addr)) <> 0 then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('Unable to bind to %s:%d', [AAddress, APort]);
  end;

  if listen(Result, SOMAXCONN) <> 0 then
  begin
    closesocket(Result);
    raise Exception.CreateFmt('Unable to listen on port %d', [APort]);
  end;
end;

function AcceptConnection(AListenSocket: TSocketHandle): TSocketHandle;
var
  Addr: TSockAddr;
  AddrLen: Integer;
begin
  AddrLen := SizeOf(Addr);
  Result := accept(AListenSocket, @Addr, @AddrLen);
  if Result = INVALID_SOCKET then
    raise Exception.Create('Accept failed');
end;

procedure CloseSocket(var ASocket: TSocketHandle);
begin
  if ASocket = INVALID_SOCKET then Exit;
  closesocket(ASocket);
  ASocket := INVALID_SOCKET;
end;
{$ELSE}
function ConnectTCP(const AHost: string; APort: Word): TSocketHandle;
var
  Sock: cint;
  Addr: TInetSockAddr;
  HostEntry: THostEntry;
begin
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    raise Exception.Create('Unable to create socket');

  // NetDB's ResolveHostByName returns H.Addr in network byte order.
  if not ResolveHostByName(AHost, HostEntry) then
  begin
    fpClose(Sock);
    raise Exception.CreateFmt('Unable to resolve host: %s', [AHost]);
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Addr.sin_addr := HostEntry.Addr;

  if fpConnect(Sock, @Addr, SizeOf(Addr)) <> 0 then
  begin
    fpClose(Sock);
    raise Exception.CreateFmt('Unable to connect to %s:%d', [AHost, APort]);
  end;

  Result := Sock;
end;

function ListenTCP(APort: Word; const AAddress: string = '0.0.0.0'): TSocketHandle;
var
  Sock: cint;
  Addr: TInetSockAddr;
  HostEntry: THostEntry;
  OptVal: cint;
begin
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    raise Exception.Create('Unable to create listen socket');

  // Set SO_REUSEADDR
  OptVal := 1;
  if fpSetSockOpt(Sock, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal)) <> 0 then
  begin
    fpClose(Sock);
    raise Exception.Create('Unable to set SO_REUSEADDR');
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);

  if AAddress = '0.0.0.0' then
    Addr.sin_addr.s_addr := 0  // INADDR_ANY
  else
  begin
    if not ResolveHostByName(AAddress, HostEntry) then
    begin
      fpClose(Sock);
      raise Exception.CreateFmt('Unable to resolve bind address: %s', [AAddress]);
    end;
    Addr.sin_addr := HostEntry.Addr;
  end;

  if fpBind(Sock, @Addr, SizeOf(Addr)) <> 0 then
  begin
    fpClose(Sock);
    raise Exception.CreateFmt('Unable to bind to %s:%d', [AAddress, APort]);
  end;

  if fpListen(Sock, SOMAXCONN) <> 0 then
  begin
    fpClose(Sock);
    raise Exception.CreateFmt('Unable to listen on port %d', [APort]);
  end;

  Result := Sock;
end;

function AcceptConnection(AListenSocket: TSocketHandle): TSocketHandle;
var
  Addr: TInetSockAddr;
  AddrLen: TSockLen;
begin
  AddrLen := SizeOf(Addr);
  Result := fpAccept(AListenSocket, @Addr, @AddrLen);
  if Result < 0 then
    raise Exception.Create('Accept failed');
end;

procedure CloseSocket(var ASocket: TSocketHandle);
begin
  if ASocket = INVALID_SOCKET then Exit;
  fpClose(ASocket);
  ASocket := INVALID_SOCKET;
end;
{$ENDIF}

procedure GetCertificateVerificationInfo(AConnection: ISSLConnection;
  out AVerifyResult: Integer; out AVerifyResultString: string);
var
  LCertVerify: ISSLCertificateVerification;
begin
  AVerifyResult := -1;
  AVerifyResultString := 'Not verified';

  if AConnection = nil then
    Exit;

  if Supports(AConnection, ISSLCertificateVerification, LCertVerify) then
  begin
    AVerifyResult := LCertVerify.GetVerifyResult;
    AVerifyResultString := LCertVerify.GetVerifyResultString;
  end
  else
  begin
    {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
    AVerifyResult := AConnection.GetVerifyResult;
    AVerifyResultString := AConnection.GetVerifyResultString;
    {$POP}
  end;
end;

end.
