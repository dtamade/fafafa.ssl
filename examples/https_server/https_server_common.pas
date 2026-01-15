unit https_server_common;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}Windows, WinSock2{$ELSE}BaseUnix, UnixType, Sockets{$ENDIF},
  fafafa.ssl.base;

type
  {$IFDEF MSWINDOWS}
  TSocketHandle = TSocket;
  {$ELSE}
  TSocketHandle = cint;
  {$ENDIF}

const
  INVALID_SOCKET_HANDLE: TSocketHandle = {$IFDEF MSWINDOWS}INVALID_SOCKET{$ELSE}-1{$ENDIF};
  MAX_HTTP_REQUEST_SIZE = 65536;
  SERVER_BACKLOG = 16;

function InitNetwork(out AError: string): Boolean;
procedure CleanupNetwork;

function CreateListeningSocket(APort: Word; out AError: string): TSocketHandle;
function AcceptClient(AServer: TSocketHandle; out AError: string): TSocketHandle;
procedure CloseSocket(var ASocket: TSocketHandle);

function ReadHTTPRequest(AConnection: ISSLConnection; out ARequest: string): Boolean;
procedure SendHTTPResponse(AConnection: ISSLConnection; const ABody: string;
  const AContentType: string; const AStatus: string = '200 OK');
function ExtractRequestLine(const ARequest: string): string;
function ExtractPath(const ARequest: string): string;

implementation

function InitNetwork(out AError: string): Boolean;
{$IFDEF MSWINDOWS}
var
  WSAData: TWSAData;
begin
  Result := WSAStartup($0202, WSAData) = 0;
  if not Result then
    AError := SysErrorMessage(WSAGetLastError)
  else
    AError := '';
end;

procedure CleanupNetwork;
begin
  WSACleanup;
end;
{$ELSE}
begin
  AError := '';
  Result := True;
end;

procedure CleanupNetwork;
begin
  // no-op on Unix
end;
{$ENDIF}

function GetSocketErrorMessage: string;
{$IFDEF MSWINDOWS}
var
  Code: Integer;
begin
  Code := WSAGetLastError;
  Result := SysErrorMessage(Code);
end;
{$ELSE}
begin
  Result := SysErrorMessage(fpgeterrno);
end;
{$ENDIF}

function CreateListeningSocket(APort: Word; out AError: string): TSocketHandle;
var
  Addr: {$IFDEF MSWINDOWS}TSockAddrIn{$ELSE}TInetSockAddr{$ENDIF};
  Opt: Integer;
begin
  AError := '';
  {$IFDEF MSWINDOWS}
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET_HANDLE then
  begin
    AError := GetSocketErrorMessage;
    Exit;
  end;
  {$ELSE}
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result = INVALID_SOCKET_HANDLE then
  begin
    AError := GetSocketErrorMessage;
    Exit;
  end;
  {$ENDIF}

  Opt := 1;
  {$IFDEF MSWINDOWS}
  setsockopt(Result, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Opt), SizeOf(Opt));
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_addr.S_addr := INADDR_ANY;
  Addr.sin_port := htons(APort);
  if bind(Result, PSockAddr(@Addr)^, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    AError := GetSocketErrorMessage;
    CloseSocket(Result);
    Exit(INVALID_SOCKET_HANDLE);
  end;
  if listen(Result, SERVER_BACKLOG) = SOCKET_ERROR then
  begin
    AError := GetSocketErrorMessage;
    CloseSocket(Result);
    Exit(INVALID_SOCKET_HANDLE);
  end;
  {$ELSE}
  fpSetSockOpt(Result, SOL_SOCKET, SO_REUSEADDR, @Opt, SizeOf(Opt));
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Addr.sin_addr.s_addr := htonl(INADDR_ANY);
  if fpBind(Result, @Addr, SizeOf(Addr)) <> 0 then
  begin
    AError := GetSocketErrorMessage;
    CloseSocket(Result);
    Exit(INVALID_SOCKET_HANDLE);
  end;
  if fpListen(Result, SERVER_BACKLOG) <> 0 then
  begin
    AError := GetSocketErrorMessage;
    CloseSocket(Result);
    Exit(INVALID_SOCKET_HANDLE);
  end;
  {$ENDIF}
end;

function AcceptClient(AServer: TSocketHandle; out AError: string): TSocketHandle;
var
  Addr: {$IFDEF MSWINDOWS}TSockAddrIn{$ELSE}TInetSockAddr{$ENDIF};
  AddrLen: {$IFDEF MSWINDOWS}Integer{$ELSE}TSockLen{$ENDIF};
begin
  AError := '';
  AddrLen := SizeOf(Addr);
  {$IFDEF MSWINDOWS}
  Result := accept(AServer, PSockAddr(@Addr)^, AddrLen);
  if Result = INVALID_SOCKET_HANDLE then
    AError := GetSocketErrorMessage;
  {$ELSE}
  Result := fpAccept(AServer, @Addr, @AddrLen);
  if Result = INVALID_SOCKET_HANDLE then
    AError := GetSocketErrorMessage;
  {$ENDIF}
end;

procedure CloseSocket(var ASocket: TSocketHandle);
begin
  if ASocket = INVALID_SOCKET_HANDLE then Exit;
  {$IFDEF MSWINDOWS}
  closesocket(ASocket);
  {$ELSE}
  fpClose(ASocket);
  {$ENDIF}
  ASocket := INVALID_SOCKET_HANDLE;
end;

function ReadHTTPRequest(AConnection: ISSLConnection; out ARequest: string): Boolean;
const
  BUFFER_SIZE = 4096;
var
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  Stream: TMemoryStream;
  Temp: RawByteString;
begin
  Result := False;
  ARequest := '';
  Stream := TMemoryStream.Create;
  try
    repeat
      try
        BytesRead := AConnection.Read(Buffer[0], BUFFER_SIZE);
      except
        on E: Exception do
        begin
          if Stream.Size = 0 then
            Exit
          else
            Break;
        end;
      end;
      if BytesRead > 0 then
      begin
        Stream.WriteBuffer(Buffer[0], BytesRead);
        if Stream.Size >= MAX_HTTP_REQUEST_SIZE then
          Break;
        SetLength(Temp, Stream.Size);
        Move(Stream.Memory^, Temp[1], Stream.Size);
        if Pos(#13#10#13#10, Temp) > 0 then
          Break;
      end
      else
        Break;
    until False;

    if Stream.Size = 0 then
      Exit;

    SetLength(ARequest, Stream.Size);
    if Stream.Size > 0 then
      Move(Stream.Memory^, ARequest[1], Stream.Size);
    Result := True;
  finally
    Stream.Free;
  end;
end;

procedure SendHTTPResponse(AConnection: ISSLConnection; const ABody: string;
  const AContentType: string; const AStatus: string);
var
  Response: RawByteString;
begin
  Response := 'HTTP/1.1 ' + AStatus + #13#10 +
              'Content-Type: ' + AContentType + #13#10 +
              'Content-Length: ' + IntToStr(Length(ABody)) + #13#10 +
              'Connection: close' + #13#10 +
              #13#10 +
              ABody;
  if Length(Response) > 0 then
    AConnection.Write(Response[1], Length(Response));
end;

function ExtractRequestLine(const ARequest: string): string;
var
  PosEnd: SizeInt;
begin
  PosEnd := Pos(#13#10, ARequest);
  if PosEnd > 0 then
    Result := Copy(ARequest, 1, PosEnd - 1)
  else
    Result := Trim(ARequest);
end;

function ExtractPath(const ARequest: string): string;
var
  Line: string;
  FirstSpace, SecondSpace: SizeInt;
  Tail: string;
begin
  Result := '';
  Line := Trim(ExtractRequestLine(ARequest));
  if Line = '' then Exit;
  FirstSpace := Pos(' ', Line);
  if FirstSpace = 0 then Exit;
  Tail := Trim(Copy(Line, FirstSpace + 1, MaxInt));
  if Tail = '' then Exit;
  SecondSpace := Pos(' ', Tail);
  if SecondSpace = 0 then
    Result := Tail
  else
    Result := Copy(Tail, 1, SecondSpace - 1);
end;

end.
