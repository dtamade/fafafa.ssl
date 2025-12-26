{******************************************************************************}
{  fafafa.ssl.pool - SSL Connection Pool                                       }
{                                                                              }
{  P2 Performance Improvement: Connection pooling to reduce TLS handshake      }
{  overhead for repeated connections to the same host.                         }
{                                                                              }
{  Features:                                                                   }
{    - Configurable pool size per host                                         }
{    - Idle connection timeout                                                 }
{    - Thread-safe connection management                                       }
{    - Automatic connection health check                                       }
{******************************************************************************}

unit fafafa.ssl.pool;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, SyncObjs, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.logging;

type
  { Pool configuration }
  TSSLPoolConfig = record
    MaxPoolSize: Integer;        // Max connections per host (default: 10)
    IdleTimeoutSec: Integer;     // Idle timeout in seconds (default: 300)
    ConnectTimeoutMs: Integer;   // Connect timeout in ms (default: 30000)
    HealthCheckIntervalSec: Integer;  // Health check interval (default: 60)

    class function Default: TSSLPoolConfig; static;
  end;

  { Pooled connection wrapper }
  TPooledConnection = class
  private
    FConnection: ISSLConnection;
    FContext: ISSLContext;
    FHostKey: string;
    FCreatedAt: TDateTime;
    FLastUsedAt: TDateTime;
    FInUse: Boolean;
    FSocket: THandle;
  public
    constructor Create(AConnection: ISSLConnection; AContext: ISSLContext;
      const AHostKey: string; ASocket: THandle);

    property Connection: ISSLConnection read FConnection;
    property Context: ISSLContext read FContext;
    property HostKey: string read FHostKey;
    property CreatedAt: TDateTime read FCreatedAt;
    property LastUsedAt: TDateTime read FLastUsedAt write FLastUsedAt;
    property InUse: Boolean read FInUse write FInUse;
    property Socket: THandle read FSocket;

    function IsExpired(IdleTimeoutSec: Integer): Boolean;
    function IsHealthy: Boolean;
  end;

  { Connection pool interface }
  ISSLConnectionPool = interface
    ['{F8A1B2C3-D4E5-6789-ABCD-EF0123456789}']

    { Acquire a connection from pool or create new one }
    function Acquire(AContext: ISSLContext; const AHost: string; APort: Word;
      out AConnection: ISSLConnection): Boolean;

    { Release connection back to pool }
    procedure Release(AConnection: ISSLConnection);

    { Close specific connection (don't return to pool) }
    procedure CloseConnection(AConnection: ISSLConnection);

    { Clear all pooled connections for a host }
    procedure ClearHost(const AHost: string; APort: Word);

    { Clear all pooled connections }
    procedure ClearAll;

    { Get pool statistics }
    function GetTotalConnections: Integer;
    function GetActiveConnections: Integer;
    function GetIdleConnections: Integer;

    { Configuration }
    procedure SetConfig(const AConfig: TSSLPoolConfig);
    function GetConfig: TSSLPoolConfig;
  end;

  { Connection pool implementation }
  TSSLConnectionPool = class(TInterfacedObject, ISSLConnectionPool)
  private
    FConfig: TSSLPoolConfig;
    FConnections: TThreadList;
    FLock: TCriticalSection;
    FCleanupTimer: TThread;
    FShutdown: Boolean;

    function MakeHostKey(const AHost: string; APort: Word): string;
    function FindIdleConnection(const AHostKey: string): TPooledConnection;
    function CreateNewConnection(AContext: ISSLContext; const AHost: string;
      APort: Word; const AHostKey: string): TPooledConnection;
    procedure RemoveConnection(APooled: TPooledConnection);
    procedure CleanupExpiredConnections;
    function ConnectSocket(const AHost: string; APort: Word): THandle;
    procedure CloseSocket(ASocket: THandle);
  public
    constructor Create; overload;
    constructor Create(const AConfig: TSSLPoolConfig); overload;
    destructor Destroy; override;

    { ISSLConnectionPool }
    function Acquire(AContext: ISSLContext; const AHost: string; APort: Word;
      out AConnection: ISSLConnection): Boolean;
    procedure Release(AConnection: ISSLConnection);
    procedure CloseConnection(AConnection: ISSLConnection);
    procedure ClearHost(const AHost: string; APort: Word);
    procedure ClearAll;
    function GetTotalConnections: Integer;
    function GetActiveConnections: Integer;
    function GetIdleConnections: Integer;
    procedure SetConfig(const AConfig: TSSLPoolConfig);
    function GetConfig: TSSLPoolConfig;
  end;

{ Global connection pool singleton }
function GlobalConnectionPool: ISSLConnectionPool;

implementation

uses
  {$IFDEF WINDOWS}
  WinSock2
  {$ELSE}
  Sockets, BaseUnix
  {$ENDIF};

{$IFNDEF WINDOWS}
type
  PHostEntC = ^THostEntC;
  THostEntC = record
    h_name: PChar;
    h_aliases: PPChar;
    h_addrtype: cint;
    h_length: cint;
    h_addr_list: PPChar;
  end;

  TInetSockAddrC = record
    sin_family: cushort;
    sin_port: cushort;
    sin_addr: in_addr;
    sin_zero: array[0..7] of char;
  end;

function gethostbyname_c(name: PChar): PHostEntC; cdecl; external 'c' name 'gethostbyname';
{$ENDIF}

var
  GConnectionPool: ISSLConnectionPool = nil;
  GPoolLock: TCriticalSection = nil;

{ TSSLPoolConfig }

class function TSSLPoolConfig.Default: TSSLPoolConfig;
begin
  Result.MaxPoolSize := 10;
  Result.IdleTimeoutSec := 300;
  Result.ConnectTimeoutMs := 30000;
  Result.HealthCheckIntervalSec := 60;
end;

{ TPooledConnection }

constructor TPooledConnection.Create(AConnection: ISSLConnection;
  AContext: ISSLContext; const AHostKey: string; ASocket: THandle);
begin
  inherited Create;
  FConnection := AConnection;
  FContext := AContext;
  FHostKey := AHostKey;
  FSocket := ASocket;
  FCreatedAt := Now;
  FLastUsedAt := Now;
  FInUse := True;
end;

function TPooledConnection.IsExpired(IdleTimeoutSec: Integer): Boolean;
begin
  Result := SecondsBetween(Now, FLastUsedAt) > IdleTimeoutSec;
end;

function TPooledConnection.IsHealthy: Boolean;
begin
  // Basic health check: connection should still be valid
  Result := (FConnection <> nil) and FConnection.IsConnected;
end;

{ Cleanup thread }
type
  TPoolCleanupThread = class(TThread)
  private
    FPool: TSSLConnectionPool;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TSSLConnectionPool);
  end;

constructor TPoolCleanupThread.Create(APool: TSSLConnectionPool);
begin
  inherited Create(True);
  FPool := APool;
  FreeOnTerminate := False;
end;

procedure TPoolCleanupThread.Execute;
var
  WaitSec: Integer;
begin
  while not Terminated do
  begin
    WaitSec := FPool.FConfig.HealthCheckIntervalSec;
    if WaitSec < 10 then
      WaitSec := 10;

    Sleep(WaitSec * 1000);

    if not Terminated then
      FPool.CleanupExpiredConnections;
  end;
end;

{ TSSLConnectionPool }

constructor TSSLConnectionPool.Create;
begin
  Create(TSSLPoolConfig.Default);
end;

constructor TSSLConnectionPool.Create(const AConfig: TSSLPoolConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FConnections := TThreadList.Create;
  FLock := TCriticalSection.Create;
  FShutdown := False;

  // Start cleanup thread
  FCleanupTimer := TPoolCleanupThread.Create(Self);
  FCleanupTimer.Start;

  TSecurityLog.Info('ConnectionPool', 'Connection pool initialized');
end;

destructor TSSLConnectionPool.Destroy;
begin
  FShutdown := True;

  // Stop cleanup thread
  if FCleanupTimer <> nil then
  begin
    FCleanupTimer.Terminate;
    FCleanupTimer.WaitFor;
    FCleanupTimer.Free;
  end;

  // Clear all connections
  ClearAll;

  FConnections.Free;
  FLock.Free;

  TSecurityLog.Info('ConnectionPool', 'Connection pool destroyed');
  inherited;
end;

function TSSLConnectionPool.MakeHostKey(const AHost: string; APort: Word): string;
begin
  Result := LowerCase(AHost) + ':' + IntToStr(APort);
end;

function TSSLConnectionPool.FindIdleConnection(const AHostKey: string): TPooledConnection;
var
  List: TList;
  I: Integer;
  Pooled: TPooledConnection;
  ToRemove: TList;
begin
  Result := nil;
  ToRemove := TList.Create;
  try
    List := FConnections.LockList;
    try
      for I := 0 to List.Count - 1 do
      begin
        Pooled := TPooledConnection(List[I]);
        if (Pooled.HostKey = AHostKey) and (not Pooled.InUse) then
        begin
          // Check if connection is still healthy
          if Pooled.IsHealthy and not Pooled.IsExpired(FConfig.IdleTimeoutSec) then
          begin
            Pooled.InUse := True;
            Pooled.LastUsedAt := Now;
            Result := Pooled;
            TSecurityLog.Info('ConnectionPool',
              Format('Reusing pooled connection for %s', [AHostKey]));
            Break;
          end
          else
          begin
            // Mark for removal
            ToRemove.Add(Pooled);
          end;
        end;
      end;

      // Remove unhealthy/expired connections
      for I := 0 to ToRemove.Count - 1 do
      begin
        Pooled := TPooledConnection(ToRemove[I]);
        List.Remove(Pooled);
        if Pooled.Connection <> nil then
          Pooled.Connection.Close;
        CloseSocket(Pooled.Socket);
        Pooled.Free;
      end;
    finally
      FConnections.UnlockList;
    end;
  finally
    ToRemove.Free;
  end;
end;

function TSSLConnectionPool.ConnectSocket(const AHost: string; APort: Word): THandle;
{$IFDEF WINDOWS}
var
  WSAData: TWSAData;
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  InAddr: TInAddr;
  Timeout: Integer;
begin
  Result := INVALID_HANDLE_VALUE;

  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
    Exit;

  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Result = INVALID_SOCKET then
  begin
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;

  // Set timeout
  Timeout := FConfig.ConnectTimeoutMs;
  setsockopt(Result, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  setsockopt(Result, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));

  // Resolve hostname
  HostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
  if HostEnt = nil then
  begin
    closesocket(Result);
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Move(HostEnt^.h_addr_list^^, InAddr, SizeOf(InAddr));
  Addr.sin_addr := InAddr;

  if connect(Result, @Addr, SizeOf(Addr)) <> 0 then
  begin
    closesocket(Result);
    Result := INVALID_HANDLE_VALUE;
  end;
end;
{$ELSE}
var
  Addr: TInetSockAddrC;
  HE: PHostEntC;
begin
  Result := THandle(-1);

  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if cint(Result) < 0 then
  begin
    Result := THandle(-1);
    Exit;
  end;

  // Resolve hostname using gethostbyname
  HE := gethostbyname_c(PChar(AHost));
  if HE = nil then
  begin
    fpClose(cint(Result));
    Result := THandle(-1);
    Exit;
  end;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(APort);
  Move(HE^.h_addr_list^^, Addr.sin_addr, SizeOf(Addr.sin_addr));

  if fpConnect(cint(Result), @Addr, SizeOf(Addr)) <> 0 then
  begin
    fpClose(cint(Result));
    Result := THandle(-1);
  end;
end;
{$ENDIF}

procedure TSSLConnectionPool.CloseSocket(ASocket: THandle);
begin
  {$IFDEF WINDOWS}
  if ASocket <> INVALID_HANDLE_VALUE then
    closesocket(ASocket);
  {$ELSE}
  if ASocket <> THandle(-1) then
    fpClose(cint(ASocket));
  {$ENDIF}
end;

function TSSLConnectionPool.CreateNewConnection(AContext: ISSLContext;
  const AHost: string; APort: Word; const AHostKey: string): TPooledConnection;
var
  Socket: THandle;
  Conn: ISSLConnection;
begin
  Result := nil;

  // Connect TCP socket
  Socket := ConnectSocket(AHost, APort);
  {$IFDEF WINDOWS}
  if Socket = INVALID_HANDLE_VALUE then
  {$ELSE}
  if Socket = THandle(-1) then
  {$ENDIF}
  begin
    TSecurityLog.Warning('ConnectionPool',
      Format('Failed to connect to %s', [AHostKey]));
    Exit;
  end;

  try
    // Create SSL connection
    Conn := AContext.CreateConnection(Socket);
    if Conn = nil then
    begin
      CloseSocket(Socket);
      Exit;
    end;

    // Perform TLS handshake
    if not Conn.Connect then
    begin
      CloseSocket(Socket);
      Exit;
    end;

    // Create pooled wrapper
    Result := TPooledConnection.Create(Conn, AContext, AHostKey, Socket);

    TSecurityLog.Info('ConnectionPool',
      Format('Created new connection to %s', [AHostKey]));
  except
    on E: Exception do
    begin
      CloseSocket(Socket);
      TSecurityLog.Error('ConnectionPool',
        Format('Error creating connection to %s: %s', [AHostKey, E.Message]));
    end;
  end;
end;

function TSSLConnectionPool.Acquire(AContext: ISSLContext; const AHost: string;
  APort: Word; out AConnection: ISSLConnection): Boolean;
var
  HostKey: string;
  Pooled: TPooledConnection;
  List: TList;
  HostCount: Integer;
  I: Integer;
begin
  Result := False;
  AConnection := nil;

  if FShutdown then
    Exit;

  HostKey := MakeHostKey(AHost, APort);

  // Try to find an idle connection
  Pooled := FindIdleConnection(HostKey);

  if Pooled = nil then
  begin
    // Check if we can create a new connection
    List := FConnections.LockList;
    try
      HostCount := 0;
      for I := 0 to List.Count - 1 do
        if TPooledConnection(List[I]).HostKey = HostKey then
          Inc(HostCount);

      if HostCount >= FConfig.MaxPoolSize then
      begin
        TSecurityLog.Warning('ConnectionPool',
          Format('Pool exhausted for %s (max: %d)', [HostKey, FConfig.MaxPoolSize]));
        Exit;
      end;
    finally
      FConnections.UnlockList;
    end;

    // Create new connection
    Pooled := CreateNewConnection(AContext, AHost, APort, HostKey);
    if Pooled = nil then
      Exit;

    // Add to pool
    List := FConnections.LockList;
    try
      List.Add(Pooled);
    finally
      FConnections.UnlockList;
    end;
  end;

  AConnection := Pooled.Connection;
  Result := True;
end;

procedure TSSLConnectionPool.Release(AConnection: ISSLConnection);
var
  List: TList;
  I: Integer;
  Pooled: TPooledConnection;
begin
  if AConnection = nil then
    Exit;

  List := FConnections.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Pooled := TPooledConnection(List[I]);
      if Pooled.Connection = AConnection then
      begin
        Pooled.InUse := False;
        Pooled.LastUsedAt := Now;
        TSecurityLog.Info('ConnectionPool',
          Format('Released connection for %s', [Pooled.HostKey]));
        Break;
      end;
    end;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TSSLConnectionPool.CloseConnection(AConnection: ISSLConnection);
var
  List: TList;
  I: Integer;
  Pooled: TPooledConnection;
begin
  if AConnection = nil then
    Exit;

  List := FConnections.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Pooled := TPooledConnection(List[I]);
      if Pooled.Connection = AConnection then
      begin
        List.Delete(I);
        Pooled.Connection.Close;
        CloseSocket(Pooled.Socket);
        Pooled.Free;
        Break;
      end;
    end;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TSSLConnectionPool.RemoveConnection(APooled: TPooledConnection);
var
  List: TList;
begin
  List := FConnections.LockList;
  try
    List.Remove(APooled);
    if APooled.Connection <> nil then
      APooled.Connection.Close;
    CloseSocket(APooled.Socket);
    APooled.Free;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TSSLConnectionPool.CleanupExpiredConnections;
var
  List: TList;
  I: Integer;
  Pooled: TPooledConnection;
  Removed: Integer;
begin
  Removed := 0;
  List := FConnections.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Pooled := TPooledConnection(List[I]);
      if (not Pooled.InUse) and
         (Pooled.IsExpired(FConfig.IdleTimeoutSec) or not Pooled.IsHealthy) then
      begin
        List.Delete(I);
        if Pooled.Connection <> nil then
          Pooled.Connection.Close;
        CloseSocket(Pooled.Socket);
        Pooled.Free;
        Inc(Removed);
      end;
    end;
  finally
    FConnections.UnlockList;
  end;

  if Removed > 0 then
    TSecurityLog.Info('ConnectionPool',
      Format('Cleaned up %d expired connections', [Removed]));
end;

procedure TSSLConnectionPool.ClearHost(const AHost: string; APort: Word);
var
  HostKey: string;
  List: TList;
  I: Integer;
  Pooled: TPooledConnection;
begin
  HostKey := MakeHostKey(AHost, APort);

  List := FConnections.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Pooled := TPooledConnection(List[I]);
      if Pooled.HostKey = HostKey then
      begin
        List.Delete(I);
        if Pooled.Connection <> nil then
          Pooled.Connection.Close;
        CloseSocket(Pooled.Socket);
        Pooled.Free;
      end;
    end;
  finally
    FConnections.UnlockList;
  end;

  TSecurityLog.Info('ConnectionPool', Format('Cleared connections for %s', [HostKey]));
end;

procedure TSSLConnectionPool.ClearAll;
var
  List: TList;
  I: Integer;
  Pooled: TPooledConnection;
begin
  List := FConnections.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Pooled := TPooledConnection(List[I]);
      if Pooled.Connection <> nil then
        Pooled.Connection.Close;
      CloseSocket(Pooled.Socket);
      Pooled.Free;
    end;
    List.Clear;
  finally
    FConnections.UnlockList;
  end;

  TSecurityLog.Info('ConnectionPool', 'Cleared all connections');
end;

function TSSLConnectionPool.GetTotalConnections: Integer;
var
  List: TList;
begin
  List := FConnections.LockList;
  try
    Result := List.Count;
  finally
    FConnections.UnlockList;
  end;
end;

function TSSLConnectionPool.GetActiveConnections: Integer;
var
  List: TList;
  I: Integer;
begin
  Result := 0;
  List := FConnections.LockList;
  try
    for I := 0 to List.Count - 1 do
      if TPooledConnection(List[I]).InUse then
        Inc(Result);
  finally
    FConnections.UnlockList;
  end;
end;

function TSSLConnectionPool.GetIdleConnections: Integer;
begin
  Result := GetTotalConnections - GetActiveConnections;
end;

procedure TSSLConnectionPool.SetConfig(const AConfig: TSSLPoolConfig);
begin
  FLock.Enter;
  try
    FConfig := AConfig;
  finally
    FLock.Leave;
  end;
end;

function TSSLConnectionPool.GetConfig: TSSLPoolConfig;
begin
  FLock.Enter;
  try
    Result := FConfig;
  finally
    FLock.Leave;
  end;
end;

{ Global pool singleton }

function GlobalConnectionPool: ISSLConnectionPool;
begin
  if GConnectionPool = nil then
  begin
    if GPoolLock = nil then
      GPoolLock := TCriticalSection.Create;

    GPoolLock.Enter;
    try
      if GConnectionPool = nil then
        GConnectionPool := TSSLConnectionPool.Create;
    finally
      GPoolLock.Leave;
    end;
  end;
  Result := GConnectionPool;
end;

initialization
  GPoolLock := TCriticalSection.Create;

finalization
  GConnectionPool := nil;
  GPoolLock.Free;

end.
