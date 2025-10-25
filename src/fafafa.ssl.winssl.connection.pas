{
  fafafa.ssl.winssl.connection - WinSSL 连接实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-06
  
  描述:
    实现 ISSLConnection 接口的 WinSSL 后端。
    负责 Schannel TLS 握手和安全数据传输。
}

unit fafafa.ssl.winssl.connection;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ELSE}
  Sockets,
  {$ENDIF}
  SysUtils, Classes, DateUtils, SyncObjs,
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.winssl.types,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.certificate;

type
  { TWinSSLSession - Windows Schannel 会话实现 }
  TWinSSLSession = class(TInterfacedObject, ISSLSession)
  private
    FID: string;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FSessionData: TBytes;
    FSessionHandle: CtxtHandle;
    FHasHandle: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { ISSLSession 实现 }
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(aTimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;

    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;

    function Serialize: TBytes;
    function Deserialize(const aData: TBytes): Boolean;

    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;

    { 内部方法 }
    procedure SetSessionHandle(const aHandle: CtxtHandle);
    function HasSessionHandle: Boolean;
  end;

  { TWinSSLSessionManager - 会话缓存管理器 }
  TWinSSLSessionManager = class
  private
    FSessions: TStringList;
    FLock: TCriticalSection;
    FMaxSessions: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSession(const aID: string; aSession: ISSLSession);
    function GetSession(const aID: string): ISSLSession;
    procedure RemoveSession(const aID: string);
    procedure CleanupExpired;
    procedure SetMaxSessions(aMax: Integer);
  end;
  TWinSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: ISSLContext;
    FSocket: THandle;
    FStream: TStream;
    FCtxtHandle: CtxtHandle;
    FHandshakeState: TSSLHandshakeState;
    FConnected: Boolean;
    FBlocking: Boolean;
    FTimeout: Integer;
    
    // 缓冲区
    FRecvBuffer: array[0..16384-1] of Byte;
    FRecvBufferUsed: Integer;
    FDecryptedBuffer: array[0..16384-1] of Byte;
    FDecryptedBufferUsed: Integer;
    FExtraData: array[0..16384-1] of Byte;
    FExtraDataSize: Integer;

    // 会话管理
    FCurrentSession: ISSLSession;
    FSessionReused: Boolean;

    // 内部方法
    function PerformHandshake: TSSLHandshakeState;
    function ClientHandshake: Boolean;
    function ServerHandshake: Boolean;
    function SendData(const aBuffer; aSize: Integer): Integer;
    function RecvData(var aBuffer; aSize: Integer): Integer;
    
  public
    constructor Create(aContext: ISSLContext; aSocket: THandle); overload;
    constructor Create(aContext: ISSLContext; aStream: TStream); overload;
    destructor Destroy; override;
    
    { ISSLConnection - 连接管理 }
    function Connect: Boolean;
    function Accept: Boolean;
    function Shutdown: Boolean;
    procedure Close;
    
    { ISSLConnection - 握手控制 }
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    
    { ISSLConnection - 数据传输 }
    function Read(var aBuffer; aCount: Integer): Integer;
    function Write(const aBuffer; aCount: Integer): Integer;
    function ReadString(out aStr: string): Boolean;
    function WriteString(const aStr: string): Boolean;
    
    { ISSLConnection - 异步操作支持 }
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(aRet: Integer): TSSLErrorCode;
    
    { ISSLConnection - 连接信息 }
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    
    { ISSLConnection - 会话管理 }
    function GetSession: ISSLSession;
    procedure SetSession(aSession: ISSLSession);
    function IsSessionReused: Boolean;
    
    { ISSLConnection - ALPN/NPN }
    function GetSelectedALPNProtocol: string;
    
    { ISSLConnection - 状态查询 }
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    
    { ISSLConnection - 选项设置 }
    procedure SetTimeout(aTimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(aBlocking: Boolean);
    function GetBlocking: Boolean;
    
    { ISSLConnection - 原生句柄 }
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  end;

implementation

// ============================================================================
// TWinSSLSession - 会话实现
// ============================================================================

constructor TWinSSLSession.Create;
begin
  inherited Create;
  FID := '';
  FCreationTime := Now;
  FTimeout := 300; // 5 分钟
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
  SetLength(FSessionData, 0);
  InitSecHandle(FSessionHandle);
  FHasHandle := False;
end;

destructor TWinSSLSession.Destroy;
begin
  if FHasHandle then
    DeleteSecurityContext(@FSessionHandle);
  inherited Destroy;
end;

function TWinSSLSession.GetID: string;
begin
  Result := FID;
end;

function TWinSSLSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TWinSSLSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWinSSLSession.SetTimeout(aTimeout: Integer);
begin
  FTimeout := aTimeout;
end;

function TWinSSLSession.IsValid: Boolean;
begin
  Result := (FID <> '') and (SecondsBetween(Now, FCreationTime) < FTimeout);
end;

function TWinSSLSession.IsResumable: Boolean;
begin
  Result := IsValid;
end;

function TWinSSLSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TWinSSLSession.GetCipherName: string;
begin
  Result := FCipherName;
end;

function TWinSSLSession.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil; // TODO: 可扩展以支持证书存储
end;

function TWinSSLSession.Serialize: TBytes;
begin
  Result := FSessionData;
end;

function TWinSSLSession.Deserialize(const aData: TBytes): Boolean;
begin
  FSessionData := aData;
  Result := Length(FSessionData) > 0;
end;

function TWinSSLSession.GetNativeHandle: Pointer;
begin
  if FHasHandle then
    Result := @FSessionHandle
  else
    Result := nil;
end;

function TWinSSLSession.Clone: ISSLSession;
var
  LSession: TWinSSLSession;
begin
  LSession := TWinSSLSession.Create;
  LSession.FID := FID;
  LSession.FCreationTime := FCreationTime;
  LSession.FTimeout := FTimeout;
  LSession.FProtocolVersion := FProtocolVersion;
  LSession.FCipherName := FCipherName;
  LSession.FSessionData := FSessionData;
  Result := LSession;
end;

procedure TWinSSLSession.SetSessionHandle(const aHandle: CtxtHandle);
begin
  if FHasHandle then
    DeleteSecurityContext(@FSessionHandle);
  FSessionHandle := aHandle;
  FHasHandle := True;
end;

function TWinSSLSession.HasSessionHandle: Boolean;
begin
  Result := FHasHandle and IsValidSecHandle(FSessionHandle);
end;

// ============================================================================
// TWinSSLSessionManager - 会话缓存管理器
// ============================================================================

constructor TWinSSLSessionManager.Create;
begin
  inherited Create;
  FSessions := TStringList.Create;
  FSessions.Duplicates := dupIgnore;
  FSessions.Sorted := True;
  FLock := TCriticalSection.Create;
  FMaxSessions := 100;
end;

destructor TWinSSLSessionManager.Destroy;
begin
  FSessions.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TWinSSLSessionManager.AddSession(const aID: string; aSession: ISSLSession);
begin
  FLock.Enter;
  try
    FSessions.AddObject(aID, TObject(aSession));
    // 限制最大会话数
    while FSessions.Count > FMaxSessions do
      FSessions.Delete(0);
  finally
    FLock.Leave;
  end;
end;

function TWinSSLSessionManager.GetSession(const aID: string): ISSLSession;
var
  LIndex: Integer;
begin
  FLock.Enter;
  try
    LIndex := FSessions.IndexOf(aID);
    if LIndex >= 0 then
    begin
      Result := ISSLSession(FSessions.Objects[LIndex]);
      // 检查会话是否仍然有效
      if not Result.IsValid then
      begin
        FSessions.Delete(LIndex);
        Result := nil;
      end;
    end
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLSessionManager.RemoveSession(const aID: string);
var
  LIndex: Integer;
begin
  FLock.Enter;
  try
    LIndex := FSessions.IndexOf(aID);
    if LIndex >= 0 then
      FSessions.Delete(LIndex);
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLSessionManager.CleanupExpired;
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := FSessions.Count - 1 downto 0 do
    begin
      if not ISSLSession(FSessions.Objects[i]).IsValid then
        FSessions.Delete(i);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLSessionManager.SetMaxSessions(aMax: Integer);
begin
  if aMax > 0 then
    FMaxSessions := aMax;
end;

// ============================================================================
// TWinSSLConnection - 构造和析构
// ============================================================================

constructor TWinSSLConnection.Create(aContext: ISSLContext; aSocket: THandle);
begin
  inherited Create;
  FContext := aContext;
  FSocket := aSocket;
  FStream := nil;
  FHandshakeState := sslHsNotStarted;
  FConnected := False;
  FBlocking := True;
  FTimeout := 30000; // 30 秒
  FRecvBufferUsed := 0;
  FDecryptedBufferUsed := 0;
  FExtraDataSize := 0;

  // 初始化会话字段
  FCurrentSession := nil;
  FSessionReused := False;

  InitSecHandle(FCtxtHandle);
end;

constructor TWinSSLConnection.Create(aContext: ISSLContext; aStream: TStream);
begin
  inherited Create;
  FContext := aContext;
  FSocket := INVALID_HANDLE_VALUE;
  FStream := aStream;
  FHandshakeState := sslHsNotStarted;
  FConnected := False;
  FBlocking := True;
  FTimeout := 30000;
  FRecvBufferUsed := 0;
  FDecryptedBufferUsed := 0;
  FExtraDataSize := 0;

  // 初始化会话字段
  FCurrentSession := nil;
  FSessionReused := False;

  InitSecHandle(FCtxtHandle);
end;

destructor TWinSSLConnection.Destroy;
begin
  if FConnected then
    Shutdown;
  if IsValidSecHandle(FCtxtHandle) then
    DeleteSecurityContext(@FCtxtHandle);
  inherited Destroy;
end;

// ============================================================================
// 内部方法 - 数据收发
// ============================================================================

function TWinSSLConnection.SendData(const aBuffer; aSize: Integer): Integer;
begin
  if FStream <> nil then
    Result := FStream.Write(aBuffer, aSize)
  else if FSocket <> INVALID_HANDLE_VALUE then
  begin
    {$IFDEF WINDOWS}
    Result := WinSock2.send(FSocket, aBuffer, aSize, 0);
    if Result = SOCKET_ERROR then
      Result := -1;
    {$ELSE}
    Result := fpSend(FSocket, @aBuffer, aSize, 0);
    {$ENDIF}
  end
  else
    Result := -1;
end;

function TWinSSLConnection.RecvData(var aBuffer; aSize: Integer): Integer;
begin
  if FStream <> nil then
    Result := FStream.Read(aBuffer, aSize)
  else if FSocket <> INVALID_HANDLE_VALUE then
  begin
    {$IFDEF WINDOWS}
    Result := WinSock2.recv(FSocket, aBuffer, aSize, 0);
    if Result = SOCKET_ERROR then
      Result := -1;
    {$ELSE}
    Result := fpRecv(FSocket, @aBuffer, aSize, 0);
    {$ENDIF}
  end
  else
    Result := -1;
end;

// ============================================================================
// ISSLConnection - 连接管理
// ============================================================================

function TWinSSLConnection.Connect: Boolean;
begin
  Result := ClientHandshake;
  if Result then
  begin
    FConnected := True;
    FHandshakeState := sslHsCompleted;
  end
  else
    FHandshakeState := sslHsFailed;
end;

function TWinSSLConnection.Accept: Boolean;
begin
  Result := ServerHandshake;
  if Result then
  begin
    FConnected := True;
    FHandshakeState := sslHsCompleted;
  end
  else
    FHandshakeState := sslHsFailed;
end;

function TWinSSLConnection.Shutdown: Boolean;
var
  OutBuffers: array[0..0] of TSecBuffer;
  OutBufferDesc: TSecBufferDesc;
  dwType, Status: DWORD;
begin
  Result := False;
  
  if not FConnected then
    Exit;
  
  // 发送关闭通知
  dwType := SCHANNEL_SHUTDOWN;
  
  OutBuffers[0].pvBuffer := @dwType;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer := SizeOf(dwType);
  
  OutBufferDesc.cBuffers := 1;
  OutBufferDesc.pBuffers := @OutBuffers[0];
  OutBufferDesc.ulVersion := SECBUFFER_VERSION;
  
  Status := ApplyControlToken(@FCtxtHandle, @OutBufferDesc);
  
  if IsSuccess(Status) then
  begin
    DeleteSecurityContext(@FCtxtHandle);
    FConnected := False;
    Result := True;
  end;
end;

procedure TWinSSLConnection.Close;
begin
  if FConnected then
    Shutdown;
  FConnected := False;
  FHandshakeState := sslHsNotStarted;
end;

// ============================================================================
// ISSLConnection - 握手控制
// ============================================================================

function TWinSSLConnection.DoHandshake: TSSLHandshakeState;
begin
  Result := PerformHandshake;
end;

function TWinSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeState = sslHsCompleted;
end;

function TWinSSLConnection.Renegotiate: Boolean;
begin
  // TODO: 实现重新协商
  Result := False;
end;

function TWinSSLConnection.PerformHandshake: TSSLHandshakeState;
begin
  if FHandshakeState = sslHsCompleted then
    Exit(sslHsCompleted);
    
  if FHandshakeState = sslHsFailed then
    Exit(sslHsFailed);
    
  FHandshakeState := sslHsInProgress;
  
  if FContext.GetContextType = sslCtxClient then
  begin
    if ClientHandshake then
      FHandshakeState := sslHsCompleted
    else
      FHandshakeState := sslHsFailed;
  end
  else
  begin
    if ServerHandshake then
      FHandshakeState := sslHsCompleted
    else
      FHandshakeState := sslHsFailed;
  end;
  
  Result := FHandshakeState;
end;

// ============================================================================
// 握手实现 - 客户端
// ============================================================================

function TWinSSLConnection.ClientHandshake: Boolean;
var
  OutBuffers: array[0..0] of TSecBuffer;
  OutBufferDesc: TSecBufferDesc;
  InBuffers: array[0..1] of TSecBuffer;
  InBufferDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  dwSSPIFlags, dwSSPIOutFlags: DWORD;
  ServerName: PWideChar;
  cbData, cbIoBuffer: DWORD;
  IoBuffer: array[0..16384-1] of Byte;
begin
  Result := False;
  
  // 设置标志
  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT or
                 ISC_REQ_REPLAY_DETECT or
                 ISC_REQ_CONFIDENTIALITY or
                 ISC_RET_EXTENDED_ERROR or
                 ISC_REQ_ALLOCATE_MEMORY or
                 ISC_REQ_STREAM;
  
  ServerName := StringToPWideChar(FContext.GetServerName);
  try
    // 初始化输出缓冲区
    OutBuffers[0].pvBuffer := nil;
    OutBuffers[0].BufferType := SECBUFFER_TOKEN;
    OutBuffers[0].cbBuffer := 0;
    
    OutBufferDesc.cBuffers := 1;
    OutBufferDesc.pBuffers := @OutBuffers[0];
    OutBufferDesc.ulVersion := SECBUFFER_VERSION;
    
    // 第一次调用 InitializeSecurityContext
    Status := InitializeSecurityContextW(
      PCredHandle(FContext.GetNativeHandle),
      nil,
      ServerName,
      dwSSPIFlags,
      0,
      0,
      nil,
      0,
      @FCtxtHandle,
      @OutBufferDesc,
      @dwSSPIOutFlags,
      nil
    );
    
    if not ((Status = SEC_I_CONTINUE_NEEDED) or IsSuccess(Status)) then
      Exit;
    
    // 发送客户端 hello
    if (OutBuffers[0].cbBuffer > 0) and (OutBuffers[0].pvBuffer <> nil) then
    begin
      cbData := SendData(OutBuffers[0].pvBuffer^, OutBuffers[0].cbBuffer);
      FreeContextBuffer(OutBuffers[0].pvBuffer);
      if cbData <= 0 then
        Exit;
    end;
    
    // 继续握手循环
    cbIoBuffer := 0;
    while (Status = SEC_I_CONTINUE_NEEDED) or (Status = SEC_E_INCOMPLETE_MESSAGE) do
    begin
      // 接收服务器数据
      // 当状态为 SEC_E_INCOMPLETE_MESSAGE 时，即使缓冲区有数据也需要接收更多数据
      if (cbIoBuffer = 0) or (Status = SEC_E_INCOMPLETE_MESSAGE) then
      begin
        // 如果缓冲区已有数据，追加新数据；否则从头开始接收
        cbData := RecvData(IoBuffer[cbIoBuffer], SizeOf(IoBuffer) - cbIoBuffer);
        if cbData <= 0 then
          Exit;
        Inc(cbIoBuffer, cbData);
      end;
      
      // 设置输入缓冲区
      InBuffers[0].pvBuffer := @IoBuffer[0];
      InBuffers[0].cbBuffer := cbIoBuffer;
      InBuffers[0].BufferType := SECBUFFER_TOKEN;
      
      InBuffers[1].pvBuffer := nil;
      InBuffers[1].cbBuffer := 0;
      InBuffers[1].BufferType := SECBUFFER_EMPTY;
      
      InBufferDesc.cBuffers := 2;
      InBufferDesc.pBuffers := @InBuffers[0];
      InBufferDesc.ulVersion := SECBUFFER_VERSION;
      
      // 设置输出缓冲区
      OutBuffers[0].pvBuffer := nil;
      OutBuffers[0].BufferType := SECBUFFER_TOKEN;
      OutBuffers[0].cbBuffer := 0;
      
      OutBufferDesc.cBuffers := 1;
      OutBufferDesc.pBuffers := @OutBuffers[0];
      OutBufferDesc.ulVersion := SECBUFFER_VERSION;
      
      // 调用 InitializeSecurityContext
      Status := InitializeSecurityContextW(
        PCredHandle(FContext.GetNativeHandle),
        @FCtxtHandle,
        ServerName,
        dwSSPIFlags,
        0,
        0,
        @InBufferDesc,
        0,
        nil,
        @OutBufferDesc,
        @dwSSPIOutFlags,
        nil
      );
      
      // 处理额外数据
      if (InBuffers[1].BufferType = SECBUFFER_EXTRA) and (InBuffers[1].cbBuffer > 0) then
      begin
        Move(IoBuffer[cbIoBuffer - InBuffers[1].cbBuffer], IoBuffer[0], InBuffers[1].cbBuffer);
        cbIoBuffer := InBuffers[1].cbBuffer;
      end
      else if Status <> SEC_E_INCOMPLETE_MESSAGE then
        cbIoBuffer := 0;  // 只有在不需要更多数据时才清空缓冲区
      
      // 发送响应数据
      if (OutBuffers[0].cbBuffer > 0) and (OutBuffers[0].pvBuffer <> nil) then
      begin
        cbData := SendData(OutBuffers[0].pvBuffer^, OutBuffers[0].cbBuffer);
        FreeContextBuffer(OutBuffers[0].pvBuffer);
        if cbData <= 0 then
          Exit;
      end;
      
      // 检查状态
      if Status = SEC_E_INCOMPLETE_MESSAGE then
        Continue;  // 需要更多数据，继续循环
      if not ((Status = SEC_I_CONTINUE_NEEDED) or IsSuccess(Status)) then
        Exit;
    end;
    
    Result := IsSuccess(Status);
    
  finally
    if ServerName <> nil then
      FreePWideCharString(ServerName);
  end;
end;

// ============================================================================
// 握手实现 - 服务器端
// ============================================================================

function TWinSSLConnection.ServerHandshake: Boolean;
var
  OutBuffers: array[0..0] of TSecBuffer;
  OutBufferDesc: TSecBufferDesc;
  InBuffers: array[0..1] of TSecBuffer;
  InBufferDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  dwSSPIPackageFlags, dwSSPIOutFlags: DWORD;
  cbData, cbIoBuffer: DWORD;
  IoBuffer: array[0..16384-1] of Byte;
begin
  Result := False;

  // 设置服务器端标志
  dwSSPIPackageFlags := ASC_REQ_SEQUENCE_DETECT or
                        ASC_REQ_REPLAY_DETECT or
                        ASC_REQ_CONFIDENTIALITY or
                        ASC_RET_EXTENDED_ERROR or
                        ASC_REQ_ALLOCATE_MEMORY or
                        ASC_REQ_STREAM;

  // 初始化输出缓冲区
  OutBuffers[0].pvBuffer := nil;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer := 0;

  OutBufferDesc.cBuffers := 1;
  OutBufferDesc.pBuffers := @OutBuffers[0];
  OutBufferDesc.ulVersion := SECBUFFER_VERSION;

  cbIoBuffer := 0;

  // 握手主循环
  while True do
  begin
    // 接收客户端数据
    if (cbIoBuffer = 0) or (Status = SEC_E_INCOMPLETE_MESSAGE) then
    begin
      cbData := RecvData(IoBuffer[cbIoBuffer], SizeOf(IoBuffer) - cbIoBuffer);
      if cbData <= 0 then
      begin
        FHandshakeState := sslHsFailed;
        Exit;
      end;
      Inc(cbIoBuffer, cbData);
    end;

    // 设置输入缓冲区
    InBuffers[0].pvBuffer := @IoBuffer[0];
    InBuffers[0].cbBuffer := cbIoBuffer;
    InBuffers[0].BufferType := SECBUFFER_TOKEN;

    InBuffers[1].pvBuffer := nil;
    InBuffers[1].cbBuffer := 0;
    InBuffers[1].BufferType := SECBUFFER_EMPTY;

    InBufferDesc.cBuffers := 2;
    InBufferDesc.pBuffers := @InBuffers[0];
    InBufferDesc.ulVersion := SECBUFFER_VERSION;

    // 调用 AcceptSecurityContext
    Status := AcceptSecurityContextW(
      PCredHandle(FContext.GetNativeHandle),
      nil,
      @InBufferDesc,
      dwSSPIPackageFlags,
      0,
      @FCtxtHandle,
      @OutBufferDesc,
      @dwSSPIOutFlags,
      nil
    );

    // 处理额外数据
    if (InBuffers[1].BufferType = SECBUFFER_EXTRA) and (InBuffers[1].cbBuffer > 0) then
    begin
      Move(IoBuffer[cbIoBuffer - InBuffers[1].cbBuffer], IoBuffer[0], InBuffers[1].cbBuffer);
      cbIoBuffer := InBuffers[1].cbBuffer;
    end
    else if Status <> SEC_E_INCOMPLETE_MESSAGE then
      cbIoBuffer := 0;

    // 发送响应数据
    if (OutBuffers[0].cbBuffer > 0) and (OutBuffers[0].pvBuffer <> nil) then
    begin
      cbData := SendData(OutBuffers[0].pvBuffer^, OutBuffers[0].cbBuffer);
      FreeContextBuffer(OutBuffers[0].pvBuffer);
      if cbData <= 0 then
      begin
        FHandshakeState := sslHsFailed;
        Exit;
      end;
    end;

    // 检查握手状态
    if Status = SEC_E_INCOMPLETE_MESSAGE then
      Continue  // 需要更多数据，继续循环

    if IsSuccess(Status) then
    begin
      // 握手成功
      FHandshakeState := sslHsCompleted;
      FConnected := True;
      Result := True;
      Break;
    end
    else if Status = SEC_I_CONTINUE_NEEDED then
    begin
      // 需要继续握手
      Continue;
    end
    else
    begin
      // 握手失败
      FHandshakeState := sslHsFailed;
      Break;
    end;
  end;
end;

// ============================================================================
// ISSLConnection - 数据传输
// ============================================================================

function TWinSSLConnection.Read(var aBuffer; aCount: Integer): Integer;
var
  InBuffers: array[0..3] of TSecBuffer;
  InBufferDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  i, cbData: Integer;
begin
  Result := 0;
  
  if not FConnected then
    Exit;
  
  // 如果有已解密的数据，直接返回
  if FDecryptedBufferUsed > 0 then
  begin
    Result := Min(aCount, FDecryptedBufferUsed);
    Move(FDecryptedBuffer[0], aBuffer, Result);
    Dec(FDecryptedBufferUsed, Result);
    if FDecryptedBufferUsed > 0 then
      Move(FDecryptedBuffer[Result], FDecryptedBuffer[0], FDecryptedBufferUsed);
    Exit;
  end;
  
  // 读取加密数据
  if FRecvBufferUsed < SizeOf(FRecvBuffer) then
  begin
    cbData := RecvData(FRecvBuffer[FRecvBufferUsed], SizeOf(FRecvBuffer) - FRecvBufferUsed);
    if cbData <= 0 then
      Exit;
    Inc(FRecvBufferUsed, cbData);
  end;
  
  // 解密数据
  InBuffers[0].pvBuffer := @FRecvBuffer[0];
  InBuffers[0].cbBuffer := FRecvBufferUsed;
  InBuffers[0].BufferType := SECBUFFER_DATA;
  
  InBuffers[1].BufferType := SECBUFFER_EMPTY;
  InBuffers[2].BufferType := SECBUFFER_EMPTY;
  InBuffers[3].BufferType := SECBUFFER_EMPTY;
  
  InBufferDesc.cBuffers := 4;
  InBufferDesc.pBuffers := @InBuffers[0];
  InBufferDesc.ulVersion := SECBUFFER_VERSION;
  
  Status := DecryptMessage(@FCtxtHandle, @InBufferDesc, 0, nil);
  
  if Status = SEC_E_INCOMPLETE_MESSAGE then
    Exit; // 需要更多数据
  
  if not IsSuccess(Status) then
    Exit;
  
  // 查找解密的数据
  for i := 0 to 3 do
  begin
    if InBuffers[i].BufferType = SECBUFFER_DATA then
    begin
      Result := Min(aCount, Integer(InBuffers[i].cbBuffer));
      Move(InBuffers[i].pvBuffer^, aBuffer, Result);
      
      // 保存剩余数据
      if Integer(InBuffers[i].cbBuffer) > Result then
      begin
        FDecryptedBufferUsed := InBuffers[i].cbBuffer - Result;
        Move(PByte(InBuffers[i].pvBuffer)[Result], FDecryptedBuffer[0], FDecryptedBufferUsed);
      end;
      Break;
    end;
  end;
  
  // 处理额外数据
  for i := 0 to 3 do
  begin
    if InBuffers[i].BufferType = SECBUFFER_EXTRA then
    begin
      Move(FRecvBuffer[FRecvBufferUsed - InBuffers[i].cbBuffer], FRecvBuffer[0], InBuffers[i].cbBuffer);
      FRecvBufferUsed := InBuffers[i].cbBuffer;
      Exit;
    end;
  end;
  
  FRecvBufferUsed := 0;
end;

function TWinSSLConnection.Write(const aBuffer; aCount: Integer): Integer;
var
  OutBuffers: array[0..3] of TSecBuffer;
  OutBufferDesc: TSecBufferDesc;
  StreamSizes: TSecPkgContext_StreamSizes;
  Status: SECURITY_STATUS;
  Message: array of Byte;
  cbMessage, cbData: DWORD;
begin
  Result := 0;
  
  if not FConnected then
    Exit;
  
  // 获取流大小
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_STREAM_SIZES, @StreamSizes);
  if not IsSuccess(Status) then
    Exit;
  
  // 分配消息缓冲区
  cbMessage := StreamSizes.cbHeader + aCount + StreamSizes.cbTrailer;
  SetLength(Message, cbMessage);
  
  // 设置缓冲区
  OutBuffers[0].pvBuffer := @Message[0];
  OutBuffers[0].cbBuffer := StreamSizes.cbHeader;
  OutBuffers[0].BufferType := SECBUFFER_STREAM_HEADER;
  
  OutBuffers[1].pvBuffer := @Message[StreamSizes.cbHeader];
  OutBuffers[1].cbBuffer := aCount;
  OutBuffers[1].BufferType := SECBUFFER_DATA;
  Move(aBuffer, OutBuffers[1].pvBuffer^, aCount);
  
  OutBuffers[2].pvBuffer := @Message[StreamSizes.cbHeader + aCount];
  OutBuffers[2].cbBuffer := StreamSizes.cbTrailer;
  OutBuffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
  
  OutBuffers[3].BufferType := SECBUFFER_EMPTY;
  
  OutBufferDesc.cBuffers := 4;
  OutBufferDesc.pBuffers := @OutBuffers[0];
  OutBufferDesc.ulVersion := SECBUFFER_VERSION;
  
  // 加密数据
  Status := EncryptMessage(@FCtxtHandle, 0, @OutBufferDesc, 0);
  if not IsSuccess(Status) then
    Exit;
  
  // 发送加密数据
  cbData := SendData(Message[0], OutBuffers[0].cbBuffer + OutBuffers[1].cbBuffer + OutBuffers[2].cbBuffer);
  if cbData > 0 then
    Result := aCount;
end;

function TWinSSLConnection.ReadString(out aStr: string): Boolean;
var
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, SizeOf(Buffer));
  Result := BytesRead > 0;
  if Result then
    SetString(aStr, PChar(@Buffer[0]), BytesRead);
end;

function TWinSSLConnection.WriteString(const aStr: string): Boolean;
begin
  Result := Write(PChar(aStr)^, Length(aStr)) = Length(aStr);
end;

// ============================================================================
// ISSLConnection - 异步操作支持（存根）
// ============================================================================

function TWinSSLConnection.WantRead: Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLConnection.WantWrite: Boolean;
begin
  Result := False; // TODO
end;

function TWinSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone; // TODO
end;

// ============================================================================
// ISSLConnection - 连接信息（存根）
// ============================================================================

function TWinSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  // TODO: 实现
end;

function TWinSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
var
  ConnInfo: TSecPkgContext_ConnectionInfo;
  Status: SECURITY_STATUS;
begin
  Result := sslProtocolTLS12; // 默认值
  
  if not FConnected then
    Exit;
  
  // 查询连接信息
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_CONNECTION_INFO, @ConnInfo);
  if not IsSuccess(Status) then
    Exit;
  
  // 将 Schannel 协议标志转换为 TSSLProtocolVersion
  if (ConnInfo.dwProtocol and SP_PROT_TLS1_3) <> 0 then
    Result := sslProtocolTLS13
  else if (ConnInfo.dwProtocol and SP_PROT_TLS1_2) <> 0 then
    Result := sslProtocolTLS12
  else if (ConnInfo.dwProtocol and SP_PROT_TLS1_1) <> 0 then
    Result := sslProtocolTLS11
  else if (ConnInfo.dwProtocol and SP_PROT_TLS1_0) <> 0 then
    Result := sslProtocolTLS10
  else if (ConnInfo.dwProtocol and SP_PROT_SSL3) <> 0 then
    Result := sslProtocolSSL3
  else if (ConnInfo.dwProtocol and SP_PROT_SSL2) <> 0 then
    Result := sslProtocolSSL2;
end;

function TWinSSLConnection.GetCipherName: string;
var
  ConnInfo: TSecPkgContext_ConnectionInfo;
  Status: SECURITY_STATUS;
begin
  Result := '';
  
  if not FConnected then
    Exit;
  
  // 查询连接信息
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_CONNECTION_INFO, @ConnInfo);
  if not IsSuccess(Status) then
    Exit;
  
  // 构建加密套件描述
  Result := Format('0x%x (Strength: %d bits)', [
    ConnInfo.aiCipher,
    ConnInfo.dwCipherStrength
  ]);
end;

function TWinSSLConnection.GetPeerCertificate: ISSLCertificate;
var
  CertContext: PCCERT_CONTEXT;
  Status: SECURITY_STATUS;
begin
  Result := nil;
  
  if not FConnected then
    Exit;
  
  // 查询远端证书
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @CertContext);
  
  if IsSuccess(Status) and (CertContext <> nil) then
  begin
    // 创建证书对象，并让它拥有上下文
    Result := CreateWinSSLCertificateFromContext(CertContext, True);
  end;
end;

function TWinSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
var
  CertContext: PCCERT_CONTEXT;
  ChainPara: CERT_CHAIN_PARA;
  ChainContext: PCCERT_CHAIN_CONTEXT;
  Status: SECURITY_STATUS;
  i, j, ChainCount: Integer;
  SimpleChain: PCERT_SIMPLE_CHAIN;
begin
  SetLength(Result, 0);
  
  if not FConnected then
    Exit;
  
  // 获取对端证书
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @CertContext);
  
  if not IsSuccess(Status) or (CertContext = nil) then
    Exit;
  
  try
    // 准备证书链参数
    FillChar(ChainPara, SizeOf(ChainPara), 0);
    ChainPara.cbSize := SizeOf(CERT_CHAIN_PARA);
    
    // 获取证书链
    if CertGetCertificateChain(
      nil,                  // 使用默认链引擎
      CertContext,
      nil,                  // 当前时间
      nil,                  // 无附加存储
      @ChainPara,
      0,                    // 默认标志
      nil,
      @ChainContext
    ) then
    begin
      try
        // 遍历所有简单链（通常只有一个）
        ChainCount := 0;
        for i := 0 to Integer(ChainContext^.cChain) - 1 do
        begin
          SimpleChain := PPCERT_SIMPLE_CHAIN(ChainContext^.rgpChain)[i];
          if SimpleChain <> nil then
            Inc(ChainCount, SimpleChain^.cElement);
        end;
        
        SetLength(Result, ChainCount);
        ChainCount := 0;
        
        // 提取证书
        for i := 0 to Integer(ChainContext^.cChain) - 1 do
        begin
          SimpleChain := PPCERT_SIMPLE_CHAIN(ChainContext^.rgpChain)[i];
          if SimpleChain <> nil then
          begin
            for j := 0 to Integer(SimpleChain^.cElement) - 1 do
            begin
              if PPCERT_CHAIN_ELEMENT(SimpleChain^.rgpElement)[j] <> nil then
              begin
                Result[ChainCount] := CreateWinSSLCertificateFromContext(
                  CertDuplicateCertificateContext(PPCERT_CHAIN_ELEMENT(SimpleChain^.rgpElement)[j]^.pCertContext),
                  True
                );
                Inc(ChainCount);
              end;
            end;
          end;
        end;
        
        // 调整数组大小
        SetLength(Result, ChainCount);
      finally
        CertFreeCertificateChain(ChainContext);
      end;
    end;
  finally
    CertFreeCertificateContext(CertContext);
  end;
end;

function TWinSSLConnection.GetVerifyResult: Integer;
var
  CertContext: PCCERT_CONTEXT;
  ChainPara: CERT_CHAIN_PARA;
  ChainContext: PCCERT_CHAIN_CONTEXT;
  PolicyPara: CERT_CHAIN_POLICY_PARA;
  PolicyStatus: CERT_CHAIN_POLICY_STATUS;
  Status: SECURITY_STATUS;
begin
  Result := -1; // 默认错误
  
  if not FConnected then
    Exit;
  
  // 获取对端证书
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @CertContext);
  
  if not IsSuccess(Status) or (CertContext = nil) then
    Exit;
  
  try
    // 准备证书链参数
    FillChar(ChainPara, SizeOf(ChainPara), 0);
    ChainPara.cbSize := SizeOf(CERT_CHAIN_PARA);
    
    // 获取证书链
    if CertGetCertificateChain(
      nil,
      CertContext,
      nil,
      nil,
      @ChainPara,
      0,
      nil,
      @ChainContext
    ) then
    begin
      try
        // 准备策略参数
        FillChar(PolicyPara, SizeOf(PolicyPara), 0);
        PolicyPara.cbSize := SizeOf(CERT_CHAIN_POLICY_PARA);
        
        FillChar(PolicyStatus, SizeOf(PolicyStatus), 0);
        PolicyStatus.cbSize := SizeOf(CERT_CHAIN_POLICY_STATUS);
        
        // 验证证书链
        if CertVerifyCertificateChainPolicy(
          CERT_CHAIN_POLICY_SSL,
          ChainContext,
          @PolicyPara,
          @PolicyStatus
        ) then
        begin
          if PolicyStatus.dwError = 0 then
            Result := 0  // 验证成功
          else
            Result := Integer(PolicyStatus.dwError);
        end;
      finally
        CertFreeCertificateChain(ChainContext);
      end;
    end;
  finally
    CertFreeCertificateContext(CertContext);
  end;
end;

function TWinSSLConnection.GetVerifyResultString: string;
var
  VerifyResult: Integer;
begin
  VerifyResult := GetVerifyResult;
  
  case VerifyResult of
    0: Result := 'OK';
    -1: Result := 'Certificate not available';
    CERT_E_EXPIRED: Result := 'Certificate expired';
    CERT_E_WRONG_USAGE: Result := 'Wrong usage';
    CERT_E_UNTRUSTEDROOT: Result := 'Untrusted root';
    CERT_E_REVOKED: Result := 'Certificate revoked';
    CERT_E_CN_NO_MATCH: Result := 'Common name mismatch';
    CERT_E_INVALID_NAME: Result := 'Invalid name';
    TRUST_E_CERT_SIGNATURE: Result := 'Invalid signature';
  else
    Result := Format('Verification error: 0x%x', [VerifyResult]);
  end;
end;

// ============================================================================
// ISSLConnection - 会话管理（存根）
// ============================================================================

function TWinSSLConnection.GetSession: ISSLSession;
begin
  Result := FCurrentSession;
end;

procedure TWinSSLConnection.SetSession(aSession: ISSLSession);
begin
  FCurrentSession := aSession;
  if aSession <> nil then
  begin
    FSessionReused := True;
    // 如果有上下文句柄，存储到会话中
    if IsValidSecHandle(FCtxtHandle) and (aSession is TWinSSLSession) then
      TWinSSLSession(aSession).SetSessionHandle(FCtxtHandle);
  end;
end;

function TWinSSLConnection.IsSessionReused: Boolean;
begin
  Result := FSessionReused;
end;

// ============================================================================
// ISSLConnection - ALPN/NPN（存根）
// ============================================================================

function TWinSSLConnection.GetSelectedALPNProtocol: string;
var
  AppProto: TSecPkgContext_ApplicationProtocol;
  Status: SECURITY_STATUS;
begin
  Result := '';
  
  if not FConnected then
    Exit;
  
  // 查询应用层协议
  FillChar(AppProto, SizeOf(AppProto), 0);
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_APPLICATION_PROTOCOL, @AppProto);
  
  // 如果查询成功且协商成功
  if IsSuccess(Status) and (AppProto.ProtoNegoStatus = 0) and (AppProto.ProtocolIdSize > 0) then
  begin
    SetString(Result, PChar(@AppProto.ProtocolId[0]), AppProto.ProtocolIdSize);
  end;
end;

// ============================================================================
// ISSLConnection - 状态查询
// ============================================================================

function TWinSSLConnection.IsConnected: Boolean;
begin
  Result := FConnected;
end;

function TWinSSLConnection.GetState: string;
begin
  case FHandshakeState of
    sslHsNotStarted: Result := 'not_started';
    sslHsInProgress: Result := 'in_progress';
    sslHsCompleted: Result := 'completed';
    sslHsFailed: Result := 'failed';
    sslHsRenegotiating: Result := 'renegotiating';
  else
    Result := 'unknown';
  end;
end;

function TWinSSLConnection.GetStateString: string;
begin
  Result := GetState; // 简化版本
end;

// ============================================================================
// ISSLConnection - 选项设置
// ============================================================================

procedure TWinSSLConnection.SetTimeout(aTimeout: Integer);
begin
  FTimeout := aTimeout;
end;

function TWinSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWinSSLConnection.SetBlocking(aBlocking: Boolean);
begin
  FBlocking := aBlocking;
end;

function TWinSSLConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

// ============================================================================
// ISSLConnection - 原生句柄
// ============================================================================

function TWinSSLConnection.GetNativeHandle: Pointer;
begin
  Result := @FCtxtHandle;
end;

function TWinSSLConnection.GetContext: ISSLContext;
begin
  Result := FContext;
end;

end.
