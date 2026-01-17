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
  Windows, winsock2,
  {$ELSE}
  Sockets,
  {$ENDIF}
  SysUtils, Classes, SyncObjs,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.errors,  // 任务 4.2: 添加错误处理单元
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.context;  // P0-2: 访问 TWinSSLContext.GetCAStoreHandle

type
  { TWinSSLSession - Windows Schannel 会话实现
    P0-4: 安全化重构 - 不再持有 CtxtHandle，改为元数据模式
    Schannel 的会话复用由系统自动管理，此类仅保存会话元数据 }
  TWinSSLSession = class(TInterfacedObject, ISSLSession)
  private
    FID: string;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FSessionData: TBytes;
    // P0-4: 移除 FSessionHandle 和 FHasHandle，避免双重释放风险
    // Schannel 会话由系统管理，不需要显式持有句柄
    FPeerCertificate: ISSLCertificate;  // P1.2: 缓存对端证书以匹配 OpenSSL 行为
    FIsResumed: Boolean;  // P0-4: 标记是否为恢复的会话
  public
    constructor Create;
    destructor Destroy; override;

    { ISSLSession 实现 }
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(ATimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;

    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;

    function Serialize: TBytes;
    function Deserialize(const AData: TBytes): Boolean;

    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;

    { P1.2: 设置对端证书（供 Connection 调用） }
    procedure SetPeerCertificate(ACert: ISSLCertificate);

    { P0-4: 元数据设置方法（供 Connection 调用） }
    procedure SetSessionMetadata(const AID: string; AProtocol: TSSLProtocolVersion;
      const ACipher: string; AResumed: Boolean);
    function WasResumed: Boolean;
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

    procedure AddSession(const AID: string; ASession: ISSLSession);
    function GetSession(const AID: string): ISSLSession;
    procedure RemoveSession(const AID: string);
    procedure CleanupExpired;
    procedure SetMaxSessions(AMax: Integer);
  end;
  TWinSSLConnection = class(TInterfacedObject, ISSLConnection, ISSLClientConnection)
  private
    FContext: ISSLContext;
    FSocket: THandle;
    FStream: TStream;
    FCtxtHandle: CtxtHandle;
    FHandshakeState: TSSLHandshakeState;
    FConnected: Boolean;
    FBlocking: Boolean;
    FTimeout: Integer;
    FServerName: string;
    
    // 缓冲区 - 使用常量而非魔法数字
    FRecvBuffer: array[0..SSL_DEFAULT_BUFFER_SIZE-1] of Byte;
    FRecvBufferUsed: Integer;
    FDecryptedBuffer: array[0..SSL_DEFAULT_BUFFER_SIZE-1] of Byte;
    FDecryptedBufferUsed: Integer;
    FExtraData: array[0..SSL_DEFAULT_BUFFER_SIZE-1] of Byte;
    FExtraDataSize: Integer;

    // 会话管理
    FCurrentSession: ISSLSession;
    FSessionReused: Boolean;

    // 最后一次操作状态（用于 WantRead/WantWrite）
    FLastError: TSSLErrorCode;

    // 内部方法
    function PerformHandshake: TSSLHandshakeState;
    function ClientHandshake: Boolean;
    function ServerHandshake: Boolean;
    function SendData(const ABuffer; ASize: Integer): Integer;
    function RecvData(var ABuffer; ASize: Integer): Integer;

    // P1-1: 提取的握手辅助方法
    procedure PrepareInputBufferDesc(var AInBuffers: array of TSecBuffer;
      var AInBufferDesc: TSecBufferDesc; AData: Pointer; ADataSize: DWORD);
    procedure PrepareOutputBufferDesc(var AOutBuffers: array of TSecBuffer;
      var AOutBufferDesc: TSecBufferDesc);
    procedure HandleExtraData(var AExtraBuffer: array of TSecBuffer;
      var AIoBuffer: array of Byte; var AIoBufferSize: DWORD; AStatus: SECURITY_STATUS);
    function SendOutputBuffer(const AOutBuffer: TSecBuffer): Boolean;

    // Post-handshake verification (Strategy A)
    function ValidatePeerCertificate(out AVerifyError: Integer): Boolean;

    // P1-6: ALPN 协议缓冲区构建
    function BuildALPNBuffer(const AProtocols: string; out ABuffer: TBytes): Boolean;

    // P1-7: InfoCallback 辅助方法
    procedure NotifyInfoCallback(AWhere: Integer; ARet: Integer; const AState: string);

    // P11.2: 会话保存辅助方法
    procedure SaveSessionAfterHandshake;

  public
    constructor Create(AContext: ISSLContext; ASocket: THandle); overload;
    constructor Create(AContext: ISSLContext; AStream: TStream); overload;
    destructor Destroy; override;

    { ISSLClientConnection }
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
    
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
    function Read(var ABuffer; ACount: Integer): Integer;
    function Write(const ABuffer; ACount: Integer): Integer;
    function ReadString(out AStr: string): Boolean;
    function WriteString(const AStr: string): Boolean;
    
    { ISSLConnection - 异步操作支持 }
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(ARet: Integer): TSSLErrorCode;
    
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
    procedure SetSession(ASession: ISSLSession);
    function IsSessionReused: Boolean;
    
    { ISSLConnection - ALPN/NPN }
    function GetSelectedALPNProtocol: string;
    
    { ISSLConnection - 状态查询 }
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    
    { ISSLConnection - 选项设置 }
    procedure SetTimeout(ATimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(ABlocking: Boolean);
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
  FTimeout := SSL_DEFAULT_SESSION_TIMEOUT; // 使用常量而非魔法数字
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
  SetLength(FSessionData, 0);
  // P0-4: 移除 FSessionHandle 初始化，不再持有句柄
  FPeerCertificate := nil;
  FIsResumed := False;
end;

destructor TWinSSLSession.Destroy;
begin
  // P0-4: 移除 DeleteSecurityContext 调用，不再持有句柄
  // Schannel 会话由系统自动管理
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

procedure TWinSSLSession.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TWinSSLSession.IsValid: Boolean;
begin
  Result := (FID <> '') and ((Now - FCreationTime) * 86400 < FTimeout);
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
  Result := FPeerCertificate;
end;

procedure TWinSSLSession.SetPeerCertificate(ACert: ISSLCertificate);
begin
  FPeerCertificate := ACert;
end;

function TWinSSLSession.Serialize: TBytes;
begin
  Result := FSessionData;
end;

function TWinSSLSession.Deserialize(const AData: TBytes): Boolean;
begin
  FSessionData := AData;
  Result := Length(FSessionData) > 0;
end;

function TWinSSLSession.GetNativeHandle: Pointer;
begin
  // P0-4: 不再持有 CtxtHandle，返回 nil
  // Schannel 会话由系统自动管理
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
  LSession.FIsResumed := FIsResumed;
  // P1.2: 克隆对端证书
  if FPeerCertificate <> nil then
    LSession.FPeerCertificate := FPeerCertificate.Clone
  else
    LSession.FPeerCertificate := nil;
  Result := LSession;
end;

{ P0-4: 元数据设置方法 }
procedure TWinSSLSession.SetSessionMetadata(const AID: string;
  AProtocol: TSSLProtocolVersion; const ACipher: string; AResumed: Boolean);
begin
  FID := AID;
  FProtocolVersion := AProtocol;
  FCipherName := ACipher;
  FIsResumed := AResumed;
end;

function TWinSSLSession.WasResumed: Boolean;
begin
  Result := FIsResumed;
end;

// ============================================================================
// TWinSSLSessionManager - 会话缓存管理器
// ============================================================================

constructor TWinSSLSessionManager.Create;
begin
  inherited Create;
  FSessions := TStringList.Create;
  FSessions.Duplicates := dupIgnore;
  FSessions.Sorted := False;  // P10.1: 不排序,保持插入顺序以实现 FIFO
  FLock := TCriticalSection.Create;
  FMaxSessions := 100;
end;

destructor TWinSSLSessionManager.Destroy;
var
  i: Integer;
begin
  // 释放所有会话的引用
  for i := 0 to FSessions.Count - 1 do
  begin
    if FSessions.Objects[i] <> nil then
      ISSLSession(Pointer(FSessions.Objects[i]))._Release;
  end;
  FSessions.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TWinSSLSessionManager.AddSession(const AID: string; ASession: ISSLSession);
var
  LIndex: Integer;
begin
  FLock.Enter;
  try
    // 检查是否已存在,如果存在则更新
    LIndex := FSessions.IndexOf(AID);
    if LIndex >= 0 then
    begin
      // 释放旧会话的引用
      if FSessions.Objects[LIndex] <> nil then
        ISSLSession(Pointer(FSessions.Objects[LIndex]))._Release;
      FSessions.Delete(LIndex);
    end;
    
    // 增加引用计数
    if ASession <> nil then
      ASession._AddRef;
    
    FSessions.AddObject(AID, TObject(Pointer(ASession)));
    
    // 限制最大会话数
    while FSessions.Count > FMaxSessions do
    begin
      // 释放被删除会话的引用
      if FSessions.Objects[0] <> nil then
        ISSLSession(Pointer(FSessions.Objects[0]))._Release;
      FSessions.Delete(0);
    end;
  finally
    FLock.Leave;
  end;
end;

function TWinSSLSessionManager.GetSession(const AID: string): ISSLSession;
var
  LIndex: Integer;
begin
  FLock.Enter;
  try
    LIndex := FSessions.IndexOf(AID);
    if LIndex >= 0 then
    begin
      Result := ISSLSession(Pointer(FSessions.Objects[LIndex]));
      // 检查会话是否仍然有效
      if not Result.IsValid then
      begin
        // 释放引用
        if FSessions.Objects[LIndex] <> nil then
          ISSLSession(Pointer(FSessions.Objects[LIndex]))._Release;
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

procedure TWinSSLSessionManager.RemoveSession(const AID: string);
var
  LIndex: Integer;
begin
  FLock.Enter;
  try
    LIndex := FSessions.IndexOf(AID);
    if LIndex >= 0 then
    begin
      // 释放引用
      if FSessions.Objects[LIndex] <> nil then
        ISSLSession(Pointer(FSessions.Objects[LIndex]))._Release;
      FSessions.Delete(LIndex);
    end;
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
      if not ISSLSession(Pointer(FSessions.Objects[i])).IsValid then
      begin
        // 释放引用
        if FSessions.Objects[i] <> nil then
          ISSLSession(Pointer(FSessions.Objects[i]))._Release;
        FSessions.Delete(i);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TWinSSLSessionManager.SetMaxSessions(AMax: Integer);
begin
  if AMax > 0 then
    FMaxSessions := AMax;
end;

// ============================================================================
// TWinSSLConnection - 构造和析构
// ============================================================================

constructor TWinSSLConnection.Create(AContext: ISSLContext; ASocket: THandle);
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FStream := nil;
  FHandshakeState := sslHsNotStarted;
  FConnected := False;
  FBlocking := True;
  FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT; // Rust-quality: 使用常量替代魔数

  // Initialize per-connection server name from context default (backward compatibility)
  FServerName := '';
  if AContext.GetServerName <> '' then
    FServerName := AContext.GetServerName;

  FRecvBufferUsed := 0;
  FDecryptedBufferUsed := 0;
  FExtraDataSize := 0;

  // 初始化会话字段
  FCurrentSession := nil;
  FSessionReused := False;

  // 初始化错误状态
  FLastError := sslErrNone;

  InitSecHandle(FCtxtHandle);
end;

constructor TWinSSLConnection.Create(AContext: ISSLContext; AStream: TStream);
begin
  inherited Create;
  FContext := AContext;
  FSocket := INVALID_HANDLE_VALUE;
  FStream := AStream;
  FHandshakeState := sslHsNotStarted;
  FConnected := False;
  FBlocking := True;
  FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;

  // Initialize per-connection server name from context default (backward compatibility)
  FServerName := '';
  if AContext.GetServerName <> '' then
    FServerName := AContext.GetServerName;

  FRecvBufferUsed := 0;
  FDecryptedBufferUsed := 0;
  FExtraDataSize := 0;

  // 初始化会话字段
  FCurrentSession := nil;
  FSessionReused := False;

  // 初始化错误状态
  FLastError := sslErrNone;

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

procedure TWinSSLConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TWinSSLConnection.GetServerName: string;
begin
  Result := FServerName;
end;

// ============================================================================
// 内部方法 - 数据收发
// ============================================================================

function TWinSSLConnection.SendData(const ABuffer; ASize: Integer): Integer;
begin
  if FStream <> nil then
    Result := FStream.Write(ABuffer, ASize)
  else if FSocket <> INVALID_HANDLE_VALUE then
  begin
    {$IFDEF WINDOWS}
    Result := winsock2.send(FSocket, ABuffer, ASize, 0);
    if Result = SOCKET_ERROR then
      Result := -1;
    {$ELSE}
    Result := fpSend(FSocket, @ABuffer, ASize, 0);
    {$ENDIF}
  end
  else
    Result := -1;
end;

function TWinSSLConnection.RecvData(var ABuffer; ASize: Integer): Integer;
begin
  if FStream <> nil then
    Result := FStream.Read(ABuffer, ASize)
  else if FSocket <> INVALID_HANDLE_VALUE then
  begin
    {$IFDEF WINDOWS}
    Result := winsock2.recv(FSocket, ABuffer, ASize, 0);
    if Result = SOCKET_ERROR then
      Result := -1;
    {$ELSE}
    Result := fpRecv(FSocket, @ABuffer, ASize, 0);
    {$ENDIF}
  end
  else
    Result := -1;
end;

// ============================================================================
// ISSLConnection - 连接管理
// ============================================================================

function TWinSSLConnection.Connect: Boolean;
var
  LVerifyError: Integer;
begin
  // P1-7: 通知握手开始
  NotifyInfoCallback(1, 0, 'handshake_start');

  Result := ClientHandshake;
  if not Result then
  begin
    FConnected := False;
    FHandshakeState := sslHsFailed;
    // P1-7: 通知握手失败
    NotifyInfoCallback(2, -1, 'handshake_failed');
    Exit;
  end;

  // Handshake succeeded - mark connected first so verify routines can query peer cert
  FConnected := True;

  // Strategy A: fail closed if validation fails
  if not ValidatePeerCertificate(LVerifyError) then
  begin
    FConnected := False;
    FHandshakeState := sslHsFailed;
    // P1-7: 通知验证失败
    NotifyInfoCallback(2, LVerifyError, 'verify_failed');
    Result := False;
    Exit;
  end;

  FHandshakeState := sslHsCompleted;
  // P1-7: 通知握手完成
  NotifyInfoCallback(3, 0, 'handshake_done');
  Result := True;
end;

function TWinSSLConnection.Accept: Boolean;
var
  LVerifyError: Integer;
begin
  // P1-7: 通知握手开始
  NotifyInfoCallback(1, 0, 'handshake_start');

  Result := ServerHandshake;
  if not Result then
  begin
    FConnected := False;
    FHandshakeState := sslHsFailed;
    // P1-7: 通知握手失败
    NotifyInfoCallback(2, -1, 'handshake_failed');
    Exit;
  end;

  // Handshake succeeded - mark connected first so verify routines can query peer cert
  FConnected := True;

  // Strategy A: fail closed if validation fails
  if not ValidatePeerCertificate(LVerifyError) then
  begin
    FConnected := False;
    FHandshakeState := sslHsFailed;
    // P1-7: 通知验证失败
    NotifyInfoCallback(2, LVerifyError, 'verify_failed');
    Result := False;
    Exit;
  end;

  FHandshakeState := sslHsCompleted;
  
  // P11.2: 保存会话信息到会话管理器
  SaveSessionAfterHandshake;
  
  // P1-7: 通知握手完成
  NotifyInfoCallback(3, 0, 'handshake_done');
  Result := True;
end;

function TWinSSLConnection.Shutdown: Boolean;
var
  OutBuffers: array[0..0] of TSecBuffer;
  OutBufferDesc: TSecBufferDesc;
  dwType, Status: DWORD;
  dwSSPIFlags, dwSSPIOutFlags: DWORD;
  tsExpiry: TTimeStamp;
  LCredHandle: PCredHandle;
  LSent: Integer;
begin
  Result := False;

  if not FConnected then
    Exit;

  // P0-3: 实现完整的 close_notify 发送
  // Step 1: 应用关闭控制令牌
  dwType := SCHANNEL_SHUTDOWN;

  OutBuffers[0].pvBuffer := @dwType;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer := SizeOf(dwType);

  OutBufferDesc.cBuffers := 1;
  OutBufferDesc.pBuffers := @OutBuffers[0];
  OutBufferDesc.ulVersion := SECBUFFER_VERSION;

  Status := ApplyControlToken(@FCtxtHandle, @OutBufferDesc);

  if not IsSuccess(Status) then
  begin
    // 即使 ApplyControlToken 失败，也尝试清理
    DeleteSecurityContext(@FCtxtHandle);
    FConnected := False;
    Exit;
  end;

  // Step 2: 生成 close_notify 令牌
  // 准备输出缓冲区
  OutBuffers[0].pvBuffer := nil;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer := 0;

  OutBufferDesc.cBuffers := 1;
  OutBufferDesc.pBuffers := @OutBuffers[0];
  OutBufferDesc.ulVersion := SECBUFFER_VERSION;

  // 获取凭据句柄
  LCredHandle := PCredHandle(FContext.GetNativeHandle);

  // 根据上下文类型调用相应的函数生成关闭令牌
  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
    ISC_REQ_CONFIDENTIALITY or ISC_RET_EXTENDED_ERROR or
    ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM;

  if FContext.GetContextType = sslCtxClient then
  begin
    Status := InitializeSecurityContextW(
      LCredHandle,
      @FCtxtHandle,
      nil,
      dwSSPIFlags,
      0,
      SECURITY_NATIVE_DREP,
      nil,
      0,
      @FCtxtHandle,
      @OutBufferDesc,
      @dwSSPIOutFlags,
      @tsExpiry
    );
  end
  else
  begin
    Status := AcceptSecurityContext(
      LCredHandle,
      @FCtxtHandle,
      nil,
      dwSSPIFlags,
      SECURITY_NATIVE_DREP,
      @FCtxtHandle,
      @OutBufferDesc,
      @dwSSPIOutFlags,
      @tsExpiry
    );
  end;

  // Step 3: 发送 close_notify 令牌到对端
  if (OutBuffers[0].pvBuffer <> nil) and (OutBuffers[0].cbBuffer > 0) then
  begin
    LSent := SendData(OutBuffers[0].pvBuffer^, OutBuffers[0].cbBuffer);
    // 释放 SSPI 分配的内存
    FreeContextBuffer(OutBuffers[0].pvBuffer);
    // 即使发送失败也继续清理
    if LSent <= 0 then
    begin
      // 发送失败，但仍然标记为关闭
    end;
  end;

  // Step 4: 清理安全上下文
  DeleteSecurityContext(@FCtxtHandle);
  FConnected := False;
  Result := True;
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
  // Windows Schannel 不完全支持 TLS 重协商
  // RFC 5746 要求的安全重协商在 Schannel 中实现有限
  // 建议：需要重协商时，关闭当前连接并建立新连接
  raise ESSLPlatformNotSupportedException.CreateWithContext(
    'TLS renegotiation is not supported by Windows Schannel. ' +
    'Close the connection and establish a new one instead.',
    sslErrOther,
    'TWinSSLConnection.Renegotiate',
    0,
    sslWinSSL
  );
  // Rust-quality: 删除了 raise 后不可达的 Result := False
end;

function TWinSSLConnection.PerformHandshake: TSSLHandshakeState;
var
  LHandshakeOk: Boolean;
  LVerifyError: Integer;
begin
  if FHandshakeState = sslHsCompleted then
    Exit(sslHsCompleted);

  if FHandshakeState = sslHsFailed then
    Exit(sslHsFailed);

  FHandshakeState := sslHsInProgress;
  // P1-7: 通知握手开始
  NotifyInfoCallback(1, 0, 'handshake_start');

  if FContext.GetContextType = sslCtxClient then
    LHandshakeOk := ClientHandshake
  else
    LHandshakeOk := ServerHandshake;

  if not LHandshakeOk then
  begin
    FConnected := False;
    FHandshakeState := sslHsFailed;
    // P1-7: 通知握手失败
    NotifyInfoCallback(2, -1, 'handshake_failed');
    Exit(FHandshakeState);
  end;

  // Handshake succeeded - mark connected first so verify routines can query peer cert
  FConnected := True;

  // Strategy A: fail closed if validation fails
  if not ValidatePeerCertificate(LVerifyError) then
  begin
    FConnected := False;
    FHandshakeState := sslHsFailed;
    // P1-7: 通知验证失败
    NotifyInfoCallback(2, LVerifyError, 'verify_failed');
    Exit(FHandshakeState);
  end;

  FHandshakeState := sslHsCompleted;
  // P1-7: 通知握手完成
  NotifyInfoCallback(3, 0, 'handshake_done');
  Result := FHandshakeState;
end;

// ============================================================================
// P1-1: 握手辅助方法
// ============================================================================

procedure TWinSSLConnection.PrepareInputBufferDesc(var AInBuffers: array of TSecBuffer;
  var AInBufferDesc: TSecBufferDesc; AData: Pointer; ADataSize: DWORD);
begin
  // 第一个缓冲区：实际数据
  AInBuffers[0].pvBuffer := AData;
  AInBuffers[0].cbBuffer := ADataSize;
  AInBuffers[0].BufferType := SECBUFFER_TOKEN;

  // 第二个缓冲区：用于接收额外数据
  if Length(AInBuffers) > 1 then
  begin
    AInBuffers[1].pvBuffer := nil;
    AInBuffers[1].cbBuffer := 0;
    AInBuffers[1].BufferType := SECBUFFER_EMPTY;
  end;

  // 设置描述符
  AInBufferDesc.cBuffers := Length(AInBuffers);
  AInBufferDesc.pBuffers := @AInBuffers[0];
  AInBufferDesc.ulVersion := SECBUFFER_VERSION;
end;

procedure TWinSSLConnection.PrepareOutputBufferDesc(var AOutBuffers: array of TSecBuffer;
  var AOutBufferDesc: TSecBufferDesc);
begin
  AOutBuffers[0].pvBuffer := nil;
  AOutBuffers[0].BufferType := SECBUFFER_TOKEN;
  AOutBuffers[0].cbBuffer := 0;

  AOutBufferDesc.cBuffers := 1;
  AOutBufferDesc.pBuffers := @AOutBuffers[0];
  AOutBufferDesc.ulVersion := SECBUFFER_VERSION;
end;

procedure TWinSSLConnection.HandleExtraData(var AExtraBuffer: array of TSecBuffer;
  var AIoBuffer: array of Byte; var AIoBufferSize: DWORD; AStatus: SECURITY_STATUS);
begin
  // AExtraBuffer[1] 包含未处理的额外数据
  if Length(AExtraBuffer) > 1 then
  begin
    if (AExtraBuffer[1].BufferType = SECBUFFER_EXTRA) and (AExtraBuffer[1].cbBuffer > 0) then
    begin
      // 将额外数据移动到缓冲区开始位置
      Move(AIoBuffer[AIoBufferSize - AExtraBuffer[1].cbBuffer], AIoBuffer[0], AExtraBuffer[1].cbBuffer);
      AIoBufferSize := AExtraBuffer[1].cbBuffer;
    end
    else if AStatus <> SEC_E_INCOMPLETE_MESSAGE then
      AIoBufferSize := 0;  // 只有在不需要更多数据时才清空缓冲区
  end
  else if AStatus <> SEC_E_INCOMPLETE_MESSAGE then
    AIoBufferSize := 0;
end;

function TWinSSLConnection.SendOutputBuffer(const AOutBuffer: TSecBuffer): Boolean;
var
  cbData: DWORD;
begin
  Result := True;
  if (AOutBuffer.cbBuffer > 0) and (AOutBuffer.pvBuffer <> nil) then
  begin
    cbData := SendData(AOutBuffer.pvBuffer^, AOutBuffer.cbBuffer);
    FreeContextBuffer(AOutBuffer.pvBuffer);
    if cbData <= 0 then
      Result := False;
  end;
end;

{ P1-6: 构建 ALPN 协议缓冲区
  格式: SEC_APPLICATION_PROTOCOLS
    - ProtocolListsSize: DWORD (总大小)
    - ProtocolLists: SEC_APPLICATION_PROTOCOL_LIST[]
      - ProtoNegoExt: DWORD (SecApplicationProtocolNegotiationExt_ALPN = 2)
      - ProtocolListSize: Word (协议列表大小)
      - ProtocolList: 每个协议以长度前缀 (1字节) + 协议名称

  输入格式: 逗号分隔的协议列表，如 "h2,http/1.1" }
function TWinSSLConnection.BuildALPNBuffer(const AProtocols: string; out ABuffer: TBytes): Boolean;
var
  LProtocols: TStringList;
  LTotalSize, LListSize, LOffset, I: Integer;
  LProtoLen: Byte;
begin
  Result := False;
  SetLength(ABuffer, 0);

  if AProtocols = '' then
    Exit;

  LProtocols := TStringList.Create;
  try
    LProtocols.Delimiter := ',';
    LProtocols.StrictDelimiter := True;
    LProtocols.DelimitedText := AProtocols;

    if LProtocols.Count = 0 then
      Exit;

    // 计算协议列表大小 (每个协议: 1字节长度 + 协议名称)
    LListSize := 0;
    for I := 0 to LProtocols.Count - 1 do
    begin
      if Length(LProtocols[I]) > 255 then
        Continue;  // 跳过过长的协议名
      Inc(LListSize, 1 + Length(LProtocols[I]));
    end;

    if LListSize = 0 then
      Exit;

    // 总大小: SEC_APPLICATION_PROTOCOLS header (4) + SEC_APPLICATION_PROTOCOL_LIST header (6) + 协议列表
    LTotalSize := 4 + 4 + 2 + LListSize;
    SetLength(ABuffer, LTotalSize);
    FillChar(ABuffer[0], LTotalSize, 0);

    LOffset := 0;

    // SEC_APPLICATION_PROTOCOLS.ProtocolListsSize
    PDWORD(@ABuffer[LOffset])^ := LTotalSize - 4;  // 不包括自身的 4 字节
    Inc(LOffset, 4);

    // SEC_APPLICATION_PROTOCOL_LIST.ProtoNegoExt
    PDWORD(@ABuffer[LOffset])^ := SecApplicationProtocolNegotiationExt_ALPN;
    Inc(LOffset, 4);

    // SEC_APPLICATION_PROTOCOL_LIST.ProtocolListSize
    PWord(@ABuffer[LOffset])^ := Word(LListSize);
    Inc(LOffset, 2);

    // 协议列表
    for I := 0 to LProtocols.Count - 1 do
    begin
      if Length(LProtocols[I]) > 255 then
        Continue;
      LProtoLen := Length(LProtocols[I]);
      ABuffer[LOffset] := LProtoLen;
      Inc(LOffset);
      if LProtoLen > 0 then
      begin
        Move(LProtocols[I][1], ABuffer[LOffset], LProtoLen);
        Inc(LOffset, LProtoLen);
      end;
    end;

    Result := True;
  finally
    LProtocols.Free;
  end;
end;

{ P1-7: InfoCallback 辅助方法 - 在握手状态变化时通知用户 }
procedure TWinSSLConnection.NotifyInfoCallback(AWhere: Integer; ARet: Integer; const AState: string);
var
  LCallback: TSSLInfoCallback;
begin
  LCallback := TWinSSLContext(FContext).GetInfoCallback;
  if Assigned(LCallback) then
    LCallback(AWhere, ARet, AState);
end;

{ P11.2: 会话保存辅助方法 - 在握手完成后保存会话信息 }
procedure TWinSSLConnection.SaveSessionAfterHandshake;
var
  LSession: TWinSSLSession;
  LSessionID: string;
  LProtocol: TSSLProtocolVersion;
  LCipher: string;
  LPeerCert: ISSLCertificate;
begin
  // 任务 11.2: 提取会话信息
  // 注意: Schannel 的会话复用由系统自动管理,我们只保存元数据
  
  // 生成会话 ID (使用连接句柄的地址作为唯一标识)
  LSessionID := Format('winssl-session-%p', [Pointer(@FCtxtHandle)]);
  
  // 获取协议版本
  LProtocol := GetProtocolVersion;
  
  // 获取密码套件名称
  LCipher := GetCipherName;
  
  // 获取对端证书(如果有)
  LPeerCert := GetPeerCertificate;
  
  // 任务 11.2: 创建 TWinSSLSession 对象
  LSession := TWinSSLSession.Create;
  try
    // 设置会话元数据
    LSession.SetSessionMetadata(LSessionID, LProtocol, LCipher, False);
    
    // 设置对端证书
    if LPeerCert <> nil then
      LSession.SetPeerCertificate(LPeerCert);
    
    // 保存当前会话引用
    FCurrentSession := LSession;
    
    // 任务 11.2: 添加到会话管理器
    // 注意: 这里需要一个全局或上下文级别的会话管理器
    // 由于当前架构中没有全局会话管理器,我们只保存到连接对象
    // 实际应用中,应该将会话添加到 TWinSSLContext 的会话管理器中
    
  except
    // 如果保存会话失败,不影响连接的正常使用
    // 只是无法进行会话复用
  end;
end;

function TWinSSLConnection.ValidatePeerCertificate(out AVerifyError: Integer): Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LVerifyFlags: TSSLCertVerifyFlags;
  LNeedCert: Boolean;
  LContextType: TSSLContextType;

  LCertContext: PCCERT_CONTEXT;
  LChainPara: CERT_CHAIN_PARA;
  LChainContext: PCCERT_CHAIN_CONTEXT;
  LPolicyPara: CERT_CHAIN_POLICY_PARA;
  LPolicyStatus: CERT_CHAIN_POLICY_STATUS;
  LSSLExtra: SSL_EXTRA_CERT_CHAIN_POLICY_PARA;

  LChainFlags: DWORD;
  LHostname: string;
  LServerNameW: PWideChar;
  LStatus: SECURITY_STATUS;

  function NormalizeHostForVerify(const S: string): string;
  var
    LHost: string;
    P, PEnd: SizeInt;
    PortPart: string;
    I: Integer;
  begin
    LHost := Trim(S);

    // Strip IPv6 brackets: [::1]
    if (LHost <> '') and (LHost[1] = '[') then
    begin
      PEnd := Pos(']', LHost);
      if PEnd > 0 then
        LHost := Copy(LHost, 2, PEnd - 2);
    end;

    // Strip zone id: fe80::1%eth0
    P := Pos('%', LHost);
    if P > 0 then
      LHost := Copy(LHost, 1, P - 1);

    // Strip port for the host:port case (not valid for plain IPv6 without brackets)
    if (Pos(':', LHost) > 0) and (Pos(':', LHost) = LastDelimiter(':', LHost)) then
    begin
      P := Pos(':', LHost);
      PortPart := Copy(LHost, P + 1, Length(LHost) - P);
      if PortPart <> '' then
      begin
        for I := 1 to Length(PortPart) do
          if not (PortPart[I] in ['0'..'9']) then
          begin
            PortPart := '';
            Break;
          end;
        if PortPart <> '' then
          LHost := Copy(LHost, 1, P - 1);
      end;
    end;

    Result := LHost;
  end;

begin
  AVerifyError := 0;

  if not IsValidSecHandle(FCtxtHandle) then
  begin
    AVerifyError := -1;
    Result := False;
    Exit;
  end;

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
  begin
    Result := True;
    Exit;
  end;

  LContextType := FContext.GetContextType;
  LNeedCert := (LContextType = sslCtxClient) or (sslVerifyFailIfNoPeerCert in LVerifyMode);

  LCertContext := nil;
  LStatus := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @LCertContext);
  if (not IsSuccess(LStatus)) or (LCertContext = nil) then
  begin
    if not LNeedCert then
    begin
      Result := True;
      Exit;
    end;
    AVerifyError := -1;
    Result := False;
    Exit;
  end;

  try
    LVerifyFlags := FContext.GetCertVerifyFlags;

    // Hostname verification is only meaningful for client contexts.
    if (LContextType = sslCtxClient) and
      not (sslCertVerifyIgnoreHostname in LVerifyFlags) then
    begin
      LHostname := NormalizeHostForVerify(FServerName);
      if LHostname = '' then
      begin
        AVerifyError := CERT_E_INVALID_NAME;
        Result := False;
        Exit;
      end;
    end;

    // Chain build flags
    LChainFlags := 0;
    if (sslCertVerifyCheckRevocation in LVerifyFlags) or
      (sslCertVerifyCheckOCSP in LVerifyFlags) then
      LChainFlags := LChainFlags or CERT_CHAIN_REVOCATION_CHECK_CHAIN;

    if sslCertVerifyCheckCRL in LVerifyFlags then
      LChainFlags := LChainFlags or CERT_CHAIN_REVOCATION_CHECK_END_CERT;

    // Build chain
    FillChar(LChainPara, SizeOf(LChainPara), 0);
    LChainPara.cbSize := SizeOf(CERT_CHAIN_PARA);

    // P0-2: 使用自定义 CA 存储进行证书链验证
    // 如果上下文有 CA 存储，传递给 CertGetCertificateChain
    if not CertGetCertificateChain(
      nil,
      LCertContext,
      nil,
      TWinSSLContext(FContext).GetCAStoreHandle,  // P0-2: 传入 CA 存储
      @LChainPara,
      LChainFlags,
      nil,
      @LChainContext
    ) then
    begin
      AVerifyError := GetLastError;
      Result := False;
      Exit;
    end;

    try
      // Policy parameters
      FillChar(LPolicyPara, SizeOf(LPolicyPara), 0);
      LPolicyPara.cbSize := SizeOf(CERT_CHAIN_POLICY_PARA);
      LPolicyPara.dwFlags := 0;

      if sslCertVerifyIgnoreExpiry in LVerifyFlags then
        LPolicyPara.dwFlags := LPolicyPara.dwFlags or CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG;

      if sslCertVerifyAllowSelfSigned in LVerifyFlags then
        LPolicyPara.dwFlags := LPolicyPara.dwFlags or CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG;

      if sslCertVerifyIgnoreHostname in LVerifyFlags then
        LPolicyPara.dwFlags := LPolicyPara.dwFlags or CERT_CHAIN_POLICY_IGNORE_INVALID_NAME_FLAG;

      FillChar(LSSLExtra, SizeOf(LSSLExtra), 0);
      LSSLExtra.cbSize := SizeOf(SSL_EXTRA_CERT_CHAIN_POLICY_PARA);

      if LContextType = sslCtxServer then
        LSSLExtra.dwAuthType := AUTHTYPE_CLIENT
      else
        LSSLExtra.dwAuthType := AUTHTYPE_SERVER;

      LSSLExtra.fdwChecks := 0;
      LServerNameW := nil;
      try
        if (LContextType = sslCtxClient) and
          not (sslCertVerifyIgnoreHostname in LVerifyFlags) then
        begin
          LHostname := NormalizeHostForVerify(FServerName);
          if LHostname <> '' then
            LServerNameW := StringToPWideChar(LHostname);
        end;

        LSSLExtra.pwszServerName := LServerNameW;
        LPolicyPara.pvExtraPolicyPara := @LSSLExtra;

        FillChar(LPolicyStatus, SizeOf(LPolicyStatus), 0);
        LPolicyStatus.cbSize := SizeOf(CERT_CHAIN_POLICY_STATUS);

        if not CertVerifyCertificateChainPolicy(
          CERT_CHAIN_POLICY_SSL,
          LChainContext,
          @LPolicyPara,
          @LPolicyStatus
        ) then
        begin
          AVerifyError := GetLastError;
          Result := False;
          Exit;
        end;

        if LPolicyStatus.dwError <> 0 then
        begin
          AVerifyError := Integer(LPolicyStatus.dwError);

          // P1-7: 调用验证回调，允许用户覆盖验证结果
          if Assigned(TWinSSLContext(FContext).GetVerifyCallback) then
          begin
            // 获取证书信息用于回调
            if TWinSSLContext(FContext).GetVerifyCallback(
              GetPeerCertificate.GetInfo,
              AVerifyError,
              GetVerifyResultString
            ) then
            begin
              // 回调返回 True，允许连接继续
              Result := True;
              Exit;
            end;
          end;

          Result := False;
          Exit;
        end;

        Result := True;
      finally
        if LServerNameW <> nil then
          FreePWideCharString(LServerNameW);
      end;
    finally
      CertFreeCertificateChain(LChainContext);
    end;
  finally
    CertFreeCertificateContext(LCertContext);
  end;
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
  // P1-6: ALPN 支持
  LALPNBuffer: TBytes;
  LALPNInBuffers: array[0..1] of TSecBuffer;
  LALPNInBufferDesc: TSecBufferDesc;
  LHasALPN: Boolean;
begin
  Result := False;

  // 设置标志
  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT or
                ISC_REQ_REPLAY_DETECT or
                ISC_REQ_CONFIDENTIALITY or
                ISC_RET_EXTENDED_ERROR or
                ISC_REQ_ALLOCATE_MEMORY or
                ISC_REQ_STREAM;

  ServerName := StringToPWideChar(FServerName);
  try
    // P1-6: 构建 ALPN 缓冲区
    LHasALPN := BuildALPNBuffer(FContext.GetALPNProtocols, LALPNBuffer);

    // P1-1: 使用辅助方法初始化输出缓冲区
    PrepareOutputBufferDesc(OutBuffers, OutBufferDesc);

    // P1-6: 如果有 ALPN，准备输入缓冲区
    if LHasALPN and (Length(LALPNBuffer) > 0) then
    begin
      LALPNInBuffers[0].pvBuffer := @LALPNBuffer[0];
      LALPNInBuffers[0].cbBuffer := Length(LALPNBuffer);
      LALPNInBuffers[0].BufferType := SECBUFFER_APPLICATION_PROTOCOLS;

      LALPNInBuffers[1].pvBuffer := nil;
      LALPNInBuffers[1].cbBuffer := 0;
      LALPNInBuffers[1].BufferType := SECBUFFER_EMPTY;

      LALPNInBufferDesc.cBuffers := 2;
      LALPNInBufferDesc.pBuffers := @LALPNInBuffers[0];
      LALPNInBufferDesc.ulVersion := SECBUFFER_VERSION;

      // 第一次调用 InitializeSecurityContext（带 ALPN）
      Status := InitializeSecurityContextW(
        PCredHandle(FContext.GetNativeHandle),
        nil,
        ServerName,
        dwSSPIFlags,
        0,
        0,
        @LALPNInBufferDesc,
        0,
        @FCtxtHandle,
        @OutBufferDesc,
        @dwSSPIOutFlags,
        nil
      );
    end
    else
    begin
      // 第一次调用 InitializeSecurityContext（无 ALPN）
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
    end;

    if not ((Status = SEC_I_CONTINUE_NEEDED) or IsSuccess(Status)) then
    begin
      // 任务 4.2: 第一次握手调用失败 - 增强错误处理
      FLastError := MapSchannelError(Status);
      NotifyInfoCallback(2, Integer(Status), 
        Format('Client handshake initialization failed: %s (0x%x)', 
          [GetSchannelErrorMessageEN(Status), Status]));
      
      // 根据错误类型抛出异常
      case Status of
        SEC_E_UNSUPPORTED_FUNCTION:
          raise ESSLConfigurationException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrConfiguration,
            'TWinSSLConnection.ClientHandshake',
            Status,
            sslWinSSL
          );
      else
        raise ESSLHandshakeException.CreateWithContext(
          Format('Client handshake initialization failed: %s', [GetSchannelErrorMessageEN(Status)]),
          sslErrHandshake,
          'TWinSSLConnection.ClientHandshake',
          Status,
          sslWinSSL
        );
      end;
    end;

    // P1-1: 使用辅助方法发送客户端 hello
    if not SendOutputBuffer(OutBuffers[0]) then
      Exit;

    // 继续握手循环
    cbIoBuffer := 0;
    while (Status = SEC_I_CONTINUE_NEEDED) or (Status = SEC_E_INCOMPLETE_MESSAGE) do
    begin
      // 接收服务器数据
      if (cbIoBuffer = 0) or (Status = SEC_E_INCOMPLETE_MESSAGE) then
      begin
        cbData := RecvData(IoBuffer[cbIoBuffer], SizeOf(IoBuffer) - cbIoBuffer);
        if cbData <= 0 then
          Exit;
        Inc(cbIoBuffer, cbData);
      end;

      // P1-1: 使用辅助方法设置输入缓冲区
      PrepareInputBufferDesc(InBuffers, InBufferDesc, @IoBuffer[0], cbIoBuffer);

      // P1-1: 使用辅助方法设置输出缓冲区
      PrepareOutputBufferDesc(OutBuffers, OutBufferDesc);

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

      // P1-1: 使用辅助方法处理额外数据
      HandleExtraData(InBuffers, IoBuffer, cbIoBuffer, Status);

      // P1-1: 使用辅助方法发送响应数据
      if not SendOutputBuffer(OutBuffers[0]) then
        Exit;

      // 检查状态
      if Status = SEC_E_INCOMPLETE_MESSAGE then
        Continue;  // 需要更多数据，继续循环
      if not ((Status = SEC_I_CONTINUE_NEEDED) or IsSuccess(Status)) then
      begin
        // 任务 4.2: 握手循环中的错误处理
        FLastError := MapSchannelError(Status);
        NotifyInfoCallback(2, Integer(Status), 
          Format('Client handshake failed: %s (0x%x)', 
            [GetSchannelErrorMessageEN(Status), Status]));
        
        // 根据错误类型抛出异常
        case Status of
          SEC_E_CERT_EXPIRED,
          CERT_E_EXPIRED:
            raise ESSLCertificateException.CreateWithContext(
              GetSchannelErrorMessageEN(Status),
              sslErrCertificate,
              'TWinSSLConnection.ClientHandshake',
              Status,
              sslWinSSL
            );
            
          SEC_E_UNTRUSTED_ROOT,
          CERT_E_UNTRUSTEDROOT:
            raise ESSLCertificateException.CreateWithContext(
              GetSchannelErrorMessageEN(Status),
              sslErrCertificateUntrusted,
              'TWinSSLConnection.ClientHandshake',
              Status,
              sslWinSSL
            );
            
          SEC_E_ALGORITHM_MISMATCH:
            raise ESSLHandshakeException.CreateWithContext(
              GetSchannelErrorMessageEN(Status),
              sslErrHandshake,
              'TWinSSLConnection.ClientHandshake',
              Status,
              sslWinSSL
            );
            
          SEC_E_INVALID_TOKEN:
            raise ESSLProtocolException.CreateWithContext(
              GetSchannelErrorMessageEN(Status),
              sslErrProtocol,
              'TWinSSLConnection.ClientHandshake',
              Status,
              sslWinSSL
            );
            
        else
          raise ESSLHandshakeException.CreateWithContext(
            Format('Client handshake failed: %s', [GetSchannelErrorMessageEN(Status)]),
            sslErrHandshake,
            'TWinSSLConnection.ClientHandshake',
            Status,
            sslWinSSL
          );
        end;
      end;
    end;

    Result := IsSuccess(Status);

  finally
    if ServerName <> nil then
      FreePWideCharString(ServerName);
  end;
end;

// ============================================================================
// 握手实现 - 服务器端
// WinSSL 服务端支持 - 任务 3.1: 实现服务端握手
// ============================================================================

function TWinSSLConnection.ServerHandshake: Boolean;
var
  OutBuffers: array[0..0] of TSecBuffer;
  OutBufferDesc: TSecBufferDesc;
  InBuffers: array[0..1] of TSecBuffer;
  InBufferDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  dwSSPIFlags, dwSSPIOutFlags: DWORD;
  cbData, cbIoBuffer: DWORD;
  IoBuffer: array[0..16384-1] of Byte;
  fDoRead: Boolean;
  phContext: PCtxtHandle;
begin
  Result := False;

  // 任务 3.1: 设置服务器端标志
  dwSSPIFlags := ASC_REQ_SEQUENCE_DETECT or
                 ASC_REQ_REPLAY_DETECT or
                 ASC_REQ_CONFIDENTIALITY or
                 ASC_RET_EXTENDED_ERROR or
                 ASC_REQ_ALLOCATE_MEMORY or
                 ASC_REQ_STREAM;

  // 任务 3.1: 如果启用了对端验证,请求客户端证书
  if sslVerifyPeer in FContext.GetVerifyMode then
    dwSSPIFlags := dwSSPIFlags or ASC_REQ_MUTUAL_AUTH;

  // 初始化握手状态
  FHandshakeState := sslHsInProgress;
  cbIoBuffer := 0;
  fDoRead := True;
  phContext := nil;  // 第一次调用时为 nil

  // 任务 3.1: 握手主循环 - 处理握手消息直到完成或失败
  while True do
  begin
    // 任务 3.1: 接收客户端数据(如果需要)
    if fDoRead then
    begin
      cbData := RecvData(IoBuffer[cbIoBuffer], SizeOf(IoBuffer) - cbIoBuffer);
      if cbData <= 0 then
      begin
        // 网络错误或连接关闭
        FLastError := sslErrConnection;
        Exit;
      end;
      Inc(cbIoBuffer, cbData);
    end
    else
      fDoRead := True;  // 下次循环需要读取

    // 任务 3.1: 准备输入缓冲区(客户端 Hello 或其他握手消息)
    PrepareInputBufferDesc(InBuffers, InBufferDesc, @IoBuffer[0], cbIoBuffer);

    // 任务 3.1: 准备输出缓冲区(服务端响应)
    PrepareOutputBufferDesc(OutBuffers, OutBufferDesc);

    // 任务 3.1: 调用 AcceptSecurityContext 处理握手
    Status := AcceptSecurityContextW(
      PCredHandle(FContext.GetNativeHandle),
      phContext,
      @InBufferDesc,
      dwSSPIFlags,
      SECURITY_NATIVE_DREP,
      @FCtxtHandle,
      @OutBufferDesc,
      @dwSSPIOutFlags,
      nil
    );

    // 第一次调用后,使用已创建的上下文
    if phContext = nil then
      phContext := @FCtxtHandle;

    // 任务 3.1: 处理额外数据(SECBUFFER_EXTRA)
    HandleExtraData(InBuffers, IoBuffer, cbIoBuffer, Status);

    // 任务 3.1: 发送服务端响应消息到客户端
    if (OutBuffers[0].cbBuffer > 0) and (OutBuffers[0].pvBuffer <> nil) then
    begin
      if not SendOutputBuffer(OutBuffers[0]) then
      begin
        FLastError := sslErrConnection;
        Exit;
      end;
    end;

    // 任务 3.1: 检查握手状态
    case Status of
      SEC_E_OK:
      begin
        // 握手成功完成
        Result := True;
        FHandshakeState := sslHsCompleted;
        Break;
      end;

      SEC_I_CONTINUE_NEEDED:
      begin
        // 需要继续握手,循环继续
        Continue;
      end;

      SEC_E_INCOMPLETE_MESSAGE:
      begin
        // 消息不完整,需要更多数据
        // 不清空缓冲区,继续读取
        fDoRead := True;
        Continue;
      end;

      SEC_I_INCOMPLETE_CREDENTIALS:
      begin
        // 凭据不完整(可能需要客户端证书)
        // 继续握手
        Continue;
      end;

    else
      // 任务 4.2: 握手失败 - 增强错误处理
      FHandshakeState := sslHsFailed;
      
      // 任务 4.2: 使用 MapSchannelError 映射错误类型
      FLastError := MapSchannelError(Status);
      
      // 任务 4.2: 记录详细的错误信息
      NotifyInfoCallback(2, Integer(Status), 
        Format('Server handshake failed: %s (0x%x)', 
          [GetSchannelErrorMessageEN(Status), Status]));
      
      // 任务 4.2: 发送 TLS 警报消息(如果适用)
      // 注意: Schannel 会自动发送某些警报消息,但我们可以尝试显式发送
      // 对于某些错误,Schannel 已经在 AcceptSecurityContext 中发送了警报
      // 这里我们主要确保上下文被正确清理
      if IsValidSecHandle(FCtxtHandle) then
      begin
        // 尝试发送关闭通知(如果连接仍然有效)
        // 这会触发 Schannel 发送 close_notify 警报
        try
          DeleteSecurityContext(@FCtxtHandle);
          InitSecHandle(FCtxtHandle);
        except
          // 忽略清理错误
        end;
      end;
      
      // 任务 4.2: 根据错误类型抛出相应的异常
      case Status of
        SEC_E_UNSUPPORTED_FUNCTION:
          raise ESSLConfigurationException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrConfiguration,
            'TWinSSLConnection.ServerHandshake',
            Status,
            sslWinSSL
          );
          
        SEC_E_CERT_EXPIRED,
        CERT_E_EXPIRED:
          raise ESSLCertificateException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrCertificate,
            'TWinSSLConnection.ServerHandshake',
            Status,
            sslWinSSL
          );
          
        SEC_E_UNTRUSTED_ROOT,
        CERT_E_UNTRUSTEDROOT,
        SEC_E_CERT_UNKNOWN:
          raise ESSLCertificateException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrCertificateUntrusted,
            'TWinSSLConnection.ServerHandshake',
            Status,
            sslWinSSL
          );
          
        SEC_E_ALGORITHM_MISMATCH:
          raise ESSLHandshakeException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrHandshake,
            'TWinSSLConnection.ServerHandshake',
            Status,
            sslWinSSL
          );
          
        SEC_E_INVALID_TOKEN:
          raise ESSLProtocolException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrProtocol,
            'TWinSSLConnection.ServerHandshake',
            Status,
            sslWinSSL
          );
          
        SEC_E_MESSAGE_ALTERED:
          raise ESSLProtocolException.CreateWithContext(
            GetSchannelErrorMessageEN(Status),
            sslErrProtocol,
            'TWinSSLConnection.ServerHandshake',
            Status,
            sslWinSSL
          );
          
      else
        // 通用握手错误
        raise ESSLHandshakeException.CreateWithContext(
          Format('Server handshake failed: %s', [GetSchannelErrorMessageEN(Status)]),
          sslErrHandshake,
          'TWinSSLConnection.ServerHandshake',
          Status,
          sslWinSSL
        );
      end;
    end;
  end;
end;

// ============================================================================
// ISSLConnection - 数据传输
// ============================================================================

function TWinSSLConnection.Read(var ABuffer; ACount: Integer): Integer;
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
    Result := Min(ACount, FDecryptedBufferUsed);
    Move(FDecryptedBuffer[0], ABuffer, Result);
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
      Result := Min(ACount, Integer(InBuffers[i].cbBuffer));
      Move(InBuffers[i].pvBuffer^, ABuffer, Result);
      
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

function TWinSSLConnection.Write(const ABuffer; ACount: Integer): Integer;
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
  cbMessage := StreamSizes.cbHeader + ACount + StreamSizes.cbTrailer;
  SetLength(Message, cbMessage);
  
  // 设置缓冲区
  OutBuffers[0].pvBuffer := @Message[0];
  OutBuffers[0].cbBuffer := StreamSizes.cbHeader;
  OutBuffers[0].BufferType := SECBUFFER_STREAM_HEADER;
  
  OutBuffers[1].pvBuffer := @Message[StreamSizes.cbHeader];
  OutBuffers[1].cbBuffer := ACount;
  OutBuffers[1].BufferType := SECBUFFER_DATA;
  Move(ABuffer, OutBuffers[1].pvBuffer^, ACount);
  
  OutBuffers[2].pvBuffer := @Message[StreamSizes.cbHeader + ACount];
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
    Result := ACount;
end;

function TWinSSLConnection.ReadString(out AStr: string): Boolean;
var
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
begin
  BytesRead := Read(Buffer, SizeOf(Buffer));
  Result := BytesRead > 0;
  if Result then
    SetString(AStr, PChar(@Buffer[0]), BytesRead);
end;

function TWinSSLConnection.WriteString(const AStr: string): Boolean;
begin
  Result := Write(PChar(AStr)^, Length(AStr)) = Length(AStr);
end;

// ============================================================================
// ISSLConnection - 异步操作支持
// ============================================================================

function TWinSSLConnection.WantRead: Boolean;
begin
  // 返回最后一次操作是否需要读取更多数据
  // 与 OpenSSL 的 SSL_want() 行为一致
  Result := (FLastError = sslErrWantRead);
end;

function TWinSSLConnection.WantWrite: Boolean;
begin
  // 返回最后一次操作是否需要写入更多数据
  // 与 OpenSSL 的 SSL_want() 行为一致
  Result := (FLastError = sslErrWantWrite);
end;

function TWinSSLConnection.GetError(ARet: Integer): TSSLErrorCode;
var
  LastErr: DWORD;
  {$IFDEF WINDOWS}
  WsaErr: Integer;
  {$ENDIF}
begin
  // 成功或非错误情况
  if ARet >= 0 then
  begin
    FLastError := sslErrNone;
    Result := sslErrNone;
    Exit;
  end;

  {$IFDEF WINDOWS}
  // P1-8: 对 Winsock 操作使用 WSAGetLastError
  // Winsock 错误码与 GetLastError 分开存储
  WsaErr := WSAGetLastError;

  // 首先检查 Winsock 错误
  case WsaErr of
    WSAEWOULDBLOCK:
    begin
      FLastError := sslErrWantRead;
      Result := sslErrWantRead;
      Exit;
    end;

    WSAENOTCONN,
    WSAECONNRESET,
    WSAECONNABORTED:
    begin
      FLastError := sslErrConnection;
      Result := sslErrConnection;
      Exit;
    end;

    WSAETIMEDOUT:
    begin
      FLastError := sslErrTimeout;
      Result := sslErrTimeout;
      Exit;
    end;
  end;
  {$ENDIF}

  // 获取 Windows/SSPI 错误码
  LastErr := GetLastError;

  // 映射到 SSL 错误码
  case LastErr of
    {$IFDEF WINDOWS}
    ERROR_IO_PENDING:
      Result := sslErrWantRead;  // 非阻塞操作需要等待

    ERROR_NOT_CONNECTED:
      Result := sslErrConnection;
    {$ENDIF}

    SEC_E_INCOMPLETE_MESSAGE:
      Result := sslErrWantRead;  // 需要更多数据

    SEC_I_CONTINUE_NEEDED,
    SEC_I_INCOMPLETE_CREDENTIALS:
      Result := sslErrWantWrite;  // 需要发送更多数据
  else
    Result := sslErrOther;
  end;

  // 保存最后错误状态，供 WantRead/WantWrite 使用
  FLastError := Result;
end;



// ============================================================================
// ISSLConnection - 连接信息
// ============================================================================

function TWinSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
var
  ConnInfo: TSecPkgContext_ConnectionInfo;
  Status: SECURITY_STATUS;
  PeerCert: ISSLCertificate;
begin
  FillChar(Result, SizeOf(Result), 0);

  // 基本信息
  Result.ProtocolVersion := GetProtocolVersion;
  Result.CipherSuite := GetCipherName;

  if not FConnected then
    Exit;

  // 查询 Schannel 连接信息
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_CONNECTION_INFO, @ConnInfo);
  if IsSuccess(Status) then
  begin
    Result.CipherSuiteId := Word(ConnInfo.aiCipher);
    Result.KeySize := ConnInfo.dwCipherStrength;
    Result.MacSize := ConnInfo.dwHashStrength div 8;

    // 密钥交换算法映射（尽力而为）
    case ConnInfo.aiExch of
      CALG_RSA_KEYX, CALG_RSA_SIGN:
        Result.KeyExchange := sslKexRSA;
      CALG_DH_EPHEM:
        Result.KeyExchange := sslKexDHE_RSA;
    else
      Result.KeyExchange := sslKexRSA;
    end;

    // 加密算法映射
    case ConnInfo.aiCipher of
      CALG_AES_128: Result.Cipher := sslCipherAES128;
      CALG_AES_256: Result.Cipher := sslCipherAES256;
      CALG_3DES:    Result.Cipher := sslCipher3DES;
      CALG_DES:     Result.Cipher := sslCipherDES;
      CALG_RC4:     Result.Cipher := sslCipherRC4;
    else
      Result.Cipher := sslCipherNone;
    end;

    // 哈希算法映射
    case ConnInfo.aiHash of
      CALG_MD5:     Result.Hash := sslHashMD5;
      CALG_SHA1:    Result.Hash := sslHashSHA1;
      CALG_SHA_256: Result.Hash := sslHashSHA256;
      CALG_SHA_384: Result.Hash := sslHashSHA384;
      CALG_SHA_512: Result.Hash := sslHashSHA512;
    else
      Result.Hash := sslHashSHA256;
    end;
  end;

  Result.IsResumed := FSessionReused;
  Result.CompressionMethod := 'none';
  Result.ServerName := FServerName;
  Result.ALPNProtocol := GetSelectedALPNProtocol;

  PeerCert := GetPeerCertificate;
  if PeerCert <> nil then
    Result.PeerCertificate := PeerCert.GetInfo;
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
  CipherName, HashName: string;
begin
  Result := '';

  if not FConnected then
    Exit;

  // 查询连接信息
  Status := QueryContextAttributesW(@FCtxtHandle, SECPKG_ATTR_CONNECTION_INFO, @ConnInfo);
  if not IsSuccess(Status) then
    Exit;

  // 将 ALG_ID 映射到标准密码套件名称
  case ConnInfo.aiCipher of
    CALG_AES_256: CipherName := 'AES_256';
    CALG_AES_192: CipherName := 'AES_192';
    CALG_AES_128: CipherName := 'AES_128';
    CALG_3DES:    CipherName := '3DES';
    CALG_DES:     CipherName := 'DES';
    CALG_RC4:     CipherName := 'RC4';
  else
    CipherName := Format('0x%x', [ConnInfo.aiCipher]);
  end;

  // 映射哈希算法
  case ConnInfo.aiHash of
    CALG_SHA_512: HashName := 'SHA512';
    CALG_SHA_384: HashName := 'SHA384';
    CALG_SHA_256: HashName := 'SHA256';
    CALG_SHA1:    HashName := 'SHA';
    CALG_MD5:     HashName := 'MD5';
  else
    HashName := '';
  end;

  // 构建类似 OpenSSL 的格式
  if HashName <> '' then
    Result := Format('%s_%s (%d bits)', [CipherName, HashName, ConnInfo.dwCipherStrength])
  else
    Result := Format('%s (%d bits)', [CipherName, ConnInfo.dwCipherStrength]);
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
  LVerifyError: Integer;
begin
  if ValidatePeerCertificate(LVerifyError) then
    Result := 0
  else
    Result := LVerifyError;
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
// ISSLConnection - 会话管理
// ============================================================================

function TWinSSLConnection.GetSession: ISSLSession;
var
  LSession: TWinSSLSession;
  LPeerCert: ISSLCertificate;
begin
  // 如果已有缓存的会话，直接返回
  if FCurrentSession <> nil then
  begin
    Result := FCurrentSession;
    Exit;
  end;

  // 如果未连接，无法获取会话
  if not FConnected then
  begin
    Result := nil;
    Exit;
  end;

  // P1.2: 动态创建会话（类似 OpenSSL 的 SSL_get1_session）
  LSession := TWinSSLSession.Create;
  LSession.FID := FormatDateTime('yyyymmddhhnnsszzz', Now);  // 唯一标识符
  LSession.FCreationTime := Now;
  LSession.FTimeout := 300;  // 默认 5 分钟
  LSession.FProtocolVersion := GetProtocolVersion;
  LSession.FCipherName := GetCipherName;

  // 保存上下文句柄
  if IsValidSecHandle(FCtxtHandle) then
    LSession.SetSessionHandle(FCtxtHandle);

  // P1.2: 缓存对端证书
  LPeerCert := GetPeerCertificate;
  if LPeerCert <> nil then
    LSession.SetPeerCertificate(LPeerCert);

  FCurrentSession := LSession;
  Result := LSession;
end;

procedure TWinSSLConnection.SetSession(ASession: ISSLSession);
begin
  FCurrentSession := ASession;
  if ASession <> nil then
  begin
    FSessionReused := True;
    // 如果有上下文句柄，存储到会话中
    if IsValidSecHandle(FCtxtHandle) and (ASession is TWinSSLSession) then
      TWinSSLSession(ASession).SetSessionHandle(FCtxtHandle);
  end;
end;

function TWinSSLConnection.IsSessionReused: Boolean;
begin
  Result := FSessionReused;
end;

// ============================================================================
// ISSLConnection - ALPN/NPN
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

procedure TWinSSLConnection.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TWinSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWinSSLConnection.SetBlocking(ABlocking: Boolean);
begin
  FBlocking := ABlocking;
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
