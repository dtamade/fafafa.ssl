{**
 * Unit: fafafa.ssl.factory
 * Purpose: SSL/TLS 库工厂模式 - 统一创建和管理SSL对象
 *
 * Features:
 * - 自动检测可用的SSL后端（OpenSSL、WinSSL等）
 * - 工厂模式创建SSL对象（Context、Certificate、Store等）
 * - 多后端支持与自动切换
 * - 简化的单一入口API
 *
 * Thread Safety: 所有类方法线程安全
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-09-28
 *
 * @example
 * <code>
 *   // 自动检测并创建SSL Context
 *   LCtx := TSSLFactory.CreateContext(sslClient);
 *
 *   // 指定使用OpenSSL后端
 *   LCtx := TSSLFactory.CreateContext(sslClient, sslOpenSSL);
 *
 *   // 使用配置对象创建
 *   LConfig := TSSLConfig.Create;
 *   LConfig.ContextType := sslClient;
 *   LCtx := TSSLFactory.CreateContext(LConfig);
 * </code>
 *}

unit fafafa.ssl.factory;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,  // 新增：类型化异常
  fafafa.ssl.logging,
  fafafa.ssl.collections; // P0: 可替换的 Map 接口

type
  {** SSL库类类型 (用于内部注册) *}
  TSSLLibraryClass = class of TInterfacedObject;

  {**
   * SSL库注册信息
   *
   * 用于后端库的注册和管理
   *}
  TSSLLibraryRegistration = record
    LibraryType: TSSLLibraryType;      // 库类型标识
    LibraryClass: TSSLLibraryClass;    // 库类（必须实现 ISSLLibrary）
    Description: string;                // 库描述
    Priority: Integer;                  // 优先级（数字越大越优先）
  end;

  {**
   * TSSLFactory - SSL工厂类
   *
   * 提供统一的SSL对象创建接口，支持多后端自动选择。
   *
   * 主要功能:
   * - 创建SSL Context (客户端/服务器)
   * - 创建证书和证书存储对象
   * - 后端库检测和切换
   * - 库注册和管理
   *
   * 使用模式:
   * <code>
   *   // 最简单：自动检测后端
   *   LCtx := TSSLFactory.CreateContext(sslClient);
   *
   *   // 指定后端
   *   LCtx := TSSLFactory.CreateContext(sslClient, sslOpenSSL);
   *
   *   // 高级：使用配置
   *   LCtx := TSSLFactory.CreateContext(LConfig);
   * </code>
   *}
  TSSLFactory = class
  private
    class var
      // P0: 使用 Map 接口替代动态数组，方便后续替换为 fafafa.core 的 HashMap
      FRegistrationMap: specialize IIntegerMap<TSSLLibraryRegistration>;
      FLibraries: array[TSSLLibraryType] of ISSLLibrary;
      FDefaultLibraryType: TSSLLibraryType;
      FInitialized: Boolean;
      FAutoInitialize: Boolean;
      
    class procedure Initialize;
    class procedure Finalize;
    class function CreateLibraryInstance(ALibType: TSSLLibraryType): ISSLLibrary;
    class procedure CheckInitialized;
  public
    { ==================== 库访问（高级用法） ==================== }

    {**
     * 获取指定类型的库实例
     *
     * @param ALibType 库类型（sslOpenSSL, sslWinSSL等）
     * @return 库实例接口
     * @raises ESSLConfigurationException 库不可用时
     *
     * 注意: 普通用户通常不需要直接使用此方法
     *}
    class function GetLibrary(ALibType: TSSLLibraryType): ISSLLibrary;

    { ==================== 库注册（后端开发者使用） ==================== }

    {**
     * 注册SSL后端库
     *
     * @param ALibType 库类型标识
     * @param ALibraryClass 库类（必须实现 ISSLLibrary）
     * @param ADescription 库描述（可选）
     * @param APriority 优先级（默认0，数字越大越优先）
     *
     * 注意: 此方法供后端实现的初始化代码调用
     *}
    class procedure RegisterLibrary(
      ALibType: TSSLLibraryType;
      ALibraryClass: TSSLLibraryClass;
      const ADescription: string = '';
      APriority: Integer = 0
    );

    {**
     * 取消注册SSL后端库
     *
     * @param ALibType 库类型标识
     *}
    class procedure UnregisterLibrary(ALibType: TSSLLibraryType);

    { ==================== 库检测与查询 ==================== }

    {**
     * 检查指定库是否可用
     *
     * @param ALibType 库类型
     * @return True=可用，False=不可用
     *
     * @example
     * <code>
     *   if TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
     *     WriteLn('OpenSSL is available');
     * </code>
     *}
    class function IsLibraryAvailable(ALibType: TSSLLibraryType): Boolean;

    {**
     * 获取所有可用的库列表
     *
     * @return 可用库类型集合
     *
     * @example
     * <code>
     *   LLibs := TSSLFactory.GetAvailableLibraries;
     *   for LLib in LLibs do
     *     WriteLn(TSSLFactory.GetLibraryDescription(LLib));
     * </code>
     *}
    class function GetAvailableLibraries: TSSLLibraryTypes;

    {**
     * 获取库的描述信息
     *
     * @param ALibType 库类型
     * @return 库描述字符串
     *}
    class function GetLibraryDescription(ALibType: TSSLLibraryType): string;

    {**
     * 自动检测最佳可用库
     *
     * 根据平台和可用性选择最适合的库:
     * - Windows: WinSSL（如果可用）> OpenSSL
     * - Linux/macOS: OpenSSL
     *
     * @return 最佳库类型
     * @raises ESSLConfigurationException 无可用库时
     *}
    class function DetectBestLibrary: TSSLLibraryType;

    { ==================== 默认库管理 ==================== }

    {**
     * 设置默认使用的SSL库
     *
     * @param ALibType 库类型
     * @raises ESSLConfigurationException 库不可用时
     *
     * @example
     * <code>
     *   TSSLFactory.SetDefaultLibrary(sslOpenSSL);
     * </code>
     *}
    class procedure SetDefaultLibrary(ALibType: TSSLLibraryType);

    {**
     * 获取当前默认库
     *
     * @return 默认库类型
     *}
    class function GetDefaultLibrary: TSSLLibraryType;

    { ==================== 对象创建（主要API） ==================== }

    {**
     * 创建SSL Context
     *
     * @param AContextType 上下文类型（sslClient或sslServer）
     * @param ALibType 库类型（默认sslAutoDetect自动选择）
     * @return SSL Context接口
     * @raises ESSLInitializationException 初始化失败时
     *
     * @example
     * <code>
     *   // 自动检测库
     *   LCtx := TSSLFactory.CreateContext(sslClient);
     *
     *   // 指定使用OpenSSL
     *   LCtx := TSSLFactory.CreateContext(sslServer, sslOpenSSL);
     * </code>
     *}
    class function CreateContext(
      AContextType: TSSLContextType;
      ALibType: TSSLLibraryType = sslAutoDetect
    ): ISSLContext; overload;

    {**
     * 使用配置对象创建SSL Context
     *
     * @param AConfig SSL配置对象
     * @return SSL Context接口
     * @raises ESSLInitializationException 初始化失败时
     *
     * @example
     * <code>
     *   LConfig := TSSLConfig.Create;
     *   LConfig.ContextType := sslClient;
     *   LConfig.ProtocolVersions := [sslTLS12, sslTLS13];
     *   LCtx := TSSLFactory.CreateContext(LConfig);
     * </code>
     *}
    class function CreateContext(const AConfig: TSSLConfig): ISSLContext; overload;

    class procedure NormalizeConfig(var AConfig: TSSLConfig);

    {**
     * 创建证书对象
     *
     * @param ALibType 库类型（默认自动检测）
     * @return 证书接口
     *}
    class function CreateCertificate(ALibType: TSSLLibraryType = sslAutoDetect): ISSLCertificate;

    {**
     * 创建证书存储对象
     *
     * @param ALibType 库类型（默认自动检测）
     * @return 证书存储接口
     *}
    class function CreateCertificateStore(ALibType: TSSLLibraryType = sslAutoDetect): ISSLCertificateStore;
    
    // 快捷方法 - 简化的服务端上下文
    class function CreateServerContext(const ACertFile, AKeyFile: string;
                                      ALibType: TSSLLibraryType = sslAutoDetect): ISSLContext;
    
    // 库管理
    class function GetLibraryInstance(ALibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
    class procedure ReleaseLibrary(ALibType: TSSLLibraryType);
    class procedure ReleaseAllLibraries;
    
    // 全局配置
    class procedure SetAutoInitialize(AValue: Boolean);
    class function GetAutoInitialize: Boolean;
    
    // 版本和信息
    class function GetVersionInfo: string;
    class function GetSystemInfo: string;
    
    // 属性 - 注释掉 class property，使用方法代替
    // class property DefaultLibrary: TSSLLibraryType read GetDefaultLibrary write SetDefaultLibrary;
    // class property AutoInitialize: Boolean read GetAutoInitialize write SetAutoInitialize;
    // class property AvailableLibraries: TSSLLibraryTypes read GetAvailableLibraries;
  end;

  { TSSLHelper - SSL辅助类，提供更简单的API }
  TSSLHelper = class
  public
    // 证书验证
    class function VerifyCertificateFile(const AFileName: string): Boolean;
    class function GetCertificateInfo(const AFileName: string): TSSLCertificateInfo;
    
    // 工具方法
    class function GenerateRandomBytes(ACount: Integer): TBytes;
    class function HashData(const AData: TBytes; AHashType: TSSLHash): string;
  end;

  { 全局函数 - 便捷接口 }
  function SSLFactory: TSSLFactory;
    deprecated 'Use TSSLFactory class methods directly (no instance needed)';
  function SSLHelper: TSSLHelper;
    deprecated 'Use TSSLHelper class methods directly (no instance needed)';

  // 快速创建函数
  function CreateSSLLibrary(ALibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
    deprecated 'Use TSSLFactory.GetLibraryInstance(...)';
  function CreateSSLContext(AType: TSSLContextType = sslCtxClient): ISSLContext;
    deprecated 'Use TSSLFactory.CreateContext(...) or fafafa.ssl.context.builder';
  function CreateSSLCertificate: ISSLCertificate;
    deprecated 'Use TSSLFactory.CreateCertificate(...)';
  function CreateSSLConnection(AContext: ISSLContext; ASocket: THandle): ISSLConnection;
    deprecated 'Use AContext.CreateConnection(...) and per-connection SNI via ISSLClientConnection';

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  fafafa.ssl.errors,
  fafafa.ssl.random;         // Phase 3.3 P1 - 平台无关的加密安全随机数生成
                             // 修复严重设计缺陷: 移除对 OpenSSL 模块的无条件依赖
                             // 这样 WinSSL 用户可以完全独立于 OpenSSL 使用本库
                             //
                             // 移除的依赖:
                             // - fafafa.ssl.openssl.api.rand (导致加载 OpenSSL DLL)
                             // - fafafa.ssl.openssl.errors (导致加载 OpenSSL DLL)
                             // - fafafa.ssl.crypto.utils (依赖 OpenSSL EVP)
                             // - fafafa.ssl.encoding (依赖 OpenSSL)

var
  GSSLFactory: TSSLFactory;
  GSSLHelper: TSSLHelper;
  GFactoryLock: TRTLCriticalSection;  // 工厂类的全局锁
  GFactoryLockInitialized: Boolean = False;

procedure NormalizeConfigOptions(var AConfig: TSSLConfig);
begin
  if AConfig.EnableCompression then
    Exclude(AConfig.Options, ssoDisableCompression)
  else
    Include(AConfig.Options, ssoDisableCompression);

  Include(AConfig.Options, ssoDisableRenegotiation);

  Include(AConfig.Options, ssoNoSSLv2);
  Include(AConfig.Options, ssoNoSSLv3);
  Include(AConfig.Options, ssoNoTLSv1);
  Include(AConfig.Options, ssoNoTLSv1_1);

  if AConfig.ProtocolVersions <> [] then
  begin
    if sslProtocolSSL2 in AConfig.ProtocolVersions then
      Exclude(AConfig.Options, ssoNoSSLv2)
    else
      Include(AConfig.Options, ssoNoSSLv2);

    if sslProtocolSSL3 in AConfig.ProtocolVersions then
      Exclude(AConfig.Options, ssoNoSSLv3)
    else
      Include(AConfig.Options, ssoNoSSLv3);

    if sslProtocolTLS10 in AConfig.ProtocolVersions then
      Exclude(AConfig.Options, ssoNoTLSv1)
    else
      Include(AConfig.Options, ssoNoTLSv1);

    if sslProtocolTLS11 in AConfig.ProtocolVersions then
      Exclude(AConfig.Options, ssoNoTLSv1_1)
    else
      Include(AConfig.Options, ssoNoTLSv1_1);

    if sslProtocolTLS12 in AConfig.ProtocolVersions then
      Exclude(AConfig.Options, ssoNoTLSv1_2)
    else
      Include(AConfig.Options, ssoNoTLSv1_2);

    if sslProtocolTLS13 in AConfig.ProtocolVersions then
      Exclude(AConfig.Options, ssoNoTLSv1_3)
    else
      Include(AConfig.Options, ssoNoTLSv1_3);
  end;

  if AConfig.EnableSessionTickets then
    Include(AConfig.Options, ssoEnableSessionTickets)
  else
    Exclude(AConfig.Options, ssoEnableSessionTickets);

  if AConfig.EnableOCSPStapling then
    Include(AConfig.Options, ssoEnableOCSPStapling)
  else
    Exclude(AConfig.Options, ssoEnableOCSPStapling);

  // 设置会话超时默认值
  if AConfig.SessionTimeout <= 0 then
    AConfig.SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;

  // 设置会话缓存大小默认值
  if AConfig.SessionCacheSize <= 0 then
    AConfig.SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;

  if AConfig.SessionCacheSize > 0 then
    Include(AConfig.Options, ssoEnableSessionCache)
  else
    Exclude(AConfig.Options, ssoEnableSessionCache);

  if AConfig.VerifyDepth <= 0 then
    AConfig.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;

  if AConfig.CipherList = '' then
    AConfig.CipherList := SSL_DEFAULT_CIPHER_LIST;

  if AConfig.CipherSuites = '' then
    AConfig.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
end;

class procedure TSSLFactory.NormalizeConfig(var AConfig: TSSLConfig);
begin
  NormalizeConfigOptions(AConfig);
end;

{ 全局函数实现 }

function SSLFactory: TSSLFactory;
begin
  if GSSLFactory = nil then
    GSSLFactory := TSSLFactory.Create;
  Result := GSSLFactory;
end;

function SSLHelper: TSSLHelper;
begin
  if GSSLHelper = nil then
    GSSLHelper := TSSLHelper.Create;
  Result := GSSLHelper;
end;

function CreateSSLLibrary(ALibType: TSSLLibraryType): ISSLLibrary;
begin
  // 直接使用工厂获取库实例
  // 这会自动检测最佳库（如果 ALibType = sslAutoDetect）
  // 并自动初始化库
  Result := TSSLFactory.GetLibraryInstance(ALibType);
end;

function CreateSSLContext(AType: TSSLContextType): ISSLContext;
begin
  Result := TSSLFactory.CreateContext(AType);
end;

function CreateSSLCertificate: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificate;
end;

function CreateSSLConnection(AContext: ISSLContext; ASocket: THandle): ISSLConnection;
begin
  if Assigned(AContext) then
    Result := AContext.CreateConnection(ASocket)
  else
    RaiseNotInitialized('SSL context');
end;

{ TSSLFactory }

class procedure TSSLFactory.Initialize;
begin
  if not FInitialized then
  begin
    // 初始化全局锁
    if not GFactoryLockInitialized then
    begin
      InitCriticalSection(GFactoryLock);
      GFactoryLockInitialized := True;
    end;
    // P0: 创建 Map 实例（后续可替换为 fafafa.core 的 HashMap）
    FRegistrationMap := TMapFactory.specialize CreateIntegerMap<TSSLLibraryRegistration>;
    FDefaultLibraryType := sslAutoDetect;
    FAutoInitialize := True;
    FInitialized := True;

    // 注册清理过程
    // 在程序退出时自动清理
  end;
end;

class procedure TSSLFactory.Finalize;
begin
  if FInitialized then
  begin
    ReleaseAllLibraries;
    // P0: 清理 Map
    FRegistrationMap := nil;
    if GFactoryLockInitialized then
    begin
      DoneCriticalSection(GFactoryLock);
      GFactoryLockInitialized := False;
    end;
    FInitialized := False;
  end;
end;

class procedure TSSLFactory.CheckInitialized;
begin
  if not FInitialized then
    Initialize;
  if not GFactoryLockInitialized then
    RaiseSSLError('Internal error: Lock not initialized', sslErrGeneral);
end;

class procedure TSSLFactory.RegisterLibrary(ALibType: TSSLLibraryType;
  ALibraryClass: TSSLLibraryClass; const ADescription: string; APriority: Integer);
var
  LReg: TSSLLibraryRegistration;
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    // P0: 使用 Map 接口，O(1) 查找（当使用 HashMap 实现时）
    LReg.LibraryType := ALibType;
    LReg.LibraryClass := ALibraryClass;
    LReg.Description := ADescription;
    LReg.Priority := APriority;
    FRegistrationMap.Put(Ord(ALibType), LReg);
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class procedure TSSLFactory.UnregisterLibrary(ALibType: TSSLLibraryType);
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    // P0: 使用 Map 接口
    if FRegistrationMap.Contains(Ord(ALibType)) then
    begin
      // 释放库实例
      ReleaseLibrary(ALibType);
      // 从 Map 中删除
      FRegistrationMap.Remove(Ord(ALibType));
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class function TSSLFactory.IsLibraryAvailable(ALibType: TSSLLibraryType): Boolean;
var
  LLib: ISSLLibrary;
begin
  Result := False;
  CheckInitialized;

  if ALibType = sslAutoDetect then
  begin
    // P0: 使用 Map 接口
    Result := FRegistrationMap.Count > 0;
    Exit;
  end;

  EnterCriticalSection(GFactoryLock);
  try
    // P0: 使用 Map 接口检查是否已注册
    if FRegistrationMap.Contains(Ord(ALibType)) then
    begin
      // 尝试创建实例以验证可用性
      try
        LLib := CreateLibraryInstance(ALibType);
        Result := Assigned(LLib) and LLib.Initialize;
        if Result and not Assigned(FLibraries[ALibType]) then
          FLibraries[ALibType] := LLib;
      except
        Result := False;
      end;
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class function TSSLFactory.GetAvailableLibraries: TSSLLibraryTypes;
var
  LType: TSSLLibraryType;
begin
  Result := [];
  CheckInitialized;
  
  for LType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if (LType <> sslAutoDetect) and IsLibraryAvailable(LType) then
      Include(Result, LType);
  end;
end;

class function TSSLFactory.GetLibraryDescription(ALibType: TSSLLibraryType): string;
var
  LReg: TSSLLibraryRegistration;
begin
  Result := '';
  CheckInitialized;

  EnterCriticalSection(GFactoryLock);
  try
    // P0: 使用 Map 接口
    if FRegistrationMap.TryGet(Ord(ALibType), LReg) then
    begin
      Result := LReg.Description;
      if Result = '' then
        Result := SSL_LIBRARY_NAMES[ALibType];
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class function TSSLFactory.DetectBestLibrary: TSSLLibraryType;
var
  LIndex: Integer;
  LBestPriority: Integer;
  LBestType: TSSLLibraryType;
  LCandidates: specialize TArray<TSSLLibraryRegistration>;
begin
  CheckInitialized;

  LBestType := sslAutoDetect;
  LBestPriority := -1;

  // P0: 使用 Map 接口获取所有注册项
  EnterCriticalSection(GFactoryLock);
  try
    LCandidates := FRegistrationMap.Values;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;

  // 现在在锁外检查可用性
  for LIndex := 0 to High(LCandidates) do
  begin
    if (LCandidates[LIndex].Priority > LBestPriority) and
      IsLibraryAvailable(LCandidates[LIndex].LibraryType) then
    begin
      LBestPriority := LCandidates[LIndex].Priority;
      LBestType := LCandidates[LIndex].LibraryType;
    end;
  end;

  // 如果没有找到可用库，按平台默认选择
  if LBestType = sslAutoDetect then
  begin
    {$IFDEF WINDOWS}
    // Windows平台优先使用系统自带的WinSSL
    if IsLibraryAvailable(sslWinSSL) then
      LBestType := sslWinSSL
    else if IsLibraryAvailable(sslOpenSSL) then
      LBestType := sslOpenSSL;
    {$ELSE}
    // 其他平台优先使用OpenSSL
    if IsLibraryAvailable(sslOpenSSL) then
      LBestType := sslOpenSSL
    else if IsLibraryAvailable(sslMbedTLS) then
      LBestType := sslMbedTLS;
    {$ENDIF}
  end;

  Result := LBestType;
end;

class procedure TSSLFactory.SetDefaultLibrary(ALibType: TSSLLibraryType);
begin
  CheckInitialized;
  
  if (ALibType <> sslAutoDetect) and not IsLibraryAvailable(ALibType) then
    raise ESSLConfigurationException.CreateWithContext(
      Format('SSL library %s is not available on this system', [SSL_LIBRARY_NAMES[ALibType]]),
      sslErrLibraryNotFound,
      'TSSLFactory.SetDefaultLibrary',
      0,
      ALibType
    );
  
  FDefaultLibraryType := ALibType;
end;

class function TSSLFactory.GetDefaultLibrary: TSSLLibraryType;
begin
  CheckInitialized;
  
  if FDefaultLibraryType = sslAutoDetect then
    FDefaultLibraryType := DetectBestLibrary;
    
  Result := FDefaultLibraryType;
end;

class function TSSLFactory.CreateLibraryInstance(ALibType: TSSLLibraryType): ISSLLibrary;
var
  LReg: TSSLLibraryRegistration;
  LClass: TSSLLibraryClass;
begin
  Result := nil;

  // P0: 使用 Map 接口查找注册信息
  LClass := nil;
  if FRegistrationMap.TryGet(Ord(ALibType), LReg) then
    LClass := LReg.LibraryClass;

  if Assigned(LClass) then
  begin
    try
      Result := LClass.Create as ISSLLibrary;
    except
      Result := nil;
    end;
    if Assigned(Result) then
      Exit;

    Exit;
  end;

  case ALibType of
    sslWolfSSL, sslMbedTLS:
    begin
      // WolfSSL and MbedTLS backends are now supported
      // If we reach here, the backend was not registered
      raise ESSLConfigurationException.CreateWithContext(
        Format('%s backend is available but not registered. ' +
               'Ensure the backend unit is included in uses clause.',
               [SSL_LIBRARY_NAMES[ALibType]]),
        sslErrLibraryNotFound,
        'TSSLFactory.CreateLibraryInstance'
      );
    end;
  else
    raise ESSLConfigurationException.CreateWithContext(
      Format('SSL backend %s is not registered', [SSL_LIBRARY_NAMES[ALibType]]),
      sslErrLibraryNotFound,
      'TSSLFactory.CreateLibraryInstance',
      0,
      ALibType
    );
  end;
end;

class function TSSLFactory.GetLibrary(ALibType: TSSLLibraryType): ISSLLibrary;
var
  LType: TSSLLibraryType;
  LInitError: Integer;
  LInitErrorString: string;
begin
  CheckInitialized;
  
  LType := ALibType;
  if LType = sslAutoDetect then
    LType := GetDefaultLibrary;
  
  if LType = sslAutoDetect then
    raise ESSLConfigurationException.CreateWithContext(
      'No SSL library available - could not detect OpenSSL or WinSSL',
      sslErrLibraryNotFound,
      'TSSLFactory.GetLibrary',
      0,
      sslAutoDetect
    );
  
  EnterCriticalSection(GFactoryLock);
  try
    Result := FLibraries[LType];
    
    if not Assigned(Result) then
    begin
      Result := CreateLibraryInstance(LType);
      if Assigned(Result) then
      begin
        if FAutoInitialize then
        begin
          if not Result.Initialize then
          begin
            LInitError := Result.GetLastError;
            LInitErrorString := Result.GetLastErrorString;
            raise ESSLInitializationException.CreateWithContext(
              Format('Failed to initialize SSL library: %s (LastError=%d, Details=%s)',
                [SSL_LIBRARY_NAMES[LType], LInitError, LInitErrorString]),
              sslErrNotInitialized,
              'TSSLFactory.GetLibrary',
              LInitError,
              LType
            );
          end;
        end;
        FLibraries[LType] := Result;
      end
      else
        raise ESSLInitializationException.CreateWithContext(
          Format('Failed to create SSL library instance for %s', [SSL_LIBRARY_NAMES[LType]]),
          sslErrLibraryNotFound,
          'TSSLFactory.GetLibrary',
          0,
          LType
        );
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class function TSSLFactory.CreateContext(AContextType: TSSLContextType;
  ALibType: TSSLLibraryType): ISSLContext;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  LLib := GetLibrary(ALibType);
  Result := LLib.CreateContext(AContextType);
  if Result <> nil then
  begin
    LConfig := LLib.GetDefaultConfig;
    NormalizeConfigOptions(LConfig);
    if LConfig.Options <> [] then
      Result.SetOptions(LConfig.Options);

    if LConfig.ProtocolVersions <> [] then
      Result.SetProtocolVersions(LConfig.ProtocolVersions);

    if LConfig.VerifyMode <> [] then
      Result.SetVerifyMode(LConfig.VerifyMode);

    if LConfig.VerifyDepth > 0 then
      Result.SetVerifyDepth(LConfig.VerifyDepth);

    if LConfig.CipherList <> '' then
      Result.SetCipherList(LConfig.CipherList);

    if LConfig.CipherSuites <> '' then
      Result.SetCipherSuites(LConfig.CipherSuites);

    Result.SetSessionCacheSize(LConfig.SessionCacheSize);
    Result.SetSessionTimeout(LConfig.SessionTimeout);
    Result.SetSessionCacheMode(ssoEnableSessionCache in LConfig.Options);

    if LConfig.ServerName <> '' then
      Result.SetServerName(LConfig.ServerName);

    if LConfig.ALPNProtocols <> '' then
      Result.SetALPNProtocols(LConfig.ALPNProtocols);
  end;
end;

class function TSSLFactory.CreateContext(const AConfig: TSSLConfig): ISSLContext;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  LConfig := AConfig;
  NormalizeConfigOptions(LConfig);

  LLib := GetLibrary(LConfig.LibraryType);
  LLib.SetDefaultConfig(LConfig);
  Result := LLib.CreateContext(LConfig.ContextType);
  if Result = nil then
    Exit;

  Result.SetOptions(LConfig.Options);
  Result.SetSessionCacheSize(LConfig.SessionCacheSize);
  Result.SetSessionTimeout(LConfig.SessionTimeout);
  Result.SetSessionCacheMode(ssoEnableSessionCache in LConfig.Options);
  
  // 应用配置
  if LConfig.ProtocolVersions <> [] then
    Result.SetProtocolVersions(LConfig.ProtocolVersions);
    
  if LConfig.CertificateFile <> '' then
    Result.LoadCertificate(LConfig.CertificateFile);
    
  if LConfig.PrivateKeyFile <> '' then
    Result.LoadPrivateKey(LConfig.PrivateKeyFile, LConfig.PrivateKeyPassword);
    
  if LConfig.CAFile <> '' then
    Result.LoadCAFile(LConfig.CAFile);
    
  if LConfig.VerifyMode <> [] then
    Result.SetVerifyMode(LConfig.VerifyMode);

  if LConfig.VerifyDepth > 0 then
    Result.SetVerifyDepth(LConfig.VerifyDepth);
    
  if LConfig.CipherList <> '' then
    Result.SetCipherList(LConfig.CipherList);

  if LConfig.CipherSuites <> '' then
    Result.SetCipherSuites(LConfig.CipherSuites);
    
  if LConfig.ServerName <> '' then
    Result.SetServerName(LConfig.ServerName);
    
  if LConfig.ALPNProtocols <> '' then
    Result.SetALPNProtocols(LConfig.ALPNProtocols);
end;

class function TSSLFactory.CreateCertificate(ALibType: TSSLLibraryType): ISSLCertificate;
var
  LLib: ISSLLibrary;
begin
  LLib := GetLibrary(ALibType);
  Result := LLib.CreateCertificate;
end;

class function TSSLFactory.CreateCertificateStore(ALibType: TSSLLibraryType): ISSLCertificateStore;
var
  LLib: ISSLLibrary;
begin
  LLib := GetLibrary(ALibType);
  Result := LLib.CreateCertificateStore;
end;


class function TSSLFactory.CreateServerContext(const ACertFile, AKeyFile: string;
  ALibType: TSSLLibraryType): ISSLContext;
begin
  Result := CreateContext(sslCtxServer, ALibType);
  Result.LoadCertificate(ACertFile);
  Result.LoadPrivateKey(AKeyFile);
  Result.SetVerifyMode([sslVerifyNone]); // 服务端默认不验证客户端
end;

class function TSSLFactory.GetLibraryInstance(ALibType: TSSLLibraryType): ISSLLibrary;
begin
  Result := GetLibrary(ALibType);
end;

class procedure TSSLFactory.ReleaseLibrary(ALibType: TSSLLibraryType);
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    if Assigned(FLibraries[ALibType]) then
    begin
      FLibraries[ALibType].Finalize;
      FLibraries[ALibType] := nil;
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class procedure TSSLFactory.ReleaseAllLibraries;
var
  LType: TSSLLibraryType;
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    for LType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
    begin
      if Assigned(FLibraries[LType]) then
      begin
        FLibraries[LType].Finalize;
        FLibraries[LType] := nil;
      end;
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class procedure TSSLFactory.SetAutoInitialize(AValue: Boolean);
begin
  CheckInitialized;
  FAutoInitialize := AValue;
end;

class function TSSLFactory.GetAutoInitialize: Boolean;
begin
  CheckInitialized;
  Result := FAutoInitialize;
end;

class function TSSLFactory.GetVersionInfo: string;
var
  LType: TSSLLibraryType;
  LLib: ISSLLibrary;
begin
  Result := 'fafafa.ssl v1.0' + LineEnding;
  Result := Result + '可用的SSL库:' + LineEnding;
  
  for LType := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    if (LType <> sslAutoDetect) and IsLibraryAvailable(LType) then
    begin
      try
        LLib := GetLibrary(LType);
        Result := Result + Format('  - %s: %s' + LineEnding,
          [SSL_LIBRARY_NAMES[LType], LLib.GetVersionString]);
      except
        on E: Exception do
          TSecurityLog.Debug('Factory', Format('GetLibrary failed for %s: %s', [SSL_LIBRARY_NAMES[LType], E.Message]));
      end;
    end;
  end;
end;

class function TSSLFactory.GetSystemInfo: string;
begin
  Result := 'SSL/TLS System Information' + LineEnding;
  {$IFDEF WINDOWS}
  Result := Result + 'Platform: Windows' + LineEnding;
  Result := Result + Format('OS Version: %d.%d Build %d' + LineEnding,
    [Win32MajorVersion, Win32MinorVersion, Win32BuildNumber]);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := Result + 'Platform: Linux' + LineEnding;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := Result + 'Platform: macOS' + LineEnding;
  {$ENDIF}
  
  Result := Result + Format('Default Library: %s' + LineEnding,
    [SSL_LIBRARY_NAMES[GetDefaultLibrary]]);
end;

{ TSSLHelper }

class function TSSLHelper.VerifyCertificateFile(const AFileName: string): Boolean;
var
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
begin
  try
    LCert := TSSLFactory.CreateCertificate;
    LCert.LoadFromFile(AFileName);
    
    LStore := TSSLFactory.CreateCertificateStore;
    LStore.LoadSystemStore;
    
    Result := LCert.Verify(LStore);
  except
    Result := False;
  end;
end;

class function TSSLHelper.GetCertificateInfo(const AFileName: string): TSSLCertificateInfo;
var
  LCert: ISSLCertificate;
begin
  LCert := TSSLFactory.CreateCertificate;
  LCert.LoadFromFile(AFileName);
  Result := LCert.GetInfo;
end;


class function TSSLHelper.GenerateRandomBytes(ACount: Integer): TBytes;
begin
  // Phase 3.3 P1: 使用平台无关的加密安全随机数生成器
  // 修复严重设计缺陷: 原实现依赖 OpenSSL RAND_bytes，导致:
  // - WinSSL 用户必须安装 OpenSSL DLL
  // - 没有 OpenSSL 时程序启动即崩溃 (STATUS_ENTRYPOINT_NOT_FOUND)
  //
  // 新实现使用平台原生 API:
  // - Windows: CryptGenRandom (CryptoAPI)
  // - Linux/macOS: /dev/urandom
  //
  // 这样 WinSSL 用户可以完全独立于 OpenSSL 使用本库

  Result := GenerateSecureRandomBytes(ACount);
end;

class function TSSLHelper.HashData(const AData: TBytes;
  AHashType: TSSLHash): string;
begin
  // Phase 3.3 P1: 哈希功能需要 SSL 后端支持
  // 此方法已被标记为需要后端支持，用户应使用具体后端的哈希功能
  // 或者在确保 OpenSSL 可用时使用 TCryptoUtils
  //
  // 为了保持向后兼容，我们抛出一个明确的异常
  raise ESSLException.Create(
    'TSSLHelper.HashData requires a specific SSL backend. ' +
    'Use TCryptoUtils.SHA256/SHA512 (requires OpenSSL) or ' +
    'implement platform-specific hashing.'
  );
end;

initialization
  GSSLFactory := nil;
  GSSLHelper := nil;
  // GFactoryLock 在 TSSLFactory.Initialize 中初始化
  TSSLFactory.Initialize;
  
finalization
  TSSLFactory.Finalize;
  FreeAndNil(GSSLFactory);
  FreeAndNil(GSSLHelper);

end.
