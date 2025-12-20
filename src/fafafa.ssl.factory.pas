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
  fafafa.ssl.exceptions;  // 新增：类型化异常

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
      FRegistrations: array of TSSLLibraryRegistration;
      FLibraries: array[TSSLLibraryType] of ISSLLibrary;
      FDefaultLibraryType: TSSLLibraryType;
      FInitialized: Boolean;
      FAutoInitialize: Boolean;
      
    class procedure Initialize;
    class procedure Finalize;
    class function CreateLibraryInstance(aLibType: TSSLLibraryType): ISSLLibrary;
    class procedure CheckInitialized;
  public
    { ==================== 库访问（高级用法） ==================== }

    {**
     * 获取指定类型的库实例
     *
     * @param aLibType 库类型（sslOpenSSL, sslWinSSL等）
     * @return 库实例接口
     * @raises ESSLConfigurationException 库不可用时
     *
     * 注意: 普通用户通常不需要直接使用此方法
     *}
    class function GetLibrary(aLibType: TSSLLibraryType): ISSLLibrary;

    { ==================== 库注册（后端开发者使用） ==================== }

    {**
     * 注册SSL后端库
     *
     * @param aLibType 库类型标识
     * @param aLibraryClass 库类（必须实现 ISSLLibrary）
     * @param aDescription 库描述（可选）
     * @param aPriority 优先级（默认0，数字越大越优先）
     *
     * 注意: 此方法供后端实现的初始化代码调用
     *}
    class procedure RegisterLibrary(
      aLibType: TSSLLibraryType;
      aLibraryClass: TSSLLibraryClass;
      const aDescription: string = '';
      aPriority: Integer = 0
    );

    {**
     * 取消注册SSL后端库
     *
     * @param aLibType 库类型标识
     *}
    class procedure UnregisterLibrary(aLibType: TSSLLibraryType);

    { ==================== 库检测与查询 ==================== }

    {**
     * 检查指定库是否可用
     *
     * @param aLibType 库类型
     * @return True=可用，False=不可用
     *
     * @example
     * <code>
     *   if TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
     *     WriteLn('OpenSSL is available');
     * </code>
     *}
    class function IsLibraryAvailable(aLibType: TSSLLibraryType): Boolean;

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
     * @param aLibType 库类型
     * @return 库描述字符串
     *}
    class function GetLibraryDescription(aLibType: TSSLLibraryType): string;

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
     * @param aLibType 库类型
     * @raises ESSLConfigurationException 库不可用时
     *
     * @example
     * <code>
     *   TSSLFactory.SetDefaultLibrary(sslOpenSSL);
     * </code>
     *}
    class procedure SetDefaultLibrary(aLibType: TSSLLibraryType);

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
     * @param aContextType 上下文类型（sslClient或sslServer）
     * @param aLibType 库类型（默认sslAutoDetect自动选择）
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
      aContextType: TSSLContextType;
      aLibType: TSSLLibraryType = sslAutoDetect
    ): ISSLContext; overload;

    {**
     * 使用配置对象创建SSL Context
     *
     * @param aConfig SSL配置对象
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
    class function CreateContext(const aConfig: TSSLConfig): ISSLContext; overload;

    class procedure NormalizeConfig(var aConfig: TSSLConfig);

    {**
     * 创建证书对象
     *
     * @param aLibType 库类型（默认自动检测）
     * @return 证书接口
     *}
    class function CreateCertificate(aLibType: TSSLLibraryType = sslAutoDetect): ISSLCertificate;

    {**
     * 创建证书存储对象
     *
     * @param aLibType 库类型（默认自动检测）
     * @return 证书存储接口
     *}
    class function CreateCertificateStore(aLibType: TSSLLibraryType = sslAutoDetect): ISSLCertificateStore;
    
    // 快捷方法 - 简化的服务端上下文
    class function CreateServerContext(const aCertFile, aKeyFile: string;
                                      aLibType: TSSLLibraryType = sslAutoDetect): ISSLContext;
    
    // 库管理
    class function GetLibraryInstance(aLibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
    class procedure ReleaseLibrary(aLibType: TSSLLibraryType);
    class procedure ReleaseAllLibraries;
    
    // 全局配置
    class procedure SetAutoInitialize(aValue: Boolean);
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
    class function VerifyCertificateFile(const aFileName: string): Boolean;
    class function GetCertificateInfo(const aFileName: string): TSSLCertificateInfo;
    
    // 工具方法
    class function GenerateRandomBytes(aCount: Integer): TBytes;
    class function HashData(const aData: TBytes; aHashType: TSSLHash): string;
  end;

  { 全局函数 - 便捷接口 }
  function SSLFactory: TSSLFactory;
  function SSLHelper: TSSLHelper;

  // 快速创建函数
  function CreateSSLLibrary(aLibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
  function CreateSSLContext(aType: TSSLContextType = sslCtxClient): ISSLContext;
  function CreateSSLCertificate: ISSLCertificate;
  function CreateSSLConnection(aContext: ISSLContext; aSocket: THandle): ISSLConnection;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  fafafa.ssl.crypto.utils,   // Phase 2.3.5 - 加密工具（哈希计算）
  fafafa.ssl.encoding,       // Phase 2.3.5 - 编码工具（Hex转换）
  fafafa.ssl.errors,
  fafafa.ssl.openssl.api.rand;  // Phase 3.3 P0 - 加密安全随机数生成

var
  GSSLFactory: TSSLFactory;
  GSSLHelper: TSSLHelper;
  GFactoryLock: TRTLCriticalSection;  // 工厂类的全局锁
  GFactoryLockInitialized: Boolean = False;

procedure NormalizeConfigOptions(var aConfig: TSSLConfig);
begin
  if aConfig.EnableCompression then
    Exclude(aConfig.Options, ssoDisableCompression)
  else
    Include(aConfig.Options, ssoDisableCompression);

  Include(aConfig.Options, ssoDisableRenegotiation);

  Include(aConfig.Options, ssoNoSSLv2);
  Include(aConfig.Options, ssoNoSSLv3);
  Include(aConfig.Options, ssoNoTLSv1);
  Include(aConfig.Options, ssoNoTLSv1_1);

  if aConfig.ProtocolVersions <> [] then
  begin
    if sslProtocolSSL2 in aConfig.ProtocolVersions then
      Exclude(aConfig.Options, ssoNoSSLv2)
    else
      Include(aConfig.Options, ssoNoSSLv2);

    if sslProtocolSSL3 in aConfig.ProtocolVersions then
      Exclude(aConfig.Options, ssoNoSSLv3)
    else
      Include(aConfig.Options, ssoNoSSLv3);

    if sslProtocolTLS10 in aConfig.ProtocolVersions then
      Exclude(aConfig.Options, ssoNoTLSv1)
    else
      Include(aConfig.Options, ssoNoTLSv1);

    if sslProtocolTLS11 in aConfig.ProtocolVersions then
      Exclude(aConfig.Options, ssoNoTLSv1_1)
    else
      Include(aConfig.Options, ssoNoTLSv1_1);

    if sslProtocolTLS12 in aConfig.ProtocolVersions then
      Exclude(aConfig.Options, ssoNoTLSv1_2)
    else
      Include(aConfig.Options, ssoNoTLSv1_2);

    if sslProtocolTLS13 in aConfig.ProtocolVersions then
      Exclude(aConfig.Options, ssoNoTLSv1_3)
    else
      Include(aConfig.Options, ssoNoTLSv1_3);
  end;

  if aConfig.EnableSessionTickets then
    Include(aConfig.Options, ssoEnableSessionTickets)
  else
    Exclude(aConfig.Options, ssoEnableSessionTickets);

  if aConfig.EnableOCSPStapling then
    Include(aConfig.Options, ssoEnableOCSPStapling)
  else
    Exclude(aConfig.Options, ssoEnableOCSPStapling);

  if aConfig.SessionCacheSize > 0 then
    Include(aConfig.Options, ssoEnableSessionCache)
  else
    Exclude(aConfig.Options, ssoEnableSessionCache);

  if aConfig.VerifyDepth <= 0 then
    aConfig.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;

  if aConfig.CipherList = '' then
    aConfig.CipherList := SSL_DEFAULT_CIPHER_LIST;

  if aConfig.CipherSuites = '' then
    aConfig.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
end;

class procedure TSSLFactory.NormalizeConfig(var aConfig: TSSLConfig);
begin
  NormalizeConfigOptions(aConfig);
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

function CreateSSLLibrary(aLibType: TSSLLibraryType): ISSLLibrary;
begin
  // 直接使用工厂获取库实例
  // 这会自动检测最佳库（如果 aLibType = sslAutoDetect）
  // 并自动初始化库
  Result := TSSLFactory.GetLibraryInstance(aLibType);
end;

function CreateSSLContext(aType: TSSLContextType): ISSLContext;
begin
  Result := TSSLFactory.CreateContext(aType);
end;

function CreateSSLCertificate: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificate;
end;

function CreateSSLConnection(aContext: ISSLContext; aSocket: THandle): ISSLConnection;
begin
  if Assigned(aContext) then
    Result := aContext.CreateConnection(aSocket)
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

class procedure TSSLFactory.RegisterLibrary(aLibType: TSSLLibraryType;
  aLibraryClass: TSSLLibraryClass; const aDescription: string; aPriority: Integer);
var
  LIndex: Integer;
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    // 检查是否已注册
    for LIndex := 0 to High(FRegistrations) do
    begin
      if FRegistrations[LIndex].LibraryType = aLibType then
      begin
        // 更新现有注册
        FRegistrations[LIndex].LibraryClass := aLibraryClass;
        FRegistrations[LIndex].Description := aDescription;
        FRegistrations[LIndex].Priority := aPriority;
        Exit;
      end;
    end;
    
    // 添加新注册
    SetLength(FRegistrations, Length(FRegistrations) + 1);
    with FRegistrations[High(FRegistrations)] do
    begin
      LibraryType := aLibType;
      LibraryClass := aLibraryClass;
      Description := aDescription;
      Priority := aPriority;
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class procedure TSSLFactory.UnregisterLibrary(aLibType: TSSLLibraryType);
var
  LIndex, LPos: Integer;
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    LPos := -1;
    for LIndex := 0 to High(FRegistrations) do
    begin
      if FRegistrations[LIndex].LibraryType = aLibType then
      begin
        LPos := LIndex;
        Break;
      end;
    end;
    
    if LPos >= 0 then
    begin
      // 释放库实例
      ReleaseLibrary(aLibType);
      
      // 从数组中删除
      for LIndex := LPos to High(FRegistrations) - 1 do
        FRegistrations[LIndex] := FRegistrations[LIndex + 1];
      SetLength(FRegistrations, Length(FRegistrations) - 1);
    end;
  finally
    LeaveCriticalSection(GFactoryLock);
  end;
end;

class function TSSLFactory.IsLibraryAvailable(aLibType: TSSLLibraryType): Boolean;
var
  LIndex: Integer;
  LLib: ISSLLibrary;
begin
  Result := False;
  CheckInitialized;
  
  if aLibType = sslAutoDetect then
  begin
    Result := Length(FRegistrations) > 0;
    Exit;
  end;
  
  EnterCriticalSection(GFactoryLock);
  try
    // 检查是否已注册
    for LIndex := 0 to High(FRegistrations) do
    begin
      if FRegistrations[LIndex].LibraryType = aLibType then
      begin
        // 尝试创建实例以验证可用性
        try
          LLib := CreateLibraryInstance(aLibType);
          Result := Assigned(LLib) and LLib.Initialize;
          if Result and not Assigned(FLibraries[aLibType]) then
            FLibraries[aLibType] := LLib;
        except
          Result := False;
        end;
        Break;
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

class function TSSLFactory.GetLibraryDescription(aLibType: TSSLLibraryType): string;
var
  LIndex: Integer;
begin
  Result := '';
  CheckInitialized;
  
  EnterCriticalSection(GFactoryLock);
  try
    for LIndex := 0 to High(FRegistrations) do
    begin
      if FRegistrations[LIndex].LibraryType = aLibType then
      begin
        Result := FRegistrations[LIndex].Description;
        if Result = '' then
          Result := SSL_LIBRARY_NAMES[aLibType];
        Break;
      end;
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
  LCandidates: array of TSSLLibraryRegistration;
begin
  CheckInitialized;
  
  LBestType := sslAutoDetect;
  LBestPriority := -1;
  
  // 先复制注册信息，避免在持有锁时调用 IsLibraryAvailable
  EnterCriticalSection(GFactoryLock);
  try
    SetLength(LCandidates, Length(FRegistrations));
    for LIndex := 0 to High(FRegistrations) do
      LCandidates[LIndex] := FRegistrations[LIndex];
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

class procedure TSSLFactory.SetDefaultLibrary(aLibType: TSSLLibraryType);
begin
  CheckInitialized;
  
  if (aLibType <> sslAutoDetect) and not IsLibraryAvailable(aLibType) then
    raise ESSLConfigurationException.CreateWithContext(
      Format('SSL library %s is not available on this system', [SSL_LIBRARY_NAMES[aLibType]]),
      sslErrLibraryNotFound,
      'TSSLFactory.SetDefaultLibrary',
      0,
      aLibType
    );
  
  FDefaultLibraryType := aLibType;
end;

class function TSSLFactory.GetDefaultLibrary: TSSLLibraryType;
begin
  CheckInitialized;
  
  if FDefaultLibraryType = sslAutoDetect then
    FDefaultLibraryType := DetectBestLibrary;
    
  Result := FDefaultLibraryType;
end;

class function TSSLFactory.CreateLibraryInstance(aLibType: TSSLLibraryType): ISSLLibrary;
var
  LIndex: Integer;
  LClass: TSSLLibraryClass;
begin
  Result := nil;

  LClass := nil;
  for LIndex := 0 to High(FRegistrations) do
  begin
    if FRegistrations[LIndex].LibraryType = aLibType then
    begin
      LClass := FRegistrations[LIndex].LibraryClass;
      Break;
    end;
  end;

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

  case aLibType of
    sslWolfSSL:
    begin
      // Future: WolfSSL backend support (not currently implemented)
      raise ESSLConfigurationException.CreateWithContext(
        'WolfSSL backend is planned but not yet implemented',
        sslErrUnsupported,
        'TSSLFactory.CreateLibraryInstance'
      );
    end;
    
    sslMbedTLS:
    begin
      // Future: MbedTLS backend support (not currently implemented)
      raise ESSLConfigurationException.CreateWithContext(
        'MbedTLS backend is planned but not yet implemented',
        sslErrUnsupported,
        'TSSLFactory.CreateLibraryInstance'
      );
    end;
  else
    raise ESSLConfigurationException.CreateWithContext(
      Format('SSL backend %s is not registered', [SSL_LIBRARY_NAMES[aLibType]]),
      sslErrLibraryNotFound,
      'TSSLFactory.CreateLibraryInstance',
      0,
      aLibType
    );
  end;
end;

class function TSSLFactory.GetLibrary(aLibType: TSSLLibraryType): ISSLLibrary;
var
  LType: TSSLLibraryType;
  LInitError: Integer;
  LInitErrorString: string;
begin
  CheckInitialized;
  
  LType := aLibType;
  if LType = sslAutoDetect then
    LType := GetDefaultLibrary;
  
  if LType = sslAutoDetect then
    RaiseSSLConfigError(
      'No SSL library available - could not detect OpenSSL or WinSSL',
      'TSSLFactory.GetLibrary'
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

class function TSSLFactory.CreateContext(aContextType: TSSLContextType;
  aLibType: TSSLLibraryType): ISSLContext;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  LLib := GetLibrary(aLibType);
  Result := LLib.CreateContext(aContextType);
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

class function TSSLFactory.CreateContext(const aConfig: TSSLConfig): ISSLContext;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  LConfig := aConfig;
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

class function TSSLFactory.CreateCertificate(aLibType: TSSLLibraryType): ISSLCertificate;
var
  LLib: ISSLLibrary;
begin
  LLib := GetLibrary(aLibType);
  Result := LLib.CreateCertificate;
end;

class function TSSLFactory.CreateCertificateStore(aLibType: TSSLLibraryType): ISSLCertificateStore;
var
  LLib: ISSLLibrary;
begin
  LLib := GetLibrary(aLibType);
  Result := LLib.CreateCertificateStore;
end;


class function TSSLFactory.CreateServerContext(const aCertFile, aKeyFile: string;
  aLibType: TSSLLibraryType): ISSLContext;
begin
  Result := CreateContext(sslCtxServer, aLibType);
  Result.LoadCertificate(aCertFile);
  Result.LoadPrivateKey(aKeyFile);
  Result.SetVerifyMode([sslVerifyNone]); // 服务端默认不验证客户端
end;

class function TSSLFactory.GetLibraryInstance(aLibType: TSSLLibraryType): ISSLLibrary;
begin
  Result := GetLibrary(aLibType);
end;

class procedure TSSLFactory.ReleaseLibrary(aLibType: TSSLLibraryType);
begin
  CheckInitialized;
  EnterCriticalSection(GFactoryLock);
  try
    if Assigned(FLibraries[aLibType]) then
    begin
      FLibraries[aLibType].Finalize;
      FLibraries[aLibType] := nil;
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

class procedure TSSLFactory.SetAutoInitialize(aValue: Boolean);
begin
  CheckInitialized;
  FAutoInitialize := aValue;
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
        // 忽略错误
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

class function TSSLHelper.VerifyCertificateFile(const aFileName: string): Boolean;
var
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
begin
  try
    LCert := TSSLFactory.CreateCertificate;
    LCert.LoadFromFile(aFileName);
    
    LStore := TSSLFactory.CreateCertificateStore;
    LStore.LoadSystemStore;
    
    Result := LCert.Verify(LStore);
  except
    Result := False;
  end;
end;

class function TSSLHelper.GetCertificateInfo(const aFileName: string): TSSLCertificateInfo;
var
  LCert: ISSLCertificate;
begin
  LCert := TSSLFactory.CreateCertificate;
  LCert.LoadFromFile(aFileName);
  Result := LCert.GetInfo;
end;


class function TSSLHelper.GenerateRandomBytes(aCount: Integer): TBytes;
begin
  // Phase 3.3 P0: 修复安全漏洞 - 使用加密安全的随机数生成器
  // 原实现使用 Random(256) 是不安全的，不适合加密场景
  // 现使用 OpenSSL 的 RAND_bytes 提供加密安全的随机数

  Result := nil;

  if aCount <= 0 then
    raise ESSLInvalidArgument.CreateFmt('Invalid random bytes count: %d', [aCount]);

  SetLength(Result, aCount);

  // 使用 OpenSSL RAND_bytes 生成加密安全的随机数
  if RAND_bytes(@Result[0], aCount) <> 1 then
    raise ESSLCryptoError.CreateWithContext(
      'Failed to generate cryptographically secure random bytes',
      sslErrOther,
      'TSSLHelper.GenerateRandomBytes',
      0,
      sslOpenSSL
    );
end;

class function TSSLHelper.HashData(const aData: TBytes;
  aHashType: TSSLHash): string;
var
  LHashAlg: THashAlgorithm;
  LHashBytes: TBytes;
begin
  // Phase 2.3.5: 迁移至 crypto.utils
  // 映射 TSSLHash 到 THashAlgorithm
  case aHashType of
    sslHashMD5: LHashAlg := HASH_MD5;
    sslHashSHA1: LHashAlg := HASH_SHA1;
    sslHashSHA256: LHashAlg := HASH_SHA256;
    sslHashSHA512: LHashAlg := HASH_SHA512;
  else
    // SHA224, SHA384, SHA3-*, BLAKE2b 尚未实现
    raise ESSLInvalidArgument.CreateFmt(
      'Hash algorithm %d not yet supported',
      [Ord(aHashType)]
    );
  end;

  try
    LHashBytes := TCryptoUtils.CalculateHash(aData, LHashAlg);
    Result := TEncodingUtils.BytesToHex(LHashBytes, False);
  except
    on E: Exception do
    begin
      // 如果失败，返回空字符串
      Result := '';
    end;
  end;
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
