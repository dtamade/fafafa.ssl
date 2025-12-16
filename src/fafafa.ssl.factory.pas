{
  fafafa.ssl.factory - SSL/TLS 库工厂模式实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    工厂模式实现，负责：
    1. 自动检测可用的SSL库
    2. 创建合适的SSL实现实例
    3. 管理库的全局状态
    4. 提供简化的API接口
}

unit fafafa.ssl.factory;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions;  // 新增：类型化异常

type
  { SSL库类类型 }
  TSSLLibraryClass = class of TInterfacedObject;
  
  { SSL库注册信息 }
  TSSLLibraryRegistration = record
    LibraryType: TSSLLibraryType;
    LibraryClass: TSSLLibraryClass;  // 必须实现 ISSLLibrary
    Description: string;
    Priority: Integer;      // 优先级，数字越大优先级越高
  end;

  { TSSLFactory - SSL工厂类 }
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
    // Direct library access (for advanced usage)
    class function GetLibrary(aLibType: TSSLLibraryType): ISSLLibrary;
    // 库注册（供各个后端实现调用）
    class procedure RegisterLibrary(aLibType: TSSLLibraryType; 
                                  aLibraryClass: TSSLLibraryClass;
                                  const aDescription: string = '';
                                  aPriority: Integer = 0);
    class procedure UnregisterLibrary(aLibType: TSSLLibraryType);
    
    // 库可用性检测
    class function IsLibraryAvailable(aLibType: TSSLLibraryType): Boolean;
    class function GetAvailableLibraries: TSSLLibraryTypes;
    class function GetLibraryDescription(aLibType: TSSLLibraryType): string;
    class function DetectBestLibrary: TSSLLibraryType;
    
    // 默认库设置
    class procedure SetDefaultLibrary(aLibType: TSSLLibraryType);
    class function GetDefaultLibrary: TSSLLibraryType;
    
    // 创建实例 - 主要接口
    class function CreateContext(aContextType: TSSLContextType;
                                aLibType: TSSLLibraryType = sslAutoDetect): ISSLContext; overload;
    class function CreateContext(const aConfig: TSSLConfig): ISSLContext; overload;
    
    class function CreateCertificate(aLibType: TSSLLibraryType = sslAutoDetect): ISSLCertificate;
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
  fafafa.ssl.openssl.lib,    // OpenSSL 后端支持 (新实现)
  {$IFDEF WINDOWS}
  fafafa.ssl.winssl.lib,  // WinSSL Phase 2.2 新实现（替换老的 winssl.pas）
  {$ENDIF}
  fafafa.ssl.utils,      // 工具函数
  fafafa.ssl.errors;

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
begin
  Result := nil;
  
  // 直接根据类型创建相应的库实例
  case aLibType of
    sslWinSSL:
    begin
      {$IFDEF WINDOWS}
      Result := TWinSSLLibrary.Create;
      {$ENDIF}
    end;
    
    sslOpenSSL:
    begin
      Result := TOpenSSLLibrary.Create;
    end;
    
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
    // sslAutoDetect and any other values - should not reach here
    raise ESSLConfigurationException.CreateWithContext(
      'Invalid library type for direct instantiation',
      sslErrUnsupported,
      'TSSLFactory.CreateLibraryInstance'
    );
  end;
end;

class function TSSLFactory.GetLibrary(aLibType: TSSLLibraryType): ISSLLibrary;
var
  LType: TSSLLibraryType;
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
            raise ESSLInitializationException.CreateWithContext(
              Format('Failed to initialize SSL library: %s', [SSL_LIBRARY_NAMES[LType]]),
              sslErrNotInitialized,
              'TSSLFactory.GetLibrary',
              0,
              LType
            );
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
    
  if LConfig.CipherList <> '' then
    Result.SetCipherList(LConfig.CipherList);
    
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
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, aCount);
  for LIndex := 0 to aCount - 1 do
    Result[LIndex] := Random(256);
end;

class function TSSLHelper.HashData(const aData: TBytes;
  aHashType: TSSLHash): string;
const
  HASH_ALGORITHMS: array[TSSLHash] of AnsiString = (
    'md5',      // sslHashMD5
    'sha1',     // sslHashSHA1
    'sha224',   // sslHashSHA224
    'sha256',   // sslHashSHA256
    'sha384',   // sslHashSHA384
    'sha512',   // sslHashSHA512
    'sha3-256', // sslHashSHA3_256
    'sha3-512', // sslHashSHA3_512
    'blake2b512'// sslHashBLAKE2b
  );
begin
  try
    Result := fafafa.ssl.utils.ComputeDigest(HASH_ALGORITHMS[aHashType], aData);
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
