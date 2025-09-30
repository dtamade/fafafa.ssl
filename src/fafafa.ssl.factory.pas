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
  SysUtils, Classes, SyncObjs,
  fafafa.ssl.types, fafafa.ssl.intf;

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
    class function GetLibrary(aLibType: TSSLLibraryType): ISSLLibrary;
    class function CreateLibraryInstance(aLibType: TSSLLibraryType): ISSLLibrary;
    class procedure CheckInitialized;
  public
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
    
    // 快捷方法 - 简化的客户端连接
    class function CreateClientConnection(const aHost: string; 
                                         aPort: Integer;
                                         aVerifyMode: TSSLVerifyModes = [sslVerifyPeer];
                                         aLibType: TSSLLibraryType = sslAutoDetect): ISSLConnection;
    
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
    // HTTPS 客户端快捷方法
    class function HTTPSGet(const aURL: string; out aResponse: string;
                           aVerifyMode: TSSLVerifyModes = [sslVerifyPeer]): Boolean;
    class function HTTPSPost(const aURL, aData: string; out aResponse: string;
                            aVerifyMode: TSSLVerifyModes = [sslVerifyPeer]): Boolean;
    
    // 证书验证
    class function VerifyCertificateFile(const aFileName: string): Boolean;
    class function GetCertificateInfo(const aFileName: string): TSSLCertificateInfo;
    
    // 测试连接
    class function TestSSLConnection(const aHost: string; aPort: Integer;
                                    out aInfo: TSSLConnectionInfo;
                                    aTimeout: Integer = 10000): Boolean;
    
    // 工具方法
    class function GenerateRandomBytes(aCount: Integer): TBytes;
    class function HashData(const aData: TBytes; aHashType: TSSLHash): string;
  end;

  { 全局函数 - 便捷接口 }
  function SSLFactory: TSSLFactory;
  function SSLHelper: TSSLHelper;
  
  // 快速创建函数
  function CreateSSLContext(aType: TSSLContextType = sslCtxClient): ISSLContext;
  function CreateSSLCertificate: ISSLCertificate;
  function CreateSSLConnection(aContext: ISSLContext; aSocket: THandle): ISSLConnection;

implementation

uses
  {$IFDEF WINDOWS}
  Windows, fafafa.ssl.winssl,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  fafafa.ssl.openssl,  // OpenSSL 后端支持
  DateUtils;

var
  GSSLFactory: TSSLFactory;
  GSSLHelper: TSSLHelper;
  GFactoryLock: TRTLCriticalSection;  // 工厂类的全局锁
  GFactoryLockInitialized: Boolean = False;

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
    raise ESSLException.Create('SSL上下文未初始化', sslErrNotInitialized);
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
    raise ESSLException.Create('内部错误: 锁未初始化', sslErrGeneral);
end;

class procedure TSSLFactory.RegisterLibrary(aLibType: TSSLLibraryType;
  aLibraryClass: TSSLLibraryClass; const aDescription: string; aPriority: Integer);
var
  LIndex: Integer;
begin
  {$IFDEF DEBUG}
  WriteLn('[DEBUG] RegisterLibrary: 开始注册 ', SSL_LIBRARY_NAMES[aLibType]);
  {$ENDIF}
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
    raise ESSLException.Create(
      Format('SSL库 %s 不可用', [SSL_LIBRARY_NAMES[aLibType]]),
      sslErrLibraryNotFound,
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
      // TODO: Result := TWolfSSLLibrary.Create;
    end;
    
    sslMbedTLS:
    begin
      // TODO: Result := TMbedTLSLibrary.Create;
    end;
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
    raise ESSLException.Create('没有可用的SSL库', sslErrLibraryNotFound);
  
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
            raise ESSLException.Create(
              Format('初始化SSL库 %s 失败', [SSL_LIBRARY_NAMES[LType]]),
              sslErrNotInitialized,
              LType
            );
        end;
        FLibraries[LType] := Result;
      end
      else
        raise ESSLException.Create(
          Format('创建SSL库 %s 实例失败', [SSL_LIBRARY_NAMES[LType]]),
          sslErrLibraryNotFound,
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
begin
  LLib := GetLibrary(aLibType);
  Result := LLib.CreateContext(aContextType);
end;

class function TSSLFactory.CreateContext(const aConfig: TSSLConfig): ISSLContext;
var
  LLib: ISSLLibrary;
begin
  LLib := GetLibrary(aConfig.LibraryType);
  LLib.SetDefaultConfig(aConfig);
  Result := LLib.CreateContext(aConfig.ContextType);
  
  // 应用配置
  if aConfig.ProtocolVersions <> [] then
    Result.SetProtocolVersions(aConfig.ProtocolVersions);
    
  if aConfig.CertificateFile <> '' then
    Result.LoadCertificate(aConfig.CertificateFile);
    
  if aConfig.PrivateKeyFile <> '' then
    Result.LoadPrivateKey(aConfig.PrivateKeyFile);
    
  if aConfig.CAFile <> '' then
    Result.LoadCAFile(aConfig.CAFile);
    
  if aConfig.VerifyMode <> [] then
    Result.SetVerifyMode(aConfig.VerifyMode);
    
  if aConfig.CipherList <> '' then
    Result.SetCipherList(aConfig.CipherList);
    
  if aConfig.ServerName <> '' then
    Result.SetServerName(aConfig.ServerName);
    
  if aConfig.ALPNProtocols <> '' then
    Result.SetALPNProtocols(aConfig.ALPNProtocols);
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

class function TSSLFactory.CreateClientConnection(const aHost: string;
  aPort: Integer; aVerifyMode: TSSLVerifyModes;
  aLibType: TSSLLibraryType): ISSLConnection;
var
  LContext: ISSLContext;
  LSocket: THandle;
begin
  // 创建客户端上下文
  LContext := CreateContext(sslCtxClient, aLibType);
  LContext.SetVerifyMode(aVerifyMode);
  LContext.SetServerName(aHost); // SNI
  
  // TODO: 创建socket并连接到服务器
  // 这里需要实现socket连接逻辑
  LSocket := 0; // 临时占位
  
  // 创建SSL连接
  Result := LContext.CreateConnection(LSocket);
  
  // 执行握手
  if not Result.Connect then
    raise ESSLHandshakeException.Create(
      Format('连接到 %s:%d 失败', [aHost, aPort]),
      sslErrHandshake,
      aLibType
    );
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

class function TSSLHelper.HTTPSGet(const aURL: string; out aResponse: string;
  aVerifyMode: TSSLVerifyModes): Boolean;
begin
  // TODO: 实现HTTPS GET请求
  Result := False;
  aResponse := '';
end;

class function TSSLHelper.HTTPSPost(const aURL, aData: string;
  out aResponse: string; aVerifyMode: TSSLVerifyModes): Boolean;
begin
  // TODO: 实现HTTPS POST请求
  Result := False;
  aResponse := '';
end;

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

class function TSSLHelper.TestSSLConnection(const aHost: string;
  aPort: Integer; out aInfo: TSSLConnectionInfo; aTimeout: Integer): Boolean;
var
  LConn: ISSLConnection;
begin
  try
    LConn := TSSLFactory.CreateClientConnection(aHost, aPort, [sslVerifyPeer]);
    aInfo := LConn.GetConnectionInfo;
    Result := True;
  except
    Result := False;
  end;
end;

class function TSSLHelper.GenerateRandomBytes(aCount: Integer): TBytes;
var
  LIndex: Integer;
begin
  SetLength(Result, aCount);
  for LIndex := 0 to aCount - 1 do
    Result[LIndex] := Random(256);
end;

class function TSSLHelper.HashData(const aData: TBytes;
  aHashType: TSSLHash): string;
begin
  // TODO: 实现哈希计算
  Result := '';
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