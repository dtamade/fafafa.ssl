unit fafafa.ssl.openssl.loader;

{******************************************************************************}
{                                                                              }
{  OpenSSL 动态库加载器 - 统一的动态库管理                                      }
{                                                                              }
{  目的: 消除重复的 LoadLibrary/GetProcAddress 代码                           }
{  范围: OpenSSL libcrypto 和 libssl 动态库加载                                }
{                                                                              }
{  设计原则:                                                                   }
{  1. 单例模式 - 全局共享库句柄                                                }
{  2. 懒加载 - 按需加载库                                                      }
{  3. 版本兼容 - 支持 OpenSSL 3.x, 1.1.x, 1.0.x                               }
{  4. 错误处理 - 统一的错误报告机制                                            }
{                                                                              }
{  Phase: 3.3 P0 - 代码去重                                                   }
{  Author: fafafa.ssl team                                                    }
{  Date: 2025-12-16                                                           }
{                                                                              }
{******************************************************************************}

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils,
  ctypes,  // culong, cint 等 C 类型
  {$IFDEF WINDOWS}
  Windows
  {$ELSE}
  dynlibs
  {$ENDIF};

type
  {**
   * OpenSSL 库类型
   *}
  TOpenSSLLibraryType = (
    osslLibCrypto,  // libcrypto (加密库)
    osslLibSSL      // libssl (SSL/TLS库)
  );

  {**
   * OpenSSL 库版本信息
   *}
  TOpenSSLVersionInfo = record
    Major: Integer;         // 主版本号 (3, 1, 等)
    Minor: Integer;         // 次版本号
    Patch: Integer;         // 补丁版本号
    VersionString: string;  // 完整版本字符串
    IsOpenSSL3: Boolean;    // 是否为 OpenSSL 3.x
  end;

  {**
   * OpenSSL 动态库加载器（单例类）
   *
   * 提供统一的 OpenSSL 动态库加载和函数指针获取功能。
   * 所有 OpenSSL API 绑定模块应使用此类而非自行加载库。
   *
   * 使用示例:
   * <code>
   *   // 获取 libcrypto 句柄
   *   LHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
   *
   *   // 获取函数指针
   *   EVP_MD_CTX_new := TOpenSSLLoader.GetFunction(LHandle, 'EVP_MD_CTX_new');
   *
   *   // 检查函数是否可用
   *   if TOpenSSLLoader.IsFunctionAvailable(LHandle, 'EVP_MAC_fetch') then
   *     WriteLn('OpenSSL 3.x EVP_MAC API is available');
   * </code>
   *}
  TOpenSSLLoader = class
  private
    class var
      FLibCrypto: {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF};
      FLibSSL: {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF};
      FInitialized: Boolean;
      FVersionInfo: TOpenSSLVersionInfo;

    class procedure DetectVersion;
    class function TryLoadLibrary(const ANames: array of string): {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF};
  public
    {**
     * 获取指定类型的库句柄
     *
     * @param ALibType 库类型（libcrypto 或 libssl）
     * @return 库句柄，如果加载失败返回 0
     *
     * 注意: 此方法会自动尝试加载库（懒加载）
     *}
    class function GetLibraryHandle(ALibType: TOpenSSLLibraryType): {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF};

    {**
     * 从指定库获取函数指针
     *
     * @param AHandle 库句柄
     * @param AFunctionName 函数名称
     * @return 函数指针，如果函数不存在返回 nil
     *
     * 注意: 此方法不抛出异常，调用者需检查返回值
     *}
    class function GetFunction(AHandle: {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF}; const AFunctionName: string): Pointer;

    {**
     * 检查函数是否可用
     *
     * @param AHandle 库句柄
     * @param AFunctionName 函数名称
     * @return True=函数存在，False=不存在
     *}
    class function IsFunctionAvailable(AHandle: {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF}; const AFunctionName: string): Boolean;

    {**
     * 获取 OpenSSL 版本信息
     *
     * @return 版本信息结构体
     *}
    class function GetVersionInfo: TOpenSSLVersionInfo;

    {**
     * 检查是否为 OpenSSL 3.x
     *
     * @return True=OpenSSL 3.x, False=1.x 或未加载
     *}
    class function IsOpenSSL3: Boolean;

    {**
     * 卸载所有已加载的库
     *
     * 注意: 通常在程序退出时调用，正常使用不需要手动卸载
     *}
    class procedure UnloadLibraries;

    {**
     * 检查库是否已加载
     *
     * @param ALibType 库类型
     * @return True=已加载，False=未加载
     *}
    class function IsLoaded(ALibType: TOpenSSLLibraryType): Boolean;
  end;

implementation

uses
  fafafa.ssl.base,
  fafafa.ssl.exceptions;

{ TOpenSSLLoader }

class function TOpenSSLLoader.TryLoadLibrary(const ANames: array of string): {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF};
var
  I: Integer;
begin
  Result := 0;
  for I := Low(ANames) to High(ANames) do
  begin
    {$IFDEF WINDOWS}
    Result := LoadLibrary(PChar(ANames[I]));
    {$ELSE}
    Result := LoadLibrary(ANames[I]);
    {$ENDIF}

    if Result <> 0 then
      Break;  // 加载成功
  end;
end;

class function TOpenSSLLoader.GetLibraryHandle(ALibType: TOpenSSLLibraryType): {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF};
begin
  Result := 0;

  case ALibType of
    osslLibCrypto:
    begin
      if FLibCrypto = 0 then
      begin
        // 尝试加载 libcrypto，按优先级顺序
        FLibCrypto := TryLoadLibrary([
          {$IFDEF WINDOWS}
          'libcrypto-3-x64.dll',      // OpenSSL 3.x (Windows 64-bit)
          'libcrypto-3.dll',          // OpenSSL 3.x (Windows 32-bit)
          'libcrypto-1_1-x64.dll',    // OpenSSL 1.1.x (Windows 64-bit)
          'libcrypto-1_1.dll',        // OpenSSL 1.1.x (Windows 32-bit)
          'libeay32.dll'              // OpenSSL 1.0.x (Windows)
          {$ELSE}
          'libcrypto.so.3',           // OpenSSL 3.x (Linux/Unix)
          'libcrypto.so.1.1',         // OpenSSL 1.1.x
          'libcrypto.so.1.0.0',       // OpenSSL 1.0.x
          'libcrypto.so',             // 通用符号链接
          'libcrypto.dylib'           // macOS
          {$ENDIF}
        ]);

        if FLibCrypto <> 0 then
        begin
          FInitialized := True;
          DetectVersion;
        end;
      end;
      Result := FLibCrypto;
    end;

    osslLibSSL:
    begin
      if FLibSSL = 0 then
      begin
        // 尝试加载 libssl
        FLibSSL := TryLoadLibrary([
          {$IFDEF WINDOWS}
          'libssl-3-x64.dll',
          'libssl-3.dll',
          'libssl-1_1-x64.dll',
          'libssl-1_1.dll',
          'ssleay32.dll'
          {$ELSE}
          'libssl.so.3',
          'libssl.so.1.1',
          'libssl.so.1.0.0',
          'libssl.so',
          'libssl.dylib'
          {$ENDIF}
        ]);
      end;
      Result := FLibSSL;
    end;
  end;
end;

class function TOpenSSLLoader.GetFunction(AHandle: {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF}; const AFunctionName: string): Pointer;
begin
  Result := nil;

  if AHandle = 0 then
    Exit;

  {$IFDEF WINDOWS}
  Result := GetProcAddress(AHandle, PChar(AFunctionName));
  {$ELSE}
  Result := GetProcAddress(AHandle, PChar(AFunctionName));
  {$ENDIF}
end;

class function TOpenSSLLoader.IsFunctionAvailable(AHandle: {$IFDEF WINDOWS}HMODULE{$ELSE}TLibHandle{$ENDIF}; const AFunctionName: string): Boolean;
begin
  Result := GetFunction(AHandle, AFunctionName) <> nil;
end;

class procedure TOpenSSLLoader.DetectVersion;
type
  TOpenSSL_version_num = function: culong; cdecl;
  TOpenSSL_version = function(t: cint): PAnsiChar; cdecl;
var
  LVersionNum: TOpenSSL_version_num;
  LVersion: TOpenSSL_version;
  LVersionValue: culong;
begin
  // 初始化默认值
  FillChar(FVersionInfo, SizeOf(FVersionInfo), 0);
  FVersionInfo.VersionString := 'Unknown';

  if FLibCrypto = 0 then
    Exit;

  // 尝试获取版本号
  LVersionNum := TOpenSSL_version_num(GetFunction(FLibCrypto, 'OpenSSL_version_num'));
  if not Assigned(LVersionNum) then
    LVersionNum := TOpenSSL_version_num(GetFunction(FLibCrypto, 'SSLeay'));  // OpenSSL 1.0.x

  if Assigned(LVersionNum) then
  begin
    LVersionValue := LVersionNum();

    // 解析版本号 (格式: 0xMNNFFPPS - Major, miNor, Fix, Patch, Status)
    FVersionInfo.Major := (LVersionValue shr 28) and $FF;
    FVersionInfo.Minor := (LVersionValue shr 20) and $FF;
    FVersionInfo.Patch := (LVersionValue shr 12) and $FF;
    FVersionInfo.IsOpenSSL3 := FVersionInfo.Major >= 3;
  end;

  // 尝试获取版本字符串
  LVersion := TOpenSSL_version(GetFunction(FLibCrypto, 'OpenSSL_version'));
  if not Assigned(LVersion) then
    LVersion := TOpenSSL_version(GetFunction(FLibCrypto, 'SSLeay_version'));  // OpenSSL 1.0.x

  if Assigned(LVersion) then
    FVersionInfo.VersionString := string(LVersion(0));  // OPENSSL_VERSION
end;

class function TOpenSSLLoader.GetVersionInfo: TOpenSSLVersionInfo;
begin
  if not FInitialized then
    GetLibraryHandle(osslLibCrypto);  // 触发加载和版本检测

  Result := FVersionInfo;
end;

class function TOpenSSLLoader.IsOpenSSL3: Boolean;
begin
  Result := GetVersionInfo.IsOpenSSL3;
end;

class procedure TOpenSSLLoader.UnloadLibraries;
begin
  if FLibCrypto <> 0 then
  begin
    FreeLibrary(FLibCrypto);
    FLibCrypto := 0;
  end;

  if FLibSSL <> 0 then
  begin
    FreeLibrary(FLibSSL);
    FLibSSL := 0;
  end;

  FInitialized := False;
  FillChar(FVersionInfo, SizeOf(FVersionInfo), 0);
end;

class function TOpenSSLLoader.IsLoaded(ALibType: TOpenSSLLibraryType): Boolean;
begin
  case ALibType of
    osslLibCrypto: Result := FLibCrypto <> 0;
    osslLibSSL: Result := FLibSSL <> 0;
  else
    Result := False;
  end;
end;

initialization
  TOpenSSLLoader.FLibCrypto := 0;
  TOpenSSLLoader.FLibSSL := 0;
  TOpenSSLLoader.FInitialized := False;

finalization
  TOpenSSLLoader.UnloadLibraries;

end.
