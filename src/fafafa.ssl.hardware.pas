{**
 * Unit: fafafa.ssl.hardware
 * Purpose: 硬件加速抽象层
 *
 * P3-10: 硬件加速支持
 *
 * 功能范围：
 * - CPU 指令集检测 (AES-NI, SHA-NI, AVX 等)
 * - OpenSSL 3.x Provider 选择接口
 * - 硬件密钥存储支持 (PKCS#11, Engine)
 * - 跨后端统一的硬件加速能力查询
 *
 * 设计原则：
 * - 后端无关：抽象层不依赖特定 SSL 后端
 * - 能力门控：不支持的功能通过能力矩阵显式标记
 * - 安全优先：硬件密钥不可导出
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.hardware;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  { CPU 硬件加速特性 }
  TCPUFeature = (
    cpuAESNI,           // AES-NI 指令集 (Intel/AMD)
    cpuSHANI,           // SHA 扩展指令集
    cpuAVX,             // AVX 向量扩展
    cpuAVX2,            // AVX2 向量扩展
    cpuAVX512,          // AVX-512 向量扩展
    cpuRDRAND,          // 硬件随机数生成器
    cpuRDSEED,          // 硬件随机种子
    cpuPCLMULQDQ,       // 多项式乘法 (用于 GCM)
    cpuARMCrypto,       // ARM 加密扩展
    cpuARMNEON          // ARM NEON SIMD
  );
  TCPUFeatures = set of TCPUFeature;

  { 硬件密钥存储类型 }
  THardwareKeyStoreType = (
    hksNone,            // 无硬件密钥存储
    hksPKCS11,          // PKCS#11 (智能卡、HSM)
    hksTPM,             // TPM 可信平台模块
    hksEngine,          // OpenSSL Engine (已弃用，兼容用)
    hksProvider,        // OpenSSL 3.x Provider
    hksCNG,             // Windows CNG (Cryptography Next Generation)
    hksKeychain         // macOS Keychain
  );
  THardwareKeyStoreTypes = set of THardwareKeyStoreType;

  { Provider 类型 (OpenSSL 3.x) }
  TProviderType = (
    provDefault,        // 默认 provider (现代算法)
    provLegacy,         // 遗留 provider (旧算法如 Blowfish, RC4)
    provFIPS,           // FIPS 140-2/3 认证 provider
    provBase,           // 基础 provider (编码器/解码器)
    provNull,           // 空 provider (测试用)
    provCustom          // 自定义 provider
  );
  TProviderTypes = set of TProviderType;

  { 硬件加速能力记录 }
  THardwareCapabilities = record
    // CPU 特性
    CPUFeatures: TCPUFeatures;
    CPUVendor: string;            // 'Intel', 'AMD', 'ARM' 等
    CPUModel: string;             // CPU 型号字符串

    // 硬件密钥存储
    KeyStoreTypes: THardwareKeyStoreTypes;

    // Provider 支持 (OpenSSL 3.x)
    AvailableProviders: TProviderTypes;
    ActiveProviders: TProviderTypes;

    // 性能指标
    HasHardwareAES: Boolean;      // 是否有硬件 AES 加速
    HasHardwareSHA: Boolean;      // 是否有硬件 SHA 加速
    HasHardwareRNG: Boolean;      // 是否有硬件随机数

    // 估算的加速倍数 (相对于纯软件实现)
    AESSpeedup: Double;           // 例如 3.5 表示 3.5 倍加速
    SHASpeedup: Double;
  end;

  { Provider 配置 }
  TProviderConfig = record
    ProviderType: TProviderType;
    Name: string;                 // Provider 名称
    ModulePath: string;           // 模块路径 (可选)
    ConfigFile: string;           // 配置文件路径 (可选)
    Properties: string;           // 属性字符串 (如 'fips=yes')
  end;

  { 硬件密钥引用 }
  THardwareKeyRef = record
    StoreType: THardwareKeyStoreType;
    KeyID: string;                // 密钥标识符
    SlotID: Integer;              // PKCS#11 槽位 ID
    TokenLabel: string;           // 令牌标签
    IsPrivate: Boolean;           // 是否为私钥
  end;

  { IHardwareAcceleration - 硬件加速接口 }
  IHardwareAcceleration = interface
    ['{B1C2D3E4-F5A6-4B7C-8D9E-0A1B2C3D4E5F}']

    { 能力查询 }
    function GetCapabilities: THardwareCapabilities;
    function IsCPUFeatureAvailable(AFeature: TCPUFeature): Boolean;
    function IsProviderAvailable(AProvider: TProviderType): Boolean;
    function IsKeyStoreAvailable(AStoreType: THardwareKeyStoreType): Boolean;

    { Provider 管理 (OpenSSL 3.x) }
    function LoadProvider(const AConfig: TProviderConfig): Boolean;
    function UnloadProvider(AProvider: TProviderType): Boolean;
    function SetDefaultProvider(AProvider: TProviderType): Boolean;
    function GetActiveProviders: TProviderTypes;

    { 硬件密钥操作 }
    function LoadHardwareKey(const AKeyRef: THardwareKeyRef;
      const APIN: string = ''): Pointer;  // 返回 EVP_PKEY 或等效句柄
    function UnloadHardwareKey(AKeyHandle: Pointer): Boolean;
    function ListHardwareKeys(AStoreType: THardwareKeyStoreType): TStringList;

    { 性能提示 }
    function GetRecommendedCipherSuite: string;
    function ShouldUseHardwareAES: Boolean;
    function ShouldUseChaCha20: Boolean;  // 无 AES-NI 时推荐
  end;

{ 全局函数 }

{ 检测 CPU 硬件加速特性 }
function DetectCPUFeatures: TCPUFeatures;

{ 获取 CPU 厂商字符串 }
function GetCPUVendor: string;

{ 检查特定 CPU 特性 }
function HasAESNI: Boolean;
function HasSHANI: Boolean;
function HasRDRAND: Boolean;
function HasAVX: Boolean;
function HasAVX2: Boolean;

{ 获取推荐的密码套件 (基于硬件能力) }
function GetRecommendedCipherSuites: string;

{ 创建 Provider 配置 }
function CreateProviderConfig(AType: TProviderType;
  const AName: string = ''): TProviderConfig;

{ 创建硬件密钥引用 }
function CreateHardwareKeyRef(AStoreType: THardwareKeyStoreType;
  const AKeyID: string; ASlotID: Integer = 0): THardwareKeyRef;

implementation

{$IFDEF CPUX86_64}
  {$DEFINE HAS_CPUID}
{$ENDIF}
{$IFDEF CPUI386}
  {$DEFINE HAS_CPUID}
{$ENDIF}

const
  // CPUID 特性位
  CPUID_FEATURE_AES    = 1 shl 25;  // ECX bit 25
  CPUID_FEATURE_AVX    = 1 shl 28;  // ECX bit 28
  CPUID_FEATURE_PCLMUL = 1 shl 1;   // ECX bit 1
  CPUID_FEATURE_RDRAND = 1 shl 30;  // ECX bit 30

  // CPUID 扩展特性位 (EAX=7, ECX=0)
  CPUID_EXT_AVX2       = 1 shl 5;   // EBX bit 5
  CPUID_EXT_SHA        = 1 shl 29;  // EBX bit 29
  CPUID_EXT_RDSEED     = 1 shl 18;  // EBX bit 18
  CPUID_EXT_AVX512F    = 1 shl 16;  // EBX bit 16

var
  GCPUFeaturesDetected: Boolean = False;
  GCPUFeatures: TCPUFeatures = [];
  GCPUVendor: string = '';

{$IFDEF LINUX}
procedure DetectCPUFeaturesFromProcCpuInfo;
var
  LFile: TextFile;
  LLine: string;
  LFlags: string;
begin
  GCPUFeatures := [];
  GCPUVendor := 'Unknown';

  try
    AssignFile(LFile, '/proc/cpuinfo');
    Reset(LFile);
    try
      while not EOF(LFile) do
      begin
        ReadLn(LFile, LLine);

        // 获取厂商
        if Pos('vendor_id', LLine) = 1 then
        begin
          GCPUVendor := Trim(Copy(LLine, Pos(':', LLine) + 1, Length(LLine)));
        end
        // 获取特性标志
        else if Pos('flags', LLine) = 1 then
        begin
          LFlags := LowerCase(LLine);

          if Pos(' aes', LFlags) > 0 then
            Include(GCPUFeatures, cpuAESNI);
          if Pos(' sha_ni', LFlags) > 0 then
            Include(GCPUFeatures, cpuSHANI);
          if Pos(' avx ', LFlags) > 0 then
            Include(GCPUFeatures, cpuAVX);
          if Pos(' avx2', LFlags) > 0 then
            Include(GCPUFeatures, cpuAVX2);
          if Pos(' avx512f', LFlags) > 0 then
            Include(GCPUFeatures, cpuAVX512);
          if Pos(' rdrand', LFlags) > 0 then
            Include(GCPUFeatures, cpuRDRAND);
          if Pos(' rdseed', LFlags) > 0 then
            Include(GCPUFeatures, cpuRDSEED);
          if Pos(' pclmulqdq', LFlags) > 0 then
            Include(GCPUFeatures, cpuPCLMULQDQ);

          Break;  // 只需要读取第一个 CPU 的信息
        end;
      end;
    finally
      CloseFile(LFile);
    end;
  except
    // 如果无法读取 /proc/cpuinfo，保持默认值
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure DetectCPUFeaturesWindows;
begin
  // Windows 上使用简化检测
  // 现代 x86-64 CPU 通常都支持这些特性
  GCPUFeatures := [];
  GCPUVendor := 'Unknown';

  {$IFDEF CPUX86_64}
  // 假设现代 64 位 CPU 支持基本特性
  Include(GCPUFeatures, cpuAESNI);
  Include(GCPUFeatures, cpuAVX);
  Include(GCPUFeatures, cpuPCLMULQDQ);
  GCPUVendor := 'x86_64';
  {$ENDIF}
end;
{$ENDIF}

procedure DetectCPUFeaturesInternal;
begin
  if GCPUFeaturesDetected then
    Exit;

  {$IFDEF LINUX}
  DetectCPUFeaturesFromProcCpuInfo;
  {$ENDIF}

  {$IFDEF WINDOWS}
  DetectCPUFeaturesWindows;
  {$ENDIF}

  {$IFDEF DARWIN}
  // macOS - 假设现代 Mac 支持基本特性
  GCPUFeatures := [];
  GCPUVendor := 'Apple';
  {$IFDEF CPUX86_64}
  Include(GCPUFeatures, cpuAESNI);
  Include(GCPUFeatures, cpuAVX);
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  Include(GCPUFeatures, cpuARMCrypto);
  Include(GCPUFeatures, cpuARMNEON);
  {$ENDIF}
  {$ENDIF}

  GCPUFeaturesDetected := True;
end;

function DetectCPUFeatures: TCPUFeatures;
begin
  DetectCPUFeaturesInternal;
  Result := GCPUFeatures;
end;

function GetCPUVendor: string;
begin
  DetectCPUFeaturesInternal;
  Result := GCPUVendor;
end;

function HasAESNI: Boolean;
begin
  DetectCPUFeaturesInternal;
  Result := cpuAESNI in GCPUFeatures;
end;

function HasSHANI: Boolean;
begin
  DetectCPUFeaturesInternal;
  Result := cpuSHANI in GCPUFeatures;
end;

function HasRDRAND: Boolean;
begin
  DetectCPUFeaturesInternal;
  Result := cpuRDRAND in GCPUFeatures;
end;

function HasAVX: Boolean;
begin
  DetectCPUFeaturesInternal;
  Result := cpuAVX in GCPUFeatures;
end;

function HasAVX2: Boolean;
begin
  DetectCPUFeaturesInternal;
  Result := cpuAVX2 in GCPUFeatures;
end;

function GetRecommendedCipherSuites: string;
begin
  DetectCPUFeaturesInternal;

  // 根据硬件能力推荐密码套件
  if cpuAESNI in GCPUFeatures then
  begin
    // 有 AES-NI，优先使用 AES-GCM
    Result := 'TLS_AES_256_GCM_SHA384:TLS_AES_128_GCM_SHA256:' +
              'TLS_CHACHA20_POLY1305_SHA256:' +
              'ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256';
  end
  else
  begin
    // 无 AES-NI，优先使用 ChaCha20-Poly1305
    Result := 'TLS_CHACHA20_POLY1305_SHA256:' +
              'TLS_AES_256_GCM_SHA384:TLS_AES_128_GCM_SHA256:' +
              'ECDHE-RSA-CHACHA20-POLY1305:ECDHE-RSA-AES256-GCM-SHA384';
  end;
end;

function CreateProviderConfig(AType: TProviderType;
  const AName: string): TProviderConfig;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ProviderType := AType;

  case AType of
    provDefault: Result.Name := 'default';
    provLegacy:  Result.Name := 'legacy';
    provFIPS:    Result.Name := 'fips';
    provBase:    Result.Name := 'base';
    provNull:    Result.Name := 'null';
    provCustom:  Result.Name := AName;
  end;

  if (AName <> '') and (AType <> provCustom) then
    Result.Name := AName;
end;

function CreateHardwareKeyRef(AStoreType: THardwareKeyStoreType;
  const AKeyID: string; ASlotID: Integer): THardwareKeyRef;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StoreType := AStoreType;
  Result.KeyID := AKeyID;
  Result.SlotID := ASlotID;
  Result.IsPrivate := True;  // 默认为私钥
end;

end.

