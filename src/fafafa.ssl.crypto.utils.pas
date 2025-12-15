unit fafafa.ssl.crypto.utils;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{**
 * Unit: fafafa.ssl.crypto.utils
 * Purpose: 企业级加密工具类，提供完整的加密、哈希和随机数功能
 * 
 * Features:
 * - AES-256-GCM/CBC symmetric encryption
 * - SHA-256/SHA-512 cryptographic hashing  
 * - Secure random number generation with fallback
 * - Hex/Base64 encoding utilities
 * 
 * Thread Safety: All class methods are thread-safe
 * 
 * Dependencies:
 *   - OpenSSL 1.1.1+ or 3.0+
 *   - fafafa.ssl.exceptions
 * 
 * @author fafafa.ssl team
 * @version 2.0.0
 * @since 2025-11-26
 * 
 * @example
 * <code>
 *   // Simple hashing
 *   LHash := TCryptoUtils.SHA256('Hello World');
 *   
 *   // Safe encryption with Try pattern
 *   if TCryptoUtils.TryAES_GCM_Encrypt(LData, LKey, LIV, LResult) then
 *     WriteLn('Success');
 * </code>
 *}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio, // Added for Base64
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rand,
  fafafa.ssl.openssl.api.err; // Added for Base64 error handling

type
  {**
   * 加密算法枚举
   *}
  TEncryptionAlgorithm = (
    ENCRYPT_AES_256_GCM,
    ENCRYPT_AES_256_CBC,
    ENCRYPT_AES_128_GCM,
    ENCRYPT_AES_128_CBC
  );

  {**
   * 哈希算法枚举
   *}
  THashAlgorithm = (
    HASH_SHA256,
    HASH_SHA512,
    HASH_SHA1,
    HASH_MD5
  );

  {**
   * 加密操作结果
   *}
  TEncryptionResult = record
    Success: Boolean;
    Data: TBytes;
    ErrorMessage: string;
    ErrorCode: Integer;
  end;

  {**
   * 流式哈希器 (Phase 2.3.4)
   *
   * 支持增量哈希计算，适用于大数据或流式处理场景。
   * 允许分块输入数据，最后获取完整哈希值。
   *
   * 用法:
   * <code>
   *   LHasher := TStreamingHasher.Create(HASH_SHA256);
   *   try
   *     LHasher.Update(LChunk1);
   *     LHasher.Update(LChunk2);
   *     LHash := LHasher.Finalize;
   *   finally
   *     LHasher.Free;
   *   end;
   * </code>
   *}
  TStreamingHasher = class
  private
    FCtx: PEVP_MD_CTX;
    FAlgorithm: THashAlgorithm;
    FFinalized: Boolean;
    FHashSize: Integer;
    procedure CheckNotFinalized;
  public
    {**
     * 创建流式哈希器
     * @param AAlgorithm 哈希算法
     *}
    constructor Create(AAlgorithm: THashAlgorithm);

    {**
     * 销毁哈希器并释放资源
     *}
    destructor Destroy; override;

    {**
     * 更新哈希（TBytes 输入）
     * @param AData 数据块
     *}
    procedure Update(const AData: TBytes);

    {**
     * 更新哈希（TBytesView 输入，零拷贝）
     * @param ADataView 数据视图
     *}
    procedure UpdateView(const ADataView: TBytesView);

    {**
     * 完成哈希计算并返回结果
     * @return 哈希值（32字节 for SHA256, 64字节 for SHA512）
     * 注意：调用后哈希器进入完成状态，无法继续Update
     *}
    function Finalize: TBytes;

    {**
     * 重置哈希器到初始状态
     * 允许重用同一个哈希器实例
     *}
    procedure Reset;

    {**
     * 检查是否已完成
     *}
    property IsFinalized: Boolean read FFinalized;
  end;

  {**
   * 流式加密器 (Phase 2.3.4)
   *
   * 支持增量加密，适用于大数据或流式处理场景。
   * 注意：仅支持流式模式（如 AES-GCM，不支持 CBC 的流式处理）
   *
   * 用法:
   * <code>
   *   LCipher := TStreamingCipher.CreateEncrypt(ENCRYPT_AES_256_GCM, LKey, LIV);
   *   try
   *     LCipher.Update(LChunk1, LOut1);
   *     LCipher.Update(LChunk2, LOut2);
   *     LCipher.Finalize(LFinal, LTag);
   *   finally
   *     LCipher.Free;
   *   end;
   * </code>
   *}
  TStreamingCipher = class
  private
    FCtx: PEVP_CIPHER_CTX;
    FAlgorithm: TEncryptionAlgorithm;
    FIsEncrypt: Boolean;
    FFinalized: Boolean;
    procedure CheckNotFinalized;
  public
    {**
     * 创建流式加密器（加密模式）
     * @param AAlgorithm 加密算法
     * @param AKey 密钥
     * @param AIV 初始化向量
     *}
    class function CreateEncrypt(
      AAlgorithm: TEncryptionAlgorithm;
      const AKey, AIV: TBytes
    ): TStreamingCipher; static;

    {**
     * 创建流式加密器（解密模式）
     * @param AAlgorithm 加密算法
     * @param AKey 密钥
     * @param AIV 初始化向量
     *}
    class function CreateDecrypt(
      AAlgorithm: TEncryptionAlgorithm;
      const AKey, AIV: TBytes
    ): TStreamingCipher; static;

    {**
     * 销毁加密器并释放资源
     *}
    destructor Destroy; override;

    {**
     * 更新加密/解密（TBytes 输入）
     * @param AData 输入数据
     * @param AResult 输出数据
     * @return 成功返回True
     *}
    function Update(const AData: TBytes; out AResult: TBytes): Boolean;

    {**
     * 更新加密/解密（TBytesView 输入，零拷贝）
     * @param ADataView 输入数据视图
     * @param AResult 输出数据
     * @return 成功返回True
     *}
    function UpdateView(const ADataView: TBytesView; out AResult: TBytes): Boolean;

    {**
     * 完成加密/解密
     * @param AResult 最后的输出块
     * @param ATag GCM模式下的认证标签（加密时输出，解密时输入验证）
     * @return 成功返回True
     *}
    function Finalize(out AResult: TBytes; var ATag: TBytes): Boolean;

    {**
     * 检查是否已完成
     *}
    property IsFinalized: Boolean read FFinalized;

    {**
     * 检查是否为加密模式
     *}
    property IsEncrypt: Boolean read FIsEncrypt;
  end;

  {**
   * 加密工具类 - 企业级版本
   * 
   * 提供完整的加密、解密、哈希和随机数功能。
   * 所有方法都是类方法（静态），无需创建实例。
   * 
   * 线程安全性: 所有方法都是线程安全的
   * 
   * API设计:
   * - 基础方法: 失败抛异常
   * - Try方法: 失败返回False
   * - 多种重载: 支持TBytes/string/Stream
   * - 便利方法: Hex/Base64格式化输出
   *}
  TCryptoUtils = class
  private
    class procedure SystemRandom(ABuffer: PByte; ASize: Integer); static;
    class function GetEVPCipher(AAlgorithm: TEncryptionAlgorithm): PEVP_CIPHER; static;
    class function GetEVPDigest(AAlgorithm: THashAlgorithm): PEVP_MD; static;
    
  public
    class procedure EnsureInitialized; static;
    { ==================== 对称加密 - AES-GCM ==================== }
    
    {**
     * AES-256-GCM 加密（基础版本）
     * 
     * GCM模式提供认证加密（AEAD），确保机密性和完整性。
     * 输出格式: 密文 + 16字节认证标签
     * 
     * @param AData 明文数据
     * @param AKey 32字节(256位)密钥
     * @param AIV 12字节初始化向量（GCM推荐长度）
     * @param AAAD 附加认证数据（可选），不加密但参与认证
     * @return 密文+16字节标签
     * 
     * @raises ESSLInvalidArgument 密钥长度不是32字节
     * @raises ESSLInvalidArgument IV长度不是12字节  
     * @raises ESSLCryptoError OpenSSL操作失败
     *}
    class function AES_GCM_Encrypt(
      const AData, AKey, AIV: TBytes;
      const AAAD: TBytes = nil
    ): TBytes; static;
    
    {**
     * AES-256-GCM 加密（Try版本）
     * 
     * 不抛异常的安全版本，适用于不确定是否成功的场景
     * 
     * @param AData 明文数据
     * @param AKey 32字节密钥
     * @param AIV 12字节IV
     * @param AResult 输出密文
     * @param AAAD 附加认证数据（可选）
     * @return 成功返回True，失败返回False
     *}
    class function TryAES_GCM_Encrypt(
      const AData, AKey, AIV: TBytes;
      out AResult: TBytes;
      const AAAD: TBytes = nil
    ): Boolean; static;
    
    {**
     * AES-256-GCM 解密
     *}
    class function AES_GCM_Decrypt(
      const ACiphertext, AKey, AIV: TBytes;
      const AAAD: TBytes = nil
    ): TBytes; static;
    
    {**
     * AES-256-GCM 解密（Try版本）
     *}
    class function TryAES_GCM_Decrypt(
      const ACiphertext, AKey, AIV: TBytes;
      out AResult: TBytes;
      const AAAD: TBytes = nil
    ): Boolean; static;
    
    {**
     * AES-256-GCM 加密（Ex版本）
     * 返回详细结果对象，不抛出异常
     *}
    class function AES_GCM_EncryptEx(
      const AData, AKey, AIV: TBytes;
      const AAAD: TBytes = nil
    ): TEncryptionResult; static;

    {**
     * AES-256-GCM 解密（Ex版本）
     *}
    class function AES_GCM_DecryptEx(
      const ACiphertext, AKey, AIV: TBytes;
      const AAAD: TBytes = nil
    ): TEncryptionResult; static;
    
    { ==================== 对称加密 - AES-CBC ==================== }
    
    {** AES-256-CBC 加密 *}
    class function AES_CBC_Encrypt(
      const AData, AKey, AIV: TBytes
    ): TBytes; static;

    {** AES-256-CBC 加密（Try版本） *}
    class function TryAES_CBC_Encrypt(
      const AData, AKey, AIV: TBytes;
      out AResult: TBytes
    ): Boolean; static;

    {** AES-256-CBC 解密 *}
    class function AES_CBC_Decrypt(
      const ACiphertext, AKey, AIV: TBytes
    ): TBytes; static;

    {** AES-256-CBC 解密（Try版本） *}
    class function TryAES_CBC_Decrypt(
      const ACiphertext, AKey, AIV: TBytes;
      out AResult: TBytes
    ): Boolean; static;
    
    { ==================== 哈希函数 ==================== }
    
    {**
     * 计算SHA-256哈希（TBytes输入）
     * 
     * @param AData 要计算哈希的数据
     * @return 32字节哈希值
     * @raises ESSLCryptoError OpenSSL操作失败
     *}
    class function SHA256(const AData: TBytes): TBytes; overload; static;
    
    {**
     * 计算SHA-256哈希（string输入）
     * 字符串使用UTF-8编码
     *}
    class function SHA256(const AData: string): TBytes; overload; static;
    
    {**
     * 计算SHA-256哈希（Stream输入）
     * 适用于大文件，流式处理
     *}
    class function SHA256(AStream: TStream): TBytes; overload; static;
    
    {**
     * 计算文件的SHA-256哈希
     * @param AFileName 文件路径
     *}
    class function SHA256File(const AFileName: string): TBytes; static;
    
    {**
     * 计算SHA-256并返回十六进制字符串
     * @param AData 输入数据
     * @return 64字符十六进制字符串（小写）
     *}
    class function SHA256Hex(const AData: TBytes): string; overload; static;
    class function SHA256Hex(const AData: string): string; overload; static;
    
    { Base64编码/解码 }
    class function Base64Encode(const AInput: TBytes): string; overload; static;
    class function Base64Encode(const AInput: string): string; overload; static;
    class function Base64Decode(const AInput: string): TBytes; overload; static;
    class function Base64DecodeString(const AInput: string): string; overload; static;
    
    { SHA-256 (Base64) }
    class function SHA256Base64(const AData: TBytes): string; overload; static;
    class function SHA256Base64(const AData: string): string; overload; static;
    
    {** SHA-512 系列方法（与SHA-256相同的重载模式） *}
    class function SHA512(const AData: TBytes): TBytes; overload; static;
    class function SHA512(const AData: string): TBytes; overload; static;
    class function SHA512(AStream: TStream): TBytes; overload; static;
    class function SHA512File(const AFileName: string): TBytes; static;
    class function SHA512Hex(const AData: TBytes): string; overload; static;
    class function SHA512Hex(const AData: string): string; overload; static;
    
    {** Try 版本哈希方法（不抛异常） *}
    class function TrySHA256(const AData: TBytes; out AResult: TBytes): Boolean; static;
    class function TrySHA256(const AData: string; out AResult: TBytes): Boolean; overload; static;
    class function TrySHA512(const AData: TBytes; out AResult: TBytes): Boolean; static;
    class function TrySHA512(const AData: string; out AResult: TBytes): Boolean; overload; static;

    { ==================== 零拷贝版本 (Phase 2.3.2) ==================== }

    {**
     * SHA-256 哈希（零拷贝版本）
     *
     * 使用 TBytesView 避免输入参数拷贝，提升性能和减少内存分配。
     * 适用于：
     * - 已有 TBytes 数据，不希望拷贝
     * - 大数据处理
     * - 性能敏感场景
     *
     * @param ADataView 输入数据的视图（零拷贝）
     * @return 32字节哈希值
     *}
    class function SHA256View(const ADataView: TBytesView): TBytes; static;

    {**
     * SHA-512 哈希（零拷贝版本）
     *
     * @param ADataView 输入数据的视图（零拷贝）
     * @return 64字节哈希值
     *}
    class function SHA512View(const ADataView: TBytesView): TBytes; static;

    {**
     * AES-256-GCM 加密（零拷贝版本）
     *
     * 使用 TBytesView 避免输入参数（数据、密钥、IV）的拷贝。
     *
     * @param ADataView 明文数据视图（零拷贝）
     * @param AKeyView 32字节密钥视图（零拷贝）
     * @param AIVView 12字节IV视图（零拷贝）
     * @param AResult 输出密文
     * @param ATag 输出16字节认证标签
     * @return 成功返回True，失败返回False
     *}
    class function AES_GCM_EncryptView(
      const ADataView, AKeyView, AIVView: TBytesView;
      out AResult, ATag: TBytes
    ): Boolean; static;

    {**
     * AES-256-GCM 解密（零拷贝版本）
     *
     * @param ACiphertextView 密文视图（零拷贝）
     * @param AKeyView 密钥视图（零拷贝）
     * @param AIVView IV视图（零拷贝）
     * @param ATagView 标签视图（零拷贝）
     * @param AResult 输出明文
     * @return 成功返回True，失败返回False
     *}
    class function AES_GCM_DecryptView(
      const ACiphertextView, AKeyView, AIVView, ATagView: TBytesView;
      out AResult: TBytes
    ): Boolean; static;

    {**
     * Base64 编码（零拷贝版本）
     *
     * @param AInputView 输入数据视图（零拷贝）
     * @return Base64编码字符串
     *}
    class function Base64EncodeView(const AInputView: TBytesView): string; static;

    { ==================== 就地操作 (Phase 2.3.3) ==================== }

    {**
     * AES-256-GCM 就地加密
     *
     * 直接在原始数据缓冲区中进行加密，避免输出分配。
     * 适用于：
     * - 大数据加密（减少内存使用）
     * - 性能敏感场景
     * - 临时数据处理
     *
     * 注意：AData 将被密文覆盖
     *
     * @param AData 输入明文，输出密文（就地修改）
     * @param AKey 32字节密钥
     * @param AIV 12字节IV
     * @param ATag 输出16字节认证标签
     * @return 成功返回True
     *}
    class function AES_GCM_EncryptInPlace(
      var AData: TBytes;
      const AKey, AIV: TBytes;
      out ATag: TBytes
    ): Boolean; static;

    {**
     * AES-256-GCM 就地解密
     *
     * @param AData 输入密文，输出明文（就地修改）
     * @param AKey 32字节密钥
     * @param AIV 12字节IV
     * @param ATag 16字节认证标签
     * @return 成功返回True（认证失败返回False）
     *}
    class function AES_GCM_DecryptInPlace(
      var AData: TBytes;
      const AKey, AIV, ATag: TBytes
    ): Boolean; static;

    {** 通用哈希方法 *}
    class function CalculateHash(
      const AData: TBytes;
      AAlgorithm: THashAlgorithm
    ): TBytes; static;
    
    { ==================== 随机数生成 ==================== }
    
    {**
     * 生成安全随机数
     * 
     * 优先使用OpenSSL RAND_bytes，如不可用则降级使用系统随机源
     * Unix: /dev/urandom
     * Windows: CryptGenRandom (将来实现)
     * 
     * @param ALength 随机字节数
     * @return 随机字节数组
     * @raises ESSLInvalidArgument 长度<=0
     * @raises ESSLCryptoError 随机数生成失败
     *}
    class function SecureRandom(ALength: Integer): TBytes; static;
    
    {**
     * 生成安全随机数（Try版本，不抛异常）
     * @param ALength 随机字节数
     * @param AResult 输出随机字节数组
     * @return 成功返回True
     *}
    class function TrySecureRandom(ALength: Integer; out AResult: TBytes): Boolean; static;
    
    {**
     * 生成加密密钥
     * @param ABits 密钥位数（必须是8的倍数）
     *}
    class function GenerateKey(ABits: Integer = 256): TBytes; static;
    
    {**
     * 生成初始化向量（IV）
     * @param ALength IV字节数（GCM推荐12，CBC推荐16）
     *}
    class function GenerateIV(ALength: Integer = 12): TBytes; static;
    
    { ==================== 工具函数 ==================== }
    
    {**
     * 字节数组转十六进制字符串
     * @param ABytes 输入字节数组
     * @param AUpperCase True=大写，False=小写（默认）
     * @return 十六进制字符串
     *}
    class function BytesToHex(
      const ABytes: TBytes;
      AUpperCase: Boolean = False
    ): string; static;
    
    {**
     * 十六进制字符串转字节数组
     * @param AHex 十六进制字符串（忽略大小写）
     * @raises ESSLInvalidArgument 字符串长度不是偶数或包含非法字符
     *}
    class function HexToBytes(const AHex: string): TBytes; static;
    
    {**
     * 安全比较两个字节数组（防时序攻击）
     * 
     * 使用恒定时间比较，避免时序攻击
     * 
     * @param ABytes1 第一个数组
     * @param ABytes2 第二个数组
     * @return 相等返回True
     *}
    class function SecureCompare(
      const ABytes1, ABytes2: TBytes
    ): Boolean; static;
  end;

{**
 * 哈希算法转字符串
 *}
function HashAlgorithmToString(AAlgorithm: THashAlgorithm): string;

{**
 * 字符串转哈希算法
 *}
function StringToHashAlgorithm(const AName: string): THashAlgorithm;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}

const
  // 缓冲区大小常量
  STREAM_BUFFER_SIZE = 8192;
  
  // 算法相关常量
  AES_256_KEY_SIZE = 32;
  AES_128_KEY_SIZE = 16;
  AES_GCM_IV_SIZE = 12;
  AES_CBC_IV_SIZE = 16;
  GCM_TAG_SIZE = 16;
  
  SHA256_HASH_SIZE = 32;
  SHA512_HASH_SIZE = 64;

var
  GInitialized: Boolean = False;
  GRANDAvailable: Boolean = False;

{ TCryptoUtils }

class procedure TCryptoUtils.EnsureInitialized;
begin
  if GInitialized then
    Exit;
    
  // 加载OpenSSL核心
  if not IsOpenSSLCoreLoaded then
  begin
    try
      LoadOpenSSLCore();
    except
      on E: Exception do
        raise ESSLInitError.Create('Failed to load OpenSSL core: ' + E.Message);
    end;
  end;
  
  if not IsOpenSSLCoreLoaded then
    raise ESSLInitError.Create('OpenSSL core library not available');
  
  // 加载BIO (用于Base64)
  try
    if not Assigned(BIO_new) or not Assigned(BIO_f_base64) then
      LoadOpenSSLBIO();
  except
    on E: Exception do
      raise ESSLInitError.Create('Failed to load BIO module: ' + E.Message);
  end;

  // 加载EVP模块
  try
    LoadEVP(GetCryptoLibHandle);
  except
    on E: Exception do
      raise ESSLInitError.Create('Failed to load EVP module: ' + E.Message);
  end;
  
  // 检测RAND可用性
  GRANDAvailable := Assigned(RAND_bytes);
  
  GInitialized := True;
end;

class procedure TCryptoUtils.SystemRandom(ABuffer: PByte; ASize: Integer);
{$IFDEF UNIX}
var
  LFile: File;
  LBytesRead: Integer;
{$ENDIF}
begin
  {$IFDEF UNIX}
  AssignFile(LFile, '/dev/urandom');
  try
    Reset(LFile, 1);
    try
      BlockRead(LFile, ABuffer^, ASize, LBytesRead);
      if LBytesRead <> ASize then
        raise ESSLCryptoError.Create('Insufficient random bytes from /dev/urandom');
    finally
      CloseFile(LFile);
    end;
  except
    on E: Exception do
      raise ESSLCryptoError.Create('System random source failed: ' + E.Message);
  end;
  {$ELSE}
  // Windows降级（临时方案）
  Randomize;
  var I: Integer;
  for I := 0 to ASize - 1 do
    PByte(PtrUInt(ABuffer) + PtrUInt(I))^ := Byte(Random(256));
  {$ENDIF}
end;

class function TCryptoUtils.GetEVPCipher(AAlgorithm: TEncryptionAlgorithm): PEVP_CIPHER;
begin
  case AAlgorithm of
    ENCRYPT_AES_256_GCM: 
      if Assigned(EVP_aes_256_gcm) then Result := EVP_aes_256_gcm() else Result := nil;
    ENCRYPT_AES_256_CBC: 
      if Assigned(EVP_aes_256_cbc) then Result := EVP_aes_256_cbc() else Result := nil;
    ENCRYPT_AES_128_GCM: 
      if Assigned(EVP_aes_128_gcm) then Result := EVP_aes_128_gcm() else Result := nil;
    ENCRYPT_AES_128_CBC: 
      if Assigned(EVP_aes_128_cbc) then Result := EVP_aes_128_cbc() else Result := nil;
  else
    Result := nil;
  end;
end;

class function TCryptoUtils.GetEVPDigest(AAlgorithm: THashAlgorithm): PEVP_MD;
begin
  case AAlgorithm of
    HASH_SHA256: Result := EVP_sha256();
    HASH_SHA512: Result := EVP_sha512();
    HASH_SHA1: Result := EVP_sha1();
    HASH_MD5: Result := EVP_md5();
  else
    Result := nil;
  end;
end;

{ AES-GCM 加密实现 }

class function TCryptoUtils.AES_GCM_Encrypt(
  const AData, AKey, AIV: TBytes;
  const AAAD: TBytes
): TBytes;
var
  LCtx: PEVP_CIPHER_CTX;
  LCipher: PEVP_CIPHER;
  LOutLen, LTotalLen: Integer;
  LTag: array[0..GCM_TAG_SIZE-1] of Byte;
begin
  Result := nil;
  SetLength(Result, 0);
  
  EnsureInitialized;
  
  // 参数验证
  if Length(AKey) <> AES_256_KEY_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-256-GCM requires %d-byte key, got %d bytes',
      [AES_256_KEY_SIZE, Length(AKey)]
    );
    
  if Length(AIV) <> AES_GCM_IV_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-GCM requires %d-byte IV, got %d bytes',
      [AES_GCM_IV_SIZE, Length(AIV)]
    );
  
  LCipher := GetEVPCipher(ENCRYPT_AES_256_GCM);
  if LCipher = nil then
    raise ESSLCryptoError.Create('Failed to get AES-256-GCM cipher');
  
  LCtx := EVP_CIPHER_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create cipher context');
  
  try
    if EVP_EncryptInit_ex(LCtx, LCipher, nil, @AKey[0], @AIV[0]) <> 1 then
      raise ESSLEncryptionException.CreateWithContext(
        'Failed to initialize AES-GCM encryption',
        sslErrLoadFailed,
        'TCryptoUtils.AESGCMEncrypt',
        Integer(ERR_get_error()),
        sslOpenSSL
      );
    
    // 处理AAD
    if Length(AAAD) > 0 then
      CheckOpenSSLResult(
        EVP_EncryptUpdate(LCtx, nil, LOutLen, @AAAD[0], Length(AAAD)),
        'EVP_EncryptUpdate (AAD)'
      );
    
    Result := nil;
  SetLength(Result, Length(AData) + GCM_TAG_SIZE);
    
    // 加密数据
    if Length(AData) > 0 then
    begin
      CheckOpenSSLResult(
        EVP_EncryptUpdate(LCtx, @Result[0], LOutLen, @AData[0], Length(AData)),
        'EVP_EncryptUpdate'
      );
      LTotalLen := LOutLen;
    end
    else
      LTotalLen := 0;
    
    CheckOpenSSLResult(
      EVP_EncryptFinal_ex(LCtx, @Result[LTotalLen], LOutLen),
      'EVP_EncryptFinal_ex'
    );
    LTotalLen := LTotalLen + LOutLen;
    
    if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_GET_TAG, GCM_TAG_SIZE, @LTag[0]) <> 1 then
      raise ESSLEncryptionException.CreateWithContext(
        'Failed to get authentication tag from AES-GCM encryption',
        sslErrLoadFailed,
        'TCryptoUtils.AESGCMEncrypt',
        Integer(ERR_get_error()),
        sslOpenSSL
      );
    
    Move(LTag[0], Result[LTotalLen], GCM_TAG_SIZE);
    SetLength(Result, LTotalLen + GCM_TAG_SIZE);
    
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.TryAES_GCM_Encrypt(
  const AData, AKey, AIV: TBytes;
  out AResult: TBytes;
  const AAAD: TBytes
): Boolean;
begin
  try
    AResult := AES_GCM_Encrypt(AData, AKey, AIV, AAAD);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.AES_GCM_EncryptEx(
  const AData, AKey, AIV: TBytes;
  const AAAD: TBytes
): TEncryptionResult;
begin
  Result.Success := False;
  Result.Data := nil;
  Result.ErrorCode := 0;
  Result.ErrorMessage := '';
  
  try
    Result.Data := AES_GCM_Encrypt(AData, AKey, AIV, AAAD);
    Result.Success := True;
  except
    on E: ESSLException do
    begin
      Result.ErrorMessage := E.Message;
      Result.ErrorCode := Integer(E.ErrorCode);
    end;
    on E: Exception do
    begin
      Result.ErrorMessage := E.Message;
      Result.ErrorCode := -1;
    end;
  end;
end;

class function TCryptoUtils.AES_GCM_DecryptEx(
  const ACiphertext, AKey, AIV: TBytes;
  const AAAD: TBytes
): TEncryptionResult;
begin
  Result.Success := False;
  Result.Data := nil;
  Result.ErrorCode := 0;
  Result.ErrorMessage := '';
  
  try
    Result.Data := AES_GCM_Decrypt(ACiphertext, AKey, AIV, AAAD);
    Result.Success := True;
  except
    on E: ESSLException do
    begin
      Result.ErrorMessage := E.Message;
      Result.ErrorCode := Integer(E.ErrorCode);
    end;
    on E: Exception do
    begin
      Result.ErrorMessage := E.Message;
      Result.ErrorCode := -1;
    end;
  end;
end;

class function TCryptoUtils.AES_GCM_Decrypt(
  const ACiphertext, AKey, AIV: TBytes;
  const AAAD: TBytes
): TBytes;
var
  LCtx: PEVP_CIPHER_CTX;
  LCipher: PEVP_CIPHER;
  LOutLen: Integer;
  LTag: array[0..GCM_TAG_SIZE-1] of Byte;
  LCiphertextLen: Integer;
  LDummy: array[0..63] of Byte;
begin
  Result := nil;
  SetLength(Result, 0);
  LOutLen := 0;
  
  EnsureInitialized;
  
  if Length(AKey) <> AES_256_KEY_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-256-GCM requires %d-byte key, got %d bytes',
      [AES_256_KEY_SIZE, Length(AKey)]
    );
    
  if Length(AIV) <> AES_GCM_IV_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-GCM requires %d-byte IV, got %d bytes',
      [AES_GCM_IV_SIZE, Length(AIV)]
    );

  if Length(ACiphertext) < GCM_TAG_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'Ciphertext too short (minimum %d bytes for tag)',
      [GCM_TAG_SIZE]
    );
  
  LCiphertextLen := Length(ACiphertext) - GCM_TAG_SIZE;
  Move(ACiphertext[LCiphertextLen], LTag[0], GCM_TAG_SIZE);
  
  LCipher := GetEVPCipher(ENCRYPT_AES_256_GCM);
  if LCipher = nil then
    raise ESSLCryptoError.Create('Failed to get AES-256-GCM cipher');

  LCtx := EVP_CIPHER_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create cipher context');
  
  if not Assigned(EVP_DecryptInit_ex) then
    raise ESSLCryptoError.Create('EVP_DecryptInit_ex not loaded');
  if not Assigned(EVP_DecryptUpdate) then
    raise ESSLCryptoError.Create('EVP_DecryptUpdate not loaded');
  if not Assigned(EVP_DecryptFinal_ex) then
    raise ESSLCryptoError.Create('EVP_DecryptFinal_ex not loaded');
    
  try
    if EVP_DecryptInit_ex(LCtx, LCipher, nil, @AKey[0], @AIV[0]) <> 1 then
      raise ESSLDecryptionException.CreateWithContext(
        'Failed to initialize AES-GCM decryption',
        sslErrLoadFailed,
        'TCryptoUtils.AESGCMDecrypt',
        Integer(ERR_get_error()),
        sslOpenSSL
      );
    
    if Length(AAAD) > 0 then
      CheckOpenSSLResult(
        EVP_DecryptUpdate(LCtx, nil, LOutLen, @AAAD[0], Length(AAAD)),
        'EVP_DecryptUpdate (AAD)'
      );
    
    SetLength(Result, LCiphertextLen);
    
    if LCiphertextLen > 0 then
      CheckOpenSSLResult(
        EVP_DecryptUpdate(LCtx, @Result[0], LOutLen, @ACiphertext[0], LCiphertextLen),
        'EVP_DecryptUpdate'
      );
    
    CheckOpenSSLResult(
      EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_TAG, GCM_TAG_SIZE, @LTag[0]),
      'EVP_CIPHER_CTX_ctrl (SET_TAG)'
    );
    
    // Disable padding (GCM doesn't use it, but just in case)
    EVP_CIPHER_CTX_set_padding(LCtx, 0);

    LOutLen := 0; 
    if EVP_DecryptFinal_ex(LCtx, @LDummy[0], LOutLen) <> 1 then
      raise ESSLDecryptionException.CreateWithContext(
        'AES-GCM decryption failed during finalization (authentication failed)',
        sslErrLoadFailed,
        'TCryptoUtils.AESGCMDecrypt',
        Integer(GetLastOpenSSLError),
        sslOpenSSL
      );
      
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.TryAES_GCM_Decrypt(
  const ACiphertext, AKey, AIV: TBytes;
  out AResult: TBytes;
  const AAAD: TBytes
): Boolean;
begin
  try
    AResult := AES_GCM_Decrypt(ACiphertext, AKey, AIV, AAAD);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

{ AES-CBC 实现... (保持与原来相同) }

class function TCryptoUtils.AES_CBC_Encrypt(
  const AData, AKey, AIV: TBytes
): TBytes;
var
  LCtx: PEVP_CIPHER_CTX;
  LOutLen, LTotalLen: Integer;
begin
  EnsureInitialized;
  
  if Length(AKey) <> AES_256_KEY_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-256-CBC requires %d-byte key, got %d bytes',
      [AES_256_KEY_SIZE, Length(AKey)]
    );
    
  if Length(AIV) <> AES_CBC_IV_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-CBC requires %d-byte IV, got %d bytes',
      [AES_CBC_IV_SIZE, Length(AIV)]
    );

  LCtx := EVP_CIPHER_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create cipher context');
    
  try
    CheckOpenSSLResult(
      EVP_EncryptInit_ex(LCtx, EVP_aes_256_cbc(), nil, @AKey[0], @AIV[0]),
      'EVP_EncryptInit_ex'
    );
    
    Result := nil;
  SetLength(Result, Length(AData) + AES_CBC_IV_SIZE); // CBC output can be up to input_len + block_size - 1
    
    if Length(AData) > 0 then
    begin
      CheckOpenSSLResult(
        EVP_EncryptUpdate(LCtx, @Result[0], LOutLen, @AData[0], Length(AData)),
        'EVP_EncryptUpdate'
      );
      LTotalLen := LOutLen;
    end
    else
      LTotalLen := 0;
    
    CheckOpenSSLResult(
      EVP_EncryptFinal_ex(LCtx, @Result[LTotalLen], LOutLen),
      'EVP_EncryptFinal_ex'
    );
    
    SetLength(Result, LTotalLen + LOutLen);
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.AES_CBC_Decrypt(
  const ACiphertext, AKey, AIV: TBytes
): TBytes;
var
  LCtx: PEVP_CIPHER_CTX;
  LOutLen, LTotalLen: Integer;
begin
  EnsureInitialized;

  if Length(AKey) <> AES_256_KEY_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-256-CBC requires %d-byte key, got %d bytes',
      [AES_256_KEY_SIZE, Length(AKey)]
    );
    
  if Length(AIV) <> AES_CBC_IV_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'AES-CBC requires %d-byte IV, got %d bytes',
      [AES_CBC_IV_SIZE, Length(AIV)]
    );
  
  LCtx := EVP_CIPHER_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create cipher context');
    
  try
    CheckOpenSSLResult(
      EVP_DecryptInit_ex(LCtx, EVP_aes_256_cbc(), nil, @AKey[0], @AIV[0]),
      'EVP_DecryptInit_ex'
    );
    
    Result := nil;
  SetLength(Result, Length(ACiphertext)); // Max possible output length
    
    if Length(ACiphertext) > 0 then
    begin
      if EVP_DecryptUpdate(LCtx, @Result[0], LOutLen, @ACiphertext[0], Length(ACiphertext)) <> 1 then
    raise ESSLDecryptionException.CreateWithContext(
      'AES-CBC decryption failed during update phase',
      sslErrLoadFailed,
      'TCryptoUtils.AESCBCDecrypt',
      Integer(GetLastOpenSSLError),
      sslOpenSSL
    );
      LTotalLen := LOutLen;
    end
    else
      LTotalLen := 0;
    
    CheckOpenSSLResult(
      EVP_DecryptFinal_ex(LCtx, @Result[LTotalLen], LOutLen),
      'EVP_DecryptFinal_ex'
    );
    
    SetLength(Result, LTotalLen + LOutLen);
  finally
    EVP_CIPHER_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.TryAES_CBC_Encrypt(
  const AData, AKey, AIV: TBytes;
  out AResult: TBytes
): Boolean;
begin
  try
    AResult := AES_CBC_Encrypt(AData, AKey, AIV);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.TryAES_CBC_Decrypt(
  const ACiphertext, AKey, AIV: TBytes;
  out AResult: TBytes
): Boolean;
begin
  try
    AResult := AES_CBC_Decrypt(ACiphertext, AKey, AIV);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

{ SHA-256 实现 - 完整的重载集合 }

class function TCryptoUtils.SHA256(const AData: TBytes): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LLen: Cardinal;
begin
  EnsureInitialized;
  
  LCtx := EVP_MD_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create digest context');
    
  try
    CheckOpenSSLResult(
      EVP_DigestInit_ex(LCtx, EVP_sha256(), nil),
      'EVP_DigestInit_ex'
    );
    
    if Length(AData) > 0 then
      CheckOpenSSLResult(
        EVP_DigestUpdate(LCtx, @AData[0], Length(AData)),
        'EVP_DigestUpdate'
      );
      
    Result := nil;
  SetLength(Result, SHA256_HASH_SIZE);
    CheckOpenSSLResult(
      EVP_DigestFinal_ex(LCtx, @Result[0], LLen),
      'EVP_DigestFinal_ex'
    );
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.SHA256(const AData: string): TBytes;
begin
  Result := SHA256(TEncoding.UTF8.GetBytes(AData));
end;

class function TCryptoUtils.SHA256(AStream: TStream): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LBuffer: array[0..STREAM_BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LLen: Cardinal;
begin
  EnsureInitialized;
  
  if AStream = nil then
    raise ESSLInvalidArgument.Create('Stream cannot be nil');
  
  LCtx := EVP_MD_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create digest context');
    
  try
    CheckOpenSSLResult(
      EVP_DigestInit_ex(LCtx, EVP_sha256(), nil),
      'EVP_DigestInit_ex'
    );
    
    // 流式处理
    repeat
      LBytesRead := AStream.Read(LBuffer, STREAM_BUFFER_SIZE);
      if LBytesRead > 0 then
        CheckOpenSSLResult(
          EVP_DigestUpdate(LCtx, @LBuffer[0], LBytesRead),
          'EVP_DigestUpdate'
        );
    until LBytesRead = 0;
      
    Result := nil;
  SetLength(Result, SHA256_HASH_SIZE);
    CheckOpenSSLResult(
      EVP_DigestFinal_ex(LCtx, @Result[0], LLen),
      'EVP_DigestFinal_ex'
    );
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.SHA256File(const AFileName: string): TBytes;
var
  LStream: TFileStream;
begin
  if not FileExists(AFileName) then
    raise ESSLInvalidArgument.CreateFmt('File not found: %s', [AFileName]);
    
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := SHA256(LStream);
  finally
    LStream.Free;
  end;
end;

class function TCryptoUtils.SHA256Hex(const AData: TBytes): string;
begin
  Result := BytesToHex(SHA256(AData), False);
end;

class function TCryptoUtils.SHA256Hex(const AData: string): string;
begin
  Result := BytesToHex(SHA256(AData), False);
end;

class function TCryptoUtils.Base64Encode(const AInput: TBytes): string;
var
  LBIO, LB64, LMem: PBIO;
  LPtr: PAnsiChar;
  LLen: Integer;
begin
  Result := '';
  if Length(AInput) = 0 then Exit;
  
  EnsureInitialized;
  
  LMem := BIO_new(BIO_s_mem());
  LB64 := BIO_new(BIO_f_base64());  
  LBIO := BIO_push(LB64, LMem);
  
  try
    // Don't use NO_NL flag - let it add newlines, we'll strip later
    // BIO_set_flags(LBIO, BIO_FLAGS_BASE64_NO_NL);
    
    if BIO_write(LBIO, @AInput[0], Length(AInput)) <= 0 then
      raise ESSLCryptoError.Create('Failed to write data to Base64 BIO');
      
    if BIO_flush(LBIO) <= 0 then
      raise ESSLCryptoError.Create('Failed to flush Base64 BIO');
      
    // 获取数据 - 从内存BIO获取
    LLen := BIO_get_mem_data(LMem, @LPtr);
    
    if LLen > 0 then
    begin
      SetString(Result, LPtr, LLen);
      // 移除所有换行符
      Result := StringReplace(Result, #10, '', [rfReplaceAll]);
      Result := StringReplace(Result, #13, '', [rfReplaceAll]);
    end;
  finally
    BIO_free_all(LBIO);
  end;
end;

class function TCryptoUtils.Base64Encode(const AInput: string): string;
begin
  Result := Base64Encode(TEncoding.UTF8.GetBytes(AInput));
end;

class function TCryptoUtils.Base64Decode(const AInput: string): TBytes;
var
  LBIO, LB64, LMem: PBIO;
  LLen, I, LPos: Integer;
  LBuffer: array of Byte;
  LInputWithNewlines: AnsiString;
begin
  Result := nil;
  SetLength(Result, 0);
  if AInput = '' then Exit;
  
  EnsureInitialized;
  
  // OpenSSL BIO Base64 decoder expects newlines every 64 chars
  // Add them if not present
  LInputWithNewlines := '';
  LPos := 1;
  for I := 1 to Length(AInput) do
  begin
    LInputWithNewlines := LInputWithNewlines + AInput[I];
    if (I mod 64 = 0) and (I < Length(AInput)) then
      LInputWithNewlines := LInputWithNewlines + #10;
  end;
  // Always add trailing newline
  LInputWithNewlines := LInputWithNewlines + #10;
  
  SetLength(LBuffer, Length(AInput)); // 输出肯定小于输入
  
  LMem := BIO_new_mem_buf(PAnsiChar(LInputWithNewlines), Length(LInputWithNewlines));
  LB64 := BIO_new(BIO_f_base64());
  LBIO := BIO_push(LB64, LMem);
  
  try
    LLen := BIO_read(LBIO, @LBuffer[0], Length(LBuffer));
    if LLen > 0 then
    begin
      SetLength(Result, LLen);
      Move(LBuffer[0], Result[0], LLen);
    end;
  finally
    BIO_free_all(LBIO);
  end;
end;

class function TCryptoUtils.Base64DecodeString(const AInput: string): string;
begin
  Result := TEncoding.UTF8.GetString(Base64Decode(AInput));
end;

class function TCryptoUtils.SHA256Base64(const AData: TBytes): string;
begin
  Result := Base64Encode(SHA256(AData));
end;

class function TCryptoUtils.SHA256Base64(const AData: string): string;
begin
  Result := Base64Encode(SHA256(AData));
end;

{ SHA-512实现 - 与SHA-256相同模式 }

class function TCryptoUtils.SHA512(const AData: TBytes): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LLen: Cardinal;
begin
  EnsureInitialized;
  
  LCtx := EVP_MD_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create digest context');
    
  try
    CheckOpenSSLResult(EVP_DigestInit_ex(LCtx, EVP_sha512(), nil), 'EVP_DigestInit_ex');
    if Length(AData) > 0 then
      CheckOpenSSLResult(EVP_DigestUpdate(LCtx, @AData[0], Length(AData)), 'EVP_DigestUpdate');
    Result := nil;
  SetLength(Result, SHA512_HASH_SIZE);
    CheckOpenSSLResult(EVP_DigestFinal_ex(LCtx, @Result[0], LLen), 'EVP_DigestFinal_ex');
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.SHA512(const AData: string): TBytes;
begin
  Result := SHA512(TEncoding.UTF8.GetBytes(AData));
end;

class function TCryptoUtils.SHA512(AStream: TStream): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LBuffer: array[0..STREAM_BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LLen: Cardinal;
begin
  EnsureInitialized;
  
  LCtx := EVP_MD_CTX_new();
  try
    CheckOpenSSLResult(EVP_DigestInit_ex(LCtx, EVP_sha512(), nil), 'EVP_DigestInit_ex');
    repeat
      LBytesRead := AStream.Read(LBuffer, STREAM_BUFFER_SIZE);
      if LBytesRead > 0 then
        CheckOpenSSLResult(EVP_DigestUpdate(LCtx, @LBuffer[0], LBytesRead), 'EVP_DigestUpdate');
    until LBytesRead = 0;
    Result := nil;
  SetLength(Result, SHA512_HASH_SIZE);
    CheckOpenSSLResult(EVP_DigestFinal_ex(LCtx, @Result[0], LLen), 'EVP_DigestFinal_ex');
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.SHA512File(const AFileName: string): TBytes;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := SHA512(LStream);
  finally
    LStream.Free;
  end;
end;

class function TCryptoUtils.SHA512Hex(const AData: TBytes): string;
begin
  Result := BytesToHex(SHA512(AData), False);
end;

class function TCryptoUtils.SHA512Hex(const AData: string): string;
begin
  Result := BytesToHex(SHA512(AData), False);
end;

{ 通用哈希 }

class function TCryptoUtils.CalculateHash(
  const AData: TBytes;
  AAlgorithm: THashAlgorithm
): TBytes;
begin
  case AAlgorithm of
    HASH_SHA256: Result := SHA256(AData);
    HASH_SHA512: Result := SHA512(AData);
  else
    raise ESSLInvalidArgument.CreateFmt(
      'Unsupported hash algorithm: %d',
      [Ord(AAlgorithm)]
    );
  end;
end;

{ Try版本哈希方法实现 }

class function TCryptoUtils.TrySHA256(const AData: TBytes; out AResult: TBytes): Boolean;
begin
  try
    AResult := SHA256(AData);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.TrySHA256(const AData: string; out AResult: TBytes): Boolean;
begin
  try
    AResult := SHA256(AData);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.TrySHA512(const AData: TBytes; out AResult: TBytes): Boolean;
begin
  try
    AResult := SHA512(AData);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.TrySHA512(const AData: string; out AResult: TBytes): Boolean;
begin
  try
    AResult := SHA512(AData);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

{ ==================== 零拷贝版本实现 (Phase 2.3.2) ==================== }

class function TCryptoUtils.SHA256View(const ADataView: TBytesView): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LMD: PEVP_MD;
  LOutLen: Cardinal;
begin
  EnsureInitialized;

  if not ADataView.IsValid then
    raise ESSLInvalidArgument.Create('Invalid TBytesView: data is nil or length is 0');

  LCtx := EVP_MD_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create EVP_MD_CTX');

  try
    LMD := EVP_sha256();
    if LMD = nil then
      raise ESSLCryptoError.Create('Failed to get SHA256 digest');

    if EVP_DigestInit_ex(LCtx, LMD, nil) <> 1 then
      raise ESSLCryptoError.Create('Failed to initialize SHA256 digest');

    // 零拷贝：直接使用视图的指针和长度
    if EVP_DigestUpdate(LCtx, ADataView.Data, ADataView.Length) <> 1 then
      raise ESSLCryptoError.Create('Failed to update SHA256 digest');

    SetLength(Result, 32);
    if EVP_DigestFinal_ex(LCtx, @Result[0], LOutLen) <> 1 then
      raise ESSLCryptoError.Create('Failed to finalize SHA256 digest');

    SetLength(Result, LOutLen);
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.SHA512View(const ADataView: TBytesView): TBytes;
var
  LCtx: PEVP_MD_CTX;
  LMD: PEVP_MD;
  LOutLen: Cardinal;
begin
  EnsureInitialized;

  if not ADataView.IsValid then
    raise ESSLInvalidArgument.Create('Invalid TBytesView: data is nil or length is 0');

  LCtx := EVP_MD_CTX_new();
  if LCtx = nil then
    raise ESSLCryptoError.Create('Failed to create EVP_MD_CTX');

  try
    LMD := EVP_sha512();
    if LMD = nil then
      raise ESSLCryptoError.Create('Failed to get SHA512 digest');

    if EVP_DigestInit_ex(LCtx, LMD, nil) <> 1 then
      raise ESSLCryptoError.Create('Failed to initialize SHA512 digest');

    // 零拷贝：直接使用视图的指针和长度
    if EVP_DigestUpdate(LCtx, ADataView.Data, ADataView.Length) <> 1 then
      raise ESSLCryptoError.Create('Failed to update SHA512 digest');

    SetLength(Result, 64);
    if EVP_DigestFinal_ex(LCtx, @Result[0], LOutLen) <> 1 then
      raise ESSLCryptoError.Create('Failed to finalize SHA512 digest');

    SetLength(Result, LOutLen);
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

class function TCryptoUtils.AES_GCM_EncryptView(
  const ADataView, AKeyView, AIVView: TBytesView;
  out AResult, ATag: TBytes
): Boolean;
var
  LCtx: PEVP_CIPHER_CTX;
  LCipher: PEVP_CIPHER;
  LLen, LCipherLen: Integer;
begin
  Result := False;

  try
    EnsureInitialized;

    // 验证输入视图
    if not ADataView.IsValid then Exit;
    if not AKeyView.IsValid or (AKeyView.Length <> 32) then Exit;
    if not AIVView.IsValid or (AIVView.Length <> 12) then Exit;

    LCtx := EVP_CIPHER_CTX_new();
    if LCtx = nil then Exit;

    try
      LCipher := EVP_aes_256_gcm();
      if LCipher = nil then Exit;

      if EVP_EncryptInit_ex(LCtx, LCipher, nil, nil, nil) <> 1 then Exit;

      // 设置 IV 长度
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_IVLEN, AIVView.Length, nil) <> 1 then Exit;

      // 零拷贝：直接使用视图的指针
      if EVP_EncryptInit_ex(LCtx, nil, nil, AKeyView.Data, AIVView.Data) <> 1 then Exit;

      // 分配输出缓冲区
      SetLength(AResult, ADataView.Length + 16);

      // 零拷贝：直接使用输入视图的指针
      if EVP_EncryptUpdate(LCtx, @AResult[0], LLen, ADataView.Data, ADataView.Length) <> 1 then Exit;
      LCipherLen := LLen;

      if EVP_EncryptFinal_ex(LCtx, @AResult[LCipherLen], LLen) <> 1 then Exit;
      LCipherLen := LCipherLen + LLen;

      SetLength(AResult, LCipherLen);

      // 获取认证标签
      SetLength(ATag, 16);
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_GET_TAG, 16, @ATag[0]) <> 1 then Exit;

      Result := True;
    finally
      EVP_CIPHER_CTX_free(LCtx);
    end;
  except
    SetLength(AResult, 0);
    SetLength(ATag, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.AES_GCM_DecryptView(
  const ACiphertextView, AKeyView, AIVView, ATagView: TBytesView;
  out AResult: TBytes
): Boolean;
var
  LCtx: PEVP_CIPHER_CTX;
  LCipher: PEVP_CIPHER;
  LLen, LPlainLen: Integer;
begin
  Result := False;

  try
    EnsureInitialized;

    // 验证输入视图
    if not ACiphertextView.IsValid then Exit;
    if not AKeyView.IsValid or (AKeyView.Length <> 32) then Exit;
    if not AIVView.IsValid or (AIVView.Length <> 12) then Exit;
    if not ATagView.IsValid or (ATagView.Length <> 16) then Exit;

    LCtx := EVP_CIPHER_CTX_new();
    if LCtx = nil then Exit;

    try
      LCipher := EVP_aes_256_gcm();
      if LCipher = nil then Exit;

      if EVP_DecryptInit_ex(LCtx, LCipher, nil, nil, nil) <> 1 then Exit;

      // 设置 IV 长度
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_IVLEN, AIVView.Length, nil) <> 1 then Exit;

      // 零拷贝：直接使用视图的指针
      if EVP_DecryptInit_ex(LCtx, nil, nil, AKeyView.Data, AIVView.Data) <> 1 then Exit;

      // 分配输出缓冲区
      SetLength(AResult, ACiphertextView.Length);

      // 零拷贝：直接使用输入视图的指针
      if EVP_DecryptUpdate(LCtx, @AResult[0], LLen, ACiphertextView.Data, ACiphertextView.Length) <> 1 then Exit;
      LPlainLen := LLen;

      // 设置认证标签（使用临时变量避免修改输入）
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_TAG, ATagView.Length, ATagView.Data) <> 1 then Exit;

      if EVP_DecryptFinal_ex(LCtx, @AResult[LPlainLen], LLen) <> 1 then Exit;
      LPlainLen := LPlainLen + LLen;

      SetLength(AResult, LPlainLen);

      Result := True;
    finally
      EVP_CIPHER_CTX_free(LCtx);
    end;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.Base64EncodeView(const AInputView: TBytesView): string;
var
  LBIO, LB64, LMem: PBIO;
  LPtr: PAnsiChar;
  LLen: Integer;
begin
  EnsureInitialized;

  if not AInputView.IsValid then
  begin
    Result := '';
    Exit;
  end;

  // Same BIO structure as normal Base64Encode
  LMem := BIO_new(BIO_s_mem());
  LB64 := BIO_new(BIO_f_base64());
  LBIO := BIO_push(LB64, LMem);

  try
    // 零拷贝：直接使用视图的指针
    if BIO_write(LBIO, AInputView.Data, AInputView.Length) <= 0 then
      raise ESSLCryptoError.Create('Failed to write to BIO');

    if BIO_flush(LBIO) <= 0 then
      raise ESSLCryptoError.Create('Failed to flush BIO');

    // 获取数据 - 从内存BIO获取
    LLen := BIO_get_mem_data(LMem, @LPtr);

    if LLen > 0 then
    begin
      SetString(Result, LPtr, LLen);
      // 移除所有换行符
      Result := StringReplace(Result, #10, '', [rfReplaceAll]);
      Result := StringReplace(Result, #13, '', [rfReplaceAll]);
    end
    else
      Result := '';
  finally
    BIO_free_all(LBIO);
  end;
end;

{ ==================== 就地操作实现 (Phase 2.3.3) ==================== }

class function TCryptoUtils.AES_GCM_EncryptInPlace(
  var AData: TBytes;
  const AKey, AIV: TBytes;
  out ATag: TBytes
): Boolean;
var
  LCtx: PEVP_CIPHER_CTX;
  LCipher: PEVP_CIPHER;
  LLen, LCipherLen: Integer;
  LDataLen: Integer;
begin
  Result := False;

  try
    EnsureInitialized;

    // Validate inputs
    if Length(AKey) <> 32 then Exit;
    if Length(AIV) <> 12 then Exit;

    LDataLen := Length(AData);
    if LDataLen = 0 then Exit;

    LCtx := EVP_CIPHER_CTX_new();
    if LCtx = nil then Exit;

    try
      LCipher := EVP_aes_256_gcm();
      if LCipher = nil then Exit;

      if EVP_EncryptInit_ex(LCtx, LCipher, nil, nil, nil) <> 1 then Exit;

      // Set IV length
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_IVLEN, Length(AIV), nil) <> 1 then Exit;

      // Initialize with key and IV
      if EVP_EncryptInit_ex(LCtx, nil, nil, @AKey[0], @AIV[0]) <> 1 then Exit;

      // Encrypt in place - output goes back into AData
      if EVP_EncryptUpdate(LCtx, @AData[0], LLen, @AData[0], LDataLen) <> 1 then Exit;
      LCipherLen := LLen;

      if EVP_EncryptFinal_ex(LCtx, @AData[LCipherLen], LLen) <> 1 then Exit;
      LCipherLen := LCipherLen + LLen;

      // Get authentication tag
      SetLength(ATag, 16);
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_GET_TAG, 16, @ATag[0]) <> 1 then Exit;

      // Resize AData if needed (GCM usually doesn't change length)
      SetLength(AData, LCipherLen);

      Result := True;
    finally
      EVP_CIPHER_CTX_free(LCtx);
    end;
  except
    SetLength(ATag, 0);
    Result := False;
  end;
end;

class function TCryptoUtils.AES_GCM_DecryptInPlace(
  var AData: TBytes;
  const AKey, AIV, ATag: TBytes
): Boolean;
var
  LCtx: PEVP_CIPHER_CTX;
  LCipher: PEVP_CIPHER;
  LLen, LPlainLen: Integer;
  LDataLen: Integer;
begin
  Result := False;

  try
    EnsureInitialized;

    // Validate inputs
    if Length(AKey) <> 32 then Exit;
    if Length(AIV) <> 12 then Exit;
    if Length(ATag) <> 16 then Exit;

    LDataLen := Length(AData);
    if LDataLen = 0 then Exit;

    LCtx := EVP_CIPHER_CTX_new();
    if LCtx = nil then Exit;

    try
      LCipher := EVP_aes_256_gcm();
      if LCipher = nil then Exit;

      if EVP_DecryptInit_ex(LCtx, LCipher, nil, nil, nil) <> 1 then Exit;

      // Set IV length
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_IVLEN, Length(AIV), nil) <> 1 then Exit;

      // Initialize with key and IV
      if EVP_DecryptInit_ex(LCtx, nil, nil, @AKey[0], @AIV[0]) <> 1 then Exit;

      // Decrypt in place - output goes back into AData
      if EVP_DecryptUpdate(LCtx, @AData[0], LLen, @AData[0], LDataLen) <> 1 then Exit;
      LPlainLen := LLen;

      // Set authentication tag
      if EVP_CIPHER_CTX_ctrl(LCtx, EVP_CTRL_GCM_SET_TAG, Length(ATag), @ATag[0]) <> 1 then Exit;

      // Disable padding (GCM doesn't use it)
      EVP_CIPHER_CTX_set_padding(LCtx, 0);

      // Finalize and verify tag
      if EVP_DecryptFinal_ex(LCtx, @AData[LPlainLen], LLen) <> 1 then Exit;
      LPlainLen := LPlainLen + LLen;

      // Resize AData to actual plaintext length
      SetLength(AData, LPlainLen);

      Result := True;
    finally
      EVP_CIPHER_CTX_free(LCtx);
    end;
  except
    Result := False;
  end;
end;

class function TCryptoUtils.TrySecureRandom(ALength: Integer; out AResult: TBytes): Boolean;
begin
  try
    AResult := SecureRandom(ALength);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

{ 随机数实现 }

class function TCryptoUtils.SecureRandom(ALength: Integer): TBytes;
begin
  if ALength <= 0 then
    raise ESSLInvalidArgument.CreateFmt(
      'Random length must be positive, got %d',
      [ALength]
    );
    
  EnsureInitialized;
  
  Result := nil;
  SetLength(Result, ALength);
  
  // 尝试OpenSSL RAND_bytes
  if GRANDAvailable then
  begin
    if RAND_bytes(@Result[0], ALength) = 1 then
      Exit;
  end;
  
  // 降级到系统随机源
  SystemRandom(@Result[0], ALength);
end;

class function TCryptoUtils.GenerateKey(ABits: Integer): TBytes;
begin
  if (ABits mod 8) <> 0 then
    raise ESSLInvalidArgument.CreateFmt(
      'Key size must be multiple of 8 bits, got %d',
      [ABits]
    );
    
  if ABits <= 0 then
    raise ESSLInvalidArgument.Create('Key size must be positive');
    
  Result := SecureRandom(ABits div 8);
end;

class function TCryptoUtils.GenerateIV(ALength: Integer): TBytes;
begin
  Result := SecureRandom(ALength);
end;

{ 工具函数实现 }

class function TCryptoUtils.BytesToHex(
  const ABytes: TBytes;
  AUpperCase: Boolean
): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ABytes) do
  begin
    if AUpperCase then
      Result := Result + IntToHex(ABytes[I], 2)
    else
      Result := Result + LowerCase(IntToHex(ABytes[I], 2));
  end;
end;

class function TCryptoUtils.HexToBytes(const AHex: string): TBytes;
var
  I: Integer;
  LHex: string;
begin
  if (Length(AHex) mod 2) <> 0 then
    raise ESSLInvalidArgument.CreateFmt(
      'Hex string length must be even, got %d',
      [Length(AHex)]
    );
    
  Result := nil;
  SetLength(Result, Length(AHex) div 2);
  
  LHex := UpperCase(AHex);
  I := 0;
  while I < Length(LHex) do
  begin
    try
      Result[I div 2] := StrToInt('$' + Copy(LHex, I + 1, 2));
    except
      on E: Exception do
        raise ESSLInvalidArgument.CreateFmt(
          'Invalid hex character at position %d: %s',
          [I + 1, E.Message]
        );
    end;
    Inc(I, 2);
  end;
end;

{
  Note: Base64 encoding/decoding is implemented via OpenSSL BIO functions.
  See fafafa.ssl.openssl.api.bio for BIO-based Base64 operations:
  - BIO_f_base64() for Base64 filter BIO
  - Use in combination with memory BIOs for encoding/decoding
  
  Example usage can be found in existing SSL certificate handling code.
}

class function TCryptoUtils.SecureCompare(
  const ABytes1, ABytes2: TBytes
): Boolean;
var
  I: Integer;
  LDiff: Byte;
begin
  // 长度不同直接返回False（恒定时间）
  if Length(ABytes1) <> Length(ABytes2) then
    Exit(False);
  
  // 恒定时间比较
  LDiff := 0;
  for I := 0 to High(ABytes1) do
    LDiff := LDiff or (ABytes1[I] xor ABytes2[I]);
  
  Result := (LDiff = 0);
end;

{ ==================== 流式处理实现 (Phase 2.3.4) ==================== }

{ TStreamingHasher }

constructor TStreamingHasher.Create(AAlgorithm: THashAlgorithm);
begin
  inherited Create;
  TCryptoUtils.EnsureInitialized;

  FAlgorithm := AAlgorithm;
  FFinalized := False;

  // 设置哈希大小
  case AAlgorithm of
    HASH_SHA256: FHashSize := 32;
    HASH_SHA512: FHashSize := 64;
    HASH_SHA1: FHashSize := 20;
    HASH_MD5: FHashSize := 16;
  else
    raise ESSLInvalidArgument.CreateFmt('Unsupported hash algorithm: %d', [Ord(AAlgorithm)]);
  end;

  FCtx := EVP_MD_CTX_new();
  if FCtx = nil then
    raise ESSLCryptoError.Create('Failed to create digest context');

  try
    if EVP_DigestInit_ex(FCtx, TCryptoUtils.GetEVPDigest(AAlgorithm), nil) <> 1 then
      raise ESSLCryptoError.CreateFmt('Failed to initialize %s digest', [HashAlgorithmToString(AAlgorithm)]);
  except
    EVP_MD_CTX_free(FCtx);
    FCtx := nil;
    raise;
  end;
end;

destructor TStreamingHasher.Destroy;
begin
  if FCtx <> nil then
    EVP_MD_CTX_free(FCtx);
  inherited;
end;

procedure TStreamingHasher.CheckNotFinalized;
begin
  if FFinalized then
    raise ESSLInvalidArgument.Create('Hasher already finalized. Call Reset to reuse.');
end;

procedure TStreamingHasher.Update(const AData: TBytes);
begin
  CheckNotFinalized;

  if Length(AData) = 0 then
    Exit;

  if EVP_DigestUpdate(FCtx, @AData[0], Length(AData)) <> 1 then
    raise ESSLCryptoError.Create('Failed to update digest');
end;

procedure TStreamingHasher.UpdateView(const ADataView: TBytesView);
begin
  CheckNotFinalized;

  if not ADataView.IsValid then
    raise ESSLInvalidArgument.Create('Invalid TBytesView');

  if ADataView.Length = 0 then
    Exit;

  if EVP_DigestUpdate(FCtx, ADataView.Data, ADataView.Length) <> 1 then
    raise ESSLCryptoError.Create('Failed to update digest');
end;

function TStreamingHasher.Finalize: TBytes;
var
  LLen: Cardinal;
begin
  CheckNotFinalized;

  SetLength(Result, FHashSize);
  if EVP_DigestFinal_ex(FCtx, @Result[0], LLen) <> 1 then
    raise ESSLCryptoError.Create('Failed to finalize digest');

  SetLength(Result, LLen);
  FFinalized := True;
end;

procedure TStreamingHasher.Reset;
begin
  FFinalized := False;

  if EVP_DigestInit_ex(FCtx, TCryptoUtils.GetEVPDigest(FAlgorithm), nil) <> 1 then
    raise ESSLCryptoError.CreateFmt('Failed to reset %s digest', [HashAlgorithmToString(FAlgorithm)]);
end;

{ TStreamingCipher }

class function TStreamingCipher.CreateEncrypt(
  AAlgorithm: TEncryptionAlgorithm;
  const AKey, AIV: TBytes
): TStreamingCipher;
var
  LCipher: PEVP_CIPHER;
  LKeySize, LIVSize: Integer;
begin
  TCryptoUtils.EnsureInitialized;

  Result := TStreamingCipher.Create;
  Result.FAlgorithm := AAlgorithm;
  Result.FIsEncrypt := True;
  Result.FFinalized := False;

  // 验证密钥和 IV 大小
  case AAlgorithm of
    ENCRYPT_AES_256_GCM:
    begin
      LKeySize := 32;
      LIVSize := 12;
    end;
    ENCRYPT_AES_256_CBC:
    begin
      LKeySize := 32;
      LIVSize := 16;
    end;
    ENCRYPT_AES_128_GCM:
    begin
      LKeySize := 16;
      LIVSize := 12;
    end;
    ENCRYPT_AES_128_CBC:
    begin
      LKeySize := 16;
      LIVSize := 16;
    end;
  else
    raise ESSLInvalidArgument.CreateFmt('Unsupported algorithm: %d', [Ord(AAlgorithm)]);
  end;

  if Length(AKey) <> LKeySize then
    raise ESSLInvalidArgument.CreateFmt('Invalid key size: expected %d, got %d', [LKeySize, Length(AKey)]);

  if Length(AIV) <> LIVSize then
    raise ESSLInvalidArgument.CreateFmt('Invalid IV size: expected %d, got %d', [LIVSize, Length(AIV)]);

  Result.FCtx := EVP_CIPHER_CTX_new();
  if Result.FCtx = nil then
  begin
    Result.Free;
    raise ESSLCryptoError.Create('Failed to create cipher context');
  end;

  try
    LCipher := TCryptoUtils.GetEVPCipher(AAlgorithm);
    if LCipher = nil then
      raise ESSLCryptoError.Create('Failed to get cipher');

    if EVP_EncryptInit_ex(Result.FCtx, LCipher, nil, @AKey[0], @AIV[0]) <> 1 then
      raise ESSLCryptoError.Create('Failed to initialize encryption');
  except
    Result.Free;
    raise;
  end;
end;

class function TStreamingCipher.CreateDecrypt(
  AAlgorithm: TEncryptionAlgorithm;
  const AKey, AIV: TBytes
): TStreamingCipher;
var
  LCipher: PEVP_CIPHER;
  LKeySize, LIVSize: Integer;
begin
  TCryptoUtils.EnsureInitialized;

  Result := TStreamingCipher.Create;
  Result.FAlgorithm := AAlgorithm;
  Result.FIsEncrypt := False;
  Result.FFinalized := False;

  // 验证密钥和 IV 大小
  case AAlgorithm of
    ENCRYPT_AES_256_GCM:
    begin
      LKeySize := 32;
      LIVSize := 12;
    end;
    ENCRYPT_AES_256_CBC:
    begin
      LKeySize := 32;
      LIVSize := 16;
    end;
    ENCRYPT_AES_128_GCM:
    begin
      LKeySize := 16;
      LIVSize := 12;
    end;
    ENCRYPT_AES_128_CBC:
    begin
      LKeySize := 16;
      LIVSize := 16;
    end;
  else
    raise ESSLInvalidArgument.CreateFmt('Unsupported algorithm: %d', [Ord(AAlgorithm)]);
  end;

  if Length(AKey) <> LKeySize then
    raise ESSLInvalidArgument.CreateFmt('Invalid key size: expected %d, got %d', [LKeySize, Length(AKey)]);

  if Length(AIV) <> LIVSize then
    raise ESSLInvalidArgument.CreateFmt('Invalid IV size: expected %d, got %d', [LIVSize, Length(AIV)]);

  Result.FCtx := EVP_CIPHER_CTX_new();
  if Result.FCtx = nil then
  begin
    Result.Free;
    raise ESSLCryptoError.Create('Failed to create cipher context');
  end;

  try
    LCipher := TCryptoUtils.GetEVPCipher(AAlgorithm);
    if LCipher = nil then
      raise ESSLCryptoError.Create('Failed to get cipher');

    if EVP_DecryptInit_ex(Result.FCtx, LCipher, nil, @AKey[0], @AIV[0]) <> 1 then
      raise ESSLCryptoError.Create('Failed to initialize decryption');
  except
    Result.Free;
    raise;
  end;
end;

destructor TStreamingCipher.Destroy;
begin
  if FCtx <> nil then
    EVP_CIPHER_CTX_free(FCtx);
  inherited;
end;

procedure TStreamingCipher.CheckNotFinalized;
begin
  if FFinalized then
    raise ESSLInvalidArgument.Create('Cipher already finalized');
end;

function TStreamingCipher.Update(const AData: TBytes; out AResult: TBytes): Boolean;
var
  LLen: Integer;
begin
  Result := False;

  try
    CheckNotFinalized;

    if Length(AData) = 0 then
    begin
      SetLength(AResult, 0);
      Exit(True);
    end;

    SetLength(AResult, Length(AData) + 16); // 预留额外空间

    if FIsEncrypt then
    begin
      if EVP_EncryptUpdate(FCtx, @AResult[0], LLen, @AData[0], Length(AData)) <> 1 then
        Exit(False);
    end
    else
    begin
      if EVP_DecryptUpdate(FCtx, @AResult[0], LLen, @AData[0], Length(AData)) <> 1 then
        Exit(False);
    end;

    SetLength(AResult, LLen);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

function TStreamingCipher.UpdateView(const ADataView: TBytesView; out AResult: TBytes): Boolean;
var
  LLen: Integer;
begin
  Result := False;

  try
    CheckNotFinalized;

    if not ADataView.IsValid or (ADataView.Length = 0) then
    begin
      SetLength(AResult, 0);
      Exit(True);
    end;

    SetLength(AResult, ADataView.Length + 16); // 预留额外空间

    if FIsEncrypt then
    begin
      if EVP_EncryptUpdate(FCtx, @AResult[0], LLen, ADataView.Data, ADataView.Length) <> 1 then
        Exit(False);
    end
    else
    begin
      if EVP_DecryptUpdate(FCtx, @AResult[0], LLen, ADataView.Data, ADataView.Length) <> 1 then
        Exit(False);
    end;

    SetLength(AResult, LLen);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

function TStreamingCipher.Finalize(out AResult: TBytes; var ATag: TBytes): Boolean;
var
  LLen: Integer;
  LIsGCM: Boolean;
begin
  Result := False;

  try
    CheckNotFinalized;

    LIsGCM := (FAlgorithm = ENCRYPT_AES_256_GCM) or (FAlgorithm = ENCRYPT_AES_128_GCM);

    SetLength(AResult, 32); // 预留空间用于最后的块

    if FIsEncrypt then
    begin
      if EVP_EncryptFinal_ex(FCtx, @AResult[0], LLen) <> 1 then
        Exit(False);

      SetLength(AResult, LLen);

      // GCM 模式：获取认证标签
      if LIsGCM then
      begin
        SetLength(ATag, 16);
        if EVP_CIPHER_CTX_ctrl(FCtx, EVP_CTRL_GCM_GET_TAG, 16, @ATag[0]) <> 1 then
          Exit(False);
      end;
    end
    else
    begin
      // GCM 解密模式：设置认证标签
      if LIsGCM then
      begin
        if Length(ATag) <> 16 then
          Exit(False);

        if EVP_CIPHER_CTX_ctrl(FCtx, EVP_CTRL_GCM_SET_TAG, 16, @ATag[0]) <> 1 then
          Exit(False);
      end;

      if EVP_DecryptFinal_ex(FCtx, @AResult[0], LLen) <> 1 then
        Exit(False);

      SetLength(AResult, LLen);
    end;

    FFinalized := True;
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

{ 辅助函数 }

function HashAlgorithmToString(AAlgorithm: THashAlgorithm): string;
begin
  case AAlgorithm of
    HASH_SHA256: Result := 'SHA-256';
    HASH_SHA512: Result := 'SHA-512';
    HASH_SHA1: Result := 'SHA-1';
    HASH_MD5: Result := 'MD5';
  else
    Result := 'Unknown';
  end;
end;

function StringToHashAlgorithm(const AName: string): THashAlgorithm;
var
  LName: string;
begin
  LName := UpperCase(AName);
  if (LName = 'SHA256') or (LName = 'SHA-256') then
    Result := HASH_SHA256
  else if (LName = 'SHA512') or (LName = 'SHA-512') then
    Result := HASH_SHA512
  else if (LName = 'SHA1') or (LName = 'SHA-1') then
    Result := HASH_SHA1
  else if LName = 'MD5' then
    Result := HASH_MD5
  else
    raise ESSLInvalidArgument.CreateFmt('Unknown hash algorithm: %s', [AName]);
end;

end.
