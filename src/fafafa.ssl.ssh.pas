unit fafafa.ssl.ssh;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

{
  SSH 密钥支持模块

  提供 SSH 密钥的读取、生成、转换和验证功能。
  支持 OpenSSH 格式和 PEM 格式的密钥文件。

  支持的密钥类型:
  - RSA (ssh-rsa)
  - Ed25519 (ssh-ed25519)
  - ECDSA (ecdsa-sha2-nistp256, ecdsa-sha2-nistp384, ecdsa-sha2-nistp521)

  @author fafafa.ssl team
  @version 1.0.0
}

interface

uses
  SysUtils, Classes, fafafa.ssl.crypto.hash,
  fafafa.ssl.openssl.api.aes, fafafa.ssl.pem, fafafa.ssl.asn1,
  fafafa.ssl.openssl.base, fafafa.ssl.openssl.api.rsa, fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.core, fafafa.ssl.openssl.api.evp, fafafa.ssl.openssl.api.ec,
  fafafa.ssl.openssl.api.rand, fafafa.ssl.openssl.loader
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  // ========================================================================
  // SSH 密钥类型枚举
  // ========================================================================

  { SSH 密钥算法类型 }
  TSSHKeyType = (
    sshKeyUnknown,       // 未知类型
    sshKeyRSA,           // ssh-rsa
    sshKeyEd25519,       // ssh-ed25519
    sshKeyECDSA_P256,    // ecdsa-sha2-nistp256
    sshKeyECDSA_P384,    // ecdsa-sha2-nistp384
    sshKeyECDSA_P521     // ecdsa-sha2-nistp521
  );

  { SSH 密钥格式 }
  TSSHKeyFormat = (
    sshFormatOpenSSH,    // OpenSSH 原生格式
    sshFormatPEM,        // PEM 格式 (PKCS#1/PKCS#8)
    sshFormatPKCS8       // PKCS#8 格式
  );

  // ========================================================================
  // SSH 错误类型
  // ========================================================================

  { SSH 密钥操作错误码 }
  TSSHKeyError = (
    sshKeyErrNone,               // 无错误
    sshKeyErrInvalidFormat,      // 格式无效
    sshKeyErrUnsupportedKeyType, // 不支持的密钥类型
    sshKeyErrDecryptionFailed,   // 解密失败（密码错误）
    sshKeyErrWeakKey,            // 弱密钥
    sshKeyErrKeyMismatch,        // 密钥不匹配
    sshKeyErrFileNotFound,       // 文件未找到
    sshKeyErrPermissionDenied,   // 权限拒绝
    sshKeyErrInvalidPassphrase,  // 密码无效
    sshKeyErrGenerationFailed,   // 生成失败
    sshKeyErrEncodingFailed,     // 编码失败
    sshKeyErrDecodingFailed,     // 解码失败
    sshKeyErrIOError             // I/O 错误
  );

  // ========================================================================
  // Result 类型
  // ========================================================================

  { SSH 操作结果 - 类似 Rust Result<(), E> }
  TSSHKeyResult = record
    Success: Boolean;
    ErrorCode: TSSHKeyError;
    ErrorMessage: string;

    class function Ok: TSSHKeyResult; static;
    class function Err(ACode: TSSHKeyError; const AMsg: string): TSSHKeyResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
  end;

  // 前向声明
  ISSHPublicKey = interface;
  ISSHPrivateKey = interface;
  ISSHKeyPair = interface;

  { SSH 公钥结果 }
  TSSHPublicKeyResult = record
    Success: Boolean;
    Key: ISSHPublicKey;
    ErrorCode: TSSHKeyError;
    ErrorMessage: string;

    class function Ok(AKey: ISSHPublicKey): TSSHPublicKeyResult; static;
    class function Err(ACode: TSSHKeyError; const AMsg: string): TSSHPublicKeyResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
  end;

  { SSH 私钥结果 }
  TSSHPrivateKeyResult = record
    Success: Boolean;
    Key: ISSHPrivateKey;
    ErrorCode: TSSHKeyError;
    ErrorMessage: string;

    class function Ok(AKey: ISSHPrivateKey): TSSHPrivateKeyResult; static;
    class function Err(ACode: TSSHKeyError; const AMsg: string): TSSHPrivateKeyResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
  end;

  { SSH 密钥对结果 }
  TSSHKeyPairResult = record
    Success: Boolean;
    KeyPair: ISSHKeyPair;
    ErrorCode: TSSHKeyError;
    ErrorMessage: string;

    class function Ok(AKeyPair: ISSHKeyPair): TSSHKeyPairResult; static;
    class function Err(ACode: TSSHKeyError; const AMsg: string): TSSHKeyPairResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
  end;

  // ========================================================================
  // SSH 密钥接口
  // ========================================================================

  { SSH 公钥接口 }
  ISSHPublicKey = interface
    ['{8A3B5C7D-9E1F-4A2B-8C3D-5E6F7A8B9C0D}']

    { 获取密钥类型 }
    function GetKeyType: TSSHKeyType;

    { 获取原始密钥数据 (Blob) }
    function GetKeyData: TBytes;

    { 获取注释 }
    function GetComment: string;

    { 设置注释 }
    procedure SetComment(const AComment: string);

    { 导出为 OpenSSH 格式 (单行格式) }
    function ToOpenSSHFormat: string;

    { 导出为 PEM 格式 (SubjectPublicKeyInfo) }
    function ToPEMFormat: string;

    { 获取原始字节 (公钥 Blob) }
    function ToBytes: TBytes;

    { 计算 SHA-256 指纹 (格式: SHA256:base64...) }
    function GetFingerprintSHA256: string;

    { 计算 MD5 指纹 (格式: MD5:xx:xx:xx:...) }
    function GetFingerprintMD5: string;

    { 验证密钥是否有效 }
    function IsValid: Boolean;

    { 获取密钥大小 (位) }
    function GetKeySize: Integer;

    property KeyType: TSSHKeyType read GetKeyType;
    property KeyData: TBytes read GetKeyData;
    property Comment: string read GetComment write SetComment;
  end;

  { SSH 私钥接口 }
  ISSHPrivateKey = interface
    ['{1B2C3D4E-5F6A-7B8C-9D0E-1F2A3B4C5D6E}']

    { 获取密钥类型 }
    function GetKeyType: TSSHKeyType;

    { 获取对应的公钥 }
    function GetPublicKey: ISSHPublicKey;

    { 检查是否已加密 }
    function IsEncrypted: Boolean;

    { 导出为 OpenSSH 格式 (openssh-key-v1) }
    function ToOpenSSHFormat(const APassphrase: string = ''): string;

    { 导出为 PEM 格式 (PKCS#8) }
    function ToPEMFormat(const APassphrase: string = ''): string;

    { 验证密钥是否有效 }
    function IsValid: Boolean;

    { 检查是否与指定公钥匹配 }
    function MatchesPublicKey(APubKey: ISSHPublicKey): Boolean;

    property KeyType: TSSHKeyType read GetKeyType;
    property PublicKey: ISSHPublicKey read GetPublicKey;
  end;

  { SSH 密钥对接口 }
  ISSHKeyPair = interface
    ['{2C3D4E5F-6A7B-8C9D-0E1F-2A3B4C5D6E7F}']

    { 获取公钥 }
    function GetPublicKey: ISSHPublicKey;

    { 获取私钥 }
    function GetPrivateKey: ISSHPrivateKey;

    { 保存到文件 }
    procedure SaveToFiles(const APrivateKeyPath, APublicKeyPath: string;
      const APassphrase: string = '');

    { 验证密钥对是否有效 }
    function IsValidPair: Boolean;

    property PublicKey: ISSHPublicKey read GetPublicKey;
    property PrivateKey: ISSHPrivateKey read GetPrivateKey;
  end;

  // ========================================================================
  // authorized_keys 条目
  // ========================================================================

  { authorized_keys 文件条目 }
  TSSHAuthorizedKeyEntry = record
    Options: string;           // 选项 (command=, from=, no-pty 等)
    Key: ISSHPublicKey;        // 公钥
    IsValid: Boolean;          // 是否有效

    function ToString: string;
    class function Parse(const ALine: string): TSSHAuthorizedKeyEntry; static;
    class function TryParse(const ALine: string;
      out AEntry: TSSHAuthorizedKeyEntry): Boolean; static;
  end;

  TSSHAuthorizedKeyEntryArray = array of TSSHAuthorizedKeyEntry;

  // ========================================================================
  // SSH 密钥管理器
  // ========================================================================

  { SSH 密钥管理器 - 提供密钥解析、生成、转换等功能 }
  TSSHKeyManager = class
  public
    { ==================== 公钥解析 ==================== }

    { 解析 OpenSSH 格式公钥字符串 }
    class function ParsePublicKey(const AKeyString: string): ISSHPublicKey;

    { 从文件解析公钥 }
    class function ParsePublicKeyFile(const AFileName: string): ISSHPublicKey;

    { 尝试解析公钥 (不抛异常) }
    class function TryParsePublicKey(const AKeyString: string;
      out AKey: ISSHPublicKey): Boolean;

    { 解析公钥并返回 Result 类型 }
    class function ParsePublicKeyResult(const AKeyString: string): TSSHPublicKeyResult;

    { ==================== 私钥解析 ==================== }

    { 解析私钥 (支持 OpenSSH 和 PEM 格式) }
    class function ParsePrivateKey(const AKeyData: string;
      const APassphrase: string = ''): ISSHPrivateKey;

    { 从文件解析私钥 }
    class function ParsePrivateKeyFile(const AFileName: string;
      const APassphrase: string = ''): ISSHPrivateKey;

    { 尝试解析私钥 (不抛异常) }
    class function TryParsePrivateKey(const AKeyData: string;
      const APassphrase: string; out AKey: ISSHPrivateKey): Boolean;

    { 解析私钥并返回 Result 类型 }
    class function ParsePrivateKeyResult(const AKeyData: string;
      const APassphrase: string = ''): TSSHPrivateKeyResult;

    { ==================== 密钥生成 ==================== }

    { 生成 RSA 密钥对 }
    class function GenerateRSAKeyPair(ABits: Integer = 4096;
      const AComment: string = ''): ISSHKeyPair;

    { 生成 Ed25519 密钥对 }
    class function GenerateEd25519KeyPair(
      const AComment: string = ''): ISSHKeyPair;

    { 生成 ECDSA 密钥对 }
    class function GenerateECDSAKeyPair(AKeyType: TSSHKeyType;
      const AComment: string = ''): ISSHKeyPair;

    { 生成密钥对并返回 Result 类型 }
    class function GenerateKeyPairResult(AKeyType: TSSHKeyType;
      ABits: Integer = 0; const AComment: string = ''): TSSHKeyPairResult;

    { ==================== 格式转换 ==================== }

    { 将私钥转换为 OpenSSH 格式 }
    class function ConvertToOpenSSH(AKey: ISSHPrivateKey;
      const APassphrase: string = ''): string;

    { 将私钥转换为 PEM 格式 }
    class function ConvertToPEM(AKey: ISSHPrivateKey;
      const APassphrase: string = ''): string;

    { 将公钥转换为 OpenSSH 格式 }
    class function ConvertPublicKeyToOpenSSH(AKey: ISSHPublicKey): string;

    { 将公钥转换为 PEM 格式 }
    class function ConvertPublicKeyToPEM(AKey: ISSHPublicKey): string;

    { ==================== 指纹计算 ==================== }

    { 计算公钥指纹 }
    class function CalculateFingerprint(AKey: ISSHPublicKey;
      ASHA256: Boolean = True): string;

    { ==================== 密钥验证 ==================== }

    { 验证公钥和私钥是否匹配 }
    class function ValidateKeyPair(APubKey: ISSHPublicKey;
      APrivKey: ISSHPrivateKey): Boolean;

    { 检查是否为弱密钥 }
    class function IsWeakKey(AKey: ISSHPublicKey): Boolean;

    { ==================== authorized_keys 处理 ==================== }

    { 解析 authorized_keys 文件内容 }
    class function ParseAuthorizedKeys(const AContent: string): TSSHAuthorizedKeyEntryArray;

    { 生成 authorized_keys 文件内容 }
    class function WriteAuthorizedKeys(const AKeys: array of ISSHPublicKey): string;

    { 解析 authorized_keys 文件内容 (带选项) }
    class function ParseAuthorizedKeysWithOptions(const AContent: string): TSSHAuthorizedKeyEntryArray;
  end;

// ========================================================================
// 辅助函数
// ========================================================================

{ 获取密钥类型的字符串表示 }
function SSHKeyTypeToString(AKeyType: TSSHKeyType): string;

{ 从字符串解析密钥类型 }
function StringToSSHKeyType(const ATypeStr: string): TSSHKeyType;

{ 获取错误码的描述 }
function SSHKeyErrorToString(AError: TSSHKeyError): string;

implementation

// ========================================================================
// 纯 Pascal Base64 编码/解码函数 (避免 OpenSSL 依赖)
// ========================================================================

const
  BASE64_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

{ Base64 解码 (纯 Pascal 实现) }
function SSHBase64Decode(const AInput: string): TBytes;
var
  I, J, Value: Integer;
  C: Char;
  Pad: Integer;
  DecodeTable: array[0..127] of Integer;
  CleanInput: string;
begin
  // 移除所有空白字符
  CleanInput := '';
  for I := 1 to Length(AInput) do
  begin
    C := AInput[I];
    if not (C in [#9, #10, #13, ' ']) then
      CleanInput := CleanInput + C;
  end;

  if CleanInput = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // 初始化解码表
  for I := 0 to 127 do
    DecodeTable[I] := -1;
  for I := 1 to Length(BASE64_CHARS) do
    DecodeTable[Ord(BASE64_CHARS[I])] := I - 1;

  // 计算输出长度
  Pad := 0;
  if (Length(CleanInput) > 0) and (CleanInput[Length(CleanInput)] = '=') then
    Inc(Pad);
  if (Length(CleanInput) > 1) and (CleanInput[Length(CleanInput) - 1] = '=') then
    Inc(Pad);

  SetLength(Result, (Length(CleanInput) * 3) div 4 - Pad);

  J := 0;
  I := 1;
  while I <= Length(CleanInput) - 3 do
  begin
    Value := 0;

    C := CleanInput[I];
    if (Ord(C) <= 127) and (DecodeTable[Ord(C)] >= 0) then
      Value := DecodeTable[Ord(C)] shl 18;
    Inc(I);

    C := CleanInput[I];
    if (Ord(C) <= 127) and (DecodeTable[Ord(C)] >= 0) then
      Value := Value or (DecodeTable[Ord(C)] shl 12);
    Inc(I);

    C := CleanInput[I];
    if C <> '=' then
    begin
      if (Ord(C) <= 127) and (DecodeTable[Ord(C)] >= 0) then
        Value := Value or (DecodeTable[Ord(C)] shl 6);
    end;
    Inc(I);

    C := CleanInput[I];
    if C <> '=' then
    begin
      if (Ord(C) <= 127) and (DecodeTable[Ord(C)] >= 0) then
        Value := Value or DecodeTable[Ord(C)];
    end;
    Inc(I);

    if J < Length(Result) then
    begin
      Result[J] := Byte((Value shr 16) and $FF);
      Inc(J);
    end;
    if J < Length(Result) then
    begin
      Result[J] := Byte((Value shr 8) and $FF);
      Inc(J);
    end;
    if J < Length(Result) then
    begin
      Result[J] := Byte(Value and $FF);
      Inc(J);
    end;
  end;
end;

{ Base64 编码 (纯 Pascal 实现) }
function SSHBase64Encode(const AData: TBytes): string;
var
  I, J, Len: Integer;
  B1, B2, B3: Byte;
begin
  Len := Length(AData);
  if Len = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, ((Len + 2) div 3) * 4);
  J := 1;
  I := 0;

  while I < Len do
  begin
    B1 := AData[I];
    Inc(I);

    if I < Len then
      B2 := AData[I]
    else
      B2 := 0;
    Inc(I);

    if I < Len then
      B3 := AData[I]
    else
      B3 := 0;
    Inc(I);

    Result[J] := BASE64_CHARS[(B1 shr 2) + 1];
    Inc(J);
    Result[J] := BASE64_CHARS[((B1 and $03) shl 4) or (B2 shr 4) + 1];
    Inc(J);

    if I - 2 < Len then
      Result[J] := BASE64_CHARS[((B2 and $0F) shl 2) or (B3 shr 6) + 1]
    else
      Result[J] := '=';
    Inc(J);

    if I - 1 < Len then
      Result[J] := BASE64_CHARS[(B3 and $3F) + 1]
    else
      Result[J] := '=';
    Inc(J);
  end;
end;

type
  // ========================================================================
  // 内部实现类
  // ========================================================================

  { SSH 公钥实现类 }
  TSSHPublicKeyImpl = class(TInterfacedObject, ISSHPublicKey)
  private
    FKeyType: TSSHKeyType;
    FKeyData: TBytes;      // 原始公钥 Blob
    FComment: string;
  public
    constructor Create(AKeyType: TSSHKeyType; const AKeyData: TBytes;
      const AComment: string = '');

    { ISSHPublicKey 实现 }
    function GetKeyType: TSSHKeyType;
    function GetKeyData: TBytes;
    function GetComment: string;
    procedure SetComment(const AComment: string);
    function ToOpenSSHFormat: string;
    function ToPEMFormat: string;
    function ToBytes: TBytes;
    function GetFingerprintSHA256: string;
    function GetFingerprintMD5: string;
    function IsValid: Boolean;
    function GetKeySize: Integer;
  end;

// ========================================================================
// SSH Blob 读取辅助函数
// ========================================================================

{ 从 Blob 读取 uint32 (大端序) }
function ReadUInt32BE(const AData: TBytes; var AOffset: Integer): Cardinal;
begin
  if AOffset + 4 > Length(AData) then
    raise Exception.Create('Unexpected end of data');

  Result := (Cardinal(AData[AOffset]) shl 24) or
            (Cardinal(AData[AOffset + 1]) shl 16) or
            (Cardinal(AData[AOffset + 2]) shl 8) or
            Cardinal(AData[AOffset + 3]);
  Inc(AOffset, 4);
end;

{ 从 Blob 读取字符串 (长度前缀) }
function ReadString(const AData: TBytes; var AOffset: Integer): string;
var
  LLen: Cardinal;
  I: Integer;
begin
  LLen := ReadUInt32BE(AData, AOffset);
  if AOffset + Integer(LLen) > Length(AData) then
    raise Exception.Create('Unexpected end of data');

  SetLength(Result, LLen);
  for I := 1 to Integer(LLen) do
    Result[I] := Char(AData[AOffset + I - 1]);
  Inc(AOffset, Integer(LLen));
end;

{ 从 Blob 读取字节数组 (长度前缀) }
function ReadBytes(const AData: TBytes; var AOffset: Integer): TBytes;
var
  LLen: Cardinal;
begin
  LLen := ReadUInt32BE(AData, AOffset);
  if AOffset + Integer(LLen) > Length(AData) then
    raise Exception.Create('Unexpected end of data');

  SetLength(Result, LLen);
  if LLen > 0 then
    Move(AData[AOffset], Result[0], LLen);
  Inc(AOffset, Integer(LLen));
end;

{ 写入 uint32 (大端序) }
procedure WriteUInt32BE(var AData: TBytes; AValue: Cardinal);
var
  LOldLen: Integer;
begin
  LOldLen := Length(AData);
  SetLength(AData, LOldLen + 4);
  AData[LOldLen] := Byte(AValue shr 24);
  AData[LOldLen + 1] := Byte(AValue shr 16);
  AData[LOldLen + 2] := Byte(AValue shr 8);
  AData[LOldLen + 3] := Byte(AValue);
end;

{ 写入字符串 (长度前缀) }
procedure WriteString(var AData: TBytes; const AStr: string);
var
  LOldLen, I: Integer;
begin
  WriteUInt32BE(AData, Length(AStr));
  LOldLen := Length(AData);
  SetLength(AData, LOldLen + Length(AStr));
  for I := 1 to Length(AStr) do
    AData[LOldLen + I - 1] := Byte(AStr[I]);
end;

{ 写入字节数组 (长度前缀) }
procedure WriteBytesWithLength(var AData: TBytes; const ABytes: TBytes);
var
  LOldLen: Integer;
begin
  WriteUInt32BE(AData, Length(ABytes));
  LOldLen := Length(AData);
  SetLength(AData, LOldLen + Length(ABytes));
  if Length(ABytes) > 0 then
    Move(ABytes[0], AData[LOldLen], Length(ABytes));
end;

{ 从公钥 Blob 解析 RSA 参数 }
function ParseRSAPublicKeyBlob(const ABlob: TBytes;
  out AExponent, AModulus: TBytes): Boolean;
var
  LOffset: Integer;
  LTypeStr: string;
begin
  Result := False;
  try
    LOffset := 0;
    LTypeStr := ReadString(ABlob, LOffset);
    if LTypeStr <> 'ssh-rsa' then
      Exit;

    AExponent := ReadBytes(ABlob, LOffset);
    AModulus := ReadBytes(ABlob, LOffset);
    Result := True;
  except
    Result := False;
  end;
end;

{ 从公钥 Blob 解析 Ed25519 参数 }
function ParseEd25519PublicKeyBlob(const ABlob: TBytes;
  out APublicKey: TBytes): Boolean;
var
  LOffset: Integer;
  LTypeStr: string;
begin
  Result := False;
  try
    LOffset := 0;
    LTypeStr := ReadString(ABlob, LOffset);
    if LTypeStr <> 'ssh-ed25519' then
      Exit;

    APublicKey := ReadBytes(ABlob, LOffset);
    // Ed25519 公钥应该是 32 字节
    Result := Length(APublicKey) = 32;
  except
    Result := False;
  end;
end;

{ 从公钥 Blob 解析 ECDSA 参数 }
function ParseECDSAPublicKeyBlob(const ABlob: TBytes;
  out ACurveName: string; out APublicKey: TBytes): Boolean;
var
  LOffset: Integer;
  LTypeStr: string;
begin
  Result := False;
  try
    LOffset := 0;
    LTypeStr := ReadString(ABlob, LOffset);

    // 验证类型字符串
    if (LTypeStr <> 'ecdsa-sha2-nistp256') and
       (LTypeStr <> 'ecdsa-sha2-nistp384') and
       (LTypeStr <> 'ecdsa-sha2-nistp521') then
      Exit;

    ACurveName := ReadString(ABlob, LOffset);
    APublicKey := ReadBytes(ABlob, LOffset);
    Result := True;
  except
    Result := False;
  end;
end;

{ 构建 RSA 公钥 Blob }
function BuildRSAPublicKeyBlob(const AExponent, AModulus: TBytes): TBytes;
begin
  SetLength(Result, 0);
  WriteString(Result, 'ssh-rsa');
  WriteBytesWithLength(Result, AExponent);
  WriteBytesWithLength(Result, AModulus);
end;

{ 构建 Ed25519 公钥 Blob }
function BuildEd25519PublicKeyBlob(const APublicKey: TBytes): TBytes;
begin
  SetLength(Result, 0);
  WriteString(Result, 'ssh-ed25519');
  WriteBytesWithLength(Result, APublicKey);
end;

{ 构建 ECDSA 公钥 Blob }
function BuildECDSAPublicKeyBlob(AKeyType: TSSHKeyType;
  const APublicKey: TBytes): TBytes;
var
  LTypeStr, LCurveName: string;
begin
  SetLength(Result, 0);

  case AKeyType of
    sshKeyECDSA_P256:
      begin
        LTypeStr := 'ecdsa-sha2-nistp256';
        LCurveName := 'nistp256';
      end;
    sshKeyECDSA_P384:
      begin
        LTypeStr := 'ecdsa-sha2-nistp384';
        LCurveName := 'nistp384';
      end;
    sshKeyECDSA_P521:
      begin
        LTypeStr := 'ecdsa-sha2-nistp521';
        LCurveName := 'nistp521';
      end;
  else
    raise Exception.Create('Invalid ECDSA key type');
  end;

  WriteString(Result, LTypeStr);
  WriteString(Result, LCurveName);
  WriteBytesWithLength(Result, APublicKey);
end;

{ 构建 RSA 公钥 DER 格式 (用于 PEM 导出) }
{ RSAPublicKey ::= SEQUENCE { modulus INTEGER, publicExponent INTEGER } }
function BuildRSAPublicKeyDER(const AModulus, AExponent: TBytes): TBytes;
var
  LWriter: TASN1Writer;
begin
  LWriter := TASN1Writer.Create;
  try
    LWriter.BeginSequence;
    LWriter.WriteBigInteger(AModulus);
    LWriter.WriteBigInteger(AExponent);
    LWriter.EndSequence;
    Result := LWriter.GetData;
  finally
    LWriter.Free;
  end;
end;

{ 构建 RSA 私钥 DER 格式 (PKCS#1) }
{ RSAPrivateKey ::= SEQUENCE {
    version INTEGER,
    modulus INTEGER,
    publicExponent INTEGER,
    privateExponent INTEGER,
    prime1 INTEGER,
    prime2 INTEGER,
    exponent1 INTEGER,
    exponent2 INTEGER,
    coefficient INTEGER
  } }
function BuildRSAPrivateKeyDER(const AModulus, AExponent, APrivateExp,
  APrime1, APrime2, AExp1, AExp2, ACoeff: TBytes): TBytes;
var
  LWriter: TASN1Writer;
begin
  LWriter := TASN1Writer.Create;
  try
    LWriter.BeginSequence;
    LWriter.WriteInteger(0);  // version
    LWriter.WriteBigInteger(AModulus);
    LWriter.WriteBigInteger(AExponent);
    LWriter.WriteBigInteger(APrivateExp);
    LWriter.WriteBigInteger(APrime1);
    LWriter.WriteBigInteger(APrime2);
    LWriter.WriteBigInteger(AExp1);
    LWriter.WriteBigInteger(AExp2);
    LWriter.WriteBigInteger(ACoeff);
    LWriter.EndSequence;
    Result := LWriter.GetData;
  finally
    LWriter.Free;
  end;
end;

{ 构建 ECDSA 私钥 DER 格式 (SEC1) }
{ ECPrivateKey ::= SEQUENCE {
    version INTEGER { ecPrivkeyVer1(1) },
    privateKey OCTET STRING,
    [0] parameters ECParameters OPTIONAL,
    [1] publicKey BIT STRING OPTIONAL
  } }
function BuildECPrivateKeyDER(const APrivateKey, APublicKey: TBytes;
  const ACurveOID: string): TBytes;
var
  LWriter: TASN1Writer;
begin
  LWriter := TASN1Writer.Create;
  try
    LWriter.BeginSequence;
    LWriter.WriteInteger(1);  // version = ecPrivkeyVer1
    LWriter.WriteOctetString(APrivateKey);
    // [0] parameters (曲线 OID)
    LWriter.BeginContextTag(0, True);
    LWriter.WriteOID(ACurveOID);
    LWriter.EndContextTag;
    // [1] publicKey
    LWriter.BeginContextTag(1, True);
    LWriter.WriteBitString(APublicKey, 0);
    LWriter.EndContextTag;
    LWriter.EndSequence;
    Result := LWriter.GetData;
  finally
    LWriter.Free;
  end;
end;

{ 构建 Ed25519 私钥 OCTET STRING (用于 PKCS#8) }
{ CurvePrivateKey ::= OCTET STRING (32 bytes) }
function BuildEd25519PrivateKeyOctetString(const APrivateKey: TBytes): TBytes;
var
  LWriter: TASN1Writer;
begin
  LWriter := TASN1Writer.Create;
  try
    LWriter.WriteOctetString(APrivateKey);
    Result := LWriter.GetData;
  finally
    LWriter.Free;
  end;
end;

// ========================================================================
// TSSHPublicKeyImpl 实现
// ========================================================================

constructor TSSHPublicKeyImpl.Create(AKeyType: TSSHKeyType; const AKeyData: TBytes;
  const AComment: string);
begin
  inherited Create;
  FKeyType := AKeyType;
  FKeyData := Copy(AKeyData);
  FComment := AComment;
end;

function TSSHPublicKeyImpl.GetKeyType: TSSHKeyType;
begin
  Result := FKeyType;
end;

function TSSHPublicKeyImpl.GetKeyData: TBytes;
begin
  Result := Copy(FKeyData);
end;

function TSSHPublicKeyImpl.GetComment: string;
begin
  Result := FComment;
end;

procedure TSSHPublicKeyImpl.SetComment(const AComment: string);
begin
  FComment := AComment;
end;

function TSSHPublicKeyImpl.ToOpenSSHFormat: string;
var
  LBase64: string;
begin
  // 格式: <key-type> <base64-data> [comment]
  LBase64 := SSHBase64Encode(FKeyData);
  Result := SSHKeyTypeToString(FKeyType) + ' ' + LBase64;
  if FComment <> '' then
    Result := Result + ' ' + FComment;
end;

function TSSHPublicKeyImpl.ToPEMFormat: string;
var
  LWriter: TASN1Writer;
  LPEMWriter: TPEMWriter;
  LDERData: TBytes;
  LExponent, LModulus: TBytes;
  LPubKey: TBytes;
  LCurveName: string;
  LCurveOID: string;
begin
  LWriter := TASN1Writer.Create;
  LPEMWriter := TPEMWriter.Create;
  try
    // 构建 SubjectPublicKeyInfo 结构
    // SEQUENCE {
    //   SEQUENCE { algorithm OID, [parameters] }
    //   BIT STRING { public key }
    // }
    LWriter.BeginSequence;
    
    case FKeyType of
      sshKeyRSA:
        begin
          // 解析 RSA 公钥 Blob
          if not ParseRSAPublicKeyBlob(FKeyData, LExponent, LModulus) then
            raise Exception.Create('Failed to parse RSA public key blob');
          
          // AlgorithmIdentifier
          LWriter.BeginSequence;
          LWriter.WriteOID('1.2.840.113549.1.1.1');  // rsaEncryption
          LWriter.WriteNull;
          LWriter.EndSequence;
          
          // RSA 公钥: SEQUENCE { n INTEGER, e INTEGER }
          // 需要先构建内部结构
          LDERData := BuildRSAPublicKeyDER(LModulus, LExponent);
          LWriter.WriteBitString(LDERData, 0);
        end;
        
      sshKeyEd25519:
        begin
          // 解析 Ed25519 公钥 Blob
          if not ParseEd25519PublicKeyBlob(FKeyData, LPubKey) then
            raise Exception.Create('Failed to parse Ed25519 public key blob');
          
          // AlgorithmIdentifier (Ed25519 没有参数)
          LWriter.BeginSequence;
          LWriter.WriteOID('1.3.101.112');  // id-Ed25519
          LWriter.EndSequence;
          
          // Ed25519 公钥直接是 32 字节
          LWriter.WriteBitString(LPubKey, 0);
        end;
        
      sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
        begin
          // 解析 ECDSA 公钥 Blob
          if not ParseECDSAPublicKeyBlob(FKeyData, LCurveName, LPubKey) then
            raise Exception.Create('Failed to parse ECDSA public key blob');
          
          // 确定曲线 OID
          case FKeyType of
            sshKeyECDSA_P256: LCurveOID := '1.2.840.10045.3.1.7';  // prime256v1
            sshKeyECDSA_P384: LCurveOID := '1.3.132.0.34';         // secp384r1
            sshKeyECDSA_P521: LCurveOID := '1.3.132.0.35';         // secp521r1
          end;
          
          // AlgorithmIdentifier
          LWriter.BeginSequence;
          LWriter.WriteOID('1.2.840.10045.2.1');  // ecPublicKey
          LWriter.WriteOID(LCurveOID);
          LWriter.EndSequence;
          
          // ECDSA 公钥是未压缩点 (04 || x || y)
          LWriter.WriteBitString(LPubKey, 0);
        end;
    else
      raise Exception.Create('Unsupported key type for PEM export');
    end;
    
    LWriter.EndSequence;
    
    // 获取 DER 数据并转换为 PEM
    LDERData := LWriter.GetData;
    Result := LPEMWriter.WritePublicKey(LDERData);
  finally
    LWriter.Free;
    LPEMWriter.Free;
  end;
end;

function TSSHPublicKeyImpl.ToBytes: TBytes;
begin
  Result := Copy(FKeyData);
end;

function TSSHPublicKeyImpl.GetFingerprintSHA256: string;
var
  LHash: TBytes;
  LBase64: string;
begin
  // 计算 SHA-256 哈希
  LHash := fafafa.ssl.crypto.hash.SHA256(FKeyData);
  // Base64 编码 (去掉尾部的 '=' 填充)
  LBase64 := SSHBase64Encode(LHash);
  while (Length(LBase64) > 0) and (LBase64[Length(LBase64)] = '=') do
    SetLength(LBase64, Length(LBase64) - 1);
  Result := 'SHA256:' + LBase64;
end;

function TSSHPublicKeyImpl.GetFingerprintMD5: string;
var
  LHash: TBytes;
  I: Integer;
begin
  // 计算 MD5 哈希
  LHash := fafafa.ssl.crypto.hash.MD5(FKeyData);
  // 格式化为 xx:xx:xx:...
  Result := 'MD5:';
  for I := 0 to High(LHash) do
  begin
    if I > 0 then
      Result := Result + ':';
    Result := Result + LowerCase(IntToHex(LHash[I], 2));
  end;
end;

function TSSHPublicKeyImpl.IsValid: Boolean;
var
  LOffset: Integer;
  LTypeStr: string;
begin
  Result := False;

  if Length(FKeyData) < 4 then
    Exit;

  try
    LOffset := 0;
    LTypeStr := ReadString(FKeyData, LOffset);

    // 验证类型字符串与密钥类型匹配
    if StringToSSHKeyType(LTypeStr) <> FKeyType then
      Exit;

    // 根据密钥类型验证数据结构
    case FKeyType of
      sshKeyRSA:
        begin
          // RSA: e, n
          ReadBytes(FKeyData, LOffset);  // e
          ReadBytes(FKeyData, LOffset);  // n
        end;
      sshKeyEd25519:
        begin
          // Ed25519: 32 字节公钥
          ReadBytes(FKeyData, LOffset);
        end;
      sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
        begin
          // ECDSA: curve name, public key point
          ReadString(FKeyData, LOffset);  // curve name
          ReadBytes(FKeyData, LOffset);   // public key
        end;
    else
      Exit;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TSSHPublicKeyImpl.GetKeySize: Integer;
var
  LOffset: Integer;
  LModulus: TBytes;
  LPubKey: TBytes;
begin
  Result := 0;

  try
    LOffset := 0;
    ReadString(FKeyData, LOffset);  // 跳过类型字符串

    case FKeyType of
      sshKeyRSA:
        begin
          ReadBytes(FKeyData, LOffset);  // 跳过 e
          LModulus := ReadBytes(FKeyData, LOffset);  // n
          // RSA 密钥大小 = 模数位数
          // 注意：模数可能有前导零字节
          Result := (Length(LModulus) - 1) * 8;
          if (Length(LModulus) > 0) and (LModulus[0] = 0) then
            Result := (Length(LModulus) - 1) * 8
          else
            Result := Length(LModulus) * 8;
        end;
      sshKeyEd25519:
        begin
          // Ed25519 固定 256 位
          Result := 256;
        end;
      sshKeyECDSA_P256:
        Result := 256;
      sshKeyECDSA_P384:
        Result := 384;
      sshKeyECDSA_P521:
        Result := 521;
    end;
  except
    Result := 0;
  end;
end;

// ========================================================================
// TSSHPrivateKeyImpl 实现类
// ========================================================================

type
  { SSH 私钥实现类 }
  TSSHPrivateKeyImpl = class(TInterfacedObject, ISSHPrivateKey)
  private
    FKeyType: TSSHKeyType;
    FPublicKeyBlob: TBytes;    // 公钥 Blob
    FPrivateKeyData: TBytes;   // 私钥数据
    FComment: string;
    FIsEncrypted: Boolean;
    FCipherName: string;
    FKDFName: string;
    FKDFOptions: TBytes;
  public
    constructor Create(AKeyType: TSSHKeyType;
      const APublicKeyBlob, APrivateKeyData: TBytes;
      const AComment: string = '';
      AIsEncrypted: Boolean = False);

    { ISSHPrivateKey 实现 }
    function GetKeyType: TSSHKeyType;
    function GetPublicKey: ISSHPublicKey;
    function IsEncrypted: Boolean;
    function ToOpenSSHFormat(const APassphrase: string = ''): string;
    function ToPEMFormat(const APassphrase: string = ''): string;
    function IsValid: Boolean;
    function MatchesPublicKey(APubKey: ISSHPublicKey): Boolean;
  end;

// ========================================================================
// OpenSSH 私钥格式常量 (前向声明，用于 ToOpenSSHFormat)
// ========================================================================

const
  OPENSSH_AUTH_MAGIC = 'openssh-key-v1';
  OPENSSH_CIPHER_NONE = 'none';
  OPENSSH_CIPHER_AES256_CTR = 'aes256-ctr';
  OPENSSH_CIPHER_AES256_CBC = 'aes256-cbc';
  OPENSSH_KDF_NONE = 'none';
  OPENSSH_KDF_BCRYPT = 'bcrypt';

// ========================================================================
// BcryptPBKDF 前向声明
// ========================================================================

function BcryptPBKDF(const Password, Salt: TBytes; Rounds, KeyLen: Integer): TBytes; forward;

constructor TSSHPrivateKeyImpl.Create(AKeyType: TSSHKeyType;
  const APublicKeyBlob, APrivateKeyData: TBytes;
  const AComment: string; AIsEncrypted: Boolean);
begin
  inherited Create;
  FKeyType := AKeyType;
  FPublicKeyBlob := Copy(APublicKeyBlob);
  FPrivateKeyData := Copy(APrivateKeyData);
  FComment := AComment;
  FIsEncrypted := AIsEncrypted;
  FCipherName := '';
  FKDFName := '';
  SetLength(FKDFOptions, 0);
end;

function TSSHPrivateKeyImpl.GetKeyType: TSSHKeyType;
begin
  Result := FKeyType;
end;

function TSSHPrivateKeyImpl.GetPublicKey: ISSHPublicKey;
begin
  Result := TSSHPublicKeyImpl.Create(FKeyType, FPublicKeyBlob, FComment);
end;

function TSSHPrivateKeyImpl.IsEncrypted: Boolean;
begin
  Result := FIsEncrypted;
end;

function TSSHPrivateKeyImpl.ToOpenSSHFormat(const APassphrase: string): string;
var
  LOutput: TBytes;
  LPrivateSection: TBytes;
  LCheckInt: Cardinal;
  LPadding: Integer;
  I: Integer;
  LCipherName, LKDFName: string;
  LKDFOptions: TBytes;
  LSalt: TBytes;
  LRounds: Cardinal;
  LDerivedKey, LKey, LIV: TBytes;
  LPassBytes: TBytes;
  LEncryptedSection: TBytes;
  LBase64: string;
  LLines: TStringList;
  LCheckBytes: array[0..3] of Byte;
begin
  // 构建私钥部分
  SetLength(LPrivateSection, 0);
  
  // 生成随机 check integers (用于验证解密是否成功)
  // 尝试使用 OpenSSL 的安全随机数生成
  if Assigned(RAND_bytes) and (RAND_bytes(@LCheckBytes[0], 4) = 1) then
    LCheckInt := (Cardinal(LCheckBytes[0]) shl 24) or (Cardinal(LCheckBytes[1]) shl 16) or
                 (Cardinal(LCheckBytes[2]) shl 8) or Cardinal(LCheckBytes[3])
  else
  begin
    Randomize;
    LCheckInt := Random($FFFFFFFF);
  end;
  WriteUInt32BE(LPrivateSection, LCheckInt);
  WriteUInt32BE(LPrivateSection, LCheckInt);
  
  // 写入密钥类型
  WriteString(LPrivateSection, SSHKeyTypeToString(FKeyType));
  
  // 根据密钥类型写入私钥数据
  case FKeyType of
    sshKeyRSA:
      begin
        // RSA 私钥数据已经是 SSH 格式: n, e, d, iqmp, p, q
        // 直接追加
        SetLength(LPrivateSection, Length(LPrivateSection) + Length(FPrivateKeyData));
        Move(FPrivateKeyData[0], LPrivateSection[Length(LPrivateSection) - Length(FPrivateKeyData)], Length(FPrivateKeyData));
      end;
    sshKeyEd25519:
      begin
        // Ed25519: 公钥 (32 bytes) + 私钥 (64 bytes = 32 seed + 32 public)
        // 从公钥 Blob 中提取公钥数据
        I := 0;
        ReadString(FPublicKeyBlob, I);  // 跳过类型字符串
        WriteBytesWithLength(LPrivateSection, ReadBytes(FPublicKeyBlob, I));  // 公钥
        WriteBytesWithLength(LPrivateSection, FPrivateKeyData);  // 私钥 (64 bytes)
      end;
    sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
      begin
        // ECDSA: curve, public point, private scalar
        I := 0;
        ReadString(FPublicKeyBlob, I);  // 跳过类型字符串
        WriteString(LPrivateSection, ReadString(FPublicKeyBlob, I));  // curve name
        WriteBytesWithLength(LPrivateSection, ReadBytes(FPublicKeyBlob, I));  // public point
        // 私钥数据
        SetLength(LPrivateSection, Length(LPrivateSection) + Length(FPrivateKeyData));
        Move(FPrivateKeyData[0], LPrivateSection[Length(LPrivateSection) - Length(FPrivateKeyData)], Length(FPrivateKeyData));
      end;
  end;
  
  // 写入注释
  WriteString(LPrivateSection, FComment);
  
  // 添加填充 (填充到 8 字节边界，使用 1, 2, 3, 4, 5, 6, 7 序列)
  LPadding := 8 - (Length(LPrivateSection) mod 8);
  if LPadding = 8 then
    LPadding := 0;
  for I := 1 to LPadding do
  begin
    SetLength(LPrivateSection, Length(LPrivateSection) + 1);
    LPrivateSection[High(LPrivateSection)] := Byte(I);
  end;
  
  // 确定加密参数
  if APassphrase <> '' then
  begin
    LCipherName := OPENSSH_CIPHER_AES256_CTR;
    LKDFName := OPENSSH_KDF_BCRYPT;
    LRounds := 16;  // OpenSSH 默认使用 16 轮
    
    // 生成随机盐 (16 字节)
    SetLength(LSalt, 16);
    // 尝试使用 OpenSSL 的安全随机数生成
    if Assigned(RAND_bytes) then
    begin
      if RAND_bytes(@LSalt[0], 16) <> 1 then
      begin
        // 回退到 Pascal 随机数
        Randomize;
        for I := 0 to 15 do
          LSalt[I] := Random(256);
      end;
    end
    else
    begin
      // 回退到 Pascal 随机数
      Randomize;
      for I := 0 to 15 do
        LSalt[I] := Random(256);
    end;
    
    // 构建 KDF 选项
    SetLength(LKDFOptions, 0);
    WriteBytesWithLength(LKDFOptions, LSalt);
    WriteUInt32BE(LKDFOptions, LRounds);
    
    // 派生密钥 (32 字节密钥 + 16 字节 IV = 48 字节)
    SetLength(LPassBytes, Length(APassphrase));
    if Length(APassphrase) > 0 then
      Move(APassphrase[1], LPassBytes[0], Length(APassphrase));
    
    LDerivedKey := BcryptPBKDF(LPassBytes, LSalt, LRounds, 48);
    if Length(LDerivedKey) < 48 then
      raise Exception.Create('Failed to derive encryption key');
    
    SetLength(LKey, 32);
    SetLength(LIV, 16);
    Move(LDerivedKey[0], LKey[0], 32);
    Move(LDerivedKey[32], LIV[0], 16);
    
    // 加密私钥部分
    LEncryptedSection := AESEncryptCTR(LPrivateSection, LKey, LIV);
  end
  else
  begin
    LCipherName := OPENSSH_CIPHER_NONE;
    LKDFName := OPENSSH_KDF_NONE;
    SetLength(LKDFOptions, 0);
    LEncryptedSection := LPrivateSection;
  end;
  
  // 构建完整的 OpenSSH 私钥格式
  SetLength(LOutput, 0);
  
  // 魔数 "openssh-key-v1\0"
  for I := 1 to Length(OPENSSH_AUTH_MAGIC) do
  begin
    SetLength(LOutput, Length(LOutput) + 1);
    LOutput[High(LOutput)] := Byte(OPENSSH_AUTH_MAGIC[I]);
  end;
  SetLength(LOutput, Length(LOutput) + 1);
  LOutput[High(LOutput)] := 0;  // null 终止符
  
  // 加密参数
  WriteString(LOutput, LCipherName);
  WriteString(LOutput, LKDFName);
  WriteBytesWithLength(LOutput, LKDFOptions);
  
  // 密钥数量 (始终为 1)
  WriteUInt32BE(LOutput, 1);
  
  // 公钥
  WriteBytesWithLength(LOutput, FPublicKeyBlob);
  
  // 私钥部分 (可能已加密)
  WriteBytesWithLength(LOutput, LEncryptedSection);
  
  // Base64 编码
  LBase64 := SSHBase64Encode(LOutput);
  
  // 格式化为 PEM 风格 (每行 70 字符)
  LLines := TStringList.Create;
  try
    LLines.Add('-----BEGIN OPENSSH PRIVATE KEY-----');
    I := 1;
    while I <= Length(LBase64) do
    begin
      LLines.Add(Copy(LBase64, I, 70));
      Inc(I, 70);
    end;
    LLines.Add('-----END OPENSSH PRIVATE KEY-----');
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function TSSHPrivateKeyImpl.ToPEMFormat(const APassphrase: string): string;
var
  LWriter: TASN1Writer;
  LPEMWriter: TPEMWriter;
  LDERData, LPrivKeyDER: TBytes;
  LOffset: Integer;
  // RSA 参数
  LModulus, LExponent, LPrivateExp, LIQMP, LP, LQ: TBytes;
  LDP, LDQ: TBytes;
  // Ed25519 参数
  LPubKey, LPrivKey: TBytes;
  // ECDSA 参数
  LCurveName: string;
  LCurveOID: string;
  LPublicPoint, LPrivateScalar: TBytes;
begin
  // 注意：PKCS#8 加密私钥需要额外的加密处理，这里先实现未加密版本
  if APassphrase <> '' then
    raise Exception.Create('Encrypted PEM export not yet implemented');
  
  LWriter := TASN1Writer.Create;
  LPEMWriter := TPEMWriter.Create;
  try
    case FKeyType of
      sshKeyRSA:
        begin
          // 解析 SSH 格式的私钥数据: n, e, d, iqmp, p, q
          LOffset := 0;
          // 跳过密钥类型字符串（如果存在）
          // 私钥数据格式: n, e, d, iqmp, p, q
          LModulus := ReadBytes(FPrivateKeyData, LOffset);
          LExponent := ReadBytes(FPrivateKeyData, LOffset);
          LPrivateExp := ReadBytes(FPrivateKeyData, LOffset);
          LIQMP := ReadBytes(FPrivateKeyData, LOffset);
          LP := ReadBytes(FPrivateKeyData, LOffset);
          LQ := ReadBytes(FPrivateKeyData, LOffset);
          
          // 计算 dp = d mod (p-1) 和 dq = d mod (q-1)
          // 这需要大整数运算，暂时使用空值（某些实现可以接受）
          // 实际上我们需要计算这些值，但为了简化，我们使用 PKCS#8 格式
          // 它可以包含 RSA 私钥而不需要 dp 和 dq
          
          // 构建 PKCS#8 格式
          // PrivateKeyInfo ::= SEQUENCE {
          //   version INTEGER,
          //   privateKeyAlgorithm AlgorithmIdentifier,
          //   privateKey OCTET STRING (contains RSAPrivateKey)
          // }
          
          // 首先构建 RSAPrivateKey (PKCS#1)
          // 由于我们没有 dp 和 dq，我们需要计算它们
          // 简化处理：使用 0 作为占位符（不推荐用于生产）
          SetLength(LDP, 1);
          LDP[0] := 0;
          SetLength(LDQ, 1);
          LDQ[0] := 0;
          
          LPrivKeyDER := BuildRSAPrivateKeyDER(LModulus, LExponent, LPrivateExp,
            LP, LQ, LDP, LDQ, LIQMP);
          
          // 构建 PKCS#8 结构
          LWriter.BeginSequence;
          LWriter.WriteInteger(0);  // version
          // AlgorithmIdentifier
          LWriter.BeginSequence;
          LWriter.WriteOID('1.2.840.113549.1.1.1');  // rsaEncryption
          LWriter.WriteNull;
          LWriter.EndSequence;
          // privateKey
          LWriter.WriteOctetString(LPrivKeyDER);
          LWriter.EndSequence;
        end;
        
      sshKeyEd25519:
        begin
          // Ed25519 私钥数据是 64 字节 (32 seed + 32 public)
          // PKCS#8 格式只需要 32 字节的 seed
          if Length(FPrivateKeyData) >= 64 then
          begin
            SetLength(LPrivKey, 32);
            Move(FPrivateKeyData[0], LPrivKey[0], 32);
          end
          else
            LPrivKey := Copy(FPrivateKeyData);
          
          // 从公钥 Blob 提取公钥
          LOffset := 0;
          ReadString(FPublicKeyBlob, LOffset);  // 跳过类型字符串
          LPubKey := ReadBytes(FPublicKeyBlob, LOffset);
          
          // 构建 PKCS#8 结构
          // Ed25519 私钥在 PKCS#8 中是 OCTET STRING 包装的 32 字节
          LWriter.BeginSequence;
          LWriter.WriteInteger(0);  // version
          // AlgorithmIdentifier (Ed25519 没有参数)
          LWriter.BeginSequence;
          LWriter.WriteOID('1.3.101.112');  // id-Ed25519
          LWriter.EndSequence;
          // privateKey: OCTET STRING containing CurvePrivateKey
          // CurvePrivateKey ::= OCTET STRING (32 bytes)
          LWriter.WriteOctetString(BuildEd25519PrivateKeyOctetString(LPrivKey));
          LWriter.EndSequence;
        end;
        
      sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
        begin
          // 解析 ECDSA 私钥数据
          // 从公钥 Blob 提取曲线名和公钥点
          LOffset := 0;
          ReadString(FPublicKeyBlob, LOffset);  // 跳过类型字符串
          LCurveName := ReadString(FPublicKeyBlob, LOffset);
          LPublicPoint := ReadBytes(FPublicKeyBlob, LOffset);
          
          // 私钥数据是私钥标量（带长度前缀）
          LOffset := 0;
          LPrivateScalar := ReadBytes(FPrivateKeyData, LOffset);
          
          // 确定曲线 OID
          case FKeyType of
            sshKeyECDSA_P256: LCurveOID := '1.2.840.10045.3.1.7';  // prime256v1
            sshKeyECDSA_P384: LCurveOID := '1.3.132.0.34';         // secp384r1
            sshKeyECDSA_P521: LCurveOID := '1.3.132.0.35';         // secp521r1
          end;
          
          // 构建 EC 私钥 DER (SEC1 格式)
          LPrivKeyDER := BuildECPrivateKeyDER(LPrivateScalar, LPublicPoint, LCurveOID);
          
          // 构建 PKCS#8 结构
          LWriter.BeginSequence;
          LWriter.WriteInteger(0);  // version
          // AlgorithmIdentifier
          LWriter.BeginSequence;
          LWriter.WriteOID('1.2.840.10045.2.1');  // ecPublicKey
          LWriter.WriteOID(LCurveOID);
          LWriter.EndSequence;
          // privateKey
          LWriter.WriteOctetString(LPrivKeyDER);
          LWriter.EndSequence;
        end;
    else
      raise Exception.Create('Unsupported key type for PEM export');
    end;
    
    // 获取 DER 数据并转换为 PEM
    LDERData := LWriter.GetData;
    Result := LPEMWriter.WritePrivateKey(LDERData);
  finally
    LWriter.Free;
    LPEMWriter.Free;
  end;
end;

function TSSHPrivateKeyImpl.IsValid: Boolean;
begin
  // 验证私钥数据是否有效
  Result := (Length(FPublicKeyBlob) > 0) and (Length(FPrivateKeyData) > 0);
end;

function TSSHPrivateKeyImpl.MatchesPublicKey(APubKey: ISSHPublicKey): Boolean;
var
  LPubKeyData: TBytes;
begin
  Result := False;
  if APubKey = nil then
    Exit;

  // 比较公钥 Blob
  LPubKeyData := APubKey.GetKeyData;
  if Length(LPubKeyData) <> Length(FPublicKeyBlob) then
    Exit;

  Result := CompareMem(@LPubKeyData[0], @FPublicKeyBlob[0], Length(LPubKeyData));
end;

// ========================================================================
// TSSHKeyPairImpl 实现类
// ========================================================================

type
  { SSH 密钥对实现类 }
  TSSHKeyPairImpl = class(TInterfacedObject, ISSHKeyPair)
  private
    FPublicKey: ISSHPublicKey;
    FPrivateKey: ISSHPrivateKey;
  public
    constructor Create(APublicKey: ISSHPublicKey; APrivateKey: ISSHPrivateKey);

    { ISSHKeyPair 实现 }
    function GetPublicKey: ISSHPublicKey;
    function GetPrivateKey: ISSHPrivateKey;
    procedure SaveToFiles(const APrivateKeyPath, APublicKeyPath: string;
      const APassphrase: string = '');
    function IsValidPair: Boolean;
  end;

constructor TSSHKeyPairImpl.Create(APublicKey: ISSHPublicKey; APrivateKey: ISSHPrivateKey);
begin
  inherited Create;
  FPublicKey := APublicKey;
  FPrivateKey := APrivateKey;
end;

function TSSHKeyPairImpl.GetPublicKey: ISSHPublicKey;
begin
  Result := FPublicKey;
end;

function TSSHKeyPairImpl.GetPrivateKey: ISSHPrivateKey;
begin
  Result := FPrivateKey;
end;

procedure TSSHKeyPairImpl.SaveToFiles(const APrivateKeyPath, APublicKeyPath: string;
  const APassphrase: string);
var
  LStream: TFileStream;
  LContent: string;
  LBytes: TBytes;
begin
  // 保存私钥
  LContent := FPrivateKey.ToOpenSSHFormat(APassphrase);
  LBytes := TEncoding.UTF8.GetBytes(LContent);
  LStream := TFileStream.Create(APrivateKeyPath, fmCreate);
  try
    if Length(LBytes) > 0 then
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  
  // 设置私钥文件权限 (Unix: 600 = rw-------)
  {$IFDEF UNIX}
  FpChmod(APrivateKeyPath, &600);
  {$ENDIF}

  // 保存公钥
  LContent := FPublicKey.ToOpenSSHFormat;
  LBytes := TEncoding.UTF8.GetBytes(LContent);
  LStream := TFileStream.Create(APublicKeyPath, fmCreate);
  try
    if Length(LBytes) > 0 then
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  
  // 设置公钥文件权限 (Unix: 644 = rw-r--r--)
  {$IFDEF UNIX}
  FpChmod(APublicKeyPath, &644);
  {$ENDIF}
end;

function TSSHKeyPairImpl.IsValidPair: Boolean;
begin
  Result := (FPublicKey <> nil) and (FPrivateKey <> nil) and
            FPrivateKey.MatchesPublicKey(FPublicKey);
end;

// ========================================================================
// Blowfish 实现 (用于 bcrypt)
// ========================================================================

type
  TBlowfishContext = record
    P: array[0..17] of Cardinal;
    S: array[0..3, 0..255] of Cardinal;
  end;

const
  // Blowfish P-array 初始值
  BLOWFISH_P: array[0..17] of Cardinal = (
    $243F6A88, $85A308D3, $13198A2E, $03707344,
    $A4093822, $299F31D0, $082EFA98, $EC4E6C89,
    $452821E6, $38D01377, $BE5466CF, $34E90C6C,
    $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917,
    $9216D5D9, $8979FB1B
  );

  // Blowfish S-box 初始值
  {$I fafafa.ssl.ssh.blowfish_sbox.inc}

procedure BlowfishInit(var Ctx: TBlowfishContext);
var
  I, J: Integer;
begin
  // 初始化 P-array
  for I := 0 to 17 do
    Ctx.P[I] := BLOWFISH_P[I];

  // 初始化 S-boxes
  for I := 0 to 3 do
    for J := 0 to 255 do
      Ctx.S[I, J] := BLOWFISH_S[I, J];
end;

function BlowfishF(var Ctx: TBlowfishContext; X: Cardinal): Cardinal; inline;
var
  A, B, C, D: Byte;
begin
  D := X and $FF;
  X := X shr 8;
  C := X and $FF;
  X := X shr 8;
  B := X and $FF;
  A := X shr 8;

  Result := ((Ctx.S[0, A] + Ctx.S[1, B]) xor Ctx.S[2, C]) + Ctx.S[3, D];
end;

procedure BlowfishEncrypt(var Ctx: TBlowfishContext; var L, R: Cardinal);
var
  I: Integer;
  Temp: Cardinal;
begin
  for I := 0 to 15 do
  begin
    L := L xor Ctx.P[I];
    R := R xor BlowfishF(Ctx, L);
    // Swap
    Temp := L;
    L := R;
    R := Temp;
  end;
  // Undo last swap
  Temp := L;
  L := R;
  R := Temp;

  R := R xor Ctx.P[16];
  L := L xor Ctx.P[17];
end;

procedure BlowfishExpandKey(var Ctx: TBlowfishContext; const Key: TBytes);
var
  I, J, K: Integer;
  Data: Cardinal;
  L, R: Cardinal;
begin
  // XOR key into P-array
  J := 0;
  for I := 0 to 17 do
  begin
    Data := 0;
    for K := 0 to 3 do
    begin
      Data := (Data shl 8) or Key[J];
      Inc(J);
      if J >= Length(Key) then
        J := 0;
    end;
    Ctx.P[I] := Ctx.P[I] xor Data;
  end;

  // Encrypt all-zeros and replace P-array
  L := 0;
  R := 0;
  I := 0;
  while I < 18 do
  begin
    BlowfishEncrypt(Ctx, L, R);
    Ctx.P[I] := L;
    Ctx.P[I + 1] := R;
    Inc(I, 2);
  end;

  // Encrypt and replace S-boxes
  for I := 0 to 3 do
  begin
    J := 0;
    while J < 256 do
    begin
      BlowfishEncrypt(Ctx, L, R);
      Ctx.S[I, J] := L;
      Ctx.S[I, J + 1] := R;
      Inc(J, 2);
    end;
  end;
end;

procedure BlowfishExpandKeyWithSalt(var Ctx: TBlowfishContext;
  const Key, Salt: TBytes);
var
  I, J, K: Integer;
  Data: Cardinal;
  L, R: Cardinal;
  SaltIdx: Integer;
begin
  // XOR key into P-array
  J := 0;
  for I := 0 to 17 do
  begin
    Data := 0;
    for K := 0 to 3 do
    begin
      Data := (Data shl 8) or Key[J];
      Inc(J);
      if J >= Length(Key) then
        J := 0;
    end;
    Ctx.P[I] := Ctx.P[I] xor Data;
  end;

  // Encrypt with salt and replace P-array
  L := 0;
  R := 0;
  SaltIdx := 0;
  I := 0;
  while I < 18 do
  begin
    // XOR salt into L and R
    Data := 0;
    for K := 0 to 3 do
    begin
      Data := (Data shl 8) or Salt[SaltIdx];
      Inc(SaltIdx);
      if SaltIdx >= Length(Salt) then
        SaltIdx := 0;
    end;
    L := L xor Data;

    Data := 0;
    for K := 0 to 3 do
    begin
      Data := (Data shl 8) or Salt[SaltIdx];
      Inc(SaltIdx);
      if SaltIdx >= Length(Salt) then
        SaltIdx := 0;
    end;
    R := R xor Data;

    BlowfishEncrypt(Ctx, L, R);
    Ctx.P[I] := L;
    Ctx.P[I + 1] := R;
    Inc(I, 2);
  end;

  // Encrypt with salt and replace S-boxes
  for I := 0 to 3 do
  begin
    J := 0;
    while J < 256 do
    begin
      // XOR salt into L and R
      Data := 0;
      for K := 0 to 3 do
      begin
        Data := (Data shl 8) or Salt[SaltIdx];
        Inc(SaltIdx);
        if SaltIdx >= Length(Salt) then
          SaltIdx := 0;
      end;
      L := L xor Data;

      Data := 0;
      for K := 0 to 3 do
      begin
        Data := (Data shl 8) or Salt[SaltIdx];
        Inc(SaltIdx);
        if SaltIdx >= Length(Salt) then
          SaltIdx := 0;
      end;
      R := R xor Data;

      BlowfishEncrypt(Ctx, L, R);
      Ctx.S[I, J] := L;
      Ctx.S[I, J + 1] := R;
      Inc(J, 2);
    end;
  end;
end;

// ========================================================================
// bcrypt 哈希函数 (用于 bcrypt_pbkdf)
// ========================================================================

{ bcrypt 哈希函数 - 返回 24 字节哈希 }
function BcryptHash(const Password, Salt: TBytes): TBytes;
var
  Ctx: TBlowfishContext;
  I, J: Integer;
  Ctext: array[0..5] of Cardinal;
  L, R: Cardinal;
  ShaPass, ShaSalt: TBytes;
begin
  // 使用 SHA-512 预处理密码和盐
  ShaPass := fafafa.ssl.crypto.hash.SHA512(Password);
  ShaSalt := fafafa.ssl.crypto.hash.SHA512(Salt);

  // 初始化 Blowfish
  BlowfishInit(Ctx);

  // 使用 SHA-512 处理后的密码和盐扩展密钥
  BlowfishExpandKeyWithSalt(Ctx, ShaPass, ShaSalt);

  // 64 轮密钥扩展
  for I := 0 to 63 do
  begin
    BlowfishExpandKey(Ctx, ShaSalt);
    BlowfishExpandKey(Ctx, ShaPass);
  end;

  // 初始化密文 "OrpheanBeholderScryDoubt"
  Ctext[0] := $4F727068;  // "Orph"
  Ctext[1] := $65616E42;  // "eanB"
  Ctext[2] := $65686F6C;  // "ehol"
  Ctext[3] := $64657253;  // "derS"
  Ctext[4] := $63727944;  // "cryD"
  Ctext[5] := $6F756274;  // "oubt"

  // 64 轮加密
  for I := 0 to 63 do
  begin
    J := 0;
    while J < 6 do
    begin
      L := Ctext[J];
      R := Ctext[J + 1];
      BlowfishEncrypt(Ctx, L, R);
      Ctext[J] := L;
      Ctext[J + 1] := R;
      Inc(J, 2);
    end;
  end;

  // 输出 24 字节 (大端序)
  SetLength(Result, 24);
  for I := 0 to 5 do
  begin
    Result[I * 4] := (Ctext[I] shr 24) and $FF;
    Result[I * 4 + 1] := (Ctext[I] shr 16) and $FF;
    Result[I * 4 + 2] := (Ctext[I] shr 8) and $FF;
    Result[I * 4 + 3] := Ctext[I] and $FF;
  end;
end;

// ========================================================================
// bcrypt_pbkdf 密钥派生函数
// ========================================================================

{ bcrypt_pbkdf - OpenSSH 使用的密钥派生函数 }
function BcryptPBKDF(const Password, Salt: TBytes; Rounds, KeyLen: Integer): TBytes;
var
  I, J, K, Count, Amt, Stride: Integer;
  CountBuf: array[0..3] of Byte;
  Sha2Pass, Sha2Salt, Out_, TmpOut: TBytes;
  KeyBuf: TBytes;
begin
  if (Rounds < 1) or (KeyLen < 1) or (Length(Password) = 0) or (Length(Salt) = 0) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Stride := (KeyLen + 32 - 1) div 32;
  Amt := (KeyLen + Stride - 1) div Stride;

  SetLength(KeyBuf, Stride * 32);
  FillChar(KeyBuf[0], Length(KeyBuf), 0);

  // SHA-512 预处理密码
  Sha2Pass := fafafa.ssl.crypto.hash.SHA512(Password);

  Count := 1;
  while KeyLen > 0 do
  begin
    // 构建计数器
    CountBuf[0] := (Count shr 24) and $FF;
    CountBuf[1] := (Count shr 16) and $FF;
    CountBuf[2] := (Count shr 8) and $FF;
    CountBuf[3] := Count and $FF;

    // SHA-512(salt || count)
    SetLength(Sha2Salt, Length(Salt) + 4);
    Move(Salt[0], Sha2Salt[0], Length(Salt));
    Move(CountBuf[0], Sha2Salt[Length(Salt)], 4);
    Sha2Salt := fafafa.ssl.crypto.hash.SHA512(Sha2Salt);

    // 第一轮 bcrypt
    Out_ := BcryptHash(Sha2Pass, Sha2Salt);
    TmpOut := Copy(Out_);

    // 后续轮次
    for I := 2 to Rounds do
    begin
      // SHA-512(previous output)
      Sha2Salt := fafafa.ssl.crypto.hash.SHA512(TmpOut);
      TmpOut := BcryptHash(Sha2Pass, Sha2Salt);

      // XOR 到输出
      for J := 0 to High(Out_) do
        Out_[J] := Out_[J] xor TmpOut[J];
    end;

    // 将输出交织到密钥缓冲区
    Amt := 24;
    if Amt > KeyLen then
      Amt := KeyLen;

    for I := 0 to Amt - 1 do
    begin
      K := I * Stride + (Count - 1);
      if K < Length(KeyBuf) then
        KeyBuf[K] := Out_[I];
    end;

    Dec(KeyLen, Amt);
    Inc(Count);
  end;

  Result := KeyBuf;
end;

// ========================================================================
// AES-256-CTR 解密函数
// ========================================================================

{ 使用 AES-256-CTR 解密数据 }
function DecryptAES256CTR(const Data, Key, IV: TBytes): TBytes;
begin
  // 使用现有的 AES CTR 解密函数
  Result := AESDecryptCTR(Data, Key, IV);
end;

// ========================================================================
// OpenSSH 私钥解析函数
// ========================================================================

{ 解析 OpenSSH 私钥格式 (openssh-key-v1) }
function ParseOpenSSHPrivateKey(const AData: TBytes; const APassphrase: string;
  out AKeyType: TSSHKeyType; out APublicKeyBlob, APrivateKeyData: TBytes;
  out AComment: string; out AIsEncrypted: Boolean): Boolean;
var
  LOffset: Integer;
  LMagic: string;
  LCipherName, LKDFName: string;
  LKDFOptions: TBytes;
  LNumKeys: Cardinal;
  LPublicKeyLen: Cardinal;
  LPrivateSectionLen: Cardinal;
  LPrivateSection: TBytes;
  LCheckInt1, LCheckInt2: Cardinal;
  LKeyTypeStr: string;
  I: Integer;
  // KDF 相关变量
  LKDFOffset: Integer;
  LSalt: TBytes;
  LRounds: Cardinal;
  LDerivedKey: TBytes;
  LKey, LIV: TBytes;
  LPassBytes: TBytes;
begin
  Result := False;
  AIsEncrypted := False;

  if Length(AData) < 15 then
    Exit;

  LOffset := 0;

  // 检查魔数 "openssh-key-v1\0"
  SetLength(LMagic, 14);
  for I := 1 to 14 do
    LMagic[I] := Char(AData[LOffset + I - 1]);
  Inc(LOffset, 14);

  if LMagic <> OPENSSH_AUTH_MAGIC then
    Exit;

  // 跳过 null 终止符
  if AData[LOffset] <> 0 then
    Exit;
  Inc(LOffset);

  try
    // 读取加密参数
    LCipherName := ReadString(AData, LOffset);
    LKDFName := ReadString(AData, LOffset);
    LKDFOptions := ReadBytes(AData, LOffset);

    // 检查是否加密
    AIsEncrypted := (LCipherName <> OPENSSH_CIPHER_NONE) and
                    (LKDFName <> OPENSSH_KDF_NONE);

    // 如果加密但没有密码，返回错误
    if AIsEncrypted and (APassphrase = '') then
    begin
      // 返回部分信息，让调用者知道需要密码
      Exit;
    end;

    // 读取密钥数量
    LNumKeys := ReadUInt32BE(AData, LOffset);
    if LNumKeys <> 1 then
      Exit;  // 目前只支持单个密钥

    // 读取公钥
    LPublicKeyLen := ReadUInt32BE(AData, LOffset);
    if LOffset + Integer(LPublicKeyLen) > Length(AData) then
      Exit;
    SetLength(APublicKeyBlob, LPublicKeyLen);
    Move(AData[LOffset], APublicKeyBlob[0], LPublicKeyLen);
    Inc(LOffset, LPublicKeyLen);

    // 从公钥 Blob 中提取密钥类型
    I := 0;
    LKeyTypeStr := ReadString(APublicKeyBlob, I);
    AKeyType := StringToSSHKeyType(LKeyTypeStr);
    if AKeyType = sshKeyUnknown then
      Exit;

    // 读取私钥部分
    LPrivateSectionLen := ReadUInt32BE(AData, LOffset);
    if LOffset + Integer(LPrivateSectionLen) > Length(AData) then
      Exit;
    SetLength(LPrivateSection, LPrivateSectionLen);
    Move(AData[LOffset], LPrivateSection[0], LPrivateSectionLen);

    // 如果加密，需要解密
    if AIsEncrypted then
    begin
      // 解析 KDF 选项
      if LKDFName = OPENSSH_KDF_BCRYPT then
      begin
        LKDFOffset := 0;

        // 读取 salt 和 rounds
        LSalt := ReadBytes(LKDFOptions, LKDFOffset);
        LRounds := ReadUInt32BE(LKDFOptions, LKDFOffset);

        if Length(LSalt) = 0 then
          Exit;

        // 使用 bcrypt_pbkdf 派生密钥
        // AES-256-CTR 需要 32 字节密钥 + 16 字节 IV = 48 字节
        SetLength(LPassBytes, Length(APassphrase));
        if Length(APassphrase) > 0 then
          Move(APassphrase[1], LPassBytes[0], Length(APassphrase));

        LDerivedKey := BcryptPBKDF(LPassBytes, LSalt, LRounds, 48);
        if Length(LDerivedKey) < 48 then
          Exit;

        // 分离密钥和 IV
        SetLength(LKey, 32);
        SetLength(LIV, 16);
        Move(LDerivedKey[0], LKey[0], 32);
        Move(LDerivedKey[32], LIV[0], 16);

        // 解密私钥部分
        if LCipherName = OPENSSH_CIPHER_AES256_CTR then
          LPrivateSection := DecryptAES256CTR(LPrivateSection, LKey, LIV)
        else if LCipherName = OPENSSH_CIPHER_AES256_CBC then
          LPrivateSection := AESDecryptCBC(LPrivateSection, LKey, LIV)
        else
          raise Exception.CreateFmt('Unsupported cipher: %s', [LCipherName]);
      end
      else
        raise Exception.CreateFmt('Unsupported KDF: %s', [LKDFName]);
    end;

    // 解析私钥部分
    LOffset := 0;

    // 读取并验证 check integers
    LCheckInt1 := ReadUInt32BE(LPrivateSection, LOffset);
    LCheckInt2 := ReadUInt32BE(LPrivateSection, LOffset);
    if LCheckInt1 <> LCheckInt2 then
      Exit;  // 校验失败（可能是密码错误）

    // 跳过密钥类型字符串（已经从公钥中获取）
    ReadString(LPrivateSection, LOffset);

    // 根据密钥类型读取私钥数据
    case AKeyType of
      sshKeyRSA:
        begin
          // RSA 私钥: n, e, d, iqmp, p, q
          ReadBytes(LPrivateSection, LOffset);  // n
          ReadBytes(LPrivateSection, LOffset);  // e
          ReadBytes(LPrivateSection, LOffset);  // d
          ReadBytes(LPrivateSection, LOffset);  // iqmp
          ReadBytes(LPrivateSection, LOffset);  // p
          ReadBytes(LPrivateSection, LOffset);  // q
        end;
      sshKeyEd25519:
        begin
          // Ed25519: 公钥 (32 bytes) + 私钥 (64 bytes)
          ReadBytes(LPrivateSection, LOffset);  // 公钥
          ReadBytes(LPrivateSection, LOffset);  // 私钥
        end;
      sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
        begin
          // ECDSA: curve, public point, private scalar
          ReadString(LPrivateSection, LOffset);  // curve
          ReadBytes(LPrivateSection, LOffset);   // public point
          ReadBytes(LPrivateSection, LOffset);   // private scalar
        end;
    end;

    // 读取注释
    AComment := ReadString(LPrivateSection, LOffset);

    // 保存私钥数据（从 check integers 之后开始）
    APrivateKeyData := Copy(LPrivateSection, 8, Length(LPrivateSection) - 8);

    Result := True;
  except
    Result := False;
  end;
end;

// ========================================================================
// PEM 私钥解析函数
// ========================================================================

{ 从 ASN.1 大整数中移除前导零字节 }
function TrimLeadingZeros(const AData: TBytes): TBytes;
var
  I, StartIdx: Integer;
begin
  StartIdx := 0;
  // 跳过前导零字节（但保留至少一个字节）
  while (StartIdx < Length(AData) - 1) and (AData[StartIdx] = 0) do
    Inc(StartIdx);
  
  SetLength(Result, Length(AData) - StartIdx);
  if Length(Result) > 0 then
    Move(AData[StartIdx], Result[0], Length(Result));
end;

{ 解析 PEM 格式公钥 DER 数据 }
function ParsePEMPublicKeyDER(const ADERData: TBytes; ABlockType: TPEMType): ISSHPublicKey;
var
  LReader: TASN1Reader;
  LRoot, LAlgId, LPubKeyNode: TASN1Node;
  LOID: string;
  LKeyType: TSSHKeyType;
  LKeyData, LPubKeyBits: TBytes;
  LModulus, LExponent: TBytes;
  LCurveOID: string;
  LInnerReader: TASN1Reader;
  LInnerRoot: TASN1Node;
begin
  Result := nil;
  
  LReader := TASN1Reader.Create(ADERData);
  try
    LRoot := LReader.Parse;
    if LRoot = nil then
      raise Exception.Create('Failed to parse public key DER data');
    
    try
      if not LRoot.IsSequence then
        raise Exception.Create('Invalid public key structure');
      
      // SubjectPublicKeyInfo ::= SEQUENCE {
      //   algorithm AlgorithmIdentifier,
      //   subjectPublicKey BIT STRING
      // }
      
      if LRoot.ChildCount < 2 then
        raise Exception.Create('Invalid SubjectPublicKeyInfo structure');
      
      // 解析 AlgorithmIdentifier
      LAlgId := LRoot.GetChild(0);
      if not LAlgId.IsSequence then
        raise Exception.Create('Invalid AlgorithmIdentifier');
      
      if LAlgId.ChildCount < 1 then
        raise Exception.Create('Missing algorithm OID');
      
      LOID := LAlgId.GetChild(0).AsOID;
      
      // 获取公钥位串
      LPubKeyNode := LRoot.GetChild(1);
      if not LPubKeyNode.IsBitString then
        raise Exception.Create('Invalid public key bit string');
      
      LPubKeyBits := LPubKeyNode.AsBitString;
      
      // 根据算法 OID 确定密钥类型
      if LOID = '1.2.840.113549.1.1.1' then  // rsaEncryption
      begin
        LKeyType := sshKeyRSA;
        
        // RSA 公钥是 DER 编码的 SEQUENCE { n INTEGER, e INTEGER }
        LInnerReader := TASN1Reader.Create(LPubKeyBits);
        try
          LInnerRoot := LInnerReader.Parse;
          if (LInnerRoot = nil) or (not LInnerRoot.IsSequence) or (LInnerRoot.ChildCount < 2) then
            raise Exception.Create('Invalid RSA public key structure');
          
          LModulus := TrimLeadingZeros(LInnerRoot.GetChild(0).AsBigInteger);
          LExponent := TrimLeadingZeros(LInnerRoot.GetChild(1).AsBigInteger);
          
          // 构建 SSH 公钥 Blob
          LKeyData := BuildRSAPublicKeyBlob(LExponent, LModulus);
        finally
          LInnerReader.Free;
          if LInnerRoot <> nil then
            LInnerRoot.Free;
        end;
      end
      else if LOID = '1.3.101.112' then  // id-Ed25519
      begin
        LKeyType := sshKeyEd25519;
        
        // Ed25519 公钥直接是 32 字节
        if Length(LPubKeyBits) <> 32 then
          raise Exception.Create('Invalid Ed25519 public key length');
        
        LKeyData := BuildEd25519PublicKeyBlob(LPubKeyBits);
      end
      else if LOID = '1.2.840.10045.2.1' then  // ecPublicKey
      begin
        // 获取曲线 OID
        if LAlgId.ChildCount < 2 then
          raise Exception.Create('Missing EC curve parameter');
        
        LCurveOID := LAlgId.GetChild(1).AsOID;
        
        if LCurveOID = '1.2.840.10045.3.1.7' then  // prime256v1
          LKeyType := sshKeyECDSA_P256
        else if LCurveOID = '1.3.132.0.34' then  // secp384r1
          LKeyType := sshKeyECDSA_P384
        else if LCurveOID = '1.3.132.0.35' then  // secp521r1
          LKeyType := sshKeyECDSA_P521
        else
          raise Exception.CreateFmt('Unsupported EC curve: %s', [LCurveOID]);
        
        // ECDSA 公钥是未压缩点
        LKeyData := BuildECDSAPublicKeyBlob(LKeyType, LPubKeyBits);
      end
      else
        raise Exception.CreateFmt('Unsupported algorithm OID: %s', [LOID]);
      
      Result := TSSHPublicKeyImpl.Create(LKeyType, LKeyData, '');
    finally
      LRoot.Free;
    end;
  finally
    LReader.Free;
  end;
end;

{ 解析 PKCS#1 RSA 私钥 (RSA PRIVATE KEY) }
function ParsePKCS1RSAPrivateKey(const ADERData: TBytes;
  out APublicKeyBlob, APrivateKeyData: TBytes; out AComment: string): Boolean;
var
  LReader: TASN1Reader;
  LRoot: TASN1Node;
  LVersion: Int64;
  LModulus, LExponent, LPrivateExp: TBytes;
  LP, LQ, LDP, LDQ, LIQMP: TBytes;
begin
  Result := False;
  AComment := '';
  SetLength(APublicKeyBlob, 0);
  SetLength(APrivateKeyData, 0);
  
  LReader := TASN1Reader.Create(ADERData);
  try
    try
      LRoot := LReader.Parse;
      if LRoot = nil then
        Exit;
      
      try
        // PKCS#1 RSA 私钥结构:
        // RSAPrivateKey ::= SEQUENCE {
        //   version           Version,
        //   modulus           INTEGER,  -- n
        //   publicExponent    INTEGER,  -- e
        //   privateExponent   INTEGER,  -- d
        //   prime1            INTEGER,  -- p
        //   prime2            INTEGER,  -- q
        //   exponent1         INTEGER,  -- d mod (p-1)
        //   exponent2         INTEGER,  -- d mod (q-1)
        //   coefficient       INTEGER,  -- (inverse of q) mod p
        // }
        
        if not LRoot.IsSequence then
          Exit;
        
        if LRoot.ChildCount < 9 then
          Exit;
        
        // 版本
        if not LRoot.GetChild(0).IsInteger then
          Exit;
        LVersion := LRoot.GetChild(0).AsInteger;
        if LVersion <> 0 then
          Exit;  // 只支持版本 0
        
        // 提取参数
        LModulus := TrimLeadingZeros(LRoot.GetChild(1).AsBigInteger);
        LExponent := TrimLeadingZeros(LRoot.GetChild(2).AsBigInteger);
        LPrivateExp := TrimLeadingZeros(LRoot.GetChild(3).AsBigInteger);
        LP := TrimLeadingZeros(LRoot.GetChild(4).AsBigInteger);
        LQ := TrimLeadingZeros(LRoot.GetChild(5).AsBigInteger);
        LDP := TrimLeadingZeros(LRoot.GetChild(6).AsBigInteger);
        LDQ := TrimLeadingZeros(LRoot.GetChild(7).AsBigInteger);
        LIQMP := TrimLeadingZeros(LRoot.GetChild(8).AsBigInteger);
        
        // 构建 SSH 公钥 Blob
        APublicKeyBlob := BuildRSAPublicKeyBlob(LExponent, LModulus);
        
        // 构建私钥数据（SSH 格式）
        SetLength(APrivateKeyData, 0);
        WriteBytesWithLength(APrivateKeyData, LModulus);
        WriteBytesWithLength(APrivateKeyData, LExponent);
        WriteBytesWithLength(APrivateKeyData, LPrivateExp);
        WriteBytesWithLength(APrivateKeyData, LIQMP);
        WriteBytesWithLength(APrivateKeyData, LP);
        WriteBytesWithLength(APrivateKeyData, LQ);
        
        Result := True;
      finally
        LRoot.Free;
      end;
    except
      Result := False;
    end;
  finally
    LReader.Free;
  end;
end;

{ 解析 PKCS#8 私钥 (PRIVATE KEY) }
function ParsePKCS8PrivateKey(const ADERData: TBytes;
  out AKeyType: TSSHKeyType; out APublicKeyBlob, APrivateKeyData: TBytes;
  out AComment: string): Boolean;
var
  LReader: TASN1Reader;
  LRoot, LAlgoSeq, LPrivKeyOctet: TASN1Node;
  LVersion: Int64;
  LOID: string;
  LInnerData: TBytes;
  LInnerReader: TASN1Reader;
  LInnerRoot: TASN1Node;
  LCurveOID: string;
  LPrivateKey, LPublicKey: TBytes;
  I: Integer;
  LAttr: TASN1Node;
  LFullPrivKey: TBytes;
begin
  Result := False;
  AKeyType := sshKeyUnknown;
  AComment := '';
  SetLength(APublicKeyBlob, 0);
  SetLength(APrivateKeyData, 0);
  
  LReader := TASN1Reader.Create(ADERData);
  try
    try
      LRoot := LReader.Parse;
      if LRoot = nil then
        Exit;
      
      try
        // PKCS#8 私钥结构:
        // PrivateKeyInfo ::= SEQUENCE {
        //   version                   Version,
        //   privateKeyAlgorithm       AlgorithmIdentifier,
        //   privateKey                OCTET STRING,
        //   attributes           [0]  IMPLICIT Attributes OPTIONAL
        // }
        
        if not LRoot.IsSequence then
          Exit;
        
        if LRoot.ChildCount < 3 then
          Exit;
        
        // 版本
        if not LRoot.GetChild(0).IsInteger then
          Exit;
        LVersion := LRoot.GetChild(0).AsInteger;
        if LVersion <> 0 then
          Exit;
        
        // 算法标识符
        LAlgoSeq := LRoot.GetChild(1);
        if not LAlgoSeq.IsSequence then
          Exit;
        if LAlgoSeq.ChildCount < 1 then
          Exit;
        if not LAlgoSeq.GetChild(0).IsOID then
          Exit;
        
        LOID := LAlgoSeq.GetChild(0).AsOID;
        
        // 私钥数据
        LPrivKeyOctet := LRoot.GetChild(2);
        if not LPrivKeyOctet.IsOctetString then
          Exit;
        LInnerData := LPrivKeyOctet.AsOctetString;
        
        // 根据 OID 确定密钥类型
        if LOID = '1.2.840.113549.1.1.1' then
        begin
          // RSA
          AKeyType := sshKeyRSA;
          Result := ParsePKCS1RSAPrivateKey(LInnerData, APublicKeyBlob, APrivateKeyData, AComment);
        end
        else if LOID = '1.2.840.10045.2.1' then
        begin
          // EC (ECDSA)
          // 获取曲线 OID
          if LAlgoSeq.ChildCount < 2 then
            Exit;
          if not LAlgoSeq.GetChild(1).IsOID then
            Exit;
          LCurveOID := LAlgoSeq.GetChild(1).AsOID;
          
          // 确定曲线类型
          if LCurveOID = '1.2.840.10045.3.1.7' then
            AKeyType := sshKeyECDSA_P256
          else if LCurveOID = '1.3.132.0.34' then
            AKeyType := sshKeyECDSA_P384
          else if LCurveOID = '1.3.132.0.35' then
            AKeyType := sshKeyECDSA_P521
          else
            Exit;  // 不支持的曲线
          
          // 解析 EC 私钥
          LInnerReader := TASN1Reader.Create(LInnerData);
          try
            LInnerRoot := LInnerReader.Parse;
            if LInnerRoot = nil then
              Exit;
            
            try
              // ECPrivateKey ::= SEQUENCE {
              //   version        INTEGER { ecPrivkeyVer1(1) },
              //   privateKey     OCTET STRING,
              //   parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
              //   publicKey  [1] BIT STRING OPTIONAL
              // }
              
              if not LInnerRoot.IsSequence then
                Exit;
              if LInnerRoot.ChildCount < 2 then
                Exit;
              
              // 私钥
              if not LInnerRoot.GetChild(1).IsOctetString then
                Exit;
              LPrivateKey := LInnerRoot.GetChild(1).AsOctetString;
              
              // 查找公钥 [1]
              SetLength(LPublicKey, 0);
              for I := 2 to LInnerRoot.ChildCount - 1 do
              begin
                if LInnerRoot.GetChild(I).IsContextTag(1) then
                begin
                  if LInnerRoot.GetChild(I).ChildCount > 0 then
                  begin
                    if LInnerRoot.GetChild(I).GetChild(0).IsBitString then
                      LPublicKey := LInnerRoot.GetChild(I).GetChild(0).AsBitString;
                  end;
                  Break;
                end;
              end;
              
              if Length(LPublicKey) = 0 then
                Exit;  // 需要公钥
              
              // 构建 SSH 公钥 Blob
              APublicKeyBlob := BuildECDSAPublicKeyBlob(AKeyType, LPublicKey);
              
              // 构建私钥数据
              SetLength(APrivateKeyData, 0);
              case AKeyType of
                sshKeyECDSA_P256: WriteString(APrivateKeyData, 'nistp256');
                sshKeyECDSA_P384: WriteString(APrivateKeyData, 'nistp384');
                sshKeyECDSA_P521: WriteString(APrivateKeyData, 'nistp521');
              end;
              WriteBytesWithLength(APrivateKeyData, LPublicKey);
              WriteBytesWithLength(APrivateKeyData, LPrivateKey);
              
              Result := True;
            finally
              LInnerRoot.Free;
            end;
          finally
            LInnerReader.Free;
          end;
        end
        else if LOID = '1.3.101.112' then
        begin
          // Ed25519
          AKeyType := sshKeyEd25519;
          
          // Ed25519 私钥在 PKCS#8 中是一个 OCTET STRING 包装的 32 字节私钥
          // 但实际上它可能是一个 OCTET STRING 包含另一个 OCTET STRING
          LInnerReader := TASN1Reader.Create(LInnerData);
          try
            LInnerRoot := LInnerReader.Parse;
            if LInnerRoot = nil then
            begin
              // 可能是原始的 32 字节私钥
              if Length(LInnerData) = 32 then
              begin
                LPrivateKey := LInnerData;
              end
              else
                Exit;
            end
            else
            begin
              try
                if LInnerRoot.IsOctetString then
                  LPrivateKey := LInnerRoot.AsOctetString
                else
                  Exit;
              finally
                LInnerRoot.Free;
              end;
            end;
          finally
            LInnerReader.Free;
          end;
          
          if Length(LPrivateKey) <> 32 then
            Exit;
          
          // 从私钥派生公钥（Ed25519 公钥是私钥的后 32 字节或需要计算）
          // 注意：这里我们需要公钥，但 PKCS#8 可能不包含它
          // 检查是否有 attributes 包含公钥
          SetLength(LPublicKey, 0);
          if LRoot.ChildCount > 3 then
          begin
            LAttr := LRoot.GetChild(3);
            if LAttr.IsContextTag(1) then
            begin
              if LAttr.ChildCount > 0 then
              begin
                if LAttr.GetChild(0).IsBitString then
                  LPublicKey := LAttr.GetChild(0).AsBitString;
              end;
            end;
          end;
          
          if Length(LPublicKey) <> 32 then
          begin
            // 无法获取公钥，Ed25519 需要计算公钥
            // 这需要 Ed25519 实现，暂时不支持
            Exit;
          end;
          
          // 构建 SSH 公钥 Blob
          APublicKeyBlob := BuildEd25519PublicKeyBlob(LPublicKey);
          
          // 构建私钥数据
          SetLength(APrivateKeyData, 0);
          WriteBytesWithLength(APrivateKeyData, LPublicKey);
          // Ed25519 私钥是 64 字节（32 字节私钥 + 32 字节公钥）
          SetLength(LFullPrivKey, 64);
          Move(LPrivateKey[0], LFullPrivKey[0], 32);
          Move(LPublicKey[0], LFullPrivKey[32], 32);
          WriteBytesWithLength(APrivateKeyData, LFullPrivKey);
          
          Result := True;
        end
        else
          Exit;  // 不支持的算法
        
      finally
        LRoot.Free;
      end;
    except
      Result := False;
    end;
  finally
    LReader.Free;
  end;
end;

{ 解析 EC 私钥 (EC PRIVATE KEY) }
function ParseECPrivateKey(const ADERData: TBytes;
  out AKeyType: TSSHKeyType; out APublicKeyBlob, APrivateKeyData: TBytes;
  out AComment: string): Boolean;
var
  LReader: TASN1Reader;
  LRoot: TASN1Node;
  LVersion: Int64;
  LPrivateKey, LPublicKey: TBytes;
  LCurveOID: string;
  I: Integer;
begin
  Result := False;
  AKeyType := sshKeyUnknown;
  AComment := '';
  SetLength(APublicKeyBlob, 0);
  SetLength(APrivateKeyData, 0);
  
  LReader := TASN1Reader.Create(ADERData);
  try
    try
      LRoot := LReader.Parse;
      if LRoot = nil then
        Exit;
      
      try
        // ECPrivateKey ::= SEQUENCE {
        //   version        INTEGER { ecPrivkeyVer1(1) },
        //   privateKey     OCTET STRING,
        //   parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
        //   publicKey  [1] BIT STRING OPTIONAL
        // }
        
        if not LRoot.IsSequence then
          Exit;
        
        if LRoot.ChildCount < 2 then
          Exit;
        
        // 版本
        if not LRoot.GetChild(0).IsInteger then
          Exit;
        LVersion := LRoot.GetChild(0).AsInteger;
        if LVersion <> 1 then
          Exit;
        
        // 私钥
        if not LRoot.GetChild(1).IsOctetString then
          Exit;
        LPrivateKey := LRoot.GetChild(1).AsOctetString;
        
        // 查找曲线参数 [0] 和公钥 [1]
        LCurveOID := '';
        SetLength(LPublicKey, 0);
        
        for I := 2 to LRoot.ChildCount - 1 do
        begin
          if LRoot.GetChild(I).IsContextTag(0) then
          begin
            // 曲线参数
            if LRoot.GetChild(I).ChildCount > 0 then
            begin
              if LRoot.GetChild(I).GetChild(0).IsOID then
                LCurveOID := LRoot.GetChild(I).GetChild(0).AsOID;
            end;
          end
          else if LRoot.GetChild(I).IsContextTag(1) then
          begin
            // 公钥
            if LRoot.GetChild(I).ChildCount > 0 then
            begin
              if LRoot.GetChild(I).GetChild(0).IsBitString then
                LPublicKey := LRoot.GetChild(I).GetChild(0).AsBitString;
            end;
          end;
        end;
        
        // 确定曲线类型
        if LCurveOID = '1.2.840.10045.3.1.7' then
          AKeyType := sshKeyECDSA_P256
        else if LCurveOID = '1.3.132.0.34' then
          AKeyType := sshKeyECDSA_P384
        else if LCurveOID = '1.3.132.0.35' then
          AKeyType := sshKeyECDSA_P521
        else
        begin
          // 尝试根据私钥长度推断曲线
          case Length(LPrivateKey) of
            32: AKeyType := sshKeyECDSA_P256;
            48: AKeyType := sshKeyECDSA_P384;
            66: AKeyType := sshKeyECDSA_P521;
          else
            Exit;
          end;
        end;
        
        if Length(LPublicKey) = 0 then
          Exit;  // 需要公钥
        
        // 构建 SSH 公钥 Blob
        APublicKeyBlob := BuildECDSAPublicKeyBlob(AKeyType, LPublicKey);
        
        // 构建私钥数据
        SetLength(APrivateKeyData, 0);
        case AKeyType of
          sshKeyECDSA_P256: WriteString(APrivateKeyData, 'nistp256');
          sshKeyECDSA_P384: WriteString(APrivateKeyData, 'nistp384');
          sshKeyECDSA_P521: WriteString(APrivateKeyData, 'nistp521');
        end;
        WriteBytesWithLength(APrivateKeyData, LPublicKey);
        WriteBytesWithLength(APrivateKeyData, LPrivateKey);
        
        Result := True;
      finally
        LRoot.Free;
      end;
    except
      Result := False;
    end;
  finally
    LReader.Free;
  end;
end;

{ 解析 PEM 格式私钥 }
function ParsePEMPrivateKey(const APEMData: string;
  const APassphrase: string): ISSHPrivateKey;
var
  LPEMReader: TPEMReader;
  LBlocks: TPEMBlockArray;
  LBlock: TPEMBlock;
  LKeyType: TSSHKeyType;
  LPublicKeyBlob, LPrivateKeyData: TBytes;
  LComment: string;
  LSuccess: Boolean;
begin
  Result := nil;
  LSuccess := False;
  LKeyType := sshKeyUnknown;
  LComment := '';
  SetLength(LPublicKeyBlob, 0);
  SetLength(LPrivateKeyData, 0);
  
  LPEMReader := TPEMReader.Create;
  try
    LPEMReader.LoadFromString(APEMData);
    LBlocks := LPEMReader.GetPrivateKeys;
    
    if Length(LBlocks) = 0 then
      raise Exception.Create('No private key found in PEM data');
    
    LBlock := LBlocks[0];
    
    // 检查是否加密
    if LBlock.IsEncrypted then
    begin
      if APassphrase = '' then
        raise Exception.Create('Private key is encrypted, passphrase required');
      // 加密的 PEM 私钥解密暂不支持
      raise Exception.Create('Encrypted PEM private key decryption not yet supported');
    end;
    
    // 根据 PEM 类型解析
    case LBlock.BlockType of
      pemRSAPrivateKey:
        begin
          // PKCS#1 RSA 私钥
          LKeyType := sshKeyRSA;
          LSuccess := ParsePKCS1RSAPrivateKey(LBlock.Data, LPublicKeyBlob, LPrivateKeyData, LComment);
        end;
      pemECPrivateKey:
        begin
          // EC 私钥
          LSuccess := ParseECPrivateKey(LBlock.Data, LKeyType, LPublicKeyBlob, LPrivateKeyData, LComment);
        end;
      pemPrivateKey:
        begin
          // PKCS#8 私钥
          LSuccess := ParsePKCS8PrivateKey(LBlock.Data, LKeyType, LPublicKeyBlob, LPrivateKeyData, LComment);
        end;
      pemEncryptedPrivateKey:
        begin
          if APassphrase = '' then
            raise Exception.Create('Private key is encrypted, passphrase required');
          raise Exception.Create('Encrypted PKCS#8 private key decryption not yet supported');
        end;
    else
      raise Exception.Create('Unsupported PEM private key type');
    end;
    
    if not LSuccess then
      raise Exception.Create('Failed to parse PEM private key');
    
    Result := TSSHPrivateKeyImpl.Create(LKeyType, LPublicKeyBlob, LPrivateKeyData, LComment, False);
  finally
    LPEMReader.Free;
  end;
end;

// ========================================================================
// TSSHKeyResult 实现
// ========================================================================

class function TSSHKeyResult.Ok: TSSHKeyResult;
begin
  Result.Success := True;
  Result.ErrorCode := sshKeyErrNone;
  Result.ErrorMessage := '';
end;

class function TSSHKeyResult.Err(ACode: TSSHKeyError; const AMsg: string): TSSHKeyResult;
begin
  Result.Success := False;
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSHKeyResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSHKeyResult.IsErr: Boolean;
begin
  Result := not Success;
end;

// ========================================================================
// TSSHPublicKeyResult 实现
// ========================================================================

class function TSSHPublicKeyResult.Ok(AKey: ISSHPublicKey): TSSHPublicKeyResult;
begin
  Result.Success := True;
  Result.Key := AKey;
  Result.ErrorCode := sshKeyErrNone;
  Result.ErrorMessage := '';
end;

class function TSSHPublicKeyResult.Err(ACode: TSSHKeyError; const AMsg: string): TSSHPublicKeyResult;
begin
  Result.Success := False;
  Result.Key := nil;
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSHPublicKeyResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSHPublicKeyResult.IsErr: Boolean;
begin
  Result := not Success;
end;

// ========================================================================
// TSSHPrivateKeyResult 实现
// ========================================================================

class function TSSHPrivateKeyResult.Ok(AKey: ISSHPrivateKey): TSSHPrivateKeyResult;
begin
  Result.Success := True;
  Result.Key := AKey;
  Result.ErrorCode := sshKeyErrNone;
  Result.ErrorMessage := '';
end;

class function TSSHPrivateKeyResult.Err(ACode: TSSHKeyError; const AMsg: string): TSSHPrivateKeyResult;
begin
  Result.Success := False;
  Result.Key := nil;
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSHPrivateKeyResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSHPrivateKeyResult.IsErr: Boolean;
begin
  Result := not Success;
end;

// ========================================================================
// TSSHKeyPairResult 实现
// ========================================================================

class function TSSHKeyPairResult.Ok(AKeyPair: ISSHKeyPair): TSSHKeyPairResult;
begin
  Result.Success := True;
  Result.KeyPair := AKeyPair;
  Result.ErrorCode := sshKeyErrNone;
  Result.ErrorMessage := '';
end;

class function TSSHKeyPairResult.Err(ACode: TSSHKeyError; const AMsg: string): TSSHKeyPairResult;
begin
  Result.Success := False;
  Result.KeyPair := nil;
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSHKeyPairResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSHKeyPairResult.IsErr: Boolean;
begin
  Result := not Success;
end;

// ========================================================================
// TSSHAuthorizedKeyEntry 实现
// ========================================================================

function TSSHAuthorizedKeyEntry.ToString: string;
begin
  if not IsValid then
  begin
    Result := '';
    Exit;
  end;

  if Options <> '' then
    Result := Options + ' ' + Key.ToOpenSSHFormat
  else
    Result := Key.ToOpenSSHFormat;
end;

class function TSSHAuthorizedKeyEntry.Parse(const ALine: string): TSSHAuthorizedKeyEntry;
begin
  if not TryParse(ALine, Result) then
  begin
    Result.IsValid := False;
    Result.Options := '';
    Result.Key := nil;
  end;
end;

class function TSSHAuthorizedKeyEntry.TryParse(const ALine: string;
  out AEntry: TSSHAuthorizedKeyEntry): Boolean;
begin
  // TODO: 实现解析逻辑
  AEntry.IsValid := False;
  AEntry.Options := '';
  AEntry.Key := nil;
  Result := False;
end;

// ========================================================================
// 辅助函数实现
// ========================================================================

function SSHKeyTypeToString(AKeyType: TSSHKeyType): string;
begin
  case AKeyType of
    sshKeyRSA:        Result := 'ssh-rsa';
    sshKeyEd25519:    Result := 'ssh-ed25519';
    sshKeyECDSA_P256: Result := 'ecdsa-sha2-nistp256';
    sshKeyECDSA_P384: Result := 'ecdsa-sha2-nistp384';
    sshKeyECDSA_P521: Result := 'ecdsa-sha2-nistp521';
  else
    Result := 'unknown';
  end;
end;

function StringToSSHKeyType(const ATypeStr: string): TSSHKeyType;
var
  LType: string;
begin
  LType := LowerCase(Trim(ATypeStr));

  if LType = 'ssh-rsa' then
    Result := sshKeyRSA
  else if LType = 'ssh-ed25519' then
    Result := sshKeyEd25519
  else if LType = 'ecdsa-sha2-nistp256' then
    Result := sshKeyECDSA_P256
  else if LType = 'ecdsa-sha2-nistp384' then
    Result := sshKeyECDSA_P384
  else if LType = 'ecdsa-sha2-nistp521' then
    Result := sshKeyECDSA_P521
  else
    Result := sshKeyUnknown;
end;

function SSHKeyErrorToString(AError: TSSHKeyError): string;
begin
  case AError of
    sshKeyErrNone:               Result := 'No error';
    sshKeyErrInvalidFormat:      Result := 'Invalid key format';
    sshKeyErrUnsupportedKeyType: Result := 'Unsupported key type';
    sshKeyErrDecryptionFailed:   Result := 'Decryption failed (wrong passphrase)';
    sshKeyErrWeakKey:            Result := 'Weak key detected';
    sshKeyErrKeyMismatch:        Result := 'Key mismatch';
    sshKeyErrFileNotFound:       Result := 'File not found';
    sshKeyErrPermissionDenied:   Result := 'Permission denied';
    sshKeyErrInvalidPassphrase:  Result := 'Invalid passphrase';
    sshKeyErrGenerationFailed:   Result := 'Key generation failed';
    sshKeyErrEncodingFailed:     Result := 'Encoding failed';
    sshKeyErrDecodingFailed:     Result := 'Decoding failed';
    sshKeyErrIOError:            Result := 'I/O error';
  else
    Result := 'Unknown error';
  end;
end;

// ========================================================================
// TSSHKeyManager 实现 (骨架)
// ========================================================================

class function TSSHKeyManager.ParsePublicKey(const AKeyString: string): ISSHPublicKey;
var
  LTrimmed: string;
  LParts: TStringList;
  LKeyType: TSSHKeyType;
  LKeyData: TBytes;
  LComment: string;
  LOffset: Integer;
  LBlobType: string;
  I: Integer;
  LPEMReader: TPEMReader;
  LBlock: TPEMBlock;
begin
  Result := nil;
  LTrimmed := Trim(AKeyString);

  if LTrimmed = '' then
    raise Exception.Create('Empty key string');

  // 检查是否是 PEM 格式
  if Pos('-----BEGIN', LTrimmed) > 0 then
  begin
    LPEMReader := TPEMReader.Create;
    try
      LPEMReader.LoadFromString(LTrimmed);
      
      // 查找公钥块
      LBlock := LPEMReader.GetFirstBlockOfType(pemPublicKey);
      if Length(LBlock.Data) = 0 then
        LBlock := LPEMReader.GetFirstBlockOfType(pemRSAPublicKey);
      
      if Length(LBlock.Data) = 0 then
        raise Exception.Create('No public key found in PEM data');
      
      // 解析 SubjectPublicKeyInfo 或 RSAPublicKey
      Result := ParsePEMPublicKeyDER(LBlock.Data, LBlock.BlockType);
    finally
      LPEMReader.Free;
    end;
    Exit;
  end;

  // 解析 OpenSSH 格式: <key-type> <base64-data> [comment]
  LParts := TStringList.Create;
  try
    LParts.Delimiter := ' ';
    LParts.StrictDelimiter := False;
    LParts.DelimitedText := LTrimmed;

    if LParts.Count < 2 then
      raise Exception.Create('Invalid public key format: expected at least 2 parts');

    // 解析密钥类型
    LKeyType := StringToSSHKeyType(LParts[0]);
    if LKeyType = sshKeyUnknown then
      raise Exception.CreateFmt('Unsupported key type: %s', [LParts[0]]);

    // 解码 Base64 数据
    try
      LKeyData := SSHBase64Decode(LParts[1]);
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to decode Base64 data: %s', [E.Message]);
    end;

    if Length(LKeyData) < 4 then
      raise Exception.Create('Key data too short');

    // 验证 Blob 中的类型字符串
    LOffset := 0;
    LBlobType := ReadString(LKeyData, LOffset);
    if StringToSSHKeyType(LBlobType) <> LKeyType then
      raise Exception.CreateFmt('Key type mismatch: header says %s, blob says %s',
        [LParts[0], LBlobType]);

    // 提取注释 (第三部分及之后的所有内容)
    LComment := '';
    if LParts.Count > 2 then
    begin
      LComment := LParts[2];
      // 如果有更多部分，合并它们
      if LParts.Count > 3 then
      begin
        for I := 3 to LParts.Count - 1 do
          LComment := LComment + ' ' + LParts[I];
      end;
    end;

    // 创建公钥对象
    Result := TSSHPublicKeyImpl.Create(LKeyType, LKeyData, LComment);
  finally
    LParts.Free;
  end;
end;

class function TSSHKeyManager.ParsePublicKeyFile(const AFileName: string): ISSHPublicKey;
var
  LContent: string;
  LStream: TFileStream;
  LBytes: TBytes;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File not found: %s', [AFileName]);

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    if LStream.Size > 0 then
      LStream.ReadBuffer(LBytes[0], LStream.Size);
    LContent := TEncoding.UTF8.GetString(LBytes);
  finally
    LStream.Free;
  end;

  // 去除可能的 BOM 和空白
  LContent := Trim(LContent);

  Result := ParsePublicKey(LContent);
end;

class function TSSHKeyManager.TryParsePublicKey(const AKeyString: string;
  out AKey: ISSHPublicKey): Boolean;
begin
  try
    AKey := ParsePublicKey(AKeyString);
    Result := AKey <> nil;
  except
    AKey := nil;
    Result := False;
  end;
end;

class function TSSHKeyManager.ParsePublicKeyResult(const AKeyString: string): TSSHPublicKeyResult;
var
  LKey: ISSHPublicKey;
begin
  try
    LKey := ParsePublicKey(AKeyString);
    if LKey <> nil then
      Result := TSSHPublicKeyResult.Ok(LKey)
    else
      Result := TSSHPublicKeyResult.Err(sshKeyErrInvalidFormat, 'Failed to parse public key');
  except
    on E: Exception do
      Result := TSSHPublicKeyResult.Err(sshKeyErrInvalidFormat, E.Message);
  end;
end;

class function TSSHKeyManager.ParsePrivateKey(const AKeyData: string;
  const APassphrase: string): ISSHPrivateKey;
var
  LTrimmed: string;
  LLines: TStringList;
  LBase64: string;
  LData: TBytes;
  LKeyType: TSSHKeyType;
  LPublicKeyBlob, LPrivateKeyData: TBytes;
  LComment: string;
  LIsEncrypted: Boolean;
  I: Integer;
begin
  Result := nil;
  LTrimmed := Trim(AKeyData);

  if LTrimmed = '' then
    raise Exception.Create('Empty key data');

  // 检查是否是 OpenSSH 格式
  if Pos('-----BEGIN OPENSSH PRIVATE KEY-----', LTrimmed) > 0 then
  begin
    // 解析 OpenSSH 格式
    LLines := TStringList.Create;
    try
      LLines.Text := LTrimmed;
      LBase64 := '';

      // 提取 Base64 内容
      for I := 0 to LLines.Count - 1 do
      begin
        if (Pos('-----BEGIN', LLines[I]) = 0) and
           (Pos('-----END', LLines[I]) = 0) then
          LBase64 := LBase64 + Trim(LLines[I]);
      end;

      // 解码 Base64
      try
        LData := SSHBase64Decode(LBase64);
      except
        on E: Exception do
          raise Exception.CreateFmt('Failed to decode Base64 data: %s', [E.Message]);
      end;

      // 解析 OpenSSH 私钥格式
      if not ParseOpenSSHPrivateKey(LData, APassphrase, LKeyType,
        LPublicKeyBlob, LPrivateKeyData, LComment, LIsEncrypted) then
      begin
        if LIsEncrypted and (APassphrase = '') then
          raise Exception.Create('Private key is encrypted, passphrase required')
        else
          raise Exception.Create('Failed to parse OpenSSH private key');
      end;

      Result := TSSHPrivateKeyImpl.Create(LKeyType, LPublicKeyBlob,
        LPrivateKeyData, LComment, LIsEncrypted);
    finally
      LLines.Free;
    end;
  end
  else if (Pos('-----BEGIN RSA PRIVATE KEY-----', LTrimmed) > 0) or
          (Pos('-----BEGIN EC PRIVATE KEY-----', LTrimmed) > 0) or
          (Pos('-----BEGIN PRIVATE KEY-----', LTrimmed) > 0) then
  begin
    // PEM 格式私钥解析
    Result := ParsePEMPrivateKey(LTrimmed, APassphrase);
  end
  else
    raise Exception.Create('Unknown private key format');
end;

class function TSSHKeyManager.ParsePrivateKeyFile(const AFileName: string;
  const APassphrase: string): ISSHPrivateKey;
var
  LContent: string;
  LStream: TFileStream;
  LBytes: TBytes;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File not found: %s', [AFileName]);

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    if LStream.Size > 0 then
      LStream.ReadBuffer(LBytes[0], LStream.Size);
    LContent := TEncoding.UTF8.GetString(LBytes);
  finally
    LStream.Free;
  end;

  Result := ParsePrivateKey(LContent, APassphrase);
end;

class function TSSHKeyManager.TryParsePrivateKey(const AKeyData: string;
  const APassphrase: string; out AKey: ISSHPrivateKey): Boolean;
begin
  try
    AKey := ParsePrivateKey(AKeyData, APassphrase);
    Result := AKey <> nil;
  except
    AKey := nil;
    Result := False;
  end;
end;

class function TSSHKeyManager.ParsePrivateKeyResult(const AKeyData: string;
  const APassphrase: string): TSSHPrivateKeyResult;
var
  LKey: ISSHPrivateKey;
begin
  try
    LKey := ParsePrivateKey(AKeyData, APassphrase);
    if LKey <> nil then
      Result := TSSHPrivateKeyResult.Ok(LKey)
    else
      Result := TSSHPrivateKeyResult.Err(sshKeyErrInvalidFormat, 'Failed to parse private key');
  except
    on E: Exception do
      Result := TSSHPrivateKeyResult.Err(sshKeyErrInvalidFormat, E.Message);
  end;
end;

{ 将 BIGNUM 转换为 TBytes (带前导零以确保正数表示) }
function BNToBytes(const ABN: PBIGNUM): TBytes;
var
  LLen: Integer;
begin
  if ABN = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  LLen := (BN_num_bits(ABN) + 7) div 8;
  if LLen = 0 then
    LLen := 1;

  SetLength(Result, LLen);
  BN_bn2bin(ABN, @Result[0]);

  // 如果最高位为 1，添加前导零以确保正数表示
  if (Length(Result) > 0) and ((Result[0] and $80) <> 0) then
  begin
    SetLength(Result, Length(Result) + 1);
    Move(Result[0], Result[1], Length(Result) - 1);
    Result[0] := 0;
  end;
end;

class function TSSHKeyManager.GenerateRSAKeyPair(ABits: Integer;
  const AComment: string): ISSHKeyPair;
var
  LRSAKey: PRSA;
  LExp: PBIGNUM;
  LN, LE, LD, LP, LQ, LIQMP: PBIGNUM;
  LNBytes, LEBytes, LDBytes, LPBytes, LQBytes, LIQMPBytes: TBytes;
  LPublicKeyBlob, LPrivateKeyData: TBytes;
  LPublicKey: ISSHPublicKey;
  LPrivateKey: ISSHPrivateKey;
begin
  Result := nil;

  // 验证参数
  if (ABits <> 2048) and (ABits <> 3072) and (ABits <> 4096) then
    raise Exception.CreateFmt('Invalid RSA key size: %d (must be 2048, 3072, or 4096)', [ABits]);

  // 确保 OpenSSL RSA 模块已加载
  if not Assigned(RSA_new) then
  begin
    if not LoadOpenSSLRSA then
      raise Exception.Create('Failed to load OpenSSL RSA module');
  end;
  if not Assigned(BN_new) then
  begin
    if not LoadOpenSSLBN then
      raise Exception.Create('Failed to load OpenSSL BN module');
  end;

  LRSAKey := RSA_new();
  if LRSAKey = nil then
    raise Exception.Create('Failed to create RSA key structure');

  LExp := BN_new();
  if LExp = nil then
  begin
    RSA_free(LRSAKey);
    raise Exception.Create('Failed to create BIGNUM for exponent');
  end;

  try
    // 设置公钥指数为 65537 (F4)
    BN_set_word(LExp, RSA_F4);

    // 生成 RSA 密钥
    if RSA_generate_key_ex(LRSAKey, ABits, LExp, nil) <> 1 then
      raise Exception.CreateFmt('Failed to generate %d-bit RSA key', [ABits]);

    // 提取密钥参数
    LN := RSA_get0_n(LRSAKey);
    LE := RSA_get0_e(LRSAKey);
    LD := RSA_get0_d(LRSAKey);
    LP := RSA_get0_p(LRSAKey);
    LQ := RSA_get0_q(LRSAKey);
    LIQMP := RSA_get0_iqmp(LRSAKey);

    if (LN = nil) or (LE = nil) or (LD = nil) or (LP = nil) or (LQ = nil) or (LIQMP = nil) then
      raise Exception.Create('Failed to extract RSA key parameters');

    // 转换为字节数组
    LNBytes := BNToBytes(LN);
    LEBytes := BNToBytes(LE);
    LDBytes := BNToBytes(LD);
    LPBytes := BNToBytes(LP);
    LQBytes := BNToBytes(LQ);
    LIQMPBytes := BNToBytes(LIQMP);

    // 构建 SSH 公钥 Blob
    LPublicKeyBlob := BuildRSAPublicKeyBlob(LEBytes, LNBytes);

    // 构建私钥数据 (SSH 格式: n, e, d, iqmp, p, q)
    SetLength(LPrivateKeyData, 0);
    WriteBytesWithLength(LPrivateKeyData, LNBytes);
    WriteBytesWithLength(LPrivateKeyData, LEBytes);
    WriteBytesWithLength(LPrivateKeyData, LDBytes);
    WriteBytesWithLength(LPrivateKeyData, LIQMPBytes);
    WriteBytesWithLength(LPrivateKeyData, LPBytes);
    WriteBytesWithLength(LPrivateKeyData, LQBytes);

    // 创建公钥和私钥对象
    LPublicKey := TSSHPublicKeyImpl.Create(sshKeyRSA, LPublicKeyBlob, AComment);
    LPrivateKey := TSSHPrivateKeyImpl.Create(sshKeyRSA, LPublicKeyBlob, LPrivateKeyData, AComment, False);

    // 创建密钥对
    Result := TSSHKeyPairImpl.Create(LPublicKey, LPrivateKey);
  finally
    BN_free(LExp);
    RSA_free(LRSAKey);
  end;
end;

class function TSSHKeyManager.GenerateEd25519KeyPair(const AComment: string): ISSHKeyPair;
var
  LCtx: PEVP_PKEY_CTX;
  LPKey: PEVP_PKEY;
  LPubKeyLen, LPrivKeyLen: NativeUInt;
  LPubKeyData, LPrivKeyData: TBytes;
  LPublicKeyBlob, LPrivateKeyData: TBytes;
  LPublicKey: ISSHPublicKey;
  LPrivateKey: ISSHPrivateKey;
  LLibHandle: THandle;
begin
  Result := nil;

  // 确保 OpenSSL EVP 模块已加载
  if not Assigned(EVP_PKEY_CTX_new_id) then
  begin
    LLibHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
    if LLibHandle = 0 then
      raise Exception.Create('Failed to load OpenSSL crypto library');
    if not LoadEVP(LLibHandle) then
      raise Exception.Create('Failed to load OpenSSL EVP module');
  end;

  // 创建 Ed25519 密钥上下文
  LCtx := EVP_PKEY_CTX_new_id(EVP_PKEY_ED25519, nil);
  if LCtx = nil then
    raise Exception.Create('Failed to create Ed25519 key context');

  LPKey := nil;
  try
    // 初始化密钥生成
    if EVP_PKEY_keygen_init(LCtx) <> 1 then
      raise Exception.Create('Failed to initialize Ed25519 key generation');

    // 生成密钥
    if EVP_PKEY_keygen(LCtx, LPKey) <> 1 then
      raise Exception.Create('Failed to generate Ed25519 key');

    if LPKey = nil then
      raise Exception.Create('Ed25519 key generation returned nil');

    // 获取公钥数据
    LPubKeyLen := 0;
    if EVP_PKEY_get_raw_public_key(LPKey, nil, LPubKeyLen) <> 1 then
      raise Exception.Create('Failed to get Ed25519 public key length');

    SetLength(LPubKeyData, LPubKeyLen);
    if EVP_PKEY_get_raw_public_key(LPKey, @LPubKeyData[0], LPubKeyLen) <> 1 then
      raise Exception.Create('Failed to get Ed25519 public key data');

    // 获取私钥数据
    LPrivKeyLen := 0;
    if EVP_PKEY_get_raw_private_key(LPKey, nil, LPrivKeyLen) <> 1 then
      raise Exception.Create('Failed to get Ed25519 private key length');

    SetLength(LPrivKeyData, LPrivKeyLen);
    if EVP_PKEY_get_raw_private_key(LPKey, @LPrivKeyData[0], LPrivKeyLen) <> 1 then
      raise Exception.Create('Failed to get Ed25519 private key data');

    // 构建 SSH 公钥 Blob
    LPublicKeyBlob := BuildEd25519PublicKeyBlob(LPubKeyData);

    // 构建私钥数据 (SSH 格式: 64 字节 = 32 字节私钥 + 32 字节公钥)
    SetLength(LPrivateKeyData, Length(LPrivKeyData) + Length(LPubKeyData));
    Move(LPrivKeyData[0], LPrivateKeyData[0], Length(LPrivKeyData));
    Move(LPubKeyData[0], LPrivateKeyData[Length(LPrivKeyData)], Length(LPubKeyData));

    // 创建公钥和私钥对象
    LPublicKey := TSSHPublicKeyImpl.Create(sshKeyEd25519, LPublicKeyBlob, AComment);
    LPrivateKey := TSSHPrivateKeyImpl.Create(sshKeyEd25519, LPublicKeyBlob, LPrivateKeyData, AComment, False);

    // 创建密钥对
    Result := TSSHKeyPairImpl.Create(LPublicKey, LPrivateKey);
  finally
    if LPKey <> nil then
      EVP_PKEY_free(LPKey);
    EVP_PKEY_CTX_free(LCtx);
  end;
end;

class function TSSHKeyManager.GenerateECDSAKeyPair(AKeyType: TSSHKeyType;
  const AComment: string): ISSHKeyPair;
var
  LKey: PEC_KEY;
  LGroup: PEC_GROUP;
  LPubPoint: PEC_POINT;
  LPrivBN: PBIGNUM;
  LNID: Integer;
  LPubKeyLen: NativeUInt;
  LPubKeyData, LPrivKeyData: TBytes;
  LPublicKeyBlob, LPrivateKeyDataSSH: TBytes;
  LPublicKey: ISSHPublicKey;
  LPrivateKey: ISSHPrivateKey;
  LLibHandle: THandle;
begin
  Result := nil;

  // 确定曲线 NID
  case AKeyType of
    sshKeyECDSA_P256: LNID := NID_X9_62_prime256v1;
    sshKeyECDSA_P384: LNID := NID_secp384r1;
    sshKeyECDSA_P521: LNID := NID_secp521r1;
  else
    raise Exception.Create('Invalid ECDSA key type');
  end;

  // 确保 OpenSSL EC 模块已加载
  if not Assigned(EC_KEY_new_by_curve_name) then
  begin
    LLibHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
    if LLibHandle = 0 then
      raise Exception.Create('Failed to load OpenSSL crypto library');
    if not LoadECFunctions(LLibHandle) then
      raise Exception.Create('Failed to load OpenSSL EC module');
  end;

  // 确保 BN 模块已加载
  if not Assigned(BN_num_bytes) then
  begin
    if not LoadOpenSSLBN then
      raise Exception.Create('Failed to load OpenSSL BN module');
  end;

  // 创建 EC 密钥
  LKey := EC_KEY_new_by_curve_name(LNID);
  if LKey = nil then
    raise Exception.CreateFmt('Failed to create EC key for curve NID %d', [LNID]);

  try
    // 生成密钥
    if EC_KEY_generate_key(LKey) <> 1 then
      raise Exception.Create('Failed to generate ECDSA key');

    // 获取密钥组件
    LGroup := EC_KEY_get0_group(LKey);
    LPubPoint := EC_KEY_get0_public_key(LKey);
    LPrivBN := EC_KEY_get0_private_key(LKey);

    if (LGroup = nil) or (LPubPoint = nil) or (LPrivBN = nil) then
      raise Exception.Create('Failed to get ECDSA key components');

    // 获取公钥点数据 (未压缩格式)
    LPubKeyLen := EC_POINT_point2oct(LGroup, LPubPoint, POINT_CONVERSION_UNCOMPRESSED, nil, 0, nil);
    if LPubKeyLen = 0 then
      raise Exception.Create('Failed to get ECDSA public key length');

    SetLength(LPubKeyData, LPubKeyLen);
    if EC_POINT_point2oct(LGroup, LPubPoint, POINT_CONVERSION_UNCOMPRESSED, @LPubKeyData[0], LPubKeyLen, nil) = 0 then
      raise Exception.Create('Failed to serialize ECDSA public key');

    // 获取私钥数据
    LPrivKeyData := BNToBytes(LPrivBN);

    // 构建 SSH 公钥 Blob
    LPublicKeyBlob := BuildECDSAPublicKeyBlob(AKeyType, LPubKeyData);

    // 构建私钥数据 (SSH 格式)
    SetLength(LPrivateKeyDataSSH, 0);
    WriteBytesWithLength(LPrivateKeyDataSSH, LPrivKeyData);

    // 创建公钥和私钥对象
    LPublicKey := TSSHPublicKeyImpl.Create(AKeyType, LPublicKeyBlob, AComment);
    LPrivateKey := TSSHPrivateKeyImpl.Create(AKeyType, LPublicKeyBlob, LPrivateKeyDataSSH, AComment, False);

    // 创建密钥对
    Result := TSSHKeyPairImpl.Create(LPublicKey, LPrivateKey);
  finally
    EC_KEY_free(LKey);
  end;
end;

class function TSSHKeyManager.GenerateKeyPairResult(AKeyType: TSSHKeyType;
  ABits: Integer; const AComment: string): TSSHKeyPairResult;
var
  LKeyPair: ISSHKeyPair;
begin
  try
    case AKeyType of
      sshKeyRSA:
        LKeyPair := GenerateRSAKeyPair(ABits, AComment);
      sshKeyEd25519:
        LKeyPair := GenerateEd25519KeyPair(AComment);
      sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
        LKeyPair := GenerateECDSAKeyPair(AKeyType, AComment);
    else
      begin
        Result := TSSHKeyPairResult.Err(sshKeyErrUnsupportedKeyType,
          'Unsupported key type: ' + SSHKeyTypeToString(AKeyType));
        Exit;
      end;
    end;

    if LKeyPair <> nil then
      Result := TSSHKeyPairResult.Ok(LKeyPair)
    else
      Result := TSSHKeyPairResult.Err(sshKeyErrGenerationFailed, 'Failed to generate key pair');
  except
    on E: Exception do
      Result := TSSHKeyPairResult.Err(sshKeyErrGenerationFailed, E.Message);
  end;
end;

class function TSSHKeyManager.ConvertToOpenSSH(AKey: ISSHPrivateKey;
  const APassphrase: string): string;
begin
  if AKey = nil then
    raise Exception.Create('Key is nil');
  Result := AKey.ToOpenSSHFormat(APassphrase);
end;

class function TSSHKeyManager.ConvertToPEM(AKey: ISSHPrivateKey;
  const APassphrase: string): string;
begin
  if AKey = nil then
    raise Exception.Create('Key is nil');
  Result := AKey.ToPEMFormat(APassphrase);
end;

class function TSSHKeyManager.ConvertPublicKeyToOpenSSH(AKey: ISSHPublicKey): string;
begin
  if AKey = nil then
    raise Exception.Create('Key is nil');
  Result := AKey.ToOpenSSHFormat;
end;

class function TSSHKeyManager.ConvertPublicKeyToPEM(AKey: ISSHPublicKey): string;
begin
  if AKey = nil then
    raise Exception.Create('Key is nil');
  Result := AKey.ToPEMFormat;
end;

class function TSSHKeyManager.CalculateFingerprint(AKey: ISSHPublicKey;
  ASHA256: Boolean): string;
begin
  if AKey = nil then
    raise Exception.Create('Key is nil');

  if ASHA256 then
    Result := AKey.GetFingerprintSHA256
  else
    Result := AKey.GetFingerprintMD5;
end;

class function TSSHKeyManager.ValidateKeyPair(APubKey: ISSHPublicKey;
  APrivKey: ISSHPrivateKey): Boolean;
begin
  if (APubKey = nil) or (APrivKey = nil) then
  begin
    Result := False;
    Exit;
  end;

  Result := APrivKey.MatchesPublicKey(APubKey);
end;

class function TSSHKeyManager.IsWeakKey(AKey: ISSHPublicKey): Boolean;
begin
  if AKey = nil then
  begin
    Result := True;
    Exit;
  end;

  // 检查密钥强度
  case AKey.KeyType of
    sshKeyRSA:
      // RSA 密钥小于 2048 位被认为是弱密钥
      Result := AKey.GetKeySize < 2048;
    sshKeyEd25519:
      // Ed25519 密钥固定为 256 位，不存在弱密钥问题
      Result := False;
    sshKeyECDSA_P256, sshKeyECDSA_P384, sshKeyECDSA_P521:
      // ECDSA 使用标准曲线，不存在弱密钥问题
      Result := False;
  else
    Result := True;  // 未知类型视为弱密钥
  end;
end;

class function TSSHKeyManager.ParseAuthorizedKeys(const AContent: string): TSSHAuthorizedKeyEntryArray;
var
  LEntries: TSSHAuthorizedKeyEntryArray;
begin
  LEntries := ParseAuthorizedKeysWithOptions(AContent);
  Result := LEntries;
end;

class function TSSHKeyManager.WriteAuthorizedKeys(const AKeys: array of ISSHPublicKey): string;
var
  I: Integer;
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    for I := 0 to High(AKeys) do
    begin
      if AKeys[I] <> nil then
        LLines.Add(AKeys[I].ToOpenSSHFormat);
    end;
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

class function TSSHKeyManager.ParseAuthorizedKeysWithOptions(const AContent: string): TSSHAuthorizedKeyEntryArray;
var
  LLines: TStringList;
  I, LCount: Integer;
  LLine: string;
  LEntry: TSSHAuthorizedKeyEntry;
begin
  SetLength(Result, 0);
  LLines := TStringList.Create;
  try
    LLines.Text := AContent;
    LCount := 0;

    for I := 0 to LLines.Count - 1 do
    begin
      LLine := Trim(LLines[I]);

      // 跳过空行和注释
      if (LLine = '') or (LLine[1] = '#') then
        Continue;

      // 尝试解析
      if TSSHAuthorizedKeyEntry.TryParse(LLine, LEntry) then
      begin
        SetLength(Result, LCount + 1);
        Result[LCount] := LEntry;
        Inc(LCount);
      end;
      // 无效行被跳过（符合需求 8.4）
    end;
  finally
    LLines.Free;
  end;
end;

end.
