{
  fafafa.ssl.utils - SSL/TLS 辅助工具单元
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    提供各种辅助函数和工具类，包括：
    - 十六进制编码/解码
    - Base64 编码/解码
    - 证书工具
    - 网络工具
    - 调试工具
}

unit fafafa.ssl.utils;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, StrUtils,
  fafafa.ssl.types, fafafa.ssl.intf;

type
  { TSSLUtils - SSL 工具类 }
  TSSLUtils = class
  public
    // 编码转换
    class function BytesToHex(const aBytes: TBytes): string;
    class function HexToBytes(const aHex: string): TBytes;
    class function BytesToBase64(const aBytes: TBytes): string;
    class function Base64ToBytes(const aBase64: string): TBytes;
    class function StringToHex(const aStr: string): string;
    class function HexToString(const aHex: string): string;
    
    // 证书工具
    class function IsPEMFormat(const aData: string): Boolean;
    class function IsDERFormat(const aData: TBytes): Boolean;
    class function PEMToDER(const aPEM: string): TBytes;
    class function DERToPEM(const aDER: TBytes; const aType: string = 'CERTIFICATE'): string;
    class function ExtractPEMBlock(const aPEM: string; const aType: string = 'CERTIFICATE'): string;
    class function FormatCertificateSubject(const aSubject: string): string;
    class function ParseDistinguishedName(const aDN: string): TStringList;
    
    // 指纹计算
    class function CalculateSHA1(const aData: TBytes): string;
    class function CalculateSHA256(const aData: TBytes): string;
    class function CalculateMD5(const aData: TBytes): string;
    
    // 网络工具
    class function IsIPAddress(const aStr: string): Boolean;
    class function IsIPv4Address(const aStr: string): Boolean;
    class function IsIPv6Address(const aStr: string): Boolean;
    class function IsValidHostname(const aHost: string): Boolean;
    class function ParseURL(const aURL: string; out aProtocol, aHost: string; 
                           out aPort: Integer; out aPath: string): Boolean;
    class function NormalizeHostname(const aHost: string): string;
    
    // 错误处理
    class function FormatSSLError(aError: TSSLErrorCode; const aContext: string = ''): string;
    class function GetErrorDetails(aException: ESSLException): string;
    
    // 调试工具
    class function DumpBytes(const aBytes: TBytes; aBytesPerLine: Integer = 16): string;
    class function DumpSSLConfig(const aConfig: TSSLConfig): string;
    class function DumpCertificateInfo(const aInfo: TSSLCertificateInfo): string;
    class function DumpConnectionInfo(const aInfo: TSSLConnectionInfo): string;
    
    // 版本比较
    class function CompareVersions(const aVer1, aVer2: string): Integer;
    class function IsVersionSupported(const aVersion: string; 
                                     const aMinVersion: string = ''): Boolean;
  end;

  { TSSLMemoryStream - 带位置跟踪的内存流 }
  TSSLMemoryStream = class(TMemoryStream)
  private
    FName: string;
    FDebug: Boolean;
  public
    constructor Create(const aName: string = ''; aDebug: Boolean = False);
    
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: DWord;
    function ReadBytes(aCount: Integer): TBytes;
    function ReadString(aLength: Integer): string;
    
    procedure WriteByte(aValue: Byte);
    procedure WriteWord(aValue: Word);
    procedure WriteDWord(aValue: DWord);
    procedure WriteBytes(const aBytes: TBytes);
    procedure WriteString(const aStr: string);
    
    function PeekByte: Byte;
    function RemainingBytes: Integer;
    function GetHexDump: string;
    
    property Name: string read FName write FName;
    property Debug: Boolean read FDebug write FDebug;
  end;

  { TSSLStringBuilder - 字符串构建器 }
  TSSLStringBuilder = class
  private
    FBuffer: TStringList;
    FIndentLevel: Integer;
    FIndentStr: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Append(const aStr: string);
    procedure AppendLine(const aStr: string = '');
    procedure AppendFormat(const aFormat: string; const aArgs: array of const);
    
    procedure Indent;
    procedure Unindent;
    procedure SetIndentStr(const aStr: string);
    
    procedure Clear;
    function ToString: string; override;
    
    property IndentLevel: Integer read FIndentLevel;
  end;

  { TSSLBitSet - 位集合操作 }
  TSSLBitSet = class
  private
    FBits: array of Byte;
    FSize: Integer;
    function GetBit(aIndex: Integer): Boolean;
    procedure SetBit(aIndex: Integer; aValue: Boolean);
  public
    constructor Create(aSize: Integer);
    destructor Destroy; override;
    
    procedure Clear;
    procedure SetAll;
    procedure Toggle(aIndex: Integer);
    
    function Count: Integer;  // 返回设置为1的位数
    function IsEmpty: Boolean;
    function IsFull: Boolean;
    
    function ToBytes: TBytes;
    procedure FromBytes(const aBytes: TBytes);
    
    property Bits[aIndex: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize;
  end;

const
  // PEM 块标记
  PEM_BEGIN_MARKER = '-----BEGIN ';
  PEM_END_MARKER = '-----END ';
  PEM_CERTIFICATE = 'CERTIFICATE';
  PEM_RSA_PRIVATE_KEY = 'RSA PRIVATE KEY';
  PEM_PRIVATE_KEY = 'PRIVATE KEY';
  PEM_PUBLIC_KEY = 'PUBLIC KEY';
  PEM_CERTIFICATE_REQUEST = 'CERTIFICATE REQUEST';

implementation

uses
  Base64, Math;

{ TSSLUtils }

class function TSSLUtils.BytesToHex(const aBytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(aBytes) do
    Result := Result + IntToHex(aBytes[I], 2);
end;

class function TSSLUtils.HexToBytes(const aHex: string): TBytes;
var
  I, LLen: Integer;
  LHex: string;
begin
  LHex := UpperCase(Trim(aHex));
  LHex := StringReplace(LHex, ' ', '', [rfReplaceAll]);
  LHex := StringReplace(LHex, ':', '', [rfReplaceAll]);
  
  LLen := Length(LHex) div 2;
  SetLength(Result, LLen);
  
  for I := 0 to LLen - 1 do
    Result[I] := StrToInt('$' + Copy(LHex, I * 2 + 1, 2));
end;

class function TSSLUtils.BytesToBase64(const aBytes: TBytes): string;
var
  LStream: TMemoryStream;
  LEncoder: TBase64EncodingStream;
  LOutput: TStringStream;
begin
  if Length(aBytes) = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  LStream := TMemoryStream.Create;
  LOutput := TStringStream.Create('');
  try
    LStream.Write(aBytes[0], Length(aBytes));
    LStream.Position := 0;
    
    LEncoder := TBase64EncodingStream.Create(LOutput);
    try
      LEncoder.CopyFrom(LStream, LStream.Size);
    finally
      LEncoder.Free;
    end;
    
    Result := LOutput.DataString;
  finally
    LOutput.Free;
    LStream.Free;
  end;
end;

class function TSSLUtils.Base64ToBytes(const aBase64: string): TBytes;
var
  LInput: TStringStream;
  LDecoder: TBase64DecodingStream;
  LOutput: TMemoryStream;
begin
  if aBase64 = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  
  LInput := TStringStream.Create(aBase64);
  LOutput := TMemoryStream.Create;
  try
    LInput.Position := 0;
    
    LDecoder := TBase64DecodingStream.Create(LInput);
    try
      LOutput.CopyFrom(LDecoder, 0);
    finally
      LDecoder.Free;
    end;
    
    SetLength(Result, LOutput.Size);
    if LOutput.Size > 0 then
      LOutput.Position := 0;
      LOutput.Read(Result[0], LOutput.Size);
  finally
    LOutput.Free;
    LInput.Free;
  end;
end;

class function TSSLUtils.StringToHex(const aStr: string): string;
var
  LBytes: TBytes;
begin
  LBytes := TEncoding.UTF8.GetBytes(aStr);
  Result := BytesToHex(LBytes);
end;

class function TSSLUtils.HexToString(const aHex: string): string;
var
  LBytes: TBytes;
begin
  LBytes := HexToBytes(aHex);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

class function TSSLUtils.IsPEMFormat(const aData: string): Boolean;
begin
  Result := (Pos(PEM_BEGIN_MARKER, aData) > 0) and 
            (Pos(PEM_END_MARKER, aData) > 0);
end;

class function TSSLUtils.IsDERFormat(const aData: TBytes): Boolean;
begin
  // DER 格式通常以 0x30 (SEQUENCE) 开始
  Result := (Length(aData) > 0) and (aData[0] = $30);
end;

class function TSSLUtils.PEMToDER(const aPEM: string): TBytes;
var
  LLines: TStringList;
  I: Integer;
  LInBlock: Boolean;
  LBase64: string;
begin
  SetLength(Result, 0);
  
  LLines := TStringList.Create;
  try
    LLines.Text := aPEM;
    LInBlock := False;
    LBase64 := '';
    
    for I := 0 to LLines.Count - 1 do
    begin
      if Pos(PEM_BEGIN_MARKER, LLines[I]) > 0 then
      begin
        LInBlock := True;
        Continue;
      end;
      
      if Pos(PEM_END_MARKER, LLines[I]) > 0 then
      begin
        LInBlock := False;
        Break;
      end;
      
      if LInBlock then
        LBase64 := LBase64 + Trim(LLines[I]);
    end;
    
    if LBase64 <> '' then
      Result := Base64ToBytes(LBase64);
  finally
    LLines.Free;
  end;
end;

class function TSSLUtils.DERToPEM(const aDER: TBytes; const aType: string): string;
var
  LBase64: string;
  LLines: TStringList;
  I: Integer;
begin
  Result := '';
  
  if Length(aDER) = 0 then
    Exit;
  
  LBase64 := BytesToBase64(aDER);
  
  LLines := TStringList.Create;
  try
    LLines.Add(PEM_BEGIN_MARKER + aType + '-----');
    
    // 每64个字符换行
    I := 1;
    while I <= Length(LBase64) do
    begin
      LLines.Add(Copy(LBase64, I, 64));
      Inc(I, 64);
    end;
    
    LLines.Add(PEM_END_MARKER + aType + '-----');
    
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

class function TSSLUtils.ExtractPEMBlock(const aPEM: string; const aType: string): string;
var
  LBeginMarker, LEndMarker: string;
  LBeginPos, LEndPos: Integer;
begin
  Result := '';
  
  LBeginMarker := PEM_BEGIN_MARKER + aType + '-----';
  LEndMarker := PEM_END_MARKER + aType + '-----';
  
  LBeginPos := Pos(LBeginMarker, aPEM);
  if LBeginPos = 0 then
    Exit;
  
  LEndPos := PosEx(LEndMarker, aPEM, LBeginPos);
  if LEndPos = 0 then
    Exit;
  
  Result := Copy(aPEM, LBeginPos, LEndPos - LBeginPos + Length(LEndMarker));
end;

class function TSSLUtils.FormatCertificateSubject(const aSubject: string): string;
var
  LParts: TStringList;
  I: Integer;
begin
  LParts := ParseDistinguishedName(aSubject);
  try
    Result := '';
    for I := 0 to LParts.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + LParts[I];
    end;
  finally
    LParts.Free;
  end;
end;

class function TSSLUtils.ParseDistinguishedName(const aDN: string): TStringList;
var
  LParts: TStringArray;
  I: Integer;
begin
  Result := TStringList.Create;
  
  // 简单解析，实际应该更复杂
  LParts := aDN.Split([',', '/']);
  for I := 0 to High(LParts) do
  begin
    if Trim(LParts[I]) <> '' then
      Result.Add(Trim(LParts[I]));
  end;
end;

class function TSSLUtils.CalculateSHA1(const aData: TBytes): string;
begin
  // TODO: 实现 SHA1 计算
  Result := 'SHA1_NOT_IMPLEMENTED';
end;

class function TSSLUtils.CalculateSHA256(const aData: TBytes): string;
begin
  // TODO: 实现 SHA256 计算
  Result := 'SHA256_NOT_IMPLEMENTED';
end;

class function TSSLUtils.CalculateMD5(const aData: TBytes): string;
begin
  // TODO: 实现 MD5 计算
  Result := 'MD5_NOT_IMPLEMENTED';
end;

class function TSSLUtils.IsIPAddress(const aStr: string): Boolean;
begin
  Result := IsIPv4Address(aStr) or IsIPv6Address(aStr);
end;

class function TSSLUtils.IsIPv4Address(const aStr: string): Boolean;
var
  LParts: TStringArray;
  I, LNum: Integer;
begin
  Result := False;
  
  LParts := aStr.Split(['.']);
  if Length(LParts) <> 4 then
    Exit;
  
  for I := 0 to 3 do
  begin
    if not TryStrToInt(LParts[I], LNum) then
      Exit;
    if (LNum < 0) or (LNum > 255) then
      Exit;
  end;
  
  Result := True;
end;

class function TSSLUtils.IsIPv6Address(const aStr: string): Boolean;
var
  I, Count: Integer;
begin
  // 简单的 IPv6 检查
  Count := 0;
  for I := 1 to Length(aStr) do
    if aStr[I] = ':' then
      Inc(Count);
  
  Result := (Pos(':', aStr) > 0) and 
            ((Count >= 2) or (Pos('::', aStr) > 0));
end;

class function TSSLUtils.IsValidHostname(const aHost: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  
  if (aHost = '') or (Length(aHost) > 253) then
    Exit;
  
  // 简单验证
  for I := 1 to Length(aHost) do
  begin
    if not (aHost[I] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-']) then
      Exit;
  end;
  
  Result := True;
end;

class function TSSLUtils.ParseURL(const aURL: string; out aProtocol, aHost: string;
  out aPort: Integer; out aPath: string): Boolean;
var
  LPos, LSlashPos, LColonPos: Integer;
  LHostPort: string;
begin
  Result := False;
  aProtocol := '';
  aHost := '';
  aPort := 0;
  aPath := '/';
  
  // 提取协议
  LPos := Pos('://', aURL);
  if LPos > 0 then
  begin
    aProtocol := LowerCase(Copy(aURL, 1, LPos - 1));
    LHostPort := Copy(aURL, LPos + 3, MaxInt);
  end
  else
  begin
    aProtocol := 'https';
    LHostPort := aURL;
  end;
  
  // 提取路径
  LSlashPos := Pos('/', LHostPort);
  if LSlashPos > 0 then
  begin
    aPath := Copy(LHostPort, LSlashPos, MaxInt);
    LHostPort := Copy(LHostPort, 1, LSlashPos - 1);
  end;
  
  // 提取主机和端口
  LColonPos := Pos(':', LHostPort);
  if LColonPos > 0 then
  begin
    aHost := Copy(LHostPort, 1, LColonPos - 1);
    if not TryStrToInt(Copy(LHostPort, LColonPos + 1, MaxInt), aPort) then
      Exit;
  end
  else
  begin
    aHost := LHostPort;
    if aProtocol = 'https' then
      aPort := 443
    else if aProtocol = 'http' then
      aPort := 80;
  end;
  
  Result := (aHost <> '') and (aPort > 0);
end;

class function TSSLUtils.NormalizeHostname(const aHost: string): string;
begin
  Result := LowerCase(Trim(aHost));
end;

class function TSSLUtils.FormatSSLError(aError: TSSLErrorCode; const aContext: string): string;
begin
  Result := Format('[%s] %s', [
    SSL_ERROR_MESSAGES[aError],
    IfThen(aContext <> '', ' - ' + aContext, '')
  ]);
end;

class function TSSLUtils.GetErrorDetails(aException: ESSLException): string;
var
  LSB: TSSLStringBuilder;
begin
  LSB := TSSLStringBuilder.Create;
  try
    LSB.AppendLine('SSL 错误详情:');
    LSB.Indent;
    LSB.AppendFormat('错误代码: %s', [SSL_ERROR_MESSAGES[aException.ErrorCode]]);
    LSB.AppendFormat('库类型: %s', [SSL_LIBRARY_NAMES[aException.LibraryType]]);
    
    if aException.NativeError <> 0 then
      LSB.AppendFormat('原生错误码: 0x%x', [aException.NativeError]);
    
    if aException.NativeErrorMessage <> '' then
      LSB.AppendFormat('原生错误消息: %s', [aException.NativeErrorMessage]);
    
    if aException.Context <> '' then
      LSB.AppendFormat('上下文: %s', [aException.Context]);
    
    LSB.AppendFormat('异常消息: %s', [aException.Message]);
    
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLUtils.DumpBytes(const aBytes: TBytes; aBytesPerLine: Integer): string;
var
  I, J: Integer;
  LHex, LAscii: string;
  LSB: TSSLStringBuilder;
begin
  LSB := TSSLStringBuilder.Create;
  try
    for I := 0 to (Length(aBytes) - 1) div aBytesPerLine do
    begin
      LHex := '';
      LAscii := '';
      
      for J := 0 to aBytesPerLine - 1 do
      begin
        if I * aBytesPerLine + J < Length(aBytes) then
        begin
          LHex := LHex + IntToHex(aBytes[I * aBytesPerLine + J], 2) + ' ';
          if aBytes[I * aBytesPerLine + J] in [32..126] then
            LAscii := LAscii + Chr(aBytes[I * aBytesPerLine + J])
          else
            LAscii := LAscii + '.';
        end
        else
        begin
          LHex := LHex + '   ';
          LAscii := LAscii + ' ';
        end;
      end;
      
      LSB.AppendFormat('%8.8x  %-*s  %s', [
        I * aBytesPerLine,
        aBytesPerLine * 3,
        LHex,
        LAscii
      ]);
    end;
    
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLUtils.DumpSSLConfig(const aConfig: TSSLConfig): string;
var
  LSB: TSSLStringBuilder;
begin
  LSB := TSSLStringBuilder.Create;
  try
    LSB.AppendLine('SSL 配置:');
    LSB.Indent;
    LSB.AppendFormat('库类型: %s', [SSL_LIBRARY_NAMES[aConfig.LibraryType]]);
    LSB.AppendFormat('上下文类型: %d', [Ord(aConfig.ContextType)]);
    LSB.AppendFormat('证书文件: %s', [aConfig.CertificateFile]);
    LSB.AppendFormat('私钥文件: %s', [aConfig.PrivateKeyFile]);
    LSB.AppendFormat('CA文件: %s', [aConfig.CAFile]);
    LSB.AppendFormat('缓冲区大小: %d', [aConfig.BufferSize]);
    LSB.AppendFormat('握手超时: %d ms', [aConfig.HandshakeTimeout]);
    LSB.AppendFormat('服务器名称: %s', [aConfig.ServerName]);
    
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLUtils.DumpCertificateInfo(const aInfo: TSSLCertificateInfo): string;
var
  LSB: TSSLStringBuilder;
begin
  LSB := TSSLStringBuilder.Create;
  try
    LSB.AppendLine('证书信息:');
    LSB.Indent;
    LSB.AppendFormat('主题: %s', [aInfo.Subject]);
    LSB.AppendFormat('颁发者: %s', [aInfo.Issuer]);
    LSB.AppendFormat('序列号: %s', [aInfo.SerialNumber]);
    LSB.AppendFormat('有效期开始: %s', [DateTimeToStr(aInfo.NotBefore)]);
    LSB.AppendFormat('有效期结束: %s', [DateTimeToStr(aInfo.NotAfter)]);
    LSB.AppendFormat('公钥算法: %s', [aInfo.PublicKeyAlgorithm]);
    LSB.AppendFormat('公钥长度: %d 位', [aInfo.PublicKeySize]);
    LSB.AppendFormat('签名算法: %s', [aInfo.SignatureAlgorithm]);
    LSB.AppendFormat('SHA256指纹: %s', [aInfo.FingerprintSHA256]);
    LSB.AppendFormat('是否CA证书: %s', [BoolToStr(aInfo.IsCA, '是', '否')]);
    
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLUtils.DumpConnectionInfo(const aInfo: TSSLConnectionInfo): string;
var
  LSB: TSSLStringBuilder;
begin
  LSB := TSSLStringBuilder.Create;
  try
    LSB.AppendLine('连接信息:');
    LSB.Indent;
    LSB.AppendFormat('协议版本: %s', [SSL_PROTOCOL_NAMES[aInfo.ProtocolVersion]]);
    LSB.AppendFormat('密码套件: %s', [aInfo.CipherSuite]);
    LSB.AppendFormat('密钥长度: %d 位', [aInfo.KeySize]);
    LSB.AppendFormat('会话ID: %s', [aInfo.SessionId]);
    LSB.AppendFormat('会话复用: %s', [BoolToStr(aInfo.IsResumed, '是', '否')]);
    LSB.AppendFormat('服务器名称: %s', [aInfo.ServerName]);
    LSB.AppendFormat('ALPN协议: %s', [aInfo.ALPNProtocol]);
    
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLUtils.CompareVersions(const aVer1, aVer2: string): Integer;
var
  LParts1, LParts2: TStringArray;
  I, LNum1, LNum2: Integer;
begin
  LParts1 := aVer1.Split(['.']);
  LParts2 := aVer2.Split(['.']);
  
  for I := 0 to Max(High(LParts1), High(LParts2)) do
  begin
    if I <= High(LParts1) then
      LNum1 := StrToIntDef(LParts1[I], 0)
    else
      LNum1 := 0;
    
    if I <= High(LParts2) then
      LNum2 := StrToIntDef(LParts2[I], 0)
    else
      LNum2 := 0;
    
    if LNum1 < LNum2 then
      Exit(-1)
    else if LNum1 > LNum2 then
      Exit(1);
  end;
  
  Result := 0;
end;

class function TSSLUtils.IsVersionSupported(const aVersion: string;
  const aMinVersion: string): Boolean;
begin
  if aMinVersion = '' then
    Result := True
  else
    Result := CompareVersions(aVersion, aMinVersion) >= 0;
end;

{ TSSLMemoryStream }

constructor TSSLMemoryStream.Create(const aName: string; aDebug: Boolean);
begin
  inherited Create;
  FName := aName;
  FDebug := aDebug;
end;

function TSSLMemoryStream.ReadByte: Byte;
begin
  if Read(Result, 1) <> 1 then
    raise Exception.Create('读取字节失败');
end;

function TSSLMemoryStream.ReadWord: Word;
begin
  if Read(Result, 2) <> 2 then
    raise Exception.Create('读取字失败');
end;

function TSSLMemoryStream.ReadDWord: DWord;
begin
  if Read(Result, 4) <> 4 then
    raise Exception.Create('读取双字失败');
end;

function TSSLMemoryStream.ReadBytes(aCount: Integer): TBytes;
begin
  SetLength(Result, aCount);
  if aCount > 0 then
  begin
    if Read(Result[0], aCount) <> aCount then
      raise Exception.Create('读取字节数组失败');
  end;
end;

function TSSLMemoryStream.ReadString(aLength: Integer): string;
var
  LBytes: TBytes;
begin
  LBytes := ReadBytes(aLength);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

procedure TSSLMemoryStream.WriteByte(aValue: Byte);
begin
  Write(aValue, 1);
end;

procedure TSSLMemoryStream.WriteWord(aValue: Word);
begin
  Write(aValue, 2);
end;

procedure TSSLMemoryStream.WriteDWord(aValue: DWord);
begin
  Write(aValue, 4);
end;

procedure TSSLMemoryStream.WriteBytes(const aBytes: TBytes);
begin
  if Length(aBytes) > 0 then
    Write(aBytes[0], Length(aBytes));
end;

procedure TSSLMemoryStream.WriteString(const aStr: string);
var
  LBytes: TBytes;
begin
  LBytes := TEncoding.UTF8.GetBytes(aStr);
  WriteBytes(LBytes);
end;

function TSSLMemoryStream.PeekByte: Byte;
var
  LOldPos: Int64;
begin
  LOldPos := Position;
  try
    Result := ReadByte;
  finally
    Position := LOldPos;
  end;
end;

function TSSLMemoryStream.RemainingBytes: Integer;
begin
  Result := Size - Position;
end;

function TSSLMemoryStream.GetHexDump: string;
var
  LOldPos: Int64;
  LBytes: TBytes;
begin
  LOldPos := Position;
  try
    Position := 0;
    SetLength(LBytes, Size);
    if Size > 0 then
      Read(LBytes[0], Size);
    Result := TSSLUtils.DumpBytes(LBytes);
  finally
    Position := LOldPos;
  end;
end;

{ TSSLStringBuilder }

constructor TSSLStringBuilder.Create;
begin
  inherited;
  FBuffer := TStringList.Create;
  FIndentLevel := 0;
  FIndentStr := '  ';
end;

destructor TSSLStringBuilder.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TSSLStringBuilder.Append(const aStr: string);
begin
  if FBuffer.Count = 0 then
    FBuffer.Add('');
  
  FBuffer[FBuffer.Count - 1] := FBuffer[FBuffer.Count - 1] + aStr;
end;

procedure TSSLStringBuilder.AppendLine(const aStr: string);
var
  LIndent: string;
begin
  if FIndentLevel > 0 then
    LIndent := StringOfChar(' ', Length(FIndentStr) * FIndentLevel)
  else
    LIndent := '';
  
  FBuffer.Add(LIndent + aStr);
end;

procedure TSSLStringBuilder.AppendFormat(const aFormat: string; const aArgs: array of const);
begin
  AppendLine(Format(aFormat, aArgs));
end;

procedure TSSLStringBuilder.Indent;
begin
  Inc(FIndentLevel);
end;

procedure TSSLStringBuilder.Unindent;
begin
  if FIndentLevel > 0 then
    Dec(FIndentLevel);
end;

procedure TSSLStringBuilder.SetIndentStr(const aStr: string);
begin
  FIndentStr := aStr;
end;

procedure TSSLStringBuilder.Clear;
begin
  FBuffer.Clear;
  FIndentLevel := 0;
end;

function TSSLStringBuilder.ToString: string;
begin
  Result := FBuffer.Text;
end;

{ TSSLBitSet }

constructor TSSLBitSet.Create(aSize: Integer);
begin
  inherited Create;
  FSize := aSize;
  SetLength(FBits, (aSize + 7) div 8);
  Clear;
end;

destructor TSSLBitSet.Destroy;
begin
  SetLength(FBits, 0);
  inherited;
end;

function TSSLBitSet.GetBit(aIndex: Integer): Boolean;
begin
  if (aIndex < 0) or (aIndex >= FSize) then
    raise Exception.Create('位索引超出范围');
  
  Result := (FBits[aIndex div 8] and (1 shl (aIndex mod 8))) <> 0;
end;

procedure TSSLBitSet.SetBit(aIndex: Integer; aValue: Boolean);
begin
  if (aIndex < 0) or (aIndex >= FSize) then
    raise Exception.Create('位索引超出范围');
  
  if aValue then
    FBits[aIndex div 8] := FBits[aIndex div 8] or (1 shl (aIndex mod 8))
  else
    FBits[aIndex div 8] := FBits[aIndex div 8] and not (1 shl (aIndex mod 8));
end;

procedure TSSLBitSet.Clear;
begin
  FillChar(FBits[0], Length(FBits), 0);
end;

procedure TSSLBitSet.SetAll;
begin
  FillChar(FBits[0], Length(FBits), $FF);
  
  // 清除最后一个字节中未使用的位
  if FSize mod 8 <> 0 then
    FBits[High(FBits)] := FBits[High(FBits)] and ((1 shl (FSize mod 8)) - 1);
end;

procedure TSSLBitSet.Toggle(aIndex: Integer);
begin
  SetBit(aIndex, not GetBit(aIndex));
end;

function TSSLBitSet.Count: Integer;
var
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to High(FBits) do
  begin
    for J := 0 to 7 do
    begin
      if (FBits[I] and (1 shl J)) <> 0 then
        Inc(Result);
    end;
  end;
end;

function TSSLBitSet.IsEmpty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(FBits) do
  begin
    if FBits[I] <> 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TSSLBitSet.IsFull: Boolean;
begin
  Result := Count = FSize;
end;

function TSSLBitSet.ToBytes: TBytes;
begin
  SetLength(Result, Length(FBits));
  if Length(FBits) > 0 then
    Move(FBits[0], Result[0], Length(FBits));
end;

procedure TSSLBitSet.FromBytes(const aBytes: TBytes);
var
  LLen: Integer;
begin
  LLen := Min(Length(aBytes), Length(FBits));
  if LLen > 0 then
    Move(aBytes[0], FBits[0], LLen);
end;

end.