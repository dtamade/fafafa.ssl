unit fafafa.ssl.debug.utils;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{**
 * Unit: fafafa.ssl.debug.utils
 * Purpose: SSL/TLS 调试和诊断工具
 *
 * Phase 2.3.2: Utils模块重组
 * Phase 2.4: 错误处理标准化（2025-01-18）
 *
 * 本模块从 fafafa.ssl.utils 中提取调试相关功能，
 * 包括数据转储、辅助类（TSSLMemoryStream、TSSLStringBuilder、TSSLBitSet）。
 *
 * Features:
 * - 字节数组/配置/证书/连接信息的可读格式转储
 * - TSSLMemoryStream: 带调试功能的内存流
 * - TSSLStringBuilder: 带缩进的字符串构建器
 * - TSSLBitSet: 位集合操作
 *
 * Error Handling (Phase 2.4):
 * - Stream I/O errors: ESSLResourceException with context
 * - Invalid arguments: ESSLInvalidArgument with range details
 *
 * Thread Safety: TSSLUtils 类方法线程安全，辅助类非线程安全
 *
 * @author fafafa.ssl team
 * @version 1.1.0
 * @since 2025-12-16
 *}

interface

uses
  SysUtils, Classes, Math,
  fafafa.ssl.base,
  fafafa.ssl.exceptions;

type
  { TSSLDebugUtils - 调试工具类 }
  TSSLDebugUtils = class
  public
    {**
     * 将字节数组转储为十六进制+ASCII格式
     *
     * @param aBytes 要转储的字节数组
     * @param aBytesPerLine 每行显示的字节数（默认16）
     * @return 格式化的十六进制转储字符串
     *
     * @example
     * <code>
     *   WriteLn(TSSLDebugUtils.DumpBytes(LData));
     *   // 输出:
     *   // 00000000  48 65 6C 6C 6F 20 57 6F 72 6C 64 21 00 00 00 00  Hello World!....
     * </code>
     *}
    class function DumpBytes(const aBytes: TBytes; aBytesPerLine: Integer = 16): string;

    {**
     * 转储 SSL 配置信息
     *}
    class function DumpSSLConfig(const aConfig: TSSLConfig): string;

    {**
     * 转储证书信息
     *}
    class function DumpCertificateInfo(const aInfo: TSSLCertificateInfo): string;

    {**
     * 转储连接信息
     *}
    class function DumpConnectionInfo(const aInfo: TSSLConnectionInfo): string;
  end;

  {**
   * TSSLMemoryStream - 带位置跟踪和调试功能的内存流
   *
   * 扩展 TMemoryStream，提供便捷的二进制读写方法和十六进制转储。
   * 用于 SSL/TLS 消息的调试和解析。
   *}
  TSSLMemoryStream = class(TMemoryStream)
  private
    FName: string;
    FDebug: Boolean;
  public
    constructor Create(const aName: string = ''; aDebug: Boolean = False);

    { 二进制读取 }
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: DWord;
    function ReadBytes(aCount: Integer): TBytes;
    function ReadString(aLength: Integer): string;

    { 二进制写入 }
    procedure WriteByte(aValue: Byte);
    procedure WriteWord(aValue: Word);
    procedure WriteDWord(aValue: DWord);
    procedure WriteBytes(const aBytes: TBytes);
    procedure WriteString(const aStr: string);

    { 调试工具 }
    function PeekByte: Byte;
    function RemainingBytes: Integer;
    function GetHexDump: string;

    property Name: string read FName write FName;
    property Debug: Boolean read FDebug write FDebug;
  end;

  {**
   * TSSLStringBuilder - 带缩进支持的字符串构建器
   *
   * 用于生成格式化的调试输出和日志信息。
   *}
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

  {**
   * TSSLBitSet - 位集合操作
   *
   * 用于 SSL/TLS 特性标志位、密码套件掩码等场景。
   *}
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

implementation

{ TSSLDebugUtils }

class function TSSLDebugUtils.DumpBytes(const aBytes: TBytes; aBytesPerLine: Integer): string;
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

class function TSSLDebugUtils.DumpSSLConfig(const aConfig: TSSLConfig): string;
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
    LSB.AppendFormat('私钥口令: %s', [
      BoolToStr(aConfig.PrivateKeyPassword <> '', '已设置', '未设置')
    ]);
    LSB.AppendFormat('CA文件: %s', [aConfig.CAFile]);
    LSB.AppendFormat('缓冲区大小: %d', [aConfig.BufferSize]);
    LSB.AppendFormat('握手超时: %d ms', [aConfig.HandshakeTimeout]);
    LSB.AppendFormat('服务器名称: %s', [aConfig.ServerName]);

    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLDebugUtils.DumpCertificateInfo(
  const aInfo: TSSLCertificateInfo): string;
var
  LSB: TSSLStringBuilder;
  SAN: string;
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

    if Length(aInfo.SubjectAltNames) > 0 then
    begin
      LSB.AppendLine('主题备用名称:');
      LSB.Indent;
      for SAN in aInfo.SubjectAltNames do
        LSB.AppendFormat('- %s', [SAN]);
      LSB.Unindent;
    end;

    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

class function TSSLDebugUtils.DumpConnectionInfo(const aInfo: TSSLConnectionInfo): string;
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
    raise ESSLResourceException.CreateWithContext(
      'Failed to read byte from stream',
      sslErrInvalidData,
      'TSSLMemoryStream.ReadByte'
    );
end;

function TSSLMemoryStream.ReadWord: Word;
begin
  if Read(Result, 2) <> 2 then
    raise ESSLResourceException.CreateWithContext(
      'Failed to read word from stream',
      sslErrInvalidData,
      'TSSLMemoryStream.ReadWord'
    );
end;

function TSSLMemoryStream.ReadDWord: DWord;
begin
  if Read(Result, 4) <> 4 then
    raise ESSLResourceException.CreateWithContext(
      'Failed to read dword from stream',
      sslErrInvalidData,
      'TSSLMemoryStream.ReadDWord'
    );
end;

function TSSLMemoryStream.ReadBytes(aCount: Integer): TBytes;
begin
  Result := nil;
  SetLength(Result, aCount);
  if aCount > 0 then
  begin
    if Read(Result[0], aCount) <> aCount then
      raise ESSLResourceException.CreateWithContext(
        Format('Failed to read %d bytes from stream', [aCount]),
        sslErrInvalidData,
        'TSSLMemoryStream.ReadBytes'
      );
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
    Result := TSSLDebugUtils.DumpBytes(LBytes);
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
    raise ESSLInvalidArgument.CreateWithContext(
      Format('Bit index %d out of range [0..%d)', [aIndex, FSize]),
      sslErrInvalidParam,
      'TSSLBitSet.GetBit'
    );

  Result := (FBits[aIndex div 8] and (1 shl (aIndex mod 8))) <> 0;
end;

procedure TSSLBitSet.SetBit(aIndex: Integer; aValue: Boolean);
begin
  if (aIndex < 0) or (aIndex >= FSize) then
    raise ESSLInvalidArgument.CreateWithContext(
      Format('Bit index %d out of range [0..%d)', [aIndex, FSize]),
      sslErrInvalidParam,
      'TSSLBitSet.SetBit'
    );

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
  Result := nil;
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
