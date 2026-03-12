{**
 * Unit: fafafa.ssl.winssl.session
 * Purpose: WinSSL (Schannel) 会话实现
 *
 * 实现 ISSLSession 接口的 WinSSL 后端。
 * Windows Schannel 会话管理与 OpenSSL 不同，
 * 使用 SecHandle 和凭据缓存机制。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-02-04
 *}

unit fafafa.ssl.winssl.session;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api;

type
  { TWinSSLSession - WinSSL 会话类 }
  TWinSSLSession = class(TInterfacedObject, ISSLSession)
  private
    FSessionData: TBytes;
    FSessionID: string;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FIsValid: Boolean;

  public
    constructor Create;
    constructor CreateFromData(const AData: TBytes);
    constructor CreateFromConnection(AContext: PCtxtHandle;
      AProtocol: TSSLProtocolVersion; const ACipher: string);
    destructor Destroy; override;

    { ISSLSession 接口 }
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(ATimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function Serialize: TBytes;
    function Deserialize(const AData: TBytes): Boolean;
    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;
  end;

implementation

uses
  fafafa.ssl.winssl.certificate;

const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
  DEFAULT_SESSION_TIMEOUT = 300; // 5 minutes

{ TWinSSLSession }

constructor TWinSSLSession.Create;
begin
  inherited Create;
  SetLength(FSessionData, 0);
  FSessionID := '';
  FCreationTime := Now;
  FTimeout := DEFAULT_SESSION_TIMEOUT;
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
  FIsValid := False;
end;

constructor TWinSSLSession.CreateFromData(const AData: TBytes);
begin
  Create;
  Deserialize(AData);
end;

constructor TWinSSLSession.CreateFromConnection(AContext: PCtxtHandle;
  AProtocol: TSSLProtocolVersion; const ACipher: string);
var
  LSecStatus: SECURITY_STATUS;
  LSessionInfo: SecPkgContext_SessionInfo;
  I: Integer;
begin
  Create;

  if AContext = nil then Exit;

  FProtocolVersion := AProtocol;
  FCipherName := ACipher;
  FCreationTime := Now;
  FIsValid := True;

  // 尝试获取会话信息
  if Assigned(QueryContextAttributesW) then
  begin
    FillChar(LSessionInfo, SizeOf(LSessionInfo), 0);
    LSecStatus := QueryContextAttributesW(AContext,
      SECPKG_ATTR_SESSION_INFO, @LSessionInfo);

    if LSecStatus = SEC_E_OK then
    begin
      // 生成会话 ID
      if LSessionInfo.cbSessionId > 0 then
      begin
        SetLength(FSessionID, LSessionInfo.cbSessionId * 2);
        for I := 0 to LSessionInfo.cbSessionId - 1 do
        begin
          FSessionID[I * 2 + 1] := HexDigits[(LSessionInfo.rgbSessionId[I] shr 4) and $0F];
          FSessionID[I * 2 + 2] := HexDigits[LSessionInfo.rgbSessionId[I] and $0F];
        end;
      end;
    end;
  end;

  // 如果没有会话 ID，生成一个基于时间的伪 ID
  if FSessionID = '' then
    FSessionID := IntToHex(Int64(FCreationTime * 86400000), 16);
end;

destructor TWinSSLSession.Destroy;
begin
  SetLength(FSessionData, 0);
  inherited Destroy;
end;

function TWinSSLSession.GetID: string;
begin
  Result := FSessionID;
end;

function TWinSSLSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TWinSSLSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWinSSLSession.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TWinSSLSession.IsValid: Boolean;
var
  LElapsed: TDateTime;
begin
  if not FIsValid then
    Exit(False);

  // 检查是否超时
  LElapsed := Now - FCreationTime;
  Result := (LElapsed * 86400) < FTimeout; // 转换为秒比较
end;

function TWinSSLSession.IsResumable: Boolean;
begin
  // Schannel 会话复用由系统管理，我们只检查有效性
  Result := IsValid;
end;

function TWinSSLSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TWinSSLSession.GetCipherName: string;
begin
  Result := FCipherName;
end;

function TWinSSLSession.GetPeerCertificate: ISSLCertificate;
begin
  // 会话不存储对等证书，返回 nil
  Result := nil;
end;

function TWinSSLSession.Serialize: TBytes;
var
  LStream: TMemoryStream;
  LIDLen: Integer;
  LCipherLen: Integer;
begin
  SetLength(Result, 0);

  LStream := TMemoryStream.Create;
  try
    // 写入版本标记
    LStream.WriteWord($0001);

    // 写入会话 ID
    LIDLen := Length(FSessionID);
    LStream.WriteDWord(LIDLen);
    if LIDLen > 0 then
      LStream.WriteBuffer(FSessionID[1], LIDLen);

    // 写入创建时间
    LStream.WriteBuffer(FCreationTime, SizeOf(TDateTime));

    // 写入超时
    LStream.WriteDWord(FTimeout);

    // 写入协议版本
    LStream.WriteByte(Ord(FProtocolVersion));

    // 写入密码套件名称
    LCipherLen := Length(FCipherName);
    LStream.WriteDWord(LCipherLen);
    if LCipherLen > 0 then
      LStream.WriteBuffer(FCipherName[1], LCipherLen);

    // 写入有效标志
    LStream.WriteByte(Ord(FIsValid));

    // 复制结果
    SetLength(Result, LStream.Size);
    LStream.Position := 0;
    LStream.ReadBuffer(Result[0], LStream.Size);
  finally
    LStream.Free;
  end;
end;

function TWinSSLSession.Deserialize(const AData: TBytes): Boolean;
var
  LStream: TMemoryStream;
  LVersion: Word;
  LIDLen: Integer;
  LCipherLen: Integer;
begin
  Result := False;

  if Length(AData) < 10 then Exit;

  LStream := TMemoryStream.Create;
  try
    LStream.WriteBuffer(AData[0], Length(AData));
    LStream.Position := 0;

    // 读取版本标记
    LVersion := LStream.ReadWord;
    if LVersion <> $0001 then Exit;

    // 读取会话 ID
    LIDLen := LStream.ReadDWord;
    if LIDLen > 0 then
    begin
      SetLength(FSessionID, LIDLen);
      LStream.ReadBuffer(FSessionID[1], LIDLen);
    end
    else
      FSessionID := '';

    // 读取创建时间
    LStream.ReadBuffer(FCreationTime, SizeOf(TDateTime));

    // 读取超时
    FTimeout := LStream.ReadDWord;

    // 读取协议版本
    FProtocolVersion := TSSLProtocolVersion(LStream.ReadByte);

    // 读取密码套件名称
    LCipherLen := LStream.ReadDWord;
    if LCipherLen > 0 then
    begin
      SetLength(FCipherName, LCipherLen);
      LStream.ReadBuffer(FCipherName[1], LCipherLen);
    end
    else
      FCipherName := '';

    // 读取有效标志
    FIsValid := Boolean(LStream.ReadByte);

    Result := True;
  finally
    LStream.Free;
  end;
end;

function TWinSSLSession.GetNativeHandle: Pointer;
begin
  // WinSSL 会话没有独立的原生句柄
  // 会话由 Schannel 内部管理
  Result := nil;
end;

function TWinSSLSession.Clone: ISSLSession;
var
  LNew: TWinSSLSession;
begin
  LNew := TWinSSLSession.Create;
  LNew.FSessionID := FSessionID;
  LNew.FCreationTime := FCreationTime;
  LNew.FTimeout := FTimeout;
  LNew.FProtocolVersion := FProtocolVersion;
  LNew.FCipherName := FCipherName;
  LNew.FIsValid := FIsValid;

  SetLength(LNew.FSessionData, Length(FSessionData));
  if Length(FSessionData) > 0 then
    Move(FSessionData[0], LNew.FSessionData[0], Length(FSessionData));

  Result := LNew;
end;

end.
