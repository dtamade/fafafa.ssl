{**
 * Unit: fafafa.ssl.wolfssl.session
 * Purpose: WolfSSL 会话管理实现
 *
 * 实现 ISSLSession 接口的 WolfSSL 后端。
 * 支持 TLS 会话恢复和会话票据。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.wolfssl.session;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.api;

type
  { TWolfSSLSession - WolfSSL 会话类 }
  TWolfSSLSession = class(TInterfacedObject, ISSLSession)
  private
    FSession: PWOLFSSL_SESSION;
    FOwnsSession: Boolean;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FSessionID: string;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FSerializedData: TBytes;

    procedure ExtractSessionInfo;
    function GenerateSessionID: string;

  public
    constructor Create; overload;
    constructor Create(ASession: PWOLFSSL_SESSION; AOwnsSession: Boolean = True); overload;
    destructor Destroy; override;

    { ISSLSession - 会话信息 }
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(ATimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;

    { ISSLSession - 会话属性 }
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;

    { ISSLSession - 序列化 }
    function Serialize: TBytes;
    function Deserialize(const AData: TBytes): Boolean;

    { ISSLSession - 原生句柄 }
    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;

    { 额外方法 }
    class function FromConnection(ASSL: PWOLFSSL): ISSLSession;
  end;

implementation

uses
  fafafa.ssl.wolfssl.certificate;

{ TWolfSSLSession }

constructor TWolfSSLSession.Create;
begin
  inherited Create;
  FSession := nil;
  FOwnsSession := False;
  FCreationTime := Now;
  FTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionID := GenerateSessionID;  // 总是生成会话 ID
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
  SetLength(FSerializedData, 0);
end;

constructor TWolfSSLSession.Create(ASession: PWOLFSSL_SESSION; AOwnsSession: Boolean);
begin
  Create;
  FSession := ASession;
  FOwnsSession := AOwnsSession;
  if FSession <> nil then
    ExtractSessionInfo;
end;

destructor TWolfSSLSession.Destroy;
begin
  if FOwnsSession and (FSession <> nil) then
  begin
    if Assigned(wolfSSL_SESSION_free) then
      wolfSSL_SESSION_free(FSession);
    FSession := nil;
  end;
  inherited Destroy;
end;

procedure TWolfSSLSession.ExtractSessionInfo;
begin
  if FSession = nil then Exit;

  // 生成会话 ID
  FSessionID := GenerateSessionID;
  FCreationTime := Now;

  // WolfSSL 会话信息提取需要额外 API
  // 目前使用默认值
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
end;

function TWolfSSLSession.GenerateSessionID: string;
var
  LGuid: TGUID;
begin
  // 生成唯一会话标识符
  CreateGUID(LGuid);
  Result := GUIDToString(LGuid);
end;

function TWolfSSLSession.GetID: string;
begin
  Result := FSessionID;
end;

function TWolfSSLSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TWolfSSLSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWolfSSLSession.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TWolfSSLSession.IsValid: Boolean;
var
  LElapsed: Integer;
begin
  Result := False;
  if FSession = nil then Exit;

  // 检查会话是否过期
  LElapsed := SecondsBetween(Now, FCreationTime);
  Result := LElapsed < FTimeout;
end;

function TWolfSSLSession.IsResumable: Boolean;
begin
  Result := IsValid and (FSession <> nil);
end;

function TWolfSSLSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TWolfSSLSession.GetCipherName: string;
begin
  Result := FCipherName;
end;

function TWolfSSLSession.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
  // WolfSSL 会话不直接存储对端证书
end;

function TWolfSSLSession.Serialize: TBytes;
var
  LLen: Integer;
  LBuf: PByte;
  LBufPtr: PByte;
begin
  SetLength(Result, 0);

  // 如果有缓存的序列化数据，直接返回
  if Length(FSerializedData) > 0 then
  begin
    Result := Copy(FSerializedData);
    Exit;
  end;

  if FSession = nil then Exit;

  // 使用 WolfSSL 的 i2d 函数序列化会话
  if Assigned(wolfSSL_i2d_SSL_SESSION) then
  begin
    // 首先获取所需的缓冲区大小
    LLen := wolfSSL_i2d_SSL_SESSION(FSession, nil);
    if LLen > 0 then
    begin
      SetLength(Result, LLen);
      LBufPtr := @Result[0];
      LLen := wolfSSL_i2d_SSL_SESSION(FSession, @LBufPtr);
      if LLen <= 0 then
        SetLength(Result, 0)
      else
      begin
        SetLength(Result, LLen);
        FSerializedData := Copy(Result);  // 缓存序列化数据
      end;
    end;
  end;
end;

function TWolfSSLSession.Deserialize(const AData: TBytes): Boolean;
var
  LDataPtr: PByte;
  LSession: PWOLFSSL_SESSION;
begin
  Result := False;
  if Length(AData) = 0 then Exit;

  // 保存序列化数据以供后续使用
  FSerializedData := Copy(AData);

  // 使用 WolfSSL 的 d2i 函数反序列化会话
  if Assigned(wolfSSL_d2i_SSL_SESSION) then
  begin
    // 释放旧会话
    if FOwnsSession and (FSession <> nil) then
    begin
      if Assigned(wolfSSL_SESSION_free) then
        wolfSSL_SESSION_free(FSession);
      FSession := nil;
    end;

    LDataPtr := @AData[0];
    LSession := wolfSSL_d2i_SSL_SESSION(nil, @LDataPtr, Length(AData));
    if LSession <> nil then
    begin
      FSession := LSession;
      FOwnsSession := True;
      ExtractSessionInfo;
      Result := True;
    end;
  end
  else
  begin
    // 如果没有反序列化 API，仅保存数据
    Result := True;
  end;
end;

function TWolfSSLSession.GetNativeHandle: Pointer;
begin
  Result := FSession;
end;

function TWolfSSLSession.Clone: ISSLSession;
var
  LClone: TWolfSSLSession;
begin
  LClone := TWolfSSLSession.Create;
  LClone.FCreationTime := FCreationTime;
  LClone.FTimeout := FTimeout;
  LClone.FSessionID := FSessionID;
  LClone.FProtocolVersion := FProtocolVersion;
  LClone.FCipherName := FCipherName;
  LClone.FSerializedData := Copy(FSerializedData);
  // 注意：不复制原生会话句柄，克隆不拥有会话
  LClone.FSession := nil;
  LClone.FOwnsSession := False;
  Result := LClone;
end;

class function TWolfSSLSession.FromConnection(ASSL: PWOLFSSL): ISSLSession;
var
  LSession: PWOLFSSL_SESSION;
begin
  Result := nil;
  if ASSL = nil then Exit;
  if not Assigned(wolfSSL_get_session) then Exit;

  LSession := wolfSSL_get_session(ASSL);
  if LSession <> nil then
    Result := TWolfSSLSession.Create(LSession, False);  // 不拥有会话
end;

end.
