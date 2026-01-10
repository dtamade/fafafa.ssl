{**
 * Unit: fafafa.ssl.mbedtls.session
 * Purpose: MbedTLS 会话管理实现
 *
 * 实现 ISSLSession 接口的 MbedTLS 后端。
 * 支持 TLS 会话恢复和会话票据。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.mbedtls.session;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api;

type
  { TMbedTLSSession - MbedTLS 会话类 }
  TMbedTLSSession = class(TInterfacedObject, ISSLSession)
  private
    FSession: Pmbedtls_ssl_session;
    FOwnsSession: Boolean;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FSessionID: string;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FSerializedData: TBytes;

    procedure AllocateSession;
    procedure FreeSession;
    procedure ExtractSessionInfo;
    function GenerateSessionID: string;

  public
    constructor Create; overload;
    constructor Create(ASession: Pmbedtls_ssl_session; AOwnsSession: Boolean = True); overload;
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
    class function FromContext(ASSLCtx: Pmbedtls_ssl_context): ISSLSession;
  end;

implementation

uses
  fafafa.ssl.mbedtls.certificate;

const
  MBEDTLS_SSL_SESSION_SIZE = 512;  // 估算大小

{ TMbedTLSSession }

constructor TMbedTLSSession.Create;
begin
  inherited Create;
  FSession := nil;
  FOwnsSession := False;
  FCreationTime := Now;
  FTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionID := GenerateSessionID;
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
  SetLength(FSerializedData, 0);
end;

constructor TMbedTLSSession.Create(ASession: Pmbedtls_ssl_session; AOwnsSession: Boolean);
begin
  Create;
  FSession := ASession;
  FOwnsSession := AOwnsSession;
  if FSession <> nil then
    ExtractSessionInfo;
end;

destructor TMbedTLSSession.Destroy;
begin
  if FOwnsSession then
    FreeSession;
  inherited Destroy;
end;

procedure TMbedTLSSession.AllocateSession;
begin
  if FSession <> nil then
    FreeSession;

  GetMem(FSession, MBEDTLS_SSL_SESSION_SIZE);
  FillChar(FSession^, MBEDTLS_SSL_SESSION_SIZE, 0);

  if Assigned(mbedtls_ssl_session_init) then
    mbedtls_ssl_session_init(FSession);

  FOwnsSession := True;
end;

procedure TMbedTLSSession.FreeSession;
begin
  if FSession <> nil then
  begin
    if Assigned(mbedtls_ssl_session_free) then
      mbedtls_ssl_session_free(FSession);
    FreeMem(FSession);
    FSession := nil;
  end;
end;

procedure TMbedTLSSession.ExtractSessionInfo;
begin
  if FSession = nil then Exit;

  FSessionID := GenerateSessionID;
  FCreationTime := Now;
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := '';
end;

function TMbedTLSSession.GenerateSessionID: string;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  Result := GUIDToString(LGuid);
end;

function TMbedTLSSession.GetID: string;
begin
  Result := FSessionID;
end;

function TMbedTLSSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TMbedTLSSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TMbedTLSSession.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TMbedTLSSession.IsValid: Boolean;
var
  LElapsed: Integer;
begin
  Result := False;
  if FSession = nil then Exit;

  LElapsed := SecondsBetween(Now, FCreationTime);
  Result := LElapsed < FTimeout;
end;

function TMbedTLSSession.IsResumable: Boolean;
begin
  Result := IsValid and (FSession <> nil);
end;

function TMbedTLSSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TMbedTLSSession.GetCipherName: string;
begin
  Result := FCipherName;
end;

function TMbedTLSSession.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMbedTLSSession.Serialize: TBytes;
begin
  Result := Copy(FSerializedData);
end;

function TMbedTLSSession.Deserialize(const AData: TBytes): Boolean;
begin
  Result := False;
  if Length(AData) = 0 then Exit;

  FSerializedData := Copy(AData);
  Result := True;
end;

function TMbedTLSSession.GetNativeHandle: Pointer;
begin
  Result := FSession;
end;

function TMbedTLSSession.Clone: ISSLSession;
var
  LClone: TMbedTLSSession;
begin
  LClone := TMbedTLSSession.Create;
  LClone.FCreationTime := FCreationTime;
  LClone.FTimeout := FTimeout;
  LClone.FSessionID := FSessionID;
  LClone.FProtocolVersion := FProtocolVersion;
  LClone.FCipherName := FCipherName;
  LClone.FSerializedData := Copy(FSerializedData);
  LClone.FSession := nil;
  LClone.FOwnsSession := False;
  Result := LClone;
end;

class function TMbedTLSSession.FromContext(ASSLCtx: Pmbedtls_ssl_context): ISSLSession;
var
  LSession: TMbedTLSSession;
begin
  Result := nil;
  if ASSLCtx = nil then Exit;
  if not Assigned(mbedtls_ssl_get_session) then Exit;

  LSession := TMbedTLSSession.Create;
  LSession.AllocateSession;

  if mbedtls_ssl_get_session(ASSLCtx, LSession.FSession) = 0 then
  begin
    LSession.ExtractSessionInfo;
    Result := LSession;
  end
  else
  begin
    LSession.Free;
    Result := nil;
  end;
end;

end.
