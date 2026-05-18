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
  fafafa.ssl.mbedtls.native_handle,
  fafafa.ssl.mbedtls.api;

type
  { TMbedTLSSession - MbedTLS 会话类 }
  TMbedTLSSession = class(TInterfacedObject, ISSLSession, ISSLNativeHandleAccess)
  private
    FSession: Pmbedtls_ssl_session;
    FOwnsSession: Boolean;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FSessionID: string;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FPeerCertificate: ISSLCertificate;
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

    { ISSLNativeHandleAccess implementation }
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;

    function Clone: ISSLSession;

    { 额外方法 }
    class function FromContext(ASSLCtx: Pmbedtls_ssl_context): ISSLSession;
  end;

implementation

uses
  fafafa.ssl.mbedtls.certificate;

const
  MBEDTLS_SSL_SESSION_SIZE = 512;  // 估算大小
  MBEDTLS_ERR_SSL_BUFFER_TOO_SMALL_LOCAL = -$6A00;

function ParseMbedTLSVersionString(const AVersion: string): TSSLProtocolVersion;
begin
  if Pos('TLSv1.3', AVersion) > 0 then
    Exit(sslProtocolTLS13);
  if Pos('TLSv1.2', AVersion) > 0 then
    Exit(sslProtocolTLS12);
  if Pos('TLSv1.1', AVersion) > 0 then
    Exit(sslProtocolTLS11);
  if Pos('TLSv1.0', AVersion) > 0 then
    Exit(sslProtocolTLS10);
  if Pos('SSLv3', AVersion) > 0 then
    Exit(sslProtocolSSL3);
  Result := sslProtocolUnknown;
end;

function HasSessionSerializeHelpers: Boolean;
begin
  Result := Assigned(mbedtls_ssl_session_save);
end;

function HasSessionDeserializeHelpers: Boolean;
begin
  Result := Assigned(mbedtls_ssl_session_load);
end;

function MaterializeMbedTLSPeerCertificate(ACert: Pmbedtls_x509_crt): ISSLCertificate;
var
  LDER: TBytes;
  LTemp: TMbedTLSCertificate;
  LOwned: TMbedTLSCertificate;
begin
  Result := nil;
  if ACert = nil then
    Exit;

  LTemp := TMbedTLSCertificate.Create(ACert, False);
  try
    LDER := LTemp.SaveToDER;
    if Length(LDER) = 0 then
      Exit;

    LOwned := TMbedTLSCertificate.Create;
    try
      if not LOwned.LoadFromDER(LDER) then
        Exit;
      Result := LOwned;
      LOwned := nil;
    finally
      LOwned.Free;
    end;
  finally
    LTemp.Free;
  end;
end;

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
  FPeerCertificate := nil;
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
  if FPeerCertificate <> nil then
    Result := FPeerCertificate.Clone
  else
    Result := nil;
end;

function TMbedTLSSession.Serialize: TBytes;
var
  LRequiredSize: NativeUInt;
  LResultCode: Integer;
begin
  if (Length(FSerializedData) > 0) and
     ((FSession = nil) or not HasSessionSerializeHelpers()) then
    Exit(Copy(FSerializedData));

  SetLength(Result, 0);
  if (FSession = nil) or not HasSessionSerializeHelpers() then
    Exit;

  LRequiredSize := 0;
  LResultCode := mbedtls_ssl_session_save(FSession, nil, 0, @LRequiredSize);
  if (LResultCode <> 0) and
     (LResultCode <> MBEDTLS_ERR_SSL_BUFFER_TOO_SMALL_LOCAL) then
    Exit;

  if LRequiredSize = 0 then
    Exit;

  SetLength(Result, LRequiredSize);
  LResultCode := mbedtls_ssl_session_save(FSession, @Result[0],
    Length(Result), @LRequiredSize);
  if LResultCode <> 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(Result, LRequiredSize);
  FSerializedData := Copy(Result);
end;

function TMbedTLSSession.Deserialize(const AData: TBytes): Boolean;
var
  LSession: Pmbedtls_ssl_session;
begin
  Result := False;
  if (Length(AData) = 0) or not HasSessionDeserializeHelpers() then
    Exit;

  LSession := nil;
  GetMem(LSession, MBEDTLS_SSL_SESSION_SIZE);
  FillChar(LSession^, MBEDTLS_SSL_SESSION_SIZE, 0);

  if Assigned(mbedtls_ssl_session_init) then
    mbedtls_ssl_session_init(LSession);

  if mbedtls_ssl_session_load(LSession, @AData[0], Length(AData)) <> 0 then
  begin
    if Assigned(mbedtls_ssl_session_free) then
      mbedtls_ssl_session_free(LSession);
    FreeMem(LSession);
    Exit;
  end;

  if FOwnsSession then
    FreeSession;

  FSession := LSession;
  FOwnsSession := True;
  FSerializedData := Copy(AData);
  ExtractSessionInfo;
  FPeerCertificate := nil;
  Result := True;
end;

function TMbedTLSSession.GetNativeHandle: Pointer;
begin
  Result := FSession;
end;

function TMbedTLSSession.GetBackendType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TMbedTLSSession.IsNativeHandleValid: Boolean;
begin
  Result := (FSession <> nil);
end;

function TMbedTLSSession.Clone: ISSLSession;
var
  LClone: TMbedTLSSession;
  LSerialized: TBytes;
begin
  Result := nil;
  LClone := TMbedTLSSession.Create;
  try
    LClone.FCreationTime := FCreationTime;
    LClone.FTimeout := FTimeout;
    LClone.FSessionID := FSessionID;
    LClone.FProtocolVersion := FProtocolVersion;
    LClone.FCipherName := FCipherName;
    if FPeerCertificate <> nil then
      LClone.FPeerCertificate := FPeerCertificate.Clone
    else
      LClone.FPeerCertificate := nil;
    LClone.FSerializedData := Copy(FSerializedData);

    if FSession <> nil then
    begin
      LSerialized := Serialize;
      if (Length(LSerialized) = 0) or (not LClone.Deserialize(LSerialized)) then
        Exit(nil);

      LClone.FCreationTime := FCreationTime;
      LClone.FTimeout := FTimeout;
      LClone.FSessionID := FSessionID;
      LClone.FProtocolVersion := FProtocolVersion;
      LClone.FCipherName := FCipherName;
      if FPeerCertificate <> nil then
        LClone.FPeerCertificate := FPeerCertificate.Clone
      else
        LClone.FPeerCertificate := nil;
      LClone.FSerializedData := Copy(LSerialized);
    end;

    Result := LClone;
    LClone := nil;
  finally
    LClone.Free;
  end;
end;

class function TMbedTLSSession.FromContext(ASSLCtx: Pmbedtls_ssl_context): ISSLSession;
var
  LSession: TMbedTLSSession;
  LVersion: PAnsiChar;
  LCipherName: PAnsiChar;
  LPeerCert: Pmbedtls_x509_crt;
  LParsedVersion: TSSLProtocolVersion;
begin
  Result := nil;
  if ASSLCtx = nil then Exit;
  if not Assigned(mbedtls_ssl_get_session) then Exit;

  LSession := TMbedTLSSession.Create;
  LSession.AllocateSession;

  if mbedtls_ssl_get_session(ASSLCtx, LSession.FSession) = 0 then
  begin
    LSession.ExtractSessionInfo;

    if Assigned(mbedtls_ssl_get_version) then
    begin
      LVersion := mbedtls_ssl_get_version(ASSLCtx);
      if LVersion <> nil then
      begin
        LParsedVersion := ParseMbedTLSVersionString(string(LVersion));
        if LParsedVersion <> sslProtocolUnknown then
          LSession.FProtocolVersion := LParsedVersion;
      end;
    end;

    if Assigned(mbedtls_ssl_get_ciphersuite) then
    begin
      LCipherName := mbedtls_ssl_get_ciphersuite(ASSLCtx);
      if LCipherName <> nil then
        LSession.FCipherName := string(LCipherName);
    end;

    if Assigned(mbedtls_ssl_get_peer_cert) then
    begin
      LPeerCert := mbedtls_ssl_get_peer_cert(ASSLCtx);
      if LPeerCert <> nil then
        LSession.FPeerCertificate := MaterializeMbedTLSPeerCertificate(LPeerCert);
    end;

    Result := LSession;
  end
  else
  begin
    LSession.Free;
    Result := nil;
  end;
end;

end.
