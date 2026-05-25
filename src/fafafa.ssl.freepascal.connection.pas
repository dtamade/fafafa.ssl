{**
 * Unit: fafafa.ssl.freepascal.connection
 * Purpose: 纯 FreePascal 后端连接实现（TLS 1.3 客户端握手探测骨架）
 *
 * 当前能力：
 * - 基于 socket/stream 的双向字节 I/O
 * - 发送真实 TLS 1.3 ClientHello
 * - 接收并解析 ServerHello
 * - 处理加密握手记录并校验 Server Finished
 * - 发送加密 Client Finished
 * - 派生应用流量密钥并实现应用数据记录收发（AES-128-GCM/CHACHA20-POLY1305）
 *
 * 当前限制：
 * - PSK / 会话复用等高级能力待补齐
 * - 对端证书验证链等高级能力待补齐
 *}

unit fafafa.ssl.freepascal.connection;

{$mode ObjFPC}{$H+}
{$WARN 5093 off} // Suppress false-positive "Function result not initialized" for managed types
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}
  Windows, Winsock2,
  {$ELSE}
  Sockets,
  {$ENDIF}
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.connection.base,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.keyschedule,
  fafafa.ssl.tls13.appschedule,
  fafafa.ssl.tls13.posthandshake,
  fafafa.ssl.x509;

type
  TFreePascalConnection = class(TBaseSSLConnection, ISSLClientConnection,
    ISSLEarlyDataConnection, ISSLOCSPStapling, ISSLCertificateTransparency,
    ISSLCertificateTransparencyValidation)
  private
    FSocket: THandle;
    FStream: TStream;
    FServerName: string;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FALPNProtocols: string;
    FSelectedALPNProtocol: string;
    FX25519PrivateKey: TBytes;
    FX25519PublicKey: TBytes;
    FHandshakeSharedSecret: TBytes;
    FEarlyDataSecrets: TTLS13EarlyDataSecrets;
    FHandshakeSecrets: TTLS13HandshakeSecrets;
    FServerFinishedKey: TBytes;
    FClientFinishedKey: TBytes;
    FEarlyDataSeq: QWord;
    FServerHandshakeSeq: QWord;
    FClientHandshakeSeq: QWord;

    FApplicationSecrets: TTLS13ApplicationSecrets;
    FClientApplicationSeq: QWord;
    FServerApplicationSeq: QWord;
    FApplicationReadBuffer: TBytes;
    FPostHandshakeBuffer: TBytes;
    FSessionTicketCount: Integer;
    FLastSessionTicket: TTLS13NewSessionTicket;
    FIsServerMode: Boolean;
    FCurrentSession: ISSLSession;
    FConfiguredSession: ISSLSession;
    FSessionReused: Boolean;
    FSessionBoundServerName: string;
    FPeerCertificate: ISSLCertificate;
    FPeerCertificateChain: TSSLCertificateArray;
    FOCSPResponse: TBytes;
    FOCSPResponseVerified: Boolean;
    FOCSPResponseStatus: string;
    FSignedCertificateTimestampList: TBytes;
    FSignedCertificateTimestampCount: Integer;
    FCertificateTransparencyStatus: string;
    FHasCertificateTransparencyValidationResult: Boolean;
    FCertificateTransparencyPolicySatisfied: Boolean;
    FCertificateTransparencyValidationStatus: string;
    FEarlyDataStatus: TSSLEarlyDataStatus;
    FEarlyDataLimit: Cardinal;
    FEarlyDataPayload: TBytes;

    function SendData(const ABuffer; ASize: Integer): Integer;
    function RecvData(var ABuffer; ASize: Integer): Integer;
    function SendAll(const AData: TBytes): Boolean;
    function RecvExact(var AData: TBytes; ACount: Integer): Boolean;
    function RecvTLSRecord(out AHeader: TTLSRecordHeader; out APayload, ARecord: TBytes): Boolean;
    function ProbeServerHello: Boolean;
    procedure SetHandshakeError(ACode: TSSLErrorCode; const AMessage: string);
    procedure AppendHandshakeBytes(var ADest: TBytes; const ASource: TBytes);
    function TryPopHandshakeMessage(var ABuffer: TBytes; out AMessage: TBytes): Boolean;
    function ProcessEncryptedServerFlight(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
    function SendClientFinished(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
    function RecvApplicationDataFragment(
      out AFragment: TBytes;
      AAllowNoRecord: Boolean = False
    ): Boolean;
    function SendApplicationDataFragment(const AFragment: TBytes): Boolean;
    function ProcessPostHandshakeFragment(const AHandshakeFragment: TBytes): Boolean;
    function SendPostHandshakeKeyUpdate(ARequestPeerUpdate: Boolean): Boolean;
    procedure MarkUnsupported(const AOperation: string);
    procedure MarkPrecondition(const AOperation: string);
    function SendClientEarlyDataRecord(ACipherSuite: Word): Boolean;
    function GetBufferedStreamBytesAvailable: Int64;
    function DrainBufferedApplicationRecords: Boolean;
    procedure ClearOCSPStaplingState;
    procedure ClearCertificateTransparencyState;
    procedure RefreshCertificateTransparencyValidationState;
    procedure ClearPeerCertificateCache;
    function TryCachePeerCertificatesFromHandshake(
      const AHandshakeMessage: TBytes;
      ACertificateTransparencyRequested: Boolean;
      out AError: string
    ): Boolean;
    function BuildPeerIntermediateStore: ISSLCertificateStore;
    function TryResolvePeerIssuerCertificate(
      out AIssuerCertificate: ISSLCertificate;
      out AError: string
    ): Boolean;
    function TryLoadOCSPSignedCertificateTimestampList(
      out ASignedCertificateTimestampList: TBytes;
      out ASignedCertificateTimestampCount: Integer;
      out AFound: Boolean;
      out AError: string
    ): Boolean;
    function TryBuildPeerOCSPCertificatePair(
      out ALeafCertificate, AIssuerCertificate: TX509Certificate;
      out AError: string
    ): Boolean;
    function ValidateClientPeerCertificateTrust: Boolean;
    function ValidateCertificatePinIfEnabled: Boolean;
    function ValidateClientPeerCertificateFlags: Boolean;
    function ValidateClientOCSPStapling: Boolean;
    function ValidateClientOnlineOCSP: Boolean;
    function ValidateClientCertificateTransparency: Boolean;
    function ValidateServerCertificateVerify(
      ACipherSuite: Word;
      const AHandshakeMessage: TBytes;
      const ATranscriptData: TBytes
    ): Boolean;
  protected
    function DoRead(var ABuffer; ACount: Integer): Integer; override;
    function DoWrite(const ABuffer; ACount: Integer): Integer; override;
    function DoConnect: Boolean; override;
    function DoAccept: Boolean; override;
    function DoHandshakeInternal: TSSLHandshakeState; override;
    function DoShutdown: Boolean; override;
    procedure DoClose; override;
    function DoRenegotiate: Boolean; override;
    function DoGetError(ARet: Integer): TSSLErrorCode; override;
    function DoWantRead: Boolean; override;
    function DoWantWrite: Boolean; override;
    function DoGetProtocolVersion: TSSLProtocolVersion; override;
    function DoGetCipherName: string; override;
    function DoGetPeerCertificate: ISSLCertificate; override;
    function DoGetPeerCertificateChain: TSSLCertificateArray; override;
    function DoGetVerifyResult: Integer; override;
    function DoGetVerifyResultString: string; override;
    function DoGetSession: ISSLSession; override;
    procedure DoSetSession(ASession: ISSLSession); override;
    function DoIsSessionReused: Boolean; override;
    function DoGetConnectionInfoServerName: string; override;
    function DoGetSelectedALPNProtocol: string; override;
    function DoGetState: string; override;
    function DoGetNativeHandle: Pointer; override;
    function DoGetOCSPStaplingEnabled: Boolean; override;
    function DoGetOCSPResponse: TBytes; override;
    function DoIsOCSPResponseVerified: Boolean; override;
    function DoGetOCSPResponseStatus: string; override;
    function DoGetCertificateTransparencyEnabled: Boolean; override;
    function DoGetSignedCertificateTimestampList: TBytes; override;
    function DoGetSignedCertificateTimestampCount: Integer; override;
    function DoGetCertificateTransparencyStatus: string; override;
    function DoHasCertificateTransparencyValidationResult: Boolean; override;
    function DoIsCertificateTransparencyPolicySatisfied: Boolean; override;
    function DoGetCertificateTransparencyValidationStatus: string; override;
  public
    constructor Create(AContext: ISSLContext; ASocket: THandle); overload;
    constructor Create(AContext: ISSLContext; AStream: TStream); overload;

    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
    function SetEarlyData(const AData: TBytes): TSSLOperationResult;
    function GetEarlyDataStatus: TSSLEarlyDataStatus;
    function GetEarlyDataLimit: Cardinal;
  end;

implementation

uses
  fafafa.ssl.tls13.clienthello,
  fafafa.ssl.tls13.clienthello.parser,
  fafafa.ssl.tls13.parser,
  fafafa.ssl.tls13.serverhello,
  fafafa.ssl.tls13.finished,
  fafafa.ssl.tls13.recordcrypto,
  fafafa.ssl.tls13.aead,
  fafafa.ssl.tls13.x25519,
  fafafa.ssl.factory,
  fafafa.ssl.tls13.servercertificate,
  fafafa.ssl.tls13.servercertverify,
  fafafa.ssl.freepascal.session,
  fafafa.ssl.freepascal.context.material,
  fafafa.ssl.ocsp,
  fafafa.ssl.ocsp.stapling,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.certchain,
  fafafa.ssl.random,
  fafafa.ssl.memutils,
  fafafa.ssl.crypto.constant_time,
  fafafa.ssl.ct.sct,
  fafafa.ssl.native_handle,
  fafafa.ssl.net.hooks,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.api.x509;

const
  X509_EXTENSION_EMBEDDED_SIGNED_CERTIFICATE_TIMESTAMP = '1.3.6.1.4.1.11129.2.4.2';

function TryEnsureOpenSSLCTValidationAvailable(out AError: string): Boolean;
begin
  AError := '';
  Result := False;

  try
    if not TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
    begin
      AError := 'OpenSSL library is unavailable';
      Exit;
    end;

    LoadCTFunctions;
    LoadStackFunctions;
  except
    on E: Exception do
    begin
      AError := 'Failed to initialize OpenSSL CT validation modules: ' + E.Message;
      Exit;
    end;
  end;

  if not Assigned(o2i_SCT) or
    not Assigned(CT_POLICY_EVAL_CTX_new) or
    not Assigned(CT_POLICY_EVAL_CTX_free) or
    not Assigned(CT_POLICY_EVAL_CTX_set1_cert) or
    not Assigned(SCT_validate) or
    not Assigned(SCT_get_validation_status) or
    not Assigned(SCT_free) then
  begin
    AError := 'Required OpenSSL CT functions are unavailable';
    Exit;
  end;

  Result := True;
end;

function TryCreateOpenSSLCertificateFromCertificate(
  ACertificate: ISSLCertificate;
  out AOpenSSLCertificate: ISSLCertificate;
  out AX509: PX509;
  out AError: string
): Boolean;
var
  LDER: TBytes;
  LHandle: Pointer;
begin
  AOpenSSLCertificate := nil;
  AX509 := nil;
  AError := '';
  Result := False;

  if ACertificate = nil then
  begin
    AError := 'Certificate is unavailable';
    Exit;
  end;

  LDER := ACertificate.SaveToDER;
  if Length(LDER) = 0 then
  begin
    AError := 'Certificate DER is empty';
    Exit;
  end;

  try
    AOpenSSLCertificate := TSSLFactory.CreateCertificate(sslOpenSSL);
  except
    on E: Exception do
    begin
      AError := 'Failed to create OpenSSL certificate: ' + E.Message;
      Exit;
    end;
  end;

  if (AOpenSSLCertificate = nil) or (not AOpenSSLCertificate.LoadFromDER(LDER)) then
  begin
    AError := 'Failed to materialize OpenSSL certificate from DER';
    AOpenSSLCertificate := nil;
    Exit;
  end;

  if not TryGetNativeHandle(AOpenSSLCertificate, LHandle) or (LHandle = nil) then
  begin
    AError := 'OpenSSL certificate native handle is unavailable';
    AOpenSSLCertificate := nil;
    Exit;
  end;

  AX509 := PX509(LHandle);
  Result := True;
end;

function CountValidSignedCertificateTimestamps(
  const AResults: TSCTValidationResultArray
): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AResults) do
    if AResults[I].IsValid then
      Inc(Result);
end;

procedure PopulateSignedCertificateTimestampValidationResult(
  ASCT: PSCT;
  const AOptions: TSCTValidationOptions;
  out AResult: TSCTValidationResult
);
begin
  AResult.IsValid := False;
  AResult.Status := SCT_VALIDATION_STATUS_NOT_SET;
  AResult.ErrorMessage := '';
  AResult.LogName := '';
  AResult.Timestamp := 0;

  if ASCT = nil then
  begin
    AResult.ErrorMessage := 'Null SCT';
    Exit;
  end;

  if Assigned(SCT_get_timestamp) then
    AResult.Timestamp := SCT_get_timestamp(ASCT);

  if Assigned(SCT_get_validation_status) then
    AResult.Status := SCT_get_validation_status(ASCT);

  case AResult.Status of
    SCT_VALIDATION_STATUS_VALID:
      AResult.IsValid := True;
    SCT_VALIDATION_STATUS_UNKNOWN_LOG:
      begin
        AResult.ErrorMessage := 'Unknown CT log';
        AResult.IsValid := AOptions.AllowUnknownLogs;
      end;
    SCT_VALIDATION_STATUS_INVALID:
      AResult.ErrorMessage := 'Invalid SCT signature';
    SCT_VALIDATION_STATUS_UNVERIFIED:
      AResult.ErrorMessage := 'SCT could not be verified';
    SCT_VALIDATION_STATUS_UNKNOWN_VERSION:
      AResult.ErrorMessage := 'Unknown SCT version';
  else
    AResult.ErrorMessage := 'Unknown validation status';
  end;
end;

function CheckCertificateTransparencyPolicy(
  const AResults: TSCTValidationResultArray;
  const AOptions: TSCTValidationOptions
): Boolean;
var
  LValidCount: Integer;
begin
  if Length(AResults) < AOptions.MinimumSCTCount then
    Exit(False);

  if AOptions.RequireValidSCTs then
  begin
    LValidCount := CountValidSignedCertificateTimestamps(AResults);
    Exit(LValidCount >= AOptions.MinimumSCTCount);
  end;

  Result := Length(AResults) >= AOptions.MinimumSCTCount;
end;

function TryCollectSignedCertificateTimestampValidationResults(
  const ASignedCertificateTimestampList: TBytes;
  ALeafX509: PX509;
  AIssuerX509: PX509;
  const AOptions: TSCTValidationOptions;
  out AResults: TSCTValidationResultArray;
  out AError: string
): Boolean;
var
  LEvalContext: PCT_POLICY_EVAL_CTX;
  LCurrentTime: UInt64;
  LListLength: Integer;
  LOffset: Integer;
  LSCTLength: Integer;
  LCount: Integer;
  I: Integer;
  LSCT: PSCT;
  LCursor: PByte;
begin
  SetLength(AResults, 0);
  AError := '';
  Result := False;

  if (Length(ASignedCertificateTimestampList) = 0) or (ALeafX509 = nil) then
  begin
    AError := 'OpenSSL CT validation inputs are incomplete';
    Exit;
  end;

  if not Assigned(CT_POLICY_EVAL_CTX_new) or
    not Assigned(CT_POLICY_EVAL_CTX_free) or
    not Assigned(CT_POLICY_EVAL_CTX_set1_cert) or
    not Assigned(o2i_SCT) or
    not Assigned(SCT_validate) or
    not Assigned(SCT_get_validation_status) or
    not Assigned(SCT_free) then
  begin
    AError := 'Required OpenSSL CT evaluation functions are unavailable';
    Exit;
  end;

  if Length(ASignedCertificateTimestampList) < 2 then
  begin
    AError := 'SignedCertificateTimestampList is too short';
    Exit;
  end;

  LListLength := ReadUInt16(ASignedCertificateTimestampList, 0);
  if (LListLength <= 0) or (Length(ASignedCertificateTimestampList) <> 2 + LListLength) then
  begin
    AError := 'SignedCertificateTimestampList length is invalid';
    Exit;
  end;

  LCount := 0;
  LOffset := 2;
  while LOffset < Length(ASignedCertificateTimestampList) do
  begin
    if LOffset + 2 > Length(ASignedCertificateTimestampList) then
    begin
      AError := 'Serialized SCT length is truncated';
      Exit;
    end;

    LSCTLength := ReadUInt16(ASignedCertificateTimestampList, LOffset);
    Inc(LOffset, 2);
    if (LSCTLength <= 0) or (LOffset + LSCTLength > Length(ASignedCertificateTimestampList)) then
    begin
      AError := 'Serialized SCT length is invalid';
      Exit;
    end;

    Inc(LCount);
    Inc(LOffset, LSCTLength);
  end;

  SetLength(AResults, LCount);

  LEvalContext := CT_POLICY_EVAL_CTX_new();
  if LEvalContext = nil then
  begin
    AError := 'Failed to create OpenSSL CT evaluation context';
    Exit;
  end;

  try
    CT_POLICY_EVAL_CTX_set1_cert(LEvalContext, ALeafX509);

    if (AIssuerX509 <> nil) and Assigned(CT_POLICY_EVAL_CTX_set1_issuer) then
      CT_POLICY_EVAL_CTX_set1_issuer(LEvalContext, AIssuerX509);

    if Assigned(CT_POLICY_EVAL_CTX_set_time) then
    begin
      LCurrentTime := UInt64(DateTimeToUnix(Now) * 1000);
      LCurrentTime := LCurrentTime + UInt64(AOptions.ClockDriftTolerance);
      CT_POLICY_EVAL_CTX_set_time(LEvalContext, LCurrentTime);
    end;

    LOffset := 2;
    for I := 0 to LCount - 1 do
    begin
      LSCTLength := ReadUInt16(ASignedCertificateTimestampList, LOffset);
      Inc(LOffset, 2);

      LSCT := nil;
      LCursor := @ASignedCertificateTimestampList[LOffset];
      try
        if o2i_SCT(@LSCT, @LCursor, NativeUInt(LSCTLength)) = nil then
        begin
          AResults[I].IsValid := False;
          AResults[I].Status := SCT_VALIDATION_STATUS_NOT_SET;
          AResults[I].ErrorMessage := 'Failed to decode SCT';
          AResults[I].LogName := '';
          AResults[I].Timestamp := 0;
        end
        else
        begin
          try
            SCT_validate(LSCT, LEvalContext);
          except
            on E: Exception do
            begin
              AResults[I].IsValid := False;
              AResults[I].Status := SCT_VALIDATION_STATUS_NOT_SET;
              AResults[I].ErrorMessage := 'OpenSSL SCT_validate failed: ' + E.Message;
              AResults[I].LogName := '';
              AResults[I].Timestamp := 0;
            end;
          end;

          PopulateSignedCertificateTimestampValidationResult(LSCT, AOptions, AResults[I]);
        end;
      finally
        if LSCT <> nil then
          SCT_free(LSCT);
      end;

      Inc(LOffset, LSCTLength);
    end;
  finally
    CT_POLICY_EVAL_CTX_free(LEvalContext);
  end;

  Result := Length(AResults) > 0;
end;

function BuildCertificateTransparencyValidationStatus(
  const AResults: TSCTValidationResultArray;
  APolicySatisfied: Boolean
): string;
var
  I: Integer;
  LValidCount: Integer;
  LStatuses: string;
begin
  if Length(AResults) = 0 then
    Exit('Validation unavailable: validator returned no SCT results');

  LValidCount := CountValidSignedCertificateTimestamps(AResults);
  LStatuses := '';
  for I := 0 to High(AResults) do
  begin
    if LStatuses <> '' then
      LStatuses := LStatuses + ', ';
    LStatuses := LStatuses + GetSCTValidationStatusName(AResults[I].Status);
  end;

  if APolicySatisfied then
    Result := 'Policy satisfied'
  else
    Result := 'Policy failed';

  Result := Format(
    '%s (%d/%d valid SCTs; statuses=%s)',
    [Result, LValidCount, Length(AResults), LStatuses]
  );
end;

function TryLoadEmbeddedSignedCertificateTimestampList(
  ACertificate: ISSLCertificate;
  out ASignedCertificateTimestampList: TBytes;
  out ASignedCertificateTimestampCount: Integer;
  out AFound: Boolean;
  out AError: string
): Boolean; forward;

function SelectPreferredProtocol(const AContext: ISSLContext): TSSLProtocolVersion;
var
  LProtocols: TSSLProtocolVersions;
begin
  Result := AContext.GetPreferredVersion;
  if Result <> sslProtocolUnknown then
    Exit;

  LProtocols := AContext.GetProtocolVersions;
  if sslProtocolTLS13 in LProtocols then
    Exit(sslProtocolTLS13);
  if sslProtocolTLS12 in LProtocols then
    Exit(sslProtocolTLS12);
  if sslProtocolTLS11 in LProtocols then
    Exit(sslProtocolTLS11);
  if sslProtocolTLS10 in LProtocols then
    Exit(sslProtocolTLS10);

  Result := sslProtocolUnknown;
end;

function HashTLS13TranscriptForSuite(ACipherSuite: Word; const ATranscriptData: TBytes): TBytes;
begin
  if TLS13CipherSuiteIsSHA256(ACipherSuite) then
    Exit(SHA256(ATranscriptData));

  if TLS13CipherSuiteIsSHA384(ACipherSuite) then
    Exit(SHA384(ATranscriptData));

  SetLength(Result, 0);
end;

function NormalizeHostForVerify(const S: string): string;
var
  LHost: string;
  P, PEnd: SizeInt;
  PortPart: string;
  I: Integer;
begin
  LHost := Trim(S);

  if (LHost <> '') and (LHost[1] = '[') then
  begin
    PEnd := Pos(']', LHost);
    if PEnd > 0 then
      LHost := Copy(LHost, 2, PEnd - 2);
  end;

  P := Pos('%', LHost);
  if P > 0 then
    LHost := Copy(LHost, 1, P - 1);

  if (Pos(':', LHost) > 0) and (Pos(':', LHost) = LastDelimiter(':', LHost)) then
  begin
    P := Pos(':', LHost);
    PortPart := Copy(LHost, P + 1, Length(LHost) - P);
    if PortPart <> '' then
    begin
      for I := 1 to Length(PortPart) do
        if not (PortPart[I] in ['0'..'9']) then
        begin
          PortPart := '';
          Break;
        end;
      if PortPart <> '' then
        LHost := Copy(LHost, 1, P - 1);
    end;
  end;

  Result := LHost;
end;

function BytesEqual(const ALeft, ARight: TBytes): Boolean;
begin
  Result := TConstantTime.CompareBytes(ALeft, ARight) = 1;
end;

function ClientHelloHasExtension(const AHandshake: TBytes; AExtensionType: Word): Boolean;
var
  LOffset: Integer;
  LBodyLen: Cardinal;
  LBodyEnd: Integer;
  LSessionIDLen: Integer;
  LCipherSuitesLen: Integer;
  LCompressionLen: Integer;
  LExtensionsLen: Integer;
  LExtensionsEnd: Integer;
  LExtType: Word;
  LExtLen: Word;
begin
  Result := False;

  if (Length(AHandshake) < 4) or (AHandshake[0] <> TLS_HANDSHAKE_TYPE_CLIENT_HELLO) then
    Exit;

  LBodyLen := ReadUInt24(AHandshake, 1);
  LBodyEnd := 4 + Integer(LBodyLen);
  if Length(AHandshake) <> LBodyEnd then
    Exit;

  LOffset := 4 + 2 + 32;
  if LOffset >= LBodyEnd then
    Exit;

  LSessionIDLen := AHandshake[LOffset];
  Inc(LOffset);
  Inc(LOffset, LSessionIDLen);
  if LOffset + 2 > LBodyEnd then
    Exit;

  LCipherSuitesLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2 + LCipherSuitesLen);
  if LOffset + 1 > LBodyEnd then
    Exit;

  LCompressionLen := AHandshake[LOffset];
  Inc(LOffset);
  Inc(LOffset, LCompressionLen);
  if LOffset + 2 > LBodyEnd then
    Exit;

  LExtensionsLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  LExtensionsEnd := LOffset + LExtensionsLen;
  if LExtensionsEnd <> LBodyEnd then
    Exit;

  while LOffset + 4 <= LExtensionsEnd do
  begin
    LExtType := ReadUInt16(AHandshake, LOffset);
    LExtLen := ReadUInt16(AHandshake, LOffset + 2);
    Inc(LOffset, 4);
    if LOffset + Integer(LExtLen) > LExtensionsEnd then
      Exit(False);
    if LExtType = AExtensionType then
      Exit(True);
    Inc(LOffset, Integer(LExtLen));
  end;
end;

function CloneCertificateArray(const ASource: TSSLCertificateArray): TSSLCertificateArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ASource));
  for I := 0 to High(ASource) do
    if ASource[I] <> nil then
      Result[I] := ASource[I].Clone
    else
      Result[I] := nil;
end;

{$WARN 6018 OFF}
function OCSPStaplingStateToString(
  AStatus: TOCSPStaplingStatus;
  const AErrorMessage: string
): string;
begin
  case AStatus of
    ossNotRequested:
      Result := 'Not Requested';
    ossRequested:
      Result := 'Requested';
    ossReceived:
      Result := 'Received';
    ossVerified:
      Result := 'Verified';
    ossVerificationFailed:
      Result := 'Verification Failed';
    ossNotProvided:
      Result := 'No OCSP Response';
    ossExpired:
      Result := 'Expired';
  else
    Result := 'Unknown';
  end;

  if Trim(AErrorMessage) <> '' then
    Result := Result + ': ' + Trim(AErrorMessage);
end;
{$WARN 6018 ON}

function BuildExtensionHeader(AType: Word; const AData: TBytes): TBytes;
begin
  Result := nil;
  AppendUInt16(Result, AType);
  AppendUInt16(Result, Word(Length(AData)));
  AppendBytes(Result, AData);
end;

function StringToAnsiBytes(const AValue: string): TBytes;
begin
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[1], Result[0], Length(AValue));
end;

function ParseALPNProtocolList(const AProtocols: string): TBytes;
var
  I: Integer;
  LStart: Integer;
  LStop: Integer;
  LValue: string;
  LProtocolBytes: TBytes;
begin
  SetLength(Result, 0);
  LStart := 1;

  for I := 1 to Length(AProtocols) + 1 do
  begin
    if (I <= Length(AProtocols)) and (AProtocols[I] <> ',') then
      Continue;

    LStop := I - 1;
    while (LStart <= LStop) and (AProtocols[LStart] <= ' ') do
      Inc(LStart);
    while (LStop >= LStart) and (AProtocols[LStop] <= ' ') do
      Dec(LStop);

    if LStop >= LStart then
    begin
      LValue := Copy(AProtocols, LStart, LStop - LStart + 1);
      LProtocolBytes := StringToAnsiBytes(LValue);
      if Length(LProtocolBytes) = 0 then
      begin
        SetLength(Result, 0);
        Exit;
      end;
      if Length(LProtocolBytes) > 255 then
        RaiseInvalidParameter('ALPNProtocolLength');
      AppendByte(Result, Byte(Length(LProtocolBytes)));
      AppendBytes(Result, LProtocolBytes);
    end;

    LStart := I + 1;
  end;
end;

function SelectALPNProtocol(
  const AClientHello: TTLS13ClientHelloInfo;
  const AServerALPNProtocols: string
): string;
var
  I: Integer;
  LStart: Integer;
  LStop: Integer;
  LCandidate: string;
begin
  Result := '';

  if Length(AClientHello.ALPNProtocols) = 0 then
    Exit;

  LStart := 1;
  for I := 1 to Length(AServerALPNProtocols) + 1 do
  begin
    if (I <= Length(AServerALPNProtocols)) and (AServerALPNProtocols[I] <> ',') then
      Continue;

    LStop := I - 1;
    while (LStart <= LStop) and (AServerALPNProtocols[LStart] <= ' ') do
      Inc(LStart);
    while (LStop >= LStart) and (AServerALPNProtocols[LStop] <= ' ') do
      Dec(LStop);

    if LStop >= LStart then
    begin
      LCandidate := Copy(AServerALPNProtocols, LStart, LStop - LStart + 1);
      if TLS13ClientHelloOffersALPNProtocol(AClientHello, LCandidate) then
        Exit(LCandidate);
    end;

    LStart := I + 1;
  end;
end;

function BuildTLS13EncryptedExtensionsHandshake(
  AAcceptEarlyData: Boolean;
  const ASelectedALPNProtocol: string
): TBytes;
var
  LBody: TBytes;
  LExtensions: TBytes;
  LALPNData: TBytes;
  LALPNList: TBytes;
begin
  SetLength(LExtensions, 0);
  if AAcceptEarlyData then
  begin
    AppendUInt16(LExtensions, TLS_EXTENSION_EARLY_DATA);
    AppendUInt16(LExtensions, 0);
  end;

  if ASelectedALPNProtocol <> '' then
  begin
    LALPNList := ParseALPNProtocolList(ASelectedALPNProtocol);
    if Length(LALPNList) = 0 then
      RaiseInvalidParameter('ALPNProtocol');

    SetLength(LALPNData, 0);
    AppendUInt16(LALPNData, Word(Length(LALPNList)));
    AppendBytes(LALPNData, LALPNList);
    LALPNData := BuildExtensionHeader(TLS_EXTENSION_ALPN, LALPNData);
    AppendBytes(LExtensions, LALPNData);
  end;

  SetLength(LBody, 0);
  AppendUInt16(LBody, Word(Length(LExtensions)));
  AppendBytes(LBody, LExtensions);

  SetLength(Result, 0);
  AppendByte(Result, TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

constructor TFreePascalConnection.Create(AContext: ISSLContext; ASocket: THandle);
begin
  inherited Create(AContext);
  FSocket := ASocket;
  FStream := nil;
  // Client hostname/SNI must now be set explicitly on the connection.
  FServerName := '';
  FProtocolVersion := SelectPreferredProtocol(AContext);
  FCipherName := '';
  FALPNProtocols := AContext.GetALPNProtocols;
  FSelectedALPNProtocol := '';
  SetLength(FX25519PrivateKey, 0);
  SetLength(FX25519PublicKey, 0);
  SetLength(FHandshakeSharedSecret, 0);
  InitTLS13EarlyDataSecrets(FEarlyDataSecrets);
  InitTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FEarlyDataSeq := 0;
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  InitTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
  FCurrentSession := nil;
  FConfiguredSession := nil;
  FSessionReused := False;
  FSessionBoundServerName := '';
  FPeerCertificate := nil;
  SetLength(FPeerCertificateChain, 0);
  SetLength(FOCSPResponse, 0);
  FOCSPResponseVerified := False;
  FOCSPResponseStatus := 'Not Requested';
  SetLength(FSignedCertificateTimestampList, 0);
  FSignedCertificateTimestampCount := 0;
  FCertificateTransparencyStatus := 'Not Requested';
  FHasCertificateTransparencyValidationResult := False;
  FCertificateTransparencyPolicySatisfied := False;
  FCertificateTransparencyValidationStatus := 'Not Attempted';
  FEarlyDataStatus := sslEarlyDataNone;
  FEarlyDataLimit := 0;
  SetLength(FEarlyDataPayload, 0);
end;

constructor TFreePascalConnection.Create(AContext: ISSLContext; AStream: TStream);
begin
  inherited Create(AContext);
  if AStream = nil then
    RaiseInvalidParameter('AStream');

  FSocket := -1;
  FStream := AStream;
  FServerName := '';
  FProtocolVersion := SelectPreferredProtocol(AContext);
  FCipherName := '';
  FALPNProtocols := AContext.GetALPNProtocols;
  FSelectedALPNProtocol := '';
  SetLength(FX25519PrivateKey, 0);
  SetLength(FX25519PublicKey, 0);
  SetLength(FHandshakeSharedSecret, 0);
  InitTLS13EarlyDataSecrets(FEarlyDataSecrets);
  InitTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FEarlyDataSeq := 0;
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  InitTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
  FCurrentSession := nil;
  FConfiguredSession := nil;
  FSessionReused := False;
  FSessionBoundServerName := '';
  FPeerCertificate := nil;
  SetLength(FPeerCertificateChain, 0);
  SetLength(FOCSPResponse, 0);
  FOCSPResponseVerified := False;
  FOCSPResponseStatus := 'Not Requested';
  SetLength(FSignedCertificateTimestampList, 0);
  FSignedCertificateTimestampCount := 0;
  FCertificateTransparencyStatus := 'Not Requested';
  FHasCertificateTransparencyValidationResult := False;
  FCertificateTransparencyPolicySatisfied := False;
  FCertificateTransparencyValidationStatus := 'Not Attempted';
  FEarlyDataStatus := sslEarlyDataNone;
  FEarlyDataLimit := 0;
  SetLength(FEarlyDataPayload, 0);
end;

function TFreePascalConnection.SendData(const ABuffer; ASize: Integer): Integer;
begin
  if FStream <> nil then
    Exit(FStream.Write(ABuffer, ASize));

  if FSocket < 0 then
    Exit(-1);

  {$IFDEF WINDOWS}
  Result := Winsock2.send(FSocket, ABuffer, ASize, 0);
  if Result = SOCKET_ERROR then
    Result := -1;
  {$ELSE}
  Result := fpSend(FSocket, @ABuffer, ASize, 0);
  {$ENDIF}
end;

function TFreePascalConnection.RecvData(var ABuffer; ASize: Integer): Integer;
begin
  if FStream <> nil then
    Exit(FStream.Read(ABuffer, ASize));

  if FSocket < 0 then
    Exit(-1);

  {$IFDEF WINDOWS}
  Result := Winsock2.recv(FSocket, ABuffer, ASize, 0);
  if Result = SOCKET_ERROR then
    Result := -1;
  {$ELSE}
  Result := fpRecv(FSocket, @ABuffer, ASize, 0);
  {$ENDIF}
end;

function TFreePascalConnection.SendAll(const AData: TBytes): Boolean;
var
  LOffset, LChunk, LTotal: Integer;
begin
  Result := False;
  LTotal := Length(AData);
  LOffset := 0;

  while LOffset < LTotal do
  begin
    LChunk := SendData(AData[LOffset], LTotal - LOffset);
    if LChunk <= 0 then
      Exit;
    Inc(LOffset, LChunk);
  end;

  Result := True;
end;

function TFreePascalConnection.RecvExact(var AData: TBytes; ACount: Integer): Boolean;
var
  LOffset, LChunk: Integer;
begin
  Result := False;
  if ACount < 0 then
    Exit;

  SetLength(AData, ACount);
  LOffset := 0;

  while LOffset < ACount do
  begin
    LChunk := RecvData(AData[LOffset], ACount - LOffset);
    if LChunk <= 0 then
      Exit;
    Inc(LOffset, LChunk);
  end;

  Result := True;
end;

function TFreePascalConnection.RecvTLSRecord(out AHeader: TTLSRecordHeader; out APayload, ARecord: TBytes): Boolean;
const
  TLS_MAX_CIPHERTEXT_LENGTH = 16384 + 256;
var
  LHeaderBytes: TBytes;
begin
  Result := False;
  SetLength(APayload, 0);
  SetLength(ARecord, 0);

  if not RecvExact(LHeaderBytes, 5) then
    Exit;

  if not ParseTLSRecordHeader(LHeaderBytes, AHeader) then
    Exit;

  if AHeader.Length > TLS_MAX_CIPHERTEXT_LENGTH then
    Exit;

  if not RecvExact(APayload, AHeader.Length) then
    Exit;

  SetLength(ARecord, 5 + Length(APayload));
  Move(LHeaderBytes[0], ARecord[0], 5);
  if Length(APayload) > 0 then
    Move(APayload[0], ARecord[5], Length(APayload));

  Result := True;
end;

function TFreePascalConnection.GetBufferedStreamBytesAvailable: Int64;
begin
  Result := 0;
  if FStream = nil then
    Exit;

  try
    Result := FStream.Size - FStream.Position;
  except
    Result := 0;
  end;
  if Result < 0 then
    Result := 0;
end;

procedure TFreePascalConnection.SetHandshakeError(ACode: TSSLErrorCode; const AMessage: string);
begin
  FLastErrorCode := ACode;
  FLastErrorString := AMessage;
  RecordError(FLastErrorCode, FLastErrorString);
end;

procedure TFreePascalConnection.AppendHandshakeBytes(var ADest: TBytes; const ASource: TBytes);
var
  LOldLen, LAppendLen: Integer;
begin
  LAppendLen := Length(ASource);
  if LAppendLen = 0 then
    Exit;

  LOldLen := Length(ADest);
  SetLength(ADest, LOldLen + LAppendLen);
  Move(ASource[0], ADest[LOldLen], LAppendLen);
end;

function TFreePascalConnection.TryPopHandshakeMessage(var ABuffer: TBytes; out AMessage: TBytes): Boolean;
var
  LMsgLen: Cardinal;
  LTotalLen: Integer;
  LRemainLen: Integer;
  LTemp: TBytes;
begin
  SetLength(AMessage, 0);
  Result := False;

  if Length(ABuffer) < 4 then
    Exit;

  LMsgLen := ReadUInt24(ABuffer, 1);
  if LMsgLen > Cardinal(High(Integer) - 4) then
    Exit;

  LTotalLen := 4 + Integer(LMsgLen);
  if Length(ABuffer) < LTotalLen then
    Exit;

  SetLength(AMessage, LTotalLen);
  Move(ABuffer[0], AMessage[0], LTotalLen);

  LRemainLen := Length(ABuffer) - LTotalLen;
  if LRemainLen > 0 then
  begin
    SetLength(LTemp, LRemainLen);
    Move(ABuffer[LTotalLen], LTemp[0], LRemainLen);
    ABuffer := LTemp;
  end
  else
    SetLength(ABuffer, 0);

  Result := True;
end;

function TFreePascalConnection.ProcessPostHandshakeFragment(const AHandshakeFragment: TBytes): Boolean;
var
  LMessage: TBytes;
  LType: Byte;
  LError: string;
  LTicket: TTLS13NewSessionTicket;
  LKeyUpdate: TTLS13KeyUpdateInfo;
  LResumptionPSK: TBytes;
  LSession: TFreePascalSession;
  LTimeout: Integer;
begin
  Result := False;

  if Length(AHandshakeFragment) = 0 then
  begin
    Result := True;
    Exit;
  end;

  AppendHandshakeBytes(FPostHandshakeBuffer, AHandshakeFragment);

  while TryPopHandshakeMessage(FPostHandshakeBuffer, LMessage) do
  begin
    if Length(LMessage) < 4 then
    begin
      SetHandshakeError(sslErrProtocol, 'Malformed post-handshake message header');
      Exit;
    end;

    LType := LMessage[0];
    case LType of
      TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET:
        begin
          if not TryParseTLS13NewSessionTicket(LMessage, LTicket, LError) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid NewSessionTicket: ' + LError);
            Exit;
          end;

          if (not FApplicationSecrets.Valid) or
            (Length(FApplicationSecrets.MasterSecret) <> FApplicationSecrets.HashSize) or
            (Length(FApplicationSecrets.ResumptionTranscriptHash) <> FApplicationSecrets.HashSize) then
          begin
            SetHandshakeError(sslErrProtocol, 'Application transcript state is not ready for NewSessionTicket');
            Exit;
          end;

          LResumptionPSK := TLS13DeriveResumptionPSKFromTranscriptHash(
            FApplicationSecrets.CipherSuite,
            FApplicationSecrets.MasterSecret,
            FApplicationSecrets.ResumptionTranscriptHash,
            LTicket.TicketNonce
          );
          if Length(LResumptionPSK) <> FApplicationSecrets.HashSize then
          begin
            SetHandshakeError(sslErrProtocol, 'Failed to derive resumption PSK from NewSessionTicket');
            Exit;
          end;

          if LTicket.TicketLifetime > Cardinal(High(Integer)) then
            LTimeout := High(Integer)
          else
            LTimeout := Integer(LTicket.TicketLifetime);

          LSession := TFreePascalSession.Create;
          LSession.ConfigureResumption(
            FApplicationSecrets.CipherSuite,
            TLS13CipherSuiteToString(FApplicationSecrets.CipherSuite),
            LTicket.TicketNonce,
            LTicket.Ticket,
            LResumptionPSK,
            LTicket.TicketLifetime,
            LTicket.TicketAgeAdd,
            Now,
            LTimeout,
            LTicket.MaxEarlyDataSize
          );
          LSession.BoundServerName := FServerName;
          FCurrentSession := LSession;
          FLastSessionTicket := LTicket;
          Inc(FSessionTicketCount);
        end;

      TLS_HANDSHAKE_TYPE_KEY_UPDATE:
        begin
          if not TryParseTLS13KeyUpdate(LMessage, LKeyUpdate, LError) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid KeyUpdate: ' + LError);
            Exit;
          end;

          if FIsServerMode then
          begin
            if not TryUpdateTLS13ClientApplicationReadKeys(FApplicationSecrets, LError) then
            begin
              SetHandshakeError(sslErrProtocol, 'Failed to rotate client application read key: ' + LError);
              Exit;
            end;
            FClientApplicationSeq := 0;
          end
          else
          begin
            if not TryUpdateTLS13ServerApplicationReadKeys(FApplicationSecrets, LError) then
            begin
              SetHandshakeError(sslErrProtocol, 'Failed to rotate server application read key: ' + LError);
              Exit;
            end;
            FServerApplicationSeq := 0;
          end;

          if LKeyUpdate.RequestUpdate then
          begin
            if not SendPostHandshakeKeyUpdate(False) then
              Exit;
          end;
        end;

    else
      begin
        SetHandshakeError(
          sslErrUnsupported,
          Format('Unsupported post-handshake message type %d', [LType])
        );
        Exit;
      end;
    end;
  end;

  if Length(FPostHandshakeBuffer) > 131072 then
  begin
    SetHandshakeError(sslErrProtocol, 'Post-handshake buffer exceeded limit');
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.DrainBufferedApplicationRecords: Boolean;
var
  LFragment: TBytes;
begin
  Result := True;
  if FStream = nil then
    Exit;

  while GetBufferedStreamBytesAvailable > 0 do
  begin
    if not RecvApplicationDataFragment(LFragment, True) then
      Exit(False);
    if Length(LFragment) > 0 then
      AppendHandshakeBytes(FApplicationReadBuffer, LFragment);
  end;
end;

procedure TFreePascalConnection.ClearOCSPStaplingState;
begin
  SetLength(FOCSPResponse, 0);
  FOCSPResponseVerified := False;
  FOCSPResponseStatus := 'Not Requested';
end;

procedure TFreePascalConnection.ClearCertificateTransparencyState;
begin
  SetLength(FSignedCertificateTimestampList, 0);
  FSignedCertificateTimestampCount := 0;
  FCertificateTransparencyStatus := 'Not Requested';
  FHasCertificateTransparencyValidationResult := False;
  FCertificateTransparencyPolicySatisfied := False;
  FCertificateTransparencyValidationStatus := 'Not Attempted';
end;

procedure TFreePascalConnection.RefreshCertificateTransparencyValidationState;
var
  LError: string;
  LLeafCertificate: ISSLCertificate;
  LIssuerCertificate: ISSLCertificate;
  LIssuerSource: ISSLCertificate;
  LLeafX509: PX509;
  LIssuerX509: PX509;
  LOptions: TSCTValidationOptions;
  LResults: TSCTValidationResultArray;
begin
  FHasCertificateTransparencyValidationResult := False;
  FCertificateTransparencyPolicySatisfied := False;
  FCertificateTransparencyValidationStatus := 'Not Attempted';

  if Length(FSignedCertificateTimestampList) = 0 then
    Exit;

  if not TryEnsureOpenSSLCTValidationAvailable(LError) then
  begin
    FCertificateTransparencyValidationStatus := 'Validation unavailable: ' + LError;
    Exit;
  end;

  if not TryCreateOpenSSLCertificateFromCertificate(
    FPeerCertificate,
    LLeafCertificate,
    LLeafX509,
    LError
  ) then
  begin
    FCertificateTransparencyValidationStatus := 'Validation unavailable: ' + LError;
    Exit;
  end;

  if not TryResolvePeerIssuerCertificate(LIssuerSource, LError) then
  begin
    FCertificateTransparencyValidationStatus := 'Validation unavailable: ' + LError;
    Exit;
  end;

  if not TryCreateOpenSSLCertificateFromCertificate(
    LIssuerSource,
    LIssuerCertificate,
    LIssuerX509,
    LError
  ) then
  begin
    FCertificateTransparencyValidationStatus := 'Validation unavailable: ' + LError;
    Exit;
  end;

  LOptions := CreateDefaultValidationOptions;
  try
    if not TryCollectSignedCertificateTimestampValidationResults(
      FSignedCertificateTimestampList,
      LLeafX509,
      LIssuerX509,
      LOptions,
      LResults,
      LError
    ) then
    begin
      FCertificateTransparencyValidationStatus := 'Validation unavailable: ' + LError;
      Exit;
    end;

    FHasCertificateTransparencyValidationResult := True;
    FCertificateTransparencyPolicySatisfied := CheckCertificateTransparencyPolicy(
      LResults,
      LOptions
    );
    FCertificateTransparencyValidationStatus := BuildCertificateTransparencyValidationStatus(
      LResults,
      FCertificateTransparencyPolicySatisfied
    );
  except
    on E: Exception do
    begin
      FHasCertificateTransparencyValidationResult := False;
      FCertificateTransparencyPolicySatisfied := False;
      FCertificateTransparencyValidationStatus := 'Validation unavailable: ' + E.Message;
    end;
  end;
end;

procedure TFreePascalConnection.ClearPeerCertificateCache;
begin
  ClearOCSPStaplingState;
  ClearCertificateTransparencyState;
  FPeerCertificate := nil;
  SetLength(FPeerCertificateChain, 0);
end;

function TFreePascalConnection.TryCachePeerCertificatesFromHandshake(
  const AHandshakeMessage: TBytes;
  ACertificateTransparencyRequested: Boolean;
  out AError: string
): Boolean;
var
  LCertificateInfo: TTLS13ServerCertificateInfo;
  LCertificate: ISSLCertificate;
  LEmbeddedSCTList: TBytes;
  LEmbeddedSCTCount: Integer;
  LEmbeddedSCTFound: Boolean;
  LOCSPSCTList: TBytes;
  LOCSPSCTCount: Integer;
  LOCSPSCTFound: Boolean;
  LOCSPSCTError: string;
  I: Integer;
begin
  AError := '';
  Result := False;
  ClearPeerCertificateCache;

  if not TryParseTLS13ServerCertificateHandshakeInfo(AHandshakeMessage, LCertificateInfo, AError) then
    Exit;

  FOCSPResponseStatus := 'No OCSP Response';
  if ACertificateTransparencyRequested then
    FCertificateTransparencyStatus := 'No SCT List';

  if LCertificateInfo.HasLeafOCSPStapledResponse then
  begin
    FOCSPResponse := Copy(LCertificateInfo.LeafOCSPStapledResponse);
    FOCSPResponseStatus := 'Received';
  end;

  if LCertificateInfo.HasLeafSignedCertificateTimestampList then
  begin
    FSignedCertificateTimestampList := Copy(LCertificateInfo.LeafSignedCertificateTimestampList);
    FSignedCertificateTimestampCount := LCertificateInfo.LeafSignedCertificateTimestampCount;
    FCertificateTransparencyStatus := Format(
      'Received from TLS extension (%d SCTs)',
      [FSignedCertificateTimestampCount]
    );
  end;

  SetLength(FPeerCertificateChain, Length(LCertificateInfo.Certificates));
  for I := 0 to High(LCertificateInfo.Certificates) do
  begin
    try
      LCertificate := TSSLFactory.CreateCertificate(sslFreePascal);
    except
      on E: Exception do
      begin
        AError := Format('Failed to create peer certificate #%d: %s', [I + 1, E.Message]);
        ClearPeerCertificateCache;
        Exit;
      end;
    end;

    if LCertificate = nil then
    begin
      AError := Format('Failed to create peer certificate #%d', [I + 1]);
      ClearPeerCertificateCache;
      Exit;
    end;

    if not LCertificate.LoadFromDER(LCertificateInfo.Certificates[I]) then
    begin
      AError := Format('Failed to load peer certificate #%d from DER', [I + 1]);
      ClearPeerCertificateCache;
      Exit;
    end;

    FPeerCertificateChain[I] := LCertificate;
  end;

  for I := 0 to High(FPeerCertificateChain) do
  begin
    if FPeerCertificateChain[I] = nil then
      Continue;

    if I < High(FPeerCertificateChain) then
      FPeerCertificateChain[I].SetIssuerCertificate(FPeerCertificateChain[I + 1])
    else
      FPeerCertificateChain[I].SetIssuerCertificate(nil);
  end;

  FPeerCertificate := FPeerCertificateChain[0];

  if ACertificateTransparencyRequested and
    (Length(FSignedCertificateTimestampList) = 0) and
    (FPeerCertificate <> nil) then
  begin
    if not TryLoadEmbeddedSignedCertificateTimestampList(
      FPeerCertificate,
      LEmbeddedSCTList,
      LEmbeddedSCTCount,
      LEmbeddedSCTFound,
      AError
    ) then
    begin
      if Trim(AError) = '' then
        AError := 'Failed to inspect embedded signed_certificate_timestamp';
      ClearPeerCertificateCache;
      Exit;
    end;

    if LEmbeddedSCTFound then
    begin
      FSignedCertificateTimestampList := Copy(LEmbeddedSCTList);
      FSignedCertificateTimestampCount := LEmbeddedSCTCount;
      FCertificateTransparencyStatus := Format(
        'Received from embedded X.509 extension (%d SCTs)',
        [FSignedCertificateTimestampCount]
      );
    end;
  end;

  if ACertificateTransparencyRequested and
    (Length(FSignedCertificateTimestampList) = 0) and
    (Length(FOCSPResponse) > 0) then
  begin
    if not TryLoadOCSPSignedCertificateTimestampList(
      LOCSPSCTList,
      LOCSPSCTCount,
      LOCSPSCTFound,
      LOCSPSCTError
    ) then
    begin
      if Trim(LOCSPSCTError) <> '' then
        FCertificateTransparencyStatus :=
          'OCSP-delivered signed_certificate_timestamp unavailable: ' + LOCSPSCTError;
    end
    else if LOCSPSCTFound then
    begin
      FSignedCertificateTimestampList := Copy(LOCSPSCTList);
      FSignedCertificateTimestampCount := LOCSPSCTCount;
      FCertificateTransparencyStatus := Format(
        'Received from OCSP response (%d SCTs)',
        [FSignedCertificateTimestampCount]
      );
    end;
  end;

  RefreshCertificateTransparencyValidationState;

  Result := True;
end;

function TFreePascalConnection.BuildPeerIntermediateStore: ISSLCertificateStore;
var
  I: Integer;
  LCertificate: ISSLCertificate;
begin
  Result := nil;

  if Length(FPeerCertificateChain) <= 1 then
    Exit;

  try
    Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  except
    Exit(nil);
  end;

  if Result = nil then
    Exit;

  for I := 1 to High(FPeerCertificateChain) do
  begin
    if FPeerCertificateChain[I] = nil then
      Continue;

    LCertificate := FPeerCertificateChain[I].Clone;
    if LCertificate <> nil then
      Result.AddCertificate(LCertificate);
  end;

  if Result.GetCount = 0 then
    Result := nil;
end;

function TFreePascalConnection.TryResolvePeerIssuerCertificate(
  out AIssuerCertificate: ISSLCertificate;
  out AError: string
): Boolean;
var
  LTrustStoreAccess: IFreePascalContextTrustStore;
  LVerificationStore: ISSLCertificateStore;
  LIssuerSubject: string;
begin
  AIssuerCertificate := nil;
  AError := '';
  Result := False;

  if FPeerCertificate = nil then
  begin
    AError := 'Peer certificate is unavailable';
    Exit;
  end;

  if (Length(FPeerCertificateChain) > 1) and (FPeerCertificateChain[1] <> nil) then
  begin
    AIssuerCertificate := FPeerCertificateChain[1];
    Exit(True);
  end;

  LIssuerSubject := Trim(FPeerCertificate.GetIssuer);
  if LIssuerSubject <> '' then
  begin
    if Supports(FContext, IFreePascalContextTrustStore, LTrustStoreAccess) then
    begin
      LVerificationStore := LTrustStoreAccess.BuildVerificationStore;
      if LVerificationStore <> nil then
      begin
        AIssuerCertificate := LVerificationStore.FindBySubject(LIssuerSubject);
        if AIssuerCertificate <> nil then
          Exit(True);
      end;
    end;
  end;

  if SameText(Trim(FPeerCertificate.GetSubject), LIssuerSubject) then
  begin
    AIssuerCertificate := FPeerCertificate;
    Exit(True);
  end;

  if LIssuerSubject = '' then
    AError := 'Peer certificate issuer subject is empty'
  else
    AError := 'Issuer certificate is unavailable in peer chain and trust store';
end;

function TryLoadX509Certificate(
  ACertificate: ISSLCertificate;
  out AX509Certificate: TX509Certificate;
  out AError: string
): Boolean;
var
  LDER: TBytes;
begin
  AX509Certificate := nil;
  AError := '';
  Result := False;

  if ACertificate = nil then
  begin
    AError := 'Certificate is nil';
    Exit;
  end;

  LDER := ACertificate.SaveToDER;
  if Length(LDER) = 0 then
  begin
    AError := 'Certificate DER is empty';
    Exit;
  end;

  AX509Certificate := TX509Certificate.Create;
  try
    AX509Certificate.LoadFromDER(LDER);
  except
    on E: Exception do
    begin
      AX509Certificate.Free;
      AX509Certificate := nil;
      AError := 'Failed to parse certificate DER: ' + E.Message;
      Exit;
    end;
  end;

  Result := True;
end;

function TryLoadEmbeddedSignedCertificateTimestampList(
  ACertificate: ISSLCertificate;
  out ASignedCertificateTimestampList: TBytes;
  out ASignedCertificateTimestampCount: Integer;
  out AFound: Boolean;
  out AError: string
): Boolean;
var
  LX509Certificate: TX509Certificate;
  I: Integer;
begin
  SetLength(ASignedCertificateTimestampList, 0);
  ASignedCertificateTimestampCount := 0;
  AFound := False;
  AError := '';
  Result := False;

  if not TryLoadX509Certificate(ACertificate, LX509Certificate, AError) then
  begin
    AError := 'Failed to inspect embedded signed_certificate_timestamp: ' + AError;
    Exit;
  end;

  try
    for I := 0 to High(LX509Certificate.Extensions) do
    begin
      if SameText(
        LX509Certificate.Extensions[I].OID,
        X509_EXTENSION_EMBEDDED_SIGNED_CERTIFICATE_TIMESTAMP
      ) then
      begin
        AFound := True;
        ASignedCertificateTimestampList := Copy(LX509Certificate.Extensions[I].Value);
        Break;
      end;
    end;
  finally
    LX509Certificate.Free;
  end;

  if not AFound then
    Exit(True);

  if not TryParseSignedCertificateTimestampList(
    ASignedCertificateTimestampList,
    ASignedCertificateTimestampCount,
    AError
  ) then
  begin
    AError := 'embedded signed_certificate_timestamp is invalid: ' + AError;
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.TryBuildPeerOCSPCertificatePair(
  out ALeafCertificate, AIssuerCertificate: TX509Certificate;
  out AError: string
): Boolean;
var
  LIssuer: ISSLCertificate;
begin
  ALeafCertificate := nil;
  AIssuerCertificate := nil;
  AError := '';
  Result := False;

  if FPeerCertificate = nil then
  begin
    AError := 'Peer certificate is unavailable';
    Exit;
  end;

  if not TryLoadX509Certificate(FPeerCertificate, ALeafCertificate, AError) then
    Exit;

  if not TryResolvePeerIssuerCertificate(LIssuer, AError) then
  begin
    ALeafCertificate.Free;
    ALeafCertificate := nil;
    Exit;
  end;

  if not TryLoadX509Certificate(LIssuer, AIssuerCertificate, AError) then
  begin
    ALeafCertificate.Free;
    ALeafCertificate := nil;
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.TryLoadOCSPSignedCertificateTimestampList(
  out ASignedCertificateTimestampList: TBytes;
  out ASignedCertificateTimestampCount: Integer;
  out AFound: Boolean;
  out AError: string
): Boolean;
var
  LOCSPResponse: TOCSPResponse;
  LLeafCertificate: TX509Certificate;
  LIssuerCertificate: TX509Certificate;
  LCertID: TOCSPCertID;
begin
  SetLength(ASignedCertificateTimestampList, 0);
  ASignedCertificateTimestampCount := 0;
  AFound := False;
  AError := '';
  Result := False;

  if Length(FOCSPResponse) = 0 then
    Exit(True);

  LOCSPResponse := TOCSPResponse.Create;
  try
    try
      LOCSPResponse.LoadFromDER(FOCSPResponse);
    except
      on E: Exception do
      begin
        AError := 'Failed to parse OCSP response: ' + E.Message;
        Exit;
      end;
    end;

    if LOCSPResponse.ResponseStatus <> ocsprsSuccessful then
    begin
      AError := 'OCSP response status: ' +
        OCSPResponseStatusToString(LOCSPResponse.ResponseStatus);
      Exit;
    end;

    if not TryBuildPeerOCSPCertificatePair(LLeafCertificate, LIssuerCertificate, AError) then
      Exit;

    try
      LCertID := TOCSPCertID.Create(LLeafCertificate, LIssuerCertificate);
      AFound := LOCSPResponse.TryGetSignedCertificateTimestampList(
        LCertID,
        ASignedCertificateTimestampList,
        ASignedCertificateTimestampCount
      );
      Result := True;
    finally
      LLeafCertificate.Free;
      LIssuerCertificate.Free;
    end;
  finally
    LOCSPResponse.Free;
  end;
end;

function TFreePascalConnection.ValidateClientPeerCertificateTrust: Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LVerifyFlags: TSSLCertVerifyFlags;
  LOptions: TChainVerifyOptions;
  LTrustStoreAccess: IFreePascalContextTrustStore;
  LRevocationMaterialAccess: IFreePascalContextRevocationMaterial;
  LTrustedStore: ISSLCertificateStore;
  LIntermediateStore: ISSLCertificateStore;
  LCRLStore: TStringList;
  LVerifier: ISSLCertificateChainVerifier;
  LVerifyResult: TChainVerifyResult;
  LErrorMessage: string;
begin
  Result := False;

  if FContext = nil then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS context is not available for peer certificate trust verification');
    Exit;
  end;

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  if FSessionReused then
    Exit(True);

  if FPeerCertificate = nil then
  begin
    SetHandshakeError(sslErrCertificate, 'Peer certificate is required for client trust verification');
    Exit;
  end;

  if not Supports(FContext, IFreePascalContextTrustStore, LTrustStoreAccess) then
  begin
    SetHandshakeError(sslErrUnsupported, 'FreePascal context does not expose trust-store access for client verification');
    Exit;
  end;

  LTrustedStore := LTrustStoreAccess.BuildVerificationStore;
  LIntermediateStore := BuildPeerIntermediateStore;
  LCRLStore := nil;
  if Supports(FContext, IFreePascalContextRevocationMaterial, LRevocationMaterialAccess) then
    LCRLStore := LRevocationMaterialAccess.BuildCRLStore;
  LVerifyFlags := FContext.GetCertVerifyFlags;

  LOptions := [cvoCheckSignature, cvoCheckCAConstraints];
  if sslCertVerifyAllowSelfSigned in LVerifyFlags then
    Include(LOptions, cvoAllowSelfSigned);
  if sslCertVerifyStrictChain in LVerifyFlags then
  begin
    Include(LOptions, cvoCheckKeyUsage);
    Include(LOptions, cvoCheckExtKeyUsage);
  end;
  if (sslCertVerifyCheckRevocation in LVerifyFlags) or
    (sslCertVerifyCheckCRL in LVerifyFlags) then
    Include(LOptions, cvoCheckRevocation);

  LVerifier := TSSLCertificateChainVerifier.Create;
  try
    LVerifier.SetOptions(LOptions);
    if LTrustedStore <> nil then
      LVerifier.SetTrustedStore(LTrustedStore);
    if LIntermediateStore <> nil then
      LVerifier.SetIntermediateStore(LIntermediateStore);
    if LCRLStore <> nil then
      LVerifier.SetCRLStore(LCRLStore);

    LVerifyResult := LVerifier.VerifyCertificate(FPeerCertificate);
  finally
    LVerifier := nil;
    if LCRLStore <> nil then
      LCRLStore.Free;
  end;

  try
    if not LVerifyResult.IsValid then
    begin
      LErrorMessage := Trim(LVerifyResult.ErrorMessage);
      if LErrorMessage = '' then
        LErrorMessage := 'Peer certificate is not trusted';
      if LVerifyResult.RevocationStatus = 1 then
        SetHandshakeError(
          sslErrCertificateRevoked,
          'Peer certificate trust verification failed: ' + LErrorMessage
        )
      else
        SetHandshakeError(
          sslErrCertificateUntrusted,
          'Peer certificate trust verification failed: ' + LErrorMessage
        );
      Exit;
    end;
  finally
    if Assigned(LVerifyResult.Warnings) then
      LVerifyResult.Warnings.Free;
  end;

  if not ValidateCertificatePinIfEnabled then
    Exit;

  Result := True;
end;

function TFreePascalConnection.ValidateCertificatePinIfEnabled: Boolean;
var
  LPinValidator: IFreePascalContextPinValidation;
  LFingerprintHex: string;
  LFingerprint: TBytes;
  I: Integer;
begin
  Result := True;
  if FContext = nil then
    Exit;
  if not FContext.GetCertificatePinningEnabled then
    Exit;
  if not Supports(FContext, IFreePascalContextPinValidation, LPinValidator) then
    Exit;
  if FPeerCertificate = nil then
  begin
    SetHandshakeError(sslErrCertificate, 'Certificate pinning enabled but no peer certificate available');
    Result := False;
    Exit;
  end;
  LFingerprintHex := FPeerCertificate.GetFingerprintSHA256;
  SetLength(LFingerprint, Length(LFingerprintHex) div 2);
  for I := 0 to High(LFingerprint) do
    LFingerprint[I] := StrToInt('$' + Copy(LFingerprintHex, I * 2 + 1, 2));
  if not LPinValidator.ValidateCertificatePin(LFingerprint) then
  begin
    SetHandshakeError(sslErrCertificate, 'Certificate pinning validation failed: peer certificate does not match any configured pin');
    Result := False;
  end;
end;

function TFreePascalConnection.ValidateClientPeerCertificateFlags: Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LVerifyFlags: TSSLCertVerifyFlags;
  LNormalizedHost: string;
begin
  Result := False;

  if FContext = nil then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS context is not available for peer certificate verification');
    Exit;
  end;

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  if FSessionReused then
  begin
    { PSK resumption: skip certificate chain verification but still verify
      that the current connection server name matches the name bound to the
      resumed session. This prevents cross-host session ticket misuse. }
    LVerifyFlags := FContext.GetCertVerifyFlags;
    if not (sslCertVerifyIgnoreHostname in LVerifyFlags) then
    begin
      LNormalizedHost := NormalizeHostForVerify(FServerName);
      if LNormalizedHost = '' then
      begin
        SetHandshakeError(sslErrHostnameMismatch,
          'Resumed session hostname verification requires a non-empty server name');
        Exit;
      end;
      if not SameText(NormalizeHostForVerify(FSessionBoundServerName), LNormalizedHost) then
      begin
        SetHandshakeError(sslErrHostnameMismatch,
          Format('Resumed session was bound to "%s" but current connection targets "%s"',
            [FSessionBoundServerName, FServerName]));
        Exit;
      end;
    end;
    Exit(True);
  end;

  if FPeerCertificate = nil then
  begin
    SetHandshakeError(sslErrCertificate, 'Peer certificate is required for client verification');
    Exit;
  end;

  LVerifyFlags := FContext.GetCertVerifyFlags;
  if not (sslCertVerifyIgnoreHostname in LVerifyFlags) then
  begin
    LNormalizedHost := NormalizeHostForVerify(FServerName);
    if LNormalizedHost = '' then
    begin
      SetHandshakeError(sslErrHostnameMismatch,
        'Peer certificate hostname verification requires a non-empty server name');
      Exit;
    end;

    if not FPeerCertificate.VerifyHostname(LNormalizedHost) then
    begin
      SetHandshakeError(sslErrHostnameMismatch,
        Format('Peer certificate hostname mismatch for "%s"', [LNormalizedHost]));
      Exit;
    end;
  end;

  if not (sslCertVerifyIgnoreExpiry in LVerifyFlags) and FPeerCertificate.IsExpired then
  begin
    SetHandshakeError(sslErrCertificateExpired, 'Peer certificate is expired');
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.ValidateClientOCSPStapling: Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LOptions: TSSLOptions;
  LStaplingConfig: TOCSPStaplingConfig;
  LStaplingClient: TOCSPStaplingClient;
  LStaplingResult: TOCSPStaplingResult;
  LLeafCertificate: TX509Certificate;
  LIssuerCertificate: TX509Certificate;
  LError: string;
  LEnabled: Boolean;
  LRequired: Boolean;
begin
  Result := False;

  if FContext = nil then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS context is not available for OCSP stapling validation');
    Exit;
  end;

  LOptions := FContext.GetOptions;
  LRequired := ssoRequireOCSPStapling in LOptions;
  LEnabled := LRequired or (ssoEnableOCSPStapling in LOptions);
  if not LEnabled then
    Exit(True);

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  if FSessionReused then
    Exit(True);

  if Length(FOCSPResponse) = 0 then
  begin
    FOCSPResponseVerified := False;
    FOCSPResponseStatus := 'No OCSP Response';
    if LRequired then
    begin
      SetHandshakeError(sslErrCertificate, 'Required OCSP stapling response was not provided by the server');
      Exit;
    end;
    Exit(True);
  end;

  if not TryBuildPeerOCSPCertificatePair(LLeafCertificate, LIssuerCertificate, LError) then
  begin
    FOCSPResponseVerified := False;
    FOCSPResponseStatus := 'Verification Failed: ' + LError;
    if LRequired then
    begin
      SetHandshakeError(sslErrCertificate, 'Required OCSP stapling verification context is unavailable: ' + LError);
      Exit;
    end;
    Exit(True);
  end;

  LStaplingConfig := TOCSPStaplingConfig.Default;
  LStaplingConfig.EnableClientRequest := True;
  LStaplingConfig.RequireStapling := LRequired;
  LStaplingConfig.UseCache := False;
  LStaplingClient := TOCSPStaplingClient.Create(LStaplingConfig);
  try
    LStaplingResult := LStaplingClient.ProcessStapledResponse(
      FOCSPResponse,
      LLeafCertificate,
      LIssuerCertificate
    );
    FOCSPResponseVerified := LStaplingResult.IsValid;
    FOCSPResponseStatus := OCSPStaplingStateToString(
      LStaplingResult.Status,
      LStaplingResult.ErrorMessage
    );

    if not LStaplingClient.ValidateStaplingRequirement(True) then
    begin
      if LRequired then
      begin
        SetHandshakeError(
          sslErrCertificate,
          'Required OCSP stapling validation failed: ' + FOCSPResponseStatus
        );
        Exit;
      end;
    end;
  finally
    LStaplingClient.Free;
    LLeafCertificate.Free;
    LIssuerCertificate.Free;
  end;

  Result := True;
end;

function TFreePascalConnection.ValidateClientOnlineOCSP: Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LVerifyFlags: TSSLCertVerifyFlags;
  LLeafCertificate: TX509Certificate;
  LIssuerCertificate: TX509Certificate;
  LLeafOpenSSLCertificate: ISSLCertificate;
  LIssuerOpenSSLCertificate: ISSLCertificate;
  LIssuerSource: ISSLCertificate;
  LLeafX509: PX509;
  LIssuerX509: PX509;
  LHTTPHooksAccess: ISSLHttpHooksAccess;
  LHTTPHooks: TSSLHTTPHooks;
  LHTTPHooksScope: TSSLHTTPHooksScope;
  LOCSPURL: string;
  LError: string;
  LOCSPCheck: TOCSPCheckResult;
  LTimeoutSec: Integer;
begin
  Result := False;

  if FContext = nil then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS context is not available for online OCSP verification');
    Exit;
  end;

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  if FSessionReused then
    Exit(True);

  LVerifyFlags := FContext.GetCertVerifyFlags;
  if not (sslCertVerifyCheckOCSP in LVerifyFlags) then
    Exit(True);

  if FPeerCertificate = nil then
  begin
    SetHandshakeError(sslErrCertificate, 'Peer certificate is required for online OCSP verification');
    Exit;
  end;

  if not TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
  begin
    SetHandshakeError(sslErrUnsupported,
      'Online OCSP verification requires the OpenSSL helper library');
    Exit;
  end;

  if not TryBuildPeerOCSPCertificatePair(LLeafCertificate, LIssuerCertificate, LError) then
  begin
    SetHandshakeError(sslErrCertificate,
      'Peer certificate online OCSP context is unavailable: ' + LError);
    Exit;
  end;

  try
    LOCSPURL := Trim(GetOCSPURLFromCertificate(LLeafCertificate));
  finally
    LLeafCertificate.Free;
    LIssuerCertificate.Free;
  end;

  if LOCSPURL = '' then
  begin
    SetHandshakeError(sslErrVerificationFailed,
      'Peer certificate OCSP responder URL was not found in AIA');
    Exit;
  end;

  if not LoadOpenSSLOCSP(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
  begin
    SetHandshakeError(sslErrUnsupported,
      'Online OCSP verification helper is unavailable');
    Exit;
  end;

  if not TryCreateOpenSSLCertificateFromCertificate(
    FPeerCertificate,
    LLeafOpenSSLCertificate,
    LLeafX509,
    LError
  ) then
  begin
    SetHandshakeError(sslErrVerificationFailed,
      'Online OCSP verification could not materialize peer certificate: ' + LError);
    Exit;
  end;

  if not TryResolvePeerIssuerCertificate(LIssuerSource, LError) then
  begin
    SetHandshakeError(sslErrVerificationFailed,
      'Online OCSP verification could not resolve issuer certificate: ' + LError);
    Exit;
  end;

  if not TryCreateOpenSSLCertificateFromCertificate(
    LIssuerSource,
    LIssuerOpenSSLCertificate,
    LIssuerX509,
    LError
  ) then
  begin
    SetHandshakeError(sslErrVerificationFailed,
      'Online OCSP verification could not materialize issuer certificate: ' + LError);
    Exit;
  end;

  LTimeoutSec := 10;
  if FTimeout > 0 then
  begin
    LTimeoutSec := FTimeout div 1000;
    if LTimeoutSec <= 0 then
      LTimeoutSec := 1;
  end;

  LHTTPHooks := TSSLHTTPHooks.Empty;
  if Supports(FContext, ISSLHttpHooksAccess, LHTTPHooksAccess) then
    LHTTPHooks := TSSLHTTPHooks.Create(
      LHTTPHooksAccess.GetHTTPGetCallback,
      LHTTPHooksAccess.GetHTTPPostCallback
    );

  if not LHTTPHooks.IsEmpty then
  begin
    LHTTPHooksScope := TSSLHTTPHooksScope.Push(LHTTPHooks);
    try
      LOCSPCheck := CheckCertificateStatusDetailed(
        LLeafX509,
        LIssuerX509,
        LOCSPURL,
        LTimeoutSec,
        nil
      );
    finally
      LHTTPHooksScope.Pop;
    end;
  end
  else
    LOCSPCheck := CheckCertificateStatusDetailed(
      LLeafX509,
      LIssuerX509,
      LOCSPURL,
      LTimeoutSec,
      nil
    );

  if not LOCSPCheck.Verified then
  begin
    LError := Trim(LOCSPCheck.ErrorMessage);
    if LError = '' then
      LError := 'OCSP verification failed';
    SetHandshakeError(
      sslErrVerificationFailed,
      'Peer certificate online OCSP verification failed: ' + LError
    );
    Exit;
  end;

  case LOCSPCheck.CertStatus of
    V_OCSP_CERTSTATUS_GOOD:
      Result := True;
    V_OCSP_CERTSTATUS_REVOKED:
      SetHandshakeError(sslErrCertificateRevoked,
        'Peer certificate has been revoked (OCSP)');
    V_OCSP_CERTSTATUS_UNKNOWN:
      SetHandshakeError(sslErrCertificateUnknown,
        'Peer certificate OCSP status is unknown');
  else
    SetHandshakeError(sslErrVerificationFailed,
      'Peer certificate OCSP verification failed');
  end;
end;

function TFreePascalConnection.ValidateClientCertificateTransparency: Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LStatus: string;
begin
  Result := False;

  if FContext = nil then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS context is not available for certificate transparency validation');
    Exit;
  end;

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  if FSessionReused then
    Exit(True);

  if not (ssoRequireCertificateTransparency in FContext.GetOptions) then
    Exit(True);

  if Length(FSignedCertificateTimestampList) = 0 then
  begin
    SetHandshakeError(
      sslErrCertificate,
      'Required certificate transparency SCT list was not provided by the server'
    );
    Exit;
  end;

  if not FHasCertificateTransparencyValidationResult then
  begin
    LStatus := Trim(FCertificateTransparencyValidationStatus);
    if LStatus = '' then
      LStatus := 'No validation result';
    SetHandshakeError(
      sslErrCertificate,
      'Required certificate transparency validation is unavailable: ' + LStatus
    );
    Exit;
  end;

  if not FCertificateTransparencyPolicySatisfied then
  begin
    LStatus := Trim(FCertificateTransparencyValidationStatus);
    if LStatus = '' then
      LStatus := 'Policy not satisfied';
    SetHandshakeError(
      sslErrCertificate,
      'Required certificate transparency policy failed: ' + LStatus
    );
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.SendPostHandshakeKeyUpdate(ARequestPeerUpdate: Boolean): Boolean;
var
  LHandshakeMessage: TBytes;
  LInnerPlaintext: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
  LRequestValue: Byte;
begin
  Result := False;

  if not FApplicationSecrets.Valid then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.3 application secrets are not ready for KeyUpdate');
    Exit;
  end;

  if not TLS13AEADIsSupported(FApplicationSecrets.CipherSuite) then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      Format('Cipher suite %s is unsupported for TLS 1.3 KeyUpdate',
        [TLS13CipherSuiteToString(FApplicationSecrets.CipherSuite)])
    );
    Exit;
  end;

  LRequestValue := 0;
  if ARequestPeerUpdate then
    LRequestValue := 1;

  SetLength(LHandshakeMessage, 0);
  AppendByte(LHandshakeMessage, TLS_HANDSHAKE_TYPE_KEY_UPDATE);
  AppendUInt24(LHandshakeMessage, 1);
  AppendByte(LHandshakeMessage, LRequestValue);

  LInnerPlaintext := BuildTLS13InnerPlaintext(LHandshakeMessage, TLS_CONTENT_TYPE_HANDSHAKE);

  if FIsServerMode then
  begin
    try
      LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ServerApplicationIV, FServerApplicationSeq);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrProtocol, 'Failed to build server application nonce for KeyUpdate: ' + E.Message);
        Exit;
      end;
    end;

    LAAD := BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(FApplicationSecrets.CipherSuite)));
    if not TryTLS13AEADEncrypt(
      FApplicationSecrets.CipherSuite,
      FApplicationSecrets.ServerApplicationKey,
      LNonce,
      LAAD,
      LInnerPlaintext,
      LEncrypted,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS KeyUpdate record: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendAll(LRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send TLS KeyUpdate record');
      Exit;
    end;

    if not IncrementTLS13Sequence(FServerApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Server application sequence overflow during KeyUpdate');
      Exit;
    end;

    if not TryUpdateTLS13ServerApplicationWriteKeys(FApplicationSecrets, LError) then
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to rotate server application write key: ' + LError);
      Exit;
    end;

    FServerApplicationSeq := 0;
  end
  else
  begin
    try
      LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ClientApplicationIV, FClientApplicationSeq);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrProtocol, 'Failed to build client application nonce for KeyUpdate: ' + E.Message);
        Exit;
      end;
    end;

    LAAD := BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(FApplicationSecrets.CipherSuite)));
    if not TryTLS13AEADEncrypt(
      FApplicationSecrets.CipherSuite,
      FApplicationSecrets.ClientApplicationKey,
      LNonce,
      LAAD,
      LInnerPlaintext,
      LEncrypted,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS KeyUpdate record: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendAll(LRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send TLS KeyUpdate record');
      Exit;
    end;

    if not IncrementTLS13Sequence(FClientApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Client application sequence overflow during KeyUpdate');
      Exit;
    end;

    if not TryUpdateTLS13ClientApplicationWriteKeys(FApplicationSecrets, LError) then
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to rotate client application write key: ' + LError);
      Exit;
    end;

    FClientApplicationSeq := 0;
  end;

  Result := True;
end;

function TFreePascalConnection.ValidateServerCertificateVerify(
  ACipherSuite: Word;
  const AHandshakeMessage: TBytes;
  const ATranscriptData: TBytes
): Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LSignatureScheme: Word;
  LSignature: TBytes;
  LTranscriptHash: TBytes;
  LCertVerifyInput: TBytes;
  LLeafCertificate: TX509Certificate;
  LError: string;
  LFailureCode: TSSLErrorCode;
begin
  Result := False;

  if FContext = nil then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS context is not available for CertificateVerify validation');
    Exit;
  end;

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  if FSessionReused then
    Exit(True);

  if not TryParseTLS13CertificateVerifyHandshake(
    AHandshakeMessage,
    LSignatureScheme,
    LSignature,
    LError
  ) then
  begin
    SetHandshakeError(sslErrProtocol, 'Invalid CertificateVerify: ' + LError);
    Exit;
  end;

  if FPeerCertificate = nil then
  begin
    SetHandshakeError(sslErrCertificate, 'Peer certificate is required for CertificateVerify validation');
    Exit;
  end;

  if not TryLoadX509Certificate(FPeerCertificate, LLeafCertificate, LError) then
  begin
    SetHandshakeError(sslErrCertificate, 'Failed to parse peer certificate for CertificateVerify: ' + LError);
    Exit;
  end;

  try
    LTranscriptHash := HashTLS13TranscriptForSuite(ACipherSuite, ATranscriptData);
    if Length(LTranscriptHash) = 0 then
    begin
      SetHandshakeError(
        sslErrUnsupported,
        'Unsupported TLS 1.3 cipher suite for CertificateVerify transcript hashing: ' +
        TLS13CipherSuiteToString(ACipherSuite)
      );
      Exit;
    end;

    LCertVerifyInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

    if not TryVerifyTLS13CertificateVerifySignature(
      LSignatureScheme,
      LLeafCertificate.PublicKeyInfo,
      LCertVerifyInput,
      LSignature,
      LError
    ) then
    begin
      if Pos('unsupported', LowerCase(LError)) > 0 then
        LFailureCode := sslErrUnsupported
      else
        LFailureCode := sslErrHandshake;

      SetHandshakeError(
        LFailureCode,
        'Server CertificateVerify verification failed: ' + LError
      );
      Exit;
    end;
  finally
    LLeafCertificate.Free;
  end;

  Result := True;
end;

function TFreePascalConnection.ProcessEncryptedServerFlight(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
var
  LHeader: TTLSRecordHeader;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  LPlaintext: TBytes;
  LInnerFragment: TBytes;
  LHandshakeBuffer: TBytes;
  LHandshakeMessage: TBytes;
  LInnerContentType: Byte;
  LRecordIndex: Integer;
  LError: string;
  LEncryptedExtensionsInfo: TTLS13EncryptedExtensionsInfo;
  LMsgType: Byte;
  LMsgLen: Cardinal;
  LVerifyData: TBytes;
  LTranscriptHash: TBytes;
  LRequireCertificateFlight: Boolean;
  LRequestCertificateTransparency: Boolean;
  LSeenServerCertificate: Boolean;
  LSeenServerCertificateVerify: Boolean;
begin
  Result := False;
  SetLength(LHandshakeBuffer, 0);
  FServerHandshakeSeq := 0;
  LRequireCertificateFlight :=
    (not FSessionReused) and
    (FContext <> nil) and
    (sslVerifyPeer in FContext.GetVerifyMode);
  LRequestCertificateTransparency :=
    (FContext <> nil) and
    (sslVerifyPeer in FContext.GetVerifyMode);
  LSeenServerCertificate := False;
  LSeenServerCertificateVerify := False;

  for LRecordIndex := 1 to 96 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to receive encrypted handshake record');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned TLS alert during encrypted handshake');
          Exit;
        end;

      TLS_CONTENT_TYPE_APPLICATION_DATA:
        begin
          if not TLS13AEADIsSupported(ACipherSuite) then
          begin
            SetHandshakeError(
              sslErrUnsupported,
              Format('Cipher suite %s is unsupported by pure FreePascal encrypted handshake path',
                [TLS13CipherSuiteToString(ACipherSuite)])
            );
            Exit;
          end;

          LAAD := BuildTLS13RecordAAD(LHeader.Length);
          try
            LNonce := BuildTLS13RecordNonce(FHandshakeSecrets.ServerHandshakeIV, FServerHandshakeSeq);
          except
            on E: Exception do
            begin
              SetHandshakeError(sslErrProtocol, 'Failed to build server handshake nonce: ' + E.Message);
              Exit;
            end;
          end;

          if not IncrementTLS13Sequence(FServerHandshakeSeq) then
          begin
            SetHandshakeError(sslErrProtocol, 'Server handshake sequence overflow');
            Exit;
          end;

          if not TryTLS13AEADDecrypt(
            ACipherSuite,
            FHandshakeSecrets.ServerHandshakeKey,
            LNonce,
            LAAD,
            LPayloadBytes,
            LPlaintext,
            LError
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt server handshake record: ' + LError);
            Exit;
          end;

          if not TryParseTLS13InnerPlaintext(LPlaintext, LInnerFragment, LInnerContentType) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid TLSInnerPlaintext from server handshake record');
            Exit;
          end;

          case LInnerContentType of
            TLS_CONTENT_TYPE_HANDSHAKE:
              begin
                AppendHandshakeBytes(LHandshakeBuffer, LInnerFragment);

                while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
                begin
                  LMsgType := LHandshakeMessage[0];

                  if LMsgType = TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS then
                  begin
                    if not TryParseTLS13EncryptedExtensions(
                      LHandshakeMessage,
                      LEncryptedExtensionsInfo,
                      LError
                    ) then
                    begin
                      SetHandshakeError(sslErrProtocol, 'Invalid EncryptedExtensions: ' + LError);
                      Exit;
                    end;

                    if FEarlyDataStatus = sslEarlyDataQueued then
                    begin
                      if LEncryptedExtensionsInfo.HasEarlyData then
                        FEarlyDataStatus := sslEarlyDataAccepted
                      else
                        FEarlyDataStatus := sslEarlyDataRejected;
                    end
                    else if LEncryptedExtensionsInfo.HasEarlyData then
                    begin
                      SetHandshakeError(
                        sslErrProtocol,
                        'Server accepted early_data even though client did not queue early data'
                      );
                      Exit;
                    end;

                    if LEncryptedExtensionsInfo.HasALPN then
                      FSelectedALPNProtocol := string(LEncryptedExtensionsInfo.SelectedALPNProtocol);

                    AppendHandshakeBytes(ATranscriptData, LHandshakeMessage);
                  end
                  else if LMsgType = TLS_HANDSHAKE_TYPE_CERTIFICATE then
                  begin
                    if not TryCachePeerCertificatesFromHandshake(
                      LHandshakeMessage,
                      LRequestCertificateTransparency,
                      LError
                    ) then
                    begin
                      SetHandshakeError(sslErrProtocol, 'Invalid Certificate: ' + LError);
                      Exit;
                    end;

                    LSeenServerCertificate := True;
                    AppendHandshakeBytes(ATranscriptData, LHandshakeMessage);
                  end
                  else if LMsgType = TLS_HANDSHAKE_TYPE_CERTIFICATE_VERIFY then
                  begin
                    if not ValidateServerCertificateVerify(ACipherSuite, LHandshakeMessage, ATranscriptData) then
                      Exit;

                    LSeenServerCertificateVerify := True;
                    AppendHandshakeBytes(ATranscriptData, LHandshakeMessage);
                  end
                  else if LMsgType = TLS_HANDSHAKE_TYPE_FINISHED then
                  begin
                    if LRequireCertificateFlight and
                      ((not LSeenServerCertificate) or (not LSeenServerCertificateVerify)) then
                    begin
                      SetHandshakeError(
                        sslErrProtocol,
                        'Server full handshake missing Certificate or CertificateVerify before Finished'
                      );
                      Exit;
                    end;

                    LMsgLen := ReadUInt24(LHandshakeMessage, 1);
                    if LMsgLen <> Cardinal(FHandshakeSecrets.HashSize) then
                    begin
                      SetHandshakeError(
                        sslErrProtocol,
                        Format('Server Finished length mismatch (expected=%d actual=%d)',
                          [FHandshakeSecrets.HashSize, Integer(LMsgLen)])
                      );
                      Exit;
                    end;

                    SetLength(LVerifyData, Integer(LMsgLen));
                    if Integer(LMsgLen) > 0 then
                      Move(LHandshakeMessage[4], LVerifyData[0], Integer(LMsgLen));

                    LTranscriptHash := HashTLS13TranscriptForSuite(ACipherSuite, ATranscriptData);
                    if not TLS13VerifyFinishedForCipherSuite(
                      ACipherSuite,
                      FHandshakeSecrets.ServerHandshakeTrafficSecret,
                      LTranscriptHash,
                      LVerifyData
                    ) then
                    begin
                      SetHandshakeError(sslErrHandshake, 'Server Finished verification failed');
                      Exit;
                    end;

                    AppendHandshakeBytes(ATranscriptData, LHandshakeMessage);
                    Result := True;
                    Exit;
                  end
                  else
                    AppendHandshakeBytes(ATranscriptData, LHandshakeMessage);
                end;
              end;

            TLS_CONTENT_TYPE_ALERT:
              begin
                SetHandshakeError(sslErrHandshake, 'Received TLS alert content inside encrypted handshake record');
                Exit;
              end;

          else
            begin
              SetHandshakeError(
                sslErrProtocol,
                Format('Unexpected inner content type %d during encrypted handshake', [LInnerContentType])
              );
              Exit;
            end;
          end;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d during encrypted handshake', [LHeader.ContentType])
        );
        Exit;
      end;
    end;
  end;

  SetHandshakeError(sslErrProtocol, 'Server Finished not received within encrypted handshake record budget');
end;

function TFreePascalConnection.SendClientFinished(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
var
  LClientFlight: TBytes;
  LTranscriptForFinished: TBytes;
  LTranscriptHash: TBytes;
  LVerifyData: TBytes;
  LFinishedHandshake: TBytes;
  LInnerPlaintext: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
begin
  Result := False;

  if not TLS13AEADIsSupported(ACipherSuite) then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      Format('Cipher suite %s is unsupported for client Finished encryption',
        [TLS13CipherSuiteToString(ACipherSuite)])
    );
    Exit;
  end;

  SetLength(LClientFlight, 0);
  if FEarlyDataStatus = sslEarlyDataAccepted then
    AppendHandshakeBytes(LClientFlight, BuildTLS13EndOfEarlyDataHandshake);

  LTranscriptForFinished := Copy(ATranscriptData, 0, Length(ATranscriptData));
  if Length(LClientFlight) > 0 then
    AppendHandshakeBytes(LTranscriptForFinished, LClientFlight);

  LTranscriptHash := HashTLS13TranscriptForSuite(ACipherSuite, LTranscriptForFinished);
  LVerifyData := TLS13ComputeFinishedVerifyDataForCipherSuite(
    ACipherSuite,
    FClientFinishedKey,
    LTranscriptHash
  );

  SetLength(LFinishedHandshake, 0);
  AppendByte(LFinishedHandshake, TLS_HANDSHAKE_TYPE_FINISHED);
  AppendUInt24(LFinishedHandshake, Length(LVerifyData));
  AppendHandshakeBytes(LFinishedHandshake, LVerifyData);

  AppendHandshakeBytes(LClientFlight, LFinishedHandshake);
  LInnerPlaintext := BuildTLS13InnerPlaintext(LClientFlight, TLS_CONTENT_TYPE_HANDSHAKE);

  try
    LNonce := BuildTLS13RecordNonce(FHandshakeSecrets.ClientHandshakeIV, FClientHandshakeSeq);
  except
    on E: Exception do
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to build client handshake nonce: ' + E.Message);
      Exit;
    end;
  end;

  if not TryTLS13AEADEncrypt(
    ACipherSuite,
    FHandshakeSecrets.ClientHandshakeKey,
    LNonce,
    BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(ACipherSuite))),
    LInnerPlaintext,
    LEncrypted,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt client Finished: ' + LError);
    Exit;
  end;

  if not IncrementTLS13Sequence(FClientHandshakeSeq) then
  begin
    SetHandshakeError(sslErrProtocol, 'Client handshake sequence overflow');
    Exit;
  end;

  LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
  if not SendAll(LRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send encrypted client Finished record');
    Exit;
  end;

  if FEarlyDataStatus = sslEarlyDataAccepted then
    AppendHandshakeBytes(ATranscriptData, BuildTLS13EndOfEarlyDataHandshake);
  AppendHandshakeBytes(ATranscriptData, LFinishedHandshake);
  Result := True;
end;

function TFreePascalConnection.RecvApplicationDataFragment(
  out AFragment: TBytes;
  AAllowNoRecord: Boolean
): Boolean;
var
  LHeader: TTLSRecordHeader;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  LPlaintext: TBytes;
  LInnerFragment: TBytes;
  LInnerContentType: Byte;
  LRecordIndex: Integer;
  LError: string;
  LAlertLevel: Byte;
  LAlertDescription: Byte;
begin
  SetLength(AFragment, 0);
  Result := False;

  if not FApplicationSecrets.Valid then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.3 application secrets are not ready');
    Exit;
  end;

  for LRecordIndex := 1 to 128 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if AAllowNoRecord and (FStream <> nil) and (GetBufferedStreamBytesAvailable <= 0) then
      begin
        Result := True;
        Exit;
      end;
      SetHandshakeError(sslErrIO, 'Failed to receive TLS application record');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned plaintext TLS alert during application data phase');
          Exit;
        end;

      TLS_CONTENT_TYPE_APPLICATION_DATA:
        begin
          LAAD := BuildTLS13RecordAAD(LHeader.Length);

          if FIsServerMode then
          begin
            try
              LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ClientApplicationIV, FClientApplicationSeq);
            except
              on E: Exception do
              begin
                SetHandshakeError(sslErrProtocol, 'Failed to build client application nonce: ' + E.Message);
                Exit;
              end;
            end;

            if not IncrementTLS13Sequence(FClientApplicationSeq) then
            begin
              SetHandshakeError(sslErrProtocol, 'Client application sequence overflow');
              Exit;
            end;

            if not TryTLS13AEADDecrypt(
              FApplicationSecrets.CipherSuite,
              FApplicationSecrets.ClientApplicationKey,
              LNonce,
              LAAD,
              LPayloadBytes,
              LPlaintext,
              LError
            ) then
            begin
              SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS application record: ' + LError);
              Exit;
            end;
          end
          else
          begin
            try
              LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ServerApplicationIV, FServerApplicationSeq);
            except
              on E: Exception do
              begin
                SetHandshakeError(sslErrProtocol, 'Failed to build server application nonce: ' + E.Message);
                Exit;
              end;
            end;

            if not IncrementTLS13Sequence(FServerApplicationSeq) then
            begin
              SetHandshakeError(sslErrProtocol, 'Server application sequence overflow');
              Exit;
            end;

            if not TryTLS13AEADDecrypt(
              FApplicationSecrets.CipherSuite,
              FApplicationSecrets.ServerApplicationKey,
              LNonce,
              LAAD,
              LPayloadBytes,
              LPlaintext,
              LError
            ) then
            begin
              SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS application record: ' + LError);
              Exit;
            end;
          end;

          if not TryParseTLS13InnerPlaintext(LPlaintext, LInnerFragment, LInnerContentType) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid TLSInnerPlaintext in application data phase');
            Exit;
          end;

          case LInnerContentType of
            TLS_CONTENT_TYPE_APPLICATION_DATA:
              begin
                AFragment := LInnerFragment;
                Result := True;
                Exit;
              end;

            TLS_CONTENT_TYPE_HANDSHAKE:
              begin
                if not ProcessPostHandshakeFragment(LInnerFragment) then
                  Exit;
                if AAllowNoRecord then
                begin
                  Result := True;
                  Exit;
                end;
                Continue;
              end;

            TLS_CONTENT_TYPE_ALERT:
              begin
                if Length(LInnerFragment) >= 2 then
                begin
                  LAlertLevel := LInnerFragment[0];
                  LAlertDescription := LInnerFragment[1];
                  SetHandshakeError(
                    sslErrHandshake,
                    Format('Peer sent encrypted alert (level=%d description=%d)', [LAlertLevel, LAlertDescription])
                  );
                end
                else
                  SetHandshakeError(sslErrHandshake, 'Peer sent malformed encrypted alert');
                Exit;
              end;

          else
            begin
              SetHandshakeError(
                sslErrProtocol,
                Format('Unexpected inner content type %d in application data phase', [LInnerContentType])
              );
              Exit;
            end;
          end;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d in application data phase', [LHeader.ContentType])
        );
        Exit;
      end;
    end;
  end;

  SetHandshakeError(sslErrProtocol, 'Application data record not received within processing budget');
end;

function TFreePascalConnection.SendApplicationDataFragment(const AFragment: TBytes): Boolean;
var
  LInnerPlaintext: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
begin
  Result := False;

  if not FApplicationSecrets.Valid then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.3 application secrets are not ready');
    Exit;
  end;

  if not TLS13AEADIsSupported(FApplicationSecrets.CipherSuite) then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      Format('Cipher suite %s is unsupported in pure FreePascal application data path',
        [TLS13CipherSuiteToString(FApplicationSecrets.CipherSuite)])
    );
    Exit;
  end;

  LInnerPlaintext := BuildTLS13InnerPlaintext(AFragment, TLS_CONTENT_TYPE_APPLICATION_DATA);

  if FIsServerMode then
  begin
    try
      LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ServerApplicationIV, FServerApplicationSeq);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrProtocol, 'Failed to build server application nonce: ' + E.Message);
        Exit;
      end;
    end;

    LAAD := BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(FApplicationSecrets.CipherSuite)));
    if not TryTLS13AEADEncrypt(
      FApplicationSecrets.CipherSuite,
      FApplicationSecrets.ServerApplicationKey,
      LNonce,
      LAAD,
      LInnerPlaintext,
      LEncrypted,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS application record: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendAll(LRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send TLS application record');
      Exit;
    end;

    if not IncrementTLS13Sequence(FServerApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Server application sequence overflow');
      Exit;
    end;
  end
  else
  begin
    try
      LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ClientApplicationIV, FClientApplicationSeq);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrProtocol, 'Failed to build client application nonce: ' + E.Message);
        Exit;
      end;
    end;

    LAAD := BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(FApplicationSecrets.CipherSuite)));
    if not TryTLS13AEADEncrypt(
      FApplicationSecrets.CipherSuite,
      FApplicationSecrets.ClientApplicationKey,
      LNonce,
      LAAD,
      LInnerPlaintext,
      LEncrypted,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS application record: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendAll(LRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send TLS application record');
      Exit;
    end;

    if not IncrementTLS13Sequence(FClientApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Client application sequence overflow');
      Exit;
    end;
  end;

  Result := True;
end;

function TFreePascalConnection.ProbeServerHello: Boolean;
var
  LClientHelloHandshake: TBytes;
  LClientHelloRecord: TBytes;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LHeader: TTLSRecordHeader;
  LHandshake: TBytes;
  LServerHello: TTLS13ServerHelloInfo;
  LRecordIndex: Integer;
  LTranscriptData: TBytes;
  LKeyScheduleError: string;
  LConfiguredResumption: IFreePascalResumptionSession;
  LEarlyDataContext: ISSLEarlyDataContext;
  LUseConfiguredSession: Boolean;
  LPartialClientHello: TBytes;
  LSessionAgeMs: Int64;
  LWantEarlyData: Boolean;
  LWantOCSPStapling: Boolean;
  LWantCertificateTransparency: Boolean;
  LConfiguredCipherSuites: TTLS13CipherSuiteList;
begin
  Result := False;
  FSelectedALPNProtocol := '';
  ClearPeerCertificateCache;
  SetLength(FHandshakeSharedSecret, 0);
  InitTLS13EarlyDataSecrets(FEarlyDataSecrets);
  ClearTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FEarlyDataSeq := 0;
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  ClearTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
  FSessionReused := False;
  if FEarlyDataStatus <> sslEarlyDataQueued then
    FEarlyDataStatus := sslEarlyDataNone;

  try
    FX25519PrivateKey := GenerateX25519PrivateKey;
    FX25519PublicKey := X25519PublicKeyFromPrivate(FX25519PrivateKey);
  except
    on E: Exception do
    begin
      FLastErrorCode := sslErrHandshake;
      FLastErrorString := 'Failed to generate X25519 key share: ' + E.Message;
      RecordError(FLastErrorCode, FLastErrorString);
      Exit;
    end;
  end;

  LUseConfiguredSession :=
    Supports(FConfiguredSession, IFreePascalResumptionSession, LConfiguredResumption) and
    (FConfiguredSession <> nil) and
    FConfiguredSession.IsValid and
    FConfiguredSession.IsResumable and
    (Length(LConfiguredResumption.GetTicket) > 0) and
    (Length(LConfiguredResumption.GetResumptionPSK) > 0);
  FSessionReused := False;
  if LUseConfiguredSession then
    FEarlyDataLimit := LConfiguredResumption.GetMaxEarlyDataSize
  else
    FEarlyDataLimit := 0;

  LWantEarlyData :=
    (FEarlyDataStatus = sslEarlyDataQueued) and
    (Length(FEarlyDataPayload) > 0) and
    LUseConfiguredSession and
    (FEarlyDataLimit > 0) and
    Supports(FContext, ISSLEarlyDataContext, LEarlyDataContext) and
    LEarlyDataContext.GetClientEarlyDataEnabled;
  LWantOCSPStapling :=
    (FContext <> nil) and
    ((ssoEnableOCSPStapling in FContext.GetOptions) or
    (ssoRequireOCSPStapling in FContext.GetOptions));
  LWantCertificateTransparency :=
    (FContext <> nil) and
    (sslVerifyPeer in FContext.GetVerifyMode);


  if (FContext <> nil) and (FContext.GetCipherSuites <> '') then
    LConfiguredCipherSuites := ParseTLS13CipherSuiteString(FContext.GetCipherSuites)
  else
    SetLength(LConfiguredCipherSuites, 0);
  if LUseConfiguredSession then
  begin
    LSessionAgeMs := MilliSecondsBetween(Now, FConfiguredSession.GetCreationTime);
    if LSessionAgeMs < 0 then
      LSessionAgeMs := 0;

    LClientHelloHandshake := BuildTLS13ClientHelloHandshakeWithComputedPSKBinderAndCiphers(
      FServerName,
      FALPNProtocols,
      FX25519PublicKey,
      LConfiguredResumption.GetCipherSuite,
      LConfiguredResumption.GetTicket,
      Cardinal((QWord(LSessionAgeMs) + QWord(LConfiguredResumption.GetTicketAgeAdd)) and $FFFFFFFF),
      LConfiguredResumption.GetResumptionPSK,
      LConfiguredCipherSuites,
      LPartialClientHello,
      LWantEarlyData,
      LWantOCSPStapling,
      LWantCertificateTransparency
    );

    if Length(LClientHelloHandshake) = 0 then
    begin
      LUseConfiguredSession := False;
      LWantEarlyData := False;
    end
  end;

  if not LUseConfiguredSession then
    LClientHelloHandshake := BuildTLS13ClientHelloHandshakeWithCiphers(
      FServerName,
      FALPNProtocols,
      FX25519PublicKey,
      LConfiguredCipherSuites,
      LWantOCSPStapling,
      LWantCertificateTransparency
    );

  LClientHelloRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_HANDSHAKE, LClientHelloHandshake);

  if not SendAll(LClientHelloRecord) then
  begin
    FLastErrorCode := sslErrIO;
    FLastErrorString := 'Failed to send TLS ClientHello';
    RecordError(FLastErrorCode, FLastErrorString);
    Exit;
  end;

  if LWantEarlyData then
  begin
    if not TryDeriveTLS13ClientEarlyDataSecrets(
      LConfiguredResumption.GetCipherSuite,
      LConfiguredResumption.GetResumptionPSK,
      LClientHelloHandshake,
      FEarlyDataSecrets,
      LKeyScheduleError
    ) then
    begin
      SetHandshakeError(
        sslErrUnsupported,
        'TLS 1.3 client early-data key schedule derivation failed: ' + LKeyScheduleError
      );
      Exit;
    end;

    if not SendClientEarlyDataRecord(LConfiguredResumption.GetCipherSuite) then
      Exit;
  end;

  for LRecordIndex := 1 to 8 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      FLastErrorCode := sslErrIO;
      FLastErrorString := 'Failed to receive TLS record during handshake';
      RecordError(FLastErrorCode, FLastErrorString);
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_ALERT:
        begin
          FLastErrorCode := sslErrHandshake;
          FLastErrorString := 'Peer returned TLS alert after ClientHello';
          RecordError(FLastErrorCode, FLastErrorString);
          Exit;
        end;

      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          if not TryExtractHandshakePayloadFromRecord(LRecordBytes, LHandshake) then
          begin
            FLastErrorCode := sslErrProtocol;
            FLastErrorString := 'Peer handshake record format is invalid';
            RecordError(FLastErrorCode, FLastErrorString);
            Exit;
          end;

          if not TryParseServerHelloFromHandshake(LHandshake, LServerHello) then
            Continue;

          if LServerHello.SelectedVersion <> TLS13_VERSION then
          begin
            FLastErrorCode := sslErrProtocol;
            FLastErrorString := 'Server did not negotiate TLS 1.3';
            RecordError(FLastErrorCode, FLastErrorString);
            Exit;
          end;

          if not LServerHello.HasKeyShare then
          begin
            FLastErrorCode := sslErrProtocol;
            FLastErrorString := 'ServerHello missing key_share extension';
            RecordError(FLastErrorCode, FLastErrorString);
            Exit;
          end;

          if LServerHello.KeyShareGroup <> TLS13_GROUP_X25519 then
          begin
            FLastErrorCode := sslErrUnsupported;
            FLastErrorString := 'Only X25519 key_share is supported by FreePascal backend';
            RecordError(FLastErrorCode, FLastErrorString);
            Exit;
          end;

          if Length(LServerHello.PeerKeyShare) <> 32 then
          begin
            FLastErrorCode := sslErrProtocol;
            FLastErrorString := 'Invalid X25519 key_share length from server';
            RecordError(FLastErrorCode, FLastErrorString);
            Exit;
          end;

          try
            FHandshakeSharedSecret := X25519ComputeSharedSecret(FX25519PrivateKey, LServerHello.PeerKeyShare);
          except
            on E: Exception do
            begin
              FLastErrorCode := sslErrHandshake;
              FLastErrorString := 'Failed to compute X25519 shared secret: ' + E.Message;
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;
          end;

          SetLength(LTranscriptData, Length(LClientHelloHandshake) + Length(LHandshake));
          if Length(LClientHelloHandshake) > 0 then
            Move(LClientHelloHandshake[0], LTranscriptData[0], Length(LClientHelloHandshake));
          if Length(LHandshake) > 0 then
            Move(LHandshake[0], LTranscriptData[Length(LClientHelloHandshake)], Length(LHandshake));

          if LServerHello.HasPreSharedKey then
          begin
            if (not LUseConfiguredSession) or (LConfiguredResumption = nil) then
            begin
              FLastErrorCode := sslErrProtocol;
              FLastErrorString := 'Server selected pre_shared_key without a configured resumable session';
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;

            if LServerHello.SelectedPSKIdentity <> 0 then
            begin
              FLastErrorCode := sslErrProtocol;
              FLastErrorString := 'Server selected unsupported PSK identity index';
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;

            if not TLS13CipherSuitesShareHash(
              LServerHello.SelectedCipherSuite,
              LConfiguredResumption.GetCipherSuite
            ) then
            begin
              FLastErrorCode := sslErrProtocol;
              FLastErrorString := 'Server selected pre_shared_key with incompatible hash path';
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;

            if not TryDeriveTLS13HandshakeSecretsWithPSK(
              LServerHello.SelectedCipherSuite,
              FHandshakeSharedSecret,
              LTranscriptData,
              LConfiguredResumption.GetResumptionPSK,
              FHandshakeSecrets,
              LKeyScheduleError
            ) then
            begin
              FLastErrorCode := sslErrUnsupported;
              FLastErrorString := 'TLS 1.3 PSK key schedule derivation failed: ' + LKeyScheduleError;
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;

            FSessionReused := True;
            if (FConfiguredSession <> nil) and
              ((FConfiguredSession as TObject) is TFreePascalSession) then
              FSessionBoundServerName := (FConfiguredSession as TObject as TFreePascalSession).BoundServerName
            else
              FSessionBoundServerName := FServerName;
          end
          else
          begin
            if not TryDeriveTLS13HandshakeSecrets(
              LServerHello.SelectedCipherSuite,
              FHandshakeSharedSecret,
              LTranscriptData,
              FHandshakeSecrets,
              LKeyScheduleError
            ) then
            begin
              FLastErrorCode := sslErrUnsupported;
              FLastErrorString := 'TLS 1.3 key schedule derivation failed: ' + LKeyScheduleError;
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;
          end;

          try
            FServerFinishedKey := TLS13FinishedKeyForCipherSuite(
              LServerHello.SelectedCipherSuite,
              FHandshakeSecrets.ServerHandshakeTrafficSecret
            );
            FClientFinishedKey := TLS13FinishedKeyForCipherSuite(
              LServerHello.SelectedCipherSuite,
              FHandshakeSecrets.ClientHandshakeTrafficSecret
            );
          except
            on E: Exception do
            begin
              FLastErrorCode := sslErrHandshake;
              FLastErrorString := 'TLS 1.3 finished-key derivation failed: ' + E.Message;
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;
          end;

          FServerHandshakeSeq := 0;
          FClientHandshakeSeq := 0;

          if not ProcessEncryptedServerFlight(LServerHello.SelectedCipherSuite, LTranscriptData) then
            Exit;

          if not ValidateClientPeerCertificateTrust then
            Exit;

          if not ValidateClientPeerCertificateFlags then
            Exit;

          if not ValidateClientOCSPStapling then
            Exit;

          if not ValidateClientOnlineOCSP then
            Exit;

          if not ValidateClientCertificateTransparency then
            Exit;

          { Derive application secrets BEFORE SendClientFinished because
            RFC 8446 Section 7.1 requires Transcript-Hash(CH..SF) — the
            transcript must NOT include Client Finished. }
          if not TryDeriveTLS13ApplicationSecrets(
            LServerHello.SelectedCipherSuite,
            FHandshakeSecrets.HandshakeSecret,
            LTranscriptData,
            FApplicationSecrets,
            LKeyScheduleError
          ) then
          begin
            SetHandshakeError(sslErrUnsupported, 'TLS 1.3 application key schedule derivation failed: ' + LKeyScheduleError);
            Exit;
          end;

          if not SendClientFinished(LServerHello.SelectedCipherSuite, LTranscriptData) then
            Exit;

          { RFC 8446 Section 7.1: resumption_master_secret uses Hash(CH..CF) }
          FApplicationSecrets.ResumptionTranscriptHash := HashTLS13TranscriptForSuite(
            LServerHello.SelectedCipherSuite, LTranscriptData
          );

          FClientApplicationSeq := 0;
          FServerApplicationSeq := 0;
          SetLength(FApplicationReadBuffer, 0);
          SetLength(FPostHandshakeBuffer, 0);
          FSessionTicketCount := 0;
          InitTLS13NewSessionTicket(FLastSessionTicket);
          FIsServerMode := False;

          FProtocolVersion := sslProtocolTLS13;
          FCipherName := TLS13CipherSuiteToString(LServerHello.SelectedCipherSuite);
          if FSessionReused and (FConfiguredSession <> nil) then
            FCurrentSession := FConfiguredSession.Clone;
          if not DrainBufferedApplicationRecords then
            Exit;
          Result := True;
          Exit;
        end;
    end;
  end;

  FLastErrorCode := sslErrProtocol;
  FLastErrorString := 'ServerHello not received in expected handshake records';
  RecordError(FLastErrorCode, FLastErrorString);
end;

procedure TFreePascalConnection.MarkUnsupported(const AOperation: string);
begin
  FLastErrorCode := sslErrUnsupported;
  FLastErrorString := Format('%s is unsupported by FreePascal backend', [AOperation]);
  RecordError(FLastErrorCode, FLastErrorString);
end;

procedure TFreePascalConnection.MarkPrecondition(const AOperation: string);
begin
  FLastErrorCode := sslErrProtocol;
  FLastErrorString := Format('%s requires completed TLS handshake', [AOperation]);
  RecordError(FLastErrorCode, FLastErrorString);
end;

function TFreePascalConnection.SendClientEarlyDataRecord(ACipherSuite: Word): Boolean;
var
  LInnerPlaintext: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
begin
  Result := False;

  if Length(FEarlyDataPayload) = 0 then
    Exit(True);

  if not FEarlyDataSecrets.Valid then
  begin
    SetHandshakeError(sslErrProtocol, 'Client early-data keys are not available');
    Exit;
  end;

  LInnerPlaintext := BuildTLS13InnerPlaintext(FEarlyDataPayload, TLS_CONTENT_TYPE_APPLICATION_DATA);

  try
    LNonce := BuildTLS13RecordNonce(FEarlyDataSecrets.ClientEarlyIV, FEarlyDataSeq);
  except
    on E: Exception do
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to build client early-data nonce: ' + E.Message);
      Exit;
    end;
  end;

  if not TryTLS13AEADEncrypt(
    ACipherSuite,
    FEarlyDataSecrets.ClientEarlyKey,
    LNonce,
    BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(ACipherSuite))),
    LInnerPlaintext,
    LEncrypted,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt client early-data record: ' + LError);
    Exit;
  end;

  LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
  if not SendAll(LRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send client early-data record');
    Exit;
  end;

  if not IncrementTLS13Sequence(FEarlyDataSeq) then
  begin
    SetHandshakeError(sslErrProtocol, 'Client early-data sequence overflow');
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.DoRead(var ABuffer; ACount: Integer): Integer;
var
  LFragment: TBytes;
  LCopyLen: Integer;
  LRemainLen: Integer;
  LRemain: TBytes;
begin
  if not FHandshakeComplete then
  begin
    MarkPrecondition('TLS read');
    Exit(-1);
  end;

  if ACount <= 0 then
    Exit(0);

  while Length(FApplicationReadBuffer) = 0 do
  begin
    if (FStream <> nil) and (GetBufferedStreamBytesAvailable <= 0) then
      Exit(0);

    if not RecvApplicationDataFragment(LFragment) then
      Exit(-1);

    if Length(LFragment) > 0 then
    begin
      SetLength(FApplicationReadBuffer, Length(LFragment));
      Move(LFragment[0], FApplicationReadBuffer[0], Length(LFragment));
    end;
  end;

  LCopyLen := ACount;
  if LCopyLen > Length(FApplicationReadBuffer) then
    LCopyLen := Length(FApplicationReadBuffer);

  Move(FApplicationReadBuffer[0], ABuffer, LCopyLen);

  LRemainLen := Length(FApplicationReadBuffer) - LCopyLen;
  if LRemainLen > 0 then
  begin
    SetLength(LRemain, LRemainLen);
    Move(FApplicationReadBuffer[LCopyLen], LRemain[0], LRemainLen);
    FApplicationReadBuffer := LRemain;
  end
  else
    SetLength(FApplicationReadBuffer, 0);

  Result := LCopyLen;
end;

function TFreePascalConnection.DoWrite(const ABuffer; ACount: Integer): Integer;
var
  LFragment: TBytes;
begin
  if not FHandshakeComplete then
  begin
    MarkPrecondition('TLS write');
    Exit(-1);
  end;

  if ACount <= 0 then
    Exit(0);

  SetLength(LFragment, ACount);
  Move(ABuffer, LFragment[0], ACount);

  if not SendApplicationDataFragment(LFragment) then
    Exit(-1);

  Result := ACount;
end;

function TFreePascalConnection.DoConnect: Boolean;
begin
  Result := False;
  ClearPeerCertificateCache;

  if (FStream = nil) and (FSocket < 0) then
  begin
    FLastErrorCode := sslErrInvalidParam;
    FLastErrorString := 'No transport available for TLS connection';
    RecordError(FLastErrorCode, FLastErrorString);
    Exit;
  end;

  if FProtocolVersion <> sslProtocolTLS13 then
  begin
    MarkUnsupported('TLS 1.3-only handshake path (set PreferredVersion=TLS13)');
    Exit;
  end;

  if not ProbeServerHello then
  begin
    if FLastErrorCode = sslErrNone then
      MarkUnsupported('TLS 1.3 ServerHello negotiation');
    Exit;
  end;

  Result := True;
end;

function TFreePascalConnection.DoAccept: Boolean;
var
  LHeader: TTLSRecordHeader;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LHandshakePayload: TBytes;
  LHandshakeBuffer: TBytes;
  LHandshakeMessage: TBytes;
  LInnerPlaintext: TBytes;
  LInnerFragment: TBytes;
  LInnerContentType: Byte;
  LAAD: TBytes;
  LNonce: TBytes;
  LPlaintext: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LClientHello: TTLS13ClientHelloInfo;
  LParseError: string;
  LRecordIndex: Integer;
  LSelectedCipherSuite: Word;
  LClientHelloHandshake: TBytes;
  LServerHelloHandshake: TBytes;
  LServerHelloRecord: TBytes;
  LTranscriptData: TBytes;
  LKeyScheduleError: string;
  LError: string;
  LEncryptedExtensionsMessage: TBytes;
  LFinishedMessage: TBytes;
  LServerFlightMessages: TBytes;
  LTranscriptHash: TBytes;
  LVerifyData: TBytes;
  LMsgType: Byte;
  LMsgLen: Cardinal;
  LClientFinishedReceived: Boolean;
  LAlertLevel: Byte;
  LAlertDescription: Byte;
  LContextMaterial: IFreePascalContextMaterial;
  LStaplingMaterial: IFreePascalContextServerStaplingMaterial;
  LEarlyDataContext: ISSLEarlyDataContext;
  LResumptionCache: IFreePascalResumptionCache;
  LEarlyDataReplayAccess: IFreePascalEarlyDataReplayLedgerAccess;
  LEarlyDataReplayLedger: IFreePascalEarlyDataReplayLedger;
  LCertificateBlob: TBytes;
  LPrivateKeyBlob: TBytes;
  LServerStapledOCSPResponse: TBytes;
  LLeafCertificateDER: TBytes;
  LCertificateMessage: TBytes;
  LCertificateVerifyMessage: TBytes;
  LCachedSession: ISSLSession;
  LCachedResumption: IFreePascalResumptionSession;
  LSignatureScheme: Word;
  LSignatureSchemeError: string;
  LLeafCertificate: TX509Certificate;
  LLeafKeyType: string;
  LCertVerifyInput: TBytes;
  LCertVerifySignature: TBytes;
  LSignatureLength: Integer;
  LResumedHandshake: Boolean;
  LEarlyDataOffered: Boolean;
  LEarlyDataAccepted: Boolean;
  LEarlyDataEndObserved: Boolean;
  LBinderTranscript: TBytes;
  LExpectedBinder: TBytes;
  LTicketNonce: TBytes;
  LTicket: TBytes;
  LTicketExtensions: TBytes;
  LTicketHandshake: TBytes;
  LTicketAgeAddBytes: TBytes;
  LTicketAgeAdd: Cardinal;
  LTicketLifetime: Cardinal;
  LIssuedMaxEarlyDataSize: Cardinal;
  LIssuedSession: TFreePascalSession;
  LEarlyDataBuffer: TBytes;
  LClientRequestedOCSPStapling: Boolean;
  LServerCipherSuites: TTLS13CipherSuiteList;
begin
  Result := False;
  FSelectedALPNProtocol := '';
  SetLength(FHandshakeSharedSecret, 0);
  InitTLS13EarlyDataSecrets(FEarlyDataSecrets);
  ClearTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FEarlyDataSeq := 0;
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  ClearTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
  FCurrentSession := nil;
  FSessionReused := False;
  FEarlyDataStatus := sslEarlyDataNone;
  FEarlyDataLimit := 0;
  SetLength(FEarlyDataPayload, 0);
  SetLength(LEarlyDataBuffer, 0);
  SetLength(LServerStapledOCSPResponse, 0);
  LClientRequestedOCSPStapling := False;
  LLeafKeyType := '';

  if FProtocolVersion <> sslProtocolTLS13 then
  begin
    MarkUnsupported('TLS 1.3-only accept path (set PreferredVersion=TLS13)');
    Exit;
  end;

  SetLength(LHandshakeBuffer, 0);
  SetLength(LClientHelloHandshake, 0);
  for LRecordIndex := 1 to 8 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to receive TLS record while waiting for ClientHello');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer sent TLS alert before ClientHello');
          Exit;
        end;

      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          if not TryExtractHandshakePayloadFromRecord(LRecordBytes, LHandshakePayload) then
          begin
            SetHandshakeError(sslErrProtocol, 'Peer handshake record format is invalid');
            Exit;
          end;

          AppendHandshakeBytes(LHandshakeBuffer, LHandshakePayload);

          while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
          begin
            if Length(LHandshakeMessage) < 4 then
            begin
              SetHandshakeError(sslErrProtocol, 'Malformed handshake message before ClientHello');
              Exit;
            end;

            if LHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_CLIENT_HELLO then
            begin
              SetHandshakeError(
                sslErrProtocol,
                Format('Expected ClientHello, got handshake type %d', [LHandshakeMessage[0]])
              );
              Exit;
            end;

            if not TryParseTLS13ClientHelloFromHandshake(LHandshakeMessage, LClientHello, LParseError) then
            begin
              SetHandshakeError(sslErrProtocol, 'Invalid ClientHello: ' + LParseError);
              Exit;
            end;

            LClientHelloHandshake := LHandshakeMessage;
            LClientRequestedOCSPStapling := ClientHelloHasExtension(
              LClientHelloHandshake,
              TLS_EXTENSION_STATUS_REQUEST
            );
            Break;
          end;

          if Length(LClientHelloHandshake) > 0 then
            Break;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d before ClientHello', [LHeader.ContentType])
        );
        Exit;
      end;
    end;
  end;

  if Length(LClientHelloHandshake) = 0 then
  begin
    SetHandshakeError(sslErrProtocol, 'ClientHello not received in expected handshake records');
    Exit;
  end;

  if not LClientHello.HasSupportedVersions then
  begin
    SetHandshakeError(sslErrProtocol, 'ClientHello missing supported_versions extension');
    Exit;
  end;

  if not TLS13ClientHelloSupportsVersion(LClientHello, TLS13_VERSION) then
  begin
    SetHandshakeError(sslErrProtocol, 'ClientHello does not offer TLS 1.3');
    Exit;
  end;

  LResumedHandshake := False;
  LEarlyDataOffered := False;
  LEarlyDataAccepted := False;
  LEarlyDataEndObserved := True;
  LSelectedCipherSuite := 0;
  LCachedSession := nil;
  LCachedResumption := nil;
  if LClientHello.HasPreSharedKey and
    Supports(FContext, IFreePascalResumptionCache, LResumptionCache) then
  begin
    if not TryBuildTLS13ClientHelloPSKBinderTranscript(LClientHelloHandshake, LBinderTranscript, LParseError) then
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to rebuild ClientHello PSK binder transcript: ' + LParseError);
      Exit;
    end;

    if LResumptionCache.TryGetResumptionSession(LClientHello.FirstPSKIdentity, LCachedSession) and
      Supports(LCachedSession, IFreePascalResumptionSession, LCachedResumption) and
      TLS13ClientHelloOffersCipherSuite(LClientHello, LCachedResumption.GetCipherSuite) then
    begin
      LExpectedBinder := TLS13ComputePSKBinderForCipherSuite(
        LCachedResumption.GetCipherSuite,
        LCachedResumption.GetResumptionPSK,
        LBinderTranscript
      );
      if not BytesEqual(LExpectedBinder, LClientHello.FirstPSKBinder) then
      begin
        SetHandshakeError(sslErrHandshake, 'ClientHello PSK binder verification failed');
        Exit;
      end;

      LSelectedCipherSuite := LCachedResumption.GetCipherSuite;
      LResumedHandshake := True;

      if LClientHello.HasEarlyData then
      begin
        if not TryDeriveTLS13ClientEarlyDataSecrets(
          LCachedResumption.GetCipherSuite,
          LCachedResumption.GetResumptionPSK,
          LClientHelloHandshake,
          FEarlyDataSecrets,
          LKeyScheduleError
        ) then
        begin
          SetHandshakeError(
            sslErrUnsupported,
            'TLS 1.3 server early-data key schedule derivation failed: ' + LKeyScheduleError
          );
          Exit;
        end;

        LEarlyDataOffered := True;
        FEarlyDataLimit := LCachedResumption.GetMaxEarlyDataSize;
        FEarlyDataStatus := sslEarlyDataRejected;
        LEarlyDataEndObserved := False;
        LEarlyDataReplayLedger := nil;

        if Supports(FContext, IFreePascalEarlyDataReplayLedgerAccess, LEarlyDataReplayAccess) then
          LEarlyDataReplayLedger := LEarlyDataReplayAccess.GetEarlyDataReplayLedger;
        if (LEarlyDataReplayLedger = nil) and
          Supports(FContext, IFreePascalEarlyDataReplayLedger, LEarlyDataReplayLedger) then;

        if (FEarlyDataLimit > 0) and
          Supports(FContext, ISSLEarlyDataContext, LEarlyDataContext) and
          (LEarlyDataContext.GetServerEarlyDataPolicy = sslEarlyDataServerAccept) and
          (LEarlyDataReplayLedger <> nil) and
          LEarlyDataReplayLedger.TryAcquireEarlyDataSession(LCachedSession) then
        begin
          LEarlyDataAccepted := True;
          FEarlyDataStatus := sslEarlyDataAccepted;
        end;
      end;
    end;
  end;

  if not LResumedHandshake then
  begin
    if (FContext <> nil) and (FContext.GetCipherSuites <> '') then
      LServerCipherSuites := ParseTLS13CipherSuiteString(FContext.GetCipherSuites)
    else
      SetLength(LServerCipherSuites, 0);

    if Length(LServerCipherSuites) > 0 then
    begin
      { Select first server-preferred cipher that client also offers }
      for LRecordIndex := 0 to High(LServerCipherSuites) do
        if TLS13ClientHelloOffersCipherSuite(LClientHello, LServerCipherSuites[LRecordIndex]) then
        begin
          LSelectedCipherSuite := LServerCipherSuites[LRecordIndex];
          Break;
        end;
    end
    else
    begin
      { Default preference order }
      if TLS13ClientHelloOffersCipherSuite(LClientHello, TLS13_CIPHER_AES_256_GCM_SHA384) then
        LSelectedCipherSuite := TLS13_CIPHER_AES_256_GCM_SHA384
      else if TLS13ClientHelloOffersCipherSuite(LClientHello, TLS13_CIPHER_CHACHA20_POLY1305_SHA256) then
        LSelectedCipherSuite := TLS13_CIPHER_CHACHA20_POLY1305_SHA256
      else if TLS13ClientHelloOffersCipherSuite(LClientHello, TLS13_CIPHER_AES_128_GCM_SHA256) then
        LSelectedCipherSuite := TLS13_CIPHER_AES_128_GCM_SHA256;
    end;
  end;

  if LSelectedCipherSuite = 0 then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      'No supported TLS 1.3 cipher suite intersection (requires TLS_AES_256_GCM_SHA384 or TLS_CHACHA20_POLY1305_SHA256 or TLS_AES_128_GCM_SHA256 for current pure FreePascal path)'
    );
    Exit;
  end;

  FProtocolVersion := sslProtocolTLS13;
  FCipherName := TLS13CipherSuiteToString(LSelectedCipherSuite);
  FSelectedALPNProtocol := SelectALPNProtocol(LClientHello, FALPNProtocols);

  if not LResumedHandshake then
  begin
    if not Supports(FContext, IFreePascalContextMaterial, LContextMaterial) then
    begin
      SetHandshakeError(sslErrUnsupported, 'FreePascal context does not expose certificate material interface');
      Exit;
    end;

    if not LContextMaterial.HasCertificateMaterial then
    begin
      SetHandshakeError(sslErrInvalidParam, 'Server context requires certificate material (LoadCertificate)');
      Exit;
    end;

    if not LContextMaterial.HasPrivateKeyMaterial then
    begin
      SetHandshakeError(sslErrInvalidParam, 'Server context requires private key material (LoadPrivateKey)');
      Exit;
    end;

    LCertificateBlob := LContextMaterial.GetCertificateMaterial;
    LPrivateKeyBlob := LContextMaterial.GetPrivateKeyMaterial;

    if LClientRequestedOCSPStapling and
      Supports(FContext, IFreePascalContextServerStaplingMaterial, LStaplingMaterial) and
      LStaplingMaterial.HasServerStapledOCSPResponse then
      LServerStapledOCSPResponse := LStaplingMaterial.GetServerStapledOCSPResponse;

    if not TryBuildTLS13ServerCertificateHandshakeWithStapledOCSP(
      LCertificateBlob,
      LServerStapledOCSPResponse,
      LCertificateMessage,
      LError
    ) then
    begin
      SetHandshakeError(sslErrInvalidParam, 'Failed to build TLS 1.3 Certificate message: ' + LError);
      Exit;
    end;

    if not TryExtractLeafCertificateDERFromBlob(LCertificateBlob, LLeafCertificateDER, LError) then
    begin
      SetHandshakeError(sslErrInvalidParam, 'Failed to extract leaf certificate for CertificateVerify metadata: ' + LError);
      Exit;
    end;
  end;
  if not LClientHello.HasKeyShare then
  begin
    SetHandshakeError(sslErrProtocol, 'ClientHello missing key_share extension');
    Exit;
  end;

  if LClientHello.KeyShareGroup <> TLS13_GROUP_X25519 then
  begin
    SetHandshakeError(sslErrUnsupported, 'Only X25519 key_share is supported by FreePascal backend');
    Exit;
  end;

  if Length(LClientHello.PeerKeyShare) <> 32 then
  begin
    SetHandshakeError(sslErrProtocol, 'Invalid X25519 key_share length from client');
    Exit;
  end;

  try
    FX25519PrivateKey := GenerateX25519PrivateKey;
    FX25519PublicKey := X25519PublicKeyFromPrivate(FX25519PrivateKey);
    FHandshakeSharedSecret := X25519ComputeSharedSecret(FX25519PrivateKey, LClientHello.PeerKeyShare);
  except
    on E: Exception do
    begin
      SetHandshakeError(sslErrHandshake, 'Failed to establish X25519 server key share: ' + E.Message);
      Exit;
    end;
  end;

  try
    if LResumedHandshake then
      LServerHelloHandshake := BuildTLS13ServerHelloHandshakeWithSelectedPSK(
        LClientHello.LegacySessionID,
        LSelectedCipherSuite,
        FX25519PublicKey,
        0,
        TLS13_GROUP_X25519
      )
    else
      LServerHelloHandshake := BuildTLS13ServerHelloHandshake(
        LClientHello.LegacySessionID,
        LSelectedCipherSuite,
        FX25519PublicKey,
        TLS13_GROUP_X25519
      );
  except
    on E: Exception do
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to build ServerHello: ' + E.Message);
      Exit;
    end;
  end;

  LServerHelloRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_HANDSHAKE, LServerHelloHandshake);
  if not SendAll(LServerHelloRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send ServerHello');
    Exit;
  end;

  SetLength(LTranscriptData, Length(LClientHelloHandshake) + Length(LServerHelloHandshake));
  if Length(LClientHelloHandshake) > 0 then
    Move(LClientHelloHandshake[0], LTranscriptData[0], Length(LClientHelloHandshake));
  if Length(LServerHelloHandshake) > 0 then
    Move(LServerHelloHandshake[0], LTranscriptData[Length(LClientHelloHandshake)], Length(LServerHelloHandshake));

  if LResumedHandshake then
  begin
    if not TryDeriveTLS13HandshakeSecretsWithPSK(
      LSelectedCipherSuite,
      FHandshakeSharedSecret,
      LTranscriptData,
      LCachedResumption.GetResumptionPSK,
      FHandshakeSecrets,
      LKeyScheduleError
    ) then
    begin
      SetHandshakeError(sslErrUnsupported, 'TLS 1.3 server PSK handshake key schedule derivation failed: ' + LKeyScheduleError);
      Exit;
    end;
    FSessionReused := True;
  end
  else if not TryDeriveTLS13HandshakeSecrets(
    LSelectedCipherSuite,
    FHandshakeSharedSecret,
    LTranscriptData,
    FHandshakeSecrets,
    LKeyScheduleError
  ) then
  begin
    SetHandshakeError(sslErrUnsupported, 'TLS 1.3 server handshake key schedule derivation failed: ' + LKeyScheduleError);
    Exit;
  end;

  try
    FServerFinishedKey := TLS13FinishedKeyForCipherSuite(
      LSelectedCipherSuite,
      FHandshakeSecrets.ServerHandshakeTrafficSecret
    );
    FClientFinishedKey := TLS13FinishedKeyForCipherSuite(
      LSelectedCipherSuite,
      FHandshakeSecrets.ClientHandshakeTrafficSecret
    );
  except
    on E: Exception do
    begin
      SetHandshakeError(sslErrHandshake, 'TLS 1.3 server finished-key derivation failed: ' + E.Message);
      Exit;
    end;
  end;

  if not TLS13AEADIsSupported(LSelectedCipherSuite) then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      Format('Cipher suite %s is unsupported by pure FreePascal server handshake path',
        [TLS13CipherSuiteToString(LSelectedCipherSuite)])
    );
    Exit;
  end;

  LEncryptedExtensionsMessage := BuildTLS13EncryptedExtensionsHandshake(
    LEarlyDataAccepted,
    FSelectedALPNProtocol
  );

  SetLength(LServerFlightMessages, 0);
  AppendHandshakeBytes(LServerFlightMessages, LEncryptedExtensionsMessage);
  AppendHandshakeBytes(LTranscriptData, LEncryptedExtensionsMessage);

  if not LResumedHandshake then
  begin
    AppendHandshakeBytes(LServerFlightMessages, LCertificateMessage);
    AppendHandshakeBytes(LTranscriptData, LCertificateMessage);

    LLeafCertificate := TX509Certificate.Create;
    try
      try
        LLeafCertificate.LoadFromDER(LLeafCertificateDER);
      except
        on E: Exception do
        begin
          SetHandshakeError(sslErrInvalidParam, 'Failed to parse leaf certificate DER: ' + E.Message);
          Exit;
        end;
      end;

      if SameText(LLeafCertificate.PublicKeyInfo.KeyType, 'RSA') then
      begin
        LLeafKeyType := 'RSA';
        LSignatureLength := (LLeafCertificate.PublicKeyInfo.KeySize + 7) div 8;
        if LSignatureLength <= 0 then
          LSignatureLength := Length(LLeafCertificate.PublicKeyInfo.RSAModulus);
      end
      else if SameText(LLeafCertificate.PublicKeyInfo.KeyType, 'ECDSA') then
      begin
        LLeafKeyType := 'ECDSA';
        LSignatureLength := 72
      end
      else
        LSignatureLength := 0;
    finally
      LLeafCertificate.Free;
    end;

    if LSignatureLength <= 0 then
    begin
      SetHandshakeError(sslErrUnsupported, 'Unsupported leaf certificate key type for TLS 1.3 CertificateVerify');
      Exit;
    end;

    if not TrySelectTLS13ServerCertificateVerifySchemeForKeyTypeAndCipherSuite(
      LClientHello,
      LLeafKeyType,
      LSelectedCipherSuite,
      LSignatureScheme,
      LSignatureSchemeError
    ) then
    begin
      SetHandshakeError(sslErrUnsupported, LSignatureSchemeError);
      Exit;
    end;

    if Length(LPrivateKeyBlob) = 0 then
    begin
      SetHandshakeError(sslErrInvalidParam, 'Server private key material is empty');
      Exit;
    end;

    LTranscriptHash := HashTLS13TranscriptForSuite(LSelectedCipherSuite, LTranscriptData);
    if Length(LTranscriptHash) = 0 then
    begin
      SetHandshakeError(
        sslErrUnsupported,
        'Unsupported TLS 1.3 cipher suite for server CertificateVerify transcript hashing: ' +
        TLS13CipherSuiteToString(LSelectedCipherSuite)
      );
      Exit;
    end;

    LCertVerifyInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

    case LSignatureScheme of
      TLS13_SIG_RSA_PSS_RSAE_SHA256,
      TLS13_SIG_RSA_PSS_PSS_SHA256,
      TLS13_SIG_RSA_PKCS1_SHA256,
      TLS13_SIG_RSA_PSS_RSAE_SHA384,
      TLS13_SIG_RSA_PSS_PSS_SHA384,
      TLS13_SIG_RSA_PKCS1_SHA384,
      TLS13_SIG_ECDSA_SECP256R1_SHA256:
        begin
          if not TryBuildTLS13CertificateVerifySignature(
            LSignatureScheme,
            LPrivateKeyBlob,
            LCertVerifyInput,
            LCertVerifySignature,
            LError
          ) then
          begin
            SetHandshakeError(sslErrUnsupported, 'CertificateVerify signer failed: ' + LError);
            Exit;
          end;

          if SameText(LLeafKeyType, 'RSA') then
          begin
            if Length(LCertVerifySignature) <> LSignatureLength then
            begin
              SetHandshakeError(
                sslErrHandshake,
                Format('CertificateVerify signature length mismatch (expected=%d actual=%d)',
                  [LSignatureLength, Length(LCertVerifySignature)])
              );
              Exit;
            end;
          end
          else if SameText(LLeafKeyType, 'ECDSA') then
          begin
            if (Length(LCertVerifySignature) <= 0) or
              (Length(LCertVerifySignature) > LSignatureLength) or
              (LCertVerifySignature[0] <> $30) then
            begin
              SetHandshakeError(
                sslErrHandshake,
                Format('ECDSA CertificateVerify signature is invalid DER length (max=%d actual=%d)',
                  [LSignatureLength, Length(LCertVerifySignature)])
              );
              Exit;
            end;
          end;
        end;

    else
      begin
        SetHandshakeError(
          sslErrUnsupported,
          Format('Unsupported CertificateVerify scheme selected: %s',
            [TLS13SignatureSchemeToString(LSignatureScheme)])
        );
        Exit;
      end;
    end;

    LCertificateVerifyMessage := BuildTLS13CertificateVerifyHandshake(
      LSignatureScheme,
      LCertVerifySignature
    );

    AppendHandshakeBytes(LServerFlightMessages, LCertificateVerifyMessage);
    AppendHandshakeBytes(LTranscriptData, LCertificateVerifyMessage);
  end;

  LTranscriptHash := HashTLS13TranscriptForSuite(LSelectedCipherSuite, LTranscriptData);
  LVerifyData := TLS13ComputeFinishedVerifyDataFromTrafficSecretForCipherSuite(
    LSelectedCipherSuite,
    FHandshakeSecrets.ServerHandshakeTrafficSecret,
    LTranscriptHash
  );

  SetLength(LFinishedMessage, 0);
  AppendByte(LFinishedMessage, TLS_HANDSHAKE_TYPE_FINISHED);
  AppendUInt24(LFinishedMessage, Length(LVerifyData));
  AppendBytes(LFinishedMessage, LVerifyData);

  AppendHandshakeBytes(LServerFlightMessages, LFinishedMessage);
  AppendHandshakeBytes(LTranscriptData, LFinishedMessage);

  LInnerPlaintext := BuildTLS13InnerPlaintext(LServerFlightMessages, TLS_CONTENT_TYPE_HANDSHAKE);
  try
    LNonce := BuildTLS13RecordNonce(FHandshakeSecrets.ServerHandshakeIV, FServerHandshakeSeq);
  except
    on E: Exception do
    begin
      SetHandshakeError(sslErrProtocol, 'Failed to build server handshake nonce: ' + E.Message);
      Exit;
    end;
  end;

  LAAD := BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(LSelectedCipherSuite)));
  if not TryTLS13AEADEncrypt(
    LSelectedCipherSuite,
    FHandshakeSecrets.ServerHandshakeKey,
    LNonce,
    LAAD,
    LInnerPlaintext,
    LEncrypted,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt server handshake flight: ' + LError);
    Exit;
  end;

  LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
  if not SendAll(LRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send encrypted server handshake flight');
    Exit;
  end;

  if not IncrementTLS13Sequence(FServerHandshakeSeq) then
  begin
    SetHandshakeError(sslErrProtocol, 'Server handshake sequence overflow');
    Exit;
  end;


  { Derive application secrets BEFORE receiving Client Finished because
    RFC 8446 Section 7.1 requires Transcript-Hash(CH..SF) — the
    transcript must NOT include Client Finished. }
  if not TryDeriveTLS13ApplicationSecrets(
    LSelectedCipherSuite,
    FHandshakeSecrets.HandshakeSecret,
    LTranscriptData,
    FApplicationSecrets,
    LKeyScheduleError
  ) then
  begin
    SetHandshakeError(sslErrUnsupported, 'TLS 1.3 application key schedule derivation failed: ' + LKeyScheduleError);
    Exit;
  end;
  SetLength(LHandshakeBuffer, 0);
  LClientFinishedReceived := False;

  for LRecordIndex := 1 to 64 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to receive encrypted client Finished record');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned TLS alert before client Finished');
          Exit;
        end;

      TLS_CONTENT_TYPE_APPLICATION_DATA:
        begin
          LAAD := BuildTLS13RecordAAD(LHeader.Length);

          if LEarlyDataOffered and FEarlyDataSecrets.Valid then
          begin
            try
              LNonce := BuildTLS13RecordNonce(FEarlyDataSecrets.ClientEarlyIV, FEarlyDataSeq);
            except
              on E: Exception do
              begin
                SetHandshakeError(sslErrProtocol, 'Failed to build client early-data nonce: ' + E.Message);
                Exit;
              end;
            end;

            if TryTLS13AEADDecrypt(
              LSelectedCipherSuite,
              FEarlyDataSecrets.ClientEarlyKey,
              LNonce,
              LAAD,
              LPayloadBytes,
              LPlaintext,
              LError
            ) then
            begin
              if not TryParseTLS13InnerPlaintext(LPlaintext, LInnerFragment, LInnerContentType) then
              begin
                SetHandshakeError(sslErrProtocol, 'Invalid TLSInnerPlaintext in client early-data record');
                Exit;
              end;

              if LInnerContentType <> TLS_CONTENT_TYPE_APPLICATION_DATA then
              begin
                SetHandshakeError(
                  sslErrProtocol,
                  Format('Unexpected inner content type %d in client early-data record', [LInnerContentType])
                );
                Exit;
              end;

              if LEarlyDataAccepted then
              begin
                if Cardinal(Length(LEarlyDataBuffer) + Length(LInnerFragment)) > FEarlyDataLimit then
                begin
                  LEarlyDataAccepted := False;
                  FEarlyDataStatus := sslEarlyDataRejected;
                  SetLength(LEarlyDataBuffer, 0);
                end
                else
                  AppendHandshakeBytes(LEarlyDataBuffer, LInnerFragment);
              end;

              if not IncrementTLS13Sequence(FEarlyDataSeq) then
              begin
                SetHandshakeError(sslErrProtocol, 'Client early-data sequence overflow');
                Exit;
              end;

              Continue;
            end;
          end;

          try
            LNonce := BuildTLS13RecordNonce(FHandshakeSecrets.ClientHandshakeIV, FClientHandshakeSeq);
          except
            on E: Exception do
            begin
              SetHandshakeError(sslErrProtocol, 'Failed to build client handshake nonce: ' + E.Message);
              Exit;
            end;
          end;

          if not IncrementTLS13Sequence(FClientHandshakeSeq) then
          begin
            SetHandshakeError(sslErrProtocol, 'Client handshake sequence overflow');
            Exit;
          end;

          if not TryTLS13AEADDecrypt(
            LSelectedCipherSuite,
            FHandshakeSecrets.ClientHandshakeKey,
            LNonce,
            LAAD,
            LPayloadBytes,
            LPlaintext,
            LError
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt client handshake record: ' + LError);
            Exit;
          end;

          if not TryParseTLS13InnerPlaintext(LPlaintext, LInnerFragment, LInnerContentType) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid TLSInnerPlaintext in client handshake record');
            Exit;
          end;

          case LInnerContentType of
            TLS_CONTENT_TYPE_HANDSHAKE:
              begin
                AppendHandshakeBytes(LHandshakeBuffer, LInnerFragment);

                while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
                begin
                  if Length(LHandshakeMessage) < 4 then
                  begin
                    SetHandshakeError(sslErrProtocol, 'Malformed client handshake message');
                    Exit;
                  end;

                  LMsgType := LHandshakeMessage[0];
                  if LMsgType = TLS_HANDSHAKE_TYPE_END_OF_EARLY_DATA then
                  begin
                    if not LEarlyDataAccepted then
                    begin
                      SetHandshakeError(sslErrProtocol, 'Server reject path must not receive EndOfEarlyData');
                      Exit;
                    end;

                    LEarlyDataEndObserved := True;
                    AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
                  end
                  else if LMsgType = TLS_HANDSHAKE_TYPE_FINISHED then
                  begin
                    if LEarlyDataAccepted and (not LEarlyDataEndObserved) then
                    begin
                      SetHandshakeError(
                        sslErrProtocol,
                        'Accepted early-data path must send EndOfEarlyData before Finished'
                      );
                      Exit;
                    end;

                    LMsgLen := ReadUInt24(LHandshakeMessage, 1);
                    if LMsgLen <> Cardinal(FHandshakeSecrets.HashSize) then
                    begin
                      SetHandshakeError(
                        sslErrProtocol,
                        Format('Client Finished length mismatch (expected=%d actual=%d)',
                          [FHandshakeSecrets.HashSize, Integer(LMsgLen)])
                      );
                      Exit;
                    end;

                    SetLength(LVerifyData, Integer(LMsgLen));
                    if Integer(LMsgLen) > 0 then
                      Move(LHandshakeMessage[4], LVerifyData[0], Integer(LMsgLen));

                    LTranscriptHash := HashTLS13TranscriptForSuite(LSelectedCipherSuite, LTranscriptData);
                    if not TLS13VerifyFinishedForCipherSuite(
                      LSelectedCipherSuite,
                      FHandshakeSecrets.ClientHandshakeTrafficSecret,
                      LTranscriptHash,
                      LVerifyData
                    ) then
                    begin
                      SetHandshakeError(sslErrHandshake, 'Client Finished verification failed');
                      Exit;
                    end;

                    AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
                    LClientFinishedReceived := True;
                    Break;
                  end
                  else
                    AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
                end;

                if LClientFinishedReceived then
                  Break;
              end;

            TLS_CONTENT_TYPE_ALERT:
              begin
                if Length(LInnerFragment) >= 2 then
                begin
                  LAlertLevel := LInnerFragment[0];
                  LAlertDescription := LInnerFragment[1];
                  SetHandshakeError(
                    sslErrHandshake,
                    Format('Peer sent encrypted alert before client Finished (level=%d description=%d)',
                      [LAlertLevel, LAlertDescription])
                  );
                end
                else
                  SetHandshakeError(sslErrHandshake, 'Peer sent malformed encrypted alert before client Finished');
                Exit;
              end;

          else
            begin
              SetHandshakeError(
                sslErrProtocol,
                Format('Unexpected inner content type %d before client Finished', [LInnerContentType])
              );
              Exit;
            end;
          end;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d while waiting for client Finished', [LHeader.ContentType])
        );
        Exit;
      end;
    end;

    if LClientFinishedReceived then
      Break;
  end;

  if not LClientFinishedReceived then
  begin
    SetHandshakeError(sslErrProtocol, 'Client Finished not received within encrypted handshake record budget');
    Exit;
  end;


  { RFC 8446 Section 7.1: resumption_master_secret uses Hash(CH..CF) }
  FApplicationSecrets.ResumptionTranscriptHash := HashTLS13TranscriptForSuite(
    LSelectedCipherSuite, LTranscriptData
  );

  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FPostHandshakeBuffer, 0);
  FApplicationReadBuffer := Copy(LEarlyDataBuffer);

  FProtocolVersion := sslProtocolTLS13;
  FCipherName := TLS13CipherSuiteToString(LSelectedCipherSuite);
  FIsServerMode := True;
  if LResumedHandshake and (LCachedSession <> nil) then
    FCurrentSession := LCachedSession.Clone
  else if Supports(FContext, IFreePascalResumptionCache, LResumptionCache) and
          LResumptionCache.CanIssueSessionTickets then
  begin
    LTicketLifetime := Cardinal(FContext.GetSessionTimeout);
    LTicketNonce := GenerateSecureRandomBytes(8);
    LTicket := GenerateSecureRandomBytes(32);
    LTicketAgeAddBytes := GenerateSecureRandomBytes(4);
    LTicketAgeAdd :=
      (Cardinal(LTicketAgeAddBytes[0]) shl 24) or
      (Cardinal(LTicketAgeAddBytes[1]) shl 16) or
      (Cardinal(LTicketAgeAddBytes[2]) shl 8) or
      Cardinal(LTicketAgeAddBytes[3]);

    LVerifyData := TLS13DeriveResumptionPSKFromTranscriptHash(
      FApplicationSecrets.CipherSuite,
      FApplicationSecrets.MasterSecret,
      FApplicationSecrets.ResumptionTranscriptHash,
      LTicketNonce
    );
    if Length(LVerifyData) <> FApplicationSecrets.HashSize then
    begin
      SetHandshakeError(sslErrHandshake, 'Failed to derive server resumption PSK for NewSessionTicket');
      Exit;
    end;

    LIssuedMaxEarlyDataSize := 0;
    if Supports(FContext, ISSLEarlyDataContext, LEarlyDataContext) then
      case LEarlyDataContext.GetServerEarlyDataPolicy of
        sslEarlyDataServerReject:
          LIssuedMaxEarlyDataSize := 0;
        sslEarlyDataServerAccept,
        sslEarlyDataServerIssueOnly:
          LIssuedMaxEarlyDataSize := LEarlyDataContext.GetServerMaxEarlyDataSize;
      end;

    LIssuedSession := TFreePascalSession.Create;
    LIssuedSession.ConfigureResumption(
      FApplicationSecrets.CipherSuite,
      TLS13CipherSuiteToString(FApplicationSecrets.CipherSuite),
      LTicketNonce,
      LTicket,
      LVerifyData,
      LTicketLifetime,
      LTicketAgeAdd,
      Now,
      FContext.GetSessionTimeout,
      LIssuedMaxEarlyDataSize
    );
    LResumptionCache.StoreResumptionSession(LIssuedSession);
    FCurrentSession := LIssuedSession.Clone;

    SetLength(LTicketExtensions, 0);
    if LIssuedMaxEarlyDataSize > 0 then
    begin
      AppendUInt16(LTicketExtensions, TLS_EXTENSION_EARLY_DATA);
      AppendUInt16(LTicketExtensions, 4);
      AppendByte(LTicketExtensions, Byte((LIssuedMaxEarlyDataSize shr 24) and $FF));
      AppendByte(LTicketExtensions, Byte((LIssuedMaxEarlyDataSize shr 16) and $FF));
      AppendByte(LTicketExtensions, Byte((LIssuedMaxEarlyDataSize shr 8) and $FF));
      AppendByte(LTicketExtensions, Byte(LIssuedMaxEarlyDataSize and $FF));
    end;
    LTicketHandshake := BuildTLS13NewSessionTicketHandshake(
      LTicketLifetime,
      LTicketAgeAdd,
      LTicketNonce,
      LTicket,
      LTicketExtensions
    );
    LInnerPlaintext := BuildTLS13InnerPlaintext(LTicketHandshake, TLS_CONTENT_TYPE_HANDSHAKE);

    try
      LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ServerApplicationIV, FServerApplicationSeq);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrProtocol, 'Failed to build server post-handshake nonce: ' + E.Message);
        Exit;
      end;
    end;

    LAAD := BuildTLS13RecordAAD(Word(Length(LInnerPlaintext) + TLS13AEADTagLength(LSelectedCipherSuite)));
    if not TryTLS13AEADEncrypt(
      LSelectedCipherSuite,
      FApplicationSecrets.ServerApplicationKey,
      LNonce,
      LAAD,
      LInnerPlaintext,
      LEncrypted,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt NewSessionTicket: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendAll(LRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send NewSessionTicket');
      Exit;
    end;
    if not IncrementTLS13Sequence(FServerApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Server application sequence overflow after NewSessionTicket');
      Exit;
    end;

    InitTLS13NewSessionTicket(FLastSessionTicket);
    FLastSessionTicket.Valid := True;
    FLastSessionTicket.TicketLifetime := LTicketLifetime;
    FLastSessionTicket.TicketAgeAdd := LTicketAgeAdd;
    FLastSessionTicket.TicketNonce := Copy(LTicketNonce);
    FLastSessionTicket.Ticket := Copy(LTicket);
    FLastSessionTicket.Extensions := Copy(LTicketExtensions);
    Inc(FSessionTicketCount);
  end;

  Result := True;
end;

function TFreePascalConnection.DoHandshakeInternal: TSSLHandshakeState;
begin
  if (FContext <> nil) and (FContext.GetContextType = sslCtxServer) then
  begin
    if DoAccept then
      Result := sslHsCompleted
    else
      Result := sslHsFailed;
  end
  else
  begin
    if DoConnect then
      Result := sslHsCompleted
    else
      Result := sslHsFailed;
  end;
end;

function TFreePascalConnection.DoShutdown: Boolean;
begin
  Result := True;
end;

procedure TFreePascalConnection.DoClose;
begin
  ClearPeerCertificateCache;
  SecureZeroBytes(FX25519PrivateKey);
  SecureZeroBytes(FX25519PublicKey);
  SecureZeroBytes(FHandshakeSharedSecret);
  ClearTLS13EarlyDataSecrets(FEarlyDataSecrets);
  ClearTLS13HandshakeSecrets(FHandshakeSecrets);
  SecureZeroBytes(FServerFinishedKey);
  SecureZeroBytes(FClientFinishedKey);
  FEarlyDataSeq := 0;
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  ClearTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
  FEarlyDataStatus := sslEarlyDataNone;
  FEarlyDataLimit := 0;
  SetLength(FEarlyDataPayload, 0);
end;

function TFreePascalConnection.DoRenegotiate: Boolean;
begin
  if not FHandshakeComplete then
  begin
    MarkPrecondition('TLS renegotiate/key update');
    Exit(False);
  end;

  if FProtocolVersion <> sslProtocolTLS13 then
  begin
    MarkUnsupported('Renegotiate/KeyUpdate on non-TLS1.3 connection');
    Exit(False);
  end;

  Result := SendPostHandshakeKeyUpdate(True);
end;

function TFreePascalConnection.DoGetError(ARet: Integer): TSSLErrorCode;
begin
  if ARet >= 0 then
    Exit(sslErrNone);

  if FLastErrorCode = sslErrNone then
    Result := sslErrGeneral
  else
    Result := FLastErrorCode;
end;

function TFreePascalConnection.DoWantRead: Boolean;
begin
  Result := False;
end;

function TFreePascalConnection.DoWantWrite: Boolean;
begin
  Result := False;
end;

function TFreePascalConnection.DoGetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TFreePascalConnection.DoGetCipherName: string;
begin
  Result := FCipherName;
end;

function TFreePascalConnection.DoGetPeerCertificate: ISSLCertificate;
begin
  if FPeerCertificate <> nil then
    Result := FPeerCertificate.Clone
  else
    Result := nil;
end;

function TFreePascalConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := CloneCertificateArray(FPeerCertificateChain);
end;

function TFreePascalConnection.DoGetVerifyResult: Integer;
begin
  if FLastErrorCode <> sslErrNone then
    Exit(Ord(FLastErrorCode));

  if not FHandshakeComplete then
    Exit(-1);

  Result := 0;
end;

function TFreePascalConnection.DoGetVerifyResultString: string;
begin
  if FLastErrorString <> '' then
    Exit(FLastErrorString);

  if not FHandshakeComplete then
    Exit('Not verified');

  Result := 'OK';
end;

function TFreePascalConnection.DoGetSession: ISSLSession;
begin
  Result := FCurrentSession;
end;

procedure TFreePascalConnection.DoSetSession(ASession: ISSLSession);
var
  LResumptionSession: IFreePascalResumptionSession;
begin
  FConfiguredSession := nil;
  FSessionReused := False;
  FEarlyDataStatus := sslEarlyDataNone;
  FEarlyDataLimit := 0;
  SetLength(FEarlyDataPayload, 0);
  ClearTLS13EarlyDataSecrets(FEarlyDataSecrets);
  FEarlyDataSeq := 0;

  if (ASession = nil) or (not ASession.IsValid) or (not ASession.IsResumable) then
    Exit;

  if not Supports(ASession, IFreePascalResumptionSession, LResumptionSession) then
    Exit;

  FConfiguredSession := ASession.Clone;
  FEarlyDataLimit := LResumptionSession.GetMaxEarlyDataSize;
end;

function TFreePascalConnection.DoIsSessionReused: Boolean;
begin
  Result := FSessionReused;
end;

function TFreePascalConnection.DoGetConnectionInfoServerName: string;
begin
  Result := FServerName;
end;

function TFreePascalConnection.DoGetSelectedALPNProtocol: string;
begin
  Result := FSelectedALPNProtocol;
end;

function TFreePascalConnection.DoGetState: string;
begin
  if FHandshakeComplete then
    Result := 'CONNECTED'
  else if FCipherName <> '' then
    Result := 'SERVER_HELLO_NEGOTIATED'
  else if FConnected then
    Result := 'CONNECTING'
  else
    Result := 'DISCONNECTED';
end;

function TFreePascalConnection.DoGetNativeHandle: Pointer;
begin
  Result := nil;
end;

function TFreePascalConnection.DoGetOCSPStaplingEnabled: Boolean;
begin
  Result := Length(FOCSPResponse) > 0;
end;

function TFreePascalConnection.DoGetOCSPResponse: TBytes;
begin
  Result := Copy(FOCSPResponse);
end;

function TFreePascalConnection.DoIsOCSPResponseVerified: Boolean;
begin
  Result := FOCSPResponseVerified;
end;

function TFreePascalConnection.DoGetOCSPResponseStatus: string;
begin
  Result := FOCSPResponseStatus;
end;

function TFreePascalConnection.DoGetCertificateTransparencyEnabled: Boolean;
begin
  Result := Length(FSignedCertificateTimestampList) > 0;
end;

function TFreePascalConnection.DoGetSignedCertificateTimestampList: TBytes;
begin
  Result := Copy(FSignedCertificateTimestampList);
end;

function TFreePascalConnection.DoGetSignedCertificateTimestampCount: Integer;
begin
  Result := FSignedCertificateTimestampCount;
end;

function TFreePascalConnection.DoGetCertificateTransparencyStatus: string;
begin
  Result := FCertificateTransparencyStatus;
end;

function TFreePascalConnection.DoHasCertificateTransparencyValidationResult: Boolean;
begin
  Result := FHasCertificateTransparencyValidationResult;
end;

function TFreePascalConnection.DoIsCertificateTransparencyPolicySatisfied: Boolean;
begin
  Result := FCertificateTransparencyPolicySatisfied;
end;

function TFreePascalConnection.DoGetCertificateTransparencyValidationStatus: string;
begin
  Result := FCertificateTransparencyValidationStatus;
end;

procedure TFreePascalConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TFreePascalConnection.GetServerName: string;
begin
  Result := FServerName;
end;

function TFreePascalConnection.SetEarlyData(const AData: TBytes): TSSLOperationResult;
var
  LEarlyDataContext: ISSLEarlyDataContext;
  LResumptionSession: IFreePascalResumptionSession;
begin
  if (FContext = nil) or
    (not ContextTypeSupportsClientConnectionRole(FContext.GetContextType)) then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Early data is only available on client connections'));

  if not Supports(FContext, ISSLEarlyDataContext, LEarlyDataContext) then
    Exit(TSSLOperationResult.Err(sslErrUnsupported, 'Context does not expose early-data interface'));

  if not LEarlyDataContext.GetClientEarlyDataEnabled then
    Exit(TSSLOperationResult.Err(sslErrConfiguration, 'Client early data is disabled on the context'));

  if (FConfiguredSession = nil) or
    (not Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession)) or
    (not FConfiguredSession.IsValid) or
    (not FConfiguredSession.IsResumable) then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Early data requires a configured resumable session'));

  FEarlyDataLimit := LResumptionSession.GetMaxEarlyDataSize;
  if FEarlyDataLimit = 0 then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Configured session does not allow early data'));

  if Cardinal(Length(AData)) > FEarlyDataLimit then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Early data payload exceeds max_early_data_size'));

  FEarlyDataPayload := Copy(AData, 0, Length(AData));
  if Length(FEarlyDataPayload) = 0 then
    FEarlyDataStatus := sslEarlyDataNone
  else
    FEarlyDataStatus := sslEarlyDataQueued;
  ClearTLS13EarlyDataSecrets(FEarlyDataSecrets);
  FEarlyDataSeq := 0;
  Result := TSSLOperationResult.Ok;
end;

function TFreePascalConnection.GetEarlyDataStatus: TSSLEarlyDataStatus;
begin
  Result := FEarlyDataStatus;
end;

function TFreePascalConnection.GetEarlyDataLimit: Cardinal;
begin
  Result := FEarlyDataLimit;
end;

end.
