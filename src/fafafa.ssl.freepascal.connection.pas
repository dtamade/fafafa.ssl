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
 * - TLS 1.3 AES-256-GCM-SHA384（Finished/PSK 相关）路径待补齐
 * - 对端证书验证链与会话复用等高级能力待补齐
 *}

unit fafafa.ssl.freepascal.connection;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}
  Windows, Winsock2,
  {$ELSE}
  BaseUnix, Sockets,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.connection.base,
  fafafa.ssl.tls12.wire,
  fafafa.ssl.tls12.prf,
  fafafa.ssl.tls12.clienthello,
  fafafa.ssl.tls12.clienthello.parser,
  fafafa.ssl.tls12.serverhello.parser,
  fafafa.ssl.tls12.finished,
  fafafa.ssl.tls12.recordcrypto,
  fafafa.ssl.tls12.rsa.verify,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.keyschedule,
  fafafa.ssl.tls13.appschedule,
  fafafa.ssl.tls13.posthandshake,
  fafafa.ssl.tls13.servercertificate;

type
  TTLS13CipherSuiteArray = array of Word;

  TFreePascalPendingWriteKind = (
    fpPendingWriteNone,
    fpPendingWriteApplicationData,
    fpPendingWriteAlert,
    fpPendingWriteKeyUpdate,
    fpPendingWriteSessionTicket
  );

  TFreePascalConnection = class(TBaseSSLConnection, ISSLClientConnection)
  private
    FSocket: THandle;
    FStream: TStream;
    FServerName: string;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherName: string;
    FALPNProtocols: string;
    FSelectedALPNProtocol: string;
    FTLS12CipherSuite: Word;
    FTLS12SessionID: TBytes;
    FTLS12MasterSecret: TBytes;
    FTLS12SessionTicket: TBytes;
    FTLS12SessionTicketLifetimeHint: Cardinal;
    FTLS12ClientWriteKey: TBytes;
    FTLS12ServerWriteKey: TBytes;
    FTLS12ClientWriteIV: TBytes;
    FTLS12ServerWriteIV: TBytes;
    FTLS12ClientSequence: QWord;
    FTLS12ServerSequence: QWord;
    FX25519PrivateKey: TBytes;
    FX25519PublicKey: TBytes;
    FHandshakeSharedSecret: TBytes;
    FHandshakeSecrets: TTLS13HandshakeSecrets;
    FServerFinishedKey: TBytes;
    FClientFinishedKey: TBytes;
    FServerHandshakeSeq: QWord;
    FClientHandshakeSeq: QWord;

    FApplicationSecrets: TTLS13ApplicationSecrets;
    FClientApplicationSeq: QWord;
    FServerApplicationSeq: QWord;
    FApplicationReadBuffer: TBytes;
    FTransportReadBuffer: TBytes;
    FPendingWriteKind: TFreePascalPendingWriteKind;
    FPendingWriteRecord: TBytes;
    FPendingWriteOffset: Integer;
    FPostHandshakeBuffer: TBytes;
    FSessionTicketCount: Integer;
    FLastSessionTicket: TTLS13NewSessionTicket;
    FResumptionTranscriptHash: TBytes;
    FTransportEOF: Boolean;
    FReceivedCloseNotify: Boolean;
    FIsServerMode: Boolean;
    FPeerCertificates: TTLS13CertificateArray;
    FConfiguredSession: ISSLSession;
    FSessionReused: Boolean;

    procedure ClearOperationalErrorState;
    procedure ClearReadClosureState;
    procedure MarkGracefulEOF;
    function IsRetryableIOState: Boolean;
    function IsDeferredIOState: Boolean;
    function SendTLS12AlertRecord(AAlertLevel, AAlertDescription: Byte): Boolean;
    function SendTLS13AlertRecord(AAlertLevel, AAlertDescription: Byte): Boolean;
    function SendData(const ABuffer; ASize: Integer): Integer;
    function RecvData(var ABuffer; ASize: Integer): Integer;
    procedure AppendTransportReadBytes(const ABuffer; ASize: Integer);
    function EnsureTransportReadBuffer(ACount: Integer): Boolean;
    procedure ConsumeTransportReadBytes(ACount: Integer; out AData: TBytes);
    procedure ClearPendingWriteState;
    function FlushPendingWriteRecord: Boolean;
    function SendBufferedRecord(
      const ARecord: TBytes;
      APendingKind: TFreePascalPendingWriteKind
    ): Boolean;
    function SendAll(const AData: TBytes): Boolean;
    function RecvTLSRecord(out AHeader: TTLSRecordHeader; out APayload, ARecord: TBytes): Boolean;
    function ProbeServerHello: Boolean;
    function ConnectTLS12Client: Boolean;
    function AcceptTLS12Server: Boolean;
    function ValidatePostHandshake(AIsClient: Boolean): Boolean;
    procedure SetHandshakeError(ACode: TSSLErrorCode; const AMessage: string);
    procedure AppendHandshakeBytes(var ADest: TBytes; const ASource: TBytes);
    function TryPopHandshakeMessage(var ABuffer: TBytes; out AMessage: TBytes): Boolean;
    function ProcessEncryptedServerFlight(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
    function SendClientFinished(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
    function RecvTLS12ApplicationDataFragment(out AFragment: TBytes): Boolean;
    function SendTLS12ApplicationDataFragment(const AFragment: TBytes): Boolean;
    function RecvApplicationDataFragment(out AFragment: TBytes): Boolean;
    function SendApplicationDataFragment(const AFragment: TBytes): Boolean;
    function ProcessPostHandshakeFragment(const AHandshakeFragment: TBytes): Boolean;
    function SendInitialSessionTicket: Boolean;
    function SendPostHandshakeKeyUpdate(ARequestPeerUpdate: Boolean): Boolean;
    procedure MarkUnsupported(const AOperation: string);
    procedure MarkPrecondition(const AOperation: string);
    procedure NotifyInfoCallback(AWhere: Integer; ARet: Integer; const AState: string);
    procedure NotifyHandshakeFailureInfoState;
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
    function DoGetSelectedALPNProtocol: string; override;
    function DoGetState: string; override;
    function DoGetNativeHandle: Pointer; override;
  public
    function GetConnectionInfo: TSSLConnectionInfo; override;
    constructor Create(AContext: ISSLContext; ASocket: THandle); overload;
    constructor Create(AContext: ISSLContext; AStream: TStream); overload;

    procedure SetTimeout(ATimeout: Integer); override;
    procedure SetBlocking(ABlocking: Boolean); override;
    function GetStateString: string; override;
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
  end;

implementation

uses
  DateUtils,
  fafafa.ssl.factory,
  fafafa.ssl.asn1,
  fafafa.ssl.pem,
  fafafa.ssl.tls13.clienthello,
  fafafa.ssl.tls13.clienthello.parser,
  fafafa.ssl.tls13.encryptedextensions,
  fafafa.ssl.tls13.parser,
  fafafa.ssl.tls13.serverhello,
  fafafa.ssl.tls13.finished,
  fafafa.ssl.tls13.recordcrypto,
  fafafa.ssl.tls13.aead,
  fafafa.ssl.tls13.x25519,
  fafafa.ssl.tls13.ecdsa,
  fafafa.ssl.tls13.servercertverify,
  fafafa.ssl.freepascal.context.material,
  fafafa.ssl.freepascal.session,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.random,
  fafafa.ssl.tls13.primitives,
  fafafa.ssl.x509;

type
  TTLS12CipherSuiteArray = array of Word;

function ContainsTextInsensitive(const AText, ANeedle: string): Boolean;
begin
  Result := Pos(UpperCase(ANeedle), UpperCase(AText)) > 0;
end;

procedure AddTLS12CipherCandidate(var ACandidates: TTLS12CipherSuiteArray; ACipherSuite: Word);
var
  I: Integer;
begin
  if ACipherSuite = 0 then
    Exit;

  for I := 0 to High(ACandidates) do
    if ACandidates[I] = ACipherSuite then
      Exit;

  SetLength(ACandidates, Length(ACandidates) + 1);
  ACandidates[High(ACandidates)] := ACipherSuite;
end;

function ResolveConfiguredTLS12CipherSuites(AContext: ISSLContext): TTLS12CipherSuiteArray;
var
  LCipherList: string;
begin
  Result := nil;
  if AContext <> nil then
    LCipherList := Trim(AContext.GetCipherList)
  else
    LCipherList := '';

  if LCipherList <> '' then
  begin
    if ContainsTextInsensitive(LCipherList, 'ECDHE-RSA-CHACHA20-POLY1305') or
      ContainsTextInsensitive(LCipherList, 'TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256') then
      AddTLS12CipherCandidate(Result, TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256);

    if ContainsTextInsensitive(LCipherList, 'ECDHE-RSA-AES128-GCM-SHA256') or
      ContainsTextInsensitive(LCipherList, 'TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256') then
      AddTLS12CipherCandidate(Result, TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256);

    if ContainsTextInsensitive(LCipherList, 'ECDHE-RSA-AES256-GCM-SHA384') or
      ContainsTextInsensitive(LCipherList, 'TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384') then
      AddTLS12CipherCandidate(Result, TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384);
  end;

  if Length(Result) = 0 then
  begin
    AddTLS12CipherCandidate(Result, TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256);
    AddTLS12CipherCandidate(Result, TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256);
    AddTLS12CipherCandidate(Result, TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384);
  end;
end;

function TLS12CipherSuiteIsChaCha(ACipherSuite: Word): Boolean;
begin
  Result := ACipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256;
end;

function TLS12CipherSuiteIsAES128GCM(ACipherSuite: Word): Boolean;
begin
  Result := ACipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256;
end;

function TLS12CipherSuiteIsAES256GCM(ACipherSuite: Word): Boolean;
begin
  Result := ACipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384;
end;

function TLS12CipherSuiteUsesSHA384(ACipherSuite: Word): Boolean;
begin
  Result := TLS12CipherSuiteIsAES256GCM(ACipherSuite);
end;

function TLS12CipherSuiteIsSupported(ACipherSuite: Word): Boolean;
begin
  Result := TLS12CipherSuiteIsChaCha(ACipherSuite) or
    TLS12CipherSuiteIsAES128GCM(ACipherSuite) or
    TLS12CipherSuiteIsAES256GCM(ACipherSuite);
end;

procedure PreferTLS12CipherSuite(var ACipherSuites: TTLS12CipherSuiteArray; ACipherSuite: Word);
var
  I, LIndex: Integer;
  LValue: Word;
begin
  LIndex := -1;
  for I := 0 to High(ACipherSuites) do
    if ACipherSuites[I] = ACipherSuite then
    begin
      LIndex := I;
      Break;
    end;
  if LIndex <= 0 then
    Exit;

  LValue := ACipherSuites[LIndex];
  for I := LIndex downto 1 do
    ACipherSuites[I] := ACipherSuites[I - 1];
  ACipherSuites[0] := LValue;
end;

function TLS12KeyBlockLength(ACipherSuite: Word): Integer;
begin
  if TLS12CipherSuiteIsChaCha(ACipherSuite) then
    Exit(88);
  if TLS12CipherSuiteIsAES128GCM(ACipherSuite) then
    Exit(40);
  if TLS12CipherSuiteIsAES256GCM(ACipherSuite) then
    Exit(72);
  Result := 0;
end;

function BuildTLS12ClientKeyExchangeHandshake(const APublicKey: TBytes): TBytes;
var
  LBody: TBytes;
begin
  if (Length(APublicKey) = 0) or (Length(APublicKey) > 255) then
    RaiseInvalidParameter('TLS12ClientKeyExchangePublicKey');

  LBody := nil;
  AppendByte(LBody, Byte(Length(APublicKey)));
  AppendBytes(LBody, APublicKey);

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS12ServerHelloHandshake(
  const AServerRandom: TBytes;
  const ASessionID: TBytes;
  ACipherSuite: Word;
  const ASelectedALPNProtocol: string
): TBytes;
var
  LBody: TBytes;
  LExtensions: TBytes;
  LExtData: TBytes;
begin
  LBody := nil;
  AppendUInt16(LBody, TLS12_VERSION);
  AppendBytes(LBody, AServerRandom);
  AppendByte(LBody, Byte(Length(ASessionID)));
  AppendBytes(LBody, ASessionID);
  AppendUInt16(LBody, ACipherSuite);
  AppendByte(LBody, TLS_COMPRESSION_NULL);

  LExtensions := nil;
  if ASelectedALPNProtocol <> '' then
  begin
    LExtData := nil;
    AppendUInt16(LExtData, Word(Length(ASelectedALPNProtocol) + 1));
    AppendByte(LExtData, Byte(Length(ASelectedALPNProtocol)));
    AppendBytes(LExtData, BytesOf(ASelectedALPNProtocol));
    AppendUInt16(LExtensions, TLS_EXTENSION_ALPN);
    AppendUInt16(LExtensions, Word(Length(LExtData)));
    AppendBytes(LExtensions, LExtData);
  end;

  AppendUInt16(LBody, Word(Length(LExtensions)));
  AppendBytes(LBody, LExtensions);

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_SERVER_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS12CertificateHandshakeFromBlob(
  const ACertificateBlob: TBytes;
  out AHandshake: TBytes;
  out AError: string
): Boolean;
var
  LPEMReader: TPEMReader;
  LBlocks: TPEMBlockArray;
  LBody: TBytes;
  LCertList: TBytes;
  I: Integer;
begin
  Result := False;
  AError := '';
  SetLength(AHandshake, 0);

  if Length(ACertificateBlob) = 0 then
  begin
    AError := 'Certificate blob is empty';
    Exit;
  end;

  LPEMReader := TPEMReader.Create;
  try
    try
      LPEMReader.LoadFromString(AnsiString(TEncoding.ASCII.GetString(ACertificateBlob)));
    except
      on E: Exception do
      begin
        AError := 'Failed to parse certificate PEM blob: ' + E.Message;
        Exit;
      end;
    end;

    LBlocks := LPEMReader.GetCertificates;
    if Length(LBlocks) = 0 then
    begin
      AError := 'Certificate blob does not contain CERTIFICATE blocks';
      Exit;
    end;

    LCertList := nil;
    for I := 0 to High(LBlocks) do
    begin
      AppendUInt24(LCertList, Length(LBlocks[I].Data));
      AppendBytes(LCertList, LBlocks[I].Data);
    end;

    LBody := nil;
    AppendUInt24(LBody, Length(LCertList));
    AppendBytes(LBody, LCertList);

    AHandshake := nil;
    AppendByte(AHandshake, TLS_HANDSHAKE_TYPE_CERTIFICATE);
    AppendUInt24(AHandshake, Length(LBody));
    AppendBytes(AHandshake, LBody);
    Result := True;
  finally
    LPEMReader.Free;
  end;
end;

function BuildTLS12ServerKeyExchangeHandshake(
  ANamedCurve: Word;
  const AServerPublicKey: TBytes;
  ASignatureScheme: Word;
  const ASignature: TBytes
): TBytes;
var
  LBody: TBytes;
begin
  LBody := nil;
  AppendByte(LBody, 3);
  AppendUInt16(LBody, ANamedCurve);
  AppendByte(LBody, Byte(Length(AServerPublicKey)));
  AppendBytes(LBody, AServerPublicKey);
  AppendUInt16(LBody, ASignatureScheme);
  AppendUInt16(LBody, Word(Length(ASignature)));
  AppendBytes(LBody, ASignature);

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS12ServerHelloDoneHandshake: TBytes;
begin
  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE);
  AppendUInt24(Result, 0);
end;

function TryParseTLS12ClientKeyExchangeFromHandshake(
  const AHandshake: TBytes;
  out APublicKey: TBytes;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LPublicKeyLen: Integer;
begin
  Result := False;
  AError := '';
  SetLength(APublicKey, 0);

  if Length(AHandshake) < 5 then
  begin
    AError := 'ClientKeyExchange handshake too short';
    Exit;
  end;
  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE then
  begin
    AError := 'Unexpected handshake type for ClientKeyExchange';
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) <> 4 + Integer(LBodyLen) then
  begin
    AError := 'ClientKeyExchange length mismatch';
    Exit;
  end;

  LPublicKeyLen := AHandshake[4];
  if (LPublicKeyLen <= 0) or (5 + LPublicKeyLen <> Length(AHandshake)) then
  begin
    AError := 'ClientKeyExchange public key length mismatch';
    Exit;
  end;

  SetLength(APublicKey, LPublicKeyLen);
  Move(AHandshake[5], APublicKey[0], LPublicKeyLen);
  Result := True;
end;

function SelectTLS12ServerSignatureScheme(
  const AClientHello: TTLS12ClientHelloInfo;
  out ASignatureScheme: Word
): Boolean;
begin
  ASignatureScheme := 0;
  if TLS12ClientHelloOffersSignatureScheme(AClientHello, TLS_SIG_RSA_PSS_RSAE_SHA256) then
    ASignatureScheme := TLS_SIG_RSA_PSS_RSAE_SHA256
  else if TLS12ClientHelloOffersSignatureScheme(AClientHello, TLS_SIG_RSA_PKCS1_SHA256) then
    ASignatureScheme := TLS_SIG_RSA_PKCS1_SHA256;
  Result := ASignatureScheme <> 0;
end;

function BuildTLS12FinishedHandshake(const AVerifyData: TBytes): TBytes;
begin
  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_FINISHED);
  AppendUInt24(Result, Length(AVerifyData));
  AppendBytes(Result, AVerifyData);
end;

function BuildTLS12ChangeCipherSpecRecord: TBytes;
var
  LPayload: TBytes;
begin
  LPayload := nil;
  AppendByte(LPayload, 1);
  Result := BuildTLS12Plaintext(TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC, LPayload);
end;

function BuildTLS12ServerECDHParamsBytes(const AInfo: TTLS12ServerKeyExchangeInfo): TBytes;
begin
  Result := nil;
  AppendByte(Result, AInfo.CurveType);
  AppendUInt16(Result, AInfo.NamedCurve);
  AppendByte(Result, Byte(Length(AInfo.PublicKey)));
  AppendBytes(Result, AInfo.PublicKey);
end;

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

function ZeroBytes(ALength: Integer): TBytes;
begin
  Result := nil;
  SetLength(Result, ALength);
  if ALength > 0 then
    FillChar(Result[0], ALength, 0);
end;

function TLS13CipherSuiteFromName(const ACipherName: string): Word;
begin
  if SameText(ACipherName, 'TLS_AES_128_GCM_SHA256') then
    Exit(TLS13_CIPHER_AES_128_GCM_SHA256);
  if SameText(ACipherName, 'TLS_AES_256_GCM_SHA384') then
    Exit(TLS13_CIPHER_AES_256_GCM_SHA384);
  if SameText(ACipherName, 'TLS_CHACHA20_POLY1305_SHA256') then
    Exit(TLS13_CIPHER_CHACHA20_POLY1305_SHA256);
  Result := 0;
end;

procedure AppendCipherSuiteToList(var ADest: TTLS13CipherSuiteArray; ACipherSuite: Word);
var
  I, LLen: Integer;
begin
  if ACipherSuite = 0 then
    Exit;

  for I := 0 to High(ADest) do
    if ADest[I] = ACipherSuite then
      Exit;

  LLen := Length(ADest);
  SetLength(ADest, LLen + 1);
  ADest[LLen] := ACipherSuite;
end;

function ParseConfiguredTLS13CipherSuites(const ACipherSuites: string): TTLS13CipherSuiteArray;
var
  LParts: TStringList;
  I: Integer;
begin
  Result := nil;
  LParts := TStringList.Create;
  try
    LParts.StrictDelimiter := True;
    LParts.Delimiter := ':';
    LParts.DelimitedText := StringReplace(ACipherSuites, ',', ':', [rfReplaceAll]);
    for I := 0 to LParts.Count - 1 do
      AppendCipherSuiteToList(Result, TLS13CipherSuiteFromName(Trim(LParts[I])));
  finally
    LParts.Free;
  end;
end;

function GetEffectiveTLS13CipherSuites(const AContext: ISSLContext): TTLS13CipherSuiteArray;
begin
  if AContext <> nil then
    Result := ParseConfiguredTLS13CipherSuites(AContext.GetCipherSuites)
  else
    Result := nil;

  if Length(Result) = 0 then
  begin
    AppendCipherSuiteToList(Result, TLS13_CIPHER_AES_256_GCM_SHA384);
    AppendCipherSuiteToList(Result, TLS13_CIPHER_CHACHA20_POLY1305_SHA256);
    AppendCipherSuiteToList(Result, TLS13_CIPHER_AES_128_GCM_SHA256);
  end;
end;

function BuildConfiguredTLS13ClientHelloHandshake(
  const AContext: ISSLContext;
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes
): TBytes;
var
  LCipherSuites: TTLS13CipherSuiteArray;
begin
  LCipherSuites := GetEffectiveTLS13CipherSuites(AContext);
  Result := BuildTLS13ClientHelloHandshakeWithCipherSuites(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    LCipherSuites
  );
end;

function TrySelectServerCipherSuite(
  const AContext: ISSLContext;
  const AClientHello: TTLS13ClientHelloInfo;
  out ACipherSuite: Word
): Boolean;
var
  LSupportedSuites: TTLS13CipherSuiteArray;
  I: Integer;
begin
  ACipherSuite := 0;
  LSupportedSuites := GetEffectiveTLS13CipherSuites(AContext);
  for I := 0 to High(LSupportedSuites) do
    if TLS13ClientHelloOffersCipherSuite(AClientHello, LSupportedSuites[I]) then
    begin
      ACipherSuite := LSupportedSuites[I];
      Exit(True);
    end;
  Result := False;
end;

function BuildBinderTranscript(
  const AHandshake: TBytes;
  ABinderOffset: Integer
): TBytes;
begin
  if (ABinderOffset < 0) or (ABinderOffset > Length(AHandshake)) then
    Exit(Copy(AHandshake, 0, Length(AHandshake)));
  Result := Copy(AHandshake, 0, ABinderOffset);
end;

function ConstantTimeBytesEqual(const ALeft, ARight: TBytes): Boolean;
var
  I: Integer;
  LDiff: Byte;
begin
  Result := False;
  if Length(ALeft) <> Length(ARight) then
    Exit;

  LDiff := 0;
  for I := 0 to High(ALeft) do
    LDiff := LDiff or (ALeft[I] xor ARight[I]);
  Result := LDiff = 0;
end;

function TryComputeResumptionBinder(
  ACipherSuite: Word;
  const AResumptionPSK: TBytes;
  const ABinderTranscript: TBytes;
  out ABinder: TBytes;
  out AError: string
): Boolean;
var
  LEarlySecret: TBytes;
  LBinderKey: TBytes;
  LFinishedKey: TBytes;
  LTranscriptHash: TBytes;
  LEmptyHash: TBytes;
begin
  SetLength(ABinder, 0);
  AError := '';
  Result := False;

  if Length(AResumptionPSK) <> TLS13CipherSuiteHashSize(ACipherSuite) then
  begin
    AError := 'Invalid resumption PSK length';
    Exit;
  end;

  if TLS13CipherSuiteIsSHA256(ACipherSuite) then
  begin
    LEarlySecret := HKDF_Extract_SHA256(nil, AResumptionPSK);
    LEmptyHash := SHA256(nil);
    LBinderKey := TLS13_HKDF_Expand_Label_SHA256(
      LEarlySecret,
      'res binder',
      LEmptyHash,
      TLS13CipherSuiteHashSize(ACipherSuite)
    );
    LFinishedKey := TLS13FinishedKeySHA256(LBinderKey);
    LTranscriptHash := SHA256(ABinderTranscript);
    ABinder := TLS13ComputeFinishedVerifyDataSHA256(LFinishedKey, LTranscriptHash);
  end
  else if TLS13CipherSuiteIsSHA384(ACipherSuite) then
  begin
    LEarlySecret := HKDF_Extract_SHA384(nil, AResumptionPSK);
    LEmptyHash := SHA384(nil);
    LBinderKey := TLS13_HKDF_Expand_Label_SHA384(
      LEarlySecret,
      'res binder',
      LEmptyHash,
      TLS13CipherSuiteHashSize(ACipherSuite)
    );
    LFinishedKey := TLS13FinishedKeySHA384(LBinderKey);
    LTranscriptHash := SHA384(ABinderTranscript);
    ABinder := TLS13ComputeFinishedVerifyDataSHA384(LFinishedKey, LTranscriptHash);
  end
  else
  begin
    AError := 'Unsupported TLS 1.3 cipher suite for resumption binder';
    Exit;
  end;
  Result := True;
end;

function TryDeriveResumptionPSK(
  ACipherSuite: Word;
  const AMasterSecret: TBytes;
  const ATranscriptHash: TBytes;
  const ATicketNonce: TBytes;
  out APSK: TBytes;
  out AError: string
): Boolean;
var
  LResumptionMasterSecret: TBytes;
  LHashSize: Integer;
begin
  SetLength(APSK, 0);
  AError := '';
  Result := False;

  LHashSize := TLS13CipherSuiteHashSize(ACipherSuite);
  if Length(AMasterSecret) <> LHashSize then
  begin
    AError := 'Invalid master secret length for resumption PSK';
    Exit;
  end;

  if TLS13CipherSuiteIsSHA256(ACipherSuite) then
  begin
    LResumptionMasterSecret := TLS13_HKDF_Expand_Label_SHA256(
      AMasterSecret,
      'res master',
      ATranscriptHash,
      LHashSize
    );
    APSK := TLS13_HKDF_Expand_Label_SHA256(
      LResumptionMasterSecret,
      'resumption',
      ATicketNonce,
      LHashSize
    );
  end
  else if TLS13CipherSuiteIsSHA384(ACipherSuite) then
  begin
    LResumptionMasterSecret := TLS13_HKDF_Expand_Label_SHA384(
      AMasterSecret,
      'res master',
      ATranscriptHash,
      LHashSize
    );
    APSK := TLS13_HKDF_Expand_Label_SHA384(
      LResumptionMasterSecret,
      'resumption',
      ATicketNonce,
      LHashSize
    );
  end
  else
  begin
    AError := 'Unsupported TLS 1.3 cipher suite for resumption PSK';
    Exit;
  end;
  Result := True;
end;

function ComputeTranscriptHashForCipherSuite(
  ACipherSuite: Word;
  const ATranscriptData: TBytes
): TBytes;
begin
  if TLS13CipherSuiteIsSHA256(ACipherSuite) then
    Exit(SHA256(ATranscriptData));

  if TLS13CipherSuiteIsSHA384(ACipherSuite) then
    Exit(SHA384(ATranscriptData));

  SetLength(Result, 0);
end;

procedure TFreePascalConnection.ClearOperationalErrorState;
begin
  FLastErrorCode := sslErrNone;
  FLastErrorString := '';
end;

procedure TFreePascalConnection.ClearReadClosureState;
begin
  FTransportEOF := False;
  FReceivedCloseNotify := False;
end;

procedure TFreePascalConnection.MarkGracefulEOF;
begin
  FTransportEOF := True;
  FReceivedCloseNotify := True;
  FLastErrorCode := sslErrNone;
  FLastErrorString := '';
end;

function TFreePascalConnection.IsRetryableIOState: Boolean;
begin
  Result := (FLastErrorCode = sslErrWantRead) or (FLastErrorCode = sslErrWantWrite);
end;

function TFreePascalConnection.IsDeferredIOState: Boolean;
begin
  Result := IsRetryableIOState or (FLastErrorCode = sslErrTimeout);
end;

function TFreePascalConnection.SendTLS13AlertRecord(AAlertLevel,
  AAlertDescription: Byte): Boolean;
var
  LAlertFragment: TBytes;
  LInnerPlaintext: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
begin
  Result := False;

  if not FHandshakeComplete then
    Exit(True);
  if not FApplicationSecrets.Valid then
    Exit(True);
  if not TLS13AEADIsSupported(FApplicationSecrets.CipherSuite) then
    Exit(True);

  SetLength(LAlertFragment, 2);
  LAlertFragment[0] := AAlertLevel;
  LAlertFragment[1] := AAlertDescription;
  LInnerPlaintext := BuildTLS13InnerPlaintext(LAlertFragment, TLS_CONTENT_TYPE_ALERT);

  if FIsServerMode then
  begin
    try
      LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ServerApplicationIV, FServerApplicationSeq);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrProtocol, 'Failed to build server alert nonce: ' + E.Message);
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
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS alert record: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendBufferedRecord(LRecord, fpPendingWriteAlert) then
    begin
      if IsDeferredIOState then
        Exit(False);
      SetHandshakeError(sslErrIO, 'Failed to send TLS alert record');
      Exit;
    end;

    if not IncrementTLS13Sequence(FServerApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Server application sequence overflow during alert send');
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
        SetHandshakeError(sslErrProtocol, 'Failed to build client alert nonce: ' + E.Message);
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
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS alert record: ' + LError);
      Exit;
    end;

    LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
    if not SendBufferedRecord(LRecord, fpPendingWriteAlert) then
    begin
      if IsDeferredIOState then
        Exit(False);
      SetHandshakeError(sslErrIO, 'Failed to send TLS alert record');
      Exit;
    end;

    if not IncrementTLS13Sequence(FClientApplicationSeq) then
    begin
      SetHandshakeError(sslErrProtocol, 'Client application sequence overflow during alert send');
      Exit;
    end;
  end;

  Result := True;
end;

procedure AppendByteValue(var ADest: TBytes; AValue: Byte);
var
  LLen: Integer;
begin
  LLen := Length(ADest);
  SetLength(ADest, LLen + 1);
  ADest[LLen] := AValue;
end;

procedure AppendBytesValue(var ADest: TBytes; const ASource: TBytes);
var
  LOldLen, LAddLen: Integer;
begin
  LAddLen := Length(ASource);
  if LAddLen <= 0 then
    Exit;
  LOldLen := Length(ADest);
  SetLength(ADest, LOldLen + LAddLen);
  Move(ASource[0], ADest[LOldLen], LAddLen);
end;

procedure AppendASN1Length(var ADest: TBytes; ALength: Integer);
var
  LTemp: TBytes;
  LValue: Integer;
begin
  if ALength < $80 then
  begin
    AppendByteValue(ADest, Byte(ALength));
    Exit;
  end;

  SetLength(LTemp, 0);
  LValue := ALength;
  while LValue > 0 do
  begin
    SetLength(LTemp, Length(LTemp) + 1);
    Move(LTemp[0], LTemp[1], Length(LTemp) - 1);
    LTemp[0] := Byte(LValue and $FF);
    LValue := LValue shr 8;
  end;

  AppendByteValue(ADest, Byte($80 or Length(LTemp)));
  AppendBytesValue(ADest, LTemp);
end;

procedure AppendASN1Tag(var ADest: TBytes; const ATag: TASN1Tag);
var
  LFirst: Byte;
  LParts: array of Byte;
  LTagNumber: Cardinal;
  I: Integer;
begin
  LFirst := Byte(Integer(ATag.TagClass) shl 6);
  if ATag.Constructed then
    LFirst := LFirst or ASN1_CONSTRUCTED;

  if ATag.TagNumber < $1F then
  begin
    AppendByteValue(ADest, LFirst or Byte(ATag.TagNumber));
    Exit;
  end;

  AppendByteValue(ADest, LFirst or $1F);
  SetLength(LParts, 0);
  LTagNumber := ATag.TagNumber;
  repeat
    SetLength(LParts, Length(LParts) + 1);
    Move(LParts[0], LParts[1], Length(LParts) - 1);
    LParts[0] := Byte(LTagNumber and $7F);
    LTagNumber := LTagNumber shr 7;
  until LTagNumber = 0;

  for I := 0 to High(LParts) - 1 do
    LParts[I] := LParts[I] or $80;
  AppendBytesValue(ADest, LParts);
end;

function EncodeASN1Node(ANode: TASN1Node): TBytes;
var
  I: Integer;
  LContent: TBytes;
begin
  Result := nil;
  SetLength(Result, 0);
  SetLength(LContent, 0);

  if ANode.Tag.Constructed then
  begin
    for I := 0 to ANode.ChildCount - 1 do
      AppendBytesValue(LContent, EncodeASN1Node(ANode.GetChild(I)));
  end
  else
    LContent := Copy(ANode.RawData, 0, Length(ANode.RawData));

  AppendASN1Tag(Result, ANode.Tag);
  AppendASN1Length(Result, Length(LContent));
  AppendBytesValue(Result, LContent);
end;

function TryExtractSubjectPublicKeyInfoDER(
  const ACertificate: ISSLCertificate;
  out ASPKIDER: TBytes
): Boolean;
var
  LDER: TBytes;
  LReader: TASN1Reader;
  LRoot: TASN1Node;
  LTBS: TASN1Node;
  LIndex: Integer;
begin
  Result := False;
  SetLength(ASPKIDER, 0);
  if ACertificate = nil then
    Exit;

  LDER := ACertificate.SaveToDER;
  if Length(LDER) = 0 then
    Exit;

  LReader := TASN1Reader.Create(LDER);
  try
    LRoot := LReader.Parse;
    if (LRoot = nil) or (not LRoot.IsSequence) or (LRoot.ChildCount < 1) then
      Exit;

    LTBS := LRoot.GetChild(0);
    if (LTBS = nil) or (not LTBS.IsSequence) then
      Exit;

    LIndex := 0;
    if (LTBS.ChildCount > 0) and LTBS.GetChild(0).IsContextTag(0) then
      Inc(LIndex);
    Inc(LIndex, 5);
    if LTBS.ChildCount <= LIndex then
      Exit;

    ASPKIDER := EncodeASN1Node(LTBS.GetChild(LIndex));
    Result := Length(ASPKIDER) > 0;
  finally
    LReader.Free;
  end;
end;

function TryValidatePins(
  const ACertificate: ISSLCertificate;
  const APins: TFreePascalPinInfoArray;
  out AError: string
): Boolean;
var
  LCertHash: TBytes;
  LSPKIDER: TBytes;
  LPublicKeyHash: TBytes;
  I: Integer;
begin
  Result := False;
  AError := '';

  if ACertificate = nil then
  begin
    AError := 'Certificate pinning requires peer certificate';
    Exit;
  end;

  if Length(APins) = 0 then
  begin
    AError := 'No pins configured but pinning is required';
    Exit;
  end;

  LCertHash := SHA256(ACertificate.SaveToDER);
  if TryExtractSubjectPublicKeyInfoDER(ACertificate, LSPKIDER) then
    LPublicKeyHash := SHA256(LSPKIDER)
  else
    SetLength(LPublicKeyHash, 0);

  for I := 0 to High(APins) do
  begin
    case APins[I].PinType of
      0:
        if ConstantTimeBytesEqual(LCertHash, APins[I].Hash) then
          Exit(True);
      1:
        if ConstantTimeBytesEqual(LPublicKeyHash, APins[I].Hash) then
          Exit(True);
    end;
  end;

  AError := 'Certificate pinning validation failed';
end;

constructor TFreePascalConnection.Create(AContext: ISSLContext; ASocket: THandle);
var
  LDefaultServerName: string;
begin
  inherited Create(AContext);
  FSocket := ASocket;
  FStream := nil;
  FServerName := '';
  LDefaultServerName := GetLegacyContextDefaultServerName;
  if LDefaultServerName <> '' then
    SetServerName(LDefaultServerName);
  FProtocolVersion := SelectPreferredProtocol(AContext);
  FCipherName := '';
  FALPNProtocols := AContext.GetALPNProtocols;
  FSelectedALPNProtocol := '';
  FTLS12CipherSuite := 0;
  SetLength(FTLS12SessionID, 0);
  SetLength(FTLS12MasterSecret, 0);
  SetLength(FTLS12SessionTicket, 0);
  FTLS12SessionTicketLifetimeHint := 0;
  SetLength(FTLS12ClientWriteKey, 0);
  SetLength(FTLS12ServerWriteKey, 0);
  SetLength(FTLS12ClientWriteIV, 0);
  SetLength(FTLS12ServerWriteIV, 0);
  FTLS12ClientSequence := 0;
  FTLS12ServerSequence := 0;
  SetLength(FX25519PrivateKey, 0);
  SetLength(FX25519PublicKey, 0);
  SetLength(FHandshakeSharedSecret, 0);
  InitTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  InitTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FTransportReadBuffer, 0);
  FPendingWriteKind := fpPendingWriteNone;
  SetLength(FPendingWriteRecord, 0);
  FPendingWriteOffset := 0;
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  SetLength(FResumptionTranscriptHash, 0);
  ClearReadClosureState;
  FIsServerMode := False;
  SetLength(FPeerCertificates, 0);
  FConfiguredSession := nil;
  FSessionReused := False;
end;

constructor TFreePascalConnection.Create(AContext: ISSLContext; AStream: TStream);
var
  LDefaultServerName: string;
begin
  inherited Create(AContext);
  if AStream = nil then
    RaiseInvalidParameter('AStream');

  FSocket := -1;
  FStream := AStream;
  FServerName := '';
  LDefaultServerName := GetLegacyContextDefaultServerName;
  if LDefaultServerName <> '' then
    SetServerName(LDefaultServerName);
  FProtocolVersion := SelectPreferredProtocol(AContext);
  FCipherName := '';
  FALPNProtocols := AContext.GetALPNProtocols;
  FSelectedALPNProtocol := '';
  FTLS12CipherSuite := 0;
  SetLength(FTLS12SessionID, 0);
  SetLength(FTLS12MasterSecret, 0);
  SetLength(FTLS12SessionTicket, 0);
  FTLS12SessionTicketLifetimeHint := 0;
  SetLength(FTLS12ClientWriteKey, 0);
  SetLength(FTLS12ServerWriteKey, 0);
  SetLength(FTLS12ClientWriteIV, 0);
  SetLength(FTLS12ServerWriteIV, 0);
  FTLS12ClientSequence := 0;
  FTLS12ServerSequence := 0;
  SetLength(FX25519PrivateKey, 0);
  SetLength(FX25519PublicKey, 0);
  SetLength(FHandshakeSharedSecret, 0);
  InitTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  InitTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FTransportReadBuffer, 0);
  FPendingWriteKind := fpPendingWriteNone;
  SetLength(FPendingWriteRecord, 0);
  FPendingWriteOffset := 0;
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  SetLength(FResumptionTranscriptHash, 0);
  ClearReadClosureState;
  FIsServerMode := False;
  SetLength(FPeerCertificates, 0);
  FConfiguredSession := nil;
  FSessionReused := False;
end;

procedure TFreePascalConnection.SetTimeout(ATimeout: Integer);
{$IFNDEF WINDOWS}
var
  LTimeVal: TTimeVal;
{$ELSE}
var
  LTimeoutValue: DWORD;
{$ENDIF}
begin
  inherited SetTimeout(ATimeout);

  if FSocket < 0 then
    Exit;

  {$IFDEF WINDOWS}
  LTimeoutValue := DWORD(ATimeout);
  setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@LTimeoutValue), SizeOf(LTimeoutValue));
  setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@LTimeoutValue), SizeOf(LTimeoutValue));
  {$ELSE}
  if ATimeout < 0 then
    ATimeout := 0;
  LTimeVal.tv_sec := ATimeout div 1000;
  LTimeVal.tv_usec := (ATimeout mod 1000) * 1000;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @LTimeVal, SizeOf(LTimeVal));
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @LTimeVal, SizeOf(LTimeVal));
  {$ENDIF}
end;

procedure TFreePascalConnection.SetBlocking(ABlocking: Boolean);
{$IFNDEF WINDOWS}
var
  LFlags: LongInt;
{$ELSE}
var
  LNonBlocking: u_long;
{$ENDIF}
begin
  inherited SetBlocking(ABlocking);

  if FSocket < 0 then
    Exit;

  {$IFDEF WINDOWS}
  if ABlocking then
    LNonBlocking := 0
  else
    LNonBlocking := 1;
  ioctlsocket(FSocket, FIONBIO, LNonBlocking);
  {$ELSE}
  LFlags := fpfcntl(FSocket, F_GETFL, 0);
  if LFlags < 0 then
    Exit;
  if ABlocking then
    LFlags := LFlags and (not O_NONBLOCK)
  else
    LFlags := LFlags or O_NONBLOCK;
  fpfcntl(FSocket, F_SETFL, LFlags);
  {$ENDIF}
end;

function TFreePascalConnection.SendData(const ABuffer; ASize: Integer): Integer;
{$IFNDEF WINDOWS}
var
  LErrNo: Integer;
{$ELSE}
var
  LWSAError: Integer;
{$ENDIF}
begin
  if FStream <> nil then
  begin
    try
      Exit(FStream.Write(ABuffer, ASize));
    except
      on E: Exception do
      begin
        FLastErrorCode := sslErrIO;
        FLastErrorString := 'Stream write failed: ' + E.Message;
        RecordError(FLastErrorCode, FLastErrorString);
        Exit(-1);
      end;
    end;
  end;

  if FSocket < 0 then
    Exit(-1);

  {$IFDEF WINDOWS}
  Result := Winsock2.send(FSocket, ABuffer, ASize, 0);
  if Result = SOCKET_ERROR then
  begin
    LWSAError := WSAGetLastError;
    if (LWSAError = WSAEWOULDBLOCK) or (LWSAError = WSAETIMEDOUT) then
    begin
      if FBlocking and (FTimeout > 0) then
      begin
        FLastErrorCode := sslErrTimeout;
        FLastErrorString := Format('TLS transport write timed out after %d ms', [FTimeout]);
      end
      else
      begin
        FLastErrorCode := sslErrWantWrite;
        FLastErrorString := 'TLS transport write would block';
      end;
      RecordError(FLastErrorCode, FLastErrorString);
    end;
    Result := -1;
  end;
  {$ELSE}
  Result := fpSend(FSocket, @ABuffer, ASize, 0);
  if Result < 0 then
  begin
    LErrNo := fpGetErrNo;
    if (LErrNo = ESysEAGAIN) or (LErrNo = ESysEWOULDBLOCK) then
    begin
      if FBlocking and (FTimeout > 0) then
      begin
        FLastErrorCode := sslErrTimeout;
        FLastErrorString := Format('TLS transport write timed out after %d ms', [FTimeout]);
      end
      else
      begin
        FLastErrorCode := sslErrWantWrite;
        FLastErrorString := 'TLS transport write would block';
      end;
      RecordError(FLastErrorCode, FLastErrorString);
    end;
  end;
  {$ENDIF}
end;

function TFreePascalConnection.RecvData(var ABuffer; ASize: Integer): Integer;
{$IFNDEF WINDOWS}
var
  LErrNo: Integer;
{$ELSE}
var
  LWSAError: Integer;
{$ENDIF}
begin
  if FStream <> nil then
  begin
    try
      Result := FStream.Read(ABuffer, ASize);
      if Result = 0 then
        FTransportEOF := True;
      Exit;
    except
      on E: Exception do
      begin
        FLastErrorCode := sslErrIO;
        FLastErrorString := 'Stream read failed: ' + E.Message;
        RecordError(FLastErrorCode, FLastErrorString);
        Exit(-1);
      end;
    end;
  end;

  if FSocket < 0 then
    Exit(-1);

  {$IFDEF WINDOWS}
  Result := Winsock2.recv(FSocket, ABuffer, ASize, 0);
  if Result = SOCKET_ERROR then
  begin
    LWSAError := WSAGetLastError;
    if (LWSAError = WSAEWOULDBLOCK) or (LWSAError = WSAETIMEDOUT) then
    begin
      if FBlocking and (FTimeout > 0) then
      begin
        FLastErrorCode := sslErrTimeout;
        FLastErrorString := Format('TLS transport read timed out after %d ms', [FTimeout]);
      end
      else
      begin
        FLastErrorCode := sslErrWantRead;
        FLastErrorString := 'TLS transport read would block';
      end;
      RecordError(FLastErrorCode, FLastErrorString);
    end;
    Result := -1;
  end
  else if Result = 0 then
    FTransportEOF := True;
  {$ELSE}
  Result := fpRecv(FSocket, @ABuffer, ASize, 0);
  if Result < 0 then
  begin
    LErrNo := fpGetErrNo;
    if (LErrNo = ESysEAGAIN) or (LErrNo = ESysEWOULDBLOCK) then
    begin
      if FBlocking and (FTimeout > 0) then
      begin
        FLastErrorCode := sslErrTimeout;
        FLastErrorString := Format('TLS transport read timed out after %d ms', [FTimeout]);
      end
      else
      begin
        FLastErrorCode := sslErrWantRead;
        FLastErrorString := 'TLS transport read would block';
      end;
      RecordError(FLastErrorCode, FLastErrorString);
    end;
  end
  else if Result = 0 then
    FTransportEOF := True;
  {$ENDIF}
end;

procedure TFreePascalConnection.AppendTransportReadBytes(const ABuffer; ASize: Integer);
var
  LOldLen: Integer;
begin
  if ASize <= 0 then
    Exit;

  LOldLen := Length(FTransportReadBuffer);
  SetLength(FTransportReadBuffer, LOldLen + ASize);
  Move(ABuffer, FTransportReadBuffer[LOldLen], ASize);
end;

function TFreePascalConnection.EnsureTransportReadBuffer(ACount: Integer): Boolean;
var
  LBuffer: array[0..4095] of Byte;
  LChunk, LNeed: Integer;
begin
  Result := False;
  if ACount < 0 then
    Exit;

  while Length(FTransportReadBuffer) < ACount do
  begin
    LNeed := ACount - Length(FTransportReadBuffer);
    if LNeed > SizeOf(LBuffer) then
      LNeed := SizeOf(LBuffer);

    LChunk := RecvData(LBuffer[0], LNeed);
    if LChunk <= 0 then
      Exit;

    AppendTransportReadBytes(LBuffer[0], LChunk);
  end;

  Result := True;
end;

procedure TFreePascalConnection.ConsumeTransportReadBytes(ACount: Integer; out AData: TBytes);
var
  LRemain: Integer;
begin
  if ACount <= 0 then
  begin
    SetLength(AData, 0);
    Exit;
  end;

  AData := Copy(FTransportReadBuffer, 0, ACount);
  LRemain := Length(FTransportReadBuffer) - ACount;
  if LRemain > 0 then
    FTransportReadBuffer := Copy(FTransportReadBuffer, ACount, LRemain)
  else
    SetLength(FTransportReadBuffer, 0);
end;

procedure TFreePascalConnection.ClearPendingWriteState;
begin
  FPendingWriteKind := fpPendingWriteNone;
  SetLength(FPendingWriteRecord, 0);
  FPendingWriteOffset := 0;
end;

function TFreePascalConnection.FlushPendingWriteRecord: Boolean;
var
  LChunk: Integer;
begin
  Result := False;

  while FPendingWriteOffset < Length(FPendingWriteRecord) do
  begin
    LChunk := SendData(
      FPendingWriteRecord[FPendingWriteOffset],
      Length(FPendingWriteRecord) - FPendingWriteOffset
    );
    if LChunk <= 0 then
    begin
      if not IsDeferredIOState then
        ClearPendingWriteState;
      Exit;
    end;
    Inc(FPendingWriteOffset, LChunk);
  end;

  ClearOperationalErrorState;
  ClearPendingWriteState;
  Result := True;
end;

function TFreePascalConnection.SendBufferedRecord(
  const ARecord: TBytes;
  APendingKind: TFreePascalPendingWriteKind
): Boolean;
begin
  Result := False;

  if Length(ARecord) = 0 then
    Exit(True);

  if FPendingWriteKind <> fpPendingWriteNone then
  begin
    if (FPendingWriteKind <> APendingKind) or
      (not ConstantTimeBytesEqual(FPendingWriteRecord, ARecord)) then
    begin
      FLastErrorCode := sslErrWantWrite;
      FLastErrorString := 'Previous TLS write is still pending; retry the same operation';
      RecordError(FLastErrorCode, FLastErrorString);
      Exit;
    end;
  end
  else
  begin
    FPendingWriteKind := APendingKind;
    FPendingWriteRecord := Copy(ARecord, 0, Length(ARecord));
    FPendingWriteOffset := 0;
  end;

  Result := FlushPendingWriteRecord;
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

  ClearOperationalErrorState;
  Result := True;
end;

function TFreePascalConnection.RecvTLSRecord(out AHeader: TTLSRecordHeader; out APayload, ARecord: TBytes): Boolean;
var
  LHeaderBytes: TBytes;
  LRecordLen: Integer;
begin
  Result := False;
  SetLength(APayload, 0);
  SetLength(ARecord, 0);

  if not EnsureTransportReadBuffer(5) then
    Exit;

  LHeaderBytes := Copy(FTransportReadBuffer, 0, 5);
  if not ParseTLSRecordHeader(LHeaderBytes, AHeader) then
    Exit;

  LRecordLen := 5 + AHeader.Length;
  if not EnsureTransportReadBuffer(LRecordLen) then
    Exit;

  ConsumeTransportReadBytes(LRecordLen, ARecord);
  APayload := Copy(ARecord, 5, AHeader.Length);

  Result := True;
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

  if FPendingWriteKind = fpPendingWriteKeyUpdate then
  begin
    if not SendBufferedRecord(FPendingWriteRecord, fpPendingWriteKeyUpdate) then
    begin
      if not IsDeferredIOState then
        SetHandshakeError(sslErrIO, 'Failed to send TLS KeyUpdate record');
      Exit;
    end;

    if FIsServerMode then
    begin
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
    if not SendBufferedRecord(LRecord, fpPendingWriteKeyUpdate) then
    begin
      if not IsDeferredIOState then
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
    if not SendBufferedRecord(LRecord, fpPendingWriteKeyUpdate) then
    begin
      if not IsRetryableIOState then
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

function TFreePascalConnection.SendInitialSessionTicket: Boolean;
var
  LHandshakeMessage: TBytes;
  LInnerPlaintext: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
  LAgeAddBytes: TBytes;
begin
  Result := False;

  if not FApplicationSecrets.Valid then
    Exit;
  if not TLS13AEADIsSupported(FApplicationSecrets.CipherSuite) then
    Exit;
  if not ((FContext <> nil) and (ssoEnableSessionTickets in FContext.GetOptions)) then
    Exit(True);

  if FPendingWriteKind = fpPendingWriteSessionTicket then
  begin
    if not SendBufferedRecord(FPendingWriteRecord, fpPendingWriteSessionTicket) then
      Exit;

    if not IncrementTLS13Sequence(FServerApplicationSeq) then
      Exit;

    FSessionTicketCount := 1;
    Result := True;
    Exit;
  end;

  if FPendingWriteKind <> fpPendingWriteNone then
  begin
    FLastErrorCode := sslErrWantWrite;
    FLastErrorString := 'Previous TLS write is still pending; retry the same operation';
    RecordError(FLastErrorCode, FLastErrorString);
    Exit;
  end;

  InitTLS13NewSessionTicket(FLastSessionTicket);
  FLastSessionTicket.Valid := True;
  FLastSessionTicket.TicketLifetime := 86400;
  LAgeAddBytes := GenerateSecureRandomBytes(4);
  if Length(LAgeAddBytes) = 4 then
    FLastSessionTicket.TicketAgeAdd :=
      (Cardinal(LAgeAddBytes[0]) shl 24) or
      (Cardinal(LAgeAddBytes[1]) shl 16) or
      (Cardinal(LAgeAddBytes[2]) shl 8) or
      Cardinal(LAgeAddBytes[3])
  else
    FLastSessionTicket.TicketAgeAdd := $10203040;
  FLastSessionTicket.TicketNonce := GenerateSecureRandomBytes(4);
  FLastSessionTicket.Ticket := GenerateSecureRandomBytes(32);
  SetLength(FLastSessionTicket.Extensions, 0);

  if not TryBuildTLS13NewSessionTicketHandshake(FLastSessionTicket, LHandshakeMessage, LError) then
    Exit;

  LInnerPlaintext := BuildTLS13InnerPlaintext(LHandshakeMessage, TLS_CONTENT_TYPE_HANDSHAKE);
  try
    LNonce := BuildTLS13RecordNonce(FApplicationSecrets.ServerApplicationIV, FServerApplicationSeq);
  except
    Exit;
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
    Exit;

  LRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
  if not SendBufferedRecord(LRecord, fpPendingWriteSessionTicket) then
    Exit;

  if not IncrementTLS13Sequence(FServerApplicationSeq) then
    Exit;

  FSessionTicketCount := 1;
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
  LMsgType: Byte;
  LMsgLen: Cardinal;
  LVerifyData: TBytes;
  LTranscriptHash: TBytes;
  LEncryptedExtensionsInfo: TTLS13EncryptedExtensionsInfo;
begin
  Result := False;
  SetLength(LHandshakeBuffer, 0);
  FServerHandshakeSeq := 0;

  for LRecordIndex := 1 to 96 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
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

                  if LMsgType = TLS_HANDSHAKE_TYPE_FINISHED then
                  begin
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

                    LTranscriptHash := ComputeTranscriptHashForCipherSuite(ACipherSuite, ATranscriptData);
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
                  begin
                    if LHandshakeMessage[0] = TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS then
                    begin
                      if not TryParseEncryptedExtensionsFromHandshake(
                        LHandshakeMessage,
                        LEncryptedExtensionsInfo,
                        LError
                      ) then
                      begin
                        SetHandshakeError(sslErrProtocol, 'Invalid EncryptedExtensions: ' + LError);
                        Exit;
                      end;

                      if LEncryptedExtensionsInfo.HasALPN then
                        FSelectedALPNProtocol := LEncryptedExtensionsInfo.SelectedALPNProtocol;
                    end;
                    if LHandshakeMessage[0] = TLS_HANDSHAKE_TYPE_CERTIFICATE then
                    begin
                      if not TryParseTLS13ServerCertificateHandshake(
                        LHandshakeMessage,
                        FPeerCertificates,
                        LError
                      ) then
                      begin
                        SetHandshakeError(sslErrProtocol, 'Invalid Certificate handshake: ' + LError);
                        Exit;
                      end;
                    end;
                    AppendHandshakeBytes(ATranscriptData, LHandshakeMessage);
                  end;
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

function TFreePascalConnection.ValidatePostHandshake(AIsClient: Boolean): Boolean;
var
  LVerifyMode: TSSLVerifyModes;
  LVerifyFlags: TSSLCertVerifyFlags;
  LPeerCert: ISSLCertificate;
  LTrustStore: ISSLCertificateStore;
  LTrustContext: IFreePascalContextTrustStore;
  LVerifyResult: TSSLCertVerifyResult;
  LHost: string;
  LBaseStore: ISSLCertificateStore;
  LCAFile: string;
  LCAPath: string;
  I: Integer;
  LStoreCert: ISSLCertificate;
  LVerifyCallback: TSSLVerifyCallback;
  LCallbackResult: Boolean;
  LCallbackInfo: TSSLCertificateInfo;
  LValidationOk: Boolean;
  LFailureCode: TSSLErrorCode;
  LFailureMessage: string;
  LNeedEffectiveStore: Boolean;
  LChainCert: ISSLCertificate;
  LPins: TFreePascalPinInfoArray;
  LSessionPeerCert: ISSLCertificate;
  LResumptionSession: IFreePascalResumptionSession;
  LSessionChain: TSSLCertificateArray;
begin
  Result := True;

  if FContext = nil then
    Exit(False);

  LVerifyMode := FContext.GetVerifyMode;
  if not (sslVerifyPeer in LVerifyMode) then
    Exit(True);

  LPeerCert := DoGetPeerCertificate;
  if (LPeerCert = nil) and FSessionReused and (FConfiguredSession <> nil) then
  begin
    LSessionPeerCert := FConfiguredSession.GetPeerCertificate;
    if LSessionPeerCert <> nil then
      LPeerCert := LSessionPeerCert;
  end;
  if LPeerCert = nil then
  begin
    LFailureCode := sslErrCertificate;
    LFailureMessage := 'Peer certificate is required but missing';
    LValidationOk := False;
    LPeerCert := nil;
    LVerifyCallback := nil;
    if not Supports(FContext, IFreePascalContextTrustStore, LTrustContext) then
    begin
      SetHandshakeError(LFailureCode, LFailureMessage);
      Exit(False);
    end;
    LVerifyCallback := LTrustContext.GetVerifyCallback;
    if not Assigned(LVerifyCallback) then
    begin
      SetHandshakeError(LFailureCode, LFailureMessage);
      Exit(False);
    end;

    LCallbackInfo := Default(TSSLCertificateInfo);
    LCallbackResult := LVerifyCallback(LCallbackInfo, Ord(LFailureCode), LFailureMessage);
    if not LCallbackResult then
    begin
      SetHandshakeError(LFailureCode, LFailureMessage);
      Exit(False);
    end;
    Exit(True);
  end;

  if not Supports(FContext, IFreePascalContextTrustStore, LTrustContext) then
  begin
    SetHandshakeError(sslErrCertificateUntrusted, 'FreePascal trust context interface is unavailable');
    Exit(False);
  end;

  LValidationOk := True;
  LFailureCode := sslErrNone;
  LFailureMessage := '';
  LBaseStore := LTrustContext.GetCertificateStore;
  LCAFile := LTrustContext.GetCAFile;
  LCAPath := LTrustContext.GetCAPath;
  LVerifyCallback := LTrustContext.GetVerifyCallback;

  if (LBaseStore = nil) and (LCAFile = '') and (LCAPath = '') then
  begin
    LValidationOk := False;
    LFailureCode := sslErrCertificateUntrusted;
    LFailureMessage := 'Certificate store is not configured';
  end;

  if LValidationOk then
  begin
    LNeedEffectiveStore := (LCAFile <> '') or (LCAPath <> '') or
      (Length(FPeerCertificates) > 1);
    if (not LNeedEffectiveStore) and FSessionReused and (FConfiguredSession <> nil) and
      Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession) then
    begin
      LSessionChain := LResumptionSession.GetPeerCertificateChain;
      LNeedEffectiveStore := Length(LSessionChain) > 1;
    end;

    if not LNeedEffectiveStore then
      LTrustStore := LBaseStore
    else
    begin
      LTrustStore := TSSLFactory.CreateCertificateStore(sslFreePascal);
      if LTrustStore = nil then
      begin
        LValidationOk := False;
        LFailureCode := sslErrCertificateUntrusted;
        LFailureMessage := 'Failed to create effective trust store';
      end;

      if LValidationOk and (LBaseStore <> nil) then
      begin
        for I := 0 to LBaseStore.GetCount - 1 do
        begin
          LStoreCert := LBaseStore.GetCertificate(I);
          if LStoreCert <> nil then
            LTrustStore.AddCertificate(LStoreCert);
        end;
      end;

      if LValidationOk and (LCAFile <> '') and (not LTrustStore.LoadFromFile(LCAFile)) then
      begin
        LValidationOk := False;
        LFailureCode := sslErrCertificateUntrusted;
        LFailureMessage := Format('Failed to load CA file into trust store: %s', [LCAFile]);
      end;

      if LValidationOk and (LCAPath <> '') and (not LTrustStore.LoadFromPath(LCAPath)) then
      begin
        LValidationOk := False;
        LFailureCode := sslErrCertificateUntrusted;
        LFailureMessage := Format('Failed to load CA path into trust store: %s', [LCAPath]);
      end;

      if LValidationOk then
      begin
        for I := 1 to High(FPeerCertificates) do
        begin
          LChainCert := TSSLFactory.CreateCertificate(sslFreePascal);
          if (LChainCert = nil) or (not LChainCert.LoadFromDER(FPeerCertificates[I])) then
          begin
            LValidationOk := False;
            LFailureCode := sslErrCertificateUnknown;
            LFailureMessage := Format('Failed to load peer chain certificate #%d', [I + 1]);
            Break;
          end;
          LTrustStore.AddCertificate(LChainCert);
        end;

        if FSessionReused and (Length(FPeerCertificates) = 0) and
          (FConfiguredSession <> nil) and
          Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession) then
        begin
          LSessionChain := LResumptionSession.GetPeerCertificateChain;
          for I := 1 to High(LSessionChain) do
            if LSessionChain[I] <> nil then
              LTrustStore.AddCertificate(LSessionChain[I]);
        end;
      end;
    end;
  end;

  if LValidationOk and not LPeerCert.VerifyEx(LTrustStore, FContext.GetCertVerifyFlags, LVerifyResult) then
  begin
    LValidationOk := False;
    case LVerifyResult.ErrorCode of
      3:
        begin
          LFailureCode := sslErrCertificateExpired;
          LFailureMessage := LVerifyResult.ErrorMessage;
        end;
    else
      begin
        LFailureCode := sslErrCertificateUntrusted;
        LFailureMessage := LVerifyResult.ErrorMessage;
      end;
    end;
  end;

  LVerifyFlags := FContext.GetCertVerifyFlags;
  if LValidationOk and AIsClient and not (sslCertVerifyIgnoreHostname in LVerifyFlags) then
  begin
    LHost := NormalizeHostForVerify(FServerName);
    if LHost = '' then
    begin
      LValidationOk := False;
      LFailureCode := sslErrVerificationFailed;
      LFailureMessage := 'Hostname verification requires non-empty server name';
    end;

    if LValidationOk and not LPeerCert.VerifyHostname(LHost) then
    begin
      LValidationOk := False;
      LFailureCode := sslErrVerificationFailed;
      LFailureMessage := Format('Hostname verification failed for "%s"', [LHost]);
    end;
  end;

  if LValidationOk and LTrustContext.GetCertificatePinningEnabled then
  begin
    LPins := LTrustContext.GetPins;
    if not TryValidatePins(LPeerCert, LPins, LFailureMessage) then
    begin
      LValidationOk := False;
      LFailureCode := sslErrVerificationFailed;
    end;
  end;

  LCallbackInfo := LPeerCert.GetInfo;
  if Assigned(LVerifyCallback) then
  begin
    if LValidationOk then
      LCallbackResult := LVerifyCallback(LCallbackInfo, 0, '')
    else
      LCallbackResult := LVerifyCallback(LCallbackInfo, Ord(LFailureCode), LFailureMessage);

    if not LCallbackResult then
    begin
      if LValidationOk then
      begin
        LFailureCode := sslErrVerificationFailed;
        LFailureMessage := 'Verification callback rejected certificate';
      end;
      LValidationOk := False;
    end
    else if not LValidationOk then
    begin
      LValidationOk := True;
      LFailureCode := sslErrNone;
      LFailureMessage := '';
    end;
  end;

  if not LValidationOk then
  begin
    SetHandshakeError(LFailureCode, LFailureMessage);
    Exit(False);
  end;
end;

function TFreePascalConnection.SendClientFinished(ACipherSuite: Word; var ATranscriptData: TBytes): Boolean;
var
  LTranscriptHash: TBytes;
  LVerifyData: TBytes;
  LFinishedHandshake: TBytes;
  LInnerPlaintext: TBytes;
  LNonce: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
  LResumptionTranscriptData: TBytes;
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

  LTranscriptHash := ComputeTranscriptHashForCipherSuite(ACipherSuite, ATranscriptData);
  LVerifyData := TLS13ComputeFinishedVerifyDataForCipherSuite(
    ACipherSuite,
    FClientFinishedKey,
    LTranscriptHash
  );

  SetLength(LFinishedHandshake, 0);
  AppendByte(LFinishedHandshake, TLS_HANDSHAKE_TYPE_FINISHED);
  AppendUInt24(LFinishedHandshake, Length(LVerifyData));
  AppendHandshakeBytes(LFinishedHandshake, LVerifyData);

  LInnerPlaintext := BuildTLS13InnerPlaintext(LFinishedHandshake, TLS_CONTENT_TYPE_HANDSHAKE);

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

  LResumptionTranscriptData := Copy(ATranscriptData, 0, Length(ATranscriptData));
  AppendHandshakeBytes(LResumptionTranscriptData, LFinishedHandshake);
  FResumptionTranscriptHash := ComputeTranscriptHashForCipherSuite(
    ACipherSuite,
    LResumptionTranscriptData
  );
  Result := True;
end;

function TFreePascalConnection.SendTLS12AlertRecord(AAlertLevel, AAlertDescription: Byte): Boolean;
var
  LAlertBytes: TBytes;
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
  LWriteKey, LWriteIV: TBytes;
  LSequence: QWord;
begin
  Result := False;

  if FIsServerMode then
  begin
    LWriteKey := FTLS12ServerWriteKey;
    LWriteIV := FTLS12ServerWriteIV;
    LSequence := FTLS12ServerSequence;
  end
  else
  begin
    LWriteKey := FTLS12ClientWriteKey;
    LWriteIV := FTLS12ClientWriteIV;
    LSequence := FTLS12ClientSequence;
  end;

  if (Length(LWriteKey) = 0) or (Length(LWriteIV) = 0) then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.2 application secrets are not ready for alert');
    Exit;
  end;

  LAlertBytes := nil;
  AppendByte(LAlertBytes, AAlertLevel);
  AppendByte(LAlertBytes, AAlertDescription);

  if not TryEncryptTLS12ChaCha20Poly1305Record(
    LWriteKey,
    LWriteIV,
    LSequence,
    TLS_CONTENT_TYPE_ALERT,
    TLS12_VERSION,
    LAlertBytes,
    LEncrypted,
    LError
  ) then
  begin
    if not (((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256) and
      TryEncryptTLS12AES128GCMRecord(
        LWriteKey,
        LWriteIV,
        LSequence,
        TLS_CONTENT_TYPE_ALERT,
        TLS12_VERSION,
        LAlertBytes,
        LEncrypted,
        LError
      )) or
      ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384) and
      TryEncryptTLS12AES256GCMRecord(
        LWriteKey,
        LWriteIV,
        LSequence,
        TLS_CONTENT_TYPE_ALERT,
        TLS12_VERSION,
        LAlertBytes,
        LEncrypted,
        LError
      ))) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS 1.2 alert: ' + LError);
      Exit;
    end;
  end;

  if Length(LEncrypted) = 0 then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS 1.2 alert');
    Exit;
  end;

  LRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_ALERT, LEncrypted);
  if not SendBufferedRecord(LRecord, fpPendingWriteAlert) then
  begin
    if not IsDeferredIOState then
      SetHandshakeError(sslErrIO, 'Failed to send TLS 1.2 alert');
    Exit;
  end;

  if FIsServerMode then
    Inc(FTLS12ServerSequence)
  else
    Inc(FTLS12ClientSequence);
  Result := True;
end;

function TFreePascalConnection.RecvTLS12ApplicationDataFragment(out AFragment: TBytes): Boolean;
var
  LHeader: TTLSRecordHeader;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LPlaintext: TBytes;
  LError: string;
  LHandshakeBuffer: TBytes;
  LHandshakeMessage: TBytes;
  LAlertLevel: Byte;
  LAlertDescription: Byte;
  LNewSessionTicket: TTLS12NewSessionTicketInfo;
  LReadKey, LReadIV: TBytes;
  LReadSequence: QWord;
begin
  SetLength(AFragment, 0);
  SetLength(LHandshakeBuffer, 0);
  Result := False;

  if FIsServerMode then
  begin
    LReadKey := FTLS12ClientWriteKey;
    LReadIV := FTLS12ClientWriteIV;
    LReadSequence := FTLS12ClientSequence;
  end
  else
  begin
    LReadKey := FTLS12ServerWriteKey;
    LReadIV := FTLS12ServerWriteIV;
    LReadSequence := FTLS12ServerSequence;
  end;

  if (Length(LReadKey) = 0) or (Length(LReadIV) = 0) then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.2 application secrets are not ready');
    Exit;
  end;

  if FReceivedCloseNotify or FTransportEOF then
  begin
    Result := True;
    Exit;
  end;

  while True do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FReceivedCloseNotify or FTransportEOF then
      begin
        Result := True;
        Exit;
      end;
      Exit(False);
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_APPLICATION_DATA:
        begin
          if not (
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) and
              TryDecryptTLS12ChaCha20Poly1305Record(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_APPLICATION_DATA,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              )) or
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256) and
              TryDecryptTLS12AES128GCMRecord(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_APPLICATION_DATA,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              )) or
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384) and
              TryDecryptTLS12AES256GCMRecord(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_APPLICATION_DATA,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              ))
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS 1.2 application record: ' + LError);
            Exit;
          end;

          if FIsServerMode then
            Inc(FTLS12ClientSequence)
          else
            Inc(FTLS12ServerSequence);
          AFragment := LPlaintext;
          Result := True;
          Exit;
        end;

      TLS_CONTENT_TYPE_ALERT:
        begin
          if not (
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) and
              TryDecryptTLS12ChaCha20Poly1305Record(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_ALERT,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              )) or
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256) and
              TryDecryptTLS12AES128GCMRecord(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_ALERT,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              )) or
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384) and
              TryDecryptTLS12AES256GCMRecord(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_ALERT,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              ))
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS 1.2 alert record: ' + LError);
            Exit;
          end;

          if FIsServerMode then
            Inc(FTLS12ClientSequence)
          else
            Inc(FTLS12ServerSequence);
          if Length(LPlaintext) >= 2 then
          begin
            LAlertLevel := LPlaintext[0];
            LAlertDescription := LPlaintext[1];
            if LAlertDescription = 0 then
            begin
              MarkGracefulEOF;
              Result := True;
              Exit;
            end;
            SetHandshakeError(
              sslErrHandshake,
              Format('Peer sent TLS 1.2 alert (level=%d description=%d)', [LAlertLevel, LAlertDescription])
            );
          end
          else
            SetHandshakeError(sslErrHandshake, 'Peer sent malformed TLS 1.2 alert');
          Exit;
        end;

      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          if (Length(LPayloadBytes) >= 4) and
            (LPayloadBytes[0] = TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET) then
          begin
            AppendHandshakeBytes(LHandshakeBuffer, LPayloadBytes);
            while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
            begin
              if not TryParseTLS12NewSessionTicketFromHandshake(LHandshakeMessage, LNewSessionTicket, LError) then
              begin
                SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 NewSessionTicket: ' + LError);
                Exit;
              end;
              FTLS12SessionTicket := Copy(LNewSessionTicket.Ticket, 0, Length(LNewSessionTicket.Ticket));
              FTLS12SessionTicketLifetimeHint := LNewSessionTicket.TicketLifetimeHint;
            end;
            Continue;
          end;

          if not (
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) and
              TryDecryptTLS12ChaCha20Poly1305Record(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_HANDSHAKE,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              )) or
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256) and
              TryDecryptTLS12AES128GCMRecord(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_HANDSHAKE,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              )) or
            ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384) and
              TryDecryptTLS12AES256GCMRecord(
                LReadKey,
                LReadIV,
                LReadSequence,
                TLS_CONTENT_TYPE_HANDSHAKE,
                TLS12_VERSION,
                LPayloadBytes,
                LPlaintext,
                LError
              ))
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS 1.2 encrypted handshake record: ' + LError);
            Exit;
          end;

          if FIsServerMode then
            Inc(FTLS12ClientSequence)
          else
            Inc(FTLS12ServerSequence);
          AppendHandshakeBytes(LHandshakeBuffer, LPlaintext);
          while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
          begin
            SetHandshakeError(
              sslErrUnsupported,
              Format('Unsupported TLS 1.2 post-handshake message type %d', [LHandshakeMessage[0]])
            );
            Exit;
          end;
          Continue;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d in TLS 1.2 application phase', [LHeader.ContentType])
        );
        Exit;
      end;
    end;
  end;
end;

function TFreePascalConnection.SendTLS12ApplicationDataFragment(const AFragment: TBytes): Boolean;
var
  LEncrypted: TBytes;
  LRecord: TBytes;
  LError: string;
  LWriteKey, LWriteIV: TBytes;
  LWriteSequence: QWord;
begin
  Result := False;

  if FIsServerMode then
  begin
    LWriteKey := FTLS12ServerWriteKey;
    LWriteIV := FTLS12ServerWriteIV;
    LWriteSequence := FTLS12ServerSequence;
  end
  else
  begin
    LWriteKey := FTLS12ClientWriteKey;
    LWriteIV := FTLS12ClientWriteIV;
    LWriteSequence := FTLS12ClientSequence;
  end;

  if (Length(LWriteKey) = 0) or (Length(LWriteIV) = 0) then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.2 application secrets are not ready');
    Exit;
  end;

  if not (
    ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) and
      TryEncryptTLS12ChaCha20Poly1305Record(
        LWriteKey,
        LWriteIV,
        LWriteSequence,
        TLS_CONTENT_TYPE_APPLICATION_DATA,
        TLS12_VERSION,
        AFragment,
        LEncrypted,
        LError
      )) or
    ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256) and
      TryEncryptTLS12AES128GCMRecord(
        LWriteKey,
        LWriteIV,
        LWriteSequence,
        TLS_CONTENT_TYPE_APPLICATION_DATA,
        TLS12_VERSION,
        AFragment,
        LEncrypted,
        LError
      )) or
    ((FTLS12CipherSuite = TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384) and
      TryEncryptTLS12AES256GCMRecord(
        LWriteKey,
        LWriteIV,
        LWriteSequence,
        TLS_CONTENT_TYPE_APPLICATION_DATA,
        TLS12_VERSION,
        AFragment,
        LEncrypted,
        LError
      ))
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS 1.2 application record: ' + LError);
    Exit;
  end;

  LRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_APPLICATION_DATA, LEncrypted);
  if not SendBufferedRecord(LRecord, fpPendingWriteApplicationData) then
  begin
    if not IsDeferredIOState then
      SetHandshakeError(sslErrIO, 'Failed to send TLS 1.2 application record');
    Exit;
  end;

  if FIsServerMode then
    Inc(FTLS12ServerSequence)
  else
    Inc(FTLS12ClientSequence);
  Result := True;
end;

function TFreePascalConnection.RecvApplicationDataFragment(out AFragment: TBytes): Boolean;
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

  if FTransportEOF and (Length(FTransportReadBuffer) > 0) then
  begin
    SetHandshakeError(sslErrIO, 'TLS application record truncated by transport EOF');
    Exit;
  end;

  if FReceivedCloseNotify or FTransportEOF then
  begin
    Result := True;
    Exit;
  end;

  if not FApplicationSecrets.Valid then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.3 application secrets are not ready');
    Exit;
  end;

  for LRecordIndex := 1 to 128 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FTransportEOF and (Length(FTransportReadBuffer) > 0) then
      begin
        SetHandshakeError(sslErrIO, 'TLS application record truncated by transport EOF');
        Exit;
      end;
      if FReceivedCloseNotify or FTransportEOF then
      begin
        Result := True;
        Exit;
      end;
      if IsRetryableIOState or (FLastErrorCode = sslErrTimeout) then
        Exit(False);
      SetHandshakeError(sslErrIO, 'Failed to receive TLS application record');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;

      TLS_CONTENT_TYPE_ALERT:
        begin
          if (Length(LPayloadBytes) >= 2) and (LPayloadBytes[1] = 0) then
          begin
            MarkGracefulEOF;
            Result := True;
            Exit;
          end;
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
                Continue;
              end;

            TLS_CONTENT_TYPE_ALERT:
              begin
                if Length(LInnerFragment) >= 2 then
                begin
                  LAlertLevel := LInnerFragment[0];
                  LAlertDescription := LInnerFragment[1];
                  if LAlertDescription = 0 then
                  begin
                    MarkGracefulEOF;
                    Result := True;
                    Exit;
                  end;
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
    if not SendBufferedRecord(LRecord, fpPendingWriteApplicationData) then
    begin
      if not IsDeferredIOState then
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
    if not SendBufferedRecord(LRecord, fpPendingWriteApplicationData) then
    begin
      if not IsDeferredIOState then
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
  LResumptionSession: IFreePascalResumptionSession;
  LUsingResumption: Boolean;
  LClientRandom: TBytes;
  LClientSessionID: TBytes;
  LZeroBinderHandshake: TBytes;
  LClientHelloInfo: TTLS13ClientHelloInfo;
  LClientHelloParseError: string;
  LBinder: TBytes;
  LBinderError: string;
  LTicketAgeMs: Int64;
  LObfuscatedTicketAge: Cardinal;
begin
  Result := False;

  FSelectedALPNProtocol := '';
  SetLength(FHandshakeSharedSecret, 0);
  SetLength(FPeerCertificates, 0);
  FSessionReused := False;
  ClearTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  ClearTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FTransportReadBuffer, 0);
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  SetLength(FResumptionTranscriptHash, 0);
  ClearReadClosureState;
  FIsServerMode := False;

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

  LUsingResumption := False;
  if (FConfiguredSession <> nil) and
    Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession) and
    LResumptionSession.HasResumptionMaterial and
    (TLS13CipherSuiteHashSize(LResumptionSession.GetCipherSuite) > 0) then
  begin
    LTicketAgeMs := MilliSecondsBetween(Now, FConfiguredSession.GetCreationTime);
    if LTicketAgeMs < 0 then
      LTicketAgeMs := 0;
    LObfuscatedTicketAge := Cardinal((QWord(LTicketAgeMs) + QWord(LResumptionSession.GetTicket.TicketAgeAdd)) and $FFFFFFFF);

    LClientRandom := GenerateSecureRandomBytes(32);
    LClientSessionID := GenerateSecureRandomBytes(32);
    LZeroBinderHandshake := BuildTLS13ClientHelloHandshakeWithPSK(
      FServerName,
      FALPNProtocols,
      FX25519PublicKey,
      LClientRandom,
      LClientSessionID,
      LResumptionSession.GetCipherSuite,
      LResumptionSession.GetTicket.Ticket,
      LObfuscatedTicketAge,
      ZeroBytes(TLS13CipherSuiteHashSize(LResumptionSession.GetCipherSuite))
    );

    if TryParseTLS13ClientHelloFromHandshake(LZeroBinderHandshake, LClientHelloInfo, LClientHelloParseError) and
      TryComputeResumptionBinder(
        LResumptionSession.GetCipherSuite,
        LResumptionSession.GetResumptionPSK,
        BuildBinderTranscript(LZeroBinderHandshake, LClientHelloInfo.PSKBindersOffset),
        LBinder,
        LBinderError
      ) then
    begin
      LClientHelloHandshake := Copy(LZeroBinderHandshake, 0, Length(LZeroBinderHandshake));
      Move(LBinder[0], LClientHelloHandshake[LClientHelloInfo.PSKBinderOffset], Length(LBinder));
      LUsingResumption := True;
    end;
  end;

  if not LUsingResumption then
    LClientHelloHandshake := BuildConfiguredTLS13ClientHelloHandshake(
      FContext,
      FServerName,
      FALPNProtocols,
      FX25519PublicKey
    );
  LClientHelloRecord := BuildTLSPlaintext(TLS_CONTENT_TYPE_HANDSHAKE, LClientHelloHandshake);

  if not SendAll(LClientHelloRecord) then
  begin
    FLastErrorCode := sslErrIO;
    FLastErrorString := 'Failed to send TLS ClientHello';
    RecordError(FLastErrorCode, FLastErrorString);
    Exit;
  end;

  for LRecordIndex := 1 to 8 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
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
            if (not LUsingResumption) or (LServerHello.SelectedIdentity <> 0) then
            begin
              FLastErrorCode := sslErrProtocol;
              FLastErrorString := 'Server selected unexpected TLS 1.3 PSK identity';
              RecordError(FLastErrorCode, FLastErrorString);
              Exit;
            end;

            if not TryDeriveTLS13HandshakeSecretsWithPSK(
              LServerHello.SelectedCipherSuite,
              LResumptionSession.GetResumptionPSK,
              FHandshakeSharedSecret,
              LTranscriptData,
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
          end
          else if not TryDeriveTLS13HandshakeSecrets(
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

          if not SendClientFinished(LServerHello.SelectedCipherSuite, LTranscriptData) then
            Exit;

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

          FClientApplicationSeq := 0;
          FServerApplicationSeq := 0;
          SetLength(FApplicationReadBuffer, 0);
          ClearPendingWriteState;
          SetLength(FPostHandshakeBuffer, 0);
          FSessionTicketCount := 0;
          InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;

          FProtocolVersion := sslProtocolTLS13;
          FCipherName := TLS13CipherSuiteToString(LServerHello.SelectedCipherSuite);
          if not ValidatePostHandshake(True) then
            Exit(False);
          Result := True;
          Exit;
        end;
    end;
  end;

  FLastErrorCode := sslErrProtocol;
  FLastErrorString := 'ServerHello not received in expected handshake records';
  RecordError(FLastErrorCode, FLastErrorString);
end;

function TFreePascalConnection.ConnectTLS12Client: Boolean;
var
  LCipherSuites: TTLS12CipherSuiteArray;
  LClientRandom: TBytes;
  LClientSessionID: TBytes;
  LConfiguredSessionID: TBytes;
  LConfiguredSessionTicket: TBytes;
  LConfiguredSessionTicketLifetimeHint: Cardinal;
  LClientHelloHandshake: TBytes;
  LClientHelloRecord: TBytes;
  LHandshakeBuffer: TBytes;
  LHandshakeMessage: TBytes;
  LTranscriptData: TBytes;
  LHeader: TTLSRecordHeader;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LRecordIndex: Integer;
  LServerHello: TTLS12ServerHelloInfo;
  LServerHelloSeen: Boolean;
  LServerHelloDoneSeen: Boolean;
  LServerKeyExchangeSeen: Boolean;
  LServerCCSSeen: Boolean;
  LServerFinishedSeen: Boolean;
  LCertificateError: string;
  LServerKeyExchange: TTLS12ServerKeyExchangeInfo;
  LServerCert: TX509Certificate;
  LSignedParams: TBytes;
  LPreMasterSecret: TBytes;
  LMasterSecret: TBytes;
  LKeyBlock: TBytes;
  LClientWriteKey: TBytes;
  LServerWriteKey: TBytes;
  LClientWriteIV: TBytes;
  LServerWriteIV: TBytes;
  LClientKeyExchangeHandshake: TBytes;
  LClientKeyExchangeRecord: TBytes;
  LCCSRecord: TBytes;
  LClientFinishedVerifyData: TBytes;
  LClientFinishedHandshake: TBytes;
  LEncryptedFinished: TBytes;
  LEncryptedFinishedRecord: TBytes;
  LServerFinishedPlaintext: TBytes;
  LServerFinishedExpected: TBytes;
  LServerFinishedBuffer: TBytes;
  LError: string;
  LKeyBlockLen: Integer;
  LTLS12ClientKeyShare: TBytes;
  LResumptionSession: IFreePascalResumptionSession;
  LCachedMasterSecret: TBytes;
  LCachedCipherSuite: Word;
  LNewSessionTicket: TTLS12NewSessionTicketInfo;
  LResumptionAttempted: Boolean;
  LAbbreviatedHandshake: Boolean;

  procedure DeriveTLS12TrafficKeys(const AMasterSecret: TBytes);
  begin
    LKeyBlockLen := TLS12KeyBlockLength(LServerHello.SelectedCipherSuite);
    if LKeyBlockLen = 0 then
    begin
      SetHandshakeError(sslErrUnsupported, 'Unsupported TLS 1.2 cipher suite for key block derivation');
      Exit;
    end;

    if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
      LKeyBlock := TLS12KeyBlock_SHA384(AMasterSecret, LServerHello.Random, LClientRandom, LKeyBlockLen)
    else
      LKeyBlock := TLS12KeyBlock_SHA256(AMasterSecret, LServerHello.Random, LClientRandom, LKeyBlockLen);
    if Length(LKeyBlock) <> LKeyBlockLen then
    begin
      SetHandshakeError(sslErrHandshake, 'TLS 1.2 key block length mismatch');
      Exit;
    end;

    if TLS12CipherSuiteIsChaCha(LServerHello.SelectedCipherSuite) then
    begin
      LClientWriteKey := Copy(LKeyBlock, 0, 32);
      LServerWriteKey := Copy(LKeyBlock, 32, 32);
      LClientWriteIV := Copy(LKeyBlock, 64, 12);
      LServerWriteIV := Copy(LKeyBlock, 76, 12);
    end
    else if TLS12CipherSuiteIsAES128GCM(LServerHello.SelectedCipherSuite) then
    begin
      LClientWriteKey := Copy(LKeyBlock, 0, 16);
      LServerWriteKey := Copy(LKeyBlock, 16, 16);
      LClientWriteIV := Copy(LKeyBlock, 32, 4);
      LServerWriteIV := Copy(LKeyBlock, 36, 4);
    end
    else
    begin
      LClientWriteKey := Copy(LKeyBlock, 0, 32);
      LServerWriteKey := Copy(LKeyBlock, 32, 32);
      LClientWriteIV := Copy(LKeyBlock, 64, 4);
      LServerWriteIV := Copy(LKeyBlock, 68, 4);
    end;
  end;
begin
  Result := False;

  if FStream <> nil then
  begin
    MarkUnsupported('TLS 1.2 client handshake over TStream');
    Exit(False);
  end;

  FSelectedALPNProtocol := '';
  FCipherName := '';
  FSessionReused := False;
  SetLength(FPeerCertificates, 0);
  SetLength(FTransportReadBuffer, 0);
  SetLength(FApplicationReadBuffer, 0);
  ClearPendingWriteState;
  ClearReadClosureState;
  FIsServerMode := False;
  SetLength(FTLS12SessionID, 0);
  SetLength(FTLS12MasterSecret, 0);
  SetLength(FTLS12SessionTicket, 0);
  FTLS12SessionTicketLifetimeHint := 0;

  LCipherSuites := ResolveConfiguredTLS12CipherSuites(FContext);
  LClientRandom := GenerateSecureRandomBytes(TLS12_RANDOM_SIZE);
  SetLength(LConfiguredSessionID, 0);
  SetLength(LConfiguredSessionTicket, 0);
  LConfiguredSessionTicketLifetimeHint := 0;
  SetLength(LCachedMasterSecret, 0);
  LCachedCipherSuite := 0;
  LResumptionAttempted := False;
  LAbbreviatedHandshake := False;
  if (FConfiguredSession <> nil) and
    (FConfiguredSession.GetProtocolVersion = sslProtocolTLS12) and
    Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession) and
    (LResumptionSession.HasTLS12ResumptionMaterial or LResumptionSession.HasTLS12TicketMaterial) then
  begin
    LConfiguredSessionID := LResumptionSession.GetTLS12SessionIDBytes;
    LConfiguredSessionTicket := LResumptionSession.GetTLS12SessionTicket;
    LConfiguredSessionTicketLifetimeHint := LResumptionSession.GetTLS12SessionTicketLifetimeHint;
    LCachedMasterSecret := LResumptionSession.GetTLS12MasterSecret;
    LCachedCipherSuite := LResumptionSession.GetCipherSuite;
    LResumptionAttempted := ((Length(LConfiguredSessionID) > 0) or (Length(LConfiguredSessionTicket) > 0)) and
      (Length(LCachedMasterSecret) = TLS12_MASTER_SECRET_LENGTH) and
      TLS12CipherSuiteIsSupported(LCachedCipherSuite);
  end;
  if LResumptionAttempted then
    PreferTLS12CipherSuite(LCipherSuites, LCachedCipherSuite);
  if LResumptionAttempted then
    LClientSessionID := Copy(LConfiguredSessionID, 0, Length(LConfiguredSessionID))
  else
    LClientSessionID := GenerateSecureRandomBytes(32);
  LClientHelloHandshake := BuildTLS12ClientHelloHandshakeWithParamsAndTicket(
    FServerName,
    FALPNProtocols,
    LClientRandom,
    LClientSessionID,
    LCipherSuites,
    LConfiguredSessionTicket
  );
  LClientHelloRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LClientHelloHandshake);

  if not SendAll(LClientHelloRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send TLS 1.2 ClientHello');
    Exit;
  end;

  LTranscriptData := Copy(LClientHelloHandshake, 0, Length(LClientHelloHandshake));
  SetLength(LHandshakeBuffer, 0);
  LServerHelloSeen := False;
  LServerHelloDoneSeen := False;
  LServerKeyExchangeSeen := False;

  for LRecordIndex := 1 to 32 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
      SetHandshakeError(sslErrIO, 'Failed to receive TLS 1.2 server flight record');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          AppendHandshakeBytes(LHandshakeBuffer, LPayloadBytes);
          while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
          begin
            case LHandshakeMessage[0] of
              TLS_HANDSHAKE_TYPE_SERVER_HELLO:
                begin
                  if not TryParseTLS12ServerHelloFromHandshake(LHandshakeMessage, LServerHello) then
                  begin
                    SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 ServerHello');
                    Exit;
                  end;
                  if LServerHello.ServerVersion <> TLS12_VERSION then
                  begin
                    SetHandshakeError(sslErrProtocol, 'Server did not negotiate TLS 1.2');
                    Exit;
                  end;
                  if not TLS12CipherSuiteIsSupported(LServerHello.SelectedCipherSuite) then
                  begin
                    SetHandshakeError(
                      sslErrUnsupported,
                      'Only TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256 / TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256 are supported in pure TLS 1.2 client path'
                    );
                    Exit;
                  end;
                  FSelectedALPNProtocol := LServerHello.SelectedALPNProtocol;
                  LServerHelloSeen := True;
                end;

              TLS_HANDSHAKE_TYPE_CERTIFICATE:
                begin
                  if not TryParseTLS12CertificateFromHandshake(LHandshakeMessage, FPeerCertificates, LCertificateError) then
                  begin
                    SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 Certificate: ' + LCertificateError);
                    Exit;
                  end;
                end;

              TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE:
                begin
                  if not TryParseTLS12ServerKeyExchangeECDHERSAFromHandshake(
                    LHandshakeMessage,
                    LServerKeyExchange,
                    LError
                  ) then
                  begin
                    SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 ServerKeyExchange: ' + LError);
                    Exit;
                  end;
                  LServerKeyExchangeSeen := True;
                end;

              TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE:
                begin
                  if not TryParseTLS12ServerHelloDoneFromHandshake(LHandshakeMessage) then
                  begin
                    SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 ServerHelloDone');
                    Exit;
                  end;
                  LServerHelloDoneSeen := True;
                end;

            else
              begin
                SetHandshakeError(
                  sslErrUnsupported,
                  Format('Unsupported TLS 1.2 server flight message type %d', [LHandshakeMessage[0]])
                );
                Exit;
              end;
            end;

            AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
            if LServerHelloDoneSeen then
              Break;
          end;

          if LServerHelloDoneSeen then
            Break;
        end;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned TLS alert during TLS 1.2 handshake');
          Exit;
        end;

      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        begin
          if LResumptionAttempted and LServerHelloSeen and
            (not LServerKeyExchangeSeen) and (not LServerHelloDoneSeen) and
            (Length(FPeerCertificates) = 0) then
          begin
            if (Length(LPayloadBytes) <> 1) or (LPayloadBytes[0] <> 1) then
            begin
              SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 ChangeCipherSpec payload from resumed server');
              Exit;
            end;
            LServerCCSSeen := True;
            LAbbreviatedHandshake := True;
            Break;
          end;
          Continue;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d during TLS 1.2 server flight', [LHeader.ContentType])
        );
        Exit;
      end;
    end;
  end;

  if LAbbreviatedHandshake then
  begin
    if Length(LCachedMasterSecret) <> TLS12_MASTER_SECRET_LENGTH then
    begin
      SetHandshakeError(sslErrProtocol, 'TLS 1.2 resumed handshake is missing cached master secret');
      Exit;
    end;

    LMasterSecret := Copy(LCachedMasterSecret, 0, Length(LCachedMasterSecret));
    DeriveTLS12TrafficKeys(LMasterSecret);
    if FLastErrorCode <> sslErrNone then
      Exit;

    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
      SetHandshakeError(sslErrIO, 'Failed to receive resumed TLS 1.2 server Finished');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          if TLS12CipherSuiteIsChaCha(LServerHello.SelectedCipherSuite) then
          begin
            if not TryDecryptTLS12ChaCha20Poly1305Record(
              LServerWriteKey,
              LServerWriteIV,
              0,
              TLS_CONTENT_TYPE_HANDSHAKE,
              TLS12_VERSION,
              LPayloadBytes,
              LServerFinishedPlaintext,
              LError
            ) then
            begin
              SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt resumed TLS 1.2 server Finished: ' + LError);
              Exit;
            end;
          end
          else if TLS12CipherSuiteIsAES128GCM(LServerHello.SelectedCipherSuite) and not TryDecryptTLS12AES128GCMRecord(
            LServerWriteKey,
            LServerWriteIV,
            0,
            TLS_CONTENT_TYPE_HANDSHAKE,
            TLS12_VERSION,
            LPayloadBytes,
            LServerFinishedPlaintext,
            LError
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt resumed TLS 1.2 server Finished: ' + LError);
            Exit;
          end
          else if TLS12CipherSuiteIsAES256GCM(LServerHello.SelectedCipherSuite) and not TryDecryptTLS12AES256GCMRecord(
            LServerWriteKey,
            LServerWriteIV,
            0,
            TLS_CONTENT_TYPE_HANDSHAKE,
            TLS12_VERSION,
            LPayloadBytes,
            LServerFinishedPlaintext,
            LError
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt resumed TLS 1.2 server Finished: ' + LError);
            Exit;
          end;
        end;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned TLS alert during resumed TLS 1.2 handshake');
          Exit;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d during resumed TLS 1.2 handshake', [LHeader.ContentType])
        );
        Exit;
      end;
    end;

    SetLength(LServerFinishedBuffer, 0);
    AppendHandshakeBytes(LServerFinishedBuffer, LServerFinishedPlaintext);
    if not TryPopHandshakeMessage(LServerFinishedBuffer, LHandshakeMessage) then
    begin
      SetHandshakeError(sslErrProtocol, 'Resumed TLS 1.2 server Finished was not parsed');
      Exit;
    end;
    if LHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_FINISHED then
    begin
      SetHandshakeError(sslErrProtocol, 'Resumed TLS 1.2 server sent unexpected encrypted handshake message');
      Exit;
    end;

    if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
      LServerFinishedExpected := TLS12ComputeServerFinishedVerifyData_SHA384(LMasterSecret, LTranscriptData)
    else
      LServerFinishedExpected := TLS12ComputeServerFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
    if (ReadUInt24(LHandshakeMessage, 1) <> TLS12_FINISHED_VERIFY_DATA_LENGTH) or
      (not ConstantTimeBytesEqual(Copy(LHandshakeMessage, 4, TLS12_FINISHED_VERIFY_DATA_LENGTH), LServerFinishedExpected)) then
    begin
      SetHandshakeError(sslErrHandshake, 'Resumed TLS 1.2 server Finished verify_data mismatch');
      Exit;
    end;
    AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);

    LCCSRecord := BuildTLS12ChangeCipherSpecRecord;
    if not SendAll(LCCSRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send resumed TLS 1.2 ChangeCipherSpec');
      Exit;
    end;

    if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
      LClientFinishedVerifyData := TLS12ComputeClientFinishedVerifyData_SHA384(LMasterSecret, LTranscriptData)
    else
      LClientFinishedVerifyData := TLS12ComputeClientFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
    LClientFinishedHandshake := BuildTLS12FinishedHandshake(LClientFinishedVerifyData);
    if TLS12CipherSuiteIsChaCha(LServerHello.SelectedCipherSuite) then
    begin
      if not TryEncryptTLS12ChaCha20Poly1305Record(
        LClientWriteKey,
        LClientWriteIV,
        0,
        TLS_CONTENT_TYPE_HANDSHAKE,
        TLS12_VERSION,
        LClientFinishedHandshake,
        LEncryptedFinished,
        LError
      ) then
      begin
        SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt resumed TLS 1.2 Finished: ' + LError);
        Exit;
      end;
    end
    else if TLS12CipherSuiteIsAES128GCM(LServerHello.SelectedCipherSuite) and not TryEncryptTLS12AES128GCMRecord(
      LClientWriteKey,
      LClientWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LClientFinishedHandshake,
      LEncryptedFinished,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt resumed TLS 1.2 Finished: ' + LError);
      Exit;
    end
    else if TLS12CipherSuiteIsAES256GCM(LServerHello.SelectedCipherSuite) and not TryEncryptTLS12AES256GCMRecord(
      LClientWriteKey,
      LClientWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LClientFinishedHandshake,
      LEncryptedFinished,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt resumed TLS 1.2 Finished: ' + LError);
      Exit;
    end;

    LEncryptedFinishedRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LEncryptedFinished);
    if not SendAll(LEncryptedFinishedRecord) then
    begin
      SetHandshakeError(sslErrIO, 'Failed to send resumed TLS 1.2 Finished');
      Exit;
    end;

    FTLS12SessionID := Copy(LServerHello.SessionID, 0, Length(LServerHello.SessionID));
    FTLS12MasterSecret := Copy(LMasterSecret, 0, Length(LMasterSecret));
    if Length(FTLS12SessionID) = 0 then
      FTLS12SessionID := Copy(LConfiguredSessionID, 0, Length(LConfiguredSessionID));
    if Length(FTLS12SessionTicket) = 0 then
    begin
      FTLS12SessionTicket := Copy(LConfiguredSessionTicket, 0, Length(LConfiguredSessionTicket));
      FTLS12SessionTicketLifetimeHint := LConfiguredSessionTicketLifetimeHint;
    end;
    FTLS12ClientWriteKey := Copy(LClientWriteKey, 0, Length(LClientWriteKey));
    FTLS12ServerWriteKey := Copy(LServerWriteKey, 0, Length(LServerWriteKey));
    FTLS12ClientWriteIV := Copy(LClientWriteIV, 0, Length(LClientWriteIV));
    FTLS12ServerWriteIV := Copy(LServerWriteIV, 0, Length(LServerWriteIV));
    FTLS12ClientSequence := 1;
    FTLS12ServerSequence := 1;
    FTLS12CipherSuite := LServerHello.SelectedCipherSuite;

    FSessionReused := True;
    FProtocolVersion := sslProtocolTLS12;
    FCipherName := TLS12CipherSuiteToString(LServerHello.SelectedCipherSuite);
    ClearOperationalErrorState;
    if not ValidatePostHandshake(True) then
      Exit(False);

    Result := True;
    Exit;
  end;

  if not (LServerHelloSeen and LServerKeyExchangeSeen and LServerHelloDoneSeen) then
  begin
    SetHandshakeError(sslErrProtocol, 'Incomplete TLS 1.2 server flight');
    Exit;
  end;

  if Length(FPeerCertificates) = 0 then
  begin
    SetHandshakeError(sslErrCertificate, 'TLS 1.2 server did not send certificate');
    Exit;
  end;

  if LServerKeyExchange.CurveType <> 3 then
  begin
    SetHandshakeError(sslErrUnsupported, 'Only named_curve ECDHE parameters are supported in TLS 1.2');
    Exit;
  end;

  if (LServerKeyExchange.NamedCurve <> TLS_GROUP_X25519) and
    (LServerKeyExchange.NamedCurve <> TLS_GROUP_SECP256R1) then
  begin
    SetHandshakeError(sslErrUnsupported, 'Only X25519 / secp256r1 ECDHE are supported in pure TLS 1.2 client path');
    Exit;
  end;

  if (LServerKeyExchange.SignatureAlgorithm <> TLS_SIG_RSA_PKCS1_SHA256) and
    (LServerKeyExchange.SignatureAlgorithm <> TLS_SIG_RSA_PKCS1_SHA512) and
    (LServerKeyExchange.SignatureAlgorithm <> TLS_SIG_RSA_PSS_RSAE_SHA256) then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      'Only rsa_pkcs1_sha256 / rsa_pkcs1_sha512 / rsa_pss_rsae_sha256 ServerKeyExchange signatures are supported in pure TLS 1.2 client path'
    );
    Exit;
  end;

  LServerCert := TX509Certificate.Create;
  try
    LServerCert.LoadFromDER(FPeerCertificates[0]);
    if not SameText(LServerCert.PublicKeyInfo.KeyType, 'RSA') then
    begin
      SetHandshakeError(sslErrUnsupported, 'Only RSA leaf certificates are supported in pure TLS 1.2 client path');
      Exit;
    end;

    LSignedParams := nil;
    AppendBytes(LSignedParams, LClientRandom);
    AppendBytes(LSignedParams, LServerHello.Random);
    AppendBytes(LSignedParams, BuildTLS12ServerECDHParamsBytes(LServerKeyExchange));

    if (
      ((LServerKeyExchange.SignatureAlgorithm = TLS_SIG_RSA_PKCS1_SHA256) and
        TryVerifyTLS12RSAPKCS1v15SHA256Signature(
          LSignedParams,
          LServerCert.PublicKeyInfo.RSAModulus,
          LServerCert.PublicKeyInfo.RSAExponent,
          LServerKeyExchange.Signature,
          LError
        )) or
      ((LServerKeyExchange.SignatureAlgorithm = TLS_SIG_RSA_PKCS1_SHA512) and
        TryVerifyTLS12RSAPKCS1v15SHA512Signature(
          LSignedParams,
          LServerCert.PublicKeyInfo.RSAModulus,
          LServerCert.PublicKeyInfo.RSAExponent,
          LServerKeyExchange.Signature,
          LError
        )) or
      ((LServerKeyExchange.SignatureAlgorithm = TLS_SIG_RSA_PSS_RSAE_SHA256) and
        TryVerifyTLS12RSAPSSSHA256Signature(
          LSignedParams,
          LServerCert.PublicKeyInfo.RSAModulus,
          LServerCert.PublicKeyInfo.RSAExponent,
          LServerKeyExchange.Signature,
          LError
        ))
    ) then
    begin
    end
    else
    begin
      SetHandshakeError(sslErrHandshake, 'TLS 1.2 ServerKeyExchange signature verification failed: ' + LError);
      Exit;
    end;
  finally
    LServerCert.Free;
  end;

  if LServerKeyExchange.NamedCurve = TLS_GROUP_X25519 then
  begin
    if Length(LServerKeyExchange.PublicKey) <> 32 then
    begin
      SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 X25519 public key length');
      Exit;
    end;

    FX25519PrivateKey := GenerateX25519PrivateKey;
    FX25519PublicKey := X25519PublicKeyFromPrivate(FX25519PrivateKey);
    LTLS12ClientKeyShare := Copy(FX25519PublicKey, 0, Length(FX25519PublicKey));
    try
      LPreMasterSecret := X25519ComputeSharedSecret(FX25519PrivateKey, LServerKeyExchange.PublicKey);
    except
      on E: Exception do
      begin
        SetHandshakeError(sslErrHandshake, 'Failed to compute TLS 1.2 X25519 shared secret: ' + E.Message);
        Exit;
      end;
    end;
  end
  else
  begin
    if not TryGenerateECDHEP256KeyShare(FX25519PrivateKey, LTLS12ClientKeyShare, LError) then
    begin
      SetHandshakeError(sslErrHandshake, 'Failed to generate TLS 1.2 P-256 key share: ' + LError);
      Exit;
    end;
    if not TryComputeECDHEP256SharedSecret(FX25519PrivateKey, LServerKeyExchange.PublicKey, LPreMasterSecret, LError) then
    begin
      SetHandshakeError(sslErrHandshake, 'Failed to compute TLS 1.2 P-256 shared secret: ' + LError);
      Exit;
    end;
  end;

  if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
    LMasterSecret := TLS12MasterSecret_SHA384(LPreMasterSecret, LClientRandom, LServerHello.Random)
  else
    LMasterSecret := TLS12MasterSecret_SHA256(LPreMasterSecret, LClientRandom, LServerHello.Random);
  LKeyBlockLen := TLS12KeyBlockLength(LServerHello.SelectedCipherSuite);
  if LKeyBlockLen = 0 then
  begin
    SetHandshakeError(sslErrUnsupported, 'Unsupported TLS 1.2 cipher suite for key block derivation');
    Exit;
  end;

  if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
    LKeyBlock := TLS12KeyBlock_SHA384(LMasterSecret, LServerHello.Random, LClientRandom, LKeyBlockLen)
  else
    LKeyBlock := TLS12KeyBlock_SHA256(LMasterSecret, LServerHello.Random, LClientRandom, LKeyBlockLen);
  if Length(LKeyBlock) <> LKeyBlockLen then
  begin
    SetHandshakeError(sslErrHandshake, 'TLS 1.2 key block length mismatch');
    Exit;
  end;

  if TLS12CipherSuiteIsChaCha(LServerHello.SelectedCipherSuite) then
  begin
    LClientWriteKey := Copy(LKeyBlock, 0, 32);
    LServerWriteKey := Copy(LKeyBlock, 32, 32);
    LClientWriteIV := Copy(LKeyBlock, 64, 12);
    LServerWriteIV := Copy(LKeyBlock, 76, 12);
  end
  else if TLS12CipherSuiteIsAES128GCM(LServerHello.SelectedCipherSuite) then
  begin
    LClientWriteKey := Copy(LKeyBlock, 0, 16);
    LServerWriteKey := Copy(LKeyBlock, 16, 16);
    LClientWriteIV := Copy(LKeyBlock, 32, 4);
    LServerWriteIV := Copy(LKeyBlock, 36, 4);
  end
  else
  begin
    LClientWriteKey := Copy(LKeyBlock, 0, 32);
    LServerWriteKey := Copy(LKeyBlock, 32, 32);
    LClientWriteIV := Copy(LKeyBlock, 64, 4);
    LServerWriteIV := Copy(LKeyBlock, 68, 4);
  end;

  LClientKeyExchangeHandshake := BuildTLS12ClientKeyExchangeHandshake(LTLS12ClientKeyShare);
  AppendHandshakeBytes(LTranscriptData, LClientKeyExchangeHandshake);
  LClientKeyExchangeRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LClientKeyExchangeHandshake);
  if not SendAll(LClientKeyExchangeRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send TLS 1.2 ClientKeyExchange');
    Exit;
  end;

  if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
    LClientFinishedVerifyData := TLS12ComputeClientFinishedVerifyData_SHA384(LMasterSecret, LTranscriptData)
  else
    LClientFinishedVerifyData := TLS12ComputeClientFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
  LClientFinishedHandshake := BuildTLS12FinishedHandshake(LClientFinishedVerifyData);
  LCCSRecord := BuildTLS12ChangeCipherSpecRecord;
  if not SendAll(LCCSRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send TLS 1.2 ChangeCipherSpec');
    Exit;
  end;

  if TLS12CipherSuiteIsChaCha(LServerHello.SelectedCipherSuite) then
  begin
    if not TryEncryptTLS12ChaCha20Poly1305Record(
      LClientWriteKey,
      LClientWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LClientFinishedHandshake,
      LEncryptedFinished,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS 1.2 Finished: ' + LError);
      Exit;
    end;
  end
  else if TLS12CipherSuiteIsAES128GCM(LServerHello.SelectedCipherSuite) and not TryEncryptTLS12AES128GCMRecord(
    LClientWriteKey,
    LClientWriteIV,
    0,
    TLS_CONTENT_TYPE_HANDSHAKE,
    TLS12_VERSION,
    LClientFinishedHandshake,
    LEncryptedFinished,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS 1.2 Finished: ' + LError);
    Exit;
  end
  else if TLS12CipherSuiteIsAES256GCM(LServerHello.SelectedCipherSuite) and not TryEncryptTLS12AES256GCMRecord(
    LClientWriteKey,
    LClientWriteIV,
    0,
    TLS_CONTENT_TYPE_HANDSHAKE,
    TLS12_VERSION,
    LClientFinishedHandshake,
    LEncryptedFinished,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS 1.2 Finished: ' + LError);
    Exit;
  end;

  LEncryptedFinishedRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LEncryptedFinished);
  if not SendAll(LEncryptedFinishedRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send encrypted TLS 1.2 Finished');
    Exit;
  end;

  AppendHandshakeBytes(LTranscriptData, LClientFinishedHandshake);

  SetLength(LServerFinishedBuffer, 0);
  LServerCCSSeen := False;
  LServerFinishedSeen := False;
  for LRecordIndex := 1 to 16 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
      SetHandshakeError(sslErrIO, 'Failed to receive TLS 1.2 server finished flight');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        begin
          if (Length(LPayloadBytes) <> 1) or (LPayloadBytes[0] <> 1) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 ChangeCipherSpec payload from server');
            Exit;
          end;
          LServerCCSSeen := True;
        end;

      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          if not LServerCCSSeen then
          begin
            AppendHandshakeBytes(LServerFinishedBuffer, LPayloadBytes);
            while TryPopHandshakeMessage(LServerFinishedBuffer, LHandshakeMessage) do
            begin
              if LHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET then
              begin
                SetHandshakeError(sslErrProtocol, 'Unexpected plaintext TLS 1.2 handshake message before ChangeCipherSpec');
                Exit;
              end;
              if not TryParseTLS12NewSessionTicketFromHandshake(LHandshakeMessage, LNewSessionTicket, LError) then
              begin
                SetHandshakeError(sslErrProtocol, 'Invalid TLS 1.2 NewSessionTicket: ' + LError);
                Exit;
              end;
              FTLS12SessionTicket := Copy(LNewSessionTicket.Ticket, 0, Length(LNewSessionTicket.Ticket));
              FTLS12SessionTicketLifetimeHint := LNewSessionTicket.TicketLifetimeHint;
              AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
            end;
            Continue;
          end;

          if TLS12CipherSuiteIsChaCha(LServerHello.SelectedCipherSuite) then
          begin
            if not TryDecryptTLS12ChaCha20Poly1305Record(
              LServerWriteKey,
              LServerWriteIV,
              0,
              TLS_CONTENT_TYPE_HANDSHAKE,
              TLS12_VERSION,
              LPayloadBytes,
              LServerFinishedPlaintext,
              LError
            ) then
            begin
              SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS 1.2 server Finished: ' + LError);
              Exit;
            end;
          end
          else if TLS12CipherSuiteIsAES128GCM(LServerHello.SelectedCipherSuite) and not TryDecryptTLS12AES128GCMRecord(
            LServerWriteKey,
            LServerWriteIV,
            0,
            TLS_CONTENT_TYPE_HANDSHAKE,
            TLS12_VERSION,
            LPayloadBytes,
            LServerFinishedPlaintext,
            LError
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS 1.2 server Finished: ' + LError);
            Exit;
          end
          else if TLS12CipherSuiteIsAES256GCM(LServerHello.SelectedCipherSuite) and not TryDecryptTLS12AES256GCMRecord(
            LServerWriteKey,
            LServerWriteIV,
            0,
            TLS_CONTENT_TYPE_HANDSHAKE,
            TLS12_VERSION,
            LPayloadBytes,
            LServerFinishedPlaintext,
            LError
          ) then
          begin
            SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS 1.2 server Finished: ' + LError);
            Exit;
          end;

          AppendHandshakeBytes(LServerFinishedBuffer, LServerFinishedPlaintext);
          while TryPopHandshakeMessage(LServerFinishedBuffer, LHandshakeMessage) do
          begin
            if LHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_FINISHED then
            begin
              SetHandshakeError(
                sslErrUnsupported,
                Format('Unsupported TLS 1.2 encrypted handshake message type %d', [LHandshakeMessage[0]])
              );
              Exit;
            end;

            if TLS12CipherSuiteUsesSHA384(LServerHello.SelectedCipherSuite) then
              LServerFinishedExpected := TLS12ComputeServerFinishedVerifyData_SHA384(LMasterSecret, LTranscriptData)
            else
              LServerFinishedExpected := TLS12ComputeServerFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
            if (ReadUInt24(LHandshakeMessage, 1) <> TLS12_FINISHED_VERIFY_DATA_LENGTH) or
              (not ConstantTimeBytesEqual(Copy(LHandshakeMessage, 4, TLS12_FINISHED_VERIFY_DATA_LENGTH), LServerFinishedExpected)) then
            begin
              SetHandshakeError(sslErrHandshake, 'TLS 1.2 server Finished verify_data mismatch');
              Exit;
            end;

            LServerFinishedSeen := True;
            Break;
          end;

          if LServerFinishedSeen then
            Break;
        end;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned TLS alert during TLS 1.2 finished flight');
          Exit;
        end;

    else
      begin
        SetHandshakeError(
          sslErrProtocol,
          Format('Unexpected TLS record type %d during TLS 1.2 finished flight', [LHeader.ContentType])
        );
        Exit;
      end;
    end;
  end;

  if not LServerFinishedSeen then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.2 server Finished was not received');
    Exit;
  end;

  FTLS12ClientWriteKey := Copy(LClientWriteKey, 0, Length(LClientWriteKey));
  FTLS12ServerWriteKey := Copy(LServerWriteKey, 0, Length(LServerWriteKey));
  FTLS12ClientWriteIV := Copy(LClientWriteIV, 0, Length(LClientWriteIV));
  FTLS12ServerWriteIV := Copy(LServerWriteIV, 0, Length(LServerWriteIV));
  FTLS12ClientSequence := 1;
  FTLS12ServerSequence := 1;
  FTLS12CipherSuite := LServerHello.SelectedCipherSuite;
  FTLS12SessionID := Copy(LServerHello.SessionID, 0, Length(LServerHello.SessionID));
  FTLS12MasterSecret := Copy(LMasterSecret, 0, Length(LMasterSecret));

  FProtocolVersion := sslProtocolTLS12;
  FCipherName := TLS12CipherSuiteToString(LServerHello.SelectedCipherSuite);
  ClearOperationalErrorState;
  if not ValidatePostHandshake(True) then
    Exit(False);

  Result := True;
end;

function TFreePascalConnection.AcceptTLS12Server: Boolean;
var
  LHeader: TTLSRecordHeader;
  LPayloadBytes: TBytes;
  LRecordBytes: TBytes;
  LHandshakeBuffer: TBytes;
  LHandshakeMessage: TBytes;
  LClientHello: TTLS12ClientHelloInfo;
  LParseError: string;
  LRecordIndex: Integer;
  LSelectedCipherSuite: Word;
  LSelectedALPNProtocol: string;
  LClientHelloHandshake: TBytes;
  LServerRandom: TBytes;
  LSessionID: TBytes;
  LServerHelloHandshake: TBytes;
  LCertificateBlob: TBytes;
  LPrivateKeyBlob: TBytes;
  LCertificateHandshake: TBytes;
  LServerKeyExchangeHandshake: TBytes;
  LServerHelloDoneHandshake: TBytes;
  LServerFlightPayload: TBytes;
  LServerFlightRecord: TBytes;
  LContextMaterial: IFreePascalContextMaterial;
  LLeafCertificateDER: TBytes;
  LLeafCertificate: TX509Certificate;
  LSignatureScheme: Word;
  LSignature: TBytes;
  LSignedParams: TBytes;
  LError: string;
  LClientKeyExchangeSeen: Boolean;
  LClientCCSSeen: Boolean;
  LClientFinishedSeen: Boolean;
  LClientKeyExchangePublicKey: TBytes;
  LPreMasterSecret: TBytes;
  LMasterSecret: TBytes;
  LKeyBlock: TBytes;
  LKeyBlockLen: Integer;
  LServerPublicKey: TBytes;
  LNamedCurve: Word;
  LTranscriptData: TBytes;
  LClientFinishedPlaintext: TBytes;
  LClientFinishedExpected: TBytes;
  LCCSRecord: TBytes;
  LServerFinishedVerifyData: TBytes;
  LServerFinishedHandshake: TBytes;
  LEncryptedFinished: TBytes;
  LEncryptedFinishedRecord: TBytes;

  function SelectALPNProtocol(const AClientProtocols, AServerProtocols: string): string;
  var
    LClientList, LServerList: TStringArray;
    I, J: Integer;
    LClientProtocol: string;
  begin
    Result := '';
    if (AClientProtocols = '') or (AServerProtocols = '') then
      Exit;
    LClientList := AClientProtocols.Split([',']);
    LServerList := AServerProtocols.Split([',']);
    for I := 0 to High(LClientList) do
    begin
      LClientProtocol := Trim(LClientList[I]);
      if LClientProtocol = '' then
        Continue;
      for J := 0 to High(LServerList) do
        if SameText(LClientProtocol, Trim(LServerList[J])) then
          Exit(LClientProtocol);
    end;
  end;

  procedure DeriveTLS12TrafficKeys;
  begin
    if TLS12CipherSuiteUsesSHA384(LSelectedCipherSuite) then
      LMasterSecret := TLS12MasterSecret_SHA384(LPreMasterSecret, LClientHello.Random, LServerRandom)
    else
      LMasterSecret := TLS12MasterSecret_SHA256(LPreMasterSecret, LClientHello.Random, LServerRandom);

    LKeyBlockLen := TLS12KeyBlockLength(LSelectedCipherSuite);
    if LKeyBlockLen = 0 then
    begin
      SetHandshakeError(sslErrUnsupported, 'Unsupported TLS 1.2 cipher suite for key block derivation');
      Exit;
    end;

    if TLS12CipherSuiteUsesSHA384(LSelectedCipherSuite) then
      LKeyBlock := TLS12KeyBlock_SHA384(LMasterSecret, LServerRandom, LClientHello.Random, LKeyBlockLen)
    else
      LKeyBlock := TLS12KeyBlock_SHA256(LMasterSecret, LServerRandom, LClientHello.Random, LKeyBlockLen);

    if TLS12CipherSuiteIsChaCha(LSelectedCipherSuite) then
    begin
      FTLS12ClientWriteKey := Copy(LKeyBlock, 0, 32);
      FTLS12ServerWriteKey := Copy(LKeyBlock, 32, 32);
      FTLS12ClientWriteIV := Copy(LKeyBlock, 64, 12);
      FTLS12ServerWriteIV := Copy(LKeyBlock, 76, 12);
    end
    else if TLS12CipherSuiteIsAES128GCM(LSelectedCipherSuite) then
    begin
      FTLS12ClientWriteKey := Copy(LKeyBlock, 0, 16);
      FTLS12ServerWriteKey := Copy(LKeyBlock, 16, 16);
      FTLS12ClientWriteIV := Copy(LKeyBlock, 32, 4);
      FTLS12ServerWriteIV := Copy(LKeyBlock, 36, 4);
    end
    else
    begin
      FTLS12ClientWriteKey := Copy(LKeyBlock, 0, 32);
      FTLS12ServerWriteKey := Copy(LKeyBlock, 32, 32);
      FTLS12ClientWriteIV := Copy(LKeyBlock, 64, 4);
      FTLS12ServerWriteIV := Copy(LKeyBlock, 68, 4);
    end;
  end;
begin
  Result := False;
  NotifyInfoCallback(1, 0, 'handshake_start');

  FSelectedALPNProtocol := '';
  FCipherName := '';
  FSessionReused := False;
  SetLength(FPeerCertificates, 0);
  SetLength(FTransportReadBuffer, 0);
  SetLength(FApplicationReadBuffer, 0);
  ClearPendingWriteState;
  ClearReadClosureState;
  FIsServerMode := True;
  FTLS12CipherSuite := 0;
  SetLength(FTLS12SessionID, 0);
  SetLength(FTLS12MasterSecret, 0);
  SetLength(FTLS12SessionTicket, 0);
  FTLS12SessionTicketLifetimeHint := 0;
  FTLS12ClientSequence := 0;
  FTLS12ServerSequence := 0;

  if not Supports(FContext, IFreePascalContextMaterial, LContextMaterial) then
  begin
    SetHandshakeError(sslErrUnsupported, 'FreePascal context does not expose certificate material interface');
    Exit;
  end;
  if (not LContextMaterial.HasCertificateMaterial) or (not LContextMaterial.HasPrivateKeyMaterial) then
  begin
    SetHandshakeError(sslErrInvalidParam, 'TLS1.2 server context requires certificate and private key material');
    Exit;
  end;
  LCertificateBlob := LContextMaterial.GetCertificateMaterial;
  LPrivateKeyBlob := LContextMaterial.GetPrivateKeyMaterial;

  SetLength(LHandshakeBuffer, 0);
  SetLength(LClientHelloHandshake, 0);
  for LRecordIndex := 1 to 8 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
      SetHandshakeError(sslErrIO, 'Failed to receive TLS1.2 record while waiting for ClientHello');
      Exit;
    end;
    case LHeader.ContentType of
      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        Continue;
      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          AppendHandshakeBytes(LHandshakeBuffer, LPayloadBytes);
          while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
          begin
            if LHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_CLIENT_HELLO then
            begin
              SetHandshakeError(sslErrProtocol, 'Expected TLS1.2 ClientHello');
              Exit;
            end;
            if not TryParseTLS12ClientHelloFromHandshake(LHandshakeMessage, LClientHello, LParseError) then
            begin
              SetHandshakeError(sslErrProtocol, 'Invalid TLS1.2 ClientHello: ' + LParseError);
              Exit;
            end;
            LClientHelloHandshake := LHandshakeMessage;
            Break;
          end;
          if Length(LClientHelloHandshake) > 0 then
            Break;
        end;
    else
      begin
        SetHandshakeError(sslErrProtocol, 'Unexpected TLS record before TLS1.2 ClientHello');
        Exit;
      end;
    end;
  end;

  if Length(LClientHelloHandshake) = 0 then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS1.2 ClientHello not received');
    Exit;
  end;

  LSelectedCipherSuite := 0;
  if TLS12ClientHelloOffersCipherSuite(LClientHello, TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256) then
    LSelectedCipherSuite := TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256
  else if TLS12ClientHelloOffersCipherSuite(LClientHello, TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256) then
    LSelectedCipherSuite := TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256
  else if TLS12ClientHelloOffersCipherSuite(LClientHello, TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384) then
    LSelectedCipherSuite := TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384;
  if LSelectedCipherSuite = 0 then
  begin
    SetHandshakeError(sslErrUnsupported, 'No supported TLS1.2 cipher suite intersection for pure FreePascal server path');
    Exit;
  end;

  if not SelectTLS12ServerSignatureScheme(LClientHello, LSignatureScheme) then
  begin
    SetHandshakeError(sslErrUnsupported, 'No supported TLS1.2 server signature scheme intersection');
    Exit;
  end;

  if TLS12ClientHelloOffersNamedGroup(LClientHello, TLS_GROUP_X25519) then
  begin
    LNamedCurve := TLS_GROUP_X25519;
    FX25519PrivateKey := GenerateX25519PrivateKey;
    LServerPublicKey := X25519PublicKeyFromPrivate(FX25519PrivateKey);
  end
  else
  begin
    SetHandshakeError(sslErrUnsupported, 'TLS1.2 server currently supports only X25519 key exchange');
    Exit;
  end;

  if not TryExtractLeafCertificateDERFromBlob(LCertificateBlob, LLeafCertificateDER, LError) then
  begin
    SetHandshakeError(sslErrInvalidParam, 'Failed to extract leaf certificate DER: ' + LError);
    Exit;
  end;

  LLeafCertificate := TX509Certificate.Create;
  try
    LLeafCertificate.LoadFromDER(LLeafCertificateDER);
    if not SameText(LLeafCertificate.PublicKeyInfo.KeyType, 'RSA') then
    begin
      SetHandshakeError(sslErrUnsupported, 'TLS1.2 server minimum slice currently supports only RSA leaf certificates');
      Exit;
    end;
  finally
    LLeafCertificate.Free;
  end;

  if not BuildTLS12CertificateHandshakeFromBlob(LCertificateBlob, LCertificateHandshake, LError) then
  begin
    SetHandshakeError(sslErrInvalidParam, 'Failed to build TLS1.2 Certificate handshake: ' + LError);
    Exit;
  end;

  LServerRandom := GenerateSecureRandomBytes(TLS12_RANDOM_SIZE);
  LSessionID := GenerateSecureRandomBytes(32);
  LSelectedALPNProtocol := SelectALPNProtocol(LClientHello.ALPNProtocols, FALPNProtocols);
  FSelectedALPNProtocol := LSelectedALPNProtocol;
  LServerHelloHandshake := BuildTLS12ServerHelloHandshake(
    LServerRandom,
    LSessionID,
    LSelectedCipherSuite,
    LSelectedALPNProtocol
  );

  LSignedParams := nil;
  AppendBytes(LSignedParams, LClientHello.Random);
  AppendBytes(LSignedParams, LServerRandom);
  AppendBytes(LSignedParams, BuildTLS12ServerECDHParamsBytes(Default(TTLS12ServerKeyExchangeInfo)));
  SetLength(LSignedParams, Length(LClientHello.Random) + Length(LServerRandom));
  AppendByte(LSignedParams, 3);
  AppendUInt16(LSignedParams, LNamedCurve);
  AppendByte(LSignedParams, Byte(Length(LServerPublicKey)));
  AppendBytes(LSignedParams, LServerPublicKey);

  if not TryBuildTLS13CertificateVerifySignature(
    LSignatureScheme,
    LPrivateKeyBlob,
    LSignedParams,
    LSignature,
    LError
  ) then
  begin
    SetHandshakeError(sslErrUnsupported, 'TLS1.2 ServerKeyExchange signer failed: ' + LError);
    Exit;
  end;

  LServerKeyExchangeHandshake := BuildTLS12ServerKeyExchangeHandshake(
    LNamedCurve,
    LServerPublicKey,
    LSignatureScheme,
    LSignature
  );
  LServerHelloDoneHandshake := BuildTLS12ServerHelloDoneHandshake;

  LServerFlightPayload := nil;
  AppendHandshakeBytes(LServerFlightPayload, LServerHelloHandshake);
  AppendHandshakeBytes(LServerFlightPayload, LCertificateHandshake);
  AppendHandshakeBytes(LServerFlightPayload, LServerKeyExchangeHandshake);
  AppendHandshakeBytes(LServerFlightPayload, LServerHelloDoneHandshake);

  LServerFlightRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LServerFlightPayload);
  if not SendAll(LServerFlightRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send TLS1.2 server flight');
    Exit;
  end;

  LTranscriptData := nil;
  AppendHandshakeBytes(LTranscriptData, LClientHelloHandshake);
  AppendHandshakeBytes(LTranscriptData, LServerHelloHandshake);
  AppendHandshakeBytes(LTranscriptData, LCertificateHandshake);
  AppendHandshakeBytes(LTranscriptData, LServerKeyExchangeHandshake);
  AppendHandshakeBytes(LTranscriptData, LServerHelloDoneHandshake);

  SetLength(LHandshakeBuffer, 0);
  LClientKeyExchangeSeen := False;
  LClientCCSSeen := False;
  LClientFinishedSeen := False;
  for LRecordIndex := 1 to 16 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
      SetHandshakeError(sslErrIO, 'Failed to receive TLS1.2 client flight');
      Exit;
    end;

    case LHeader.ContentType of
      TLS_CONTENT_TYPE_HANDSHAKE:
        begin
          AppendHandshakeBytes(LHandshakeBuffer, LPayloadBytes);
          while TryPopHandshakeMessage(LHandshakeBuffer, LHandshakeMessage) do
          begin
            if not TryParseTLS12ClientKeyExchangeFromHandshake(LHandshakeMessage, LClientKeyExchangePublicKey, LError) then
            begin
              SetHandshakeError(sslErrUnsupported, 'Unsupported TLS1.2 client flight message: ' + LError);
              Exit;
            end;
            LClientKeyExchangeSeen := True;
            AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
          end;
        end;

      TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC:
        begin
          if (Length(LPayloadBytes) <> 1) or (LPayloadBytes[0] <> 1) then
          begin
            SetHandshakeError(sslErrProtocol, 'Invalid TLS1.2 client ChangeCipherSpec');
            Exit;
          end;
          LClientCCSSeen := True;
        end;

      TLS_CONTENT_TYPE_ALERT:
        begin
          SetHandshakeError(sslErrHandshake, 'Peer returned TLS alert during TLS1.2 server accept');
          Exit;
        end;
    else
      begin
        if not LClientCCSSeen then
        begin
          SetHandshakeError(sslErrProtocol, 'Unexpected TLS1.2 record before client ChangeCipherSpec');
          Exit;
        end;
      end;
    end;

    if LClientKeyExchangeSeen and LClientCCSSeen then
      Break;
  end;

  if not (LClientKeyExchangeSeen and LClientCCSSeen) then
  begin
    SetHandshakeError(sslErrProtocol, 'Incomplete TLS1.2 client flight');
    Exit;
  end;

  LPreMasterSecret := X25519ComputeSharedSecret(FX25519PrivateKey, LClientKeyExchangePublicKey);
  DeriveTLS12TrafficKeys;
  if FLastErrorCode <> sslErrNone then
    Exit;

  if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
  begin
    if FLastErrorCode = sslErrTimeout then
      Exit;
    SetHandshakeError(sslErrIO, 'Failed to receive TLS1.2 client Finished');
    Exit;
  end;
  if LHeader.ContentType <> TLS_CONTENT_TYPE_HANDSHAKE then
  begin
    SetHandshakeError(sslErrProtocol, 'Expected encrypted TLS1.2 client Finished record');
    Exit;
  end;

  if TLS12CipherSuiteIsChaCha(LSelectedCipherSuite) then
  begin
    if not TryDecryptTLS12ChaCha20Poly1305Record(
      FTLS12ClientWriteKey,
      FTLS12ClientWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LPayloadBytes,
      LClientFinishedPlaintext,
      LError
    ) then
    begin
      SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS1.2 client Finished: ' + LError);
      Exit;
    end;
  end
  else if TLS12CipherSuiteIsAES128GCM(LSelectedCipherSuite) and not TryDecryptTLS12AES128GCMRecord(
    FTLS12ClientWriteKey,
    FTLS12ClientWriteIV,
    0,
    TLS_CONTENT_TYPE_HANDSHAKE,
    TLS12_VERSION,
    LPayloadBytes,
    LClientFinishedPlaintext,
    LError
  ) then
  begin
    SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS1.2 client Finished: ' + LError);
    Exit;
  end
  else if TLS12CipherSuiteIsAES256GCM(LSelectedCipherSuite) and not TryDecryptTLS12AES256GCMRecord(
    FTLS12ClientWriteKey,
    FTLS12ClientWriteIV,
    0,
    TLS_CONTENT_TYPE_HANDSHAKE,
    TLS12_VERSION,
    LPayloadBytes,
    LClientFinishedPlaintext,
    LError
  ) then
  begin
    SetHandshakeError(sslErrDecryptionFailed, 'Failed to decrypt TLS1.2 client Finished: ' + LError);
    Exit;
  end;

  if not TryPopHandshakeMessage(LClientFinishedPlaintext, LHandshakeMessage) then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS1.2 client Finished was not parsed');
    Exit;
  end;
  if TLS12CipherSuiteUsesSHA384(LSelectedCipherSuite) then
    LClientFinishedExpected := TLS12ComputeClientFinishedVerifyData_SHA384(LMasterSecret, LTranscriptData)
  else
    LClientFinishedExpected := TLS12ComputeClientFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
  if (LHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_FINISHED) or
    (not ConstantTimeBytesEqual(Copy(LHandshakeMessage, 4, TLS12_FINISHED_VERIFY_DATA_LENGTH), LClientFinishedExpected)) then
  begin
    SetHandshakeError(sslErrHandshake, 'TLS1.2 client Finished verify_data mismatch');
    Exit;
  end;
  AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
  LClientFinishedSeen := True;

  LCCSRecord := BuildTLS12ChangeCipherSpecRecord;
  if not SendAll(LCCSRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send TLS1.2 server ChangeCipherSpec');
    Exit;
  end;
  if TLS12CipherSuiteUsesSHA384(LSelectedCipherSuite) then
    LServerFinishedVerifyData := TLS12ComputeServerFinishedVerifyData_SHA384(LMasterSecret, LTranscriptData)
  else
    LServerFinishedVerifyData := TLS12ComputeServerFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
  LServerFinishedHandshake := BuildTLS12FinishedHandshake(LServerFinishedVerifyData);
  if TLS12CipherSuiteIsChaCha(LSelectedCipherSuite) then
  begin
    if not TryEncryptTLS12ChaCha20Poly1305Record(
      FTLS12ServerWriteKey,
      FTLS12ServerWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LServerFinishedHandshake,
      LEncryptedFinished,
      LError
    ) then
    begin
      SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS1.2 server Finished: ' + LError);
      Exit;
    end;
  end
  else if TLS12CipherSuiteIsAES128GCM(LSelectedCipherSuite) and not TryEncryptTLS12AES128GCMRecord(
    FTLS12ServerWriteKey,
    FTLS12ServerWriteIV,
    0,
    TLS_CONTENT_TYPE_HANDSHAKE,
    TLS12_VERSION,
    LServerFinishedHandshake,
    LEncryptedFinished,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS1.2 server Finished: ' + LError);
    Exit;
  end
  else if TLS12CipherSuiteIsAES256GCM(LSelectedCipherSuite) and not TryEncryptTLS12AES256GCMRecord(
    FTLS12ServerWriteKey,
    FTLS12ServerWriteIV,
    0,
    TLS_CONTENT_TYPE_HANDSHAKE,
    TLS12_VERSION,
    LServerFinishedHandshake,
    LEncryptedFinished,
    LError
  ) then
  begin
    SetHandshakeError(sslErrEncryptionFailed, 'Failed to encrypt TLS1.2 server Finished: ' + LError);
    Exit;
  end;
  LEncryptedFinishedRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LEncryptedFinished);
  if not SendAll(LEncryptedFinishedRecord) then
  begin
    SetHandshakeError(sslErrIO, 'Failed to send TLS1.2 server Finished');
    Exit;
  end;

  FTLS12CipherSuite := LSelectedCipherSuite;
  FTLS12SessionID := Copy(LSessionID, 0, Length(LSessionID));
  FTLS12MasterSecret := Copy(LMasterSecret, 0, Length(LMasterSecret));
  FTLS12ClientSequence := 1;
  FTLS12ServerSequence := 1;
  FProtocolVersion := sslProtocolTLS12;
  FCipherName := TLS12CipherSuiteToString(LSelectedCipherSuite);
  ClearOperationalErrorState;
  Result := LClientFinishedSeen;
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

procedure TFreePascalConnection.NotifyInfoCallback(AWhere: Integer; ARet: Integer; const AState: string);
var
  LTrustContext: IFreePascalContextTrustStore;
  LCallback: TSSLInfoCallback;
begin
  if (FContext = nil) or
    (not Supports(FContext, IFreePascalContextTrustStore, LTrustContext)) then
    Exit;

  LCallback := LTrustContext.GetInfoCallback;
  if Assigned(LCallback) then
    LCallback(AWhere, ARet, AState);
end;

procedure TFreePascalConnection.NotifyHandshakeFailureInfoState;
begin
  case FLastErrorCode of
    sslErrTimeout:
      NotifyInfoCallback(2, Ord(FLastErrorCode), 'timeout');
    sslErrVerificationFailed,
    sslErrCertificate,
    sslErrCertificateExpired,
    sslErrCertificateRevoked,
    sslErrCertificateUnknown,
    sslErrCertificateUntrusted:
      NotifyInfoCallback(2, Ord(FLastErrorCode), 'verify_failed');
  else
    NotifyInfoCallback(2, Ord(FLastErrorCode), 'handshake_failed');
  end;
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
    case FProtocolVersion of
      sslProtocolTLS13:
        begin
          if not RecvApplicationDataFragment(LFragment) then
            Exit(-1);
        end;
      sslProtocolTLS12:
        begin
          if not RecvTLS12ApplicationDataFragment(LFragment) then
            Exit(-1);
        end;
    else
      begin
        MarkUnsupported('TLS application data path for selected protocol');
        Exit(-1);
      end;
    end;

    if (Length(LFragment) = 0) and (FReceivedCloseNotify or FTransportEOF) then
      Exit(0);

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

  case FProtocolVersion of
    sslProtocolTLS13:
      begin
        if not SendApplicationDataFragment(LFragment) then
          Exit(-1);
      end;
    sslProtocolTLS12:
      begin
        if not SendTLS12ApplicationDataFragment(LFragment) then
          Exit(-1);
      end;
  else
    begin
      MarkUnsupported('TLS application data path for selected protocol');
      Exit(-1);
    end;
  end;

  Result := ACount;
end;

function TFreePascalConnection.DoConnect: Boolean;
begin
  Result := False;
  NotifyInfoCallback(1, 0, 'handshake_start');

  if (FStream = nil) and (FSocket < 0) then
  begin
    FLastErrorCode := sslErrInvalidParam;
    FLastErrorString := 'No transport available for TLS connection';
    RecordError(FLastErrorCode, FLastErrorString);
    NotifyHandshakeFailureInfoState;
    Exit;
  end;

  case FProtocolVersion of
    sslProtocolTLS13:
      begin
        if not ProbeServerHello then
        begin
          if FLastErrorCode = sslErrNone then
            MarkUnsupported('TLS 1.3 ServerHello negotiation');
          NotifyHandshakeFailureInfoState;
          Exit;
        end;
      end;

    sslProtocolTLS12:
      begin
        if not ConnectTLS12Client then
        begin
          if FLastErrorCode = sslErrNone then
            MarkUnsupported('TLS 1.2 client handshake');
          NotifyHandshakeFailureInfoState;
          Exit;
        end;
      end;

  else
    begin
      MarkUnsupported('TLS handshake path for selected protocol');
      NotifyHandshakeFailureInfoState;
      Exit;
    end;
  end;

  NotifyInfoCallback(3, 0, 'handshake_done');
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
  LEncryptedExtensionsBody: TBytes;
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
  LCertificateBlob: TBytes;
  LPrivateKeyBlob: TBytes;
  LLeafCertificateDER: TBytes;
  LCertificateMessage: TBytes;
  LCertificateVerifyMessage: TBytes;
  LSignatureScheme: Word;
  LSignatureSchemeError: string;
  LLeafCertificate: TX509Certificate;
  LLeafKeyType: string;
  LCertVerifyInput: TBytes;
  LCertVerifySignature: TBytes;
  LSignatureLength: Integer;
  LSelectedALPNProtocol: string;
  LEncryptedExtensions: TBytes;
  LResumptionSession: IFreePascalResumptionSession;
  LResumptionSelected: Boolean;
  LExpectedBinder: TBytes;
  LZeroedClientHello: TBytes;
  LApplicationTranscriptData: TBytes;

  function SelectALPNProtocol(const AClientProtocols, AServerProtocols: string): string;
  var
    LClientList, LServerList: TStringArray;
    I, J: Integer;
    LClientProtocol: string;
  begin
    Result := '';
    if (AClientProtocols = '') or (AServerProtocols = '') then
      Exit;

    LClientList := AClientProtocols.Split([',']);
    LServerList := AServerProtocols.Split([',']);
    for I := 0 to High(LClientList) do
    begin
      LClientProtocol := Trim(LClientList[I]);
      if LClientProtocol = '' then
        Continue;

      for J := 0 to High(LServerList) do
        if SameText(LClientProtocol, Trim(LServerList[J])) then
          Exit(LClientProtocol);
    end;
  end;

  function BuildEncryptedExtensionsALPN(const AProtocol: string): TBytes;
  var
    LProtocolBytes: TBytes;
    LExtData: TBytes;
    I: Integer;
  begin
    Result := nil;
    if AProtocol = '' then
      Exit;

    SetLength(LProtocolBytes, Length(AProtocol));
    for I := 1 to Length(AProtocol) do
      LProtocolBytes[I - 1] := Byte(Ord(AProtocol[I]) and $FF);
    SetLength(LExtData, 0);
    AppendUInt16(LExtData, Word(Length(LProtocolBytes) + 1));
    AppendByte(LExtData, Byte(Length(LProtocolBytes)));
    AppendHandshakeBytes(LExtData, LProtocolBytes);

    SetLength(Result, 0);
    AppendUInt16(Result, TLS_EXTENSION_ALPN);
    AppendUInt16(Result, Word(Length(LExtData)));
    AppendHandshakeBytes(Result, LExtData);
  end;
begin
  Result := False;
  NotifyInfoCallback(1, 0, 'handshake_start');
  FSelectedALPNProtocol := '';
  SetLength(FHandshakeSharedSecret, 0);
  SetLength(FPeerCertificates, 0);
  FSessionReused := False;
  ClearTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  ClearTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FTransportReadBuffer, 0);
  ClearPendingWriteState;
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  SetLength(FResumptionTranscriptHash, 0);
  ClearReadClosureState;
  FIsServerMode := False;
  SetLength(FPeerCertificates, 0);
  LLeafKeyType := '';
  LResumptionSelected := False;
  SetLength(LApplicationTranscriptData, 0);

  if FProtocolVersion <> sslProtocolTLS13 then
  begin
    MarkUnsupported('TLS 1.3-only accept path (set PreferredVersion=TLS13)');
    NotifyInfoCallback(2, Ord(FLastErrorCode), 'handshake_failed');
    Exit;
  end;

  SetLength(LHandshakeBuffer, 0);
  SetLength(LClientHelloHandshake, 0);
  for LRecordIndex := 1 to 8 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
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

  LSelectedCipherSuite := 0;
  if (FConfiguredSession <> nil) and
    Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession) and
    LResumptionSession.HasResumptionMaterial and
    LClientHello.HasPreSharedKey and
    ConstantTimeBytesEqual(LClientHello.PSKIdentity, LResumptionSession.GetTicket.Ticket) and
    TLS13ClientHelloOffersCipherSuite(LClientHello, LResumptionSession.GetCipherSuite) then
  begin
    LZeroedClientHello := BuildBinderTranscript(
      LClientHelloHandshake,
      LClientHello.PSKBindersOffset
    );
    if TryComputeResumptionBinder(
      LResumptionSession.GetCipherSuite,
      LResumptionSession.GetResumptionPSK,
      LZeroedClientHello,
      LExpectedBinder,
      LError
    ) and ConstantTimeBytesEqual(LExpectedBinder, LClientHello.PSKBinder) then
    begin
      LSelectedCipherSuite := LResumptionSession.GetCipherSuite;
      LResumptionSelected := True;
      FSessionReused := True;
    end;
  end;

  if LSelectedCipherSuite = 0 then
  begin
    if not TrySelectServerCipherSuite(FContext, LClientHello, LSelectedCipherSuite) then
      LSelectedCipherSuite := 0;
  end;

  if LSelectedCipherSuite = 0 then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      'No supported TLS 1.3 cipher suite intersection for current pure FreePascal path'
    );
    Exit;
  end;

  FProtocolVersion := sslProtocolTLS13;
  FCipherName := TLS13CipherSuiteToString(LSelectedCipherSuite);
  LSelectedALPNProtocol := SelectALPNProtocol(LClientHello.ALPNProtocols, FALPNProtocols);
  FSelectedALPNProtocol := LSelectedALPNProtocol;

  if not LResumptionSelected and not Supports(FContext, IFreePascalContextMaterial, LContextMaterial) then
  begin
    SetHandshakeError(sslErrUnsupported, 'FreePascal context does not expose certificate material interface');
    NotifyInfoCallback(2, Ord(FLastErrorCode), 'handshake_failed');
    Exit;
  end;

  if (not LResumptionSelected) and (not LContextMaterial.HasCertificateMaterial) then
  begin
    SetHandshakeError(sslErrInvalidParam, 'Server context requires certificate material (LoadCertificate)');
    Exit;
  end;

  if (not LResumptionSelected) and (not LContextMaterial.HasPrivateKeyMaterial) then
  begin
    SetHandshakeError(sslErrInvalidParam, 'Server context requires private key material (LoadPrivateKey)');
    Exit;
  end;

  if not LResumptionSelected then
  begin
    LCertificateBlob := LContextMaterial.GetCertificateMaterial;
    LPrivateKeyBlob := LContextMaterial.GetPrivateKeyMaterial;

    if not TryBuildTLS13ServerCertificateHandshake(LCertificateBlob, LCertificateMessage, LError) then
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
    if LResumptionSelected then
      LServerHelloHandshake := BuildTLS13ServerHelloHandshake(
        LClientHello.LegacySessionID,
        LSelectedCipherSuite,
        FX25519PublicKey,
        TLS13_GROUP_X25519,
        0
      )
    else
      LServerHelloHandshake := BuildTLS13ServerHelloHandshake(
        LClientHello.LegacySessionID,
        LSelectedCipherSuite,
        FX25519PublicKey,
        TLS13_GROUP_X25519,
        -1
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

  if LResumptionSelected then
  begin
    if not TryDeriveTLS13HandshakeSecretsWithPSK(
      LSelectedCipherSuite,
      LResumptionSession.GetResumptionPSK,
      FHandshakeSharedSecret,
      LTranscriptData,
      FHandshakeSecrets,
      LKeyScheduleError
    ) then
    begin
      SetHandshakeError(sslErrUnsupported, 'TLS 1.3 server PSK key schedule derivation failed: ' + LKeyScheduleError);
      Exit;
    end;
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

  SetLength(LEncryptedExtensions, 0);
  if LSelectedALPNProtocol <> '' then
    AppendHandshakeBytes(LEncryptedExtensions, BuildEncryptedExtensionsALPN(LSelectedALPNProtocol));

  SetLength(LEncryptedExtensionsBody, 0);
  AppendUInt16(LEncryptedExtensionsBody, Word(Length(LEncryptedExtensions)));
  AppendHandshakeBytes(LEncryptedExtensionsBody, LEncryptedExtensions);

  SetLength(LEncryptedExtensionsMessage, 0);
  AppendByte(LEncryptedExtensionsMessage, TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS);
  AppendUInt24(LEncryptedExtensionsMessage, Length(LEncryptedExtensionsBody));
  AppendBytes(LEncryptedExtensionsMessage, LEncryptedExtensionsBody);

  SetLength(LServerFlightMessages, 0);
  AppendHandshakeBytes(LServerFlightMessages, LEncryptedExtensionsMessage);
  AppendHandshakeBytes(LTranscriptData, LEncryptedExtensionsMessage);

  if not LResumptionSelected then
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

    if not TrySelectTLS13ServerCertificateVerifySchemeForKeyType(
      LClientHello,
      LLeafKeyType,
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

    LTranscriptHash := ComputeTranscriptHashForCipherSuite(LSelectedCipherSuite, LTranscriptData);
    if TLS13CipherSuiteIsSHA384(LSelectedCipherSuite) then
      LCertVerifyInput := BuildTLS13ServerCertificateVerifyInputSHA384(LTranscriptHash)
    else
      LCertVerifyInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

    case LSignatureScheme of
      TLS13_SIG_RSA_PSS_RSAE_SHA256,
      TLS13_SIG_RSA_PSS_PSS_SHA256,
      TLS13_SIG_RSA_PKCS1_SHA256,
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

  LTranscriptHash := ComputeTranscriptHashForCipherSuite(LSelectedCipherSuite, LTranscriptData);
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

  SetLength(LHandshakeBuffer, 0);
  LClientFinishedReceived := False;

  for LRecordIndex := 1 to 64 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
      if FLastErrorCode = sslErrTimeout then
        Exit;
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
                  if LMsgType = TLS_HANDSHAKE_TYPE_FINISHED then
                  begin
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

                    LTranscriptHash := ComputeTranscriptHashForCipherSuite(LSelectedCipherSuite, LTranscriptData);
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

                    LApplicationTranscriptData := Copy(LTranscriptData, 0, Length(LTranscriptData));
                    AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
                    FResumptionTranscriptHash := ComputeTranscriptHashForCipherSuite(
                      LSelectedCipherSuite,
                      LTranscriptData
                    );
                    LClientFinishedReceived := True;
                    Break;
                  end
                  else
                  begin
                    AppendHandshakeBytes(LTranscriptData, LHandshakeMessage);
                  end;
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
    NotifyInfoCallback(2, Ord(FLastErrorCode), 'handshake_failed');
    Exit;
  end;

  if not TryDeriveTLS13ApplicationSecrets(
    LSelectedCipherSuite,
    FHandshakeSecrets.HandshakeSecret,
    LApplicationTranscriptData,
    FApplicationSecrets,
    LKeyScheduleError
  ) then
  begin
    SetHandshakeError(sslErrUnsupported, 'TLS 1.3 application key schedule derivation failed: ' + LKeyScheduleError);
    Exit;
  end;

  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  ClearPendingWriteState;
  SetLength(FPostHandshakeBuffer, 0);

  FProtocolVersion := sslProtocolTLS13;
  FCipherName := TLS13CipherSuiteToString(LSelectedCipherSuite);
  FIsServerMode := True;

  SendInitialSessionTicket;

  NotifyInfoCallback(3, 0, 'handshake_done');
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
  case FProtocolVersion of
    sslProtocolTLS13:
      Result := SendTLS13AlertRecord(1, 0);
    sslProtocolTLS12:
      Result := SendTLS12AlertRecord(1, 0);
  else
    begin
      MarkUnsupported('TLS shutdown path for selected protocol');
      Result := False;
    end;
  end;
end;

procedure TFreePascalConnection.DoClose;
begin
  FTLS12CipherSuite := 0;
  SetLength(FTLS12SessionID, 0);
  SetLength(FTLS12MasterSecret, 0);
  SetLength(FTLS12SessionTicket, 0);
  FTLS12SessionTicketLifetimeHint := 0;
  SetLength(FTLS12ClientWriteKey, 0);
  SetLength(FTLS12ServerWriteKey, 0);
  SetLength(FTLS12ClientWriteIV, 0);
  SetLength(FTLS12ServerWriteIV, 0);
  FTLS12ClientSequence := 0;
  FTLS12ServerSequence := 0;
  SetLength(FX25519PrivateKey, 0);
  SetLength(FX25519PublicKey, 0);
  SetLength(FHandshakeSharedSecret, 0);
  ClearTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
  FServerHandshakeSeq := 0;
  FClientHandshakeSeq := 0;

  ClearTLS13ApplicationSecrets(FApplicationSecrets);
  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FTransportReadBuffer, 0);
  ClearPendingWriteState;
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  SetLength(FResumptionTranscriptHash, 0);
  ClearReadClosureState;
  FIsServerMode := False;
  SetLength(FPeerCertificates, 0);
  FSessionReused := False;
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
  Result := FLastErrorCode = sslErrWantRead;
end;

function TFreePascalConnection.DoWantWrite: Boolean;
begin
  Result := FLastErrorCode = sslErrWantWrite;
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
  Result := nil;
  if Length(FPeerCertificates) = 0 then
  begin
    if FSessionReused and (FConfiguredSession <> nil) then
      Exit(FConfiguredSession.GetPeerCertificate);
    Exit;
  end;

  Result := TSSLFactory.CreateCertificate(sslFreePascal);
  if (Result <> nil) and (not Result.LoadFromDER(FPeerCertificates[0])) then
    Result := nil;
end;

function TFreePascalConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
var
  I: Integer;
  LCert: ISSLCertificate;
  LResumptionSession: IFreePascalResumptionSession;
begin
  Result := nil;
  if Length(FPeerCertificates) = 0 then
  begin
    if FSessionReused and (FConfiguredSession <> nil) and
      Supports(FConfiguredSession, IFreePascalResumptionSession, LResumptionSession) then
      Exit(LResumptionSession.GetPeerCertificateChain);
    if FSessionReused and (FConfiguredSession <> nil) and
      (FConfiguredSession.GetPeerCertificate <> nil) then
    begin
      SetLength(Result, 1);
      Result[0] := FConfiguredSession.GetPeerCertificate;
    end;
    Exit;
  end;

  SetLength(Result, Length(FPeerCertificates));
  for I := 0 to High(FPeerCertificates) do
  begin
    LCert := TSSLFactory.CreateCertificate(sslFreePascal);
    if (LCert <> nil) and LCert.LoadFromDER(FPeerCertificates[I]) then
      Result[I] := LCert
    else
      Result[I] := nil;
  end;
end;

function TFreePascalConnection.DoGetVerifyResult: Integer;
begin
  if FLastErrorCode = sslErrNone then
    Result := 0
  else
    Result := Ord(FLastErrorCode);
end;

function TFreePascalConnection.DoGetVerifyResultString: string;
var
  LVerifyMode: TSSLVerifyModes;
begin
  if FLastErrorString <> '' then
    Exit(FLastErrorString);

  if not FHandshakeComplete then
    Exit('Not verified');

  if FContext <> nil then
    LVerifyMode := FContext.GetVerifyMode
  else
    LVerifyMode := [];

  if sslVerifyPeer in LVerifyMode then
    Result := 'Verification passed'
  else
    Result := 'Verification disabled';
end;

function TFreePascalConnection.DoGetSession: ISSLSession;
var
  LPeerCert: ISSLCertificate;
  LPeerChain: TSSLCertificateArray;
  LTimeout: Integer;
  LCipherSuite: Word;
  LResumptionPSK: TBytes;
  LResumptionError: string;
begin
  Result := nil;
  if not FHandshakeComplete then
    Exit;

  LPeerCert := DoGetPeerCertificate;
  LPeerChain := DoGetPeerCertificateChain;
  LTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  if FContext <> nil then
    LTimeout := FContext.GetSessionTimeout;

  if (FProtocolVersion = sslProtocolTLS12) and
    (FTLS12CipherSuite <> 0) and
    (Length(FTLS12MasterSecret) = TLS12_MASTER_SECRET_LENGTH) and
    ((Length(FTLS12SessionID) > 0) or (Length(FTLS12SessionTicket) > 0)) then
  begin
    Result := TFreePascalSession.CreateTLS12ResumptionSnapshot(
      FProtocolVersion,
      FTLS12CipherSuite,
      FCipherName,
      LPeerCert,
      LPeerChain,
      FTLS12SessionID,
      FTLS12MasterSecret,
      FTLS12SessionTicket,
      FTLS12SessionTicketLifetimeHint,
      LTimeout
    );
    Exit;
  end;

  LCipherSuite := TLS13CipherSuiteFromName(FCipherName);
  if (FSessionTicketCount > 0) and (LCipherSuite <> 0) and
    TryDeriveResumptionPSK(
      LCipherSuite,
      FApplicationSecrets.MasterSecret,
      FResumptionTranscriptHash,
      FLastSessionTicket.TicketNonce,
      LResumptionPSK,
      LResumptionError
    ) then
  begin
    Result := TFreePascalSession.CreateResumptionSnapshot(
      FProtocolVersion,
      LCipherSuite,
      FCipherName,
      LPeerCert,
      LPeerChain,
      FLastSessionTicket,
      LResumptionPSK,
      LTimeout
    );
  end
  else
    Result := TFreePascalSession.CreateSnapshot(
      FProtocolVersion,
      FCipherName,
      LPeerCert,
      LPeerChain,
      FLastSessionTicket,
      FSessionTicketCount > 0,
      LTimeout
    );
end;

procedure TFreePascalConnection.DoSetSession(ASession: ISSLSession);
begin
  if ASession <> nil then
    FConfiguredSession := ASession.Clone
  else
    FConfiguredSession := nil;
  FSessionReused := False;
end;

function TFreePascalConnection.DoIsSessionReused: Boolean;
begin
  Result := FSessionReused;
end;

function TFreePascalConnection.DoGetSelectedALPNProtocol: string;
begin
  Result := FSelectedALPNProtocol;
end;

function TFreePascalConnection.DoGetState: string;
begin
  if FHandshakeComplete then
    Result := 'CONNECTED'
  else if (FLastErrorCode <> sslErrNone) and (FConnectTime > 0) then
    Result := 'HANDSHAKE_FAILED'
  else if FCipherName <> '' then
    Result := 'SERVER_HELLO_NEGOTIATED'
  else if FConnected then
    Result := 'CONNECTING'
  else
    Result := 'DISCONNECTED';
end;

function TFreePascalConnection.GetStateString: string;
begin
  if FHandshakeComplete then
    Result := 'Connected'
  else if (FLastErrorCode <> sslErrNone) and (FConnectTime > 0) then
    Result := 'Handshake failed'
  else if FConnected or (FCipherName <> '') then
    Result := 'Handshaking'
  else
    Result := 'Disconnected';
end;

function TFreePascalConnection.DoGetNativeHandle: Pointer;
begin
  Result := nil;
end;

function TFreePascalConnection.GetConnectionInfo: TSSLConnectionInfo;
var
  LPeerCert: ISSLCertificate;
  LSession: ISSLSession;
  LTLS12CipherSuiteId: Word;
begin
  Result := inherited GetConnectionInfo;
  Result.CipherSuiteId := TLS13CipherSuiteFromName(Result.CipherSuite);
  if Result.CipherSuiteId = 0 then
  begin
    LTLS12CipherSuiteId := TLS12CipherSuiteFromName(Result.CipherSuite);
    Result.CipherSuiteId := LTLS12CipherSuiteId;
  end;
  case Result.CipherSuiteId of
    TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256:
      begin
        Result.KeyExchange := sslKexECDHE_RSA;
        Result.Cipher := sslCipherAES128GCM;
        Result.Hash := sslHashSHA256;
        Result.KeySize := 16;
        Result.MacSize := 16;
      end;
    TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384:
      begin
        Result.KeyExchange := sslKexECDHE_RSA;
        Result.Cipher := sslCipherAES256GCM;
        Result.Hash := sslHashSHA384;
        Result.KeySize := 32;
        Result.MacSize := 16;
      end;
    TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256:
      begin
        Result.KeyExchange := sslKexECDHE_RSA;
        Result.Cipher := sslCipherCHACHA20_POLY1305;
        Result.Hash := sslHashSHA256;
        Result.KeySize := 32;
        Result.MacSize := 16;
      end;
    TLS13_CIPHER_AES_128_GCM_SHA256:
      begin
        Result.KeyExchange := sslKexECDHE_RSA;
        Result.Cipher := sslCipherAES128GCM;
        Result.Hash := sslHashSHA256;
        Result.KeySize := 16;
        Result.MacSize := 16;
      end;
    TLS13_CIPHER_AES_256_GCM_SHA384,
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256:
      begin
        Result.KeyExchange := sslKexECDHE_RSA;
        Result.KeySize := 32;
        Result.MacSize := 16;
        if Result.CipherSuiteId = TLS13_CIPHER_AES_256_GCM_SHA384 then
        begin
          Result.Cipher := sslCipherAES256GCM;
          Result.Hash := sslHashSHA384;
        end
        else
        begin
          Result.Cipher := sslCipherCHACHA20_POLY1305;
          Result.Hash := sslHashSHA256;
        end;
      end;
  end;

  LPeerCert := DoGetPeerCertificate;
  if LPeerCert <> nil then
    Result.PeerCertificate := LPeerCert.GetInfo;

  LSession := DoGetSession;
  if LSession <> nil then
    Result.SessionId := LSession.GetID;
end;

procedure TFreePascalConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TFreePascalConnection.GetServerName: string;
begin
  Result := FServerName;
end;

end.
