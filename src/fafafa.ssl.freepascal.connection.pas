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
 * - 派生应用流量密钥并实现应用数据记录收发（CHACHA20-POLY1305）
 *
 * 当前限制：
 * - TLS 1.3 AES-GCM 套件尚未实现（纯 Pascal）
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
  Sockets,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.connection.base,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.keyschedule,
  fafafa.ssl.tls13.appschedule,
  fafafa.ssl.tls13.posthandshake;

type
  TFreePascalConnection = class(TBaseSSLConnection, ISSLClientConnection)
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
    FHandshakeSecrets: TTLS13HandshakeSecrets;
    FServerFinishedKey: TBytes;
    FClientFinishedKey: TBytes;
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
    function RecvApplicationDataFragment(out AFragment: TBytes): Boolean;
    function SendApplicationDataFragment(const AFragment: TBytes): Boolean;
    function ProcessPostHandshakeFragment(const AHandshakeFragment: TBytes): Boolean;
    function SendPostHandshakeKeyUpdate(ARequestPeerUpdate: Boolean): Boolean;
    procedure MarkUnsupported(const AOperation: string);
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
    constructor Create(AContext: ISSLContext; ASocket: THandle); overload;
    constructor Create(AContext: ISSLContext; AStream: TStream); overload;

    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
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
  fafafa.ssl.tls13.servercertificate,
  fafafa.ssl.tls13.servercertverify,
  fafafa.ssl.freepascal.context.material,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.x509;

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

constructor TFreePascalConnection.Create(AContext: ISSLContext; ASocket: THandle);
begin
  inherited Create(AContext);
  FSocket := ASocket;
  FStream := nil;
  FServerName := '';
  FProtocolVersion := SelectPreferredProtocol(AContext);
  FCipherName := '';
  FALPNProtocols := AContext.GetALPNProtocols;
  FSelectedALPNProtocol := '';
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
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
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
  InitTLS13HandshakeSecrets(FHandshakeSecrets);
  SetLength(FServerFinishedKey, 0);
  SetLength(FClientFinishedKey, 0);
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

  if not RecvExact(APayload, AHeader.Length) then
    Exit;

  SetLength(ARecord, 5 + Length(APayload));
  Move(LHeaderBytes[0], ARecord[0], 5);
  if Length(APayload) > 0 then
    Move(APayload[0], ARecord[5], Length(APayload));

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
begin
  Result := False;
  SetLength(LHandshakeBuffer, 0);
  FServerHandshakeSeq := 0;

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

                    LTranscriptHash := SHA256(ATranscriptData);
                    if not TLS13VerifyFinishedSHA256(
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

  LTranscriptHash := SHA256(ATranscriptData);
  LVerifyData := TLS13ComputeFinishedVerifyDataSHA256(FClientFinishedKey, LTranscriptHash);

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

  if not FApplicationSecrets.Valid then
  begin
    SetHandshakeError(sslErrProtocol, 'TLS 1.3 application secrets are not ready');
    Exit;
  end;

  for LRecordIndex := 1 to 128 do
  begin
    if not RecvTLSRecord(LHeader, LPayloadBytes, LRecordBytes) then
    begin
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
begin
  Result := False;
  FSelectedALPNProtocol := '';
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
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
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

  LClientHelloHandshake := BuildTLS13ClientHelloHandshake(FServerName, FALPNProtocols, FX25519PublicKey);
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

          try
            FServerFinishedKey := TLS13FinishedKeySHA256(FHandshakeSecrets.ServerHandshakeTrafficSecret);
            FClientFinishedKey := TLS13FinishedKeySHA256(FHandshakeSecrets.ClientHandshakeTrafficSecret);
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
          SetLength(FPostHandshakeBuffer, 0);
          FSessionTicketCount := 0;
          InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;

          FProtocolVersion := sslProtocolTLS13;
          FCipherName := TLS13CipherSuiteToString(LServerHello.SelectedCipherSuite);
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
  FLastErrorString := Format('%s is not implemented in FreePascal backend yet', [AOperation]);
  RecordError(FLastErrorCode, FLastErrorString);
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
    MarkUnsupported('TLS read before completed handshake');
    Exit(-1);
  end;

  if ACount <= 0 then
    Exit(0);

  while Length(FApplicationReadBuffer) = 0 do
  begin
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
    MarkUnsupported('TLS write before completed handshake');
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
  LCertVerifyInput: TBytes;
  LCertVerifySignature: TBytes;
  LSignatureLength: Integer;
begin
  Result := False;
  FSelectedALPNProtocol := '';
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
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;

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
  if TLS13ClientHelloOffersCipherSuite(LClientHello, TLS13_CIPHER_CHACHA20_POLY1305_SHA256) then
    LSelectedCipherSuite := TLS13_CIPHER_CHACHA20_POLY1305_SHA256;

  if LSelectedCipherSuite = 0 then
  begin
    SetHandshakeError(
      sslErrUnsupported,
      'No supported TLS 1.3 cipher suite intersection (requires TLS_CHACHA20_POLY1305_SHA256 for pure FreePascal path)'
    );
    Exit;
  end;

  FProtocolVersion := sslProtocolTLS13;
  FCipherName := TLS13CipherSuiteToString(LSelectedCipherSuite);

  if not TrySelectTLS13ServerCertificateVerifyScheme(LClientHello, LSignatureScheme, LSignatureSchemeError) then
  begin
    SetHandshakeError(sslErrUnsupported, LSignatureSchemeError);
    Exit;
  end;

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

  if not TryDeriveTLS13HandshakeSecrets(
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
    FServerFinishedKey := TLS13FinishedKeySHA256(FHandshakeSecrets.ServerHandshakeTrafficSecret);
    FClientFinishedKey := TLS13FinishedKeySHA256(FHandshakeSecrets.ClientHandshakeTrafficSecret);
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

  SetLength(LEncryptedExtensionsBody, 0);
  AppendUInt16(LEncryptedExtensionsBody, 0);

  SetLength(LEncryptedExtensionsMessage, 0);
  AppendByte(LEncryptedExtensionsMessage, TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS);
  AppendUInt24(LEncryptedExtensionsMessage, Length(LEncryptedExtensionsBody));
  AppendBytes(LEncryptedExtensionsMessage, LEncryptedExtensionsBody);

  SetLength(LServerFlightMessages, 0);
  AppendHandshakeBytes(LServerFlightMessages, LEncryptedExtensionsMessage);
  AppendHandshakeBytes(LTranscriptData, LEncryptedExtensionsMessage);

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
      LSignatureLength := (LLeafCertificate.PublicKeyInfo.KeySize + 7) div 8;
      if LSignatureLength <= 0 then
        LSignatureLength := Length(LLeafCertificate.PublicKeyInfo.RSAModulus);
    end
    else if SameText(LLeafCertificate.PublicKeyInfo.KeyType, 'ECDSA') then
      LSignatureLength := 72
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

  if Length(LPrivateKeyBlob) = 0 then
  begin
    SetHandshakeError(sslErrInvalidParam, 'Server private key material is empty');
    Exit;
  end;

  LTranscriptHash := SHA256(LTranscriptData);
  LCertVerifyInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  case LSignatureScheme of
    TLS13_SIG_RSA_PSS_RSAE_SHA256,
    TLS13_SIG_RSA_PSS_PSS_SHA256,
    TLS13_SIG_RSA_PKCS1_SHA256:
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

        if Length(LCertVerifySignature) <> LSignatureLength then
        begin
          SetHandshakeError(
            sslErrHandshake,
            Format('CertificateVerify signature length mismatch (expected=%d actual=%d)',
              [LSignatureLength, Length(LCertVerifySignature)])
          );
          Exit;
        end;
      end;

    TLS13_SIG_ECDSA_SECP256R1_SHA256:
      begin
        SetHandshakeError(sslErrUnsupported, 'ECDSA CertificateVerify signer is not implemented yet in pure Pascal backend');
        Exit;
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

  LTranscriptHash := SHA256(LTranscriptData);
  LVerifyData := TLS13ComputeFinishedVerifyDataFromTrafficSecretSHA256(
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

                    LTranscriptHash := SHA256(LTranscriptData);
                    if not TLS13VerifyFinishedSHA256(
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
    Exit;
  end;

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

  FClientApplicationSeq := 0;
  FServerApplicationSeq := 0;
  SetLength(FApplicationReadBuffer, 0);
  SetLength(FPostHandshakeBuffer, 0);

  FProtocolVersion := sslProtocolTLS13;
  FCipherName := TLS13CipherSuiteToString(LSelectedCipherSuite);
  FIsServerMode := True;

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
  SetLength(FPostHandshakeBuffer, 0);
  FSessionTicketCount := 0;
  InitTLS13NewSessionTicket(FLastSessionTicket);
  FIsServerMode := False;
end;

function TFreePascalConnection.DoRenegotiate: Boolean;
begin
  if not FHandshakeComplete then
  begin
    SetHandshakeError(sslErrHandshake, 'Cannot send TLS 1.3 KeyUpdate before handshake completion');
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
  Result := nil;
end;

function TFreePascalConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
begin
  SetLength(Result, 0);
end;

function TFreePascalConnection.DoGetVerifyResult: Integer;
begin
  if FLastErrorCode = sslErrNone then
    Result := 0
  else
    Result := Ord(FLastErrorCode);
end;

function TFreePascalConnection.DoGetVerifyResultString: string;
begin
  if FLastErrorString = '' then
    Result := 'Not verified'
  else
    Result := FLastErrorString;
end;

function TFreePascalConnection.DoGetSession: ISSLSession;
begin
  Result := nil;
end;

procedure TFreePascalConnection.DoSetSession(ASession: ISSLSession);
begin
end;

function TFreePascalConnection.DoIsSessionReused: Boolean;
begin
  Result := False;
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

procedure TFreePascalConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TFreePascalConnection.GetServerName: string;
begin
  Result := FServerName;
end;

end.
