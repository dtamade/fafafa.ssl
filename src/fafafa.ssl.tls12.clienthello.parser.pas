{**
 * Unit: fafafa.ssl.tls12.clienthello.parser
 * Purpose: TLS 1.2 ClientHello parser foundation
 *}

unit fafafa.ssl.tls12.clienthello.parser;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.tls12.wire;

type
  TTLS12WordArray = array of Word;
  TTLS12ByteArray = array of Byte;

  TTLS12ClientHelloInfo = record
    Valid: Boolean;
    ClientVersion: Word;
    Random: TBytes;
    SessionID: TBytes;
    CipherSuites: TTLS12WordArray;
    CompressionMethods: TTLS12ByteArray;
    ServerName: string;
    ALPNProtocols: string;
    SupportedGroups: TTLS12WordArray;
    SignatureAlgorithms: TTLS12WordArray;
  end;

function TryParseTLS12ClientHelloFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS12ClientHelloInfo;
  out AError: string
): Boolean;

function TLS12ClientHelloOffersCipherSuite(
  const AClientHello: TTLS12ClientHelloInfo;
  ACipherSuite: Word
): Boolean;

function TLS12ClientHelloOffersNamedGroup(
  const AClientHello: TTLS12ClientHelloInfo;
  AGroup: Word
): Boolean;

function TLS12ClientHelloOffersSignatureScheme(
  const AClientHello: TTLS12ClientHelloInfo;
  ASignatureScheme: Word
): Boolean;

implementation

procedure InitClientHelloInfo(out AInfo: TTLS12ClientHelloInfo);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  SetLength(AInfo.Random, 0);
  SetLength(AInfo.SessionID, 0);
  SetLength(AInfo.CipherSuites, 0);
  SetLength(AInfo.CompressionMethods, 0);
  SetLength(AInfo.SupportedGroups, 0);
  SetLength(AInfo.SignatureAlgorithms, 0);
  AInfo.ServerName := '';
  AInfo.ALPNProtocols := '';
end;

procedure AppendWordValue(var AValues: TTLS12WordArray; AValue: Word);
var
  LLen: Integer;
begin
  LLen := Length(AValues);
  SetLength(AValues, LLen + 1);
  AValues[LLen] := AValue;
end;

procedure AppendByteValue(var AValues: TTLS12ByteArray; AValue: Byte);
var
  LLen: Integer;
begin
  LLen := Length(AValues);
  SetLength(AValues, LLen + 1);
  AValues[LLen] := AValue;
end;

function JoinALPN(const AExisting, AValue: string): string;
begin
  if AExisting = '' then
    Result := AValue
  else
    Result := AExisting + ',' + AValue;
end;

function TryParseTLS12ClientHelloFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS12ClientHelloInfo;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LOffset: Integer;
  LSessionLen: Integer;
  LCipherLen: Word;
  LCompressionLen: Integer;
  LExtTotalLen: Word;
  LExtEnd: Integer;
  LExtType, LExtLen: Word;
  LExtDataStart: Integer;
  LNameListLen, LNameLen: Word;
  LProtoListLen, LGroupListLen, LSigAlgListLen: Word;
  LProtoLen: Integer;
begin
  InitClientHelloInfo(AInfo);
  AError := '';
  Result := False;

  if Length(AHandshake) < 42 then
  begin
    AError := 'ClientHello handshake too short';
    Exit;
  end;
  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_CLIENT_HELLO then
  begin
    AError := 'Unexpected handshake type for ClientHello';
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) <> 4 + Integer(LBodyLen) then
  begin
    AError := 'ClientHello length mismatch';
    Exit;
  end;

  LOffset := 4;
  AInfo.ClientVersion := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  SetLength(AInfo.Random, 32);
  Move(AHandshake[LOffset], AInfo.Random[0], 32);
  Inc(LOffset, 32);

  LSessionLen := AHandshake[LOffset];
  Inc(LOffset);
  if LOffset + LSessionLen > Length(AHandshake) then
  begin
    AError := 'ClientHello session id exceeds body';
    Exit;
  end;
  SetLength(AInfo.SessionID, LSessionLen);
  if LSessionLen > 0 then
    Move(AHandshake[LOffset], AInfo.SessionID[0], LSessionLen);
  Inc(LOffset, LSessionLen);

  LCipherLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  if (LCipherLen = 0) or ((LCipherLen and 1) <> 0) or (LOffset + LCipherLen > Length(AHandshake)) then
  begin
    AError := 'ClientHello cipher suite vector is invalid';
    Exit;
  end;
  while LCipherLen > 0 do
  begin
    AppendWordValue(AInfo.CipherSuites, ReadUInt16(AHandshake, LOffset));
    Inc(LOffset, 2);
    Dec(LCipherLen, 2);
  end;

  LCompressionLen := AHandshake[LOffset];
  Inc(LOffset);
  if (LCompressionLen <= 0) or (LOffset + LCompressionLen > Length(AHandshake)) then
  begin
    AError := 'ClientHello compression methods vector is invalid';
    Exit;
  end;
  while LCompressionLen > 0 do
  begin
    AppendByteValue(AInfo.CompressionMethods, AHandshake[LOffset]);
    Inc(LOffset);
    Dec(LCompressionLen);
  end;

  if LOffset = Length(AHandshake) then
  begin
    AInfo.Valid := True;
    Result := True;
    Exit;
  end;

  LExtTotalLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  LExtEnd := LOffset + LExtTotalLen;
  if LExtEnd <> Length(AHandshake) then
  begin
    AError := 'ClientHello extensions length mismatch';
    Exit;
  end;

  while LOffset + 4 <= LExtEnd do
  begin
    LExtType := ReadUInt16(AHandshake, LOffset);
    LExtLen := ReadUInt16(AHandshake, LOffset + 2);
    Inc(LOffset, 4);
    if LOffset + LExtLen > LExtEnd then
    begin
      AError := 'ClientHello extension exceeds body';
      Exit;
    end;

    LExtDataStart := LOffset;
    case LExtType of
      TLS_EXTENSION_SERVER_NAME:
        begin
          if LExtLen < 5 then
          begin
            AError := 'ClientHello server_name extension too short';
            Exit;
          end;
          LNameListLen := ReadUInt16(AHandshake, LExtDataStart);
          if Integer(LNameListLen) <> Integer(LExtLen) - 2 then
          begin
            AError := 'ClientHello server_name list length mismatch';
            Exit;
          end;
          if AHandshake[LExtDataStart + 2] <> 0 then
          begin
            AError := 'ClientHello only host_name SNI is supported';
            Exit;
          end;
          LNameLen := ReadUInt16(AHandshake, LExtDataStart + 3);
          if (LNameLen = 0) or (LNameLen + 5 > LExtLen) then
          begin
            AError := 'ClientHello server_name host length mismatch';
            Exit;
          end;
          SetString(AInfo.ServerName, PChar(@AHandshake[LExtDataStart + 5]), LNameLen);
        end;

      TLS_EXTENSION_ALPN:
        begin
          if LExtLen < 3 then
          begin
            AError := 'ClientHello ALPN extension too short';
            Exit;
          end;
          LProtoListLen := ReadUInt16(AHandshake, LExtDataStart);
          if Integer(LProtoListLen) <> Integer(LExtLen) - 2 then
          begin
            AError := 'ClientHello ALPN list length mismatch';
            Exit;
          end;
          Inc(LExtDataStart, 2);
          while LProtoListLen > 0 do
          begin
            LProtoLen := AHandshake[LExtDataStart];
            Inc(LExtDataStart);
            Dec(LProtoListLen);
            if (LProtoLen <= 0) or (LProtoLen > LProtoListLen) then
            begin
              AError := 'ClientHello ALPN protocol length mismatch';
              Exit;
            end;
            AInfo.ALPNProtocols := JoinALPN(
              AInfo.ALPNProtocols,
              Copy(string(PAnsiChar(@AHandshake[LExtDataStart])), 1, LProtoLen)
            );
            Inc(LExtDataStart, LProtoLen);
            Dec(LProtoListLen, LProtoLen);
          end;
        end;

      TLS_EXTENSION_SUPPORTED_GROUPS:
        begin
          if LExtLen < 2 then
          begin
            AError := 'ClientHello supported_groups extension too short';
            Exit;
          end;
          LGroupListLen := ReadUInt16(AHandshake, LExtDataStart);
          Inc(LExtDataStart, 2);
          if (Integer(LGroupListLen) <> Integer(LExtLen) - 2) or ((LGroupListLen and 1) <> 0) then
          begin
            AError := 'ClientHello supported_groups length mismatch';
            Exit;
          end;
          while LGroupListLen > 0 do
          begin
            AppendWordValue(AInfo.SupportedGroups, ReadUInt16(AHandshake, LExtDataStart));
            Inc(LExtDataStart, 2);
            Dec(LGroupListLen, 2);
          end;
        end;

      TLS_EXTENSION_SIGNATURE_ALGORITHMS:
        begin
          if LExtLen < 2 then
          begin
            AError := 'ClientHello signature_algorithms extension too short';
            Exit;
          end;
          LSigAlgListLen := ReadUInt16(AHandshake, LExtDataStart);
          Inc(LExtDataStart, 2);
          if (Integer(LSigAlgListLen) <> Integer(LExtLen) - 2) or ((LSigAlgListLen and 1) <> 0) then
          begin
            AError := 'ClientHello signature_algorithms length mismatch';
            Exit;
          end;
          while LSigAlgListLen > 0 do
          begin
            AppendWordValue(AInfo.SignatureAlgorithms, ReadUInt16(AHandshake, LExtDataStart));
            Inc(LExtDataStart, 2);
            Dec(LSigAlgListLen, 2);
          end;
        end;
    end;

    Inc(LOffset, LExtLen);
  end;

  if LOffset <> LExtEnd then
  begin
    AError := 'ClientHello trailing extension bytes mismatch';
    Exit;
  end;

  AInfo.Valid := True;
  Result := True;
end;

function TLS12ClientHelloOffersCipherSuite(
  const AClientHello: TTLS12ClientHelloInfo;
  ACipherSuite: Word
): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AClientHello.CipherSuites) do
    if AClientHello.CipherSuites[I] = ACipherSuite then
      Exit(True);
end;

function TLS12ClientHelloOffersNamedGroup(
  const AClientHello: TTLS12ClientHelloInfo;
  AGroup: Word
): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AClientHello.SupportedGroups) do
    if AClientHello.SupportedGroups[I] = AGroup then
      Exit(True);
end;

function TLS12ClientHelloOffersSignatureScheme(
  const AClientHello: TTLS12ClientHelloInfo;
  ASignatureScheme: Word
): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AClientHello.SignatureAlgorithms) do
    if AClientHello.SignatureAlgorithms[I] = ASignatureScheme then
      Exit(True);
end;

end.
