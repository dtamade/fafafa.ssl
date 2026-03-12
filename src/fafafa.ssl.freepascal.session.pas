{**
 * Unit: fafafa.ssl.freepascal.session
 * Purpose: pure Pascal backend session snapshot implementation
 *}

unit fafafa.ssl.freepascal.session;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.tls13.posthandshake,
  fafafa.ssl.tls12.wire,
  fafafa.ssl.tls13.wire;

type
  IFreePascalResumptionSession = interface
    ['{F3A5E22F-4A1B-4D44-97AF-A1E26B0D7D7E}']
    function HasResumptionMaterial: Boolean;
    function HasTLS12ResumptionMaterial: Boolean;
    function HasTLS12TicketMaterial: Boolean;
    function GetCipherSuite: Word;
    function GetTicket: TTLS13NewSessionTicket;
    function GetResumptionPSK: TBytes;
    function GetTLS12SessionIDBytes: TBytes;
    function GetTLS12MasterSecret: TBytes;
    function GetTLS12SessionTicket: TBytes;
    function GetTLS12SessionTicketLifetimeHint: Cardinal;
    function GetPeerCertificateChain: TSSLCertificateArray;
  end;

  TFreePascalSession = class(TInterfacedObject, ISSLSession, IFreePascalResumptionSession)
  private
    FSessionID: string;
    FCreationTime: TDateTime;
    FTimeout: Integer;
    FProtocolVersion: TSSLProtocolVersion;
    FCipherSuite: Word;
    FCipherName: string;
    FPeerCertificate: ISSLCertificate;
    FPeerCertificateChain: TSSLCertificateArray;
    FHasTicket: Boolean;
    FTicket: TTLS13NewSessionTicket;
    FResumptionPSK: TBytes;
    FTLS12SessionIDBytes: TBytes;
    FTLS12MasterSecret: TBytes;
    FTLS12SessionTicket: TBytes;
    FTLS12SessionTicketLifetimeHint: Cardinal;

    class function GenerateSessionID: string; static;
    class function BytesToHexString(const AValue: TBytes): string; static;
    class procedure WriteStringValue(AStream: TStream; const AValue: string); static;
    class function ReadStringValue(AStream: TStream; out AValue: string): Boolean; static;
    class procedure WriteIntegerValue(AStream: TStream; AValue: Integer); static;
    class procedure WriteBytesValue(AStream: TStream; const AValue: TBytes); static;
    class function ReadBytesValue(AStream: TStream; out AValue: TBytes): Boolean; static;
    class function CloneCertificateArray(const AChain: TSSLCertificateArray): TSSLCertificateArray; static;
  public
    constructor Create; overload;
    constructor CreateSnapshot(
      AProtocolVersion: TSSLProtocolVersion;
      const ACipherName: string;
      APeerCertificate: ISSLCertificate;
      const APeerCertificateChain: TSSLCertificateArray;
      const ATicket: TTLS13NewSessionTicket;
      AHasTicket: Boolean;
      ATimeout: Integer
    ); overload;
    constructor CreateResumptionSnapshot(
      AProtocolVersion: TSSLProtocolVersion;
      ACipherSuite: Word;
      const ACipherName: string;
      APeerCertificate: ISSLCertificate;
      const APeerCertificateChain: TSSLCertificateArray;
      const ATicket: TTLS13NewSessionTicket;
      const AResumptionPSK: TBytes;
      ATimeout: Integer
    ); overload;
    constructor CreateTLS12ResumptionSnapshot(
      AProtocolVersion: TSSLProtocolVersion;
      ACipherSuite: Word;
      const ACipherName: string;
      APeerCertificate: ISSLCertificate;
      const APeerCertificateChain: TSSLCertificateArray;
      const ASessionIDBytes: TBytes;
      const AMasterSecret: TBytes;
      const ASessionTicket: TBytes;
      ATicketLifetimeHint: Cardinal;
      ATimeout: Integer
    ); overload;

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
    function Clone: ISSLSession;

    function HasResumptionMaterial: Boolean;
    function HasTLS12ResumptionMaterial: Boolean;
    function HasTLS12TicketMaterial: Boolean;
    function GetCipherSuite: Word;
    function GetTicket: TTLS13NewSessionTicket;
    function GetResumptionPSK: TBytes;
    function GetTLS12SessionIDBytes: TBytes;
    function GetTLS12MasterSecret: TBytes;
    function GetTLS12SessionTicket: TBytes;
    function GetTLS12SessionTicketLifetimeHint: Cardinal;
    function GetPeerCertificateChain: TSSLCertificateArray;
  end;

implementation

uses
  fafafa.ssl.factory;

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

const
  FREEPASCAL_SESSION_MAGIC = 'FPS1';

class function TFreePascalSession.GenerateSessionID: string;
var
  LGUID: TGUID;
begin
  CreateGUID(LGUID);
  Result := GUIDToString(LGUID);
end;

class function TFreePascalSession.BytesToHexString(const AValue: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AValue) do
    Result := Result + IntToHex(AValue[I], 2);
end;

class procedure TFreePascalSession.WriteStringValue(AStream: TStream; const AValue: string);
var
  LAnsi: AnsiString;
  LLen: Integer;
begin
  LAnsi := AnsiString(AValue);
  LLen := Length(LAnsi);
  AStream.WriteBuffer(LLen, SizeOf(LLen));
  if LLen > 0 then
    AStream.WriteBuffer(LAnsi[1], LLen);
end;

class function TFreePascalSession.ReadStringValue(AStream: TStream; out AValue: string): Boolean;
var
  LLen: Integer;
  LAnsi: AnsiString;
begin
  Result := False;
  AValue := '';
  if AStream.Read(LLen, SizeOf(LLen)) <> SizeOf(LLen) then
    Exit;
  if LLen < 0 then
    Exit;
  SetLength(LAnsi, LLen);
  if (LLen > 0) and (AStream.Read(LAnsi[1], LLen) <> LLen) then
    Exit;
  AValue := string(LAnsi);
  Result := True;
end;

class procedure TFreePascalSession.WriteIntegerValue(AStream: TStream; AValue: Integer);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

class procedure TFreePascalSession.WriteBytesValue(AStream: TStream; const AValue: TBytes);
var
  LLen: Integer;
begin
  LLen := Length(AValue);
  AStream.WriteBuffer(LLen, SizeOf(LLen));
  if LLen > 0 then
    AStream.WriteBuffer(AValue[0], LLen);
end;

class function TFreePascalSession.ReadBytesValue(AStream: TStream; out AValue: TBytes): Boolean;
var
  LLen: Integer;
begin
  Result := False;
  SetLength(AValue, 0);
  if AStream.Read(LLen, SizeOf(LLen)) <> SizeOf(LLen) then
    Exit;
  if LLen < 0 then
    Exit;
  SetLength(AValue, LLen);
  if (LLen > 0) and (AStream.Read(AValue[0], LLen) <> LLen) then
    Exit;
  Result := True;
end;

class function TFreePascalSession.CloneCertificateArray(
  const AChain: TSSLCertificateArray
): TSSLCertificateArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AChain));
  for I := 0 to High(AChain) do
    if AChain[I] <> nil then
      Result[I] := AChain[I].Clone
    else
      Result[I] := nil;
end;

constructor TFreePascalSession.Create;
begin
  inherited Create;
  FSessionID := GenerateSessionID;
  FCreationTime := Now;
  FTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FProtocolVersion := sslProtocolUnknown;
  FCipherSuite := 0;
  FCipherName := '';
  FPeerCertificate := nil;
  SetLength(FPeerCertificateChain, 0);
  FHasTicket := False;
  InitTLS13NewSessionTicket(FTicket);
  SetLength(FResumptionPSK, 0);
  SetLength(FTLS12SessionIDBytes, 0);
  SetLength(FTLS12MasterSecret, 0);
  SetLength(FTLS12SessionTicket, 0);
  FTLS12SessionTicketLifetimeHint := 0;
end;

constructor TFreePascalSession.CreateSnapshot(
  AProtocolVersion: TSSLProtocolVersion;
  const ACipherName: string;
  APeerCertificate: ISSLCertificate;
  const APeerCertificateChain: TSSLCertificateArray;
  const ATicket: TTLS13NewSessionTicket;
  AHasTicket: Boolean;
  ATimeout: Integer
);
begin
  Create;
  FProtocolVersion := AProtocolVersion;
  FCipherSuite := TLS13CipherSuiteFromName(ACipherName);
  if FCipherSuite = 0 then
    FCipherSuite := TLS12CipherSuiteFromName(ACipherName);
  FCipherName := ACipherName;
  if APeerCertificate <> nil then
    FPeerCertificate := APeerCertificate.Clone;
  FPeerCertificateChain := CloneCertificateArray(APeerCertificateChain);
  FHasTicket := AHasTicket and ATicket.Valid and (Length(ATicket.Ticket) > 0);
  if ATimeout > 0 then
    FTimeout := ATimeout;
  if FHasTicket then
  begin
    FTicket := ATicket;
    FSessionID := IntToHex(Length(FTicket.Ticket), 4) + '-' +
      IntToHex(FTicket.TicketAgeAdd, 8);
  end;
end;

constructor TFreePascalSession.CreateResumptionSnapshot(
  AProtocolVersion: TSSLProtocolVersion;
  ACipherSuite: Word;
  const ACipherName: string;
  APeerCertificate: ISSLCertificate;
  const APeerCertificateChain: TSSLCertificateArray;
  const ATicket: TTLS13NewSessionTicket;
  const AResumptionPSK: TBytes;
  ATimeout: Integer
);
begin
  CreateSnapshot(
    AProtocolVersion,
    ACipherName,
    APeerCertificate,
    APeerCertificateChain,
    ATicket,
    True,
    ATimeout
  );
  FCipherSuite := ACipherSuite;
  FResumptionPSK := Copy(AResumptionPSK, 0, Length(AResumptionPSK));
end;

constructor TFreePascalSession.CreateTLS12ResumptionSnapshot(
  AProtocolVersion: TSSLProtocolVersion;
  ACipherSuite: Word;
  const ACipherName: string;
  APeerCertificate: ISSLCertificate;
  const APeerCertificateChain: TSSLCertificateArray;
  const ASessionIDBytes: TBytes;
  const AMasterSecret: TBytes;
  const ASessionTicket: TBytes;
  ATicketLifetimeHint: Cardinal;
  ATimeout: Integer
);
begin
  CreateSnapshot(
    AProtocolVersion,
    ACipherName,
    APeerCertificate,
    APeerCertificateChain,
    Default(TTLS13NewSessionTicket),
    False,
    ATimeout
  );
  FCipherSuite := ACipherSuite;
  FTLS12SessionIDBytes := Copy(ASessionIDBytes, 0, Length(ASessionIDBytes));
  FTLS12MasterSecret := Copy(AMasterSecret, 0, Length(AMasterSecret));
  FTLS12SessionTicket := Copy(ASessionTicket, 0, Length(ASessionTicket));
  FTLS12SessionTicketLifetimeHint := ATicketLifetimeHint;
  if Length(FTLS12SessionIDBytes) > 0 then
    FSessionID := BytesToHexString(FTLS12SessionIDBytes);
end;

function TFreePascalSession.GetID: string;
begin
  Result := FSessionID;
end;

function TFreePascalSession.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TFreePascalSession.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TFreePascalSession.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TFreePascalSession.IsValid: Boolean;
begin
  if FTimeout <= 0 then
    Exit(True);
  Result := SecondsBetween(Now, FCreationTime) < FTimeout;
end;

function TFreePascalSession.IsResumable: Boolean;
begin
  Result := IsValid and (
    (FHasTicket and FTicket.Valid and (Length(FTicket.Ticket) > 0)) or
    ((Length(FTLS12SessionIDBytes) > 0) and (Length(FTLS12MasterSecret) = TLS12_MASTER_SECRET_LENGTH))
    or
    ((Length(FTLS12SessionTicket) > 0) and (Length(FTLS12MasterSecret) = TLS12_MASTER_SECRET_LENGTH))
  );
end;

function TFreePascalSession.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TFreePascalSession.GetCipherName: string;
begin
  Result := FCipherName;
end;

function TFreePascalSession.GetPeerCertificate: ISSLCertificate;
begin
  if FPeerCertificate <> nil then
    Result := FPeerCertificate.Clone
  else
    Result := nil;
end;

function TFreePascalSession.GetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := CloneCertificateArray(FPeerCertificateChain);
end;

function TFreePascalSession.Serialize: TBytes;
var
  LStream: TMemoryStream;
  LMagic: AnsiString;
  LPeerDER: TBytes;
  LChainDER: TBytes;
  I: Integer;
begin
  Result := nil;
  LStream := TMemoryStream.Create;
  try
    LMagic := FREEPASCAL_SESSION_MAGIC;
    LStream.WriteBuffer(LMagic[1], Length(LMagic));
    LStream.WriteBuffer(FCreationTime, SizeOf(FCreationTime));
    LStream.WriteBuffer(FTimeout, SizeOf(FTimeout));
    LStream.WriteBuffer(FProtocolVersion, SizeOf(FProtocolVersion));
    LStream.WriteBuffer(FCipherSuite, SizeOf(FCipherSuite));
    WriteStringValue(LStream, FSessionID);
    WriteStringValue(LStream, FCipherName);

    if FPeerCertificate <> nil then
      LPeerDER := FPeerCertificate.SaveToDER
    else
      LPeerDER := nil;
    WriteBytesValue(LStream, LPeerDER);

    LStream.WriteBuffer(FHasTicket, SizeOf(FHasTicket));
    LStream.WriteBuffer(FTicket.Valid, SizeOf(FTicket.Valid));
    LStream.WriteBuffer(FTicket.TicketLifetime, SizeOf(FTicket.TicketLifetime));
    LStream.WriteBuffer(FTicket.TicketAgeAdd, SizeOf(FTicket.TicketAgeAdd));
    WriteBytesValue(LStream, FTicket.TicketNonce);
    WriteBytesValue(LStream, FTicket.Ticket);
    WriteBytesValue(LStream, FTicket.Extensions);
    WriteBytesValue(LStream, FResumptionPSK);
    WriteIntegerValue(LStream, Length(FPeerCertificateChain));
    for I := 0 to High(FPeerCertificateChain) do
    begin
      if FPeerCertificateChain[I] <> nil then
        LChainDER := FPeerCertificateChain[I].SaveToDER
      else
        LChainDER := nil;
      WriteBytesValue(LStream, LChainDER);
    end;
    WriteBytesValue(LStream, FTLS12SessionIDBytes);
    WriteBytesValue(LStream, FTLS12MasterSecret);
    WriteBytesValue(LStream, FTLS12SessionTicket);
    LStream.WriteBuffer(FTLS12SessionTicketLifetimeHint, SizeOf(FTLS12SessionTicketLifetimeHint));

    SetLength(Result, LStream.Size);
    if LStream.Size > 0 then
      Move(PByte(LStream.Memory)^, Result[0], LStream.Size);
  finally
    LStream.Free;
  end;
end;

function TFreePascalSession.Deserialize(const AData: TBytes): Boolean;
var
  LStream: TMemoryStream;
  LMagic: array[0..3] of AnsiChar;
  LProtocol: Integer;
  LPeerDER: TBytes;
  LChainCount: Integer;
  LChainDER: TBytes;
  I: Integer;
  LCert: ISSLCertificate;
begin
  Result := False;
  if Length(AData) < 4 then
    Exit;

  LStream := TMemoryStream.Create;
  try
    if Length(AData) > 0 then
      LStream.WriteBuffer(AData[0], Length(AData));
    LStream.Position := 0;

    if LStream.Read(LMagic[0], 4) <> 4 then
      Exit;
    if string(LMagic) <> FREEPASCAL_SESSION_MAGIC then
      Exit;

    if LStream.Read(FCreationTime, SizeOf(FCreationTime)) <> SizeOf(FCreationTime) then
      Exit;
    if LStream.Read(FTimeout, SizeOf(FTimeout)) <> SizeOf(FTimeout) then
      Exit;
    if LStream.Read(LProtocol, SizeOf(LProtocol)) <> SizeOf(LProtocol) then
      Exit;
    FProtocolVersion := TSSLProtocolVersion(LProtocol);
    if LStream.Read(FCipherSuite, SizeOf(FCipherSuite)) <> SizeOf(FCipherSuite) then
      Exit;

    if not ReadStringValue(LStream, FSessionID) then
      Exit;
    if not ReadStringValue(LStream, FCipherName) then
      Exit;
    if not ReadBytesValue(LStream, LPeerDER) then
      Exit;

    if Length(LPeerDER) > 0 then
    begin
      FPeerCertificate := TSSLFactory.CreateCertificate(sslFreePascal);
      if (FPeerCertificate = nil) or (not FPeerCertificate.LoadFromDER(LPeerDER)) then
        Exit(False);
    end
    else
      FPeerCertificate := nil;

    if LStream.Read(FHasTicket, SizeOf(FHasTicket)) <> SizeOf(FHasTicket) then
      Exit;
    if LStream.Read(FTicket.Valid, SizeOf(FTicket.Valid)) <> SizeOf(FTicket.Valid) then
      Exit;
    if LStream.Read(FTicket.TicketLifetime, SizeOf(FTicket.TicketLifetime)) <>
      SizeOf(FTicket.TicketLifetime) then
      Exit;
    if LStream.Read(FTicket.TicketAgeAdd, SizeOf(FTicket.TicketAgeAdd)) <>
      SizeOf(FTicket.TicketAgeAdd) then
      Exit;
    if not ReadBytesValue(LStream, FTicket.TicketNonce) then
      Exit;
    if not ReadBytesValue(LStream, FTicket.Ticket) then
      Exit;
    if not ReadBytesValue(LStream, FTicket.Extensions) then
      Exit;
    if not ReadBytesValue(LStream, FResumptionPSK) then
      Exit;

    if LStream.Position < LStream.Size then
    begin
      if LStream.Read(LChainCount, SizeOf(LChainCount)) <> SizeOf(LChainCount) then
        Exit;
      if LChainCount < 0 then
        Exit;
      SetLength(FPeerCertificateChain, LChainCount);
      for I := 0 to LChainCount - 1 do
      begin
        if not ReadBytesValue(LStream, LChainDER) then
          Exit;
        if Length(LChainDER) > 0 then
        begin
          LCert := TSSLFactory.CreateCertificate(sslFreePascal);
          if (LCert = nil) or (not LCert.LoadFromDER(LChainDER)) then
            Exit(False);
          FPeerCertificateChain[I] := LCert;
        end
        else
          FPeerCertificateChain[I] := nil;
      end;
    end
    else
      SetLength(FPeerCertificateChain, 0);

    if LStream.Position < LStream.Size then
    begin
      if not ReadBytesValue(LStream, FTLS12SessionIDBytes) then
        Exit;
      if not ReadBytesValue(LStream, FTLS12MasterSecret) then
        Exit;
      if (FSessionID = '') and (Length(FTLS12SessionIDBytes) > 0) then
        FSessionID := BytesToHexString(FTLS12SessionIDBytes);
    end
    else
    begin
      SetLength(FTLS12SessionIDBytes, 0);
      SetLength(FTLS12MasterSecret, 0);
    end;

    if LStream.Position < LStream.Size then
    begin
      if not ReadBytesValue(LStream, FTLS12SessionTicket) then
        Exit;
      if LStream.Read(FTLS12SessionTicketLifetimeHint, SizeOf(FTLS12SessionTicketLifetimeHint)) <>
        SizeOf(FTLS12SessionTicketLifetimeHint) then
        Exit;
    end
    else
    begin
      SetLength(FTLS12SessionTicket, 0);
      FTLS12SessionTicketLifetimeHint := 0;
    end;

    Result := True;
  finally
    LStream.Free;
  end;
end;

function TFreePascalSession.Clone: ISSLSession;
var
  LClone: TFreePascalSession;
begin
  LClone := TFreePascalSession.Create;
  LClone.FSessionID := FSessionID;
  LClone.FCreationTime := FCreationTime;
  LClone.FTimeout := FTimeout;
  LClone.FProtocolVersion := FProtocolVersion;
  LClone.FCipherSuite := FCipherSuite;
  LClone.FCipherName := FCipherName;
  if FPeerCertificate <> nil then
    LClone.FPeerCertificate := FPeerCertificate.Clone;
  LClone.FPeerCertificateChain := CloneCertificateArray(FPeerCertificateChain);
  LClone.FHasTicket := FHasTicket;
  LClone.FTicket := FTicket;
  LClone.FResumptionPSK := Copy(FResumptionPSK, 0, Length(FResumptionPSK));
  LClone.FTLS12SessionIDBytes := Copy(FTLS12SessionIDBytes, 0, Length(FTLS12SessionIDBytes));
  LClone.FTLS12MasterSecret := Copy(FTLS12MasterSecret, 0, Length(FTLS12MasterSecret));
  LClone.FTLS12SessionTicket := Copy(FTLS12SessionTicket, 0, Length(FTLS12SessionTicket));
  LClone.FTLS12SessionTicketLifetimeHint := FTLS12SessionTicketLifetimeHint;
  Result := LClone;
end;

function TFreePascalSession.HasResumptionMaterial: Boolean;
begin
  Result := FHasTicket and FTicket.Valid and (Length(FTicket.Ticket) > 0) and
    (Length(FResumptionPSK) > 0);
end;

function TFreePascalSession.HasTLS12ResumptionMaterial: Boolean;
begin
  Result := (Length(FTLS12SessionIDBytes) > 0) and
    (Length(FTLS12MasterSecret) = TLS12_MASTER_SECRET_LENGTH);
end;

function TFreePascalSession.HasTLS12TicketMaterial: Boolean;
begin
  Result := (Length(FTLS12SessionTicket) > 0) and
    (Length(FTLS12MasterSecret) = TLS12_MASTER_SECRET_LENGTH);
end;

function TFreePascalSession.GetCipherSuite: Word;
begin
  Result := FCipherSuite;
end;

function TFreePascalSession.GetTicket: TTLS13NewSessionTicket;
begin
  Result := FTicket;
end;

function TFreePascalSession.GetResumptionPSK: TBytes;
begin
  Result := Copy(FResumptionPSK, 0, Length(FResumptionPSK));
end;

function TFreePascalSession.GetTLS12SessionIDBytes: TBytes;
begin
  Result := Copy(FTLS12SessionIDBytes, 0, Length(FTLS12SessionIDBytes));
end;

function TFreePascalSession.GetTLS12MasterSecret: TBytes;
begin
  Result := Copy(FTLS12MasterSecret, 0, Length(FTLS12MasterSecret));
end;

function TFreePascalSession.GetTLS12SessionTicket: TBytes;
begin
  Result := Copy(FTLS12SessionTicket, 0, Length(FTLS12SessionTicket));
end;

function TFreePascalSession.GetTLS12SessionTicketLifetimeHint: Cardinal;
begin
  Result := FTLS12SessionTicketLifetimeHint;
end;

end.
