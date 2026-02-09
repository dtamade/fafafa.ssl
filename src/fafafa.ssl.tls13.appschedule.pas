{**
 * Unit: fafafa.ssl.tls13.appschedule
 * Purpose: TLS 1.3 应用流量密钥派生（纯 Pascal）
 *
 * 当前实现：SHA-256 路径（no-PSK）
 * - master_secret
 * - c ap traffic / s ap traffic
 * - application write/read key + iv
 *}

unit fafafa.ssl.tls13.appschedule;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TTLS13ApplicationSecrets = record
    Valid: Boolean;
    CipherSuite: Word;
    HashSize: Integer;
    KeyLength: Integer;
    IVLength: Integer;

    TranscriptHash: TBytes;
    DerivedSecret: TBytes;
    MasterSecret: TBytes;

    ClientApplicationTrafficSecret: TBytes;
    ServerApplicationTrafficSecret: TBytes;

    ClientApplicationKey: TBytes;
    ServerApplicationKey: TBytes;

    ClientApplicationIV: TBytes;
    ServerApplicationIV: TBytes;
  end;

procedure InitTLS13ApplicationSecrets(out ASecrets: TTLS13ApplicationSecrets);
procedure ClearTLS13ApplicationSecrets(var ASecrets: TTLS13ApplicationSecrets);

function TryDeriveTLS13ApplicationSecrets(
  ACipherSuite: Word;
  const AHandshakeSecret: TBytes;
  const ATranscriptData: TBytes;
  out ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;

function TryUpdateTLS13ClientApplicationWriteKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;

function TryUpdateTLS13ServerApplicationReadKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;

function TryUpdateTLS13ServerApplicationWriteKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;

function TryUpdateTLS13ClientApplicationReadKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.crypto.hash,
  fafafa.ssl.tls13.primitives,
  fafafa.ssl.tls13.keyschedule;

const
  TLS13_SHA256_HASH_SIZE = 32;
  TLS13_DEFAULT_IV_SIZE = 12;

procedure InitTLS13ApplicationSecrets(out ASecrets: TTLS13ApplicationSecrets);
begin
  FillChar(ASecrets, SizeOf(ASecrets), 0);
  SetLength(ASecrets.TranscriptHash, 0);
  SetLength(ASecrets.DerivedSecret, 0);
  SetLength(ASecrets.MasterSecret, 0);
  SetLength(ASecrets.ClientApplicationTrafficSecret, 0);
  SetLength(ASecrets.ServerApplicationTrafficSecret, 0);
  SetLength(ASecrets.ClientApplicationKey, 0);
  SetLength(ASecrets.ServerApplicationKey, 0);
  SetLength(ASecrets.ClientApplicationIV, 0);
  SetLength(ASecrets.ServerApplicationIV, 0);
end;

procedure ClearTLS13ApplicationSecrets(var ASecrets: TTLS13ApplicationSecrets);
begin
  InitTLS13ApplicationSecrets(ASecrets);
end;

function TryDeriveTLS13ApplicationSecrets(
  ACipherSuite: Word;
  const AHandshakeSecret: TBytes;
  const ATranscriptData: TBytes;
  out ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;
var
  LKeyLength: Integer;
  LZeroLength: TBytes;
  LZeroHash: TBytes;
  LEmptyHash: TBytes;
begin
  InitTLS13ApplicationSecrets(ASecrets);
  AError := '';
  Result := False;

  if not TLS13CipherSuiteIsSHA256(ACipherSuite) then
  begin
    AError := Format('Cipher suite 0x%.4x requires non-SHA256 application key schedule (not implemented yet)', [ACipherSuite]);
    Exit;
  end;

  LKeyLength := TLS13CipherSuiteKeyLength(ACipherSuite);
  if LKeyLength <= 0 then
  begin
    AError := 'Unsupported TLS 1.3 cipher suite key length';
    Exit;
  end;

  if Length(AHandshakeSecret) <> TLS13_SHA256_HASH_SIZE then
  begin
    AError := 'TLS 1.3 handshake secret length must be 32 bytes (SHA-256 path)';
    Exit;
  end;

  SetLength(LZeroLength, 0);
  SetLength(LZeroHash, TLS13_SHA256_HASH_SIZE);
  FillChar(LZeroHash[0], TLS13_SHA256_HASH_SIZE, 0);
  LEmptyHash := SHA256(LZeroLength);

  ASecrets.Valid := True;
  ASecrets.CipherSuite := ACipherSuite;
  ASecrets.HashSize := TLS13_SHA256_HASH_SIZE;
  ASecrets.KeyLength := LKeyLength;
  ASecrets.IVLength := TLS13_DEFAULT_IV_SIZE;
  ASecrets.TranscriptHash := SHA256(ATranscriptData);

  ASecrets.DerivedSecret := TLS13_HKDF_Expand_Label_SHA256(
    AHandshakeSecret,
    'derived',
    LEmptyHash,
    TLS13_SHA256_HASH_SIZE
  );
  ASecrets.MasterSecret := HKDF_Extract_SHA256(ASecrets.DerivedSecret, LZeroHash);

  ASecrets.ClientApplicationTrafficSecret := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.MasterSecret,
    'c ap traffic',
    ASecrets.TranscriptHash,
    TLS13_SHA256_HASH_SIZE
  );

  ASecrets.ServerApplicationTrafficSecret := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.MasterSecret,
    's ap traffic',
    ASecrets.TranscriptHash,
    TLS13_SHA256_HASH_SIZE
  );

  ASecrets.ClientApplicationKey := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ClientApplicationTrafficSecret,
    'key',
    LZeroLength,
    LKeyLength
  );

  ASecrets.ServerApplicationKey := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ServerApplicationTrafficSecret,
    'key',
    LZeroLength,
    LKeyLength
  );

  ASecrets.ClientApplicationIV := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ClientApplicationTrafficSecret,
    'iv',
    LZeroLength,
    TLS13_DEFAULT_IV_SIZE
  );

  ASecrets.ServerApplicationIV := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ServerApplicationTrafficSecret,
    'iv',
    LZeroLength,
    TLS13_DEFAULT_IV_SIZE
  );

  Result := True;
end;

function TryNextTLS13ApplicationTrafficSecretSHA256(
  const ACurrentTrafficSecret: TBytes;
  out ANextTrafficSecret: TBytes;
  out AError: string
): Boolean;
var
  LZeroLength: TBytes;
begin
  SetLength(ANextTrafficSecret, 0);
  AError := '';
  Result := False;

  if Length(ACurrentTrafficSecret) <> TLS13_SHA256_HASH_SIZE then
  begin
    AError := Format('Invalid traffic secret length for key update (expected=%d actual=%d)',
      [TLS13_SHA256_HASH_SIZE, Length(ACurrentTrafficSecret)]);
    Exit;
  end;

  SetLength(LZeroLength, 0);
  ANextTrafficSecret := TLS13_HKDF_Expand_Label_SHA256(
    ACurrentTrafficSecret,
    'traffic upd',
    LZeroLength,
    TLS13_SHA256_HASH_SIZE
  );

  Result := True;
end;

function TryDeriveTLS13ApplicationKeyMaterialSHA256(
  const ATrafficSecret: TBytes;
  AKeyLength, AIVLength: Integer;
  out AKey, AIV: TBytes;
  out AError: string
): Boolean;
var
  LZeroLength: TBytes;
begin
  SetLength(AKey, 0);
  SetLength(AIV, 0);
  AError := '';
  Result := False;

  if Length(ATrafficSecret) <> TLS13_SHA256_HASH_SIZE then
  begin
    AError := Format('Invalid traffic secret length for key derivation (expected=%d actual=%d)',
      [TLS13_SHA256_HASH_SIZE, Length(ATrafficSecret)]);
    Exit;
  end;

  if AKeyLength <= 0 then
  begin
    AError := 'Invalid key length for application key derivation';
    Exit;
  end;

  if AIVLength <= 0 then
  begin
    AError := 'Invalid IV length for application key derivation';
    Exit;
  end;

  SetLength(LZeroLength, 0);
  AKey := TLS13_HKDF_Expand_Label_SHA256(ATrafficSecret, 'key', LZeroLength, AKeyLength);
  AIV := TLS13_HKDF_Expand_Label_SHA256(ATrafficSecret, 'iv', LZeroLength, AIVLength);

  Result := True;
end;

function TryUpdateTLS13ApplicationDirection(
  var ASecrets: TTLS13ApplicationSecrets;
  var ATrafficSecret, AKey, AIV: TBytes;
  out AError: string
): Boolean;
var
  LNextTrafficSecret: TBytes;
  LNextKey: TBytes;
  LNextIV: TBytes;
begin
  AError := '';
  Result := False;

  if not ASecrets.Valid then
  begin
    AError := 'Application secrets are not initialized';
    Exit;
  end;

  if not TLS13CipherSuiteIsSHA256(ASecrets.CipherSuite) then
  begin
    AError := Format('Cipher suite 0x%.4x does not support SHA256 key update path yet', [ASecrets.CipherSuite]);
    Exit;
  end;

  if not TryNextTLS13ApplicationTrafficSecretSHA256(ATrafficSecret, LNextTrafficSecret, AError) then
    Exit;

  if not TryDeriveTLS13ApplicationKeyMaterialSHA256(
    LNextTrafficSecret,
    ASecrets.KeyLength,
    ASecrets.IVLength,
    LNextKey,
    LNextIV,
    AError
  ) then
    Exit;

  ATrafficSecret := LNextTrafficSecret;
  AKey := LNextKey;
  AIV := LNextIV;

  Result := True;
end;

function TryUpdateTLS13ClientApplicationWriteKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;
begin
  Result := TryUpdateTLS13ApplicationDirection(
    ASecrets,
    ASecrets.ClientApplicationTrafficSecret,
    ASecrets.ClientApplicationKey,
    ASecrets.ClientApplicationIV,
    AError
  );
end;

function TryUpdateTLS13ServerApplicationReadKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;
begin
  Result := TryUpdateTLS13ApplicationDirection(
    ASecrets,
    ASecrets.ServerApplicationTrafficSecret,
    ASecrets.ServerApplicationKey,
    ASecrets.ServerApplicationIV,
    AError
  );
end;

function TryUpdateTLS13ServerApplicationWriteKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;
begin
  Result := TryUpdateTLS13ApplicationDirection(
    ASecrets,
    ASecrets.ServerApplicationTrafficSecret,
    ASecrets.ServerApplicationKey,
    ASecrets.ServerApplicationIV,
    AError
  );
end;

function TryUpdateTLS13ClientApplicationReadKeys(
  var ASecrets: TTLS13ApplicationSecrets;
  out AError: string
): Boolean;
begin
  Result := TryUpdateTLS13ApplicationDirection(
    ASecrets,
    ASecrets.ClientApplicationTrafficSecret,
    ASecrets.ClientApplicationKey,
    ASecrets.ClientApplicationIV,
    AError
  );
end;

end.
