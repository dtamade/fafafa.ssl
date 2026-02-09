{**
 * Unit: fafafa.ssl.tls13.keyschedule
 * Purpose: TLS 1.3 Key Schedule（当前实现 SHA-256 路径）
 *}

unit fafafa.ssl.tls13.keyschedule;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.tls13.wire;

type
  TTLS13HandshakeSecrets = record
    Valid: Boolean;
    CipherSuite: Word;
    HashSize: Integer;
    KeyLength: Integer;
    IVLength: Integer;

    TranscriptHash: TBytes;

    EarlySecret: TBytes;
    DerivedSecret: TBytes;
    HandshakeSecret: TBytes;

    ClientHandshakeTrafficSecret: TBytes;
    ServerHandshakeTrafficSecret: TBytes;

    ClientHandshakeKey: TBytes;
    ServerHandshakeKey: TBytes;
    ClientHandshakeIV: TBytes;
    ServerHandshakeIV: TBytes;
  end;

procedure InitTLS13HandshakeSecrets(out ASecrets: TTLS13HandshakeSecrets);
procedure ClearTLS13HandshakeSecrets(var ASecrets: TTLS13HandshakeSecrets);

function TLS13CipherSuiteIsSHA256(ACipherSuite: Word): Boolean;
function TLS13CipherSuiteKeyLength(ACipherSuite: Word): Integer;

function TryDeriveTLS13HandshakeSecrets(
  ACipherSuite: Word;
  const ASharedSecret: TBytes;
  const ATranscriptData: TBytes;
  out ASecrets: TTLS13HandshakeSecrets;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.crypto.hash,
  fafafa.ssl.tls13.primitives;

const
  TLS13_SHA256_HASH_SIZE = 32;
  TLS13_DEFAULT_IV_SIZE = 12;

procedure InitTLS13HandshakeSecrets(out ASecrets: TTLS13HandshakeSecrets);
begin
  FillChar(ASecrets, SizeOf(ASecrets), 0);
  SetLength(ASecrets.TranscriptHash, 0);
  SetLength(ASecrets.EarlySecret, 0);
  SetLength(ASecrets.DerivedSecret, 0);
  SetLength(ASecrets.HandshakeSecret, 0);
  SetLength(ASecrets.ClientHandshakeTrafficSecret, 0);
  SetLength(ASecrets.ServerHandshakeTrafficSecret, 0);
  SetLength(ASecrets.ClientHandshakeKey, 0);
  SetLength(ASecrets.ServerHandshakeKey, 0);
  SetLength(ASecrets.ClientHandshakeIV, 0);
  SetLength(ASecrets.ServerHandshakeIV, 0);
end;

procedure ClearTLS13HandshakeSecrets(var ASecrets: TTLS13HandshakeSecrets);
begin
  InitTLS13HandshakeSecrets(ASecrets);
end;

function TLS13CipherSuiteIsSHA256(ACipherSuite: Word): Boolean;
begin
  Result :=
    (ACipherSuite = TLS13_CIPHER_AES_128_GCM_SHA256) or
    (ACipherSuite = TLS13_CIPHER_CHACHA20_POLY1305_SHA256);
end;

function TLS13CipherSuiteKeyLength(ACipherSuite: Word): Integer;
begin
  case ACipherSuite of
    TLS13_CIPHER_AES_128_GCM_SHA256:
      Result := 16;
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256:
      Result := 32;
  else
    Result := 0;
  end;
end;

function TryDeriveTLS13HandshakeSecrets(
  ACipherSuite: Word;
  const ASharedSecret: TBytes;
  const ATranscriptData: TBytes;
  out ASecrets: TTLS13HandshakeSecrets;
  out AError: string
): Boolean;
var
  LKeyLength: Integer;
  LZeroLength, LZeroHash, LEmptyHash: TBytes;
begin
  InitTLS13HandshakeSecrets(ASecrets);
  AError := '';
  Result := False;

  if not TLS13CipherSuiteIsSHA256(ACipherSuite) then
  begin
    AError := Format('Cipher suite 0x%.4x requires non-SHA256 key schedule (not implemented yet)', [ACipherSuite]);
    Exit;
  end;

  LKeyLength := TLS13CipherSuiteKeyLength(ACipherSuite);
  if LKeyLength <= 0 then
  begin
    AError := 'Unsupported TLS 1.3 cipher suite key length';
    Exit;
  end;

  if Length(ASharedSecret) <> 32 then
  begin
    AError := 'X25519 shared secret length must be 32 bytes';
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

  // TLS 1.3 key schedule (no-PSK path)
  ASecrets.EarlySecret := HKDF_Extract_SHA256(LZeroLength, LZeroHash);
  ASecrets.DerivedSecret := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.EarlySecret,
    'derived',
    LEmptyHash,
    TLS13_SHA256_HASH_SIZE
  );

  ASecrets.HandshakeSecret := HKDF_Extract_SHA256(ASecrets.DerivedSecret, ASharedSecret);

  ASecrets.ClientHandshakeTrafficSecret := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.HandshakeSecret,
    'c hs traffic',
    ASecrets.TranscriptHash,
    TLS13_SHA256_HASH_SIZE
  );

  ASecrets.ServerHandshakeTrafficSecret := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.HandshakeSecret,
    's hs traffic',
    ASecrets.TranscriptHash,
    TLS13_SHA256_HASH_SIZE
  );

  ASecrets.ClientHandshakeKey := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ClientHandshakeTrafficSecret,
    'key',
    LZeroLength,
    LKeyLength
  );

  ASecrets.ServerHandshakeKey := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ServerHandshakeTrafficSecret,
    'key',
    LZeroLength,
    LKeyLength
  );

  ASecrets.ClientHandshakeIV := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ClientHandshakeTrafficSecret,
    'iv',
    LZeroLength,
    TLS13_DEFAULT_IV_SIZE
  );

  ASecrets.ServerHandshakeIV := TLS13_HKDF_Expand_Label_SHA256(
    ASecrets.ServerHandshakeTrafficSecret,
    'iv',
    LZeroLength,
    TLS13_DEFAULT_IV_SIZE
  );

  Result := True;
end;

end.
