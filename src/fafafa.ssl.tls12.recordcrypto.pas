{**
 * Unit: fafafa.ssl.tls12.recordcrypto
 * Purpose: TLS 1.2 record crypto helpers for client-only minimum slice
 *}

unit fafafa.ssl.tls12.recordcrypto;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function BuildTLS12AdditionalData(
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  APlaintextLength: Word
): TBytes;

function BuildTLS12ChaCha20Poly1305Nonce(
  const AWriteIV: TBytes;
  ASequenceNumber: QWord
): TBytes;

function TryEncryptTLS12ChaCha20Poly1305Record(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;

function TryDecryptTLS12ChaCha20Poly1305Record(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;

function TryEncryptTLS12AES128GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;

function TryDecryptTLS12AES128GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;

function TryEncryptTLS12AES256GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;

function TryDecryptTLS12AES256GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.tls13.chacha20poly1305,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.aead;

function BuildTLS12AdditionalData(
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  APlaintextLength: Word
): TBytes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, 13);
  for I := 0 to 7 do
    Result[7 - I] := Byte((ASequenceNumber shr (I * 8)) and $FF);
  Result[8] := AContentType;
  Result[9] := Byte((AProtocolVersion shr 8) and $FF);
  Result[10] := Byte(AProtocolVersion and $FF);
  Result[11] := Byte((APlaintextLength shr 8) and $FF);
  Result[12] := Byte(APlaintextLength and $FF);
end;

function BuildTLS12AESGCMNonce(const AWriteIV: TBytes; ASequenceNumber: QWord): TBytes;
var
  I: Integer;
begin
  if Length(AWriteIV) <> 4 then
    RaiseInvalidParameter('TLS12AESGCMWriteIV');

  Result := nil;
  SetLength(Result, 12);
  Move(AWriteIV[0], Result[0], 4);
  for I := 0 to 7 do
    Result[11 - I] := Byte((ASequenceNumber shr (I * 8)) and $FF);
end;

function EnsureTLS12AESGCMAvailable(out AError: string): Boolean;
begin
  Result := False;
  AError := '';
  try
    if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
      LoadOpenSSLCore;
    if not TOpenSSLLoader.IsModuleLoaded(osmEVP) then
      if not LoadEVP(GetCryptoLibHandle) then
      begin
        AError := 'Failed to load OpenSSL EVP module';
        Exit(False);
      end;
  except
    on E: Exception do
    begin
      AError := 'OpenSSL initialization failed: ' + E.Message;
      Exit(False);
    end;
  end;

  Result := Assigned(EVP_aes_128_gcm) and
            Assigned(EVP_CIPHER_CTX_new) and
            Assigned(EVP_EncryptInit_ex) and
            Assigned(EVP_DecryptInit_ex);
  if not Result then
    AError := 'OpenSSL AES-128-GCM primitives are unavailable';
end;

function TryEncryptTLS12AESGCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;
var
  LAAD: TBytes;
  LNonce: TBytes;
  LExplicitNonce: TBytes;
  LAEADResult: TAEADEncryptResult;
  I: Integer;
begin
  SetLength(AEncrypted, 0);
  AError := '';
  Result := False;

  if (Length(AKey) <> 16) and (Length(AKey) <> 32) then
  begin
    AError := 'TLS1.2 AES-GCM key must be 16 or 32 bytes';
    Exit;
  end;

  if not EnsureTLS12AESGCMAvailable(AError) then
    Exit;

  try
    LAAD := BuildTLS12AdditionalData(
      ASequenceNumber,
      AContentType,
      AProtocolVersion,
      Length(APlaintext)
    );
    LNonce := BuildTLS12AESGCMNonce(AWriteIV, ASequenceNumber);
  except
    on E: Exception do
    begin
      AError := E.Message;
      Exit;
    end;
  end;

  SetLength(LExplicitNonce, 8);
  for I := 0 to 7 do
    LExplicitNonce[7 - I] := Byte((ASequenceNumber shr (I * 8)) and $FF);

  LAEADResult := AES_GCM_Encrypt(AKey, LNonce, APlaintext, LAAD);
  if not LAEADResult.Success then
  begin
    AError := 'TLS1.2 AES-GCM encryption failed';
    if LAEADResult.ErrorMessage <> '' then
      AError := AError + ': ' + LAEADResult.ErrorMessage;
    Exit;
  end;

  SetLength(AEncrypted, 8 + Length(LAEADResult.CipherText) + Length(LAEADResult.Tag));
  Move(LExplicitNonce[0], AEncrypted[0], 8);
  if Length(LAEADResult.CipherText) > 0 then
    Move(LAEADResult.CipherText[0], AEncrypted[8], Length(LAEADResult.CipherText));
  if Length(LAEADResult.Tag) > 0 then
    Move(LAEADResult.Tag[0], AEncrypted[8 + Length(LAEADResult.CipherText)], Length(LAEADResult.Tag));
  Result := True;
end;

function TryDecryptTLS12AESGCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;
var
  LExplicitNonce: TBytes;
  LCipherText: TBytes;
  LTag: TBytes;
  LAAD: TBytes;
  LNonce: TBytes;
  I: Integer;
  LAEADResult: TAEADDecryptResult;
begin
  SetLength(APlaintext, 0);
  AError := '';
  Result := False;

  if (Length(AKey) <> 16) and (Length(AKey) <> 32) then
  begin
    AError := 'TLS1.2 AES-GCM key must be 16 or 32 bytes';
    Exit;
  end;
  if Length(AEncrypted) < 24 then
  begin
    AError := 'TLS1.2 AES-GCM encrypted payload is too short';
    Exit;
  end;

  if not EnsureTLS12AESGCMAvailable(AError) then
    Exit;

  SetLength(LExplicitNonce, 8);
  Move(AEncrypted[0], LExplicitNonce[0], 8);

  SetLength(LTag, 16);
  Move(AEncrypted[Length(AEncrypted) - 16], LTag[0], 16);
  SetLength(LCipherText, Length(AEncrypted) - 24);
  if Length(LCipherText) > 0 then
    Move(AEncrypted[8], LCipherText[0], Length(LCipherText));

  try
    LAAD := BuildTLS12AdditionalData(
      ASequenceNumber,
      AContentType,
      AProtocolVersion,
      Length(LCipherText)
    );
    SetLength(LNonce, 12);
    Move(AWriteIV[0], LNonce[0], 4);
    for I := 0 to 7 do
      LNonce[4 + I] := LExplicitNonce[I];
  except
    on E: Exception do
    begin
      AError := E.Message;
      Exit;
    end;
  end;

  LAEADResult := AES_GCM_Decrypt(AKey, LNonce, LCipherText, LTag, LAAD);
  if not LAEADResult.Success then
  begin
    AError := 'TLS1.2 AES-GCM decryption/authentication failed';
    if LAEADResult.ErrorMessage <> '' then
      AError := AError + ': ' + LAEADResult.ErrorMessage;
    Exit;
  end;

  APlaintext := LAEADResult.PlainText;
  Result := True;
end;

function BuildTLS12ChaCha20Poly1305Nonce(
  const AWriteIV: TBytes;
  ASequenceNumber: QWord
): TBytes;
var
  I: Integer;
  LPaddedSeq: TBytes;
begin
  if Length(AWriteIV) <> 12 then
    RaiseInvalidParameter('TLS12ChaChaWriteIV');

  Result := nil;
  SetLength(Result, 12);
  Move(AWriteIV[0], Result[0], 12);

  SetLength(LPaddedSeq, 12);
  FillChar(LPaddedSeq[0], 12, 0);
  for I := 0 to 7 do
    LPaddedSeq[11 - I] := Byte((ASequenceNumber shr (I * 8)) and $FF);

  for I := 0 to 11 do
    Result[I] := Result[I] xor LPaddedSeq[I];
end;

function TryEncryptTLS12ChaCha20Poly1305Record(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;
var
  LAAD: TBytes;
  LNonce: TBytes;
begin
  SetLength(AEncrypted, 0);
  AError := '';
  Result := False;

  try
    LAAD := BuildTLS12AdditionalData(
      ASequenceNumber,
      AContentType,
      AProtocolVersion,
      Length(APlaintext)
    );
    LNonce := BuildTLS12ChaCha20Poly1305Nonce(AWriteIV, ASequenceNumber);
  except
    on E: Exception do
    begin
      AError := E.Message;
      Exit;
    end;
  end;

  if not TryChaCha20Poly1305EncryptCombined(AKey, LNonce, LAAD, APlaintext, AEncrypted) then
  begin
    AError := 'TLS1.2 ChaCha20-Poly1305 encryption failed';
    Exit;
  end;

  Result := True;
end;

function TryDecryptTLS12ChaCha20Poly1305Record(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;
var
  LAAD: TBytes;
  LNonce: TBytes;
  LPlaintextLength: Integer;
begin
  SetLength(APlaintext, 0);
  AError := '';
  Result := False;

  if Length(AEncrypted) < 16 then
  begin
    AError := 'TLS1.2 ChaCha20-Poly1305 encrypted payload is shorter than authentication tag';
    Exit;
  end;

  LPlaintextLength := Length(AEncrypted) - 16;
  try
    LAAD := BuildTLS12AdditionalData(
      ASequenceNumber,
      AContentType,
      AProtocolVersion,
      LPlaintextLength
    );
    LNonce := BuildTLS12ChaCha20Poly1305Nonce(AWriteIV, ASequenceNumber);
  except
    on E: Exception do
    begin
      AError := E.Message;
      Exit;
    end;
  end;

  if not TryChaCha20Poly1305DecryptCombined(AKey, LNonce, LAAD, AEncrypted, APlaintext) then
  begin
    AError := 'TLS1.2 ChaCha20-Poly1305 decryption/authentication failed';
    Exit;
  end;

  Result := True;
end;

function TryEncryptTLS12AES128GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;
begin
  Result := TryEncryptTLS12AESGCMRecord(
    AKey,
    AWriteIV,
    ASequenceNumber,
    AContentType,
    AProtocolVersion,
    APlaintext,
    AEncrypted,
    AError
  );
end;

function TryDecryptTLS12AES128GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;
begin
  Result := TryDecryptTLS12AESGCMRecord(
    AKey,
    AWriteIV,
    ASequenceNumber,
    AContentType,
    AProtocolVersion,
    AEncrypted,
    APlaintext,
    AError
  );
end;

function TryEncryptTLS12AES256GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;
begin
  Result := TryEncryptTLS12AESGCMRecord(
    AKey,
    AWriteIV,
    ASequenceNumber,
    AContentType,
    AProtocolVersion,
    APlaintext,
    AEncrypted,
    AError
  );
end;

function TryDecryptTLS12AES256GCMRecord(
  const AKey: TBytes;
  const AWriteIV: TBytes;
  ASequenceNumber: QWord;
  AContentType: Byte;
  AProtocolVersion: Word;
  const AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;
begin
  Result := TryDecryptTLS12AESGCMRecord(
    AKey,
    AWriteIV,
    ASequenceNumber,
    AContentType,
    AProtocolVersion,
    AEncrypted,
    APlaintext,
    AError
  );
end;

end.
