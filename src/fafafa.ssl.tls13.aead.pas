{**
 * Unit: fafafa.ssl.tls13.aead
 * Purpose: TLS 1.3 记录层 AEAD 套件调度（纯 Pascal）
 *
 * 当前支持：
 * - TLS_CHACHA20_POLY1305_SHA256（纯 Pascal）
 * - TLS_AES_128_GCM_SHA256（通过 OpenSSL EVP AEAD）
 * - TLS_AES_256_GCM_SHA384（通过 OpenSSL EVP AEAD）
 *}

unit fafafa.ssl.tls13.aead;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function TLS13AEADIsSupported(ACipherSuite: Word): Boolean;
function TLS13AEADTagLength(ACipherSuite: Word): Integer;

function TryTLS13AEADEncrypt(
  ACipherSuite: Word;
  const AKey, ANonce, AAAD, APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;

function TryTLS13AEADDecrypt(
  ACipherSuite: Word;
  const AKey, ANonce, AAAD, AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.chacha20poly1305,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.aead;

function TLS13AEADTagLength(ACipherSuite: Word): Integer;
begin
  case ACipherSuite of
    TLS13_CIPHER_AES_128_GCM_SHA256,
    TLS13_CIPHER_AES_256_GCM_SHA384,
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256:
      Result := 16;
  else
    Result := 0;
  end;
end;

function RequiredAESKeyLength(ACipherSuite: Word): Integer;
begin
  case ACipherSuite of
    TLS13_CIPHER_AES_128_GCM_SHA256: Result := 16;
    TLS13_CIPHER_AES_256_GCM_SHA384: Result := 32;
  else
    Result := 0;
  end;
end;

function EnsureAESGCMAvailable(out AError: string): Boolean;
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
            Assigned(EVP_aes_256_gcm) and
            Assigned(EVP_CIPHER_CTX_new) and
            Assigned(EVP_EncryptInit_ex) and
            Assigned(EVP_DecryptInit_ex);

  if not Result then
    AError := 'OpenSSL AES-GCM primitives are unavailable';
end;

function ValidateAESGCMInputs(
  ACipherSuite: Word;
  const AKey, ANonce: TBytes;
  out AError: string
): Boolean;
var
  LExpectedKeyLen: Integer;
begin
  Result := False;
  AError := '';

  LExpectedKeyLen := RequiredAESKeyLength(ACipherSuite);
  if LExpectedKeyLen = 0 then
  begin
    AError := Format('Unsupported TLS 1.3 AES-GCM suite: 0x%.4x', [ACipherSuite]);
    Exit(False);
  end;

  if Length(AKey) <> LExpectedKeyLen then
  begin
    AError := Format(
      'Invalid AES-GCM key length for suite 0x%.4x: expected %d bytes, got %d',
      [ACipherSuite, LExpectedKeyLen, Length(AKey)]
    );
    Exit(False);
  end;

  if Length(ANonce) <> 12 then
  begin
    AError := Format('Invalid AES-GCM nonce length: expected 12 bytes, got %d', [Length(ANonce)]);
    Exit(False);
  end;

  Result := True;
end;

procedure CombineCipherTextAndTag(const ACipherText, ATag: TBytes; out AEncrypted: TBytes);
var
  LCipherLen, LTagLen: Integer;
begin
  LCipherLen := Length(ACipherText);
  LTagLen := Length(ATag);

  SetLength(AEncrypted, LCipherLen + LTagLen);
  if LCipherLen > 0 then
    Move(ACipherText[0], AEncrypted[0], LCipherLen);
  if LTagLen > 0 then
    Move(ATag[0], AEncrypted[LCipherLen], LTagLen);
end;

function SplitCipherTextAndTag(
  const AEncrypted: TBytes;
  out ACipherText, ATag: TBytes
): Boolean;
var
  LCipherLen: Integer;
begin
  Result := False;

  if Length(AEncrypted) < 16 then
  begin
    SetLength(ACipherText, 0);
    SetLength(ATag, 0);
    Exit(False);
  end;

  LCipherLen := Length(AEncrypted) - 16;
  SetLength(ACipherText, LCipherLen);
  SetLength(ATag, 16);

  if LCipherLen > 0 then
    Move(AEncrypted[0], ACipherText[0], LCipherLen);
  Move(AEncrypted[LCipherLen], ATag[0], 16);

  Result := True;
end;

function TLS13AEADIsSupported(ACipherSuite: Word): Boolean;
var
  LError: string;
begin
  case ACipherSuite of
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256:
      Result := True;

    TLS13_CIPHER_AES_128_GCM_SHA256,
    TLS13_CIPHER_AES_256_GCM_SHA384:
      Result := EnsureAESGCMAvailable(LError);
  else
    Result := False;
  end;
end;

function TryTLS13AEADEncrypt(
  ACipherSuite: Word;
  const AKey, ANonce, AAAD, APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;
var
  LAEADResult: TAEADEncryptResult;
begin
  SetLength(AEncrypted, 0);
  AError := '';

  case ACipherSuite of
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256:
      begin
        if not TryChaCha20Poly1305EncryptCombined(AKey, ANonce, AAAD, APlaintext, AEncrypted) then
        begin
          AError := 'ChaCha20-Poly1305 encryption failed';
          Exit(False);
        end;
        Result := True;
      end;

    TLS13_CIPHER_AES_128_GCM_SHA256,
    TLS13_CIPHER_AES_256_GCM_SHA384:
      begin
        if not ValidateAESGCMInputs(ACipherSuite, AKey, ANonce, AError) then
          Exit(False);

        if not EnsureAESGCMAvailable(AError) then
          Exit(False);

        LAEADResult := AES_GCM_Encrypt(AKey, ANonce, APlaintext, AAAD);
        if not LAEADResult.Success then
        begin
          AError := 'AES-GCM encryption failed';
          if LAEADResult.ErrorMessage <> '' then
            AError := AError + ': ' + LAEADResult.ErrorMessage;
          Exit(False);
        end;

        if Length(LAEADResult.Tag) <> 16 then
        begin
          AError := Format('AES-GCM encryption returned invalid tag length: %d', [Length(LAEADResult.Tag)]);
          Exit(False);
        end;

        CombineCipherTextAndTag(LAEADResult.CipherText, LAEADResult.Tag, AEncrypted);
        Result := True;
      end;
  else
    begin
      AError := Format('Unsupported TLS 1.3 cipher suite for pure AEAD: 0x%.4x', [ACipherSuite]);
      Result := False;
    end;
  end;
end;

function TryTLS13AEADDecrypt(
  ACipherSuite: Word;
  const AKey, ANonce, AAAD, AEncrypted: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;
var
  LCipherText: TBytes;
  LTag: TBytes;
  LAEADResult: TAEADDecryptResult;
begin
  SetLength(APlaintext, 0);
  AError := '';

  case ACipherSuite of
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256:
      begin
        if not TryChaCha20Poly1305DecryptCombined(AKey, ANonce, AAAD, AEncrypted, APlaintext) then
        begin
          AError := 'ChaCha20-Poly1305 decryption/authentication failed';
          Exit(False);
        end;
        Result := True;
      end;

    TLS13_CIPHER_AES_128_GCM_SHA256,
    TLS13_CIPHER_AES_256_GCM_SHA384:
      begin
        if not ValidateAESGCMInputs(ACipherSuite, AKey, ANonce, AError) then
          Exit(False);

        if not EnsureAESGCMAvailable(AError) then
          Exit(False);

        if not SplitCipherTextAndTag(AEncrypted, LCipherText, LTag) then
        begin
          AError := 'AES-GCM encrypted payload must include 16-byte authentication tag';
          Exit(False);
        end;

        LAEADResult := AES_GCM_Decrypt(AKey, ANonce, LCipherText, LTag, AAAD);
        if not LAEADResult.Success then
        begin
          AError := 'AES-GCM decryption/authentication failed';
          if LAEADResult.ErrorMessage <> '' then
            AError := AError + ': ' + LAEADResult.ErrorMessage;
          Exit(False);
        end;

        APlaintext := LAEADResult.PlainText;
        Result := True;
      end;
  else
    begin
      AError := Format('Unsupported TLS 1.3 cipher suite for pure AEAD: 0x%.4x', [ACipherSuite]);
      Result := False;
    end;
  end;
end;

procedure WarmUpAESGCMSupport;
var
  LWarmupError: string;
begin
  EnsureAESGCMAvailable(LWarmupError);
end;

initialization
  WarmUpAESGCMSupport;

end.
