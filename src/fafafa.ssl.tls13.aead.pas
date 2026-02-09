{**
 * Unit: fafafa.ssl.tls13.aead
 * Purpose: TLS 1.3 记录层 AEAD 套件调度（纯 Pascal）
 *
 * 当前支持：
 * - TLS_CHACHA20_POLY1305_SHA256
 *
 * 预留（未实现）：
 * - TLS_AES_128_GCM_SHA256
 * - TLS_AES_256_GCM_SHA384
 *}

unit fafafa.ssl.tls13.aead;

{$mode ObjFPC}{$H+}

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
  fafafa.ssl.tls13.chacha20poly1305;

function TLS13AEADIsSupported(ACipherSuite: Word): Boolean;
begin
  Result := ACipherSuite = TLS13_CIPHER_CHACHA20_POLY1305_SHA256;
end;

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

function TryTLS13AEADEncrypt(
  ACipherSuite: Word;
  const AKey, ANonce, AAAD, APlaintext: TBytes;
  out AEncrypted: TBytes;
  out AError: string
): Boolean;
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
        AError := 'unsupported: AES-GCM TLS 1.3 AEAD is not implemented in pure FreePascal backend yet';
        Result := False;
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
        AError := 'unsupported: AES-GCM TLS 1.3 AEAD is not implemented in pure FreePascal backend yet';
        Result := False;
      end;
  else
    begin
      AError := Format('Unsupported TLS 1.3 cipher suite for pure AEAD: 0x%.4x', [ACipherSuite]);
      Result := False;
    end;
  end;
end;

end.
