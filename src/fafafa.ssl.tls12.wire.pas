{**
 * Unit: fafafa.ssl.tls12.wire
 * Purpose: TLS 1.2 线协议编码/解码基础工具
 *
 * 当前范围：
 * - record / handshake constants
 * - extension / cipher suite constants
 * - 基本字节序辅助
 *}

unit fafafa.ssl.tls12.wire;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

const
  TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC = 20;
  TLS_CONTENT_TYPE_ALERT = 21;
  TLS_CONTENT_TYPE_HANDSHAKE = 22;
  TLS_CONTENT_TYPE_APPLICATION_DATA = 23;

  TLS_HANDSHAKE_TYPE_CLIENT_HELLO = 1;
  TLS_HANDSHAKE_TYPE_SERVER_HELLO = 2;
  TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET = 4;
  TLS_HANDSHAKE_TYPE_CERTIFICATE = 11;
  TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE = 12;
  TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE = 14;
  TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE = 16;
  TLS_HANDSHAKE_TYPE_FINISHED = 20;

  TLS_EXTENSION_SERVER_NAME = $0000;
  TLS_EXTENSION_SUPPORTED_GROUPS = $000A;
  TLS_EXTENSION_EC_POINT_FORMATS = $000B;
  TLS_EXTENSION_SIGNATURE_ALGORITHMS = $000D;
  TLS_EXTENSION_ALPN = $0010;
  TLS_EXTENSION_SESSION_TICKET = $0023;
  TLS_EXTENSION_SUPPORTED_VERSIONS = $002B;

  TLS12_VERSION = $0303;

  TLS_COMPRESSION_NULL = 0;
  TLS_EC_POINT_FORMAT_UNCOMPRESSED = 0;

  TLS12_RANDOM_SIZE = 32;
  TLS12_MASTER_SECRET_LENGTH = 48;

  TLS_GROUP_SECP256R1 = $0017;
  TLS_GROUP_X25519 = $001D;

  TLS_SIG_RSA_PKCS1_SHA256 = $0401;
  TLS_SIG_ECDSA_SECP256R1_SHA256 = $0403;
  TLS_SIG_RSA_PKCS1_SHA512 = $0601;
  TLS_SIG_RSA_PSS_RSAE_SHA256 = $0804;

  TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256 = $C02F;
  TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384 = $C030;
  TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256 = $CCA8;

type
  TTLS12RecordHeader = record
    ContentType: Byte;
    ProtocolVersion: Word;
    Length: Word;
  end;

procedure AppendByte(var ADest: TBytes; AValue: Byte);
procedure AppendBytes(var ADest: TBytes; const AData: TBytes);
procedure AppendUInt16(var ADest: TBytes; AValue: Word);
procedure AppendUInt24(var ADest: TBytes; AValue: Cardinal);

function ReadUInt16(const AData: TBytes; AOffset: Integer): Word;
function ReadUInt24(const AData: TBytes; AOffset: Integer): Cardinal;

function BuildTLS12Plaintext(AContentType: Byte; const APayload: TBytes): TBytes;
function ParseTLS12RecordHeader(const AData: TBytes; out AHeader: TTLS12RecordHeader): Boolean;

function TLS12CipherSuiteToString(ACipherSuite: Word): string;
function TLS12SignatureSchemeToString(ASignatureScheme: Word): string;
function TLS12CipherSuiteFromName(const ACipherName: string): Word;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.tls13.wire;

procedure AppendByte(var ADest: TBytes; AValue: Byte);
begin
  fafafa.ssl.tls13.wire.AppendByte(ADest, AValue);
end;

procedure AppendBytes(var ADest: TBytes; const AData: TBytes);
begin
  fafafa.ssl.tls13.wire.AppendBytes(ADest, AData);
end;

procedure AppendUInt16(var ADest: TBytes; AValue: Word);
begin
  fafafa.ssl.tls13.wire.AppendUInt16(ADest, AValue);
end;

procedure AppendUInt24(var ADest: TBytes; AValue: Cardinal);
begin
  fafafa.ssl.tls13.wire.AppendUInt24(ADest, AValue);
end;

function ReadUInt16(const AData: TBytes; AOffset: Integer): Word;
begin
  Result := fafafa.ssl.tls13.wire.ReadUInt16(AData, AOffset);
end;

function ReadUInt24(const AData: TBytes; AOffset: Integer): Cardinal;
begin
  Result := fafafa.ssl.tls13.wire.ReadUInt24(AData, AOffset);
end;

function BuildTLS12Plaintext(AContentType: Byte; const APayload: TBytes): TBytes;
var
  LLen: Integer;
begin
  Result := nil;
  LLen := Length(APayload);
  if LLen > High(Word) then
    RaiseInvalidParameter('TLS12PlaintextPayloadLength');

  SetLength(Result, 5 + LLen);
  Result[0] := AContentType;
  Result[1] := Byte(TLS12_VERSION shr 8);
  Result[2] := Byte(TLS12_VERSION and $FF);
  Result[3] := Byte((LLen shr 8) and $FF);
  Result[4] := Byte(LLen and $FF);

  if LLen > 0 then
    Move(APayload[0], Result[5], LLen);
end;

function ParseTLS12RecordHeader(const AData: TBytes; out AHeader: TTLS12RecordHeader): Boolean;
begin
  FillChar(AHeader, SizeOf(AHeader), 0);
  Result := False;

  if Length(AData) < 5 then
    Exit;

  AHeader.ContentType := AData[0];
  AHeader.ProtocolVersion := ReadUInt16(AData, 1);
  AHeader.Length := ReadUInt16(AData, 3);
  Result := True;
end;

function TLS12CipherSuiteToString(ACipherSuite: Word): string;
begin
  case ACipherSuite of
    TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256:
      Result := 'TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256';
    TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384:
      Result := 'TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384';
    TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256:
      Result := 'TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256';
  else
    Result := Format('0x%.4x', [ACipherSuite]);
  end;
end;

function TLS12SignatureSchemeToString(ASignatureScheme: Word): string;
begin
  case ASignatureScheme of
    TLS_SIG_RSA_PKCS1_SHA256:
      Result := 'rsa_pkcs1_sha256';
    TLS_SIG_RSA_PKCS1_SHA512:
      Result := 'rsa_pkcs1_sha512';
    TLS_SIG_ECDSA_SECP256R1_SHA256:
      Result := 'ecdsa_secp256r1_sha256';
    TLS_SIG_RSA_PSS_RSAE_SHA256:
      Result := 'rsa_pss_rsae_sha256';
  else
    Result := Format('0x%.4x', [ASignatureScheme]);
  end;
end;

function TLS12CipherSuiteFromName(const ACipherName: string): Word;
begin
  if SameText(ACipherName, 'TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256') then
    Exit(TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256);
  if SameText(ACipherName, 'TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384') then
    Exit(TLS12_CIPHER_ECDHE_RSA_WITH_AES_256_GCM_SHA384);
  if SameText(ACipherName, 'TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256') then
    Exit(TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256);
  Result := 0;
end;

end.
