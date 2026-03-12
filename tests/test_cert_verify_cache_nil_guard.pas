program test_cert_verify_cache_nil_guard;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.cert.verify.cache,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.loader;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

function LoadCertFromPEM(const APath: string): PX509;
var
  LBio: PBIO;
begin
  Result := nil;
  LBio := BIO_new_file(PAnsiChar(AnsiString(APath)), PAnsiChar('r'));
  if LBio = nil then
    raise Exception.CreateFmt('BIO_new_file failed: %s', [APath]);

  try
    Result := PEM_read_bio_X509(LBio, nil, nil, nil);
    if Result = nil then
      raise Exception.CreateFmt('PEM_read_bio_X509 failed: %s', [APath]);
  finally
    BIO_free(LBio);
  end;
end;

var
  Cache: TCertVerifyCache;
  ZeroCapCache: TCertVerifyCache;
  ZeroTTLCache: TCertVerifyCache;
  Cert: PX509;
  VerifyResult: TCertVerifyResult;
  ReadResult: TCertVerifyResult;
  Hit: Boolean;
  Hits, Misses, Size: Int64;
begin
  WriteLn('fafafa.ssl - cert verify cache nil guard contract');

  LoadOpenSSLCore;
  LoadOpenSSLX509;
  LoadOpenSSLBIO;
  if not LoadOpenSSLPEM(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
    raise Exception.Create('LoadOpenSSLPEM failed');
  if not LoadEVP(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
    raise Exception.Create('LoadEVP failed');

  Cert := nil;
  Cache := TCertVerifyCache.Create(8, 3600);
  try
    Cert := LoadCertFromPEM('tests/certificate/test_certs/signer_cert.pem');

    VerifyResult.Valid := True;
    VerifyResult.ErrorCode := 0;
    VerifyResult.ErrorMessage := '';
    VerifyResult.VerifiedAt := Now;

    Cache.Put(nil, VerifyResult);
    Cache.GetStats(Hits, Misses, Size);
    Require(Size = 0, 'Put(nil, ...) must not add cache entry');

    Hit := Cache.TryGet(nil, ReadResult);
    Require(not Hit, 'TryGet(nil, ...) must always miss');
    Cache.GetStats(Hits, Misses, Size);
    Require(Misses >= 1, 'TryGet(nil, ...) should increase miss counter');

    // Capacity=0 should behave as disabled cache (no crash, no writes).
    ZeroCapCache := TCertVerifyCache.Create(0, 3600);
    try
      ZeroCapCache.Put(Cert, VerifyResult);
      ZeroCapCache.GetStats(Hits, Misses, Size);
      Require(Size = 0, 'Capacity=0 cache must remain empty');
    finally
      ZeroCapCache.Free;
    end;

    // TTL=0 should fail closed: entries are immediately treated as expired.
    ZeroTTLCache := TCertVerifyCache.Create(8, 0);
    try
      ZeroTTLCache.Put(Cert, VerifyResult);
      Hit := ZeroTTLCache.TryGet(Cert, ReadResult);
      Require(not Hit, 'TTL=0 cache must not return a hit');
      ZeroTTLCache.GetStats(Hits, Misses, Size);
      Require(Misses >= 1, 'TTL=0 cache miss counter should increase');
    finally
      ZeroTTLCache.Free;
    end;
  finally
    Cache.Free;
    if (Cert <> nil) and Assigned(X509_free) then
      X509_free(Cert);
  end;

  UnloadOpenSSLCore;
  WriteLn('✅ cert verify cache nil guard contract passed');
end.
