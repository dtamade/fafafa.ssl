program test_cert_verify_cache_concurrency;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.cert.verify.cache,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.loader;

const
  TEST_CERT_PATH = 'tests/certificate/test_certs/signer_cert.pem';
  ACCESSOR_THREADS = 16;
  CACHE_THREADS = 12;
  CACHE_OPS_PER_THREAD = 2500;

type
  TGlobalCacheAccessorThread = class(TThread)
  private
    FCapturedPtr: PtrUInt;
    FSuccess: Boolean;
    FError: string;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property CapturedPtr: PtrUInt read FCapturedPtr;
    property Success: Boolean read FSuccess;
    property Error: string read FError;
  end;

  TCacheWorkerThread = class(TThread)
  private
    FCache: TCertVerifyCache;
    FCert: PX509;
    FOps: Integer;
    FSuccess: Boolean;
    FError: string;
  protected
    procedure Execute; override;
  public
    constructor Create(ACache: TCertVerifyCache; ACert: PX509; AOps: Integer);
    property Success: Boolean read FSuccess;
    property Error: string read FError;
  end;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

constructor TGlobalCacheAccessorThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCapturedPtr := 0;
  FSuccess := False;
  FError := '';
end;

procedure TGlobalCacheAccessorThread.Execute;
var
  I: Integer;
begin
  try
    for I := 1 to 200 do
      FCapturedPtr := PtrUInt(GetGlobalCertVerifyCache);
    FSuccess := FCapturedPtr <> 0;
  except
    on E: Exception do
    begin
      FSuccess := False;
      FError := E.Message;
    end;
  end;
end;

constructor TCacheWorkerThread.Create(ACache: TCertVerifyCache; ACert: PX509; AOps: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCache := ACache;
  FCert := ACert;
  FOps := AOps;
  FSuccess := False;
  FError := '';
end;

procedure TCacheWorkerThread.Execute;
var
  I: Integer;
  LRes: TCertVerifyResult;
begin
  try
    for I := 1 to FOps do
    begin
      if (I mod 3) = 0 then
      begin
        LRes.Valid := True;
        LRes.ErrorCode := 0;
        LRes.ErrorMessage := '';
        LRes.VerifiedAt := Now;
        FCache.Put(FCert, LRes);
      end
      else
        FCache.TryGet(FCert, LRes);
    end;
    FSuccess := True;
  except
    on E: Exception do
    begin
      FSuccess := False;
      FError := E.Message;
    end;
  end;
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

procedure TestGlobalCacheAccessorConcurrency;
var
  Threads: array[0..ACCESSOR_THREADS - 1] of TGlobalCacheAccessorThread;
  I: Integer;
  LFirstPtr: PtrUInt;
begin
  WriteLn('--- Test: Global cert verify cache accessor concurrency');

  for I := 0 to High(Threads) do
  begin
    Threads[I] := TGlobalCacheAccessorThread.Create;
    Threads[I].Start;
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor;
    Require(Threads[I].Success, Format('Accessor thread %d failed: %s', [I, Threads[I].Error]));
  end;

  LFirstPtr := Threads[0].CapturedPtr;
  Require(LFirstPtr <> 0, 'Global cache pointer must not be nil');

  for I := 1 to High(Threads) do
    Require(Threads[I].CapturedPtr = LFirstPtr,
      Format('Accessor thread %d captured different global cache pointer', [I]));

  for I := 0 to High(Threads) do
    Threads[I].Free;
end;

procedure TestCacheGetPutConcurrency(ACert: PX509);
var
  Cache: TCertVerifyCache;
  Threads: array[0..CACHE_THREADS - 1] of TCacheWorkerThread;
  I: Integer;
  Hits, Misses, Size: Int64;
  Initial: TCertVerifyResult;
begin
  WriteLn('--- Test: Cache TryGet/Put concurrency');

  Cache := TCertVerifyCache.Create(64, 3600);
  try
    Initial.Valid := True;
    Initial.ErrorCode := 0;
    Initial.ErrorMessage := '';
    Initial.VerifiedAt := Now;
    Cache.Put(ACert, Initial);

    for I := 0 to High(Threads) do
    begin
      Threads[I] := TCacheWorkerThread.Create(Cache, ACert, CACHE_OPS_PER_THREAD);
      Threads[I].Start;
    end;

    for I := 0 to High(Threads) do
    begin
      Threads[I].WaitFor;
      Require(Threads[I].Success, Format('Cache worker %d failed: %s', [I, Threads[I].Error]));
    end;

    Cache.GetStats(Hits, Misses, Size);
    Require(Size = 1, Format('Expected cache size to stay 1 for single cert, got %d', [Size]));
    Require(Hits > 0, 'Expected cache hits to be greater than 0');
    Require((Hits + Misses) > 0, 'Expected cache operations stats to be non-zero');

    for I := 0 to High(Threads) do
      Threads[I].Free;
  finally
    Cache.Free;
  end;
end;

var
  Cert: PX509;
begin
  WriteLn('fafafa.ssl - cert verify cache concurrency contract');

  LoadOpenSSLCore;
  LoadOpenSSLX509;
  LoadOpenSSLBIO;
  if not LoadOpenSSLPEM(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
    raise Exception.Create('LoadOpenSSLPEM failed');
  if not LoadEVP(TOpenSSLLoader.GetLibraryHandle(osslLibCrypto)) then
    raise Exception.Create('LoadEVP failed');

  Cert := nil;
  try
    Cert := LoadCertFromPEM(TEST_CERT_PATH);

    TestGlobalCacheAccessorConcurrency;
    TestCacheGetPutConcurrency(Cert);

    WriteLn('✅ cert verify cache concurrency contract passed');
  finally
    if (Cert <> nil) and Assigned(X509_free) then
      X509_free(Cert);
    UnloadOpenSSLCore;
  end;
end.
