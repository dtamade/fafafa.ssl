unit fafafa.ssl.openssl.api.sha3;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.sha3.evp;

const
  SHA3_224_DIGEST_LENGTH = fafafa.ssl.openssl.api.sha3.evp.SHA3_224_DIGEST_LENGTH;
  SHA3_256_DIGEST_LENGTH = fafafa.ssl.openssl.api.sha3.evp.SHA3_256_DIGEST_LENGTH;
  SHA3_384_DIGEST_LENGTH = fafafa.ssl.openssl.api.sha3.evp.SHA3_384_DIGEST_LENGTH;
  SHA3_512_DIGEST_LENGTH = fafafa.ssl.openssl.api.sha3.evp.SHA3_512_DIGEST_LENGTH;
  SHAKE128_DIGEST_LENGTH = fafafa.ssl.openssl.api.sha3.evp.SHAKE128_DIGEST_LENGTH;
  SHAKE256_DIGEST_LENGTH = fafafa.ssl.openssl.api.sha3.evp.SHAKE256_DIGEST_LENGTH;

type
  PSHA3_CTX = ^SHA3_CTX;
  SHA3_CTX = record
    DigestCtx: PEVP_MD_CTX;
    Digest: PEVP_MD;
    OwnsDigest: Boolean;
    OutputLen: NativeUInt;
    IsXOF: Boolean;
  end;

  TSHA3_Init = function(c: PSHA3_CTX): Integer; cdecl;
  TSHA3_Update = function(c: PSHA3_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA3_Final = function(md: PByte; c: PSHA3_CTX): Integer; cdecl;
  TSHA3_Hash = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TSHAKE_Hash = function(const d: PByte; n: NativeUInt; md: PByte; outlen: NativeUInt): PByte; cdecl;

var
  SHA3_224_Init: TSHA3_Init = nil;
  SHA3_224_Update: TSHA3_Update = nil;
  SHA3_224_Final: TSHA3_Final = nil;
  SHA3_224: TSHA3_Hash = nil;

  SHA3_256_Init: TSHA3_Init = nil;
  SHA3_256_Update: TSHA3_Update = nil;
  SHA3_256_Final: TSHA3_Final = nil;
  SHA3_256: TSHA3_Hash = nil;

  SHA3_384_Init: TSHA3_Init = nil;
  SHA3_384_Update: TSHA3_Update = nil;
  SHA3_384_Final: TSHA3_Final = nil;
  SHA3_384: TSHA3_Hash = nil;

  SHA3_512_Init: TSHA3_Init = nil;
  SHA3_512_Update: TSHA3_Update = nil;
  SHA3_512_Final: TSHA3_Final = nil;
  SHA3_512: TSHA3_Hash = nil;

  SHAKE128: TSHAKE_Hash = nil;
  SHAKE256: TSHAKE_Hash = nil;

function LoadSHA3Functions(ALibHandle: THandle): Boolean;
procedure UnloadSHA3Functions;
procedure LoadSHA3Module(ALibCrypto: THandle);
procedure UnloadSHA3Module;
function LoadSHA3(ALibHandle: THandle): Boolean;
procedure UnloadSHA3;
function IsSHA3Loaded: Boolean;

implementation

procedure ResetSHA3Context(AContext: PSHA3_CTX);
begin
  if AContext = nil then
    Exit;

  if Assigned(AContext^.DigestCtx) and Assigned(EVP_MD_CTX_free) then
    EVP_MD_CTX_free(AContext^.DigestCtx);

  if AContext^.OwnsDigest and Assigned(AContext^.Digest) and Assigned(EVP_MD_free) then
    EVP_MD_free(AContext^.Digest);

  FillChar(AContext^, SizeOf(AContext^), 0);
end;

function ResolveDigest(const AName: PAnsiChar; out AOwnsDigest: Boolean): PEVP_MD;
begin
  Result := nil;
  AOwnsDigest := False;

  if Assigned(EVP_get_digestbyname) then
    Result := EVP_get_digestbyname(AName);

  if (Result = nil) and Assigned(EVP_MD_fetch) then
  begin
    Result := EVP_MD_fetch(nil, AName, nil);
    AOwnsDigest := Result <> nil;
  end;
end;

function InitSHA3Context(AContext: PSHA3_CTX; const AName: PAnsiChar; AIsXOF: Boolean;
  AOutputLen: NativeUInt): Integer;
begin
  Result := 0;

  if (AContext = nil) or (not Assigned(EVP_MD_CTX_new)) or
    (not Assigned(EVP_DigestInit_ex)) then
    Exit;

  ResetSHA3Context(AContext);

  AContext^.Digest := ResolveDigest(AName, AContext^.OwnsDigest);
  if AContext^.Digest = nil then
    Exit;

  AContext^.DigestCtx := EVP_MD_CTX_new();
  if AContext^.DigestCtx = nil then
  begin
    ResetSHA3Context(AContext);
    Exit;
  end;

  if EVP_DigestInit_ex(AContext^.DigestCtx, AContext^.Digest, nil) <> 1 then
  begin
    ResetSHA3Context(AContext);
    Exit;
  end;

  AContext^.IsXOF := AIsXOF;
  AContext^.OutputLen := AOutputLen;
  Result := 1;
end;

function UpdateSHA3Context(AContext: PSHA3_CTX; const AData: Pointer; ALength: NativeUInt): Integer; cdecl;
begin
  Result := 0;

  if (AContext = nil) or (AContext^.DigestCtx = nil) or (not Assigned(EVP_DigestUpdate)) then
    Exit;

  if ALength = 0 then
    Exit(1);

  if (AData = nil) then
    Exit;

  if EVP_DigestUpdate(AContext^.DigestCtx, AData, ALength) = 1 then
    Result := 1;
end;

function FinalSHA3Context(AMD: PByte; AContext: PSHA3_CTX): Integer; cdecl;
var
  LDigestLen: Cardinal;
begin
  Result := 0;

  if (AContext = nil) or (AContext^.DigestCtx = nil) or (AMD = nil) then
    Exit;

  if AContext^.IsXOF then
  begin
    if Assigned(EVP_DigestFinalXOF) and
      (EVP_DigestFinalXOF(AContext^.DigestCtx, AMD, AContext^.OutputLen) = 1) then
      Result := 1;
  end
  else
  begin
    if Assigned(EVP_DigestFinal_ex) then
    begin
      LDigestLen := AContext^.OutputLen;
      if EVP_DigestFinal_ex(AContext^.DigestCtx, AMD, LDigestLen) = 1 then
        Result := 1;
    end;
  end;

  ResetSHA3Context(AContext);
end;

function HashWithContext(AInit: TSHA3_Init; AUpdate: TSHA3_Update; AFinal: TSHA3_Final;
  const AData: PByte; ALength: NativeUInt; AOut: PByte): PByte;
var
  LContext: SHA3_CTX;
begin
  Result := nil;
  FillChar(LContext, SizeOf(LContext), 0);

  if (AOut = nil) or (not Assigned(AInit)) or (not Assigned(AUpdate)) or (not Assigned(AFinal)) then
    Exit;

  if AInit(@LContext) <> 1 then
    Exit;

  if (ALength > 0) and (AUpdate(@LContext, AData, ALength) <> 1) then
  begin
    ResetSHA3Context(@LContext);
    Exit;
  end;

  if AFinal(AOut, @LContext) <> 1 then
    Exit;

  Result := AOut;
end;

function CompatSHA3_224_Init(c: PSHA3_CTX): Integer; cdecl;
begin
  Result := InitSHA3Context(c, 'SHA3-224', False, SHA3_224_DIGEST_LENGTH);
end;

function CompatSHA3_256_Init(c: PSHA3_CTX): Integer; cdecl;
begin
  Result := InitSHA3Context(c, 'SHA3-256', False, SHA3_256_DIGEST_LENGTH);
end;

function CompatSHA3_384_Init(c: PSHA3_CTX): Integer; cdecl;
begin
  Result := InitSHA3Context(c, 'SHA3-384', False, SHA3_384_DIGEST_LENGTH);
end;

function CompatSHA3_512_Init(c: PSHA3_CTX): Integer; cdecl;
begin
  Result := InitSHA3Context(c, 'SHA3-512', False, SHA3_512_DIGEST_LENGTH);
end;

function CompatSHA3_224(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
begin
  Result := HashWithContext(SHA3_224_Init, SHA3_224_Update, SHA3_224_Final, d, n, md);
end;

function CompatSHA3_256(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
begin
  Result := HashWithContext(SHA3_256_Init, SHA3_256_Update, SHA3_256_Final, d, n, md);
end;

function CompatSHA3_384(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
begin
  Result := HashWithContext(SHA3_384_Init, SHA3_384_Update, SHA3_384_Final, d, n, md);
end;

function CompatSHA3_512(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
begin
  Result := HashWithContext(SHA3_512_Init, SHA3_512_Update, SHA3_512_Final, d, n, md);
end;

function CompatSHAKE128(const d: PByte; n: NativeUInt; md: PByte; outlen: NativeUInt): PByte; cdecl;
var
  LContext: SHA3_CTX;
begin
  Result := nil;
  FillChar(LContext, SizeOf(LContext), 0);

  if (md = nil) or
    (InitSHA3Context(@LContext, 'SHAKE128', True, outlen) <> 1) then
    Exit;

  if (n > 0) and (UpdateSHA3Context(@LContext, d, n) <> 1) then
  begin
    ResetSHA3Context(@LContext);
    Exit;
  end;

  if FinalSHA3Context(md, @LContext) <> 1 then
    Exit;

  Result := md;
end;

function CompatSHAKE256(const d: PByte; n: NativeUInt; md: PByte; outlen: NativeUInt): PByte; cdecl;
var
  LContext: SHA3_CTX;
begin
  Result := nil;
  FillChar(LContext, SizeOf(LContext), 0);

  if (md = nil) or
    (InitSHA3Context(@LContext, 'SHAKE256', True, outlen) <> 1) then
    Exit;

  if (n > 0) and (UpdateSHA3Context(@LContext, d, n) <> 1) then
  begin
    ResetSHA3Context(@LContext);
    Exit;
  end;

  if FinalSHA3Context(md, @LContext) <> 1 then
    Exit;

  Result := md;
end;

function LoadSHA3Functions(ALibHandle: THandle): Boolean;
begin
  Result := False;

  if ALibHandle = 0 then
    Exit;

  if not LoadEVP(ALibHandle) then
    Exit;

  if (not Assigned(EVP_MD_CTX_new)) or (not Assigned(EVP_DigestInit_ex)) or
    (not Assigned(EVP_DigestUpdate)) or (not Assigned(EVP_DigestFinal_ex)) then
    Exit;

  if (not Assigned(EVP_get_digestbyname)) and (not Assigned(EVP_MD_fetch)) then
    Exit;

  SHA3_224_Init := @CompatSHA3_224_Init;
  SHA3_224_Update := @UpdateSHA3Context;
  SHA3_224_Final := @FinalSHA3Context;
  SHA3_224 := @CompatSHA3_224;

  SHA3_256_Init := @CompatSHA3_256_Init;
  SHA3_256_Update := @UpdateSHA3Context;
  SHA3_256_Final := @FinalSHA3Context;
  SHA3_256 := @CompatSHA3_256;

  SHA3_384_Init := @CompatSHA3_384_Init;
  SHA3_384_Update := @UpdateSHA3Context;
  SHA3_384_Final := @FinalSHA3Context;
  SHA3_384 := @CompatSHA3_384;

  SHA3_512_Init := @CompatSHA3_512_Init;
  SHA3_512_Update := @UpdateSHA3Context;
  SHA3_512_Final := @FinalSHA3Context;
  SHA3_512 := @CompatSHA3_512;

  SHAKE128 := @CompatSHAKE128;
  SHAKE256 := @CompatSHAKE256;

  TOpenSSLLoader.SetModuleLoaded(osmSHA3, True);
  Result := True;
end;

procedure UnloadSHA3Functions;
begin
  SHA3_224_Init := nil;
  SHA3_224_Update := nil;
  SHA3_224_Final := nil;
  SHA3_224 := nil;

  SHA3_256_Init := nil;
  SHA3_256_Update := nil;
  SHA3_256_Final := nil;
  SHA3_256 := nil;

  SHA3_384_Init := nil;
  SHA3_384_Update := nil;
  SHA3_384_Final := nil;
  SHA3_384 := nil;

  SHA3_512_Init := nil;
  SHA3_512_Update := nil;
  SHA3_512_Final := nil;
  SHA3_512 := nil;

  SHAKE128 := nil;
  SHAKE256 := nil;

  TOpenSSLLoader.SetModuleLoaded(osmSHA3, False);
end;

procedure LoadSHA3Module(ALibCrypto: THandle);
begin
  LoadSHA3Functions(ALibCrypto);
end;

procedure UnloadSHA3Module;
begin
  UnloadSHA3Functions;
end;

function LoadSHA3(ALibHandle: THandle): Boolean;
begin
  Result := LoadSHA3Functions(ALibHandle);
end;

procedure UnloadSHA3;
begin
  UnloadSHA3Functions;
end;

function IsSHA3Loaded: Boolean;
begin
  Result := TOpenSSLLoader.IsModuleLoaded(osmSHA3);
end;

end.
