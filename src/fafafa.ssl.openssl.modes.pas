{$IFNDEF WINDOWS}{$MODE DELPHI}{$ENDIF}

unit fafafa.ssl.openssl.modes;

interface

uses
  SysUtils,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.evp;

const
  // GCM constants
  GCM_BLOCK_SIZE = 16;
  GCM_IV_SIZE = 12;
  GCM_TAG_SIZE = 16;
  
  // CCM constants
  CCM_BLOCK_SIZE = 16;
  CCM_MIN_TAG_SIZE = 4;
  CCM_MAX_TAG_SIZE = 16;
  
  // XTS constants
  XTS_BLOCK_SIZE = 16;
  
  // OCB constants
  OCB_BLOCK_SIZE = 16;
  OCB_TAG_SIZE = 16;

  // EVP control commands for AEAD modes
  EVP_CTRL_AEAD_SET_IVLEN = $9;
  EVP_CTRL_AEAD_GET_TAG = $10;
  EVP_CTRL_AEAD_SET_TAG = $11;
  EVP_CTRL_CCM_SET_L = $14;
  EVP_CTRL_CCM_SET_MSGLEN = $15;
  EVP_CTRL_AEAD_SET_IV_FIXED = $12;
  
  // GCM specific
  EVP_CTRL_GCM_SET_IVLEN = $9;
  EVP_CTRL_GCM_GET_TAG = $10;
  EVP_CTRL_GCM_SET_TAG = $11;
  EVP_CTRL_GCM_SET_IV_FIXED = $12;
  EVP_CTRL_GCM_IV_GEN = $13;
  
  // CCM specific  
  EVP_CTRL_CCM_GET_TAG = EVP_CTRL_AEAD_GET_TAG;
  EVP_CTRL_CCM_SET_TAG = EVP_CTRL_AEAD_SET_TAG;
  EVP_CTRL_CCM_SET_IVLEN = EVP_CTRL_AEAD_SET_IVLEN;
  
  // OCB specific
  EVP_CTRL_OCB_SET_TAGLEN = $1c;

type
  // GCM context (opaque)
  GCM128_CONTEXT = record
    opaque_data: array[0..511] of Byte;
  end;
  PGCM128_CONTEXT = ^GCM128_CONTEXT;
  
  // CCM context (opaque)
  CCM128_CONTEXT = record
    opaque_data: array[0..511] of Byte;
  end;
  PCCM128_CONTEXT = ^CCM128_CONTEXT;
  
  // XTS context
  XTS128_CONTEXT = record
    opaque_data: array[0..255] of Byte;
  end;
  PXTS128_CONTEXT = ^XTS128_CONTEXT;
  
  // OCB context
  OCB128_CONTEXT = record
    opaque_data: array[0..511] of Byte;
  end;
  POCB128_CONTEXT = ^OCB128_CONTEXT;
  
  // Block cipher function types
  Tblock128_f = procedure(const inp: PByte; outp: PByte; const key: Pointer); cdecl;
  Tctr128_f = procedure(const inp: PByte; outp: PByte; blocks: size_t; 
                       const key: Pointer; ivec: PByte); cdecl;
  Tccm128_f = procedure(const inp: PByte; outp: PByte; blocks: size_t;
                       const key: Pointer; ivec: PByte; cmac: PByte); cdecl;
  
  // GCM function pointer types
  TGCM128_new = function(key: Pointer; block: Tblock128_f): PGCM128_CONTEXT; cdecl;
  TGCM128_free = procedure(ctx: PGCM128_CONTEXT); cdecl;
  TGCM128_setiv = function(ctx: PGCM128_CONTEXT; const iv: PByte; len: size_t): Integer; cdecl;
  TGCM128_aad = function(ctx: PGCM128_CONTEXT; const aad: PByte; len: size_t): Integer; cdecl;
  TGCM128_encrypt = function(ctx: PGCM128_CONTEXT; const inp: PByte; outp: PByte; len: size_t): Integer; cdecl;
  TGCM128_decrypt = function(ctx: PGCM128_CONTEXT; const inp: PByte; outp: PByte; len: size_t): Integer; cdecl;
  TGCM128_finish = function(ctx: PGCM128_CONTEXT; const tag: PByte; len: size_t): Integer; cdecl;
  TGCM128_tag = procedure(ctx: PGCM128_CONTEXT; tag: PByte; len: size_t); cdecl;
  
  // CCM function pointer types
  TCCM128_new = function(key: Pointer; block: Tblock128_f): PCCM128_CONTEXT; cdecl;
  TCCM128_free = procedure(ctx: PCCM128_CONTEXT); cdecl;
  TCCM128_init = function(ctx: PCCM128_CONTEXT; M: Cardinal; L: Cardinal; key: Pointer; block: Tblock128_f): Integer; cdecl;
  TCCM128_setiv = function(ctx: PCCM128_CONTEXT; const nonce: PByte; nlen: size_t; mlen: size_t): Integer; cdecl;
  TCCM128_aad = function(ctx: PCCM128_CONTEXT; const aad: PByte; alen: size_t): Integer; cdecl;
  TCCM128_encrypt = function(ctx: PCCM128_CONTEXT; const inp: PByte; outp: PByte; len: size_t): Integer; cdecl;
  TCCM128_decrypt = function(ctx: PCCM128_CONTEXT; const inp: PByte; outp: PByte; len: size_t): Integer; cdecl;
  TCCM128_tag = function(ctx: PCCM128_CONTEXT; tag: PByte; len: size_t): size_t; cdecl;
  
  // XTS function pointer types
  TXTS128_encrypt = function(const ctx: PXTS128_CONTEXT; const iv: PByte;
                            const inp: PByte; outp: PByte; len: size_t; enc: Integer): Integer; cdecl;
  TXTS128_decrypt = function(const ctx: PXTS128_CONTEXT; const iv: PByte;
                            const inp: PByte; outp: PByte; len: size_t; enc: Integer): Integer; cdecl;
  
  // OCB function pointer types  
  TOCB128_new = function(keyenc: Pointer; keydec: Pointer; encrypt_block: Tblock128_f;
                        decrypt_block: Tblock128_f): POCB128_CONTEXT; cdecl;
  TOCB128_free = procedure(ctx: POCB128_CONTEXT); cdecl;
  TOCB128_init = function(ctx: POCB128_CONTEXT; keyenc: Pointer; keydec: Pointer;
                         encrypt_block: Tblock128_f; decrypt_block: Tblock128_f): Integer; cdecl;
  TOCB128_setiv = function(ctx: POCB128_CONTEXT; const iv: PByte; len: size_t; taglen: size_t): Integer; cdecl;
  TOCB128_aad = function(ctx: POCB128_CONTEXT; const aad: PByte; len: size_t): Integer; cdecl;
  TOCB128_encrypt = function(ctx: POCB128_CONTEXT; const inp: PByte; outp: PByte; len: size_t): Integer; cdecl;
  TOCB128_decrypt = function(ctx: POCB128_CONTEXT; const inp: PByte; outp: PByte; len: size_t): Integer; cdecl;
  TOCB128_finish = function(ctx: POCB128_CONTEXT; const tag: PByte; len: size_t): Integer; cdecl;
  TOCB128_tag = function(ctx: POCB128_CONTEXT; tag: PByte; len: size_t): Integer; cdecl;
  
  // WRAP mode functions
  TAES_wrap_key = function(key: Pointer; const iv: PByte; outp: PByte;
                          const inp: PByte; inlen: size_t): Integer; cdecl;
  TAES_unwrap_key = function(key: Pointer; const iv: PByte; outp: PByte;
                            const inp: PByte; inlen: size_t): Integer; cdecl;

var
  // GCM functions
  GCM128_new: TGCM128_new = nil;
  GCM128_free: TGCM128_free = nil;
  GCM128_setiv: TGCM128_setiv = nil;
  GCM128_aad: TGCM128_aad = nil;
  GCM128_encrypt: TGCM128_encrypt = nil;
  GCM128_decrypt: TGCM128_decrypt = nil;
  GCM128_finish: TGCM128_finish = nil;
  GCM128_tag: TGCM128_tag = nil;
  
  // CCM functions
  CCM128_new: TCCM128_new = nil;
  CCM128_free: TCCM128_free = nil;
  CCM128_init: TCCM128_init = nil;
  CCM128_setiv: TCCM128_setiv = nil;
  CCM128_aad: TCCM128_aad = nil;
  CCM128_encrypt: TCCM128_encrypt = nil;
  CCM128_decrypt: TCCM128_decrypt = nil;
  CCM128_tag: TCCM128_tag = nil;
  
  // XTS functions
  XTS128_encrypt: TXTS128_encrypt = nil;
  XTS128_decrypt: TXTS128_decrypt = nil;
  
  // OCB functions
  OCB128_new: TOCB128_new = nil;
  OCB128_free: TOCB128_free = nil;
  OCB128_init: TOCB128_init = nil;
  OCB128_setiv: TOCB128_setiv = nil;
  OCB128_aad: TOCB128_aad = nil;
  OCB128_encrypt: TOCB128_encrypt = nil;
  OCB128_decrypt: TOCB128_decrypt = nil;
  OCB128_finish: TOCB128_finish = nil;
  OCB128_tag: TOCB128_tag = nil;
  
  // WRAP functions
  AES_wrap_key: TAES_wrap_key = nil;
  AES_unwrap_key: TAES_unwrap_key = nil;

// Load and unload functions
function LoadModesFunctions: Boolean;
procedure UnloadModesFunctions;
function IsModesLoaded: Boolean;

// High-level helper functions for GCM
function AES_GCM_Encrypt(const Key: TBytes; const IV: TBytes; const AAD: TBytes;
                        const Plaintext: TBytes; out Tag: TBytes): TBytes;
function AES_GCM_Decrypt(const Key: TBytes; const IV: TBytes; const AAD: TBytes;
                        const Ciphertext: TBytes; const Tag: TBytes): TBytes;

// High-level helper functions for CCM
function AES_CCM_Encrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Plaintext: TBytes; TagSize: Integer; out Tag: TBytes): TBytes;
function AES_CCM_Decrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Ciphertext: TBytes; const Tag: TBytes): TBytes;

// High-level helper functions for XTS (disk encryption)
function AES_XTS_Encrypt(const Key1, Key2: TBytes; const Tweak: TBytes;
                        const Plaintext: TBytes): TBytes;
function AES_XTS_Decrypt(const Key1, Key2: TBytes; const Tweak: TBytes;
                        const Ciphertext: TBytes): TBytes;

// High-level helper functions for OCB
function AES_OCB_Encrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Plaintext: TBytes; out Tag: TBytes): TBytes;
function AES_OCB_Decrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Ciphertext: TBytes; const Tag: TBytes): TBytes;

// Key wrapping functions
function AES_WrapKey(const KEK: TBytes; const Plaintext: TBytes): TBytes;
function AES_UnwrapKey(const KEK: TBytes; const Ciphertext: TBytes): TBytes;

implementation

uses
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF},
  fafafa.ssl.openssl.aes;

var
  hCrypto: {$IFDEF WINDOWS}HMODULE{$ELSE}THandle{$ENDIF} = 0;
  ModesLoaded: Boolean = False;

function LoadModesFunctions: Boolean;
begin
  Result := False;
  
  // Load crypto library if not already loaded
  if hCrypto = 0 then
  begin
    {$IFDEF WINDOWS}
    hCrypto := LoadLibrary('libcrypto-3-x64.dll');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libcrypto-1_1-x64.dll');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libeay32.dll');
    {$ELSE}
    hCrypto := LoadLibrary('libcrypto.so.3');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libcrypto.so.1.1');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libcrypto.so');
    {$ENDIF}
  end;
  
  if hCrypto = 0 then
    Exit;
    
  // Load GCM mode functions
  GCM128_new := TGCM128_new(GetProcAddress(hCrypto, 'CRYPTO_gcm128_new'));
  GCM128_free := TGCM128_free(GetProcAddress(hCrypto, 'CRYPTO_gcm128_release'));
  GCM128_setiv := TGCM128_setiv(GetProcAddress(hCrypto, 'CRYPTO_gcm128_setiv'));
  GCM128_aad := TGCM128_aad(GetProcAddress(hCrypto, 'CRYPTO_gcm128_aad'));
  GCM128_encrypt := TGCM128_encrypt(GetProcAddress(hCrypto, 'CRYPTO_gcm128_encrypt'));
  GCM128_decrypt := TGCM128_decrypt(GetProcAddress(hCrypto, 'CRYPTO_gcm128_decrypt'));
  GCM128_finish := TGCM128_finish(GetProcAddress(hCrypto, 'CRYPTO_gcm128_finish'));
  GCM128_tag := TGCM128_tag(GetProcAddress(hCrypto, 'CRYPTO_gcm128_tag'));
  
  // Load CCM mode functions
  CCM128_new := TCCM128_new(GetProcAddress(hCrypto, 'CRYPTO_ccm128_new'));
  CCM128_free := TCCM128_free(GetProcAddress(hCrypto, 'CRYPTO_ccm128_release'));
  CCM128_init := TCCM128_init(GetProcAddress(hCrypto, 'CRYPTO_ccm128_init'));
  CCM128_setiv := TCCM128_setiv(GetProcAddress(hCrypto, 'CRYPTO_ccm128_setiv'));
  CCM128_aad := TCCM128_aad(GetProcAddress(hCrypto, 'CRYPTO_ccm128_aad'));
  CCM128_encrypt := TCCM128_encrypt(GetProcAddress(hCrypto, 'CRYPTO_ccm128_encrypt'));
  CCM128_decrypt := TCCM128_decrypt(GetProcAddress(hCrypto, 'CRYPTO_ccm128_decrypt'));
  CCM128_tag := TCCM128_tag(GetProcAddress(hCrypto, 'CRYPTO_ccm128_tag'));
  
  // Load XTS mode functions
  XTS128_encrypt := TXTS128_encrypt(GetProcAddress(hCrypto, 'CRYPTO_xts128_encrypt'));
  XTS128_decrypt := TXTS128_decrypt(GetProcAddress(hCrypto, 'CRYPTO_xts128_decrypt'));
  
  // Load OCB mode functions  
  OCB128_new := TOCB128_new(GetProcAddress(hCrypto, 'CRYPTO_ocb128_new'));
  OCB128_free := TOCB128_free(GetProcAddress(hCrypto, 'CRYPTO_ocb128_release'));
  OCB128_init := TOCB128_init(GetProcAddress(hCrypto, 'CRYPTO_ocb128_init'));
  OCB128_setiv := TOCB128_setiv(GetProcAddress(hCrypto, 'CRYPTO_ocb128_setiv'));
  OCB128_aad := TOCB128_aad(GetProcAddress(hCrypto, 'CRYPTO_ocb128_aad'));
  OCB128_encrypt := TOCB128_encrypt(GetProcAddress(hCrypto, 'CRYPTO_ocb128_encrypt'));
  OCB128_decrypt := TOCB128_decrypt(GetProcAddress(hCrypto, 'CRYPTO_ocb128_decrypt'));
  OCB128_finish := TOCB128_finish(GetProcAddress(hCrypto, 'CRYPTO_ocb128_finish'));
  OCB128_tag := TOCB128_tag(GetProcAddress(hCrypto, 'CRYPTO_ocb128_tag'));
  
  // Load key wrapping functions
  AES_wrap_key := TAES_wrap_key(GetProcAddress(hCrypto, 'AES_wrap_key'));
  AES_unwrap_key := TAES_unwrap_key(GetProcAddress(hCrypto, 'AES_unwrap_key'));
  
  // Note: Most mode operations are typically done through EVP interface
  // Direct mode functions may not be exported in all OpenSSL versions
  ModesLoaded := True; // Even if individual functions are nil
  Result := ModesLoaded;
end;

procedure UnloadModesFunctions;
begin
  // Clear GCM functions
  GCM128_new := nil;
  GCM128_free := nil;
  GCM128_setiv := nil;
  GCM128_aad := nil;
  GCM128_encrypt := nil;
  GCM128_decrypt := nil;
  GCM128_finish := nil;
  GCM128_tag := nil;
  
  // Clear CCM functions
  CCM128_new := nil;
  CCM128_free := nil;
  CCM128_init := nil;
  CCM128_setiv := nil;
  CCM128_aad := nil;
  CCM128_encrypt := nil;
  CCM128_decrypt := nil;
  CCM128_tag := nil;
  
  // Clear XTS functions
  XTS128_encrypt := nil;
  XTS128_decrypt := nil;
  
  // Clear OCB functions
  OCB128_new := nil;
  OCB128_free := nil;
  OCB128_init := nil;
  OCB128_setiv := nil;
  OCB128_aad := nil;
  OCB128_encrypt := nil;
  OCB128_decrypt := nil;
  OCB128_finish := nil;
  OCB128_tag := nil;
  
  // Clear wrap functions
  AES_wrap_key := nil;
  AES_unwrap_key := nil;
  
  ModesLoaded := False;
  
  if hCrypto <> 0 then
  begin
    FreeLibrary(hCrypto);
    hCrypto := 0;
  end;
end;

function IsModesLoaded: Boolean;
begin
  Result := ModesLoaded;
end;

// High-level helper function implementations

function AES_GCM_Encrypt(const Key: TBytes; const IV: TBytes; const AAD: TBytes;
                        const Plaintext: TBytes; out Tag: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  outlen, finlen: Integer;
begin
  // Load AES functions if needed
  if not Assigned(EVP_aes_128_gcm) then
    LoadAESFunctions;
    
  // Select cipher based on key size
  case Length(Key) of
    16: cipher := EVP_aes_128_gcm();
    24: cipher := EVP_aes_192_gcm();
    32: cipher := EVP_aes_256_gcm();
  else
    raise Exception.Create('Invalid key size for AES-GCM');
  end;
  
  SetLength(Result, Length(Plaintext));
  SetLength(Tag, GCM_TAG_SIZE);
  
  ctx := EVP_CIPHER_CTX_new();
  try
    // Initialize encryption
    if EVP_EncryptInit_ex(ctx, cipher, nil, nil, nil) <> 1 then
      raise Exception.Create('Failed to initialize AES-GCM');
      
    // Set IV length if not default
    if Length(IV) <> 12 then
    begin
      if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, Length(IV), nil) <> 1 then
        raise Exception.Create('Failed to set IV length');
    end;
    
    // Initialize with key and IV
    if EVP_EncryptInit_ex(ctx, nil, nil, @Key[0], @IV[0]) <> 1 then
      raise Exception.Create('Failed to set key and IV');
      
    // Process AAD if provided
    if Length(AAD) > 0 then
    begin
      if EVP_EncryptUpdate(ctx, nil, @outlen, @AAD[0], Length(AAD)) <> 1 then
        raise Exception.Create('Failed to process AAD');
    end;
    
    // Encrypt plaintext
    if EVP_EncryptUpdate(ctx, @Result[0], @outlen, @Plaintext[0], Length(Plaintext)) <> 1 then
      raise Exception.Create('Failed to encrypt');
      
    // Finalize
    if EVP_EncryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Failed to finalize encryption');
      
    SetLength(Result, outlen + finlen);
    
    // Get tag
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, GCM_TAG_SIZE, @Tag[0]) <> 1 then
      raise Exception.Create('Failed to get tag');
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_GCM_Decrypt(const Key: TBytes; const IV: TBytes; const AAD: TBytes;
                        const Ciphertext: TBytes; const Tag: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  outlen, finlen: Integer;
begin
  // Load AES functions if needed
  if not Assigned(EVP_aes_128_gcm) then
    LoadAESFunctions;
    
  // Select cipher based on key size
  case Length(Key) of
    16: cipher := EVP_aes_128_gcm();
    24: cipher := EVP_aes_192_gcm();
    32: cipher := EVP_aes_256_gcm();
  else
    raise Exception.Create('Invalid key size for AES-GCM');
  end;
  
  if Length(Tag) <> GCM_TAG_SIZE then
    raise Exception.Create('Invalid tag size');
  
  SetLength(Result, Length(Ciphertext));
  
  ctx := EVP_CIPHER_CTX_new();
  try
    // Initialize decryption
    if EVP_DecryptInit_ex(ctx, cipher, nil, nil, nil) <> 1 then
      raise Exception.Create('Failed to initialize AES-GCM');
      
    // Set IV length if not default
    if Length(IV) <> 12 then
    begin
      if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, Length(IV), nil) <> 1 then
        raise Exception.Create('Failed to set IV length');
    end;
    
    // Initialize with key and IV
    if EVP_DecryptInit_ex(ctx, nil, nil, @Key[0], @IV[0]) <> 1 then
      raise Exception.Create('Failed to set key and IV');
      
    // Process AAD if provided
    if Length(AAD) > 0 then
    begin
      if EVP_DecryptUpdate(ctx, nil, @outlen, @AAD[0], Length(AAD)) <> 1 then
        raise Exception.Create('Failed to process AAD');
    end;
    
    // Decrypt ciphertext
    if EVP_DecryptUpdate(ctx, @Result[0], @outlen, @Ciphertext[0], Length(Ciphertext)) <> 1 then
      raise Exception.Create('Failed to decrypt');
    
    // Set expected tag
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, GCM_TAG_SIZE, @Tag[0]) <> 1 then
      raise Exception.Create('Failed to set tag');
      
    // Verify tag and finalize
    if EVP_DecryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Authentication verification failed');
      
    SetLength(Result, outlen + finlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_CCM_Encrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Plaintext: TBytes; TagSize: Integer; out Tag: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  outlen, finlen: Integer;
begin
  // Load AES functions if needed
  if not Assigned(EVP_aes_128_ccm) then
    LoadAESFunctions;
    
  // Select cipher based on key size
  case Length(Key) of
    16: cipher := EVP_aes_128_ccm();
    24: cipher := EVP_aes_192_ccm();
    32: cipher := EVP_aes_256_ccm();
  else
    raise Exception.Create('Invalid key size for AES-CCM');
  end;
  
  if (TagSize < CCM_MIN_TAG_SIZE) or (TagSize > CCM_MAX_TAG_SIZE) then
    raise Exception.Create('Invalid tag size for CCM');
  
  SetLength(Result, Length(Plaintext));
  SetLength(Tag, TagSize);
  
  ctx := EVP_CIPHER_CTX_new();
  try
    // Initialize encryption
    if EVP_EncryptInit_ex(ctx, cipher, nil, nil, nil) <> 1 then
      raise Exception.Create('Failed to initialize AES-CCM');
    
    // Set nonce length
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_IVLEN, Length(Nonce), nil) <> 1 then
      raise Exception.Create('Failed to set nonce length');
    
    // Set tag length
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_TAG, TagSize, nil) <> 1 then
      raise Exception.Create('Failed to set tag length');
    
    // Initialize with key and nonce
    if EVP_EncryptInit_ex(ctx, nil, nil, @Key[0], @Nonce[0]) <> 1 then
      raise Exception.Create('Failed to set key and nonce');
    
    // Set message length (required for CCM)
    if EVP_EncryptUpdate(ctx, nil, @outlen, nil, Length(Plaintext)) <> 1 then
      raise Exception.Create('Failed to set message length');
    
    // Process AAD if provided
    if Length(AAD) > 0 then
    begin
      if EVP_EncryptUpdate(ctx, nil, @outlen, @AAD[0], Length(AAD)) <> 1 then
        raise Exception.Create('Failed to process AAD');
    end;
    
    // Encrypt plaintext
    if EVP_EncryptUpdate(ctx, @Result[0], @outlen, @Plaintext[0], Length(Plaintext)) <> 1 then
      raise Exception.Create('Failed to encrypt');
    
    // Finalize (no output for CCM)
    if EVP_EncryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Failed to finalize encryption');
    
    // Get tag
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_GET_TAG, TagSize, @Tag[0]) <> 1 then
      raise Exception.Create('Failed to get tag');
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_CCM_Decrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Ciphertext: TBytes; const Tag: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  outlen: Integer;
begin
  // Load AES functions if needed
  if not Assigned(EVP_aes_128_ccm) then
    LoadAESFunctions;
    
  // Select cipher based on key size
  case Length(Key) of
    16: cipher := EVP_aes_128_ccm();
    24: cipher := EVP_aes_192_ccm();
    32: cipher := EVP_aes_256_ccm();
  else
    raise Exception.Create('Invalid key size for AES-CCM');
  end;
  
  SetLength(Result, Length(Ciphertext));
  
  ctx := EVP_CIPHER_CTX_new();
  try
    // Initialize decryption
    if EVP_DecryptInit_ex(ctx, cipher, nil, nil, nil) <> 1 then
      raise Exception.Create('Failed to initialize AES-CCM');
    
    // Set nonce length
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_IVLEN, Length(Nonce), nil) <> 1 then
      raise Exception.Create('Failed to set nonce length');
    
    // Set tag
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_TAG, Length(Tag), @Tag[0]) <> 1 then
      raise Exception.Create('Failed to set tag');
    
    // Initialize with key and nonce
    if EVP_DecryptInit_ex(ctx, nil, nil, @Key[0], @Nonce[0]) <> 1 then
      raise Exception.Create('Failed to set key and nonce');
    
    // Set message length (required for CCM)
    if EVP_DecryptUpdate(ctx, nil, @outlen, nil, Length(Ciphertext)) <> 1 then
      raise Exception.Create('Failed to set message length');
    
    // Process AAD if provided
    if Length(AAD) > 0 then
    begin
      if EVP_DecryptUpdate(ctx, nil, @outlen, @AAD[0], Length(AAD)) <> 1 then
        raise Exception.Create('Failed to process AAD');
    end;
    
    // Decrypt and verify
    if EVP_DecryptUpdate(ctx, @Result[0], @outlen, @Ciphertext[0], Length(Ciphertext)) <= 0 then
      raise Exception.Create('Decryption or authentication failed');
      
    SetLength(Result, outlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_XTS_Encrypt(const Key1, Key2: TBytes; const Tweak: TBytes;
                        const Plaintext: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  combined_key: TBytes;
  outlen, finlen: Integer;
begin
  // XTS requires two keys
  if Length(Key1) <> Length(Key2) then
    raise Exception.Create('XTS requires two keys of same size');
    
  // Combine keys for XTS
  SetLength(combined_key, Length(Key1) + Length(Key2));
  Move(Key1[0], combined_key[0], Length(Key1));
  Move(Key2[0], combined_key[Length(Key1)], Length(Key2));
  
  // Load AES XTS cipher
  if not Assigned(EVP_aes_128_xts) then
    LoadAESFunctions;
    
  case Length(Key1) of
    16: cipher := EVP_aes_128_xts();  // Total key = 256 bits
    32: cipher := EVP_aes_256_xts();  // Total key = 512 bits
  else
    raise Exception.Create('Invalid key size for AES-XTS');
  end;
  
  SetLength(Result, Length(Plaintext));
  
  ctx := EVP_CIPHER_CTX_new();
  try
    if EVP_EncryptInit_ex(ctx, cipher, nil, @combined_key[0], @Tweak[0]) <> 1 then
      raise Exception.Create('Failed to initialize AES-XTS');
      
    if EVP_EncryptUpdate(ctx, @Result[0], @outlen, @Plaintext[0], Length(Plaintext)) <> 1 then
      raise Exception.Create('Failed to encrypt with AES-XTS');
      
    if EVP_EncryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Failed to finalize AES-XTS encryption');
      
    SetLength(Result, outlen + finlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_XTS_Decrypt(const Key1, Key2: TBytes; const Tweak: TBytes;
                        const Ciphertext: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  combined_key: TBytes;
  outlen, finlen: Integer;
begin
  // XTS requires two keys
  if Length(Key1) <> Length(Key2) then
    raise Exception.Create('XTS requires two keys of same size');
    
  // Combine keys for XTS
  SetLength(combined_key, Length(Key1) + Length(Key2));
  Move(Key1[0], combined_key[0], Length(Key1));
  Move(Key2[0], combined_key[Length(Key1)], Length(Key2));
  
  // Load AES XTS cipher
  if not Assigned(EVP_aes_128_xts) then
    LoadAESFunctions;
    
  case Length(Key1) of
    16: cipher := EVP_aes_128_xts();  // Total key = 256 bits
    32: cipher := EVP_aes_256_xts();  // Total key = 512 bits
  else
    raise Exception.Create('Invalid key size for AES-XTS');
  end;
  
  SetLength(Result, Length(Ciphertext));
  
  ctx := EVP_CIPHER_CTX_new();
  try
    if EVP_DecryptInit_ex(ctx, cipher, nil, @combined_key[0], @Tweak[0]) <> 1 then
      raise Exception.Create('Failed to initialize AES-XTS');
      
    if EVP_DecryptUpdate(ctx, @Result[0], @outlen, @Ciphertext[0], Length(Ciphertext)) <> 1 then
      raise Exception.Create('Failed to decrypt with AES-XTS');
      
    if EVP_DecryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Failed to finalize AES-XTS decryption');
      
    SetLength(Result, outlen + finlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_OCB_Encrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Plaintext: TBytes; out Tag: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  outlen, finlen: Integer;
begin
  // Load AES functions if needed
  if not Assigned(EVP_aes_128_ocb) then
    LoadAESFunctions;
    
  // Select cipher based on key size
  case Length(Key) of
    16: cipher := EVP_aes_128_ocb();
    24: cipher := EVP_aes_192_ocb();
    32: cipher := EVP_aes_256_ocb();
  else
    raise Exception.Create('Invalid key size for AES-OCB');
  end;
  
  SetLength(Result, Length(Plaintext) + OCB_TAG_SIZE); // OCB may expand
  SetLength(Tag, OCB_TAG_SIZE);
  
  ctx := EVP_CIPHER_CTX_new();
  try
    // Initialize encryption
    if EVP_EncryptInit_ex(ctx, cipher, nil, nil, nil) <> 1 then
      raise Exception.Create('Failed to initialize AES-OCB');
      
    // Set nonce length if not default
    if Length(Nonce) <> 12 then
    begin
      if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, Length(Nonce), nil) <> 1 then
        raise Exception.Create('Failed to set nonce length');
    end;
    
    // Set tag length
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_OCB_SET_TAGLEN, OCB_TAG_SIZE, nil) <> 1 then
      raise Exception.Create('Failed to set tag length');
    
    // Initialize with key and nonce
    if EVP_EncryptInit_ex(ctx, nil, nil, @Key[0], @Nonce[0]) <> 1 then
      raise Exception.Create('Failed to set key and nonce');
      
    // Process AAD if provided
    if Length(AAD) > 0 then
    begin
      if EVP_EncryptUpdate(ctx, nil, @outlen, @AAD[0], Length(AAD)) <> 1 then
        raise Exception.Create('Failed to process AAD');
    end;
    
    // Encrypt plaintext
    if EVP_EncryptUpdate(ctx, @Result[0], @outlen, @Plaintext[0], Length(Plaintext)) <> 1 then
      raise Exception.Create('Failed to encrypt');
      
    // Finalize
    if EVP_EncryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Failed to finalize encryption');
      
    SetLength(Result, outlen + finlen);
    
    // Get tag
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_GET_TAG, OCB_TAG_SIZE, @Tag[0]) <> 1 then
      raise Exception.Create('Failed to get tag');
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_OCB_Decrypt(const Key: TBytes; const Nonce: TBytes; const AAD: TBytes;
                        const Ciphertext: TBytes; const Tag: TBytes): TBytes;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  outlen, finlen: Integer;
begin
  // Load AES functions if needed
  if not Assigned(EVP_aes_128_ocb) then
    LoadAESFunctions;
    
  // Select cipher based on key size
  case Length(Key) of
    16: cipher := EVP_aes_128_ocb();
    24: cipher := EVP_aes_192_ocb();
    32: cipher := EVP_aes_256_ocb();
  else
    raise Exception.Create('Invalid key size for AES-OCB');
  end;
  
  if Length(Tag) <> OCB_TAG_SIZE then
    raise Exception.Create('Invalid tag size');
  
  SetLength(Result, Length(Ciphertext));
  
  ctx := EVP_CIPHER_CTX_new();
  try
    // Initialize decryption
    if EVP_DecryptInit_ex(ctx, cipher, nil, nil, nil) <> 1 then
      raise Exception.Create('Failed to initialize AES-OCB');
      
    // Set nonce length if not default
    if Length(Nonce) <> 12 then
    begin
      if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, Length(Nonce), nil) <> 1 then
        raise Exception.Create('Failed to set nonce length');
    end;
    
    // Initialize with key and nonce
    if EVP_DecryptInit_ex(ctx, nil, nil, @Key[0], @Nonce[0]) <> 1 then
      raise Exception.Create('Failed to set key and nonce');
      
    // Set expected tag
    if EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_TAG, OCB_TAG_SIZE, @Tag[0]) <> 1 then
      raise Exception.Create('Failed to set tag');
      
    // Process AAD if provided
    if Length(AAD) > 0 then
    begin
      if EVP_DecryptUpdate(ctx, nil, @outlen, @AAD[0], Length(AAD)) <> 1 then
        raise Exception.Create('Failed to process AAD');
    end;
    
    // Decrypt ciphertext
    if EVP_DecryptUpdate(ctx, @Result[0], @outlen, @Ciphertext[0], Length(Ciphertext)) <> 1 then
      raise Exception.Create('Failed to decrypt');
      
    // Verify tag and finalize
    if EVP_DecryptFinal_ex(ctx, @Result[outlen], @finlen) <> 1 then
      raise Exception.Create('Authentication verification failed');
      
    SetLength(Result, outlen + finlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function AES_WrapKey(const KEK: TBytes; const Plaintext: TBytes): TBytes;
begin
  if not Assigned(AES_wrap_key) then
    raise Exception.Create('AES key wrap not available');
    
  if Length(Plaintext) mod 8 <> 0 then
    raise Exception.Create('Plaintext must be multiple of 8 bytes');
    
  SetLength(Result, Length(Plaintext) + 8); // Wrapped key is 8 bytes larger
  
  if AES_wrap_key(@KEK[0], nil, @Result[0], @Plaintext[0], Length(Plaintext)) <= 0 then
    raise Exception.Create('AES key wrap failed');
end;

function AES_UnwrapKey(const KEK: TBytes; const Ciphertext: TBytes): TBytes;
begin
  if not Assigned(AES_unwrap_key) then
    raise Exception.Create('AES key unwrap not available');
    
  if Length(Ciphertext) mod 8 <> 0 then
    raise Exception.Create('Ciphertext must be multiple of 8 bytes');
    
  if Length(Ciphertext) < 16 then
    raise Exception.Create('Ciphertext too short');
    
  SetLength(Result, Length(Ciphertext) - 8); // Unwrapped key is 8 bytes smaller
  
  if AES_unwrap_key(@KEK[0], nil, @Result[0], @Ciphertext[0], Length(Ciphertext)) <= 0 then
    raise Exception.Create('AES key unwrap failed or authentication failed');
end;

initialization
  
finalization
  UnloadModesFunctions;
  
end.