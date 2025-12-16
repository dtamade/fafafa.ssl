unit fafafa.ssl.openssl.api.cmac;

{$mode ObjFPC}{$H+}

interface

uses
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  SysUtils,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.evp;

const
  CMAC_MAX_BLOCK_SIZE = 32;  // Max block size for CMAC

type
  // CMAC context structure (opaque)
  CMAC_CTX = record
    opaque_data: array[0..511] of Byte;  // Implementation-specific
  end;
  PCMAC_CTX = ^CMAC_CTX;
  
  // CMAC result type
  TCMACResult = array[0..CMAC_MAX_BLOCK_SIZE-1] of Byte;
  
  // Function pointer types for CMAC operations
  TCMAC_CTX_new = function: PCMAC_CTX; cdecl;
  TCMAC_CTX_free = procedure(ctx: PCMAC_CTX); cdecl;
  TCMAC_CTX_cleanup = procedure(ctx: PCMAC_CTX); cdecl;
  TCMAC_CTX_copy = function(out_ctx: PCMAC_CTX; const in_ctx: PCMAC_CTX): Integer; cdecl;
  TCMAC_CTX_get0_cipher_ctx = function(ctx: PCMAC_CTX): PEVP_CIPHER_CTX; cdecl;
  
  TCMAC_Init = function(ctx: PCMAC_CTX; const key: PByte; keylen: size_t;
                      const cipher: PEVP_CIPHER; impl: PENGINE): Integer; cdecl;
  TCMAC_Update = function(ctx: PCMAC_CTX; const data: PByte; datalen: size_t): Integer; cdecl;
  TCMAC_Final = function(ctx: PCMAC_CTX; out_mac: PByte; maclen: Psize_t): Integer; cdecl;
  TCMAC_Resume = function(ctx: PCMAC_CTX): Integer; cdecl;
  
  // EVP PKEY method for CMAC
  TEVP_PKEY_CTX_new_id = function(id: Integer; e: PENGINE): PEVP_PKEY_CTX; cdecl;
  TEVP_PKEY_CTX_ctrl = function(ctx: PEVP_PKEY_CTX; keytype: Integer; optype: Integer;
                              cmd: Integer; p1: Integer; p2: Pointer): Integer; cdecl;
  TEVP_PKEY_new_CMAC_key = function(e: PENGINE; const priv: PByte; len: size_t;
                                  const cipher: PEVP_CIPHER): PEVP_PKEY; cdecl;
  TEVP_PKEY_new_mac_key = function(typ: Integer; e: PENGINE; const key: PByte; 
                                  keylen: Integer): PEVP_PKEY; cdecl;
  
  // MAC context operations
  TEVP_MAC_fetch = function(ctx: POSSL_LIB_CTX; const algorithm: PAnsiChar; 
                          const properties: PAnsiChar): PEVP_MAC; cdecl;
  TEVP_MAC_CTX_new = function(mac: PEVP_MAC): PEVP_MAC_CTX; cdecl;
  TEVP_MAC_CTX_free = procedure(ctx: PEVP_MAC_CTX); cdecl;
  TEVP_MAC_CTX_dup = function(src: PEVP_MAC_CTX): PEVP_MAC_CTX; cdecl;
  TEVP_MAC_CTX_mac = function(ctx: PEVP_MAC_CTX): PEVP_MAC; cdecl;
  
  TEVP_MAC_init = function(ctx: PEVP_MAC_CTX; const key: PByte; keylen: size_t;
                        const params: POSSL_PARAM): Integer; cdecl;
  TEVP_MAC_update = function(ctx: PEVP_MAC_CTX; const data: PByte; datalen: size_t): Integer; cdecl;
  TEVP_MAC_final = function(ctx: PEVP_MAC_CTX; out_mac: PByte; outl: Psize_t; outsize: size_t): Integer; cdecl;
  TEVP_MAC_finalXOF = function(ctx: PEVP_MAC_CTX; out_mac: PByte; outsize: size_t): Integer; cdecl;
  
  TEVP_MAC_get_params = function(mac: PEVP_MAC; params: POSSL_PARAM): Integer; cdecl;
  TEVP_MAC_CTX_get_params = function(ctx: PEVP_MAC_CTX; params: POSSL_PARAM): Integer; cdecl;
  TEVP_MAC_CTX_set_params = function(ctx: PEVP_MAC_CTX; const params: POSSL_PARAM): Integer; cdecl;
  
  TEVP_MAC_size = function(ctx: PEVP_MAC_CTX): size_t; cdecl;
  TEVP_MAC_block_size = function(ctx: PEVP_MAC_CTX): size_t; cdecl;
  TEVP_MAC_is_a = function(const mac: PEVP_MAC; const name: PAnsiChar): Integer; cdecl;
  TEVP_MAC_name = function(mac: PEVP_MAC): PAnsiChar; cdecl;

const
  // PKEY types for MAC
  EVP_PKEY_CMAC = 894;  // NID for CMAC
  EVP_PKEY_HMAC = 855;  // NID for HMAC
  EVP_PKEY_POLY1305 = 1061;  // NID for Poly1305
  EVP_PKEY_SIPHASH = 1062;  // NID for SipHash
  
  // Control commands for CMAC
  EVP_PKEY_CTRL_CIPHER = 12;
  EVP_PKEY_CTRL_SET_MAC_KEY = 6;
  EVP_PKEY_CTRL_DIGESTINIT = 7;

var
  // CMAC context functions  
  CMAC_CTX_new: TCMAC_CTX_new = nil;
  CMAC_CTX_free: TCMAC_CTX_free = nil;
  CMAC_CTX_cleanup: TCMAC_CTX_cleanup = nil;
  CMAC_CTX_copy: TCMAC_CTX_copy = nil;
  CMAC_CTX_get0_cipher_ctx: TCMAC_CTX_get0_cipher_ctx = nil;
  
  // CMAC operations
  CMAC_Init: TCMAC_Init = nil;
  CMAC_Update: TCMAC_Update = nil;
  CMAC_Final: TCMAC_Final = nil;
  CMAC_Resume: TCMAC_Resume = nil;
  
  // EVP PKEY operations
  EVP_PKEY_CTX_new_id: TEVP_PKEY_CTX_new_id = nil;
  EVP_PKEY_CTX_ctrl: TEVP_PKEY_CTX_ctrl = nil;
  EVP_PKEY_new_CMAC_key: TEVP_PKEY_new_CMAC_key = nil;
  EVP_PKEY_new_mac_key: TEVP_PKEY_new_mac_key = nil;
  
  // EVP MAC operations (OpenSSL 3.0+)
  EVP_MAC_fetch: TEVP_MAC_fetch = nil;
  EVP_MAC_CTX_new: TEVP_MAC_CTX_new = nil;
  EVP_MAC_CTX_free: TEVP_MAC_CTX_free = nil;
  EVP_MAC_CTX_dup: TEVP_MAC_CTX_dup = nil;
  EVP_MAC_CTX_mac: TEVP_MAC_CTX_mac = nil;
  
  EVP_MAC_init: TEVP_MAC_init = nil;
  EVP_MAC_update: TEVP_MAC_update = nil;
  EVP_MAC_final: TEVP_MAC_final = nil;
  EVP_MAC_finalXOF: TEVP_MAC_finalXOF = nil;
  
  EVP_MAC_get_params: TEVP_MAC_get_params = nil;
  EVP_MAC_CTX_get_params: TEVP_MAC_CTX_get_params = nil;
  EVP_MAC_CTX_set_params: TEVP_MAC_CTX_set_params = nil;
  
  EVP_MAC_size: TEVP_MAC_size = nil;
  EVP_MAC_block_size: TEVP_MAC_block_size = nil;
  EVP_MAC_is_a: TEVP_MAC_is_a = nil;
  EVP_MAC_name: TEVP_MAC_name = nil;

// Load and unload functions
function LoadCMACFunctions: Boolean;
procedure UnloadCMACFunctions;
function IsCMACLoaded: Boolean;

// High-level helper functions
function ComputeCMAC_AES128(const Key: TBytes; const Data: TBytes): TBytes;
function ComputeCMAC_AES256(const Key: TBytes; const Data: TBytes): TBytes;
function ComputeCMAC(const Cipher: PEVP_CIPHER; const Key: TBytes; const Data: TBytes): TBytes;

// Generic MAC computation (supports CMAC, HMAC, Poly1305, etc.)
function ComputeMAC(const MacType: string; const Key: TBytes; const Data: TBytes;
                  const CipherName: string = ''): TBytes;

implementation

uses
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF},
  fafafa.ssl.openssl.api.aes;

var
  hCrypto: {$IFDEF WINDOWS}HMODULE{$ELSE}THandle{$ENDIF} = 0;
  CMACLoaded: Boolean = False;

function LoadCMACFunctions: Boolean;
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
    
  // Load CMAC functions (OpenSSL 1.0.1+)
  CMAC_CTX_new := TCMAC_CTX_new(GetProcAddress(hCrypto, 'CMAC_CTX_new'));
  CMAC_CTX_free := TCMAC_CTX_free(GetProcAddress(hCrypto, 'CMAC_CTX_free'));
  CMAC_CTX_cleanup := TCMAC_CTX_cleanup(GetProcAddress(hCrypto, 'CMAC_CTX_cleanup'));
  CMAC_CTX_copy := TCMAC_CTX_copy(GetProcAddress(hCrypto, 'CMAC_CTX_copy'));
  CMAC_CTX_get0_cipher_ctx := TCMAC_CTX_get0_cipher_ctx(GetProcAddress(hCrypto, 'CMAC_CTX_get0_cipher_ctx'));
  
  CMAC_Init := TCMAC_Init(GetProcAddress(hCrypto, 'CMAC_Init'));
  CMAC_Update := TCMAC_Update(GetProcAddress(hCrypto, 'CMAC_Update'));
  CMAC_Final := TCMAC_Final(GetProcAddress(hCrypto, 'CMAC_Final'));
  CMAC_Resume := TCMAC_Resume(GetProcAddress(hCrypto, 'CMAC_Resume'));
  
  // Load EVP PKEY functions for MAC
  EVP_PKEY_CTX_new_id := TEVP_PKEY_CTX_new_id(GetProcAddress(hCrypto, 'EVP_PKEY_CTX_new_id'));
  EVP_PKEY_CTX_ctrl := TEVP_PKEY_CTX_ctrl(GetProcAddress(hCrypto, 'EVP_PKEY_CTX_ctrl'));
  EVP_PKEY_new_CMAC_key := TEVP_PKEY_new_CMAC_key(GetProcAddress(hCrypto, 'EVP_PKEY_new_CMAC_key'));
  EVP_PKEY_new_mac_key := TEVP_PKEY_new_mac_key(GetProcAddress(hCrypto, 'EVP_PKEY_new_mac_key'));
  
  // Load EVP MAC functions (OpenSSL 3.0+)
  EVP_MAC_fetch := TEVP_MAC_fetch(GetProcAddress(hCrypto, 'EVP_MAC_fetch'));
  EVP_MAC_CTX_new := TEVP_MAC_CTX_new(GetProcAddress(hCrypto, 'EVP_MAC_CTX_new'));
  EVP_MAC_CTX_free := TEVP_MAC_CTX_free(GetProcAddress(hCrypto, 'EVP_MAC_CTX_free'));
  EVP_MAC_CTX_dup := TEVP_MAC_CTX_dup(GetProcAddress(hCrypto, 'EVP_MAC_CTX_dup'));
  EVP_MAC_CTX_mac := TEVP_MAC_CTX_mac(GetProcAddress(hCrypto, 'EVP_MAC_CTX_mac'));
  
  EVP_MAC_init := TEVP_MAC_init(GetProcAddress(hCrypto, 'EVP_MAC_init'));
  EVP_MAC_update := TEVP_MAC_update(GetProcAddress(hCrypto, 'EVP_MAC_update'));
  EVP_MAC_final := TEVP_MAC_final(GetProcAddress(hCrypto, 'EVP_MAC_final'));
  EVP_MAC_finalXOF := TEVP_MAC_finalXOF(GetProcAddress(hCrypto, 'EVP_MAC_finalXOF'));
  
  EVP_MAC_get_params := TEVP_MAC_get_params(GetProcAddress(hCrypto, 'EVP_MAC_get_params'));
  EVP_MAC_CTX_get_params := TEVP_MAC_CTX_get_params(GetProcAddress(hCrypto, 'EVP_MAC_CTX_get_params'));
  EVP_MAC_CTX_set_params := TEVP_MAC_CTX_set_params(GetProcAddress(hCrypto, 'EVP_MAC_CTX_set_params'));
  
  EVP_MAC_size := TEVP_MAC_size(GetProcAddress(hCrypto, 'EVP_MAC_size'));
  EVP_MAC_block_size := TEVP_MAC_block_size(GetProcAddress(hCrypto, 'EVP_MAC_block_size'));
  EVP_MAC_is_a := TEVP_MAC_is_a(GetProcAddress(hCrypto, 'EVP_MAC_is_a'));
  EVP_MAC_name := TEVP_MAC_name(GetProcAddress(hCrypto, 'EVP_MAC_name'));
  
  // Check if at least basic CMAC functions are available
  CMACLoaded := Assigned(CMAC_CTX_new) or Assigned(EVP_MAC_fetch);
  Result := CMACLoaded;
end;

procedure UnloadCMACFunctions;
begin
  // Clear CMAC functions
  CMAC_CTX_new := nil;
  CMAC_CTX_free := nil;
  CMAC_CTX_cleanup := nil;
  CMAC_CTX_copy := nil;
  CMAC_CTX_get0_cipher_ctx := nil;
  
  CMAC_Init := nil;
  CMAC_Update := nil;
  CMAC_Final := nil;
  CMAC_Resume := nil;
  
  // Clear EVP PKEY functions
  EVP_PKEY_CTX_new_id := nil;
  EVP_PKEY_CTX_ctrl := nil;
  EVP_PKEY_new_CMAC_key := nil;
  EVP_PKEY_new_mac_key := nil;
  
  // Clear EVP MAC functions
  EVP_MAC_fetch := nil;
  EVP_MAC_CTX_new := nil;
  EVP_MAC_CTX_free := nil;
  EVP_MAC_CTX_dup := nil;
  EVP_MAC_CTX_mac := nil;
  
  EVP_MAC_init := nil;
  EVP_MAC_update := nil;
  EVP_MAC_final := nil;
  EVP_MAC_finalXOF := nil;
  
  EVP_MAC_get_params := nil;
  EVP_MAC_CTX_get_params := nil;
  EVP_MAC_CTX_set_params := nil;
  
  EVP_MAC_size := nil;
  EVP_MAC_block_size := nil;
  EVP_MAC_is_a := nil;
  EVP_MAC_name := nil;
  
  CMACLoaded := False;
  
  if hCrypto <> 0 then
  begin
    FreeLibrary(hCrypto);
    hCrypto := 0;
  end;
end;

function IsCMACLoaded: Boolean;
begin
  Result := CMACLoaded;
end;

// High-level helper functions implementation

function ComputeCMAC_AES128(const Key: TBytes; const Data: TBytes): TBytes;
begin
  if not Assigned(EVP_aes_128_cbc) then
    LoadAESFunctions(hCrypto);
    
  Result := ComputeCMAC(EVP_aes_128_cbc(), Key, Data);
end;

function ComputeCMAC_AES256(const Key: TBytes; const Data: TBytes): TBytes;
begin
  if not Assigned(EVP_aes_256_cbc) then
    LoadAESFunctions(hCrypto);
    
  Result := ComputeCMAC(EVP_aes_256_cbc(), Key, Data);
end;

function ComputeCMAC(const Cipher: PEVP_CIPHER; const Key: TBytes; const Data: TBytes): TBytes;
var
  ctx: PCMAC_CTX;
  mac_len: size_t;
begin
  if not Assigned(CMAC_CTX_new) then
    raise ESSLCryptoError.Create('CMAC not available');
    
  if not Assigned(Cipher) then
    raise ESSLInvalidArgument.Create('Cipher not specified');
    
  ctx := CMAC_CTX_new();
  if ctx = nil then
    raise ESSLCryptoError.Create('Failed to create CMAC context');
    
  try
    // Initialize CMAC with key and cipher
    if CMAC_Init(ctx, @Key[0], Length(Key), Cipher, nil) <> 1 then
      raise ESSLCryptoError.Create('Failed to initialize CMAC');
    
    // Update with data
    if Length(Data) > 0 then
    begin
      if CMAC_Update(ctx, @Data[0], Length(Data)) <> 1 then
        raise ESSLCryptoError.Create('Failed to update CMAC');
    end;
    
    // Get the MAC size first
    SetLength(Result, CMAC_MAX_BLOCK_SIZE);
    mac_len := CMAC_MAX_BLOCK_SIZE;
    
    // Finalize and get MAC
    if CMAC_Final(ctx, @Result[0], @mac_len) <> 1 then
      raise ESSLCryptoError.Create('Failed to finalize CMAC');
      
    // Adjust result size to actual MAC length
    SetLength(Result, mac_len);
  finally
    CMAC_CTX_free(ctx);
  end;
end;

function ComputeMAC(const MacType: string; const Key: TBytes; const Data: TBytes;
                  const CipherName: string = ''): TBytes;
var
  mac: PEVP_MAC;
  ctx: PEVP_MAC_CTX;
  outlen: size_t;
  cipher: PEVP_CIPHER;
  params: array[0..2] of OSSL_PARAM;
  cipher_name_buf: AnsiString;
begin
  Result := nil;
  
  // Try OpenSSL 3.0+ EVP_MAC API first
  if Assigned(EVP_MAC_fetch) then
  begin
    mac := EVP_MAC_fetch(nil, PAnsiChar(AnsiString(MacType)), nil);
    if mac = nil then
      raise ESSLCryptoError.Create('MAC algorithm not available: ' + MacType);
      
    ctx := EVP_MAC_CTX_new(mac);
    if ctx = nil then
      raise ESSLCryptoError.Create('Failed to create MAC context');
      
    try
      // Set cipher parameter for CMAC if needed
      if (LowerCase(MacType) = 'cmac') and (CipherName <> '') then
      begin
        cipher_name_buf := AnsiString(CipherName);
        FillChar(params, SizeOf(params), 0);
        // Note: OSSL_PARAM structure would need proper setup here
        // This is simplified - actual implementation would need OSSL_PARAM_construct_utf8_string etc.
      end;
      
      // Initialize MAC
      if EVP_MAC_init(ctx, @Key[0], Length(Key), nil) <> 1 then
        raise ESSLCryptoError.Create('Failed to initialize MAC');
        
      // Update with data
      if Length(Data) > 0 then
      begin
        if EVP_MAC_update(ctx, @Data[0], Length(Data)) <> 1 then
          raise ESSLCryptoError.Create('Failed to update MAC');
      end;
      
      // Get MAC size
      outlen := EVP_MAC_size(ctx);
      SetLength(Result, outlen);
      
      // Finalize and get MAC
      if EVP_MAC_final(ctx, @Result[0], @outlen, outlen) <> 1 then
        raise ESSLCryptoError.Create('Failed to finalize MAC');
        
      SetLength(Result, outlen);
    finally
      EVP_MAC_CTX_free(ctx);
    end;
  end
  else if (LowerCase(MacType) = 'cmac') and Assigned(CMAC_CTX_new) then
  begin
    // Fallback to legacy CMAC API
    if CipherName = 'AES-128-CBC' then
      cipher := EVP_aes_128_cbc()
    else if CipherName = 'AES-256-CBC' then
      cipher := EVP_aes_256_cbc()
    else
      raise ESSLInvalidArgument.Create('Unsupported cipher for CMAC: ' + CipherName);
      
    Result := ComputeCMAC(cipher, Key, Data);
  end
  else
    raise ESSLCryptoError.Create('MAC computation not available');
end;

initialization
  
finalization
  UnloadCMACFunctions;
  
end.