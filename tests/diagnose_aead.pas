{$MODE DELPHI}{$H+}
{
  AEAD Modes Availability Diagnostic for OpenSSL 3.x
  
  This tool checks which AEAD cipher functions are available in the current
  OpenSSL installation.
}

program diagnose_aead;

uses
  SysUtils,
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF},
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.types;

type
  TEVP_aes_128_gcm = function: Pointer; cdecl;
  TEVP_aes_192_gcm = function: Pointer; cdecl;
  TEVP_aes_256_gcm = function: Pointer; cdecl;
  TEVP_aes_128_ccm = function: Pointer; cdecl;
  TEVP_aes_192_ccm = function: Pointer; cdecl;
  TEVP_aes_256_ccm = function: Pointer; cdecl;
  TEVP_aes_128_xts = function: Pointer; cdecl;
  TEVP_aes_256_xts = function: Pointer; cdecl;
  TEVP_aes_128_ocb = function: Pointer; cdecl;
  TEVP_aes_192_ocb = function: Pointer; cdecl;
  TEVP_aes_256_ocb = function: Pointer; cdecl;
  TEVP_chacha20 = function: Pointer; cdecl;
  TEVP_chacha20_poly1305 = function: Pointer; cdecl;
  
  TOpenSSL_version_num = function: Cardinal; cdecl;
  TOpenSSL_version = function(t: Integer): PAnsiChar; cdecl;

var
  hCrypto: THandle = 0;
  EVP_aes_128_gcm: TEVP_aes_128_gcm = nil;
  EVP_aes_192_gcm: TEVP_aes_192_gcm = nil;
  EVP_aes_256_gcm: TEVP_aes_256_gcm = nil;
  EVP_aes_128_ccm: TEVP_aes_128_ccm = nil;
  EVP_aes_192_ccm: TEVP_aes_192_ccm = nil;
  EVP_aes_256_ccm: TEVP_aes_256_ccm = nil;
  EVP_aes_128_xts: TEVP_aes_128_xts = nil;
  EVP_aes_256_xts: TEVP_aes_256_xts = nil;
  EVP_aes_128_ocb: TEVP_aes_128_ocb = nil;
  EVP_aes_192_ocb: TEVP_aes_192_ocb = nil;
  EVP_aes_256_ocb: TEVP_aes_256_ocb = nil;
  EVP_chacha20: TEVP_chacha20 = nil;
  EVP_chacha20_poly1305: TEVP_chacha20_poly1305 = nil;
  
  OpenSSL_version_num: TOpenSSL_version_num = nil;
  OpenSSL_version: TOpenSSL_version = nil;
  version: Cardinal;
  version_str: string;

procedure CheckFunction(const funcName: string; funcPtr: Pointer);
begin
  if Assigned(funcPtr) then
    WriteLn('  [+] ', funcName, ': Available')
  else
    WriteLn('  [-] ', funcName, ': NOT Available');
end;

procedure LoadAEADFunctions;
begin
  WriteLn('Loading AEAD cipher functions...');
  WriteLn;
  
  // Load AEAD cipher functions
  EVP_aes_128_gcm := TEVP_aes_128_gcm(GetProcAddress(hCrypto, 'EVP_aes_128_gcm'));
  EVP_aes_192_gcm := TEVP_aes_192_gcm(GetProcAddress(hCrypto, 'EVP_aes_192_gcm'));
  EVP_aes_256_gcm := TEVP_aes_256_gcm(GetProcAddress(hCrypto, 'EVP_aes_256_gcm'));
  
  EVP_aes_128_ccm := TEVP_aes_128_ccm(GetProcAddress(hCrypto, 'EVP_aes_128_ccm'));
  EVP_aes_192_ccm := TEVP_aes_192_ccm(GetProcAddress(hCrypto, 'EVP_aes_192_ccm'));
  EVP_aes_256_ccm := TEVP_aes_256_ccm(GetProcAddress(hCrypto, 'EVP_aes_256_ccm'));
  
  EVP_aes_128_xts := TEVP_aes_128_xts(GetProcAddress(hCrypto, 'EVP_aes_128_xts'));
  EVP_aes_256_xts := TEVP_aes_256_xts(GetProcAddress(hCrypto, 'EVP_aes_256_xts'));
  
  EVP_aes_128_ocb := TEVP_aes_128_ocb(GetProcAddress(hCrypto, 'EVP_aes_128_ocb'));
  EVP_aes_192_ocb := TEVP_aes_192_ocb(GetProcAddress(hCrypto, 'EVP_aes_192_ocb'));
  EVP_aes_256_ocb := TEVP_aes_256_ocb(GetProcAddress(hCrypto, 'EVP_aes_256_ocb'));
  
  EVP_chacha20 := TEVP_chacha20(GetProcAddress(hCrypto, 'EVP_chacha20'));
  EVP_chacha20_poly1305 := TEVP_chacha20_poly1305(GetProcAddress(hCrypto, 'EVP_chacha20_poly1305'));
end;

procedure CheckLowLevelModesFunctions;
var
  GCM128_new, CCM128_new, OCB128_new: Pointer;
begin
  WriteLn('========================================');
  WriteLn('Low-Level MODES API Check');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Checking if low-level CRYPTO_*128_* functions are available...');
  WriteLn;
  
  GCM128_new := GetProcAddress(hCrypto, 'CRYPTO_gcm128_new');
  CheckFunction('CRYPTO_gcm128_new', GCM128_new);
  
  CCM128_new := GetProcAddress(hCrypto, 'CRYPTO_ccm128_new');
  CheckFunction('CRYPTO_ccm128_new', CCM128_new);
  
  OCB128_new := GetProcAddress(hCrypto, 'CRYPTO_ocb128_new');
  CheckFunction('CRYPTO_ocb128_new', OCB128_new);
  
  WriteLn;
  if not Assigned(GCM128_new) then
  begin
    WriteLn('NOTE: Low-level MODES functions are NOT available.');
    WriteLn('      This is EXPECTED in OpenSSL 3.x.');
    WriteLn('      You must use EVP_CIPHER API instead.');
  end
  else
  begin
    WriteLn('NOTE: Low-level MODES functions ARE available.');
    WriteLn('      This suggests OpenSSL 1.1.1 or earlier.');
  end;
  WriteLn;
end;

begin
  WriteLn('========================================');
  WriteLn('AEAD Modes Availability Diagnostic');
  WriteLn('========================================');
  WriteLn;
  
  // Load crypto library directly for this diagnostic
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
  
  if hCrypto = 0 then
  begin
    WriteLn('ERROR: Could not load OpenSSL library');
    Halt(1);
  end;
  
  // Get version functions
  OpenSSL_version_num := TOpenSSL_version_num(GetProcAddress(hCrypto, 'OpenSSL_version_num'));
  if not Assigned(OpenSSL_version_num) then
    OpenSSL_version_num := TOpenSSL_version_num(GetProcAddress(hCrypto, 'SSLeay'));
  
  OpenSSL_version := TOpenSSL_version(GetProcAddress(hCrypto, 'OpenSSL_version'));
  if not Assigned(OpenSSL_version) then
    OpenSSL_version := TOpenSSL_version(GetProcAddress(hCrypto, 'SSLeay_version'));
  
  if Assigned(OpenSSL_version_num) then
    version := OpenSSL_version_num()
  else
    version := 0;
    
  if Assigned(OpenSSL_version) then
    version_str := string(OpenSSL_version(0))
  else
    version_str := 'Unknown';
  
  WriteLn('OpenSSL Version: ', version_str);
  WriteLn('Version Number:  0x', IntToHex(version, 8));
  
  if (version >= $30000000) then
    WriteLn('Detected: OpenSSL 3.x')
  else if (version >= $10101000) then
    WriteLn('Detected: OpenSSL 1.1.1')
  else
    WriteLn('Detected: Older OpenSSL version');
    
  WriteLn;
  WriteLn('========================================');
  WriteLn('Loading AEAD Functions');
  WriteLn('========================================');
  WriteLn;
  
  LoadAEADFunctions;
  
  WriteLn('========================================');
  WriteLn('AES-GCM Mode');
  WriteLn('========================================');
  CheckFunction('EVP_aes_128_gcm', @EVP_aes_128_gcm);
  CheckFunction('EVP_aes_192_gcm', @EVP_aes_192_gcm);
  CheckFunction('EVP_aes_256_gcm', @EVP_aes_256_gcm);
  WriteLn;
  
  WriteLn('========================================');
  WriteLn('AES-CCM Mode');
  WriteLn('========================================');
  CheckFunction('EVP_aes_128_ccm', @EVP_aes_128_ccm);
  CheckFunction('EVP_aes_192_ccm', @EVP_aes_192_ccm);
  CheckFunction('EVP_aes_256_ccm', @EVP_aes_256_ccm);
  WriteLn;
  
  WriteLn('========================================');
  WriteLn('AES-XTS Mode (Disk Encryption)');
  WriteLn('========================================');
  CheckFunction('EVP_aes_128_xts', @EVP_aes_128_xts);
  CheckFunction('EVP_aes_256_xts', @EVP_aes_256_xts);
  WriteLn;
  
  WriteLn('========================================');
  WriteLn('AES-OCB Mode');
  WriteLn('========================================');
  CheckFunction('EVP_aes_128_ocb', @EVP_aes_128_ocb);
  CheckFunction('EVP_aes_192_ocb', @EVP_aes_192_ocb);
  CheckFunction('EVP_aes_256_ocb', @EVP_aes_256_ocb);
  WriteLn;
  WriteLn('NOTE: OCB may not be available due to patent restrictions');
  WriteLn('      in some OpenSSL builds.');
  WriteLn;
  
  WriteLn('========================================');
  WriteLn('ChaCha20 / ChaCha20-Poly1305');
  WriteLn('========================================');
  CheckFunction('EVP_chacha20', @EVP_chacha20);
  CheckFunction('EVP_chacha20_poly1305', @EVP_chacha20_poly1305);
  WriteLn;
  
  CheckLowLevelModesFunctions;
  
  WriteLn('========================================');
  WriteLn('Summary');
  WriteLn('========================================');
  WriteLn;
  WriteLn('All AEAD cipher EVP functions have been checked.');
  WriteLn;
  WriteLn('[+] = Function is available and can be used');
  WriteLn('[-] = Function is NOT available');
  WriteLn;
  WriteLn('For OpenSSL 3.x compatibility:');
  WriteLn('  - Always use EVP_CIPHER API (EVP_aes_*_gcm, etc.)');
  WriteLn('  - DO NOT use low-level CRYPTO_*128_* functions');
  WriteLn('  - Use EVP_CIPHER_CTX_ctrl for AEAD parameters');
  WriteLn;
  
  WriteLn('Press Enter to exit...');
  ReadLn;
  
  if hCrypto <> 0 then
    FreeLibrary(hCrypto);
end.
