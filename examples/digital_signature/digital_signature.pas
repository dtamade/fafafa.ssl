{******************************************************************************}
{                                                                              }
{  fafafa.ssl - Digital Signature Tool Example                               }
{                                                                              }
{  This example demonstrates RSA digital signatures using the EVP interface.  }
{  Features:                                                                   }
{  - RSA key generation (2048/4096 bits)                                     }
{  - File signing with SHA-256                                               }
{  - Signature verification                                                   }
{  - PEM format key storage                                                   }
{                                                                              }
{******************************************************************************}

program digital_signature;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.bio;

const
  VERSION = '1.0.0';
  DEFAULT_KEY_BITS = 2048;

type
  TOperationMode = (omGenerate, omSign, omVerify, omHelp);

procedure PrintHeader;
begin
  WriteLn('========================================');
  WriteLn('Digital Signature Tool v', VERSION);
  WriteLn('========================================');
  WriteLn;
end;

procedure PrintHelp;
begin
  PrintHeader;
  WriteLn('Usage:');
  WriteLn('  Generate key pair:');
  WriteLn('    digital_signature -g <private_key> <public_key> [key_bits]');
  WriteLn('    Example: digital_signature -g private.pem public.pem 2048');
  WriteLn;
  WriteLn('  Sign file:');
  WriteLn('    digital_signature -s <file> <signature> <private_key>');
  WriteLn('    Example: digital_signature -s document.txt document.sig private.pem');
  WriteLn;
  WriteLn('  Verify signature:');
  WriteLn('    digital_signature -v <file> <signature> <public_key>');
  WriteLn('    Example: digital_signature -v document.txt document.sig public.pem');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -g  Generate RSA key pair');
  WriteLn('  -s  Sign file');
  WriteLn('  -v  Verify signature');
  WriteLn('  -h  Show help');
  WriteLn;
end;

function GenerateRSAKeyPair(const PrivateKeyFile, PublicKeyFile: string; KeyBits: Integer): Boolean;
var
  pkey_ctx: PEVP_PKEY_CTX;
  pkey: PEVP_PKEY;
  bio_priv, bio_pub: PBIO;
  rsa: PRSA;
begin
  Result := False;
  pkey := nil;
  pkey_ctx := nil;
  bio_priv := nil;
  bio_pub := nil;
  
  try
    WriteLn('Generating RSA key pair (', KeyBits, ' bits)...');
    
    // Create PKEY context for RSA key generation
    pkey_ctx := EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nil);
    if pkey_ctx = nil then
    begin
      WriteLn('ERROR: Failed to create PKEY context');
      Exit;
    end;
    
    // Initialize key generation
    if EVP_PKEY_keygen_init(pkey_ctx) <= 0 then
    begin
      WriteLn('ERROR: Failed to initialize key generation');
      Exit;
    end;
    
    // Set RSA key length
    if EVP_PKEY_CTX_ctrl(pkey_ctx, EVP_PKEY_RSA, EVP_PKEY_OP_KEYGEN,
                         EVP_PKEY_CTRL_RSA_KEYGEN_BITS, KeyBits, nil) <= 0 then
    begin
      WriteLn('ERROR: Failed to set key length');
      Exit;
    end;
    
    // Generate the key
    if EVP_PKEY_keygen(pkey_ctx, pkey) <= 0 then
    begin
      WriteLn('ERROR: Key generation failed');
      Exit;
    end;
    
    WriteLn('Key generated successfully');
    
    // Save private key
    WriteLn('Saving private key to: ', PrivateKeyFile);
    bio_priv := BIO_new_file(PAnsiChar(AnsiString(PrivateKeyFile)), 'wb');
    if bio_priv = nil then
    begin
      WriteLn('ERROR: Failed to create private key file');
      Exit;
    end;
    
    if PEM_write_bio_PrivateKey(bio_priv, pkey, nil, nil, 0, nil, nil) = 0 then
    begin
      WriteLn('ERROR: Failed to write private key');
      Exit;
    end;
    
    WriteLn('Private key saved');
    
    // Save public key
    WriteLn('Saving public key to: ', PublicKeyFile);
    bio_pub := BIO_new_file(PAnsiChar(AnsiString(PublicKeyFile)), 'wb');
    if bio_pub = nil then
    begin
      WriteLn('ERROR: Failed to create public key file');
      Exit;
    end;
    
    if PEM_write_bio_PUBKEY(bio_pub, pkey) = 0 then
    begin
      WriteLn('ERROR: Failed to write public key');
      Exit;
    end;
    
    WriteLn('Public key saved');
    
    Result := True;
    
  finally
    if bio_pub <> nil then BIO_free_all(bio_pub);
    if bio_priv <> nil then BIO_free_all(bio_priv);
    if pkey <> nil then EVP_PKEY_free(pkey);
    if pkey_ctx <> nil then EVP_PKEY_CTX_free(pkey_ctx);
  end;
end;

function SignFile(const InputFile, SignatureFile, PrivateKeyFile: string): Boolean;
var
  md_ctx: PEVP_MD_CTX;
  pkey_ctx: PEVP_PKEY_CTX;
  pkey: PEVP_PKEY;
  bio: PBIO;
  fs: TFileStream;
  buffer: array[0..4095] of Byte;
  bytes_read: Integer;
  signature: array of Byte;
  sig_len: NativeUInt;
  sig_file: TFileStream;
begin
  Result := False;
  md_ctx := nil;
  pkey := nil;
  bio := nil;
  fs := nil;
  sig_file := nil;
  pkey_ctx := nil;
  
  try
    WriteLn('Signing file...');
    WriteLn('Input: ', InputFile);
    WriteLn('Signature: ', SignatureFile);
    WriteLn('Private key: ', PrivateKeyFile);
    WriteLn;
    
    // Load private key
    bio := BIO_new_file(PAnsiChar(AnsiString(PrivateKeyFile)), 'rb');
    if bio = nil then
    begin
      WriteLn('ERROR: Failed to open private key file');
      Exit;
    end;
    
    pkey := PEM_read_bio_PrivateKey(bio, nil, nil, nil);
    if pkey = nil then
    begin
      WriteLn('ERROR: Failed to read private key');
      Exit;
    end;
    
    WriteLn('[OK] Private key loaded');
    
    // Create signing context
    md_ctx := EVP_MD_CTX_new();
    if md_ctx = nil then
    begin
      WriteLn('ERROR: Failed to create signing context');
      Exit;
    end;
    
    // Initialize signing with SHA-256
    if EVP_DigestSignInit(md_ctx, pkey_ctx, EVP_sha256(), nil, pkey) <= 0 then
    begin
      WriteLn('ERROR: Failed to initialize signing');
      Exit;
    end;
    
    // Read file and update digest
    fs := TFileStream.Create(InputFile, fmOpenRead or fmShareDenyWrite);
    repeat
      bytes_read := fs.Read(buffer[0], SizeOf(buffer));
      if bytes_read > 0 then
      begin
        if EVP_DigestSignUpdate(md_ctx, @buffer[0], bytes_read) <= 0 then
        begin
          WriteLn('ERROR: Signing update failed');
          Exit;
        end;
      end;
    until bytes_read = 0;
    
    WriteLn('[OK] File processed');
    
    // Get signature length
    sig_len := 0;
    if EVP_DigestSignFinal(md_ctx, nil, sig_len) <= 0 then
    begin
      WriteLn('ERROR: Failed to get signature length');
      Exit;
    end;
    
    // Allocate signature buffer
    SetLength(signature, sig_len);
    
    // Get signature
    if EVP_DigestSignFinal(md_ctx, @signature[0], sig_len) <= 0 then
    begin
      WriteLn('ERROR: Failed to generate signature');
      Exit;
    end;
    
    WriteLn('[OK] Signature generated successfully (', sig_len, ' bytes)');
    
    // Save signature to file
    sig_file := TFileStream.Create(SignatureFile, fmCreate);
    sig_file.WriteBuffer(signature[0], sig_len);
    
    WriteLn('>>>> Signature saved');
    
    Result := True;
    
  finally
    if sig_file <> nil then sig_file.Free;
    if fs <> nil then fs.Free;
    if md_ctx <> nil then EVP_MD_CTX_free(md_ctx);
    if pkey <> nil then EVP_PKEY_free(pkey);
    if bio <> nil then BIO_free_all(bio);
  end;
end;

function VerifyFile(const InputFile, SignatureFile, PublicKeyFile: string): Boolean;
var
  md_ctx: PEVP_MD_CTX;
  pkey_ctx: PEVP_PKEY_CTX;
  pkey: PEVP_PKEY;
  bio: PBIO;
  fs: TFileStream;
  sig_file: TFileStream;
  buffer: array[0..4095] of Byte;
  bytes_read: Integer;
  signature: array of Byte;
  sig_len: Integer;
  verify_result: Integer;
begin
  Result := False;
  md_ctx := nil;
  pkey := nil;
  bio := nil;
  fs := nil;
  sig_file := nil;
  pkey_ctx := nil;
  
  try
    WriteLn('Verifying signature...');
    WriteLn('File: ', InputFile);
    WriteLn('Signature: ', SignatureFile);
    WriteLn('Public key: ', PublicKeyFile);
    WriteLn;
    
    // Load public key
    bio := BIO_new_file(PAnsiChar(AnsiString(PublicKeyFile)), 'rb');
    if bio = nil then
    begin
      WriteLn('ERROR: Failed to open public key file');
      Exit;
    end;
    
    pkey := PEM_read_bio_PUBKEY(bio, nil, nil, nil);
    if pkey = nil then
    begin
      WriteLn('ERROR: Failed to read public key');
      Exit;
    end;
    
    WriteLn('[OK] Public key loaded');
    
    // Read signature
    sig_file := TFileStream.Create(SignatureFile, fmOpenRead or fmShareDenyWrite);
    sig_len := sig_file.Size;
    SetLength(signature, sig_len);
    sig_file.ReadBuffer(signature[0], sig_len);
    sig_file.Free;
    sig_file := nil;
    
    WriteLn('[OK] Signature file read successfully (', sig_len, ' bytes)');
    
    // Create verification context
    md_ctx := EVP_MD_CTX_new();
    if md_ctx = nil then
    begin
      WriteLn('ERROR: Failed to create verification context');
      Exit;
    end;
    
    // Initialize verification with SHA-256
    if EVP_DigestVerifyInit(md_ctx, pkey_ctx, EVP_sha256(), nil, pkey) <= 0 then
    begin
      WriteLn('ERROR: Failed to initialize verification');
      Exit;
    end;
    
    // Read file and update digest
    fs := TFileStream.Create(InputFile, fmOpenRead or fmShareDenyWrite);
    repeat
      bytes_read := fs.Read(buffer[0], SizeOf(buffer));
      if bytes_read > 0 then
      begin
        if EVP_DigestVerifyUpdate(md_ctx, @buffer[0], bytes_read) <= 0 then
        begin
          WriteLn('ERROR: Verification update failed');
          Exit;
        end;
      end;
    until bytes_read = 0;
    
    WriteLn('[OK] File processed');
    
    // Verify signature
    verify_result := EVP_DigestVerifyFinal(md_ctx, @signature[0], sig_len);
    
    if verify_result = 1 then
    begin
      WriteLn;
      WriteLn('Signature verified successfully! File is authentic and unmodified.');
      Result := True;
    end
    else if verify_result = 0 then
    begin
      WriteLn;
      WriteLn('Signature verification FAILED! File may be tampered or signature mismatch.');
    end
    else
    begin
      WriteLn;
      WriteLn('ERROR: Verification process error');
    end;
    
  finally
    if sig_file <> nil then sig_file.Free;
    if fs <> nil then fs.Free;
    if md_ctx <> nil then EVP_MD_CTX_free(md_ctx);
    if pkey <> nil then EVP_PKEY_free(pkey);
    if bio <> nil then BIO_free_all(bio);
  end;
end;

procedure Main;
var
  mode: TOperationMode;
  key_bits: Integer;
begin
  PrintHeader;
  
  // Parse command line
  if ParamCount = 0 then
  begin
    PrintHelp;
    Exit;
  end;
  
  // Determine operation mode
  mode := omHelp;
  if (ParamStr(1) = '-g') or (ParamStr(1) = '--generate') then
    mode := omGenerate
  else if (ParamStr(1) = '-s') or (ParamStr(1) = '--sign') then
    mode := omSign
  else if (ParamStr(1) = '-v') or (ParamStr(1) = '--verify') then
    mode := omVerify
  else if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') then
    mode := omHelp;
  
  // Load OpenSSL
  if mode <> omHelp then
  begin
    if not LoadOpenSSLLibrary then
    begin
      WriteLn('[ERROR] Failed to load OpenSSL');
      WriteLn('Please ensure libcrypto-3-x64.dll is in system PATH');
      ExitCode := 1;
      Exit;
    end;
    WriteLn('[OK] OpenSSL loaded successfully');
    
    // Load BIO module
    LoadOpenSSLBIO;
    WriteLn('[OK] BIO module loaded');
    
    // Load EVP module for crypto operations
    if not LoadEVP(GetCryptoLibHandle) then
    begin
      WriteLn('[ERROR] Failed to load EVP module');
      ExitCode := 1;
      Exit;
    end;
    WriteLn('[OK] EVP module loaded');
    
    // Load PEM module for key I/O
    if not LoadOpenSSLPEM(GetCryptoLibHandle) then
    begin
      WriteLn('[ERROR] Failed to load PEM module');
      ExitCode := 1;
      Exit;
    end;
    WriteLn('[OK] PEM module loaded');
    WriteLn;
  end;
  
  // Execute operation
  case mode of
    omGenerate:
      begin
        if ParamCount < 3 then
        begin
          WriteLn('ERROR: Insufficient parameters');
          WriteLn('Usage: digital_signature -g <私钥文件> <公钥文件> [密钥位数]');
          ExitCode := 1;
          Exit;
        end;
        
        key_bits := DEFAULT_KEY_BITS;
        if ParamCount >= 4 then
          key_bits := StrToIntDef(ParamStr(4), DEFAULT_KEY_BITS);
        
        if GenerateRSAKeyPair(ParamStr(2), ParamStr(3), key_bits) then
        begin
          WriteLn;
          WriteLn('🎉 Key pair generated!');
          ExitCode := 0;
        end
        else
        begin
          WriteLn;
          WriteLn('[ERROR] Key pair generation failed');
          ExitCode := 1;
        end;
      end;
      
    omSign:
      begin
        if ParamCount < 4 then
        begin
          WriteLn('ERROR: Insufficient parameters');
          WriteLn('Usage: digital_signature -s <文件> <签名文件> <私钥文件>');
          ExitCode := 1;
          Exit;
        end;
        
        if SignFile(ParamStr(2), ParamStr(3), ParamStr(4)) then
        begin
          WriteLn;
          WriteLn('🎉 File signing complete!');
          ExitCode := 0;
        end
        else
        begin
          WriteLn;
          WriteLn('[ERROR] File signing failed');
          ExitCode := 1;
        end;
      end;
      
    omVerify:
      begin
        if ParamCount < 4 then
        begin
          WriteLn('ERROR: Insufficient parameters');
          WriteLn('Usage: digital_signature -v <文件> <签名文件> <公钥文件>');
          ExitCode := 1;
          Exit;
        end;
        
        if VerifyFile(ParamStr(2), ParamStr(3), ParamStr(4)) then
          ExitCode := 0
        else
          ExitCode := 1;
      end;
      
    omHelp:
      PrintHelp;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] Exception: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
