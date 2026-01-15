{******************************************************************************}
{                                                                              }
{  fafafa.ssl - Password Hash Tool                                           }
{                                                                              }
{  Demonstrates secure password hashing using PBKDF2.                        }
{  Features:                                                                   }
{  - PBKDF2-HMAC-SHA256 password hashing                                     }
{  - Random salt generation                                                   }
{  - Configurable iterations                                                  }
{  - Password verification                                                    }
{                                                                              }
{******************************************************************************}

program password_hash;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Base64,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rand,
  fafafa.ssl.openssl.kdf;

const
  VERSION = '1.0.0';
  DEFAULT_ITERATIONS = 100000;  // OWASP recommendation for PBKDF2-SHA256
  SALT_LENGTH = 16;             // 128 bits
  HASH_LENGTH = 32;             // 256 bits

type
  TOperationMode = (omHash, omVerify, omHelp);
  
  TPasswordHash = record
    Algorithm: string;
    Iterations: Integer;
    Salt: TBytes;
    Hash: TBytes;
  end;

procedure PrintHeader;
begin
  WriteLn('========================================');
  WriteLn('Password Hash Tool v', VERSION);
  WriteLn('========================================');
  WriteLn;
end;

procedure PrintHelp;
begin
  PrintHeader;
  WriteLn('Usage:');
  WriteLn('  Hash password:');
  WriteLn('    password_hash -hash <password> [iterations]');
  WriteLn('    Example: password_hash -hash "MySecurePass123" 100000');
  WriteLn;
  WriteLn('  Verify password:');
  WriteLn('    password_hash -verify <password> <hash_string>');
  WriteLn('    Example: password_hash -verify "MySecurePass123" "pbkdf2:sha256:..."');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -hash     Generate password hash');
  WriteLn('  -verify   Verify password against hash');
  WriteLn('  -h, --help  Show this help');
  WriteLn;
  WriteLn('Algorithm: PBKDF2-HMAC-SHA256');
  WriteLn('Default iterations: ', DEFAULT_ITERATIONS);
  WriteLn('Salt length: ', SALT_LENGTH, ' bytes');
  WriteLn('Hash length: ', HASH_LENGTH, ' bytes');
  WriteLn;
end;

function GenerateSalt(Length: Integer): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length);
  
  // Use OpenSSL's RAND_bytes for cryptographically secure random
  if RAND_bytes(@Result[0], Length) <> 1 then
  begin
    // Fallback to system random if RAND_bytes fails
    Randomize;
    for i := 0 to Length - 1 do
      Result[i] := Random(256);
  end;
end;

function PBKDF2_SHA256(const Password: string; const Salt: TBytes; 
                       Iterations: Integer; KeyLength: Integer): TBytes;
var
  pwd_bytes: TBytes;
begin
  SetLength(Result, KeyLength);
  
  // Convert password to bytes
  pwd_bytes := TEncoding.UTF8.GetBytes(Password);
  
  // Use OpenSSL's PKCS5_PBKDF2_HMAC
  if PKCS5_PBKDF2_HMAC(
    @pwd_bytes[0], Length(pwd_bytes),
    @Salt[0], Length(Salt),
    Iterations,
    EVP_sha256(),
    KeyLength,
    @Result[0]
  ) <> 1 then
  begin
    raise Exception.Create('PBKDF2 derivation failed');
  end;
end;

function EncodePasswordHash(const PWHash: TPasswordHash): string;
var
  encoder: TBase64EncodingStream;
  salt_output, hash_output: TStringStream;
begin
  // Format: pbkdf2:sha256:iterations:salt_base64:hash_base64
  
  // Encode salt to Base64
  salt_output := TStringStream.Create('');
  try
    encoder := TBase64EncodingStream.Create(salt_output);
    try
      encoder.Write(PWHash.Salt[0], Length(PWHash.Salt));
    finally
      encoder.Free;
    end;
  finally
  end;
  
  // Encode hash to Base64
  hash_output := TStringStream.Create('');
  try
    encoder := TBase64EncodingStream.Create(hash_output);
    try
      encoder.Write(PWHash.Hash[0], Length(PWHash.Hash));
    finally
      encoder.Free;
    end;
  finally
  end;
  
  Result := Format('pbkdf2:sha256:%d:%s:%s', [
    PWHash.Iterations,
    salt_output.DataString,
    hash_output.DataString
  ]);
  
  salt_output.Free;
  hash_output.Free;
end;

function DecodePasswordHash(const HashString: string): TPasswordHash;
var
  parts: TStringArray;
  salt_input, hash_input: TStringStream;
  salt_output, hash_output: TMemoryStream;
  decoder: TBase64DecodingStream;
begin
  parts := HashString.Split([':']);
  
  if Length(parts) <> 5 then
    raise Exception.Create('Invalid hash format');
    
  if parts[0] <> 'pbkdf2' then
    raise Exception.Create('Unknown algorithm: ' + parts[0]);
    
  if parts[1] <> 'sha256' then
    raise Exception.Create('Unknown hash function: ' + parts[1]);
  
  Result.Algorithm := parts[0];
  Result.Iterations := StrToInt(parts[2]);
  
  // Decode base64 salt
  salt_input := TStringStream.Create(parts[3]);
  try
    salt_output := TMemoryStream.Create;
    try
      salt_input.Position := 0;
      decoder := TBase64DecodingStream.Create(salt_input);
      try
        salt_output.CopyFrom(decoder, decoder.Size);
        SetLength(Result.Salt, salt_output.Size);
        if salt_output.Size > 0 then
          Move(salt_output.Memory^, Result.Salt[0], salt_output.Size);
      finally
        decoder.Free;
      end;
    finally
      salt_output.Free;
    end;
  finally
    salt_input.Free;
  end;
  
  // Decode base64 hash
  hash_input := TStringStream.Create(parts[4]);
  try
    hash_output := TMemoryStream.Create;
    try
      hash_input.Position := 0;
      decoder := TBase64DecodingStream.Create(hash_input);
      try
        hash_output.CopyFrom(decoder, decoder.Size);
        SetLength(Result.Hash, hash_output.Size);
        if hash_output.Size > 0 then
          Move(hash_output.Memory^, Result.Hash[0], hash_output.Size);
      finally
        decoder.Free;
      end;
    finally
      hash_output.Free;
    end;
  finally
    hash_input.Free;
  end;
end;

function HashPassword(const Password: string; Iterations: Integer): string;
var
  pw_hash: TPasswordHash;
begin
  WriteLn('[>>] Generating password hash...');
  WriteLn('Algorithm: PBKDF2-HMAC-SHA256');
  WriteLn('Iterations: ', Iterations);
  WriteLn('Salt length: ', SALT_LENGTH, ' bytes');
  WriteLn('Hash length: ', HASH_LENGTH, ' bytes');
  WriteLn;
  
  // Generate random salt
  pw_hash.Algorithm := 'pbkdf2';
  pw_hash.Iterations := Iterations;
  pw_hash.Salt := GenerateSalt(SALT_LENGTH);
  
  WriteLn('[OK] Salt generated');
  
  // Derive key using PBKDF2
  pw_hash.Hash := PBKDF2_SHA256(Password, pw_hash.Salt, Iterations, HASH_LENGTH);
  
  WriteLn('[OK] Hash computed');
  
  // Encode to string format
  Result := EncodePasswordHash(pw_hash);
end;

function VerifyPassword(const Password, HashString: string): Boolean;
var
  stored_hash: TPasswordHash;
  computed_hash: TBytes;
  i: Integer;
  diff: Byte;
begin
  WriteLn('[>>] Verifying password...');
  
  try
    // Parse stored hash
    stored_hash := DecodePasswordHash(HashString);
    
    WriteLn('Algorithm: ', stored_hash.Algorithm, ':sha256');
    WriteLn('Iterations: ', stored_hash.Iterations);
    WriteLn('Salt length: ', Length(stored_hash.Salt), ' bytes');
    WriteLn('Hash length: ', Length(stored_hash.Hash), ' bytes');
    WriteLn;
    
    // Compute hash with same parameters
    computed_hash := PBKDF2_SHA256(
      Password,
      stored_hash.Salt,
      stored_hash.Iterations,
      Length(stored_hash.Hash)
    );
    
    WriteLn('[OK] Hash computed');
    
    // Constant-time comparison to prevent timing attacks
    diff := 0;
    if Length(computed_hash) <> Length(stored_hash.Hash) then
    begin
      Result := False;
      Exit;
    end;
    
    for i := 0 to Length(computed_hash) - 1 do
      diff := diff or (computed_hash[i] xor stored_hash.Hash[i]);
    
    Result := (diff = 0);
    
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ', E.Message);
      Result := False;
    end;
  end;
end;

procedure Main;
var
  mode: TOperationMode;
  password, hash_string: string;
  iterations: Integer;
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
  if (ParamStr(1) = '-hash') or (ParamStr(1) = '--hash') then
    mode := omHash
  else if (ParamStr(1) = '-verify') or (ParamStr(1) = '--verify') then
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
    WriteLn('[OK] OpenSSL loaded');
    
    // Load RAND module
    if not LoadOpenSSLRAND then
    begin
      WriteLn('[ERROR] Failed to load RAND module');
      ExitCode := 1;
      Exit;
    end;
    WriteLn('[OK] RAND module loaded');
    
    // Load EVP module
    if not LoadEVP(GetCryptoLibHandle) then
    begin
      WriteLn('[ERROR] Failed to load EVP module');
      ExitCode := 1;
      Exit;
    end;
    WriteLn('[OK] EVP module loaded');
    
    // Load KDF module for PBKDF2
    LoadKDFFunctions;
    WriteLn('[OK] KDF module loaded');
    WriteLn;
  end;
  
  // Execute operation
  case mode of
    omHash:
      begin
        if ParamCount < 2 then
        begin
          WriteLn('[ERROR] Insufficient parameters');
          WriteLn('Usage: password_hash -hash <password> [iterations]');
          ExitCode := 1;
          Exit;
        end;
        
        password := ParamStr(2);
        iterations := DEFAULT_ITERATIONS;
        
        if ParamCount >= 3 then
          iterations := StrToIntDef(ParamStr(3), DEFAULT_ITERATIONS);
        
        try
          hash_string := HashPassword(password, iterations);
          WriteLn;
          WriteLn('========================================');
          WriteLn('Password Hash:');
          WriteLn('========================================');
          WriteLn(hash_string);
          WriteLn('========================================');
          WriteLn;
          WriteLn('[OK] Password hashed successfully!');
          WriteLn;
          WriteLn('IMPORTANT: Store this hash securely in your database.');
          WriteLn('Never store plain-text passwords!');
          ExitCode := 0;
        except
          on E: Exception do
          begin
            WriteLn('[ERROR] ', E.Message);
            ExitCode := 1;
          end;
        end;
      end;
      
    omVerify:
      begin
        if ParamCount < 3 then
        begin
          WriteLn('[ERROR] Insufficient parameters');
          WriteLn('Usage: password_hash -verify <password> <hash_string>');
          ExitCode := 1;
          Exit;
        end;
        
        password := ParamStr(2);
        hash_string := ParamStr(3);
        
        if VerifyPassword(password, hash_string) then
        begin
          WriteLn;
          WriteLn('[OK] [OK] [OK] Password verification SUCCESSFUL!');
          WriteLn('The password matches the stored hash.');
          ExitCode := 0;
        end
        else
        begin
          WriteLn;
          WriteLn('[ERROR] [ERROR] [ERROR] Password verification FAILED!');
          WriteLn('The password does NOT match the stored hash.');
          ExitCode := 1;
        end;
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
