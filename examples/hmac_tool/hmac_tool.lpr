program hmac_tool;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, Classes, StrUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.evp;

const
  PROGRAM_VERSION = '1.0.0';

type
  TOperation = (opGenerate, opVerify, opHelp);
  THashAlgorithm = (haDefault, haMD5, haSHA1, haSHA256, haSHA384, haSHA512);

var
  Operation: TOperation;
  HashAlg: THashAlgorithm;
  InputFile: string;
  OutputFile: string;
  KeyFile: string;
  HMACFile: string;
  Key: string;
  HexOutput: Boolean;
  Quiet: Boolean;

procedure PrintUsage;
begin
  WriteLn('HMAC Message Authentication Tool v', PROGRAM_VERSION);
  WriteLn('Uses OpenSSL EVP API for message authentication');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  Generate HMAC:');
  WriteLn('    hmac_tool generate -i <input> [-o <output>] -k <key> [-a <algorithm>] [--hex]');
  WriteLn;
  WriteLn('  Verify HMAC:');
  WriteLn('    hmac_tool verify -i <input> -m <hmac> -k <key> [-a <algorithm>]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -i <file>        Input file to authenticate');
  WriteLn('  -o <file>        Output file for HMAC (default: stdout)');
  WriteLn('  -k <key>         Secret key (text) or @<file> to read from file');
  WriteLn('  -m <file>        HMAC file to verify against');
  WriteLn('  -a <algorithm>   Hash algorithm: md5, sha1, sha256, sha384, sha512');
  WriteLn('                   (default: sha256)');
  WriteLn('  --hex            Output HMAC in hexadecimal format (default: base64)');
  WriteLn('  -q               Quiet mode (verify only, no output except errors)');
  WriteLn('  -h, --help       Show this help');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  # Generate HMAC for a file');
  WriteLn('  hmac_tool generate -i document.txt -k "secret_key" -o document.hmac');
  WriteLn;
  WriteLn('  # Generate with key from file and hex output');
  WriteLn('  hmac_tool generate -i data.bin -k @key.txt -a sha512 --hex');
  WriteLn;
  WriteLn('  # Verify HMAC');
  WriteLn('  hmac_tool verify -i document.txt -k "secret_key" -m document.hmac');
  WriteLn;
  WriteLn('Security Notes:');
  WriteLn('  - Use strong, randomly generated keys (at least 32 bytes for SHA-256)');
  WriteLn('  - Key length should match hash output size for optimal security');
  WriteLn('  - Store keys securely, never commit to version control');
  WriteLn('  - Use constant-time comparison when verifying HMACs in production');
end;

function GetHashAlgorithmName(Alg: THashAlgorithm): string;
begin
  case Alg of
    haMD5: Result := 'md5';
    haSHA1: Result := 'sha1';
    haSHA256: Result := 'sha256';
    haSHA384: Result := 'sha384';
    haSHA512: Result := 'sha512';
    else Result := 'sha256';
  end;
end;

function ParseHashAlgorithm(const S: string): THashAlgorithm;
var
  Lower: string;
begin
  Lower := LowerCase(S);
  if Lower = 'md5' then
    Result := haMD5
  else if Lower = 'sha1' then
    Result := haSHA1
  else if Lower = 'sha256' then
    Result := haSHA256
  else if Lower = 'sha384' then
    Result := haSHA384
  else if Lower = 'sha512' then
    Result := haSHA512
  else
    raise Exception.CreateFmt('Unknown hash algorithm: %s', [S]);
end;

procedure ParseCommandLine;
var
  I: Integer;
  Arg: string;
begin
  // Defaults
  Operation := opHelp;
  HashAlg := haDefault;
  InputFile := '';
  OutputFile := '';
  KeyFile := '';
  HMACFile := '';
  Key := '';
  HexOutput := False;
  Quiet := False;

  if ParamCount = 0 then
    Exit;

  // Parse operation
  Arg := LowerCase(ParamStr(1));
  if Arg = 'generate' then
    Operation := opGenerate
  else if Arg = 'verify' then
    Operation := opVerify
  else if (Arg = '-h') or (Arg = '--help') then
    Operation := opHelp
  else
    raise Exception.CreateFmt('Unknown operation: %s', [ParamStr(1)]);

  // Parse options
  I := 2;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);
    
    if Arg = '-i' then
    begin
      Inc(I);
      if I > ParamCount then
        raise Exception.Create('-i requires an argument');
      InputFile := ParamStr(I);
    end
    else if Arg = '-o' then
    begin
      Inc(I);
      if I > ParamCount then
        raise Exception.Create('-o requires an argument');
      OutputFile := ParamStr(I);
    end
    else if Arg = '-k' then
    begin
      Inc(I);
      if I > ParamCount then
        raise Exception.Create('-k requires an argument');
      Key := ParamStr(I);
    end
    else if Arg = '-m' then
    begin
      Inc(I);
      if I > ParamCount then
        raise Exception.Create('-m requires an argument');
      HMACFile := ParamStr(I);
    end
    else if Arg = '-a' then
    begin
      Inc(I);
      if I > ParamCount then
        raise Exception.Create('-a requires an argument');
      HashAlg := ParseHashAlgorithm(ParamStr(I));
    end
    else if Arg = '--hex' then
      HexOutput := True
    else if Arg = '-q' then
      Quiet := True
    else
      raise Exception.CreateFmt('Unknown option: %s', [Arg]);
      
    Inc(I);
  end;

  // Validate
  if Operation = opGenerate then
  begin
    if InputFile = '' then
      raise Exception.Create('Input file (-i) is required');
    if Key = '' then
      raise Exception.Create('Key (-k) is required');
  end
  else if Operation = opVerify then
  begin
    if InputFile = '' then
      raise Exception.Create('Input file (-i) is required');
    if Key = '' then
      raise Exception.Create('Key (-k) is required');
    if HMACFile = '' then
      raise Exception.Create('HMAC file (-m) is required for verification');
  end;
end;

function ReadKeyFromFile(const FileName: string): TBytes;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Result[0], FS.Size);
  finally
    FS.Free;
  end;
end;

function GetKey: TBytes;
begin
  if (Length(Key) > 1) and (Key[1] = '@') then
  begin
    // Read key from file
    Result := ReadKeyFromFile(Copy(Key, 2, Length(Key) - 1));
    if not Quiet then
      WriteLn(ErrOutput, 'Key loaded from file: ', Length(Result), ' bytes');
  end
  else
  begin
    // Use key as string
    SetLength(Result, Length(Key));
    if Length(Key) > 0 then
      Move(Key[1], Result[0], Length(Key));
  end;
end;

function ComputeHMAC(const FileName: string; const KeyData: TBytes; const Algorithm: string): TBytes;
var
  FS: TFileStream;
  Ctx: PEVP_MD_CTX;
  MD: PEVP_MD;
  MDLen: NativeUInt;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
  PKey: PEVP_PKEY;
  PKCtx: PEVP_PKEY_CTX;
begin
  SetLength(Result, 0);
  
  // Get digest algorithm
  MD := EVP_get_digestbyname(PAnsiChar(AnsiString(Algorithm)));
  if MD = nil then
    raise Exception.CreateFmt('Unknown digest algorithm: %s', [Algorithm]);

  // Create PKEY from key bytes
  PKey := EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, nil, @KeyData[0], Length(KeyData));
  if PKey = nil then
    raise Exception.Create('Failed to create HMAC key');

  try
    // Create context
    Ctx := EVP_MD_CTX_new();
    if Ctx = nil then
      raise Exception.Create('Failed to create EVP context');

    try
      // Initialize HMAC
      PKCtx := nil;
      if EVP_DigestSignInit(Ctx, PKCtx, MD, nil, PKey) <> 1 then
        raise Exception.Create('Failed to initialize HMAC');

      // Process file
      FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        repeat
          BytesRead := FS.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
          begin
            if EVP_DigestSignUpdate(Ctx, @Buffer[0], BytesRead) <> 1 then
              raise Exception.Create('Failed to update HMAC');
          end;
        until BytesRead = 0;
      finally
        FS.Free;
      end;

      // Finalize HMAC
      MDLen := 0;
      if EVP_DigestSignFinal(Ctx, nil, MDLen) <> 1 then
        raise Exception.Create('Failed to get HMAC length');

      SetLength(Result, MDLen);
      if EVP_DigestSignFinal(Ctx, @Result[0], MDLen) <> 1 then
        raise Exception.Create('Failed to finalize HMAC');

      SetLength(Result, MDLen);
    finally
      EVP_MD_CTX_free(Ctx);
    end;
  finally
    EVP_PKEY_free(PKey);
  end;
end;

function BytesToHex(const Data: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Data) do
    Result := Result + LowerCase(IntToHex(Data[I], 2));
end;

function HexToBytes(const Hex: string): TBytes;
var
  I, Len: Integer;
begin
  Len := Length(Hex);
  if (Len mod 2) <> 0 then
    raise Exception.Create('Invalid hex string length');
    
  SetLength(Result, Len div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Hex, I * 2 + 1, 2));
end;

function BytesToBase64(const Data: TBytes): string;
const
  Base64Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I, J, Len: Integer;
  B: array[0..2] of Byte;
  OutLen: Integer;
begin
  Len := Length(Data);
  if Len = 0 then
  begin
    Result := '';
    Exit;
  end;

  OutLen := ((Len + 2) div 3) * 4;
  SetLength(Result, OutLen);
  
  J := 1;
  I := 0;
  while I < Len do
  begin
    B[0] := Data[I];
    Inc(I);
    if I < Len then
      B[1] := Data[I]
    else
      B[1] := 0;
    Inc(I);
    if I < Len then
      B[2] := Data[I]
    else
      B[2] := 0;
    Inc(I);

    Result[J] := Base64Chars[(B[0] shr 2) + 1];
    Inc(J);
    Result[J] := Base64Chars[(((B[0] and $03) shl 4) or (B[1] shr 4)) + 1];
    Inc(J);
    
    if I - 2 < Len then
      Result[J] := Base64Chars[(((B[1] and $0F) shl 2) or (B[2] shr 6)) + 1]
    else
      Result[J] := '=';
    Inc(J);
    
    if I - 1 < Len then
      Result[J] := Base64Chars[(B[2] and $3F) + 1]
    else
      Result[J] := '=';
    Inc(J);
  end;
end;

function Base64ToBytes(const S: string): TBytes;
const
  Base64Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I, J, Len: Integer;
  B: array[0..3] of Integer;
  
  function CharToValue(C: Char): Integer;
  var
    K: Integer;
  begin
    Result := -1;
    for K := 1 to 64 do
      if Base64Chars[K] = C then
      begin
        Result := K - 1;
        Exit;
      end;
  end;
  
begin
  Len := Length(S);
  if (Len = 0) or ((Len mod 4) <> 0) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(Result, (Len div 4) * 3);
  
  J := 0;
  I := 1;
  while I <= Len do
  begin
    B[0] := CharToValue(S[I]);
    Inc(I);
    B[1] := CharToValue(S[I]);
    Inc(I);
    if S[I] = '=' then
      B[2] := -1
    else
      B[2] := CharToValue(S[I]);
    Inc(I);
    if S[I] = '=' then
      B[3] := -1
    else
      B[3] := CharToValue(S[I]);
    Inc(I);

    if (B[0] < 0) or (B[1] < 0) then
      raise Exception.Create('Invalid Base64 character');

    Result[J] := (B[0] shl 2) or (B[1] shr 4);
    Inc(J);
    
    if B[2] >= 0 then
    begin
      Result[J] := ((B[1] and $0F) shl 4) or (B[2] shr 2);
      Inc(J);
      
      if B[3] >= 0 then
      begin
        Result[J] := ((B[2] and $03) shl 6) or B[3];
        Inc(J);
      end;
    end;
  end;
  
  SetLength(Result, J);
end;

procedure DoGenerate;
var
  KeyData: TBytes;
  HMAC: TBytes;
  AlgName: string;
  Output: string;
  FS: TFileStream;
begin
  if not FileExists(InputFile) then
    raise Exception.CreateFmt('Input file not found: %s', [InputFile]);

  if not Quiet then
    WriteLn(ErrOutput, 'Generating HMAC...');

  KeyData := GetKey;
  if Length(KeyData) = 0 then
    raise Exception.Create('Key cannot be empty');

  if HashAlg = haDefault then
    HashAlg := haSHA256;

  AlgName := GetHashAlgorithmName(HashAlg);
  
  HMAC := ComputeHMAC(InputFile, KeyData, AlgName);

  if HexOutput then
    Output := BytesToHex(HMAC)
  else
    Output := BytesToBase64(HMAC);

  if OutputFile <> '' then
  begin
    FS := TFileStream.Create(OutputFile, fmCreate);
    try
      if Length(Output) > 0 then
        FS.WriteBuffer(Output[1], Length(Output));
    finally
      FS.Free;
    end;
    if not Quiet then
      WriteLn(ErrOutput, 'HMAC written to: ', OutputFile);
  end
  else
    WriteLn(Output);

  if not Quiet then
  begin
    WriteLn(ErrOutput);
    WriteLn(ErrOutput, 'Algorithm: ', AlgName);
    WriteLn(ErrOutput, 'HMAC size: ', Length(HMAC), ' bytes');
  end;
end;

function ConstantTimeCompare(const A, B: TBytes): Boolean;
var
  I, Diff: Integer;
begin
  if Length(A) <> Length(B) then
  begin
    Result := False;
    Exit;
  end;

  Diff := 0;
  for I := 0 to High(A) do
    Diff := Diff or (A[I] xor B[I]);

  Result := (Diff = 0);
end;

procedure DoVerify;
var
  KeyData: TBytes;
  ComputedHMAC: TBytes;
  StoredHMAC: TBytes;
  AlgName: string;
  HMACStr: string;
  FS: TFileStream;
  IsHex: Boolean;
  I: Integer;
begin
  if not FileExists(InputFile) then
    raise Exception.CreateFmt('Input file not found: %s', [InputFile]);

  if not FileExists(HMACFile) then
    raise Exception.CreateFmt('HMAC file not found: %s', [HMACFile]);

  if not Quiet then
    WriteLn(ErrOutput, 'Verifying HMAC...');

  // Read stored HMAC
  FS := TFileStream.Create(HMACFile, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(HMACStr, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(HMACStr[1], FS.Size);
  finally
    FS.Free;
  end;

  HMACStr := Trim(HMACStr);

  // Auto-detect format (hex vs base64)
  IsHex := True;
  for I := 1 to Length(HMACStr) do
  begin
    if not (HMACStr[I] in ['0'..'9', 'a'..'f', 'A'..'F']) then
    begin
      IsHex := False;
      Break;
    end;
  end;

  if IsHex then
    StoredHMAC := HexToBytes(HMACStr)
  else
    StoredHMAC := Base64ToBytes(HMACStr);

  KeyData := GetKey;
  if Length(KeyData) = 0 then
    raise Exception.Create('Key cannot be empty');

  if HashAlg = haDefault then
    HashAlg := haSHA256;

  AlgName := GetHashAlgorithmName(HashAlg);
  
  ComputedHMAC := ComputeHMAC(InputFile, KeyData, AlgName);

  if ConstantTimeCompare(ComputedHMAC, StoredHMAC) then
  begin
    if not Quiet then
      WriteLn('HMAC verification: SUCCESS');
    ExitCode := 0;
  end
  else
  begin
    if not Quiet then
      WriteLn('HMAC verification: FAILED');
    ExitCode := 1;
  end;

  if not Quiet then
  begin
    WriteLn(ErrOutput);
    WriteLn(ErrOutput, 'Algorithm: ', AlgName);
    if IsHex then
      WriteLn(ErrOutput, 'Format: Hexadecimal')
    else
      WriteLn(ErrOutput, 'Format: Base64');
  end;
end;

begin
  try
    ParseCommandLine;

    if Operation = opHelp then
    begin
      PrintUsage;
      ExitCode := 0;
      Exit;
    end;

    // Initialize OpenSSL
    if not LoadOpenSSLLibrary then
      raise Exception.Create('Failed to load OpenSSL');

    if not LoadEVP(GetCryptoLibHandle) then
      raise Exception.Create('Failed to load OpenSSL EVP functions');

    case Operation of
      opGenerate: DoGenerate;
      opVerify: DoVerify;
    end;

  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
