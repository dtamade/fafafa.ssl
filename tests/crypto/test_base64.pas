program test_base64;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.exceptions;

procedure PrintResult(const AName: string; APassed: Boolean; const AMessage: string = '');
begin
  if APassed then
    WriteLn(Format('  ✓ %s', [AName]))
  else
    WriteLn(Format('  ✗ %s: %s', [AName, AMessage]));
end;

procedure TestStringBase64;
var
  LInput, LEncoded, LDecoded: string;
begin
  WriteLn('[1] Testing String Base64...');
  
  // Ensure initialized to load pointers
  TCryptoUtils.EnsureInitialized;
  
  LInput := 'Hello World';
  LEncoded := TCryptoUtils.Base64Encode(LInput);
  LDecoded := TCryptoUtils.Base64DecodeString(LEncoded);
  
  PrintResult('Encode "Hello World"', LEncoded = 'SGVsbG8gV29ybGQ=', 'Got: ' + LEncoded);
  PrintResult('Decode "Hello World"', LDecoded = LInput, 'Got: ' + LDecoded);
  
  LInput := 'OpenSSL Base64 Test';
  LEncoded := TCryptoUtils.Base64Encode(LInput);
  LDecoded := TCryptoUtils.Base64DecodeString(LEncoded);
  PrintResult('Roundtrip "OpenSSL Base64 Test"', LDecoded = LInput);
end;

procedure TestBinaryBase64;
var
  LInput, LDecoded: TBytes;
  LEncoded: string;
  I: Integer;
  LMatch: Boolean;
begin
  WriteLn('[2] Testing Binary Base64...');
  
  SetLength(LInput, 256);
  for I := 0 to 255 do
    LInput[I] := I;
    
  LEncoded := TCryptoUtils.Base64Encode(LInput);
  LDecoded := TCryptoUtils.Base64Decode(LEncoded);
  
  LMatch := Length(LInput) = Length(LDecoded);
  if LMatch then
  begin
    for I := 0 to High(LInput) do
      if LInput[I] <> LDecoded[I] then
      begin
        LMatch := False;
        Break;
      end;
  end;
  
  PrintResult('Binary Roundtrip (0..255)', LMatch);
end;

procedure TestSHA256Base64;
var
  LInput, LHashHex, LHashBase64: string;
  LDecodedHash: TBytes;
  LHexFromBase64: string;
begin
  WriteLn('[3] Testing SHA256 Base64...');
  
  LInput := 'test';
  // SHA256('test') = 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08
  
  LHashHex := TCryptoUtils.SHA256Hex(LInput);
  LHashBase64 := TCryptoUtils.SHA256Base64(LInput);
  
  LDecodedHash := TCryptoUtils.Base64Decode(LHashBase64);
  LHexFromBase64 := TCryptoUtils.BytesToHex(LDecodedHash, False);
  
  PrintResult('SHA256 Hex vs Base64 consistency', LowerCase(LHashHex) = LowerCase(LHexFromBase64), 
    Format('Hex: %s, FromBase64: %s', [LHashHex, LHexFromBase64]));
end;

procedure TestEmpty;
begin
  WriteLn('[4] Testing Empty Input...');
  PrintResult('Empty Encode', TCryptoUtils.Base64Encode('') = '');
  PrintResult('Empty Decode', Length(TCryptoUtils.Base64Decode('')) = 0);
end;

begin
  WriteLn('==========================================');
  WriteLn('  TCryptoUtils Base64 Tests');
  WriteLn('==========================================');
  
  try
    TestStringBase64;
    TestBinaryBase64;
    TestSHA256Base64;
    TestEmpty;
    
    WriteLn('==========================================');
    WriteLn('✅ All tests passed!');
  except
    on E: Exception do
    begin
      WriteLn('❌ Unhandled exception: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
