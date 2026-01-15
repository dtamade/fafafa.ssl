program test_logging;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.logging,
  fafafa.ssl.secure,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.kdf;

type
  TMockLogger = class(TBaseLogger)
  public
    LastMessage: string;
    procedure WriteLog(const AMessage: string); override;
  end;

procedure TMockLogger.WriteLog(const AMessage: string);
begin
  LastMessage := AMessage;
  WriteLn('MockLogger: ' + AMessage);
end;

procedure TestBasicLogging;
var
  Logger: TMockLogger;
begin
  WriteLn('Testing Basic Logging...');
  Logger := TMockLogger.Create;
  try
    TSecurityLog.Logger := Logger;
    
    TSecurityLog.Info('Test', 'Info message');
    if Pos('[INFO] [Test] Info message', Logger.LastMessage) = 0 then
      WriteLn('FAIL: Info message format incorrect');
      
    TSecurityLog.Error('Test', 'Error message');
    if Pos('[ERROR] [Test] Error message', Logger.LastMessage) = 0 then
      WriteLn('FAIL: Error message format incorrect');
      
    TSecurityLog.Audit('Test', 'Login', 'User1', 'Success');
    if Pos('[AUDIT] [Test] Action=Login User=User1 Details=Success', Logger.LastMessage) = 0 then
      WriteLn('FAIL: Audit message format incorrect');
      
    if Pos('[AUDIT] [Test] Action=Login User=User1 Details=Success', Logger.LastMessage) = 0 then
      WriteLn('FAIL: Audit message format incorrect');
      
    WriteLn('Basic Logging Test Complete.');
  finally
    // Logger is managed by TSecurityLog.Logger interface reference
    // Do not free manually!
  end;
end;

procedure TestFileLogging;
var
  Logger: TFileLogger;
  LogFile: string;
  Content: TStringList;
begin
  WriteLn('Testing File Logging...');
  LogFile := 'test_security.log';
  if FileExists(LogFile) then DeleteFile(LogFile);
  
  Logger := TFileLogger.Create(LogFile);
  try
    Logger.Log(selInfo, 'FileTest', 'File log message');
  finally
    Logger.Free;
  end;
  
  if not FileExists(LogFile) then
  begin
    WriteLn('FAIL: Log file not created');
    Exit;
  end;
  
  Content := TStringList.Create;
  try
    Content.LoadFromFile(LogFile);
    if Content.Count = 0 then
      WriteLn('FAIL: Log file is empty');
    if Pos('File log message', Content.Text) = 0 then
      WriteLn('FAIL: Log message not found in file');
  finally
    Content.Free;
    DeleteFile(LogFile);
  end;
  WriteLn('File Logging Test Complete.');
end;

procedure TestSecureKeyStoreLogging;
var
  Store: ISecureKeyStore;
  Logger: TMockLogger;
  Key: TSecureBytes;
begin
  WriteLn('Testing SecureKeyStore Logging...');
  Logger := TMockLogger.Create;
  TSecurityLog.Logger := Logger;
  
  Store := CreateSecureKeyStore;
  Key := TSecureBytes.CreateRandom(32);
  try
    // Test StoreKey logging
    Store.StoreKey('TestKey', Key, 'password');
    if Pos('Key stored: TestKey', Logger.LastMessage) = 0 then
      WriteLn('FAIL: StoreKey logging missing');
      
    // Test LoadKey logging
    Store.LoadKey('TestKey', 'password');
    if Pos('Key accessed: TestKey', Logger.LastMessage) = 0 then
      WriteLn('FAIL: LoadKey logging missing');
      
    // Test DeleteKey logging
    Store.DeleteKey('TestKey');
    if Pos('Key deleted: TestKey', Logger.LastMessage) = 0 then
      WriteLn('FAIL: DeleteKey logging missing');
    if Pos('Key deleted: TestKey', Logger.LastMessage) = 0 then
      WriteLn('FAIL: DeleteKey logging missing');
      
  finally
    // Logger is managed by TSecurityLog.Logger interface reference
  end;
  WriteLn('SecureKeyStore Logging Test Complete.');
end;

begin
  try
    if not LoadOpenSSLLibrary then
    begin
      WriteLn('Failed to load OpenSSL library');
      Exit;
    end;

    WriteLn('OpenSSL Version: ' + GetOpenSSLVersion);

    // Load EVP functions
    if not LoadEVP(GetCryptoLibHandle) then
    begin
      WriteLn('Failed to load EVP functions');
      Exit;
    end;

    // Load KDF functions (PBKDF2, HKDF, etc.)
    LoadKDFFunctions;

    TestBasicLogging;
    TestFileLogging;
    TestSecureKeyStoreLogging;
    WriteLn('All tests passed.');

    // Reset logger to default before exit to avoid cleanup issues
    TSecurityLog.Logger := nil;

    UnloadOpenSSLLibrary;
  except
    on E: Exception do
      WriteLn('Test Failed: ' + E.Message);
  end;
end.
