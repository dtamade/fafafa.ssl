program test_tsa_api;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl,
  fafafa.ssl.openssl.lib,
  fafafa.ssl.openssl.api.ts,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.obj;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;

procedure AssertTrue(const Msg: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(Format('[TEST] %s... ', [Msg]));
  if Condition then
  begin
    WriteLn('✓ PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('✗ FAIL');
  end;
end;

procedure AssertNotNil(const Msg: string; P: Pointer);
begin
  AssertTrue(Msg, P <> nil);
end;

procedure TestCreateTimestampRequest;
var
  Req: PTS_REQ;
  Data: TBytes;
  PolicyOID: string;
begin
  WriteLn('=== Test CreateTimestampRequest ===');
  
  // Prepare data
  Data := TBytes.Create(1, 2, 3, 4, 5);
  PolicyOID := '1.2.3.4.5';
  
  // Create request
  Req := CreateTimestampRequest(Data, PolicyOID);
  
  AssertNotNil('Request created', Req);
  
  if Req <> nil then
  begin
    // Cleanup
    if Assigned(TS_REQ_free) then
      TS_REQ_free(Req);
  end;
end;

var
  SSLLib: ISSLLibrary;
begin
  WriteLn('========================================');
  WriteLn('TSA API Tests');
  WriteLn('========================================');
  
  try
    // Initialize OpenSSL
    SSLLib := TSSLFactory.GetLibrary(sslOpenSSL);
    if not SSLLib.Initialize then
    begin
      WriteLn('Failed to initialize OpenSSL');
      Halt(1);
    end;
    WriteLn('OpenSSL initialized: ', SSLLib.GetVersionString);
    
    // Run tests
    TestCreateTimestampRequest;
    
    WriteLn('========================================');
    WriteLn('TSA API Test Summary');
    WriteLn('========================================');
    WriteLn(Format('Total tests: %d', [TotalTests]));
    WriteLn(Format('Passed: %d', [PassedTests]));
    WriteLn(Format('Failed: %d', [TotalTests - PassedTests]));
    
    if PassedTests = TotalTests then
      WriteLn('✅ ALL TSA API TESTS PASSED!')
    else
      Halt(1);
      
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('❌ FATAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.
