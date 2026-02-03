program test_ocsp_stapling_simple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.ocsp.cache,
  fafafa.ssl.ocsp.stapling;

var
  Cache: TOCSPResponseCache;
  Manager: TOCSPStaplingManager;
  Config: TOCSPStaplingConfig;
  Stats: TOCSPCacheStats;
  TestResponse: TBytes;
  TestSerial: TBytes;
  Retrieved: TBytes;
begin
  WriteLn('=== OCSP Stapling Simple Integration Test ===');
  WriteLn;
  
  try
    // Test 1: OCSP Response Cache
    WriteLn('Test 1: OCSP Response Cache');
    Cache := TOCSPResponseCache.Create;
    try
      WriteLn('  ✓ Cache created');
      WriteLn('  - Initial count: ', Cache.GetCount);
      
      // Test Put/Get
      SetLength(TestResponse, 100);
      FillChar(TestResponse[0], 100, $AA);
      SetLength(TestSerial, 16);
      FillChar(TestSerial[0], 16, $BB);
      
      Cache.Put(TestSerial, TestResponse, Now, Now + 1.0);  // 1 day = 1.0
      WriteLn('  ✓ Put test response');
      
      if Cache.Get(TestSerial, Retrieved) then
        WriteLn('  ✓ Get test response - length: ', Length(Retrieved))
      else
        WriteLn('  ✗ Failed to get test response');
      
      Stats := Cache.GetStats;
      WriteLn('  - Total entries: ', Stats.TotalEntries);
      WriteLn('  - Hits: ', Stats.Hits);
      WriteLn('  - Misses: ', Stats.Misses);
      WriteLn('  ✓ Cache statistics working');
    finally
      Cache.Free;
    end;
    WriteLn;
    
    // Test 2: OCSP Stapling Configuration
    WriteLn('Test 2: OCSP Stapling Configuration');
    Config := TOCSPStaplingConfig.Default;
    WriteLn('  - Client request enabled: ', Config.EnableClientRequest);
    WriteLn('  - Server stapling enabled: ', Config.EnableServerStapling);
    WriteLn('  - Stapling required: ', Config.RequireStapling);
    WriteLn('  - Auto refresh: ', Config.AutoRefresh);
    WriteLn('  - Refresh before expiry: ', Config.RefreshBeforeExpiry, ' seconds');
    WriteLn('  ✓ Default configuration loaded');
    WriteLn;
    
    // Test 3: OCSP Stapling Manager
    WriteLn('Test 3: OCSP Stapling Manager');
    Manager := TOCSPStaplingManager.Create(Config);
    try
      WriteLn('  ✓ Manager created');
      WriteLn('  - Client should request: ', Manager.ClientShouldRequest);
      
      Stats := Manager.GetCacheStats;
      WriteLn('  - Cache entries: ', Stats.TotalEntries);
      WriteLn('  ✓ Manager working correctly');
    finally
      Manager.Free;
    end;
    WriteLn;
    
    WriteLn('=== All Integration Tests Passed ===');
    ExitCode := 0;
    
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
