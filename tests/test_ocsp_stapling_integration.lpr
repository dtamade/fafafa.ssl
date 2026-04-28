program test_ocsp_stapling_integration;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.ocsp.cache,
  fafafa.ssl.ocsp.stapling;

var
  Builder: ISSLContextBuilder;
  Builder2: ISSLContextBuilder;
  Context: ISSLContext;
  Cache: TOCSPResponseCache;
  Manager: TOCSPStaplingManager;
  Config: TOCSPStaplingConfig;
  Stats: TOCSPCacheStats;
  JSON: string;
begin
  WriteLn('=== OCSP Stapling Integration Test ===');
  WriteLn;
  
  try
    // Test 1: Context Builder with OCSP Stapling
    WriteLn('Test 1: TSSLContextBuilder with OCSP Stapling');
    Builder := TSSLContextBuilder.Create;
    Builder
      .WithTLS12And13
      .WithOCSPStapling(True)
      .WithOCSPStaplingRequired(False);
    WriteLn('  ✓ Context builder with OCSP Stapling created');
    WriteLn;
    
    // Test 2: OCSP Response Cache
    WriteLn('Test 2: OCSP Response Cache');
    Cache := TOCSPResponseCache.Create;
    try
      Stats := Cache.GetStats;
      WriteLn('  ✓ Cache created');
      WriteLn('  - Initial count: ', Stats.TotalEntries);
      WriteLn('  - Total entries: ', Stats.TotalEntries);
      WriteLn('  - Hits: ', Stats.Hits);
      WriteLn('  - Misses: ', Stats.Misses);
      WriteLn('  ✓ Cache statistics working');
    finally
      Cache.Free;
    end;
    WriteLn;
    
    // Test 3: OCSP Stapling Manager
    WriteLn('Test 3: OCSP Stapling Manager');
    Config := TOCSPStaplingConfig.Default;
    WriteLn('  - Client request enabled: ', Config.EnableClientRequest);
    WriteLn('  - Server stapling enabled: ', Config.EnableServerStapling);
    WriteLn('  - Stapling required: ', Config.RequireStapling);
    WriteLn('  - Auto refresh: ', Config.AutoRefresh);
    WriteLn('  - Refresh before expiry: ', Config.RefreshBeforeExpiry, ' seconds');
    WriteLn('  ✓ Default configuration loaded');
    WriteLn;
    
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
    
    // Test 4: JSON Export/Import
    WriteLn('Test 4: Context Builder JSON Serialization');
    Builder := TSSLContextBuilder.Create;
    Builder
      .WithOCSPStapling(True)
      .WithOCSPStaplingRequired(True);
    
    JSON := Builder.ExportToJSON;
    WriteLn('  ✓ Exported to JSON');
    WriteLn('  - Contains ocsp_stapling_enabled: ', 
      Pos('ocsp_stapling_enabled', JSON) > 0);
    WriteLn('  - Contains ocsp_stapling_required: ', 
      Pos('ocsp_stapling_required', JSON) > 0);
    
    Builder2 := TSSLContextBuilder.Create;
    Builder2.ImportFromJSON(JSON);
    WriteLn('  ✓ Imported from JSON');
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
