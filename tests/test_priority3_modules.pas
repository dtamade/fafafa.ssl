program test_priority3_modules;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  // Priority 3 - Symmetric Ciphers (1 module)
  fafafa.ssl.openssl.legacy_ciphers,
  
  // Priority 3 - Advanced Features (2 modules)
  fafafa.ssl.openssl.api.async,  // Fixed!
  fafafa.ssl.openssl.api.comp,  // Fixed!
  
  // Priority 3 - Utilities (5 modules)
  fafafa.ssl.openssl.txt_db,
  fafafa.ssl.openssl.api.ui,  // Fixed in Priority 2!
  fafafa.ssl.openssl.api.dso,
  fafafa.ssl.openssl.srp;
  //fafafa.ssl.openssl.api.rand_old;  // Needs type conversions

var
  TotalTests, PassedTests: Integer;

procedure Test(const Name: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(Name + ': ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
    WriteLn('FAIL');
end;

procedure PrintSeparator;
begin
  WriteLn('================================================');
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  
  WriteLn('Testing Priority 3 Modules Compilation');
  PrintSeparator;
  
  WriteLn('Symmetric Ciphers (1 module)');
  Test('Legacy ciphers module loaded', True);
  WriteLn;
  
  WriteLn('Advanced Features (2 modules)');
  Test('Async module loaded', True);
  Test('Compression module loaded', True);
  WriteLn;
  
  WriteLn('Utilities (5 modules)');
  Test('Text database module loaded', True);
  Test('User interface module loaded', True);
  Test('DSO module loaded', True);
  Test('SRP module loaded', True);
  Test('Legacy RAND module loaded (SKIPPED - needs type conversions)', False);
  WriteLn;
  
  PrintSeparator;
  WriteLn(Format('Results: %d/%d tests passed (%.1f%%)', 
    [PassedTests, TotalTests, (PassedTests * 100.0) / TotalTests]));
  PrintSeparator;
  
  if PassedTests = TotalTests then
  begin
    WriteLn('SUCCESS: All Priority 3 modules compiled successfully!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('FAILURE: Some modules failed to compile.');
    ExitCode := 1;
  end;
end.
