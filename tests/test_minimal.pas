program test_minimal;
{$mode objfpc}{$H+}
uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory
  {$IFDEF WINDOWS}
  , fafafa.ssl.winssl.lib
  {$ENDIF}
  ;

var
  LCtx: ISSLContext;
begin
  WriteLn('=== Minimal SSL Test ===');
  WriteLn('Step 1: Check available libraries...');
  
  try
    WriteLn('  Registered libraries: ', TSSLFactory.GetAvailableLibraries <> []);
    WriteLn('  WinSSL available: ', TSSLFactory.IsLibraryAvailable(sslWinSSL));
    WriteLn('  OpenSSL available: ', TSSLFactory.IsLibraryAvailable(sslOpenSSL));
  except
    on E: Exception do
      WriteLn('  ERROR: ', E.Message);
  end;
  
  WriteLn('Step 2: Try to detect best library...');
  try
    WriteLn('  Best library: ', Ord(TSSLFactory.DetectBestLibrary));
  except
    on E: Exception do
      WriteLn('  ERROR: ', E.Message);
  end;
  
  WriteLn('Step 3: Try to create context...');
  try
    LCtx := TSSLFactory.CreateContext(sslCtxClient);
    if LCtx = nil then
      WriteLn('  FAIL: Context is nil')
    else
      WriteLn('  SUCCESS: Context created');
  except
    on E: Exception do
      WriteLn('  ERROR: ', E.Message);
  end;
  
  WriteLn('=== Test Complete ===');
end.
