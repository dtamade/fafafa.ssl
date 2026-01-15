{**
 * 简单注册测试
 *}

program test_register_simple;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 1: Modules loaded');
  try
    WriteLn('Step 2: Calling RegisterWinSSLBackend...');
    RegisterWinSSLBackend;
    WriteLn('Step 3: Registration done');
    
    WriteLn('Step 4: Checking availability...');
    if TSSLFactory.IsLibraryAvailable(sslWinSSL) then
      WriteLn('Step 5: WinSSL is available')
    else
      WriteLn('Step 5: WinSSL is NOT available');
    
    WriteLn('SUCCESS');
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
