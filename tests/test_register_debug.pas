{**
 * 诊断测试: 详细调试 RegisterWinSSLBackend
 *}

program test_register_debug;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.collections,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

var
  LMap: specialize IIntegerMap<Integer>;
begin
  WriteLn('Step 1: Modules imported');
  
  WriteLn('Step 2: Testing collections...');
  LMap := TMapFactory.specialize CreateIntegerMap<Integer>;
  LMap.Put(1, 100);
  WriteLn('  Map created and used successfully');
  LMap := nil;
  
  WriteLn('Step 3: Testing TSSLFactory.IsLibraryAvailable...');
  try
    if TSSLFactory.IsLibraryAvailable(sslAutoDetect) then
      WriteLn('  AutoDetect: available')
    else
      WriteLn('  AutoDetect: not available');
  except
    on E: Exception do
      WriteLn('  Exception: ', E.Message);
  end;
  
  WriteLn('Step 4: Calling RegisterWinSSLBackend...');
  try
    RegisterWinSSLBackend;
    WriteLn('Step 5: RegisterWinSSLBackend completed');
  except
    on E: Exception do
      WriteLn('Step 5: RegisterWinSSLBackend FAILED: ', E.Message);
  end;
  
  WriteLn('SUCCESS');
end.
