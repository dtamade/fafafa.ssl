{**
 * 诊断测试: 创建 TWinSSLLibrary 实例
 *}

program test_create_lib;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib;

var
  LLib: ISSLLibrary;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Creating TWinSSLLibrary...');
  Flush(Output);
  
  try
    LLib := TWinSSLLibrary.Create;
    WriteLn('Step 3: TWinSSLLibrary created');
  except
    on E: Exception do
      WriteLn('Step 3: Create FAILED: ', E.Message);
  end;
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
