{**
 * 诊断测试: 导入 lib 和 factory，引用 TWinSSLLibrary
 *}

program test_lib_factory_ref;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

var
  LClass: TClass;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Getting TWinSSLLibrary class reference...');
  Flush(Output);
  
  LClass := TWinSSLLibrary;
  
  WriteLn('Step 3: Class reference obtained: ', LClass.ClassName);
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
