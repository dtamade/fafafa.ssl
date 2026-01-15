{**
 * 诊断测试: 导入 context 但不创建实例
 *}

program test_context_no_create;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.context;

var
  LClass: TClass;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Getting TWinSSLContext class reference...');
  Flush(Output);
  
  LClass := TWinSSLContext;
  
  WriteLn('Step 3: Class reference obtained: ', LClass.ClassName);
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
