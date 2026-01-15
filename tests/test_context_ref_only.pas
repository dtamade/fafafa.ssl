{**
 * 诊断测试: 引用 TWinSSLContext 类
 *}

program test_context_ref_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.connection;

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
