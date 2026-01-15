{**
 * 诊断测试: 引用 TWinSSLConnection
 *}

program test_connection_ref;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.connection;

var
  LClass: TClass;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Getting TWinSSLConnection class reference...');
  Flush(Output);
  
  LClass := TWinSSLConnection;
  
  WriteLn('Step 3: Class reference obtained: ', LClass.ClassName);
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
