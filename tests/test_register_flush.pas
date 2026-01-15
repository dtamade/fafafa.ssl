{**
 * 诊断测试: 调用 RegisterWinSSLBackend 并 flush 输出
 *}

program test_register_flush;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  Write('Step 1: Modules imported');
  WriteLn;
  Flush(Output);
  
  Write('Step 2: About to call RegisterWinSSLBackend');
  WriteLn;
  Flush(Output);
  
  RegisterWinSSLBackend;
  
  Write('Step 3: RegisterWinSSLBackend completed');
  WriteLn;
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
