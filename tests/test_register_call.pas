{**
 * 诊断测试: 调用 RegisterWinSSLBackend
 *}

program test_register_call;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 1: Modules imported');
  WriteLn('Step 2: Calling RegisterWinSSLBackend...');
  RegisterWinSSLBackend;
  WriteLn('Step 3: RegisterWinSSLBackend completed');
  WriteLn('SUCCESS');
end.
