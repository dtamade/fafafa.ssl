{**
 * 诊断测试: 导入 winssl.lib 但不使用 TWinSSLLibrary
 *}

program test_lib_no_use;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 1: Modules imported');
  WriteLn('Step 2: Not using TWinSSLLibrary');
  WriteLn('SUCCESS');
end.
