{**
 * 测试最小化 winssl.lib
 *}

program test_winssl_lib_test2;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib.test2;

begin
  WriteLn('Minimal winssl.lib loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
