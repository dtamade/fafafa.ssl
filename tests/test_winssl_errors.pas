{**
 * 测试 winssl.errors 模块
 *}

program test_winssl_errors;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.errors;

begin
  WriteLn('winssl.errors loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
