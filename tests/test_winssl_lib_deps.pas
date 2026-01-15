{**
 * 测试 winssl.lib 的接口依赖
 *}

program test_winssl_lib_deps;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils;

begin
  WriteLn('All interface dependencies loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
