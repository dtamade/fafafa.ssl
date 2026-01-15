{**
 * 测试 winssl.lib 的实现依赖
 *}

program test_winssl_lib_impl_deps;

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
  fafafa.ssl.winssl.utils,
  // 实现依赖
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('All dependencies loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
