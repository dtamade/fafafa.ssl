{**
 * 测试 WinSSL 依赖链
 *}

program test_winssl_chain;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  // winssl.lib 的依赖
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  // winssl.lib 的 implementation 依赖
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('All WinSSL dependencies loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
