{**
 * 详细诊断测试 - 追踪初始化过程 (with winssl.lib)
 *}

program test_winssl_debug3;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.errors,
  fafafa.ssl.logging,
  fafafa.ssl.collections,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Debug: All modules including winssl.lib loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
