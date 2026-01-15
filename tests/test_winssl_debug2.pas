{**
 * 详细诊断测试 - 追踪初始化过程 (with factory)
 *}

program test_winssl_debug2;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.errors,
  fafafa.ssl.logging,
  fafafa.ssl.collections,
  fafafa.ssl.factory;

begin
  WriteLn('Debug: All modules including factory loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
