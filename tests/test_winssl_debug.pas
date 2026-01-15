{**
 * 详细诊断测试 - 追踪初始化过程
 *}

program test_winssl_debug;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.errors,
  fafafa.ssl.logging,
  fafafa.ssl.collections;

begin
  WriteLn('Debug: All base modules loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
