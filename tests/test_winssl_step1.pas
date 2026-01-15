{**
 * 诊断测试 Step 1: 只导入 fafafa.ssl.base
 *}

program test_winssl_step1;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base;

begin
  WriteLn('Step 1: Program started with fafafa.ssl.base');
  WriteLn('SUCCESS');
  Halt(0);
end.
