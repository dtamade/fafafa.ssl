{**
 * 诊断测试 Step 2: 导入 fafafa.ssl.factory
 *}

program test_winssl_step2;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory;

begin
  WriteLn('Step 2: Program started with fafafa.ssl.factory');
  WriteLn('SUCCESS');
  Halt(0);
end.
