{**
 * 诊断测试 Step 3e: 导入 factory 然后 winssl.lib
 *}

program test_winssl_step3e;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 3e: Program started with factory + winssl.lib');
  WriteLn('SUCCESS');
  Halt(0);
end.
