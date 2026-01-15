{**
 * 诊断测试 Step 3c: 导入 fafafa.ssl.winssl.context
 *}

program test_winssl_step3c;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.context;

begin
  WriteLn('Step 3c: Program started with fafafa.ssl.winssl.context');
  WriteLn('SUCCESS');
  Halt(0);
end.
