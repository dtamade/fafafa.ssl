{**
 * 诊断测试 Step 3d: 导入 fafafa.ssl.winssl.connection
 *}

program test_winssl_step3d;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.connection;

begin
  WriteLn('Step 3d: Program started with fafafa.ssl.winssl.connection');
  WriteLn('SUCCESS');
  Halt(0);
end.
