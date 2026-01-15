{**
 * 测试导入 winssl.connection
 *}

program test_winssl_with_connection;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.connection;

begin
  WriteLn('All modules loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
