{**
 * 诊断测试: 只导入 connection
 *}

program test_connection_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.connection;

begin
  WriteLn('SUCCESS: connection imported');
end.
