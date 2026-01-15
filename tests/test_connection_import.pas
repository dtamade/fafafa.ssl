{**
 * 诊断测试: 只导入 WinSSL connection 模块
 *}

program test_connection_import;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.connection;

begin
  WriteLn('SUCCESS: fafafa.ssl.winssl.connection imported');
end.
