{**
 * 诊断测试: 同时导入 context 和 connection
 *}

program test_context_connection;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.connection;

begin
  WriteLn('SUCCESS: context and connection imported');
end.
