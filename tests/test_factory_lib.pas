{**
 * 诊断测试: 同时导入 factory 和 winssl.lib
 *}

program test_factory_lib;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('SUCCESS: factory and winssl.lib imported');
end.
