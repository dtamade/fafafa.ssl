{**
 * 诊断测试: 同时导入 factory 和 context
 *}

program test_factory_context;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context;

begin
  WriteLn('SUCCESS: factory and context imported');
end.
