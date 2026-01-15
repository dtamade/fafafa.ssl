{**
 * 诊断测试: 只导入 winsock2
 *}

program test_winsock2_import;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Windows,
  winsock2;

begin
  WriteLn('SUCCESS: winsock2 imported');
end.
