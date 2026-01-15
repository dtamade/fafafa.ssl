{**
 * 诊断测试: 只导入 SyncObjs
 *}

program test_syncobjs_import;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  SyncObjs;

begin
  WriteLn('SUCCESS: SyncObjs imported');
end.
