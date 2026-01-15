{**
 * Test factory only to see if it causes the crash
 *}
program test_factory_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: Factory loaded successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
