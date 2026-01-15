{**
 * Test winsock2 import
 *}
program test_winsock2;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Windows,
  winsock2;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: winsock2 loaded successfully');
  WriteLn('Step 3: About to exit normally');
  Halt(0);
end.
