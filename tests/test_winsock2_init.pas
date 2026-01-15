{**
 * 诊断测试: 初始化 winsock2
 *}

program test_winsock2_init;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Windows,
  winsock2;

var
  LWSAData: TWSAData;
  LResult: Integer;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Calling WSAStartup...');
  Flush(Output);
  
  LResult := WSAStartup($0202, LWSAData);
  if LResult = 0 then
  begin
    WriteLn('Step 3: WSAStartup succeeded');
    WSACleanup;
  end
  else
    WriteLn('Step 3: WSAStartup failed: ', LResult);
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
