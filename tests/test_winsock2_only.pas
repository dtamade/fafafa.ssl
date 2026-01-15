{**
 * 测试 winsock2 模块
 *}

program test_winsock2_only;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, winsock2;

var
  WSAData: TWSAData;
  LResult: Integer;
begin
  WriteLn('Testing winsock2...');
  LResult := WSAStartup($0202, WSAData);
  if LResult = 0 then
  begin
    WriteLn('WSAStartup succeeded');
    WriteLn('Version: ', WSAData.wVersion);
    WSACleanup;
    WriteLn('SUCCESS');
    Halt(0);
  end
  else
  begin
    WriteLn('WSAStartup failed: ', LResult);
    Halt(1);
  end;
end.
