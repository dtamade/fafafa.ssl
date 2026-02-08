program probe_sockets;
{$mode objfpc}{$H+}
uses BaseUnix, Unix, Sockets;

var
  s: cint;
  addr: TInetSockAddr;
begin
  s := fpSocket(AF_INET, SOCK_STREAM, 0);
  addr.sin_family := AF_INET;
  addr.sin_port := htons(80);
  addr.sin_addr.s_addr := 0;
  fpConnect(s, @addr, SizeOf(addr));
end.
