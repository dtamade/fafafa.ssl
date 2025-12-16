unit fafafa.ssl.sockets;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  ctypes,
  {$ENDIF}
  SysUtils;

{$IFDEF UNIX}
const
  AF_INET = 2;
  SOCK_STREAM = 1;
  IPPROTO_TCP = 6;
  
  SOL_SOCKET = 1;
  SO_RCVTIMEO = 20;
  SO_SNDTIMEO = 21;
  
  INVALID_SOCKET = -1;
  SOCKET_ERROR = -1;

type
  TSocket = cint;
  
  tsocklen = cuint;
  psockaddr = ^tsockaddr;
  tsockaddr = record
    sa_family: cushort;
    sa_data: array[0..13] of char;
  end;

  PInAddr = ^TInAddr;
  TInAddr = record
    s_addr: cuint;
  end;

  psockaddr_in = ^tsockaddr_in;
  tsockaddr_in = record
    sin_family: cushort;
    sin_port: cushort;
    sin_addr: TInAddr;
    sin_zero: array[0..7] of char;
  end;
  // Alias to match Windows/Delphi types often used
  TSockAddrIn = tsockaddr_in;
  
  PHostEnt = ^THostEnt;
  THostEnt = record
    h_name: PChar;
    h_aliases: PPChar;
    h_addrtype: cint;
    h_length: cint;
    h_addr_list: PPChar;
  end;
  
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: clong;
    tv_usec: clong;
  end;

function socket(domain, atype, protocol: cint): cint; cdecl; external 'c';
function connect(sockfd: cint; addr: psockaddr; addrlen: tsocklen): cint; cdecl; external 'c';
function close(fd: cint): cint; cdecl; external 'c';
function htons(hostshort: cushort): cushort; cdecl; external 'c';
function inet_addr(cp: PChar): cuint; cdecl; external 'c';
function gethostbyname(name: PChar): PHostEnt; cdecl; external 'c';
function setsockopt(sockfd: cint; level, optname: cint; optval: pointer; optlen: tsocklen): cint; cdecl; external 'c';

const
  INADDR_NONE = $FFFFFFFF;

{$ENDIF}

implementation

end.
