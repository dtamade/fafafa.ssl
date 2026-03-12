{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.thread`.
  New code should prefer `fafafa.ssl.openssl.api.thread`.
}

unit fafafa.ssl.openssl.thread;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.thread;

implementation

end.
