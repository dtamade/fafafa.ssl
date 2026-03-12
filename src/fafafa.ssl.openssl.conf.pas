{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.conf`.
  New code should prefer `fafafa.ssl.openssl.api.conf`.
}

unit fafafa.ssl.openssl.conf;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.conf;

implementation

end.
