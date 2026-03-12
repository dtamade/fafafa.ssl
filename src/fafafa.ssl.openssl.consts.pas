{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.consts`.
  New code should prefer `fafafa.ssl.openssl.api.consts`.
}

unit fafafa.ssl.openssl.consts;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.consts;

implementation

end.
