{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.err`.
  New code should prefer `fafafa.ssl.openssl.api.err`.
}

unit fafafa.ssl.openssl.err;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.err;

implementation

end.
