{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.rand`.
  New code should prefer `fafafa.ssl.openssl.api.rand`.
}

unit fafafa.ssl.openssl.rand;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.rand;

implementation

end.
