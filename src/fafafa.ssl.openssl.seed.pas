{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.seed`.
  New code should prefer `fafafa.ssl.openssl.api.seed`.
}

unit fafafa.ssl.openssl.seed;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.seed;

implementation

end.
