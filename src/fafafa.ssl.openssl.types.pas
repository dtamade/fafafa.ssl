{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.types`.
  New code should prefer `fafafa.ssl.openssl.api.types`.
}

unit fafafa.ssl.openssl.types;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.types;

implementation

end.
