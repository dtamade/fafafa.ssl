{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.comp`.
  New code should prefer `fafafa.ssl.openssl.api.comp`.
}

unit fafafa.ssl.openssl.comp;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.comp;

implementation

end.
