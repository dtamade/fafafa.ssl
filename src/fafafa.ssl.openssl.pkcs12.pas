{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.pkcs12`.
  New code should prefer `fafafa.ssl.openssl.api.pkcs12`.
}

unit fafafa.ssl.openssl.pkcs12;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.pkcs12;

implementation

end.
