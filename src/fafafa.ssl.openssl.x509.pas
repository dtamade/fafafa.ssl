{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.x509`.
  New code should prefer `fafafa.ssl.openssl.api.x509`.
}

unit fafafa.ssl.openssl.x509;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.x509;

implementation

end.
