{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.pkcs7`.
  New code should prefer `fafafa.ssl.openssl.api.pkcs7`.
}

unit fafafa.ssl.openssl.pkcs7;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.pkcs7;

implementation

end.
