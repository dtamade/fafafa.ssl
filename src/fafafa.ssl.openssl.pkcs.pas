{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.pkcs`.
  New code should prefer `fafafa.ssl.openssl.api.pkcs`.
}

unit fafafa.ssl.openssl.pkcs;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.pkcs;

implementation

end.
