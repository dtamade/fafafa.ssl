{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ecdsa`.
  New code should prefer `fafafa.ssl.openssl.api.ecdsa`.
}

unit fafafa.ssl.openssl.ecdsa;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ecdsa;

implementation

end.
