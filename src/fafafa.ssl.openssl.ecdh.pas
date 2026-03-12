{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ecdh`.
  New code should prefer `fafafa.ssl.openssl.api.ecdh`.
}

unit fafafa.ssl.openssl.ecdh;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ecdh;

implementation

end.
