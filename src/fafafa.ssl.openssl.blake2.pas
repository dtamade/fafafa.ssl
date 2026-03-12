{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.blake2`.
  New code should prefer `fafafa.ssl.openssl.api.blake2`.
}

unit fafafa.ssl.openssl.blake2;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.blake2;

implementation

end.
