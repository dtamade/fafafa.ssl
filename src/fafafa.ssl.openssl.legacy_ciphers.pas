{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.legacy_ciphers`.
  New code should prefer `fafafa.ssl.openssl.api.legacy_ciphers`.
}

unit fafafa.ssl.openssl.legacy_ciphers;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.legacy_ciphers;

implementation

end.
