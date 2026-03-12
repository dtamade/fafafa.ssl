{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.kdf`.
  New code should prefer `fafafa.ssl.openssl.api.kdf`.
}

unit fafafa.ssl.openssl.kdf;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.kdf;

implementation

end.
