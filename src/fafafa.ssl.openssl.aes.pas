{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.aes`.
  New code should prefer `fafafa.ssl.openssl.api.aes`.
}

unit fafafa.ssl.openssl.aes;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.aes;

implementation

end.
