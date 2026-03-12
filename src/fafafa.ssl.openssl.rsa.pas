{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.rsa`.
  New code should prefer `fafafa.ssl.openssl.api.rsa`.
}

unit fafafa.ssl.openssl.rsa;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.rsa;

implementation

end.
