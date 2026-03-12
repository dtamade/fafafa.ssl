{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.pem`.
  New code should prefer `fafafa.ssl.openssl.api.pem`.
}

unit fafafa.ssl.openssl.pem;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.pem;

implementation

end.
