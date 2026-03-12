{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.provider`.
  New code should prefer `fafafa.ssl.openssl.api.provider`.
}

unit fafafa.ssl.openssl.provider;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.provider;

implementation

end.
