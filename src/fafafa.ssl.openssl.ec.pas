{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ec`.
  New code should prefer `fafafa.ssl.openssl.api.ec`.
}

unit fafafa.ssl.openssl.ec;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ec;

implementation

end.
