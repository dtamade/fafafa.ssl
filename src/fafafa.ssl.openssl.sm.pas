{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.sm`.
  New code should prefer `fafafa.ssl.openssl.api.sm`.
}

unit fafafa.ssl.openssl.sm;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.sm;

implementation

end.
