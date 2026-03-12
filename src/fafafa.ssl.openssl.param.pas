{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.param`.
  New code should prefer `fafafa.ssl.openssl.api.param`.
}

unit fafafa.ssl.openssl.param;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.param;

implementation

end.
