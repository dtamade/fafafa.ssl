{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ct`.
  New code should prefer `fafafa.ssl.openssl.api.ct`.
}

unit fafafa.ssl.openssl.ct;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ct;

implementation

end.
