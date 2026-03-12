{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.dso`.
  New code should prefer `fafafa.ssl.openssl.api.dso`.
}

unit fafafa.ssl.openssl.dso;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.dso;

implementation

end.
