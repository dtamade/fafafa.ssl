{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ui`.
  New code should prefer `fafafa.ssl.openssl.api.ui`.
}

unit fafafa.ssl.openssl.ui;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ui;

implementation

end.
