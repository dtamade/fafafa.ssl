{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.core`.
  New code should prefer `fafafa.ssl.openssl.api.core`.
}

unit fafafa.ssl.openssl.core;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.core;

implementation

end.
