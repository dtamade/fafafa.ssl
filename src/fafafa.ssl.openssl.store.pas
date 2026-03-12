{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.store`.
  New code should prefer `fafafa.ssl.openssl.api.store`.
}

unit fafafa.ssl.openssl.store;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.store;

implementation

end.
