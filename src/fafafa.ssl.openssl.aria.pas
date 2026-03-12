{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.aria`.
  New code should prefer `fafafa.ssl.openssl.api.aria`.
}

unit fafafa.ssl.openssl.aria;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.aria;

implementation

end.
