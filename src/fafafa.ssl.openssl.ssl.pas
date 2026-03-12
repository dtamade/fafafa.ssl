{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ssl`.
  New code should prefer `fafafa.ssl.openssl.api.ssl`.
}

unit fafafa.ssl.openssl.ssl;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ssl;

implementation

end.
