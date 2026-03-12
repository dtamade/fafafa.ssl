{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.engine`.
  New code should prefer `fafafa.ssl.openssl.api.engine`.
}

unit fafafa.ssl.openssl.engine;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.engine;

implementation

end.
