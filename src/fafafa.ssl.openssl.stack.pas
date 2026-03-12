{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.stack`.
  New code should prefer `fafafa.ssl.openssl.api.stack`.
}

unit fafafa.ssl.openssl.stack;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.stack;

implementation

end.
