{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.ts`.
  New code should prefer `fafafa.ssl.openssl.api.ts`.
}

unit fafafa.ssl.openssl.ts;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.ts;

implementation

end.
