{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.obj`.
  New code should prefer `fafafa.ssl.openssl.api.obj`.
}

unit fafafa.ssl.openssl.obj;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.obj;

implementation

end.
