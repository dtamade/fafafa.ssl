{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.cmac`.
  New code should prefer `fafafa.ssl.openssl.api.cmac.evp`.
}

unit fafafa.ssl.openssl.cmac;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.cmac.evp;

implementation

end.
