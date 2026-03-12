{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.modes`.
  New code should prefer `fafafa.ssl.openssl.api.modes`.
}

unit fafafa.ssl.openssl.modes;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.modes;

implementation

end.
