{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.cms`.
  New code should prefer `fafafa.ssl.openssl.api.cms`.
}

unit fafafa.ssl.openssl.cms;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.cms;

implementation

end.
