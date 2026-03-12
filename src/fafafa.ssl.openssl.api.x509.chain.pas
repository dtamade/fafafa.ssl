unit fafafa.ssl.openssl.api.x509.chain;

{$mode ObjFPC}{$H+}

interface

uses
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.x509.chain;

function FindIssuerX509InChain(ALeaf: PX509; AChain: PSTACK_OF_X509): PX509;

implementation

function FindIssuerX509InChain(ALeaf: PX509; AChain: PSTACK_OF_X509): PX509;
begin
  Result := fafafa.ssl.openssl.x509.chain.FindIssuerX509InChain(ALeaf, AChain);
end;

end.
