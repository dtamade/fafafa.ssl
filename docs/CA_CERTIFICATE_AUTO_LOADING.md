# CA Certificate Auto-Loading

## Current Status

This page is now a current-state note, not a feature-completion record.

Do not assume that `Lib.CreateContext(sslCtxClient)` by itself automatically loads
system CA certificates across backends. The supported, documented path is to ask
for system trust explicitly through the context builder.

## Recommended Client Pattern

```pascal
uses
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.ssl.tls;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
begin
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(SocketHandle, 'www.example.com');
  try
    WriteLn('TLS OK: ', Stream.Connection.GetCipherName);
  finally
    Stream.Free;
  end;
end.
```

`WithSystemRoots` is the portable contract:

- it asks the selected backend to load its platform-appropriate trust source
- it keeps the trust-store setup explicit in user code
- it composes cleanly with `.WithCAFile`, `.WithCAPath`, or `SetCertificateStore(...)`

## What To Avoid Documenting

- Do not document `Lib.CreateContext(sslCtxClient)` alone as "automatic CA loading".
- Do not treat one backend's internal helper path as the cross-backend contract.
- Do not describe hostname verification as a context-level option; use
  `TSSLConnector.Connect*(..., host)` or `ISSLClientConnection.SetServerName(...)`
  on the connection.

## Backend Notes

- OpenSSL, WinSSL, MbedTLS, and WolfSSL do not share the same native trust-store
  implementation details.
- WinSSL ultimately validates against Windows certificate-store semantics.
- OpenSSL-family backends may rely on file/path-based trust loading.
- The common API surface is the builder/store abstraction, not an implicit
  "auto-loaded client context" guarantee.

## Custom or Private Trust Anchors

If you need non-system trust anchors, layer them explicitly:

```pascal
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithSystemRoots
  .WithCAFile('/path/to/internal-ca.pem')
  .BuildClient;
```

Or inject a backend-specific `ISSLCertificateStore` through `SetCertificateStore(...)`
when you need finer control.

## Verification Pointers

- Builder system-roots runtime contract:
  `tests/config/test_context_builder_system_roots_contract.pas`
- Current doc/runtime guidance contract:
  `tests/scripts/test_active_tls_guidance_contract.sh`
