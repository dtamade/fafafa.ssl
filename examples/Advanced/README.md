# Advanced Examples (Level 3)

**适用人群**: 架构师、需要完整控制的开发者
**复杂度**: 完整 ISSLContext 控制、多后端切换

## 推荐示例

| 示例 | 描述 | 文件 |
|------|------|------|
| Mutual TLS | 双向 TLS 认证 | `../08_mutual_tls.pas` |
| WinSSL FIPS | Windows FIPS 模式 | `../09_winssl_fips.pas` |
| Factory Usage | 工厂模式和后端切换 | `../example_factory_usage.pas` |
| Certificate Chain | 证书链验证 | `../07_certificate_chain.pas` |
| Certificate Renewal | 证书续期 | `../10_cert_renewal.pas` |
| Error Handling | 完整错误处理 | `../example_error_handling.pas` |

## 完整上下文控制示例

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.factory;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  Ctx.SetCipherList('ECDHE+AESGCM:ECDHE+CHACHA20');
  Ctx.SetVerifyMode([sslVerifyPeer]);
  Ctx.SetVerifyDepth(4);
  Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
  // ...
end;
```

## 多后端切换

```pascal
uses fafafa.ssl.factory;

begin
  // 查询可用后端
  for Lib in TSSLFactory.GetAvailableLibraries do
    WriteLn('Available: ', TSSLFactory.GetLibraryDescription(Lib));

  // Windows 零依赖部署
  {$IFDEF WINDOWS}
  TSSLFactory.SetDefaultLibrary(sslWinSSL);
  {$ENDIF}
end;
```

## 相关文档

- [架构设计](../../docs/ARCHITECTURE.md)
- [API 参考](../../docs/API_REFERENCE.md)
- [WinSSL 设计](../../docs/WINSSL_DESIGN.md)
