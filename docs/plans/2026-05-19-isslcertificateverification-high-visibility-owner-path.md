# ISSLCertificateVerification High-Visibility Owner Path

## Goal

把高可见的 builder / TLS facade / CT/OCSP 指南切到 `ISSLCertificateVerification` owner path，确保：

- 普通握手失败路径不再默认读取 core `GetVerifyResult` / `GetVerifyResultString`
- `src/fafafa.ssl.connection.builder.pas` 与 `src/fafafa.ssl.tls.pas` 优先走 optional owner interface
- 高可见指南不再继续教学 direct core verify-result mirrors

## Scope

- `src/fafafa.ssl.connection.builder.pas`
- `src/fafafa.ssl.tls.pas`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Steps

1. 扩展现有 `ISSLCertificateVerification` active-guidance contract，先锁住新的高可见 docs/source truth
2. 对 builder / TLS facade 做最小 owner-path helper 修复
3. 更新 CT/OCSP 指南示例到 `ISSLCertificateVerification`
4. 运行 shell contract、focused compile/run、backend contract，到绿色后记账

## Commands

```bash
bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh

mkdir -p tmp/test_connection_builder_hostname_precedence && \
  fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_connection_builder_hostname_precedence \
  -FEtmp/test_connection_builder_hostname_precedence \
  -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence \
  tests/test_connection_builder_hostname_precedence.pas && \
  ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence

mkdir -p tmp/test_tls_connector_hostname_override_precedence && \
  fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_tls_connector_hostname_override_precedence \
  -FEtmp/test_tls_connector_hostname_override_precedence \
  -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence \
  tests/test_tls_connector_hostname_override_precedence.pas && \
  ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence

mkdir -p tmp/backend_contract_units && \
  fpc -B -Fu./src -Fu./tests \
  -FUtmp/backend_contract_units \
  -FEtmp \
  -otmp/tmp_backend_contract \
  tests/contract/test_backend_contract.pas && \
  ./tmp/tmp_backend_contract

git diff --check
```

## Expected Closeout

- 高可见 docs/source contract 先 RED 后 GREEN
- builder / TLS facade focused compile-run 继续 green
- backend contract 持续 green
