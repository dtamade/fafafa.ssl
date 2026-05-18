# ISSLCertificateVerification Generic Examples Owner Path

## Goal

把 generic examples / 通用测试示例里仍直接读取 core `GetVerifyResult` / `GetVerifyResultString` 的入口切到 `ISSLCertificateVerification` owner path，确保：

- `examples/` 下高复用示例不再把 core verify getters 当作默认 public guidance
- `tests/examples/` 下通用示例程序也不再继续教学 direct core verify-result mirrors
- 收口方式尽量复用公共 helper，避免在多个示例程序里反复复制 owner-path 逻辑

## Scope

- `examples/fafafa.examples.tcp.pas`
- `examples/01_tls_client.pas`
- `examples/example_https_api.pas`
- `examples/production/https_client_auth.pas`
- `examples/validation/real_world_test.pas`
- `tests/examples/test_openssl.pas`
- `tests/examples/test_real_websites.pas`
- `tests/examples/test_real_websites_enhanced.pas`
- `tests/examples/test_real_websites_comprehensive.pas`
- `tests/connection/test_ssl_client_connection.pas`
- `tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Steps

1. 新增 source contract，先锁住这些 generic examples / tests 不再直接读 core verify getters
2. 在 `examples/fafafa.examples.tcp.pas` 增加复用 helper，优先走 `ISSLCertificateVerification`
3. 将目标 examples / tests 改为复用 helper；不依赖该单元的连接测试保留本地 helper
4. 运行 shell contract 与目标编译验证，到绿色后记账

## Commands

```bash
bash -n tests/scripts/test_isslcertificateverification_generic_examples_contract.sh
bash tests/scripts/test_isslcertificateverification_generic_examples_contract.sh

mkdir -p tmp/example_owner_path_01_tls_client && \
  fpc -B -Fu./src -Fu./examples \
  -FEtmp/example_owner_path_01_tls_client \
  -otmp/example_owner_path_01_tls_client/01_tls_client \
  examples/01_tls_client.pas

mkdir -p tmp/example_owner_path_https_api && \
  fpc -B -Fu./src -Fu./examples \
  -FEtmp/example_owner_path_https_api \
  -otmp/example_owner_path_https_api/example_https_api \
  examples/example_https_api.pas

mkdir -p tmp/example_owner_path_real_websites && \
  fpc -B -Fu./src -Fu./examples -Fu./tests/examples \
  -FEtmp/example_owner_path_real_websites \
  -otmp/example_owner_path_real_websites/test_real_websites \
  tests/examples/test_real_websites.pas

mkdir -p tmp/example_owner_path_ssl_client_connection && \
  fpc -B -Fu./src -Fu./tests \
  -FEtmp/example_owner_path_ssl_client_connection \
  -otmp/example_owner_path_ssl_client_connection/test_ssl_client_connection \
  tests/connection/test_ssl_client_connection.pas

git diff --check
```

## Expected Closeout

- source contract 先 RED 后 GREEN
- generic examples / tests 优先走 `ISSLCertificateVerification` owner path
- 目标编译验证通过
- 不扩大到 backend-specific runtime 测试或更深的 verify-result deprecation 语义
