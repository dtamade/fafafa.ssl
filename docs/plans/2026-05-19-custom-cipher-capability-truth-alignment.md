# 2026-05-19 Custom Cipher Capability Truth Alignment

## Goal

继续沿着 interface/backend completeness 主线推进，收口 `SupportsCustomCipherSuites` 与
`SetCipherList(...)` / `SetCipherSuites(...)` 的一个真实实现漂移：

- `OpenSSL` 之前无条件发布 `SupportsCustomCipherSuites=True`
- `FreePascal` 之前也发布 `SupportsCustomCipherSuites=True`
- 但除 `OpenSSL` 外，其它 backend 当前都没有把 custom non-default cipher override 真正落到 runtime
- 而 `OpenSSL` 也仍然依赖 `SSL_CTX_set_cipher_list` / `SSL_CTX_set_ciphersuites` helper 是否真的存在

这会造成 public contract 撒谎：

- capability 说支持
- 但 backend 只是存字段，或 runtime helper 缺失时 silently degrade

本批把 custom cipher publication 收紧到 runtime-aware / fail-closed truth，同时保住 shipped baseline defaults 和默认上下文创建路径。

## Scope

- 只处理：
  - `SupportsCustomCipherSuites` 的 published truth
  - `SetCipherList(...)` / `SetCipherSuites(...)` 对 custom non-default override 的 fail-closed 语义
  - shipped baseline defaults / default-context path 的 compatibility 保留
  - 对应 focused contract、受污染测试、以及最直接的 backend docs
- 不重做：
  - 更细粒度的 TLS1.2 / TLS1.3 分离 capability
  - generic onboarding 全量文档 sweep
  - cipher-string validation / parse correctness

## Files

- `src/fafafa.ssl.openssl.api.ssl.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.openssl.context.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `src/fafafa.ssl.wolfssl.context.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.mbedtls.context.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.winssl.context.pas`
- `src/fafafa.ssl.base.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_PERFORMANCE_TUNING.md`
- `docs/guides/WINSSL_BEST_PRACTICES.md`
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `tests/scripts/test_custom_cipher_capability_truth_contract.sh`
- `tests/test_backend_custom_cipher_capability_truth_contract.pas`
- `tests/test_direct_library_default_config_parity.pas`
- `tests/mbedtls/test_mbedtls_server_accept_simple.pas`
- `tests/winssl/test_winssl_context_config.pas`
- `tests/winssl/test_winssl_context_comprehensive.pas`
- `tests/unit/test_winssl_comprehensive.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `SupportsCustomCipherSuites` 代表的是 coarse-grained published surface，不区分：
  - TLS 1.2 cipher-list override
  - TLS 1.3 cipher-suites override
- 因而当前最保守且一致的真相应是：
  - `OpenSSL` 只有在 `SSL_CTX_set_cipher_list` 与 `SSL_CTX_set_ciphersuites` 都就绪时，才发布 `SupportsCustomCipherSuites=True`
  - `FreePascal` / `WinSSL` / `MbedTLS` / `WolfSSL` 当前都发布 `SupportsCustomCipherSuites=False`
- 但 shipped baseline defaults 不能被这批修法误伤：
  - `SSL_DEFAULT_CIPHER_LIST`
  - `SSL_DEFAULT_TLS13_CIPHERSUITES`
  仍应作为 compatibility/default-context path 允许通过
- 因而 setter 的最小正确语义是：
  - custom non-default override -> 先检查 `SupportsCustomCipherSuites`
  - `SupportsCustomCipherSuites=False` -> fail-closed `unsupported`
  - empty clear / shipped baseline defaults -> 继续允许

## Steps

1. 先补 shell contract 与 focused Pascal runtime contract，让旧 capability/source truth 先 RED。
2. 修 capability publication 与 setter fail-closed 语义。
3. 修受污染的 WinSSL / MbedTLS 测试与最直接的 backend docs。
4. 跑 focused verification，更新台账并提交。

## Commands

```bash
bash -n tests/scripts/test_custom_cipher_capability_truth_contract.sh
bash tests/scripts/test_custom_cipher_capability_truth_contract.sh
mkdir -p tmp/test_custom_cipher_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_custom_cipher_truth -FEtmp/test_custom_cipher_truth -otmp/test_custom_cipher_truth/test_backend_custom_cipher_capability_truth_contract tests/test_backend_custom_cipher_capability_truth_contract.pas
./tmp/test_custom_cipher_truth/test_backend_custom_cipher_capability_truth_contract
fpc -B -Fu./src -Fu./tests -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas
./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity
git diff --check
```

## Expected Result

- `OpenSSL` custom-cipher publication 不再无条件为真
- `FreePascal` / `WinSSL` / `MbedTLS` / `WolfSSL` 不再继续把 custom non-default override 冒充成已发布能力
- custom non-default cipher override 与 capability/public truth 重新一致
- shipped baseline defaults / 默认上下文创建路径仍然保持可用
