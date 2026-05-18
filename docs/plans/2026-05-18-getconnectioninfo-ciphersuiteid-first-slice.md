# `GetConnectionInfo` CipherSuiteId First Slice

## Goal

把 `GetConnectionInfo` 剩余的 crypto detail completeness debt 再收一刀：先补齐最容易形成双重证据的 `CipherSuiteId`，同时保持 `MacSize` 继续留在后续 backend-specific 审查队列中。

## Scope

本批只处理 `CipherSuiteId` 的 shared truth、OpenSSL low-level truth、focused proof、文档说明与台账：

- `src/fafafa.ssl.connection.base.pas`
- `src/fafafa.ssl.openssl.api.ssl.pas`
- `src/fafafa.ssl.openssl.connection.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `tests/test_openssl_connection_info_cipher_contract.pas`
- `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不在本批补 `MacSize`
- 不做 WinSSL override 重构
- 不重跑整条 backend contract / CI 大门禁

## Why This Batch

在 `Cipher` / `Hash` / `KeySize` / legacy `KeyExchange` 已经由 shared layer first slice 收口后，剩余字段里最适合继续推进的是 `CipherSuiteId`：

- shared layer 已经能对标准 TLS 1.3 cipher-suite name 做稳定推导
- OpenSSL 也有明确的 low-level helper truth：
  - 优先 `SSL_CIPHER_get_protocol_id`
  - 回退 `SSL_CIPHER_get_id and $FFFF`
- 相比之下，`MacSize` 仍更依赖库/平台细节与语义口径，不适合在本批贸然归一

这说明当前最稳妥的 next slice 不是继续扩 shared parser 范围，而是把 `CipherSuiteId` 做成 “shared + OpenSSL low-level” 双重闭环。

## Planned Changes

1. 保留 shared layer 对标准 TLS 1.3 suite name 的 `CipherSuiteId` best-effort 推导
2. 在 OpenSSL API loader 中正式暴露 `SSL_CIPHER_get_protocol_id`
3. 在 `TOpenSSLConnection.GetConnectionInfo` 中增加 low-level 回填：
   - 优先 `SSL_CIPHER_get_protocol_id`
   - 回退 `SSL_CIPHER_get_id` 的低 16 位
4. focused proof 扩成两层：
   - mock proof 继续证明 shared TLS 1.3 name-derived `CipherSuiteId`
   - OpenSSL contract 单独证明 `protocol_id` 优先、`get_id` 回退
5. 同步 residual allowlist、roadmap 与台账，明确：
   - `CipherSuiteId` 已从主债务中移出
   - `MacSize` 继续保留为下一条高价值 bounded batch

## Verification

```bash
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh
git diff --check
```

## Execution Result

- focused mock proof:
  - `tests/test_connection_builder_hostname_precedence.pas`
  - result:
    - `21 passed, 0 failed`
    - on negotiated suite name `TLS_AES_128_GCM_SHA256`, shared `GetConnectionInfo` now derives:
      - `CipherSuiteId = $1301`
      - `Cipher = sslCipherAES128GCM`
      - `Hash = sslHashSHA256`
      - `KeySize = 128`

- focused OpenSSL guard + truth proof:
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - result:
    - `14 passed, 0 failed`
    - fresh-connection path still degrades safely when cipher helpers are intentionally unavailable
    - when low-level helpers are available:
      - `SSL_CIPHER_get_protocol_id` is preferred
      - `SSL_CIPHER_get_id` low word is the fallback

- residual contract follow-up:
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result:
    - current intentional direct-core allowlist stayed controlled
    - expected direct-core hit count is now `10`, because the OpenSSL focused contract gained one new intentional `GetConnectionInfo` proof site

## Expected Outcome

- `GetConnectionInfo` 的 `CipherSuiteId` 不再只是“看起来还能继续补”的未定债务，而是已经形成：
  - shared TLS 1.3 name-derived truth
  - OpenSSL low-level truth
- 当前 implementation-completeness 主线将进一步收缩到：
  - `MacSize`
  - 以及无法只靠名字或统一 low-level helper 安全归一的更细平台差异
