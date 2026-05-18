# `GetConnectionInfo` Crypto Detail Name-Derived First Slice

## Goal

把 `GetConnectionInfo` 剩余的 crypto detail completeness debt 先切出一刀最稳的共享层实现：当后端已经提供稳定的 negotiated `CipherSuite` 名称时，由 shared `GetConnectionInfo` 先补齐 `Cipher` / `Hash` / `KeySize`，并在名字显式携带旧式前缀时顺手补 `KeyExchange`。

## Scope

本批只处理共享连接层、focused mock proof、文档说明与台账：

- `src/fafafa.ssl.connection.base.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不在本批补 `CipherSuiteId`
- 不在本批补 `MacSize`
- 不做 WinSSL override 重构
- 不重跑整条 backend contract / minimal CI gate

## Why This Batch

静态盘点后，各 backend 在剩余 6 个字段上的真实来源明显分成两类：

- `CipherSuiteId` / `MacSize`
  - 更偏底层库/平台专属信息
  - 当前主要由 WinSSL / OpenSSL 这类 override 路径掌握
- `Cipher` / `Hash` / `KeySize`
  - 在 OpenSSL / MbedTLS / WolfSSL / FreePascal 上，很多时候已经能从 negotiated `CipherSuite` 名称安全推导
  - WinSSL 虽然名字格式不同，但它已经有自己的 override，不需要 shared layer 抢这条路

这说明最合理的 first slice 不是“一次补完 6 个字段”，而是先把名字可推导的 3 到 4 项共享归一落下。

## Planned Changes

1. 在 shared `GetConnectionInfo` 中新增一条 name-derived normalization：
   - 解析 negotiated cipher-suite name
   - 补齐 `Cipher`
   - 补齐 `Hash`
   - 补齐 `KeySize`
   - 当名字显式包含 `ECDHE-RSA` / `ECDHE-ECDSA` / `DHE-RSA` 等旧式前缀时，补齐 `KeyExchange`
2. focused mock proof 改成使用一个真实可解析的 cipher-suite name：
   - `ECDHE-RSA-AES128-GCM-SHA256`
3. 同步 `API_REFERENCE.md`，把这几个字段的 truth 改成：
   - 共享层会先基于 negotiated cipher-suite name 做 best-effort derivation
   - 更细的平台专属 detail 仍由 backend override 决定

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
    - `19 passed, 0 failed`
    - on negotiated suite name `ECDHE-RSA-AES128-GCM-SHA256`, shared `GetConnectionInfo` now derives:
      - `KeyExchange = sslKexECDHE_RSA`
      - `Cipher = sslCipherAES128GCM`
      - `Hash = sslHashSHA256`
      - `KeySize = 128`

- focused OpenSSL guard proof:
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - result:
    - `10 passed, 0 failed`
    - fresh-connection path stayed safe after the shared cipher-suite-name parser was introduced

- residual contract follow-up:
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result:
    - current intentional direct-core allowlist stayed unchanged
    - no new residual file or hit-count update was needed

## Expected Outcome

- `GetConnectionInfo` shared completeness 不再停留在 metadata-only，而开始补第一批 crypto detail
- non-WinSSL backends 即使没有各自的 full override，也能从 negotiated cipher-suite name 获得更完整的 connection info
- 下一批真正剩下的 debt 会更聚焦到：
  - `CipherSuiteId`
  - `MacSize`
  - 以及无法只靠名字安全推导的更细平台差异
