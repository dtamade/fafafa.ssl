# WinSSL `GetConnectionInfo` Cipher Truth Correction

## Goal

修正 WinSSL `GetConnectionInfo` 里一个更基础的 truth-source 错误：`CipherSuiteId` 不能继续从 `SecPkgContext_ConnectionInfo.aiCipher` 这种算法级字段回填，而应改为 Schannel 官方的 `SECPKG_ATTR_CIPHER_INFO` / `dwCipherSuite` 路径。

## Scope

本批只处理 WinSSL cipher truth correction、静态 contract、文档说明与台账：

- `src/fafafa.ssl.winssl.base.pas`
- `src/fafafa.ssl.winssl.connection.pas`
- `tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_DESIGN.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不在本批统一 `MacSize`
- 不在本批重构整个 WinSSL `GetConnectionInfo` 字段矩阵
- 不重跑整条重型 Linux/macOS gate

## Why This Batch

原计划是继续静态盘点 `MacSize`。但盘点过程中暴露出一个更高优先级、而且是确定性的 WinSSL truth bug：

- `TSecPkgContext_ConnectionInfo.aiCipher` 在仓库定义中就是“加密算法 ID”
- 同一个 WinSSL 文件里，它也一直被当成算法字段来生成 `CipherSuite` 名称和 `Cipher` 枚举
- 但 `GetConnectionInfo` 却错误地把它直接写进了 `CipherSuiteId`

这说明当前问题不是“字段还没补齐”，而是“已经写了一个错误来源”。相比仍然存在语义争议的 `MacSize`，这个更应该先修掉。

## Planned Changes

1. 在 WinSSL base 常量中补正式的 `SECPKG_ATTR_CIPHER_INFO`
2. 在 WinSSL connection 中增加一个最小 helper：
   - 读取 Schannel `SECPKG_ATTR_CIPHER_INFO`
   - 提取真实 `dwCipherSuite`
   - 在可安全读取时同步拿到真实 suite name
3. 把 `GetConnectionInfo.CipherSuiteId` 从错误的 `ConnInfo.aiCipher` 改为 cipher-info truth
4. 让 `DoGetCipherName` 在可用时优先返回真实 suite name，保持与 `GetConnectionInfo.CipherSuite` 一致
5. 补一个本地静态 contract，防止以后再把 `aiCipher` 当 suite id 写回
6. 同步文档与台账：
   - WinSSL cipher truth 已更正
   - `MacSize` 仍留在下一条 bounded 审查线

## Verification

```bash
bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract
git diff --check
```

## Execution Result

- local WinSSL static contract:
  - `tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
  - result:
    - PASS
    - verified:
      - `SECPKG_ATTR_CIPHER_INFO` is defined
      - WinSSL connection now queries `SECPKG_ATTR_CIPHER_INFO`
      - `ConnInfo.aiCipher -> CipherSuiteId` direct write is gone

- regression sanity proofs:
  - `tests/test_connection_builder_hostname_precedence.pas`
  - result:
    - `21 passed, 0 failed`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - result:
    - `14 passed, 0 failed`

## Expected Outcome

- WinSSL `CipherSuiteId` 不再建立在错误的算法 ID 假设上
- WinSSL `CipherSuite` / `CipherSuiteId` 更接近官方 Schannel truth source
- `MacSize` 继续保留为下一条需要单独定语义和来源矩阵的 bounded batch
