# GetConnectionInfo `MacSize` Semantics Matrix

## Goal

把 `GetConnectionInfo.MacSize` 从“WinSSL 独有且口径可疑的字段”收成一条可复用的 bounded truth：

- 先静态盘清 5 个 backend 的真实来源
- 只对共享层能稳定识别的 AEAD suite-name 场景补统一值
- 保留 legacy/non-AEAD 的 backend-specific best-effort 边界，不强猜

## Scope

- `src/fafafa.ssl.connection.base.pas`
- `src/fafafa.ssl.winssl.connection.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_DESIGN.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不在本批为所有 legacy HMAC suites 统一 `MacSize`
- 不在本批为 OpenSSL 再额外加 digest-nid/EVP size 的 low-level `MacSize` 路径
- 不重跑整条重型 Linux/macOS/Windows 门禁

## Static Matrix

### Shared layer

- `TBaseSSLConnection.GetConnectionInfo`
  - 已统一拥有：
    - `ProtocolVersion`
    - `CipherSuite`
    - `CipherSuiteId`（标准 TLS 1.3 名称）
    - `KeyExchange` / `Cipher` / `Hash` / `KeySize`（suite-name best-effort）
    - `ServerName` / `SessionId` / `PeerCertificate`
- 之前没有统一 `MacSize`

### OpenSSL

- `TOpenSSLConnection.GetConnectionInfo`
  - 先走 inherited shared path
  - 再补：
    - real cipher-suite name
    - `KeySize`
    - `CipherSuiteId`
- 当前没有 dedicated `MacSize` fill
- 可参考能力：
  - `SSL_CIPHER_is_aead`
  - `SSL_CIPHER_get_digest_nid`
  - `EVP_MD_get_size`
- 但这批不需要新增 low-level helper，shared suite-name truth 已足够先收一刀

### WinSSL

- `TWinSSLConnection.GetConnectionInfo`
  - 之前完全绕过 shared path
  - `MacSize := ConnInfo.dwHashStrength div 8`
- 这个值更像 hash-strength proxy：
  - 对 legacy HMAC suites 可能仍是有用 best-effort
  - 对 AEAD / TLS 1.3 则会偏离记录层 auth tag 长度

### FreePascal

- 当前使用 shared `GetConnectionInfo`
- 本地 TLS 1.3 AEAD 路径已存在明确 tag-length truth：
  - `TLS13AEADTagLength(...)`
  - 当前支持的 TLS 1.3 suites 都返回 `16`

### MbedTLS / WolfSSL

- 当前都主要依赖 shared `GetConnectionInfo`
- 没有 dedicated `MacSize` fill

## Decision

本批采用 **AEAD-first, legacy-conservative** 收法：

1. 在共享 suite-name 推导里补 `MacSize`
   - `...GCM` / `...POLY1305` / `...OCB` / `...CCM` -> `16`
   - `...CCM_8` -> `8`
2. WinSSL 改为先从 inherited shared path 起步
3. WinSSL 只在 shared path 没给出 `MacSize` 时，才回退到 `dwHashStrength div 8`
4. legacy non-AEAD suites 继续保持 `0`，避免把 trailing hash 名称误写成统一 truth

## Why This Shape

- 共享层现在已经能从 suite name 稳定识别一批 AEAD 场景
- 这条路径同时覆盖：
  - OpenSSL
  - FreePascal
  - MbedTLS
  - WolfSSL
  - 以及改成 inherited-first 之后的 WinSSL
- `dwHashStrength div 8` 继续保留，但降格成 WinSSL 的 legacy fallback，而不是跨 backend 统一语义

## Verification

```bash
bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh
bash tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract
git diff --check
```

## Execution Result

- static matrix result:
  - shared layer 之前完全未填 `MacSize`
  - OpenSSL / FreePascal / MbedTLS / WolfSSL 当前主要都依赖 shared path
  - WinSSL 之前独自使用 `dwHashStrength div 8`
  - 由此确认当前最小统一口径应该是：
    - AEAD suite-name shared truth
    - WinSSL guarded legacy fallback

- implementation:
  - shared `TryDeriveConnectionInfoFromCipherSuiteName(...)` 现在会补：
    - `...GCM` / `...POLY1305` / `...OCB` / `...CCM` -> `16`
    - `...CCM_8` -> `8`
  - WinSSL `GetConnectionInfo` 现在改成：
    - `Result := inherited GetConnectionInfo`
    - 只有当 shared path 仍未给出 `MacSize` 时，才回退 `dwHashStrength div 8`

- focused proof:
  - `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
    - PASS
  - `tests/test_connection_builder_hostname_precedence.pas`
    - PASS
    - `26 passed, 0 failed`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
    - PASS
    - `14 passed, 0 failed`
  - `git diff --check`
    - PASS
