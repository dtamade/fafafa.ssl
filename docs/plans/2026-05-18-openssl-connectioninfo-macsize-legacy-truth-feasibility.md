# OpenSSL `GetConnectionInfo` Legacy `MacSize` Truth Feasibility

## Goal

继续沿着 `GetConnectionInfo` implementation-completeness 主线推进，但不再扩散 shared 层的 legacy `MacSize` 猜值；先把 OpenSSL 在 non-AEAD 场景下本来就具备的 low-level truth 接起来。

## Scope

- `src/fafafa.ssl.openssl.api.ssl.pas`
- `src/fafafa.ssl.openssl.api.evp.pas`
- `src/fafafa.ssl.openssl.connection.pas`
- `tests/test_openssl_connection_info_cipher_contract.pas`
- `tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不把 legacy/non-AEAD `MacSize` 回推规则扩散到 shared suite-name parser
- 不在本批额外实现 MbedTLS / WolfSSL 的 low-level `MacSize` truth
- 不重跑整条重型 gate

## Why This Batch

上一批已经把 `MacSize` 压成：

- shared AEAD suite-name truth
- WinSSL guarded legacy fallback

剩余真正高价值、且边界清晰的一刀，是 OpenSSL：

- 当前 `TOpenSSLConnection.GetConnectionInfo` 已有 current cipher 句柄
- OpenSSL 也具备：
  - `SSL_CIPHER_is_aead`
  - `SSL_CIPHER_get_digest_nid`
  - `EVP_get_digestbynid`
  - `EVP_MD_size`
- 但 active export/binding path 还没有把这条 low-level truth 真正接到 connection-info

## Planned Changes

1. 在 `api.ssl` 补齐：
   - `SSL_CIPHER_get_digest_nid`
   - `SSL_CIPHER_is_aead`
2. 在 `api.evp` 补齐：
   - `EVP_get_digestbynid`
3. 在 `TOpenSSLConnection.GetConnectionInfo` 中加入 guarded logic：
   - 仅当 shared path 仍未给出 `MacSize`
   - 且当前 cipher 明确是 non-AEAD
   - 才用 digest truth 回填 legacy `MacSize`
4. 扩展 focused OpenSSL contract：
   - helper unavailable -> safe degrade
   - non-AEAD digest truth -> `MacSize` 正确回填
   - AEAD suite -> 继续保持 shared `MacSize`，不被 digest size 覆盖

## Verification

```bash
bash tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh
mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
git diff --check
```

## Execution Result

- static feasibility result:
  - OpenSSL active source already had the right ingredients:
    - `SSL_CIPHER_is_aead`
    - `SSL_CIPHER_get_digest_nid`
    - `EVP_get_digestbynid`
    - `EVP_MD_size`
  - but the active export/binding path was incomplete:
    - `api.ssl` was still missing `is_aead` / `digest_nid`
    - `api.evp` was still missing `EVP_get_digestbynid`

- implementation:
  - `api.ssl` now exports/binds:
    - `SSL_CIPHER_is_aead`
    - `SSL_CIPHER_get_digest_nid`
  - `api.evp` now exports/binds:
    - `EVP_get_digestbynid`
  - `TOpenSSLConnection.GetConnectionInfo` now:
    - preserves shared AEAD `MacSize`
    - only fills legacy/non-AEAD `MacSize` when digest truth is available

- focused proof:
  - `bash tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
    - PASS
  - `tests/test_openssl_connection_info_cipher_contract.pas`
    - PASS
    - `20 passed, 0 failed`
    - now explicitly covers:
      - helper unavailable safe degrade
      - non-AEAD digest truth -> `MacSize = 32`
      - AEAD digest truth does not override shared `MacSize = 16`
  - `tests/test_connection_builder_hostname_precedence.pas`
    - PASS
    - `26 passed, 0 failed`
  - `git diff --check`
    - PASS
