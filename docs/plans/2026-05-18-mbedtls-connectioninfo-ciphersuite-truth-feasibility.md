# MbedTLS `GetConnectionInfo` Ciphersuite Truth Feasibility

## Goal

继续沿着 `GetConnectionInfo` implementation-completeness 主线推进，把 MbedTLS 已经具备的 ciphersuite-info low-level truth 接到 active runtime path，同时修正当前仓库里一个会污染该路径的 `MD` 常量真相错误。

## Scope

- `src/fafafa.ssl.mbedtls.base.pas`
- `src/fafafa.ssl.mbedtls.api.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `src/fafafa.ssl.connection.base.pas`
- `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
- `tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不重跑整条重型 gate
- 不在本批进入 broader owner / deprecation wording route
- 不在本批为 FreePascal 额外强行发明 low-level `MacSize` source

## Why This Batch

在 OpenSSL / WolfSSL legacy `MacSize` truth 落地后，MbedTLS 成了 `GetConnectionInfo` 这条 implementation-completeness 主线上最值得继续推进的一刀：

- 本机头文件和导出符号都确认存在：
  - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
  - `mbedtls_ssl_get_ciphersuite_id`
  - `mbedtls_ssl_ciphersuite_from_id`
  - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
- 当前仓库 binding 还没有把这条 ciphersuite-info 路径接到 connection-info

同时，静态审查也暴露出一个必须先修的基础真相错误：

- `src/fafafa.ssl.mbedtls.base.pas` 里的：
  - `MBEDTLS_MD_SHA1`
  - `MBEDTLS_MD_RIPEMD160`
- 和本机 `mbedtls/md.h` 实际枚举值对不上，发生了对调

## Planned Changes

1. 修正 `MBEDTLS_MD_SHA1` / `MBEDTLS_MD_RIPEMD160` 常量真相
2. 在 `mbedtls.base` 增加最小 `ciphersuite_info` record 映射
3. 在 `mbedtls.api` 补齐：
   - `mbedtls_ssl_get_ciphersuite_id`
   - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
   - `mbedtls_ssl_ciphersuite_from_id`
   - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
4. 在 `TMbedTLSConnection.GetConnectionInfo` 中加入 guarded logic：
   - direct `ssl -> ciphersuite id` truth
   - name-based `ciphersuite -> id` fallback
   - `ciphersuite info -> KeySize`
   - shared path 未给 `MacSize` 时，再用 `ciphersuite mac -> md size` 回填 legacy truth
5. 顺手修 shared parser 对 MbedTLS 连字符 suite name 的兼容缺口：
   - `TLS-RSA-...`
   - `AES-128[-GCM]`
   - `AES-256[-GCM]`

## Verification

```bash
bash tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh
mkdir -p tmp/test_mbedtls_connection_info_ciphersuite_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_info_ciphersuite_contract -FEtmp/test_mbedtls_connection_info_ciphersuite_contract -otmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract tests/test_mbedtls_connection_info_ciphersuite_contract.pas && ./tmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
git diff --check
```

## Execution Result

- local feasibility result:
  - MbedTLS active source already had enough low-level truth to complete the connection-info path:
    - direct ciphersuite id
    - ciphersuite descriptor
    - key-bit helper
    - md-info / md-size helper
  - the main missing piece was the active Pascal binding / runtime write path, not library capability

- implementation:
  - `mbedtls.base` now:
    - fixes `MBEDTLS_MD_SHA1` / `MBEDTLS_MD_RIPEMD160` truth
    - exposes the minimal `ciphersuite_info` layout needed for runtime truth extraction
  - `mbedtls.api` now exports/binds:
    - `mbedtls_ssl_get_ciphersuite_id`
    - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
    - `mbedtls_ssl_ciphersuite_from_id`
    - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
  - `TMbedTLSConnection.GetConnectionInfo` now:
    - prefers low-level `CipherSuiteId` truth
    - falls back to name-based ciphersuite id truth when direct helper is unavailable
    - fills `KeySize` from ciphersuite info
    - fills legacy/non-AEAD `MacSize` from digest truth only when shared AEAD owner path still leaves `MacSize = 0`
  - `TBaseSSLConnection` now also recognizes MbedTLS-style hyphenated AES / TLS-RSA suite names

- focused proof:
  - `bash tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
    - PASS
  - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
    - PASS
    - `15 passed, 0 failed`
    - explicitly covers:
      - corrected `MBEDTLS_MD_SHA1` runtime digest truth against canonical SHA1(`abc`)
      - helper unavailable safe degrade
      - direct ciphersuite-id truth
      - name-based ciphersuite-id fallback
      - legacy non-AEAD digest-truth `MacSize`
      - shared AEAD `MacSize` owner primacy
  - `tests/test_connection_builder_hostname_precedence.pas`
    - PASS
    - `26 passed, 0 failed`
  - `git diff --check`
    - PASS

## Expected Outcome

- MbedTLS 不再属于 “只有 shared AEAD truth” 的 backend
- `GetConnectionInfo` 当前实现完整性主线会进一步收缩到：
  - 这条 backend truth 线是否已经足够收口
  - 是否需要对 FreePascal 再做一次 completion audit
  - 以及何时切回 owner / deprecation wording route
