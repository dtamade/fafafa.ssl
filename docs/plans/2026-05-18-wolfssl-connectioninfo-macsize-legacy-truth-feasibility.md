# WolfSSL `GetConnectionInfo` Legacy `MacSize` Truth Feasibility

## Goal

继续沿着 `GetConnectionInfo` implementation-completeness 主线推进，把 WolfSSL 在 legacy/non-AEAD 场景下已经具备的 low-level `MacSize` truth 接起来，同时保持 shared AEAD suite-name 语义仍是第一 owner。

## Scope

- `src/fafafa.ssl.wolfssl.api.pas`
- `src/fafafa.ssl.wolfssl.connection.pas`
- `tests/test_wolfssl_connection_info_macsize_contract.pas`
- `tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不把 legacy/non-AEAD `MacSize` 猜值扩散到 shared suite-name parser
- 不在本批顺手扩成 `MbedTLS` 的 low-level truth 接入
- 不重跑整条重型 gate

## Why This Batch

上一批已经把 `MacSize` 主线压成：

- shared AEAD suite-name truth
- WinSSL guarded fallback
- OpenSSL legacy digest truth

当前剩余高价值 backend 里，WolfSSL 的条件最好：

- 本机有 `libwolfssl.so`
- 本机头文件和导出符号都确认存在：
  - `wolfSSL_GetHmacSize`
- 当前仓库 binding 还没把这条 low-level truth 接到 `GetConnectionInfo`

## Planned Changes

1. 在 `wolfssl.api` 补齐 `wolfSSL_GetHmacSize` 动态绑定
2. 在 `TWolfSSLConnection.GetConnectionInfo` 中加入 guarded logic：
   - 先保留 inherited shared truth
   - 仅当 shared path 仍未给出 `MacSize`
   - 才回退到 `wolfSSL_GetHmacSize(FWolfSSL)`
3. 增加 focused WolfSSL contract：
   - helper unavailable -> safe degrade
   - legacy non-AEAD HMAC truth -> `MacSize` 正确回填
   - AEAD suite -> 继续保持 shared `MacSize`，不被 backend helper 覆盖

## Verification

```bash
bash tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh
mkdir -p tmp/test_wolfssl_connection_info_macsize_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_info_macsize_contract -FEtmp/test_wolfssl_connection_info_macsize_contract -otmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract tests/test_wolfssl_connection_info_macsize_contract.pas && ./tmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
git diff --check
```

## Execution Result

- local feasibility result:
  - WolfSSL active source already had a direct legacy `MacSize` truth source:
    - `wolfSSL_GetHmacSize`
  - 当前缺口只在 active binding / connection-info write path，而不是底层库能力缺失

- implementation:
  - `wolfssl.api` 现在已导出并绑定：
    - `wolfSSL_GetHmacSize`
  - `TWolfSSLConnection.GetConnectionInfo` 现在：
    - preserves shared AEAD `MacSize`
    - only fills legacy/non-AEAD `MacSize` when shared truth still leaves `MacSize = 0`

- focused proof:
  - `bash tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
    - PASS
  - `tests/test_wolfssl_connection_info_macsize_contract.pas`
    - PASS
    - `3 passed, 0 failed`
    - explicitly covers:
      - helper unavailable safe degrade
      - non-AEAD HMAC truth -> `MacSize = 32`
      - AEAD shared truth keeps owner primacy at `MacSize = 16`
  - `tests/test_connection_builder_hostname_precedence.pas`
    - PASS
    - `26 passed, 0 failed`
  - `git diff --check`
    - PASS

- contract note:
  - the first WolfSSL test attempt failed because optional backend tests must define `ENABLE_WOLFSSL` and pull in `fafafa.ssl.wolfssl.lib`
  - this is now encoded in the focused test so the factory registration precondition is no longer rediscovered by accident

## Expected Outcome

- WolfSSL 不再属于 “legacy `MacSize` 完全空白” 的 backend
- shared 继续拥有 AEAD `MacSize` 语义
- `GetConnectionInfo` implementation-completeness 主线会进一步收缩到：
  - MbedTLS 是否也值得接 low-level truth
  - 以及不再适合继续深挖 `MacSize` 时的 owner / deprecation wording route
