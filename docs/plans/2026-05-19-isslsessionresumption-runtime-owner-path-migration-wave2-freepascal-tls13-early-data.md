# `ISSLSessionResumption` Runtime Owner-Path Migration Wave 2: FreePascal TLS 1.3 Early Data

## Goal

把 `tests/test_freepascal_tls13_early_data.pas` 这份超大 ordinary runtime test
里的 direct-core session-resumption mirrors 迁到
`ISSLSessionResumption` owner path，并用 focused contract 锁住，避免后续重复回退。

## Scope

本批只处理：

- `tests/test_freepascal_tls13_early_data.pas`
- `tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
- `docs/plans/2026-05-19-isslsessionresumption-runtime-owner-path-migration-wave2-freepascal-tls13-early-data.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

暂不处理：

- `tests/contract/test_backend_contract.pas`
- `tests/test_mbedtls_connection_session_reused_contract.pas`
- `tests/test_openssl_connection_session_reused_contract.pas`
- `tests/winssl/test_winssl_session_resumption.pas`
- `tests/winssl/test_session_save_logic.pas`

## Why This Batch

wave 1 已经把普通生产路径和几份更小的 runtime tests 收到了
`ISSLSessionResumption` owner path，但当前 residual 里最大的 ordinary runtime 文件
还是 `tests/test_freepascal_tls13_early_data.pas`。

如果不单独把它收掉，后续每次审查 session-resumption runtime truth 时都会被这份文件反复拉起。

## Planned Changes

1. 在目标文件顶部 helper 区新增统一的 session-resumption owner-path helper。
2. 把目标文件里的 direct-core：
   - `GetSession`
   - `SetSession`
   - `IsSessionReused`
   迁成 helper 驱动的 `ISSLSessionResumption` 调用。
3. 新增 focused shell contract，锁住这份文件不再直接使用 core session mirrors。
4. 运行 focused compile / execute proof，确认迁移不引入行为回归。

## Verification

```bash
bash -n tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh
bash tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh
mkdir -p tmp/test_freepascal_tls13_early_data_owner_path && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_tls13_early_data_owner_path \
  -FEtmp/test_freepascal_tls13_early_data_owner_path \
  -otmp/test_freepascal_tls13_early_data_owner_path/test_freepascal_tls13_early_data \
  tests/test_freepascal_tls13_early_data.pas && \
  ./tmp/test_freepascal_tls13_early_data_owner_path/test_freepascal_tls13_early_data
git diff --check
```

## Expected Outcome

- `tests/test_freepascal_tls13_early_data.pas`
  里的 ordinary runtime session usage 统一走 `ISSLSessionResumption`
- 这条线的 residual 集合进一步收窄到：
  - backend semantic truth proofs
  - WinSSL runtime-specific residuals
- 后续 session-resumption 审查不再被这份大文件反复拉起
