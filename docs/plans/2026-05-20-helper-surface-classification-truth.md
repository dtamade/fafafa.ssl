# Helper Surface Classification Truth（2026-05-20）

## Goal
- 把当前 shipped helper surfaces 的权威分级说明收回到同一张图：
  - 哪些还是当前 TLS bootstrap 主入口
  - 哪些只是 facade convenience helpers
  - 哪些 WinSSL enterprise helpers 已经有新的主路径，但旧全局 wrapper 仍仅作 compatibility/convenience

## Why now
- `src/fafafa.ssl.pas` 仍导出：
  - `CreateDefaultConfig`
  - `QuickServer`
  - `CreateOCSPClient`
  - `CreateCRLManager`
  - `TSSLHelper`
- 但 active canonical docs 对这组 surface 还没有统一分级说明。
- `docs/reference/API_REFERENCE.md` 里更具体的漂移是：
  - `WinSSL 企业工具` 小节仍把
    `IsFIPSModeEnabled` / `GetEnterpriseTrustedRoots`
    摆成主入口
  - 而 `docs/guides/MIGRATION_GUIDE.md` / `docs/guides/USER_GUIDE.md`
    已经把当前 helper 主路径收到了
    `TSSLEnterpriseConfig.IsFIPSEnabled` /
    `GetTrustedRoots` / `GetAllPolicies`

## Scope
- `docs/reference/API_REFERENCE.md`
- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.factory.pas`
- `tests/scripts/test_helper_surface_classification_truth_contract.sh`
- `docs/plans/2026-05-20-helper-surface-classification-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不删除 `TSSLHelper` / `QuickServer` / `CreateOCSPClient` / `CreateCRLManager`。
- 不改 WinSSL enterprise 生产实现。
- 不重开 broader facade slimming / v2 API surgery。

## Approach
1. 新增 focused shell contract，冻结：
   - `TSSLFactory.GetLibraryInstance(...)` / connector surfaces 是 TLS bootstrap 主入口
   - `TSSLHelper` / `QuickServer` / `CreateOCSPClient` / `CreateCRLManager`
     只是 convenience helpers
   - `WinSSL enterprise` 当前主路径是 `TSSLEnterpriseConfig` methods
   - 旧全局 enterprise wrappers 只作为 legacy convenience wrappers 记录
2. 更新 canonical `API_REFERENCE`：
   - 增加 facade helper surface classification 小节
   - 修正 WinSSL enterprise helper 主路径说明
3. 更新 facade source comments：
   - `src/fafafa.ssl.pas`
   - `src/fafafa.ssl.factory.pas`
   让 exported helper 的 shipped truth 在源码入口也一致

## Commands
```bash
bash -n tests/scripts/test_helper_surface_classification_truth_contract.sh
bash tests/scripts/test_helper_surface_classification_truth_contract.sh

bash tests/scripts/test_migration_guide_active_truth_contract.sh

git diff --check
```

## Expected Outputs
- canonical API docs and source comments agree on helper-surface layering
- WinSSL enterprise helper main path no longer drifts between API reference and migration/user guides
- future drift back to “all exported helpers are equal entrypoints” trips a focused contract

## Execution Result
- completed
- local proof:
  - `bash -n tests/scripts/test_helper_surface_classification_truth_contract.sh` PASS
  - `bash tests/scripts/test_helper_surface_classification_truth_contract.sh` PASS
  - `bash tests/scripts/test_migration_guide_active_truth_contract.sh` PASS
  - `bash tests/scripts/test_active_fips_docs_truth_contract.sh` PASS
  - `git diff --check` PASS
- closeout:
  - canonical `API_REFERENCE` now classifies:
    - `TSSLFactory.GetLibraryInstance(...)` / connector surfaces as bootstrap main entry
    - `CreateDefaultConfig` / `TSSLHelper` / `QuickServer` / `CreateOCSPClient` / `CreateCRLManager`
      as convenience helpers
  - `WinSSL enterprise` section now uses
    - `TSSLEnterpriseConfig.IsFIPSEnabled`
    - `GetTrustedRoots`
    - `GetAllPolicies`
    as main path
  - old globals
    - `IsFIPSModeEnabled(...)`
    - `GetEnterpriseTrustedRoots(...)`
    are now recorded only as legacy convenience wrappers
