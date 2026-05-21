# 2026-05-21 Active Custom Cipher Guidance Truth Alignment

## Goal

继续沿着“字段存在，但 backend runtime 语义不同”的主线推进，收掉 generic active docs 里仍把
`SetCipherList(...)` / `SetCipherSuites(...)` 当成普通跨后端配置入口的 drift。

当前 shipped/runtime truth 已经明确：

- `SupportsCustomCipherSuites=True`
  当前主要是
  `OpenSSL`
- `FreePascal` / `WinSSL` / `MbedTLS` / `WolfSSL`
  对 custom non-default cipher override
  当前都应该 fail-closed

但多个活跃入口页仍在把 custom cipher 配置写成 generic 推荐用法，会继续误导：

- 普通新代码该优先走
  `WithSafeDefaults`
  /
  shipped baseline defaults
- custom cipher allowlist / denylist
  只该在
  `SupportsCustomCipherSuites=True`
  的 backend 上使用

## Architecture

- runtime truth
  - `src/fafafa.ssl.base.pas`
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/API_REFERENCE.md`
- active docs truth
  - `README.md`
  - `docs/reference/API_DOCUMENTATION.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/SECURITY_GUIDE.md`
  - `docs/guides/security-best-practices.md`
  - `docs/guides/SECURITY_AUDIT.md`
- focused verification
  - `tests/scripts/test_active_custom_cipher_guidance_truth_contract.sh`

## TDD

### RED

1. 新增 focused docs-truth contract，锁住：
   - generic docs 不再把 custom cipher override 当成跨后端普适推荐
   - active docs 必须明确：
     - 普通跨后端路径优先 shipped baseline / `WithSafeDefaults`
     - 只有在 `SupportsCustomCipherSuites=True` 的 backend 上才配置 custom cipher
   - `API_REFERENCE.md` 的 direct-library default-config note 需要补这条 backend-gated caveat
2. 运行 contract，确认当前活跃 generic docs 失败。

### GREEN

- 只修 active docs / guidance truth
- 不改 runtime code
- generic docs 统一切到 capability-aware / backend-gated 表述

### REGRESSION

- `bash -n tests/scripts/test_active_custom_cipher_guidance_truth_contract.sh`
- `bash tests/scripts/test_active_custom_cipher_guidance_truth_contract.sh`
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-21-active-custom-cipher-guidance-truth-alignment.md`
- Add: `tests/scripts/test_active_custom_cipher_guidance_truth_contract.sh`
- Update: `README.md`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/reference/API_DOCUMENTATION.md`
- Update: `docs/guides/USER_GUIDE.md`
- Update: `docs/guides/TROUBLESHOOTING.md`
- Update: `docs/guides/SECURITY_GUIDE.md`
- Update: `docs/guides/security-best-practices.md`
- Update: `docs/guides/SECURITY_AUDIT.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_active_custom_cipher_guidance_truth_contract.sh`
2. `bash tests/scripts/test_active_custom_cipher_guidance_truth_contract.sh`
3. `git diff --check`

## Expected Outcome

- generic active docs 不再把 custom cipher override 写成“随手可配”的默认推荐
- 当前总路线图会更清楚地区分：
  - cross-backend baseline guidance
  - backend-gated advanced tuning surface
- “字段存在” 与 “所有 backend 都支持同样 runtime 语义” 之间的边界再往前收一层
