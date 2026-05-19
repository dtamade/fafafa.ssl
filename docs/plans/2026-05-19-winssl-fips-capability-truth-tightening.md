# WinSSL FIPS Capability Truth Tightening

## Goal

收紧 `WinSSL` 当前对外发布的 `SupportsFIPSMode` truth，把“Windows 系统 FIPS policy 检测 helper”与“fafafa.ssl 已发布 backend capability”明确拆开，避免 selector / security score / active docs 因假阳性 capability 持续误导。

## Architecture

这批只做当前已经能静态压实的问题：

- `src/fafafa.ssl.winssl.lib.pas`
  - 收紧 `SupportsFIPSMode`
- 活跃文档
  - 不再把 WinSSL 写成当前已发布 `FIPS = ✅` backend capability
  - 保留并澄清 `fafafa.ssl.winssl.enterprise` 只是系统 FIPS policy / GPO / enterprise helper
- focused contracts
  - 静态 source/doc truth contract
  - 轻量 capability contract

这批不做：

- 新增 WinSSL FIPS runtime enable/disable API
- 新增专门的 Windows FIPS runtime proof harness
- 重开更大的 selector 评分体系重构

## Why This Batch

当前静态复核已经确认：

- `src/fafafa.ssl.winssl.lib.pas`
  - 仍在发布：
    - `SupportsFIPSMode := True`
- 但源码里能找到的 WinSSL FIPS 相关实现主要是：
  - `src/fafafa.ssl.winssl.enterprise.pas`
    - `IsFIPSModeEnabled`
    - `TSSLEnterpriseConfig.IsFIPSEnabled`
- 这条线目前体现的是：
  - Windows 系统 policy / 注册表检测 helper
  - 而不是 fafafa.ssl 已发布的 backend runtime/control surface

如果继续保留旧 capability：

- backend selector 会把 WinSSL 当作已满足 FIPS 偏好
- security score / reason text 会继续把 WinSSL写成已支持 FIPS
- 多份 active docs 会继续把“系统可检测/可遵循 FIPS policy”误教成：
  - `ISSLLibrary.GetCapabilities.SupportsFIPSMode=True`

## Files

- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `tests/scripts/test_active_fips_docs_truth_contract.sh`
- Modify: `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
- Add: `tests/test_backend_fips_capability_truth_contract.pas`
- Modify: `docs/reference/WINSSL_DESIGN.md`
- Modify: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/WINSSL_USER_GUIDE.md`
- Modify: `docs/PLATFORM_SUPPORT.md`
- Modify: `docs/reference/BACKEND_SELECTOR_DESIGN.md`
- Modify: `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
- Modify: `docs/reference/API_REFERENCE.md`
- Modify: `docs/MIGRATION_GUIDE_V1.1.md`
- Modify: `docs/guides/MIGRATION_GUIDE.md`
- Modify: `docs/guides/USER_GUIDE.md`
- Modify: `docs/guides/TROUBLESHOOTING.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 先把 focused FIPS source/doc contracts 收紧到当前预期 truth。
2. 跑 RED，确认当前 WinSSL FIPS capability 假阳性和 active docs 漂移确实还存在。
3. 最小修源码与活跃文档，明确 enterprise helper 与 published capability 的边界。
4. 跑 focused 验证并同步 planning files。
5. 简短 review 结论后提交并推送。

## Verification

```bash
bash -n tests/scripts/test_active_fips_docs_truth_contract.sh
bash tests/scripts/test_active_fips_docs_truth_contract.sh
bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_fips_capability_truth -FEtmp/test_backend_fips_capability_truth -otmp/test_backend_fips_capability_truth/test_backend_fips_capability_truth_contract tests/test_backend_fips_capability_truth_contract.pas
./tmp/test_backend_fips_capability_truth/test_backend_fips_capability_truth_contract
git diff --check
```

## Expected Outcome

- `WinSSL` 不再把系统 FIPS policy/helper 检测发布成 `SupportsFIPSMode=True`
- active docs 不再把 WinSSL 讲成当前 FIPS published backend
- enterprise helper 文档继续保留，但会明确：
  - 这是 policy / helper 检测
  - 不是当前 `ISSLLibrary.GetCapabilities` capability truth
