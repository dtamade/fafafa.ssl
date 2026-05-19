# Active FIPS Docs Truth Sweep

## Goal

收掉当前仍会误导后续开发的 active docs FIPS truth 漂移，重点对齐 3 个高可见入口：

- `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
- `docs/reference/BACKEND_SELECTOR_DESIGN.md`
- `docs/PLATFORM_SUPPORT.md`

## Scope

这批只改 active docs 和静态合同：

- 不改生产代码
- 不重开 WinSSL enterprise/runtime 证明
- 不把 OpenSSL “可通过特殊构建/模块实现 FIPS”误写成当前默认 backend capability

## Why This Batch

静态复审已确认：

- `BACKEND_ABSTRACTION_LAYER_DESIGN.md` 仍写 `OpenSSL FIPS = ✅`
- `BACKEND_SELECTOR_DESIGN.md` 仍写 `OpenSSL FIPS = ✅`
- `PLATFORM_SUPPORT.md` 仍把 OpenSSL / WinSSL 对比写成两边都“FIPS 模式支持”

但当前 source truth 已明确：

- `src/fafafa.ssl.openssl.backed.pas`
  - `SupportsFIPSMode := False`
- `src/fafafa.ssl.winssl.lib.pas`
  - `SupportsFIPSMode := True`

## Files

- Add: `tests/scripts/test_active_fips_docs_truth_contract.sh`
- Modify: `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
- Modify: `docs/reference/BACKEND_SELECTOR_DESIGN.md`
- Modify: `docs/PLATFORM_SUPPORT.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Verification

```bash
bash -n tests/scripts/test_active_fips_docs_truth_contract.sh
bash tests/scripts/test_active_fips_docs_truth_contract.sh
git diff --check
```

## Expected Outcome

- active docs 不再把 OpenSSL 默认 backend truth 写成 `FIPS = ✅`
- active docs 明确区分：
  - OpenSSL 默认构建 capability 不发布
  - WinSSL 当前 capability 发布 `FIPS = ✅`
