# 2026-03-14 CI Workflows Consolidation + Fast-local Cleanup

## Goal

- 让仓库的 GitHub Actions 工作流与“本地最小门禁”口径一致，默认低成本、可重复、可审查。
- 将历史全量/草案 workflow 全部收敛为 **`.disabled` 模板**，避免误触发与维护漂移。
- 增加一个安全的本地清理入口，专门处理 `--fast-local` 相关产物，保持工作区干净。

## Non-goals

- 不在本批次启用全量多平台矩阵（保留为 `.disabled` 模板，按需启用）。
- 不重写既有测试/模块逻辑，只做 workflow/文档/清理工具的收口与契约保护。

## Scope

### Workflows

- Kept enabled:
  - `.github/workflows/ci.yml`
  - `.github/workflows/tls13-signer-gate.yml`
  - `.github/workflows/wave-b-b2-manual.yml`
- Disabled (kept as templates):
  - `.github/workflows/test-all-platforms.yml.disabled`
  - `.github/workflows/ci-matrix-draft.yml.disabled`
  - `.github/workflows/phase_c_tests.yml.disabled`

### Docs

- `.github/README.md`
- `.github/GITHUB_ACTIONS_GUIDE.md`
- `.github/BASIC_CI_GUIDE.md`
- `.github/PRIVATE_REPO_GUIDE.md`
- `docs/DOCUMENTATION_INDEX.md`
- `docs/PLATFORM_SUPPORT.md`
- `docs/DEVELOPMENT_ROADMAP_2026.md`
- `docs/plans/*` references updated to `.disabled` where applicable

### Tooling

- `scripts/cleanup_fast_local_outputs.sh`
- Contract: `tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh`

## Architecture / Policy

1. **Default CI = Linux minimal gate**
   - CI 只跑 `scripts/run_minimal_ci_gate.sh --fast-local`（对齐本地 smoke）
   - 产物全部落在 `./tmp` 并上传 artifact
2. **Specialized gates are opt-in**
   - TLS13 signer 使用单独的 `tls13-signer-gate.yml`（path filter + 手动触发）
   - B2 跨平台证据回填使用 `wave-b-b2-manual.yml`（手动）
3. **Draft workflows must not run by accident**
   - 草案/模板统一用 `.yml.disabled` 后缀保存

## Step-by-step (Commands)

### 1) Local contracts (scripts)

```bash
bash -n scripts/cleanup_fast_local_outputs.sh
bash tests/scripts/test_cleanup_fast_local_outputs_safe_defaults_contract.sh

bash -n scripts/run_minimal_ci_gate.sh scripts/run_all_module_tests.sh scripts/run_wave_b_ci_gate.sh scripts/run_tls13_signer_gate_ci.sh

bash tests/scripts/test_run_all_module_tests_dry_run_paths_contract.sh
bash tests/scripts/test_run_all_module_tests_timeout_portability_contract.sh
bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh
```

Expected:
- all commands exit `0`

### 2) Minimal smoke (local)

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local --skip-compile --skip-phase2-dryrun --modules PKCS7 --verbose
```

Expected:
- module tests pass (`2/2`) and report file appears under `tmp/test-reports/`

### 3) Optional cleanup (local)

Dry-run:

```bash
bash scripts/cleanup_fast_local_outputs.sh --older-than-days 30
```

Apply:

```bash
bash scripts/cleanup_fast_local_outputs.sh --older-than-days 30 --apply
```

Expected:
- dry-run does not delete anything
- apply only removes known fast-local outputs under `./tmp`

## Notes

- 如果需要重新启用某个 `.disabled` workflow：移除后缀并提交即可（见 `.github/GITHUB_ACTIONS_GUIDE.md`）。
