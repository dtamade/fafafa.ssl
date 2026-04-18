# GitHub Actions CI/CD

本仓库以“可重复 + 可审查”为优先，默认启用 Linux 最小门禁，并额外保留一条更贴近 pure Pascal 主线的 FreePascal TLS 1.3 focused gate；跨平台与专项门禁按需启用或手动触发。

---

## ✅ 当前启用的工作流

### 1) `ci.yml`（默认启用）
Linux 最小门禁（push / PR 自动触发）

- 入口：`bash scripts/run_minimal_ci_gate.sh --fast-local`
- 产物：`tmp/test-reports/`（artifact 上传）
- 覆盖：compile gate + P2 核心模块回归 + Phase2 baseline dry-run

### 2) `tls13-signer-gate.yml`
TLS 1.3 signer 专项门禁（按路径触发 + `workflow_dispatch`）

- 入口：`bash scripts/run_tls13_signer_gate_bundle.sh --strict`
- 产物：`test-reports/` + `artifacts/ci/`

### 3) `ci.yml` 中的 FreePascal TLS 1.3 focused gate
FreePascal TLS 1.3 completeness 主线门禁（push / PR 自动触发）

- 入口：`bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- 产物：`tmp/test-reports/freepascal_tls13_completeness_*`
- 覆盖：`test_tls13_posthandshake`、`test_tls13_resumption`、`test_tls13_clienthello_parser`、`test_tls13_servercertverify`、`test_freepascal_client_certificateverify_runtime`、client/server resumption、`test_freepascal_tls13_early_data`、backend basic、capability cache

### 4) `wave-b-b2-manual.yml`
Wave B/B2 跨平台手动门禁（`workflow_dispatch`）

- 用途：Linux/macOS/Windows 证据回填 + cross summary + closure/consistency
- 产物：`test-reports/`（各平台摘要 + 汇总）

---

## ⏸ 默认禁用的模板/草案

以下 workflow 以 `.disabled` 后缀保留为模板，默认不执行：

- `.github/workflows/test-all-platforms.yml.disabled`（全量多平台模板）
- `.github/workflows/ci-matrix-draft.yml.disabled`（多平台矩阵草案）
- `.github/workflows/phase_c_tests.yml.disabled`（历史 Phase C workflow）

启用方式（需要提交到仓库并 push）：

```bash
mv .github/workflows/<file>.yml.disabled .github/workflows/<file>.yml
git add .github/workflows/<file>.yml
```

---

## 🎯 建议使用方式

- 日常开发：依赖 `ci.yml`（Minimal Gate）
- 触及 pure Pascal TLS 1.3 主线：同时关注 `ci.yml` 里的 FreePascal focused gate
- 触及 TLS13 signer：额外关注 `tls13-signer-gate.yml`
- 需要跨平台证据：手动触发 `wave-b-b2-manual.yml`
