# GitHub Actions CI/CD

本仓库以“可重复 + 可审查”为优先。当前默认执行控制面已经切到 `post-release route selection`：`v1.5.0` 已正式发布，先看 roadmap、release readiness、已发布 workflow truth 和下一条产品主线，再决定是否需要新的门禁执行。

---

## ✅ 当前启用的工作流

### 1) `ci.yml`（默认启用）

Linux 最小门禁（push / PR 自动触发）

- 入口：`bash scripts/run_minimal_ci_gate.sh --fast-local`
- 产物：`tmp/test-reports/`（artifact 上传）
- 覆盖：compile gate + P2 核心模块回归 + Phase2 baseline dry-run

### 2) `ci.yml` 中的 FreePascal TLS 1.3 focused gate

FreePascal TLS 1.3 completeness 主线门禁（push / PR 自动触发）

- 入口：`bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- 产物：`tmp/test-reports/freepascal_tls13_completeness_*`
- 覆盖：`test_tls13_posthandshake`、`test_tls13_resumption`、`test_tls13_clienthello_parser`、`test_tls13_servercertverify`、`test_freepascal_client_certificateverify_runtime`、client/server resumption、`test_freepascal_tls13_early_data`、backend basic、capability cache

### 3) `release.yml`

当前 v1.5.0 release workflow（tag `v1.5.0` / `workflow_dispatch`）

- 入口：`.github/workflows/release.yml`
- 覆盖：版本真值校验、compile gate、minimal gate、FreePascal TLS 1.3 focused gate、style gate、Phase 2 dry-run、`RELEASE_NOTES_V1.5.0.md`
- 最新已确认真相：run `25991977801` 在 head `e775ac5` 上 `SUCCESS`，并已发布 `https://github.com/dtamade/fafafa.ssl/releases/tag/v1.5.0`
- 产物：source archive + GitHub release body

### 4) `tls13-signer-gate.yml`

TLS 1.3 signer 专项门禁（按路径触发 + `workflow_dispatch`）

- 入口：`bash scripts/run_tls13_signer_gate_bundle.sh --strict`
- 产物：`test-reports/` + `artifacts/ci/`

### 5) `wave-b-b2-manual.yml`

Wave B/B2 跨平台手动门禁（`workflow_dispatch`）

- 用途：Linux/macOS/Windows 证据回填 + cross summary + closure/consistency
- Windows lane 现在会先安装并验证 Lazarus / `lazbuild`，再跑 quick smoke、Wave B Windows gate、broader WinSSL suite transcript
- 可选输入 `winssl_session_host` 可把 Windows broader WinSSL runtime suite 的 session-resumption 调查切到指定 host；留空时继续使用测试程序内置默认 host
- 最新已确认真相：manual run `25989095571` 在 head `b95044d` 上的 `windows-gate` / `macos-gate` / `linux-gate` / `summary` 全部 `SUCCESS`
- 当前不应为“再确认一次”重复派发这条 workflow；只有当提交改动可能影响它的运行时边界时才重新 dispatch
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
- 查看已发布 `v1.5.0` 的 release truth：先看 `docs/test_reports/RELEASE_READINESS_V1.5.0.md`，再看 `release.yml`
- 触及 TLS13 signer：额外关注 `tls13-signer-gate.yml`
- 需要新的跨平台证据：只在当前 head 尚无 fresh green proof，或本批改动可能影响跨平台运行时时，再手动触发 `wave-b-b2-manual.yml`
