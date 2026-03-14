# GitHub Actions 使用指南（当前口径）

本仓库当前以“低成本、可重复、证据可审查”为优先，工作流分为：

- **默认自动门禁（Linux）**
- **专项门禁（TLS13 signer）**
- **跨平台手动回填（Wave B/B2）**
- **模板/草案（默认禁用，`.disabled`）**

---

## 1) 当前启用的工作流

| 工作流文件 | 触发 | 用途 | 主要产物 |
|---|---|---|---|
| `.github/workflows/ci.yml` | push / PR | Linux minimal gate（默认门禁） | `tmp/test-reports/` |
| `.github/workflows/tls13-signer-gate.yml` | path-filter + 手动 | TLS13 signer purity/bench/snapshot | `test-reports/`, `artifacts/ci/` |
| `.github/workflows/wave-b-b2-manual.yml` | 手动 | 三平台证据回填 + 汇总/闭环判定 | `test-reports/` |

---

## 2) 如何运行（Web UI）

GitHub 仓库 → **Actions**：

1. 选择工作流（CI / TLS13 Signer Gate / Wave B B2 Manual Gate）
2. 点击 **Run workflow**
3.（可选）填写 inputs（TLS13 signer bench 参数 / Wave B run_id 等）
4. 等待完成后，从 run 页面下载 artifacts

---

## 3) 如何运行（GitHub CLI）

> 前提：已安装并登录 `gh`。

```bash
# 运行默认 Linux minimal gate
gh workflow run ci.yml

# 运行 TLS13 signer gate（可选覆盖 bench 参数）
gh workflow run tls13-signer-gate.yml -f bench_iterations=2 -f bench_warmup=1 -f bench_scheme=rsa_pkcs1_sha256 -f bench_timeout=180

# 运行 Wave B/B2 跨平台手动门禁（可选 run_id）
gh workflow run wave-b-b2-manual.yml -f run_id="20260314_120000"
```

---

## 4) 默认禁用的模板/草案

这些文件以 `.disabled` 后缀保留为模板，默认不执行：

- `.github/workflows/test-all-platforms.yml.disabled`
- `.github/workflows/ci-matrix-draft.yml.disabled`
- `.github/workflows/phase_c_tests.yml.disabled`
- 以及其它 `*.yml.disabled`

启用方式（需要提交到仓库并 push）：

```bash
mv .github/workflows/<file>.yml.disabled .github/workflows/<file>.yml
git add .github/workflows/<file>.yml
git commit -m "chore(ci): enable <file> workflow"
git push
```

---

## 5) 排障建议

- 本地优先：先跑 `bash scripts/run_minimal_ci_gate.sh --fast-local`，再看 CI 产物对齐。
- `--fast-local` 相关脚本默认输出到 `./tmp`，避免污染 git 工作区。
