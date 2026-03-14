# GitHub Actions 基础检查（可选）

本仓库默认启用的是 **Linux minimal gate**（见 `.github/workflows/ci.yml`），它会安装 FPC/OpenSSL 并执行 `scripts/run_minimal_ci_gate.sh`。

如果你在某些场景需要一个 **完全不依赖 FPC 编译** 的超轻量检查，可以启用 `basic-checks.yml.disabled` 作为补充（默认禁用）。

---

## 当前默认门禁（推荐）

- 工作流：`.github/workflows/ci.yml`
- 入口：`bash scripts/run_minimal_ci_gate.sh --fast-local`
- 产物：`tmp/test-reports/`

---

## 启用基础检查（可选）

启用方式（需要提交到仓库并 push）：

```bash
mv .github/workflows/basic-checks.yml.disabled .github/workflows/basic-checks.yml
git add .github/workflows/basic-checks.yml
```

禁用方式：

```bash
mv .github/workflows/basic-checks.yml .github/workflows/basic-checks.yml.disabled
git add .github/workflows/basic-checks.yml.disabled
```
