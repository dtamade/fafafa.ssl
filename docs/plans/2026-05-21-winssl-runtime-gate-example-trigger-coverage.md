# WinSSL Runtime Gate Example Trigger Coverage

## Goal

把 `WinSSL Runtime Gate`
从“会覆盖 WinSSL backend / shared-core / tests / scripts 改动”
补齐到当前更真实的自动证明边界：

- 当前活跃的 WinSSL 示例源码改动
  也必须触发 Windows runtime lane

避免继续出现：

- 提交确实改到了
  `examples/winssl_*`
  这类 Windows-facing 示例
- Linux `CI` 绿色
- 但 `WinSSL Runtime Gate`
  根本没有自动建单

## Scope

- Update:
  - `.github/workflows/winssl-tests.yml`
  - `.github/workflows/winssl-tests.yml.disabled`
  - `tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - `.github/README.md`
  - `task_plan.md`
- Add:
  - `docs/plans/2026-05-21-winssl-runtime-gate-example-trigger-coverage.md`

不做：

- 不扩大到所有 `examples/**`
- 不改 workflow runtime steps
- 不重开 broader Windows/manual workflow lane

## Why This Batch

当前 workflow 已经覆盖：

- `src/fafafa.ssl.winssl*.pas`
- 一批 shared/core units
- `tests/winssl/**`
- WinSSL runtime scripts

但它还没有覆盖：

- `examples/*winssl*.pas`

而本轮刚好已经拿到 live evidence：

- commit
  `examples: align active public imports`
  修改了
  `examples/winssl_health_checker.pas`
  /
  `examples/winssl_rest_client.pas`
- push 后只出现
  通用 `CI`
  run
- `WinSSL Runtime Gate`
  没有自动建单

这说明当前缺口不是理论担忧，
而是已经实际发生的 workflow trigger omission。

## Minimal Fix

1. 把 `examples/*winssl*.pas`
   加入
   active + disabled
   WinSSL workflow 的 push / PR path filter
2. 收紧 existing workflow contract，
   锁住这条新 truth
3. 同步 `.github/README.md`
   的 WinSSL gate 用途说明
4. push 后观察新的
   `WinSSL Runtime Gate`
   run

## Verification

```bash
bash -n tests/scripts/test_workflow_winssl_tests_truth_contract.sh
bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh
git diff --check
gh run list --workflow "WinSSL Runtime Gate" --branch master --limit 3
```

## Expected Outcome

- 以后改动活跃 WinSSL 示例源码时，
  会自动拉起 Windows runtime lane
- WinSSL 自动 gate 的 path coverage
  更贴近当前真实 public/runtime surface
- 触发面仍保持 bounded，
  不会退化成所有 examples 改动都跑 Windows

## Execution Result

- PASS
- focused RED 首轮证明的是
  真实 workflow trigger omission，
  不是
  GitHub 列表刷新延迟：
  - `HEAD` 快照下
    `test_workflow_winssl_tests_truth_contract.sh`
    会直接因为缺少
    `examples/*winssl*.pas`
    而失败
- 最小修复后：
  - `.github/workflows/winssl-tests.yml`
  - `.github/workflows/winssl-tests.yml.disabled`
    现已把
    `examples/*winssl*.pas`
    纳入 push / PR trigger coverage
  - `.github/README.md`
    也已同步说明：
    WinSSL 自动 gate
    当前覆盖
    活跃 WinSSL 示例源码
- focused verification：
  - `bash -n tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
    - PASS
  - `HEAD` snapshot contract
    - FAIL
  - `bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
- push 后需继续观察：
  - 新的
    `WinSSL Runtime Gate`
    run
    是否自动建单
