# Noninteractive WinSSL Active Tests

## Goal

把仍然属于 WinSSL 活跃测试面的测试程序收成真正适合自动化执行的非交互测试：

- `tests/unit/test_winssl_comprehensive.pas`
- `tests/winssl/test_winssl_context_comprehensive.pas`
- `tests/winssl/test_winssl_errors_comprehensive.pas`
- `tests/winssl/test_winssl_monitoring.pas`
- `tests/winssl/test_winssl_connection_edge_cases.pas`
- `tests/winssl/test_winssl_certstore.pas`
- `tests/winssl/test_winssl_session_management.pas`
- `tests/winssl/test_winssl_library_basic.pas`
- `tests/winssl/test_winssl_certificate_loading.pas`

## Why This Batch

上一批已经收掉顶层 core tests 的交互尾巴；repo-wide 扫描表明下一层残留主要集中在 WinSSL 专项测试、examples、diagnostics。

其中这批文件仍然属于“活跃测试程序”，而不是 examples / diagnostics / benchmark：

- `run_winssl_tests.ps1` 明确把 `tests/unit/test_winssl_comprehensive.pas` 归类为 `Minimal, non-network, non-interactive tests`
- `scripts/run_tests_windows.ps1` 仍试图自动编译运行 WinSSL 单元级测试
- 多个文件还有 `.lpi` / Windows validation bundle / checklist 入口

因此它们继续保留：

- `WriteLn('按回车键退出...')`
- `WriteLn('Press Enter to exit...')`
- `ReadLn`

会直接污染自动化执行路径，也会让 “non-interactive” 承诺失真。

## Deliverables

1. 移除这批 WinSSL 活跃测试程序末尾的交互式退出逻辑
2. 新增 focused shell contract，防止这些文件把交互尾巴重新带回
3. 在当前 Linux 环境可做的范围内补静态/可编译验证
4. 不混入 `examples` / `diagnostic` / benchmark 清理

## Files

- `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
- `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
- `tests/unit/test_winssl_comprehensive.pas`
- `tests/winssl/test_winssl_context_comprehensive.pas`
- `tests/winssl/test_winssl_errors_comprehensive.pas`
- `tests/winssl/test_winssl_monitoring.pas`
- `tests/winssl/test_winssl_connection_edge_cases.pas`
- `tests/winssl/test_winssl_certstore.pas`
- `tests/winssl/test_winssl_session_management.pas`
- `tests/winssl/test_winssl_library_basic.pas`
- `tests/winssl/test_winssl_certificate_loading.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash -n tests/scripts/test_winssl_active_tests_noninteractive_contract.sh
bash tests/scripts/test_winssl_active_tests_noninteractive_contract.sh

mkdir -p tmp/test_unit_winssl_comprehensive_nonwindows && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_unit_winssl_comprehensive_nonwindows \
  -FEtmp/test_unit_winssl_comprehensive_nonwindows \
  -otmp/test_unit_winssl_comprehensive_nonwindows/test_winssl_comprehensive \
  tests/unit/test_winssl_comprehensive.pas && \
  timeout 2 ./tmp/test_unit_winssl_comprehensive_nonwindows/test_winssl_comprehensive

git diff --check
```

## Expected Outcome

- 这批 WinSSL 活跃测试程序不再要求人工按回车退出
- `run_winssl_tests.ps1` 的 non-interactive 意图与源码重新对齐
- examples / diagnostics / benchmark 保持留待后续单独分层处理
