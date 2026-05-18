# Noninteractive Top-Level Core Tests

## Goal

把两份仍然承担顶层 core contract coverage 的测试程序收成真正适合自动化执行的非交互测试：

- `tests/test_exceptions.pas`
- `tests/test_base_interface_contract.pas`

## Why This Batch

这两份文件都属于顶层 core test：

- `test_exceptions.pas` 负责异常层级与构造语义
- `test_base_interface_contract.pas` 负责 base unit 的接口/常量/record-shape 契约

它们当前已经能在无 stdin 场景下退出，但源码末尾仍保留：

- `WriteLn('按回车键退出...')`
- `ReadLn`

这会继续让自动化测试带着手工演示尾巴，也让“是否需要人工输入”依赖运行方式而不是测试本身。

## Deliverables

1. 移除两份顶层 core test 末尾的交互式退出逻辑
2. 新增 focused shell contract，防止这两份文件把交互尾巴重新带回
3. focused 重新编译并直接执行，确认输出只保留测试总结

## Files

- `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
- `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
- `tests/test_exceptions.pas`
- `tests/test_base_interface_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash -n tests/scripts/test_top_level_core_tests_noninteractive_contract.sh
bash tests/scripts/test_top_level_core_tests_noninteractive_contract.sh

mkdir -p tmp/test_exceptions && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_exceptions \
  -FEtmp/test_exceptions \
  -otmp/test_exceptions/test_exceptions \
  tests/test_exceptions.pas && \
  timeout 2 ./tmp/test_exceptions/test_exceptions

mkdir -p tmp/test_base_interface_contract && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_base_interface_contract \
  -FEtmp/test_base_interface_contract \
  -otmp/test_base_interface_contract/test_base_interface_contract \
  tests/test_base_interface_contract.pas && \
  timeout 2 ./tmp/test_base_interface_contract/test_base_interface_contract

git diff --check
```

## Expected Outcome

- 两份顶层 core test 都能直接作为自动化测试程序运行
- 不再输出“按回车键退出...”这类手工提示
- 这条修复被 focused shell contract 固定下来，不会被后续回退
