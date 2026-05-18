# Noninteractive Core Compat Tests

## Goal

把两份仍然承担核心 `TSSLConfig` record-shape / compatibility coverage 的老测试程序收成真正适合自动化执行的非交互测试：

- `tests/test_factory_logic.pas`
- `tests/test_data_structures.pas`

## Why This Batch

这两份文件当前已经承担了明确的 compatibility / record-shape 覆盖：

- deprecated `TSSLConfig.ServerName`
- option-bridge booleans
- mixed-scope 字段可见性（例如 `BufferSize` / `HandshakeTimeout`）

但它们在运行结束时仍然保留：

- `WriteLn('按回车键退出...')`
- `ReadLn`

这虽然在当前无 stdin 场景下不会卡死，但会继续把核心测试伪装成“手工演示程序”，也会给自动化输出留下无意义交互尾巴。

## Deliverables

1. 移除这两份核心测试末尾的交互式退出逻辑
2. 补清头部 `INTENTIONAL_COMPAT` 注释，明确 mixed-scope record-shape coverage 范围
3. focused 重新编译并直接执行，不再依赖 stdin 注入

## Files

- `docs/plans/2026-05-18-noninteractive-core-compat-tests.md`
- `tests/test_factory_logic.pas`
- `tests/test_data_structures.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
mkdir -p tmp/test_factory_logic && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_factory_logic \
  -FEtmp/test_factory_logic \
  -otmp/test_factory_logic/test_factory_logic \
  tests/test_factory_logic.pas && \
  timeout 2 ./tmp/test_factory_logic/test_factory_logic

mkdir -p tmp/test_data_structures && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_data_structures \
  -FEtmp/test_data_structures \
  -otmp/test_data_structures/test_data_structures \
  tests/test_data_structures.pas && \
  timeout 2 ./tmp/test_data_structures/test_data_structures

git diff --check
```

## Expected Outcome

- 两份核心兼容/结构测试都能直接作为自动化测试程序运行
- 不再输出“按回车键退出...”这类手工演示尾巴
- mixed-scope record-shape coverage 的意图在文件头部更清晰
