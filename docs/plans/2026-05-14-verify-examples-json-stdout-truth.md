# 2026-05-14 Verify Examples JSON Stdout Truth

## Goal
收口 `scripts/verify_examples_compile.sh` 的 stdout 输出契约，避免调用者在使用 `-f json` 直出 stdout 时拿到混入 banner / 进度行 / `[PASS]` 日志的伪 JSON。

## Architecture
- `scripts/verify_examples_compile.sh` 当前同时承担两层职责：
  - 遍历 `examples/*.pas` 并输出人类可读进度
  - 根据 `-f text|json|markdown` 生成最终摘要
- 修复前这两层输出都走 stdout：
  - 顶部环境 banner：`FPC 版本`、`项目根目录`
  - 中间运行日志：`开始编译验证...`、`[PASS]/[FAIL]/[SKIP]`
  - 结尾格式化摘要：JSON / Markdown / text
- 这意味着：
  - `-f text` 仍然可读
  - `-f json` / `-f markdown` 在未使用 `-o` 时并不是纯格式输出
  - 机器消费者无法直接从 stdout 做 `json.load(...)`
- 这批不改计数、跳过规则或退出码，只把“进度日志”和“格式化结果”分流：
  - 非 text 且未写文件时，进度日志改走 stderr
  - stdout 保留为纯 JSON / Markdown

## Files
- `scripts/verify_examples_compile.sh`
- `tests/scripts/test_verify_examples_compile_json_stdout_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 新增 focused contract，用 fake `fpc` + 沙箱 examples 证明 `-f json` 当前 stdout 不是可解析 JSON。
2. 最小修改 `scripts/verify_examples_compile.sh` 的日志流向，不改 `summary.*` 与 exit code。
3. 复跑 focused contract，并补一个保底 shell 语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_verify_examples_compile_json_stdout_contract.sh
bash tests/scripts/test_verify_examples_compile_json_stdout_contract.sh
bash -n scripts/verify_examples_compile.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `python3 -c 'json.load(...)'` 无法解析 stdout 捕获文件
  - stdout 中能看到 `FPC 版本` 或 `[PASS]`
- 修复后：
  - stdout 是可解析 JSON
  - 进度/banner 仍保留，但改走 stderr
  - `summary.total/passed/failed/skipped/pass_rate` 和 exit code 语义不变。
