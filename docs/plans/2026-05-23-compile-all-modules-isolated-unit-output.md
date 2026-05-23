# 2026-05-23 Compile All Modules Isolated Unit Output

## Goal

把 `scripts/compile_all_modules.py` 的批量编译门禁改成每个 unit 独立 `-FU` 输出目录，避免共享单元产物污染后续编译并触发 Free Pascal 内部 AV。

## Scope

- `scripts/compile_all_modules.py`
- `tests/scripts/test_compile_all_modules_unit_output_isolation_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `compile_all_modules.py` 是仓库当前默认 Linux 编译门禁。
- 该脚本原先使用一个共享 `-FU` 临时目录编译全部 unit。
- 在本机，`fafafa.ssl.pkcs11.engine.pas` 会在共享输出目录的 batch 流程中触发 FPC internal exception，但单独编译是绿的。
- 将每个 unit 的输出目录拆成独立子目录后，批量编译恢复稳定，且不改变单个 unit 的编译命令语义。

## Steps

1. 复现 `python3 scripts/compile_all_modules.py` 的共享输出目录失败。
2. 在 `compile_module(...)` 中为每个 `.pas` 文件构造独立的 `-FU` 子目录。
3. 用契约测试锁定 `compile_module(...)` 的 per-unit 输出隔离。
4. 重跑 `python3 scripts/compile_all_modules.py`，确认 100% 通过。

## Verification

- `bash tests/scripts/test_compile_all_modules_unit_output_isolation_contract.sh`
- `python3 -m py_compile scripts/compile_all_modules.py`
- `python3 -u scripts/compile_all_modules.py`
- `git diff --check`

## Outcome

- `scripts/compile_all_modules.py` 现在对每个 unit 使用独立的输出子目录。
- `python3 scripts/compile_all_modules.py` 现已稳定通过 `186/186`。
- `fafafa.ssl.pkcs11.engine.pas` 不再在批量门禁中触发 internal exception。
