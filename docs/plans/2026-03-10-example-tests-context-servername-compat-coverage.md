# 2026-03-10 example tests context ServerName compat coverage

## Goal
- 明确 `tests/examples/test_basic.pas` 与 `tests/examples/test_lib_core_functionality.pas` 里保留的 context-level `SetServerName(...)` 是**兼容 API 覆盖**，不是推荐写法。
- 防止后续维护者把它们误当成应继续推广的示例路径。

## Scope
- `tests/examples/test_basic.pas`
- `tests/examples/test_lib_core_functionality.pas`
- `tests/scripts/test_examples_context_server_name_compat_coverage_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 判定两个入口的角色
- [x] 新增 focused shell contract
- [x] 加显式兼容覆盖标记
- [x] 跑 contract 与 compile smoke
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n tests/scripts/test_examples_context_server_name_compat_coverage_contract.sh && bash tests/scripts/test_examples_context_server_name_compat_coverage_contract.sh` => PASS
- `fpc -Fu./src -Fi./src tests/examples/test_basic.pas -otmp/test_examples_test_basic_smoke` => PASS（warnings only）
- `fpc -Fu./src -Fi./src tests/examples/test_lib_core_functionality.pas -otmp/test_examples_test_lib_core_smoke` => existing FAIL（`TCryptoUtils.Base64Encode/Base64DecodeString` 缺失，非本波引入）

## Result
- 两个文件现在都明确标记为 “Deprecated compatibility coverage”。
- 这波不改它们的行为，只把意图写清楚：它们保留旧 API 是为了覆盖兼容面，不是为了教新调用方继续这么写。

## Next Queue
- 若继续 `ServerName` 主线，可继续扫描 `tests/examples/` 中剩余 context-level setter 的定位，判断是兼容覆盖还是仍应迁移。
- 或切回 linked-evidence/script 链继续做边界治理。
