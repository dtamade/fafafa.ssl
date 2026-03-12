# 2026-03-10 test_lib_core_functionality encoding API modernization

## Goal
- 修复 `tests/examples/test_lib_core_functionality.pas` 的历史编译漂移。
- 让这个 core-smoke 示例测试重新对齐当前 Base64/registration API，而不是继续卡在旧接口名上。

## Scope
- `tests/examples/test_lib_core_functionality.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 复现 compile drift
- [x] 对齐当前 encoding API
- [x] 补 OpenSSL 注册单元导入
- [x] 跑 focused 编译与运行
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -Fi./src tests/examples/test_lib_core_functionality.pas -otmp/test_examples_test_lib_core_smoke && ./tmp/test_examples_test_lib_core_smoke` => PASS
- `bash -n tests/scripts/test_examples_context_server_name_compat_coverage_contract.sh && bash tests/scripts/test_examples_context_server_name_compat_coverage_contract.sh` => PASS
- `fpc -Fu./src -Fi./src tests/examples/test_basic.pas -otmp/test_examples_test_basic_smoke` => PASS（warnings only）

## Result
- `TCryptoUtils.Base64Encode` / `Base64DecodeString` 的旧调用已迁到 `TEncodingUtils`。
- 文件同时补上了 `fafafa.ssl.openssl.lib` 导入，避免 focused 运行时再因 OpenSSL backend 未注册而失败。
- 这个旧失败已从 “已知历史编译漂移” 变成绿色 smoke。

## Next Queue
- 若继续 examples 层治理，可再审 `tests/examples/test_basic.pas` 的 deprecated warning/noise 是否需要单独合同化。
- 或切回 linked-evidence/script 链继续做边界治理。
