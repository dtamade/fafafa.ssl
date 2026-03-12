# 2026-03-10 test_basic deprecated warning noise

## Goal
- 收口 `tests/examples/test_basic.pas` 的 file-local deprecated warning/noise。
- 在保留 compatibility coverage 语义的前提下，避免这个高可见入口继续制造无价值的 deprecated 编译噪音。

## Scope
- `tests/examples/test_basic.pas`
- `tests/scripts/test_test_basic_deprecated_warning_noise_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 定位 deprecated warning 来源
- [x] 新增 focused warning contract
- [x] 局部抑制 deprecated noise
- [x] 跑 focused 编译回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n tests/scripts/test_test_basic_deprecated_warning_noise_contract.sh && bash tests/scripts/test_test_basic_deprecated_warning_noise_contract.sh` => PASS
- `fpc -Fu./src -Fi./src tests/examples/test_basic.pas -otmp/test_examples_test_basic_smoke` => PASS（notes only）

## Result
- `test_basic.pas` 里的 compatibility coverage 仍保留，但 file-local deprecated warnings 已被局部 warning scope 收口。
- 本波继续清掉了最后一个 file-local note（未使用局部变量）；当前 focused 编译输出已不再包含 `test_basic.pas` 自身的 warning/note 噪音。

## Verification Refresh
- `bash -n tests/scripts/test_test_basic_deprecated_warning_noise_contract.sh && bash tests/scripts/test_test_basic_deprecated_warning_noise_contract.sh` => PASS
- `fpc -Fu./src -Fi./src tests/examples/test_basic.pas -otmp/test_examples_test_basic_smoke` => PASS（compile output clean for `test_basic.pas`）

## Next Queue
- 若继续 examples 层治理，可决定是否顺手清掉 `test_basic.pas` 的 note 级噪音。
- 或切回 linked-evidence/script 链继续做边界治理。
