# Capability Matrix FreePascal Coverage Completeness

## Goal

把 `tests/test_capability_matrix_v12.pas`
里遗漏的
`FreePascal`
backend coverage
补齐，
避免我们继续把
“backend capability matrix verification”
误当成
四后端而不是五后端。

这批不改生产实现，
只修：

- focused capability-matrix regression 的 coverage truth
- 防回退 contract
- plan / findings / progress 账本

## Why This Batch

当前主线已经明确回到
backend implementation-completeness 审查。

但 live focused test
`tests/test_capability_matrix_v12.pas`
当前只执行：

- `OpenSSL`
- `WolfSSL`
- `MbedTLS`
- `WinSSL`

漏掉了当前产品主线 backend
`FreePascal`。

这会导致：

- capability matrix audit
  仍然不是完整五后端覆盖
- Linux 本机最容易稳定执行的
  `FreePascal`
  capability truth
  没有进入这条 focused regression

## Scope

- Add:
  - `docs/plans/2026-05-21-capability-matrix-freepascal-coverage-completeness.md`
  - `tests/scripts/test_capability_matrix_v12_freepascal_coverage_contract.sh`
- Update:
  - `tests/test_capability_matrix_v12.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## TDD / Verification

1. 先证明当前 audit gap：
   - `tests/test_capability_matrix_v12.pas`
     里没有
     `TestBackendCapabilities('FreePascal', sslFreePascal);`
2. 最小修复：
   - 把
     `FreePascal`
     加进 capability-matrix regression
   - 用静态 contract
     守住 5 backend call set
3. Focused verification：

```bash
bash -n tests/scripts/test_capability_matrix_v12_freepascal_coverage_contract.sh
bash tests/scripts/test_capability_matrix_v12_freepascal_coverage_contract.sh
mkdir -p tmp/test_capability_matrix_v12 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_matrix_v12 -FEtmp/test_capability_matrix_v12 -otmp/test_capability_matrix_v12/test_capability_matrix_v12 tests/test_capability_matrix_v12.pas
./tmp/test_capability_matrix_v12/test_capability_matrix_v12
git diff --check
```

## Expected Result

- capability-matrix focused regression
  明确覆盖：
  - `OpenSSL`
  - `FreePascal`
  - `WolfSSL`
  - `MbedTLS`
  - `WinSSL`
- 当前 Linux host
  至少会稳定执行：
  - `OpenSSL`
  - `FreePascal`
- contract failures
  保持 `0`
- 如果
  `FreePascal`
  在这条 shared regression 上暴露 fresh RED，
  就把它当作下一条真正的 capability drift 继续收口
