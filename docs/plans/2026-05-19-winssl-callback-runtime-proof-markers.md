# WinSSL Callback Runtime Proof Markers

## Goal

把 `WinSSL` callback surface 的 Windows 运行时证据变成 artifact 内可检索的稳定 marker，收掉当前这条 proof gap：

- 本地 Linux 侧只能静态证明：
  - verify/info callback 已发布
  - password callback 当前 fail-closed `unsupported`
- 但 GitHub Windows runner 现有 broader suite transcript 还不能直接检索出这三条 callback 结论

## Scope

- 只处理 WinSSL runtime transcript evidence：
  - `tests/run_winssl_tests.ps1`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - focused shell contract
- 不改 WinSSL library callback runtime 行为
- 不改 callback capability/source truth
- 不扩张 broader workflow 结构

## Files

- Add: `docs/plans/2026-05-19-winssl-callback-runtime-proof-markers.md`
- Add: `tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
- Modify: `tests/run_winssl_tests.ps1`
- Modify: `tests/winssl/test_winssl_unit_comprehensive.pas`
- Modify: `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Architecture Truth

- Windows broader suite 实际运行的 `test_winssl_unit_comprehensive.lpi`
  对应的是：
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
- 本批在下载 artifact 后确认：
  - workflow `26092105397` 已经写出了：
    - `[WINSSL-RUNTIME] callback_surface verify=missing password=missing info=missing`
  - 这不是 marker 缺失，而是提取链路抓错了 truth source
- 根因是：
  - `tests/run_winssl_tests.ps1` 之前按 `test_winssl_unit_comprehensive.lpi` 去提取 callback truth
  - 但该 LPI 对应的 Windows comprehensive test source 当时并不包含：
    - `Verify callback set`
    - `Password callback unsupported as expected`
    - `Info callback set`
- 因而最小正确修法不是继续改 marker 聚合格式，而是先让实际 Windows comprehensive unit test 产出这三条稳定 callback 断言，再由脚本汇总成 artifact marker

## Steps

1. 先用 artifact 反证当前 proof gap 是否真实存在：
   - 下载 Windows transcript
   - 确认当前 marker 不是缺失，而是 `missing/missing/missing`
2. 补 focused shell contract，锁住：
   - 实际 Windows comprehensive unit test source 必须包含 callback truth
   - broader-suite script 继续对同一 LPI 的 captured output 汇总 marker
3. 在 `tests/winssl/test_winssl_unit_comprehensive.pas` 增加 callback configuration tests，产出稳定的：
   - `Verify callback set`
   - `Password callback unsupported as expected`
   - `Info callback set`
4. 跑本地 focused contract 与 diff hygiene
5. 提交推送后 dispatch `Wave B B2 Manual Gate (Template)`，再次下载 Windows transcript，确认 callback marker 回到真实值：
   - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`

## Verification

```bash
bash -n tests/scripts/test_winssl_runtime_callback_markers_contract.sh
bash tests/scripts/test_winssl_runtime_callback_markers_contract.sh
git diff --check
gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_callback_markers_<timestamp>
gh run watch <run-id>
gh run download <run-id> -n wave-b-windows-<run_id> -D tmp/wave_b_windows_<run_id>
rg -n "\\[WINSSL-RUNTIME\\] callback_surface" tmp/wave_b_windows_<run_id>
```

## Expected Result

- Windows runtime transcript 内出现稳定 callback marker，例如：
  - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
- 这条证据可以直接支撑：
  - verify/info callback 已发布并通过当前 WinSSL broader suite
  - password callback 仍然 fail-closed
- 后续不再需要把 callback granularity proof 留成“Windows CI 待补”的口头结论
