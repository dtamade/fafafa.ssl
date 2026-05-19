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
  - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - focused shell contract
- 不改 WinSSL callback runtime 行为
- 不改 callback capability/source truth
- 不扩张 broader workflow 结构

## Files

- Add: `docs/plans/2026-05-19-winssl-callback-runtime-proof-markers.md`
- Add: `tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
- Modify: `tests/run_winssl_tests.ps1`
- Modify: `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Architecture Truth

- `tests/unit/test_winssl_comprehensive.pas` 当前已经有稳定 callback 断言输出：
  - `Verify callback set`
  - `Password callback unsupported as expected`
  - `Info callback set`
- 但 `tests/run_winssl_tests.ps1` 成功时默认不会把这类单项 `[PASS]` 输出打印进 broader runtime transcript
- 因而 workflow 即使整套通过，也缺一条可直接 grep 的 callback runtime proof marker

## Steps

1. 给 `tests/run_winssl_tests.ps1` 增加 callback summary marker：
   - 只在 `test_winssl_unit_comprehensive.lpi` 上从 captured output 提取
   - 输出稳定的 `[WINSSL-RUNTIME] callback_surface ...` 行
2. 补 focused shell contract，锁住 marker 提取逻辑和 Windows checklist 文案
3. 跑本地 focused contract 与 diff hygiene
4. 提交推送后 dispatch `Wave B B2 Manual Gate (Template)`，下载 Windows transcript，确认 callback marker 真正出现在 artifact 中

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
