# WinSSL Native Probe Manual Investigation Lane

## Goal

把已有的 WinSSL native-probe 证据能力，提升成 `wave-b-b2-manual.yml` 里的显式手动调查入口：

- `workflow_dispatch` 暴露可选 `winssl_enable_native_probe`
- Windows broader WinSSL runtime suite 只在显式 opt-in 时才注入 `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE`
- 默认 lane 继续保持 native probe disabled，避免把已知风险重新带回常规手动验证路径
- `.github/README.md` 与台账记录这条 lane 的风险语义和真实运行结果

## Scope

- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `.github/README.md`
- `tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/fafafa.ssl.winssl.connection.pas` 的 shared reconnect / session-info 逻辑
- 不把 `SetSession(...)` 重解释成 native handle 注入
- 不默认开启 native probe
- 不在 workflow 里注入 `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE`

## Why This Batch

现在 host-override 调查 lane 已经 live 证明接通，并且 `www.google.com` 仍然得到：

- `observed_reuse=false`
- `session_configured=true`

所以当前更高价值的问题已经不是 workflow plumbing，也不只是 host 差异，而是：

- 开启 dedicated native probe 后，
- `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
- 在 GitHub Windows runner 上到底会返回什么 evidence，或者是否仍会触发已知风险

repo 里其实已经具备一半能力：

- `tests/winssl/test_winssl_session_resumption.pas` 已支持 `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE`
- 旧计划 `2026-05-18-winssl-native-probe-evidence-lane.md` 已记录它必须保持 opt-in

缺的是把它提升成 manual workflow 的一等输入，并把“只做 Schannel evidence、可能导致 broader suite fail”这层语义锁进 contract 和 README。

## Planned Changes

1. 新增 focused workflow/source contract：
   - 锁住 `winssl_enable_native_probe`
   - 锁住默认 `false`
   - 锁住 Windows broader runtime step 的 opt-in env 注入与风险日志
   - 锁住 workflow 不得把 `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE` 作为默认/自动注入
2. 更新 active + disabled workflow：
   - 增加 `workflow_dispatch.inputs.winssl_enable_native_probe`
   - 仅在输入为 truthy 时设置 `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1`
   - 明确打印启用/禁用日志
3. 更新 `.github/README.md`：
   - 记录 native probe 是 bounded Schannel evidence lane，不属于默认 broader suite 路径
4. push 后手动 dispatch 一次启用 native probe 的 run，下载 Windows artifact 记录真实结果

## Verification

```bash
bash -n tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh
bash tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh
bash tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh
bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh
bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
git diff --check
git push origin master
gh workflow run wave-b-b2-manual.yml \
  -f run_id=winssl_native_probe_20260519_google \
  -f strict_closure=false \
  -f winssl_session_host=www.google.com \
  -f winssl_enable_native_probe=true
```

## Expected Outcome

- repo 有一个真实可用的 native-probe manual investigation lane
- 默认 Wave B/B2 manual workflow 风险面不扩大
- Windows artifact 能进一步回答：
  - native probe 有没有成功执行
  - 如果执行了，是否看到了 `SSL_SESSION_RECONNECT`
  - 如果没有执行成功，失败边界具体落在哪里
