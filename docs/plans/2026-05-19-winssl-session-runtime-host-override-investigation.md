# WinSSL Session Runtime Host-Override Investigation Lane

## Goal

把 `wave-b-b2-manual.yml` 收口成一个可复用的 WinSSL session-runtime 调查入口：

- `workflow_dispatch` 暴露可选 `winssl_session_host` 输入
- Windows broader WinSSL runtime suite 只在手动提供该输入时，才注入 `FAFAFA_WINSSL_SESSION_HOST`
- 默认 lane 保持当前行为不变，不把本批变成多 host matrix 或默认额外风险
- `.github/README.md` 与计划台账明确记录这条调查通道，避免之后再靠口头说明反复拉起

## Scope

- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `.github/README.md`
- `tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `tests/winssl/test_winssl_session_resumption.pas` 的默认 host 逻辑
- 不把 workflow 改成默认多 host matrix
- 不重开 shared probe / benchmark wording / old guide truth lane
- 不在本批里直接承诺 WinSSL native resumed-handshake 已被 runtime 证实

## Why This Batch

当前 WinSSL session-resumption 线里，普通 docs / guide / benchmark truth 已经基本收口；真正剩下的高风险点，是 dedicated Windows runner 上观察到的：

- `observed_reuse=false`
- `session_configured=true`

这个现象到底是：

- Schannel/WinSSL runtime 行为本身，
- 还是当前默认目标 host 的差异，

现在还没有一个 repo 内建、可重复、可审查的“换 host 做真实调查”入口。

现状已经具备一半基础：

- `tests/winssl/test_winssl_session_resumption.pas` 已支持 `FAFAFA_WINSSL_SESSION_HOST`
- `tests/run_winssl_tests.ps1` 不会覆盖这个变量

缺的是把它提升到 GitHub Actions 手动工作流的一等输入，并用 focused contract 锁住，防止未来又回到“只能靠聊天说明怎么查”的状态。

## Planned Changes

1. 新增 focused workflow/source contract：
   - 锁住 `winssl_session_host` input、默认空值、README 说明、以及 Windows broader runtime step 的可选 env 注入语义
2. 更新 `wave-b-b2-manual.yml` 与 `.disabled` 模板：
   - 增加 `workflow_dispatch.inputs.winssl_session_host`
   - 在 `Run broader WinSSL runtime suite` step 中仅当输入非空时设置 `FAFAFA_WINSSL_SESSION_HOST`
   - 额外打印当前是否启用 host override，便于 artifact 直接审查
3. 更新 `.github/README.md`：
   - 说明 Wave B/B2 manual workflow 现在支持可选 `winssl_session_host` 调查输入
4. 视 `gh auth status` 与 push 结果决定是否做一次真实 dispatch，验证 GitHub UI/runner 端能接收该输入

## Verification

```bash
bash -n tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh
bash tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh
git diff --check
gh auth status
git push origin master
gh workflow run wave-b-b2-manual.yml \
  -f run_id=winssl_host_probe_20260519 \
  -f strict_closure=false \
  -f winssl_session_host=www.google.com
```

如果 dispatch 成功，再补充：

```bash
gh run list --workflow wave-b-b2-manual.yml --limit 5
gh run watch <run-id>
gh run view <run-id> --log
```

## Expected Outcome

- repo 内建了一个 bounded、可重复的 Windows host-override 调查通道
- 默认 manual workflow 风险面不扩大
- 之后如果要验证 `observed_reuse=false` 是否受目标 host 影响，可以直接在 GitHub runner 上复用这条 lane，而不是重新设计调查流程
