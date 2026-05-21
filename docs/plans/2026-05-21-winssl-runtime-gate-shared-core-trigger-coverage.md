# WinSSL Runtime Gate Shared/Core Trigger Coverage

## Goal

把 `WinSSL Runtime Gate`
从“只会在 `src/fafafa.ssl.winssl*.pas` / `tests/winssl/**` 改动时自动触发”
收紧到当前更真实的自动证明边界：

- WinSSL backend 源码改动会触发
- 当前已经明确会影响 WinSSL runtime proof 的 shared/core units 改动也会触发

避免后续继续出现：

- Linux `CI` 绿色
- shared/core 改动已经落地
- 但 Windows runtime lane 根本没有自动跟上

## Scope

- Update:
  - `.github/workflows/winssl-tests.yml`
  - `.github/workflows/winssl-tests.yml.disabled`
  - `tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - `.github/README.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-winssl-runtime-gate-shared-core-trigger-coverage.md`

不做：

- 不改 WinSSL production source
- 不把 Windows gate 扩成“任意 `src/**` 改动都跑”
- 不在这一批顺手扩新的 WinSSL runtime tests

## Architecture Truth

- 当前 workflow path filter 只盯：
  - `src/fafafa.ssl.winssl*.pas`
  - `tests/winssl/**`
  - Windows runtime scripts
- 但最近几轮接口/实现审查已经证明，
  下列 shared/core units 的改动也会真实影响 WinSSL runtime proof：
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.context.config.pas`
  - `src/fafafa.ssl.asn1.pas`
  - `src/fafafa.ssl.x509.pas`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.context.builder.pas`
- 这条 lane 的目标不是扩大 Windows gate 的所有权，
  而是让“当前确实会影响 WinSSL runtime truth 的 shared/core 改动”
  不再静默绕过 Windows runner

## Steps

1. 先把 workflow contract 收紧到新的 shared/core trigger truth，拿到 RED。
2. 最小修改 active + disabled WinSSL workflows：
   - push / PR path filter 同步加入当前需要的 shared/core units
3. 同步 `.github/README.md`，明确这条 auto gate 的当前触发边界
4. 跑 focused verification
5. 更新台账并准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_workflow_winssl_tests_truth_contract.sh
bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh
git diff --check
```

## Expected Outcome

- `WinSSL Runtime Gate`
  会在当前真实相关的 shared/core 改动上自动触发
- 后续继续推进 `ISSLConnection` / `TSSLConfig` / facade / parser 这类 shared 线路时，
  Windows runtime proof 不会再被 path filter 静默漏掉
- 自动 Windows lane 仍保持 bounded，
  不会退化成“任何源码改动都跑”
