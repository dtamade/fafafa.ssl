# API Reference Library Context Surface Truth Plan

**Goal:** 把 `docs/reference/API_REFERENCE.md` 里 `ISSLLibrary` / `ISSLContext` 两个高入口代码块重新对齐到当前 `src/fafafa.ssl.base.pas` 的 shipped source truth，避免活跃 canonical doc 继续把调用方带回旧接口面。

**Architecture:** 这批不改 runtime，不改 public Pascal source，只做 active API reference truth repair：
- `tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`：先补 focused RED，证明当前 `API_REFERENCE` 的 `ISSLLibrary` / `ISSLContext` 代码块漏掉了一批已经公开 shipping 的方法。
- `docs/reference/API_REFERENCE.md`：把两个代码块补回当前源码已有的 method surface，并保持文档内对 mixed-scope / deprecated surface 的现有解释不回退。

**Files:**
- Add: `tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
- Modify: `docs/reference/API_REFERENCE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove active API reference drift

Run:

```bash
bash -n tests/scripts/test_api_reference_library_context_surface_truth_contract.sh
bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh
```

Expected RED before doc fix:
- `ISSLLibrary` code block still misses:
  - `SetDefaultConfig`
  - `GetDefaultConfig`
  - `GetStatistics`
  - `ResetStatistics`
- `ISSLContext` code block still misses current shipped surfaces such as:
  - `SetPreferredVersion` / `GetPreferredVersion`
  - PEM direct-load helpers
  - session-cache size / options / ALPN / cert-verify flags
  - deprecated context-level SNI compatibility signatures
  - password/info callbacks
  - certificate pinning helpers

## Task 2: GREEN - restore current source truth in API reference

Change:
- keep the `ISSLLibrary` / `ISSLContext` markdown code blocks as active source-truth views, not “minimal subset” snippets
- add the currently shipped methods back into the two code blocks
- do not reopen broader `ISSLConnection` slimming or `TSSLConfig` redesign in this batch

## Task 3: Verification

Run:

```bash
bash -n tests/scripts/test_api_reference_library_context_surface_truth_contract.sh
bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh
git diff --check
```

## Definition Of Done

- `API_REFERENCE` no longer omits shipped `ISSLLibrary` / `ISSLContext` methods from its primary code blocks
- active API docs stop teaching an older, narrower interface surface than the real public source
- focused contract stays green
- planning files record this as a canonical doc-truth closeout so the same drift is not re-litigated later

## Execution Result

- focused RED 先直接压实了当前 drift 不是抽象担忧，而是 `API_REFERENCE` 的主代码块真的漏了 shipped methods：
  - `ISSLLibrary` 首个命中缺口就是：
    - `SetDefaultConfig`
  - `ISSLContext` 同一条 active-doc surface 也确实缺：
    - `SetPreferredVersion` / `GetPreferredVersion`
    - PEM direct-load helpers
    - session-cache size / options / ALPN / cert-verify flags
    - context-level SNI compatibility signatures
    - password/info callbacks
    - certificate pinning helpers
- 最小 GREEN 没有改 runtime，也没有重开 broader interface surgery：
  - 只把 `docs/reference/API_REFERENCE.md` 的 `ISSLLibrary` / `ISSLContext` 代码块补回当前 shipped source truth
  - 并补一句明确说明这些代码块现在是 current source-truth view，而不是旧的精简子集
- 验证结果：
  - `bash -n tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`：PASS
  - `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`：PASS
  - `git diff --check`：PASS
