# 2026-05-19 Callback Publication Matrix Truth

## Goal

继续沿着 callback publication/completeness 主线推进，把当前已经收口的 callback truth 写回 active capability docs，
避免出现：

- `API_REFERENCE` 已经说明 callback gating / WinSSL partial publication
- 但 `BACKEND_CAPABILITY_MATRIX` / `WINSSL_BACKEND_CAPABILITY_MATRIX` 仍缺失 callback 粒度真相

这种会让调用方在“总览矩阵”和“API 细节”之间读到不同心智模型的 docs drift。

## Scope

- 只处理 active capability docs truth：
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- 用 focused shell contract 锁住 callback publication matrix truth
- 不改生产代码
- 不重新设计 capability 结构

## Files

- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_callback_publication_matrix_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `OpenSSL`
  - `SupportsCallbacks=True`
  - verify/password/info callback 都有 runtime wiring
- `WinSSL`
  - `SupportsCallbacks=True`
  - 仅 verify/info callback 已发布
  - password callback 当前仍 unsupported
- `FreePascal` / `WolfSSL` / `MbedTLS`
  - `SupportsCallbacks=False`
  - verify/password/info setter 已 fail-closed

## Steps

1. 补 focused doc contract，让 active matrix 缺口先 RED。
2. 把 callback publication row / notes 写回 active matrix docs。
3. 跑 focused doc contract 与相关现有 docs/source truth contract。
4. 回写台账并提交。

## Commands

```bash
bash -n tests/scripts/test_callback_publication_matrix_truth_contract.sh
bash tests/scripts/test_callback_publication_matrix_truth_contract.sh
bash tests/scripts/test_callback_capability_truth_contract.sh
bash tests/scripts/test_winssl_password_callback_partial_publication_contract.sh
bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh
git diff --check
```

## Expected Result

- `BACKEND_CAPABILITY_MATRIX` 与 `WINSSL_BACKEND_CAPABILITY_MATRIX` 不再遗漏 callback publication 粒度真相
- callback docs 真相从 API 参考页推进到 capability 总览矩阵
- 后续 callback completeness 审查不再需要重复解释 coarse bool vs per-callback publication
