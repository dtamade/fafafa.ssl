# 2026-05-21 CAPABILITY_MATRIX_GUIDE façade backend-name 真相对齐

## Goal

修复 `docs/CAPABILITY_MATRIX_GUIDE.md`
里仍残留的
`SSL_LIBRARY_NAMES[...]`
直取，
让这份 capability guide
继续保留
capability / backend
决策与动态枚举路径，
但不再让 façade-only
示例回流到
`fafafa.ssl.base`
常量心智。

## Scope

- Add:
  - `docs/plans/2026-05-21-capability-matrix-guide-facade-backend-name-truth.md`
- Update:
  - `docs/CAPABILITY_MATRIX_GUIDE.md`
  - `tests/scripts/test_capability_matrix_guide_current_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不重开 backend 集合 / FAQ / placeholder 链接收口
- 不把 `SSL_LIBRARY_NAMES` 重新包装进主门面

## Architecture Truth

- `CAPABILITY_MATRIX_GUIDE`
  当前 capability 示例
  已经使用：
  - `uses fafafa.ssl;`
- 这不等于
  它的 backend-name
  输出
  还可以继续直取：
  - `SSL_LIBRARY_NAMES[...]`
- 当前 façade 已公开的
  backend-name
  helper
  是：
  - `LibraryTypeToString(...)`
- 所以 façade-only 示例
  当前应统一使用：
  - `LibraryTypeToString(Caps.BackendType)`
  - `LibraryTypeToString(ABackend)`

## Steps

1. 收紧现有
   `tests/scripts/test_capability_matrix_guide_current_truth_contract.sh`：
   - capability guide
     必须使用
     `LibraryTypeToString(...)`
   - 不得继续出现
     `SSL_LIBRARY_NAMES[...]`
2. 跑 contract，拿到 RED。
3. 最小修改 `CAPABILITY_MATRIX_GUIDE.md` 两处输出。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_capability_matrix_guide_current_truth_contract.sh
bash tests/scripts/test_capability_matrix_guide_current_truth_contract.sh
git diff --check
```

## Expected Result

- `CAPABILITY_MATRIX_GUIDE`
  façade-only 示例
  不再继续教学
  `SSL_LIBRARY_NAMES[...]`
- backend name
  输出
  重新统一到
  `LibraryTypeToString(...)`

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - `CAPABILITY_MATRIX_GUIDE`
    在已经回到
    `uses fafafa.ssl;`
    之后，
    仍有两处
    backend-name
    输出
    在使用
    `SSL_LIBRARY_NAMES[...]`
- 最小修复后：
  - quickstart
    的
    `Backend`
    输出
    已切到：
    - `LibraryTypeToString(Caps.BackendType)`
  - `GenerateOptimalConfig(...)`
    的
    backend-name
    输出
    已切到：
    - `LibraryTypeToString(ABackend)`
  - guide
    继续保持
    capability /
    backend-decision
    结构
- focused verification：
  - `bash -n tests/scripts/test_capability_matrix_guide_current_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_capability_matrix_guide_current_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
