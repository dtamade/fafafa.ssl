# 2026-05-21 API_REFERENCE 当前 public import 真相对齐

## Goal

修复 `docs/reference/API_REFERENCE.md`
里 canonical capability 示例
仍在使用
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
split import
与
`SSL_LIBRARY_NAMES`
旧常量直取的问题，
让主参考页继续保持
capability canonical
定位，
但不再偏离当前 façade 已公开的 import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-api-reference-current-public-import-truth.md`
- Update:
  - `docs/reference/API_REFERENCE.md`
  - `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不扩张到 API_REFERENCE 其它更大范围签名/coverage 收口
- 不把 `SSL_LIBRARY_NAMES` 重新包装进主门面

## Architecture Truth

- `API_REFERENCE`
  当前是
  canonical public reference
- 这不等于
  capability 示例
  还要继续 split：
  - `fafafa.ssl.base`
  - `fafafa.ssl.factory`
- 当前 façade 已公开：
  - `ISSLLibrary`
  - `TSSLFactory`
  - `TSSLBackendCapabilities`
  - `LibraryTypeToString(...)`
  - capability helper functions
  都可直接来自：
  - `fafafa.ssl`
- `SSL_LIBRARY_NAMES`
  当前仍是
  `fafafa.ssl.base`
  内部常量，
  不应继续出现在
  façade-only canonical 示例里

## Steps

1. 收紧现有
   `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`：
   - `API_REFERENCE`
     capability 示例
     必须使用：
     - `uses fafafa.ssl;`
     - `LibraryTypeToString(...)`
   - 不得继续使用：
     - `SysUtils, fafafa.ssl.base, fafafa.ssl.factory;`
     - `SSL_LIBRARY_NAMES[...]`
2. 跑 contract，拿到 RED。
3. 最小修改 `API_REFERENCE.md` 的 capability 示例。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
git diff --check
```

## Expected Result

- `API_REFERENCE`
  capability canonical 示例
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
  这组旧 split import
- façade-only 调用方
  不需要因为示例
  被带回
  `SSL_LIBRARY_NAMES`
  这类 base-only 常量

## Execution Result

- PASS
- focused RED
  首轮直接证明：
  - `API_REFERENCE`
    capability canonical
    示例
    仍在使用
    `fafafa.ssl.base`
    /
    `fafafa.ssl.factory`
  - contract
    继续暴露出
    另一处
    `SSL_LIBRARY_NAMES[...]`
    residual，
    说明 canonical page
    已经不只是一处 split import 漂移
- 最小修复后：
  - capability canonical
    示例
    已收回到：
    - `uses fafafa.ssl;`
  - backend name
    输出
    已统一改成：
    - `LibraryTypeToString(...)`
  - `SSL_LIBRARY_NAMES[...]`
    不再出现在
    façade-only
    canonical 示例里
- focused verification：
  - `bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
