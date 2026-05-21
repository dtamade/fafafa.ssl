# 2026-05-21 BACKEND_SELECTION_GUIDE 当前 public/import 真相对齐

## Goal

修复 `docs/BACKEND_SELECTION_GUIDE.md`
里仍会把
builder 集成调用方
带回
`fafafa.ssl.base`
拆分入口
的高入口漂移，
让这份专项指南继续讲
backend auto-selection /
builder integration /
selector APIs，
但不再让推荐 builder path
偏离当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-backend-selection-guide-current-public-import-truth.md`
  - `tests/scripts/test_backend_selection_guide_current_public_import_truth_contract.sh`
- Update:
  - `docs/BACKEND_SELECTION_GUIDE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不重开 selector runtime-aware capability truth
- 不改 selector 算法
- 不把专项指南改成普通入门页

## Architecture Truth

- 这份文档的定位是：
  backend auto-selection /
  builder integration /
  direct selector API
  专项指南
- 它不是普通 TLS bootstrap
  第一入口页；
  普通客户端/服务端建立 TLS
  仍优先回到：
  `docs/guides/GETTING_STARTED.md`
  中的
  `TSSLContextBuilder`
  /
  `TSSLConnector`
  /
  `TSSLAcceptor`
  /
  `TSSLStream`
- 但在这份专项指南里，
  “方式 1: 使用 Builder（推荐）”
  当前仍应使用：
  `uses fafafa.ssl, fafafa.ssl.context.builder;`
  而不是
  `fafafa.ssl.base`
- direct selector
  示例
  仍可继续显式使用：
  `fafafa.ssl.backend.selector`

## Steps

1. 新增 focused contract：
   - 顶部必须明确本页是专项 guide，
     普通 TLS 建立请回到 `GETTING_STARTED`
   - builder 推荐示例必须使用
     `fafafa.ssl`
     +
     `fafafa.ssl.context.builder`
   - builder 推荐示例不得继续使用
     `fafafa.ssl.base`
2. 跑 focused contract，拿到 RED。
3. 最小修复 guide 与台账。
4. 重跑 focused contract 与相关现有 guide contracts。

## Verification

```bash
bash -n tests/scripts/test_backend_selection_guide_current_public_import_truth_contract.sh
bash tests/scripts/test_backend_selection_guide_current_public_import_truth_contract.sh
bash tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh
bash tests/scripts/test_active_server_example_verify_intent_truth_contract.sh
git diff --check
```

## Expected Result

- `BACKEND_SELECTION_GUIDE`
  不再把 builder 推荐路径
  带回
  `fafafa.ssl.base`
- 这份文档会明确：
  - 自己是专项 guide
  - 普通 TLS 建立主入口在 `GETTING_STARTED`
  - direct selector path
    仍是专项 API，
    不是普通入门入口
