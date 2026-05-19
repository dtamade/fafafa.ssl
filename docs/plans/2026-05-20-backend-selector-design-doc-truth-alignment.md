# Backend Selector Design Doc Truth Alignment

## Goal

收口 `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md` 与
`docs/reference/BACKEND_SELECTOR_DESIGN.md`
中仍在传播的旧 selector / builder API 与旧 backend capability 叙事，
避免设计层继续把后续接口设计、后端实现判断带偏。

## Scope

- 新增一条 focused shell contract，先把当前 drift 做成 RED
- 最小修正两份 design/reference 文档
- 同步更新 `task_plan.md` / `findings.md` / `progress.md`
- 不重跑大门禁，不重开 Windows runtime proof

## Architecture Truth

- 当前 selector public API 是函数式：
  - `SelectBestBackend(...)`
  - `SelectBestBackends(...)`
  - `CreateDefaultRequirements(...)`
  - `CreateSecurityFirstRequirements`
  - `CreatePerformanceFirstRequirements`
  - `CreateCompatibilityFirstRequirements`
- 当前 builder 自动选择入口是：
  - `TSSLContextBuilder.WithAutoBackendSelection(...)`
  - `WithSecurityFirst`
  - `WithPerformanceFirst`
  - `WithCompatibilityFirst`
  - `RequireTLS13`
  - `RequireCipher`
  - `RequirePKCS11Support`
  - `PreferOSNative`
- 当前 source 没有发布：
  - `TBackendSelector`
  - `TBackendSelectionResult`
  - `WithPreferredBackend(...)`
  - `WithFallbackBackend(...)`
  - `WithAllowPartialMatch`
  - dedicated `FAFAFA_SSL_BACKEND` / `FAFAFA_SSL_DISABLE_BACKEND` / `FAFAFA_SSL_SELECTOR_DEBUG`
    selector entrypoint
- `FreePascal` 当前已是活跃 backend，不是 future lane
- `WinSSL` 当前：
  - `OCSPStaplingSupport=sslSupportNone`
  - `EarlyDataSupport=sslSupportNone`
- design/reference 文档不应再维护一套会漂的 capability 大表；
  当前 capability truth 应以 `docs/BACKEND_CAPABILITY_MATRIX.md`
  和 dedicated backend matrices 为准

## Files

- `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
- `docs/reference/BACKEND_SELECTOR_DESIGN.md`
- `tests/scripts/test_backend_selector_design_doc_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract，冻结当前 source truth 与 design-doc truth
2. 先运行 contract，确认旧 design docs 先 RED
3. 最小修 design docs：
   - 去掉旧草案 API 名字
   - 去掉 `FreePascal (Future)` 叙事
   - 把 capability 细节收敛到 canonical matrix
4. 重新跑 focused verification
5. 更新 planning files，准备 commit/push

## Verification

```bash
bash -n tests/scripts/test_backend_selector_design_doc_truth_contract.sh
bash tests/scripts/test_backend_selector_design_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- design/reference 文档不再把旧 selector/builder 草案 API 写成当前 public surface
- `FreePascal` 不再被 design 文档描述成 future backend
- `WinSSL` 的 early-data / server-OCSP none-published truth
  不再被 design 层间接冲淡
- 后续若有人再次把 design 文档写回旧 API / 旧能力表，
  focused contract 会第一时间报红
