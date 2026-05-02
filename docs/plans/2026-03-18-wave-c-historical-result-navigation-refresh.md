# Wave C historical result navigation refresh（2026-03-18）

## Goal
- 系统性刷新 `docs/test_reports/WAVE_C_B12x~B14x` 历史结果页入口，让文档导航默认指向当前 2026-03-15/2026-03-16 新证据链，而不是 2026-02-08/2026-02-09 历史样例。

## Architecture / Approach
1. 新增一个当前链路入口页
   - 文件：`docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
   - 作用：把 local-first / pre-CI / submission 三段最新入口集中到一个 docs 页面
2. 调整文档索引默认导航
   - 在 `docs/DOCUMENTATION_INDEX.md` 的 Wave C 区段显式加入 “Current Wave C Chain”
   - 保留历史条目，但单独放入 “Historical Result Pages”
3. 给旧结果页加统一前导
   - `B123~B145`：跳回新的 current-chain 入口页
   - `B146~B149`：直接跳到对应的 2026-03-15/2026-03-16 新结果页
4. 保持历史内容不删改
   - 历史执行命令、历史样例报告仍保留，作为归档证据
   - 只增加“当前入口 / 历史定位”说明，不重写结论

## Files
- `docs/DOCUMENTATION_INDEX.md`
- `docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- `docs/test_reports/WAVE_C_B123_LOCAL_FIRST_CONTINUITY_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B124_LOCAL_DRIFT_WATCH_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B125_LOCAL_GUARD_BUNDLE_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B126_LOCAL_GUARD_HISTORY_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B128_LOCAL_FIRST_DOC_ENTRY_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B129_ONCALL_CHECK_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B132_LOCAL_FIRST_STATUS_SNAPSHOT_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B134_LOCAL_FIRST_CLOSURE_SUMMARY_2026-02-09.md`
- `docs/test_reports/WAVE_C_B136_DELIVERABLES_OVERVIEW_2026-02-09.md`
- `docs/test_reports/WAVE_C_B137_PRE_CI_REENABLE_PACKET_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B138_PRE_CI_REENABLE_FULL_GATE_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B140_LOCAL_GUARD_CONSISTENCY_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B141_LOCAL_GUARD_OPERATIONS_SUMMARY_2026-02-09.md`
- `docs/test_reports/WAVE_C_B142_LOCAL_GUARD_STATUS_EXPORT_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B143_LOCAL_GUARD_ALERT_THRESHOLDS_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B144_LOCAL_GUARD_OPS_PACK_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B145_FAST_TRACK_BUNDLE_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-02-09.md`
- `docs/test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-02-09.md`

## Step-by-step Commands
1. Inspect current/legacy docs:
   - `sed -n '108,168p' docs/DOCUMENTATION_INDEX.md`
   - `rg -n "test-reports/|2026-02-09|2026-02-08" docs/test_reports/WAVE_C_B1*.md`
2. Edit docs:
   - add current-chain landing page
   - adjust index ordering and section headers
   - add “Current Wave C Chain” pointers to legacy result pages
3. Verify links/markers:
   - `rg -n "Current Wave C Chain|Historical Result Pages|WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16|WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-03-16|WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-03-15|WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-03-16" docs/DOCUMENTATION_INDEX.md docs/test_reports/WAVE_C_B12*.md docs/test_reports/WAVE_C_B13*.md docs/test_reports/WAVE_C_B14*.md`
4. Format docs:
   - `yarn prettier --write <changed docs>`

## Expected Outputs
- `docs/DOCUMENTATION_INDEX.md` 明确区分当前链路与历史结果页
- 历史 `B123~B145` 页面顶部都能把读者带回当前 2026-03-16 链路入口页
- 历史 `B146~B149` 页面顶部都能直接导向新的 2026-03-15/2026-03-16 结果页
- 历史页保留原始证据，不破坏归档语义
