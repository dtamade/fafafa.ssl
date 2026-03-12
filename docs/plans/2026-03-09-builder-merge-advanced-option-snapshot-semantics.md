# 2026-03-09 Builder Merge Advanced Option Snapshot Semantics

## Goal
- 收口 `TSSLContextBuilder.Merge(...)` 在 advanced empty-value 与 `options=[]` 上的 source-snapshot 语义。
- 防止 merge 后出现 `FOptions` 已替换、但 `server_name` / `alpn_protocols` 等字段仍残留旧值的 field/option 漂移。

## Scope
- `src/fafafa.ssl.context.builder.pas`
- `tests/config/test_config_advanced_option_merge_semantics.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 `Merge(...)` advanced snapshot 语义
- [x] 新增 focused RED contract
- [x] 最小修复 `Merge(...)` 空值与空 option-set 处理
- [x] 跑 focused + 相邻回归 + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src -otmp/test_config_advanced_option_merge_semantics tests/config/test_config_advanced_option_merge_semantics.pas && ./tmp/test_config_advanced_option_merge_semantics` => PASS (`6/6`)
- `fpc -Fu./src -otmp/test_config_pkcs11_json_parity tests/config/test_config_pkcs11_json_parity.pas && ./tmp/test_config_pkcs11_json_parity` => PASS (`14/14`)
- `fpc -Fu./src -otmp/test_config_advanced_option_empty_value_semantics tests/config/test_config_advanced_option_empty_value_semantics.pas && ./tmp/test_config_advanced_option_empty_value_semantics` => PASS (`12/12`)
- `python3 scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `Merge(...)` 现在会把 source snapshot 中显式存在的 advanced empty values 真正合并进来：
  - `server_name=''` 会清空目标 `server_name`
  - `alpn_protocols=''` 会清空目标 `alpn_protocols`
- `Merge(...)` 现在也会保留 source 的显式空 option-set：
  - `options=[]` 会清空目标 `FOptions`
- `Merge(...)` 同时补上了 `ocsp_stapling_enabled` / `ocsp_stapling_required` 的布尔面复制，避免 source options 与 OCSP booleans 再次漂移。

## Adjacent Audit
- `Merge(...)` 对证书/密钥/CA/PKCS#11/cipher 这类字符串字段仍是“仅非空才覆盖”；如果继续把 Merge 定义成 source snapshot 语义，这组字段仍值得单独合同化。

## Next Queue
- 审 `Merge(...)` 在 certificate / key / CA / PKCS#11 / cipher string fields 上是否也应支持 empty-value clearing。
- 或单独排查 `tests/config/test_config_snapshot_clone.pas` 当前的 build-related 失败，确认是否为环境既有问题。

## Non-Blocking Observation
- `fpc -Fu./src -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone` 仍为 FAIL (`18/22`)，失败点落在 reset/build 路径，不在本波 `Merge(...)` 变更覆盖面内；本波未扩修该既有问题。
