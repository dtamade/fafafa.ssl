# Capability Serialization Truth Projection

## Goal

验证 capability serializer 是否把已经确立的 support-level 真相正确投影到 legacy boolean 输出面；如果 JSON/XML 仍会导出自相矛盾的 `supports*` / `*Support` 组合，则用最小修复收口，并把剩余不可判定边界明确记录下来。

## Why This Batch

- runtime `GetCapabilities`
- deserializer precedence
- capability diff

以上三条链已经先后改成“support-level 为真相源”。如果 serializer 继续原样吐出冲突的 legacy boolean，就会把内部已收敛的真相再次泄漏成外部 payload 漂移。

## Scope

- `src/fafafa.ssl.capability.serializer.pas`
- `tests/test_capability_serialization_truth_projection.pas`
- `tests/test_capability_deserialization_roundtrip.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Plan

1. 新增 focused RED，直接检查 JSON/XML 输出字符串，而不是只做 round-trip：
   - `supportsSNI=false + SNISupport=stable` 必须投影成 `supportsSNI=true`
   - `supportsSNI=true + SNISupport=none` 在 record 已有其他 v1.2 support-level 信号时，必须投影成 `supportsSNI=false`
2. 在 serializer 内部增加 prepare/projection helper：
   - 仅当 record 已携带 support-level truth 时，才用 `NormalizeLegacyCapabilityBooleans(...)` 回填 legacy boolean 输出视图
   - 不去猜纯 legacy-only record 中 `none` 究竟是“默认未设置”还是“显式不支持”
3. focused 验证：
   - `tests/test_capability_serialization_truth_projection.pas`
   - `tests/test_capability_deserialization_roundtrip.pas`
   - `git diff --check`

## Expected Outcome

- v1.2-aware capability record 的 JSON/XML 输出不再泄漏 bool/support-level 自相矛盾
- 既有 round-trip 兼容保持不变
- “纯 legacy-only in-memory record 缺少 presence bit” 这条残余歧义被明确写进记录，不再反复争论
