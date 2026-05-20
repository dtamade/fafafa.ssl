# Capability Support-Level Serialization Precedence

## Goal

把 `TSSLBackendCapabilities` 的
`support-level`
字段与 legacy boolean 字段之间的真相优先级，
在 JSON / XML 序列化与 round-trip 路径上正式锁死：

- 当 record 已携带
  `SNISupport`
  /
  `OCSPStaplingSupport`
  /
  `SessionTicketsSupport`
  等 support-level truth 时，
  legacy booleans 只能作为派生值
- 当 record 只有 legacy booleans、
  没有任何 support-level truth 时，
  serializer 不应再无条件输出
  `sniSupport="none"`
  之类字段把旧真相反向抹掉

## Scope

- 修改：
  - `src/fafafa.ssl.capability.serializer.pas`
  - `tests/test_capability_serialization_support_level_truth.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- Focused verification：
  - `fpc -B -Fu./src -Fu./tests -otmp/test_capability_serialization_support_level_truth tests/test_capability_serialization_support_level_truth.pas`
  - `./tmp/test_capability_serialization_support_level_truth`
  - `mkdir -p tmp/cap_roundtrip`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/cap_roundtrip -FEtmp/cap_roundtrip -otest_capability_deserialization_roundtrip tests/test_capability_deserialization_roundtrip.pas`
  - `./tmp/cap_roundtrip/test_capability_deserialization_roundtrip`
  - `git diff --check`

## Why This Batch

当前 capability truth 已经部分收口：

- `NormalizeLegacyCapabilityBooleans(...)`
  已经存在
- serializer
  在 support-level truth 存在时
  也会先归一 legacy booleans
- selector
  已优先按 support-level 解释能力

但 round-trip 路径仍可能留着一个 legacy-only 漂移口：

- 如果 in-memory record
  只有 legacy booleans
- serializer
  仍无条件输出
  `sniSupport`
  /
  `ocspStaplingSupport`
  /
  `sessionTicketsSupport`
  这些字段
- 那么反序列化时，
  `none`
  级别字段会被当成显式 truth，
  反过来覆盖 legacy booleans

这会让“legacy-only record round-trip preserve truth”
失效。

## Expected Result

- support-level truth present:
  JSON / XML 输出继续显式带 support-level 字段，
  legacy booleans 与它保持一致
- legacy-only truth:
  JSON / XML 不再凭空生成 support-level 字段，
  round-trip 后原有 boolean truth 保持不变
- 这条 precedence 被 focused test 固定下来，
  后续不再靠静态阅读记忆
