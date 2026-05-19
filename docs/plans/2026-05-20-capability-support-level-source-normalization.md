# Capability Support-Level Source Normalization

## Goal

把 backend `GetCapabilities` 的 paired capability source 收成单真相：

- backend 只发布 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` /
  `CertTransparencySupport` / `SessionTicketsSupport`
- legacy `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` /
  `SupportsCertificateTransparency` / `SupportsSessionTickets`
  不再由各 backend 手工双写
- 统一通过 `NormalizeLegacyCapabilityBooleans(Result);`
  投影兼容布尔视图

## Architecture

这批做真实 source 收口，不做新一轮大治理：

1. 先补一条 focused shell contract
   - backend source 仍必须调用共享 helper
   - backend source 不得继续直接赋值 paired legacy bool
2. 让五个 live backend 的 `GetCapabilities` 改成：
   - 先设置 support-level truth
   - 最后统一 `NormalizeLegacyCapabilityBooleans(Result);`
3. 复跑 focused static contract + cross-backend runtime contract

## Files

- Add: `docs/plans/2026-05-20-capability-support-level-source-normalization.md`
- Add: `tests/scripts/test_capability_support_level_source_normalization_contract.sh`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `src/fafafa.ssl.mbedtls.lib.pas`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

上一轮已经把 capability truth 的这些层面收紧了：

- selector / builder 走 support-level truth
- serializer / deserializer 走 support-level-first precedence
- diff 已比较 support-level truth
- backend source 末尾统一调用 `NormalizeLegacyCapabilityBooleans(Result);`

但 live backend source 里仍保留“先手工写 legacy bool，再写 support-level，再 normalize 一次”的双写入口。
这会让 source shape 继续暗示 legacy bool 也是 producer 主真相。

因此这批要把 source 形态也收口到和现有 contracts 一致。

## Verification

```bash
bash -n tests/scripts/test_capability_support_level_source_normalization_contract.sh
bash tests/scripts/test_capability_support_level_source_normalization_contract.sh
bash tests/scripts/test_capability_legacy_bool_normalization_contract.sh

mkdir -p tmp/test_capability_source_normalization_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_capability_source_normalization_units \
  -FEtmp/test_capability_source_normalization_units \
  -otmp/test_capability_source_normalization_units/test_capabilities_contract \
  tests/contract/test_capabilities_contract.pas && \
./tmp/test_capability_source_normalization_units/test_capabilities_contract

git diff --check
```

## Expected Outcome

- source 不再手工双写 paired capability bool
- runtime contract 继续证明：
  - support-level truth 仍完整存在
  - compatibility bool 仍与 support-level 投影一致
- 后续若再审 capability producer，入口会更单一、更不容易重新漂移
