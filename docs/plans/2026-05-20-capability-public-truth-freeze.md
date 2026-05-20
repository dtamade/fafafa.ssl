# Capability Public Truth Freeze

## Goal

把 `TSSLBackendCapabilities` 这条 public surface 的剩余双真相叙事正式收口：

- 在源码 record 注释里直接声明 paired feature 的主真相
- 把活跃文档里仍以 legacy `Supports*` 解释 capability 的入口改回 support-level-first
- 用现有 focused contract 锁住，避免下次又从旧示例把路线带偏

## Scope

- Modify: `src/fafafa.ssl.base.pas`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/MIGRATION_GUIDE_V1.1.md`
- Modify: `tests/scripts/test_capability_precedence_docs_truth_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Architecture Truth

- `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport` / `SessionCacheSupport` 是 paired feature 的 source/runtime truth
- legacy `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` / `SupportsCertificateTransparency` / `SupportsSessionTickets` 只是 compatibility projection
- 这些 projection 应由 `NormalizeLegacyCapabilityBooleans(...)` 回填，而不是再被文档当成主真相
- `SupportsTLS13` 当前仍是唯一保留的 primary bool truth，因为还没有 `TLS13Support`

## Why This Batch

- `ISSLConnection` truth-freeze 已基本完成
- `TSSLConfig` scope buckets / migration map 已基本完成
- `ISSLServerConnection` 目前没有足够清晰的 per-connection server-only surface，不适合硬做
- 当前更真实的剩余设计债，是 public capability 入口还残留 legacy-bool-first 叙事，会继续误导后续接口设计与 backend 完整性审查

## Verification

```bash
bash -n tests/scripts/test_capability_precedence_docs_truth_contract.sh
bash tests/scripts/test_capability_precedence_docs_truth_contract.sh
git diff --check
```

## Expected Outcome

- `TSSLBackendCapabilities` record 自身就能说明 paired feature 真相模型
- `BACKEND_CAPABILITY_MATRIX` 不再用 legacy bool 解释 FreePascal ALPN/SNI、WinSSL OCSP、OpenSSL CT
- `MIGRATION_GUIDE_V1.1` 不再教用户从 `SupportsALPN` 读取 paired feature
- 下轮继续做 completeness 主线时，不需要再重新解释 capability dual-truth 的入口规则
