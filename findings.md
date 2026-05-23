# Findings

## Conclusions
- `ISSLConnection` 当前 shipped surface 可以稳定冻结成一张 41-method taxonomy：`17` core、`6` convenience mirrors、`18` compatibility-core mirrors。
- `ReadString` / `WriteString` 和 timeout / blocking 已经是明确的 owner-mapped convenience buckets，分别对应 `ISSLConnectionTextIO` 和 `ISSLConnectionControl`。
- 其余 18 个 compatibility-core mirrors 现在可以按 `ISSLConnectionInfo`、`ISSLDiagnostics`、`ISSLSessionResumption`、`ISSLCertificateVerification`、`ISSLOCSPStapling` 五个 owner family 来读。
- `INTERFACE_DESIGN_V2` 是承载这张 current-shipped taxonomy 的正确位置，因为它既能保留 v2 目标 core，又能不遮蔽当前 source truth。
- taxonomy 这批做完后，下一条更自然的 implementation batch 很可能就是剩余 `ISSLConnectionInfo` family 的进一步收口。
- `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md` 已经把下一条推荐批次前移到 remaining `ISSLConnectionInfo` family，而不再把 taxonomy 当作未完成事项。
- The repo now has an explicit excellence-level architecture anchor: `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md` is the new top-level place to reason about north star, principles, and evolution order.
- The highest-value unfinished design debt remains the same three families, but they now sit inside a clearer global route:
  - `ISSLConnection` core-too-fat / owner taxonomy
  - `TSSLConfig` mixed-scope public model
  - facade historical-path simplification
- The next implementation batch should not reopen closed early-data / OCSP / CT / connection-scope families without fresh RED; it should first build a whole-surface taxonomy for `ISSLConnection`.

## Notes
- `src/fafafa.ssl.base.pas` now makes the current source truth easy to split without ambiguity; the current whole-surface partition is a good candidate for shell-contract guarding.
- The current work should stay doc- and contract-focused; runtime signature churn would only re-open a family that is already stable enough to classify cleanly.
- The key strategic decision in this batch is to treat `ReadString` / `WriteString` and timeout/blocking as explicit `v1.x` convenience mirrors, not as owner-less clutter and not as the immediate first removal target.
- Another key decision is to delay `ISSLServerConnection` symmetry work until after connection-core clarity and config-scope clarity are stronger; fake symmetry would make the public model worse, not better.
