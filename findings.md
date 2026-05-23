# Findings

## Conclusions
- The repo now has an explicit excellence-level architecture anchor: `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md` is the new top-level place to reason about north star, principles, and evolution order.
- The highest-value unfinished design debt remains the same three families, but they now sit inside a clearer global route:
  - `ISSLConnection` core-too-fat / owner taxonomy
  - `TSSLConfig` mixed-scope public model
  - facade historical-path simplification
- The next implementation batch should not reopen closed early-data / OCSP / CT / connection-scope families without fresh RED; it should first build a whole-surface taxonomy for `ISSLConnection`.

## Notes
- The key strategic decision in this batch is to treat `ReadString` / `WriteString` and timeout/blocking as explicit `v1.x` convenience mirrors, not as owner-less clutter and not as the immediate first removal target.
- Another key decision is to delay `ISSLServerConnection` symmetry work until after connection-core clarity and config-scope clarity are stronger; fake symmetry would make the public model worse, not better.
