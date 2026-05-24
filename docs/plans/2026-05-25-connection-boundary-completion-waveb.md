# Wave B: Connection Boundary Completion

## Goal

对齐 `fafafa.ssl` 的连接侧 owner surfaces、compatibility mirrors 和文档叙事，
让 `ISSLConnection` / `ISSLConnectionInfo` / `ISSLDiagnostics` /
`ISSLSessionResumption` / `ISSLCertificateVerification` / `ISSLOCSPStapling`
在 source truth、architecture doc、API reference 和 roadmap 中保持同一套语义。

## Scope

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `docs/ARCHITECTURE.md`
- `docs/reference/API_REFERENCE.md`
- `docs/ROADMAP.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Rationale

连接侧 owner surfaces 已经存在，当前缺口不是实现能力，而是推荐路径与兼容镜像的叙事还不够统一。
这轮只做 source/doc truth sync，不改 runtime 行为，也不引入新的 public API。

## Steps

1. 为 optional owner interfaces 补齐 `@preferred-access` / `@owner-note` 文案，突出推荐 owner path。
2. 同步 `docs/ARCHITECTURE.md` 与 `docs/reference/API_REFERENCE.md` 的连接侧分层叙事。
3. 更新 `docs/ROADMAP.md`，把当前活跃批次明确成 connection boundary completion。
4. 运行连接侧契约脚本、全量编译和 TLS 1.3 completeness gate。
5. 用 `git diff --check` 做收口，再提交 git。

## Verification

```bash
bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh
python3 scripts/compile_all_modules.py --rebuild
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local
git diff --check
```

## Expected Result

- Source comments and docs agree on connection-side owner paths.
- Roadmap points at the active connection-boundary batch.
- Existing verify-result owner guidance remains green.
