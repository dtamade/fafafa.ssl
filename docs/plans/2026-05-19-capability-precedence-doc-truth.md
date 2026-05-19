# 2026-05-19 Capability Precedence Doc Truth

## Goal

把 capability matrix 的“v1.2 `*Support` 才是 paired feature 真相、legacy `Supports*` 只是兼容投影”明确写进当前高入口文档，避免后续再把两套字段当成并列主真相：

- `docs/CAPABILITY_MATRIX_GUIDE.md`
- `docs/reference/API_REFERENCE.md`
- `docs/BACKEND_CAPABILITY_MATRIX.md`

同时顺手收掉同页还残留的高入口示例漂移：

- capability guide 仍把 `TSSLFactory.GetLibrary(...)` 写成主入口
- capability snippets 仍把 `CompatibilityLevel` 写成 `Byte`
- new-backend 示例还没有把 support-level-first 的构造方式讲清楚

## Scope

- 只修 capability precedence 的活跃文档真相与同页相邻示例漂移
- 用 focused shell contract 锁住：
  - paired feature 在文档中明确以 `*Support` 为真相源
  - legacy `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` / `SupportsCertificateTransparency` / `SupportsSessionTickets` 被标成兼容投影
  - `SupportsTLS13` 仍被明确保留为主 bool truth（因为当前没有 `TLS13Support`）
  - capability guide / API reference 的高入口示例回到 `TSSLFactory.GetLibraryInstance(...)`
- 不修改 runtime 实现
- 不重开 serializer / diff / backend capability 运行时审查

## Files

- `docs/CAPABILITY_MATRIX_GUIDE.md`
- `docs/reference/API_REFERENCE.md`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_capability_precedence_docs_truth_contract.sh`
- `docs/plans/2026-05-19-capability-precedence-doc-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前 capability runtime/source truth 已经固定为：
  - `SNISupport`
  - `ALPNSupport`
  - `OCSPStaplingSupport`
  - `CertTransparencySupport`
  - `SessionTicketsSupport`
  是 paired feature 的主真相源
- 对应 legacy bool：
  - `SupportsSNI`
  - `SupportsALPN`
  - `SupportsOCSPStapling`
  - `SupportsCertificateTransparency`
  - `SupportsSessionTickets`
  只应被视为 compatibility projection
- 这一点已被当前 source comments / serializer / diff / runtime normalization 明确固定：
  - `NormalizeLegacyCapabilityBooleans(...)`
  - support-level-first serializer/deserializer precedence
  - support-level-first diff
- `SupportsTLS13` 当前仍是 primary bool truth：
  - 因为目前没有 `TLS13Support` 支持级别字段
- 高入口 capability 示例当前应优先使用：
  - `TSSLFactory.GetLibraryInstance(...)`
  - `IsFeatureStable(...)`
  - `IsFeatureUsable(...)`

## Steps

1. 新增 focused contract，让 capability docs precedence drift 先 RED。
2. 修正 capability guide / API reference / backend capability matrix 的 precedence 说明与相邻示例。
3. 同步台账，避免后续再把 dual-truth doc drift 当成未验证问题反复拉起。
4. 跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_capability_precedence_docs_truth_contract.sh
bash tests/scripts/test_capability_precedence_docs_truth_contract.sh
git diff --check
```

## Expected Result

- capability 高入口文档不再把 paired feature 的 legacy bool 和 `*Support` 写成并列主真相
- capability guide / API reference 重新回到 support-level-first 的阅读和示例心智
- backend capability matrix 对表格口径增加一条简洁但明确的 precedence 说明

## Result

- 已完成。
- `docs/CAPABILITY_MATRIX_GUIDE.md` / `docs/reference/API_REFERENCE.md` 现在都已明确：
  - paired feature 的 `*Support` 是 truth source
  - legacy `Supports*` 只是 compatibility projection
  - `SupportsTLS13` 仍是 primary bool truth
- capability guide / API reference 的高入口示例现已回到：
  - `TSSLFactory.GetLibraryInstance(...)`
- capability 记录示例中的 `CompatibilityLevel` 现已对齐源码真相：
  - `Integer`
- capability guide 的 new-backend 示例现已明确：
  - paired feature 先写 `*Support`
  - 再 `NormalizeLegacyCapabilityBooleans(Result);`

## Verification

```bash
bash -n tests/scripts/test_capability_precedence_docs_truth_contract.sh
bash tests/scripts/test_capability_precedence_docs_truth_contract.sh
git diff --check
```

- 结果：全部通过
