# 2026-05-21 Interface Audit Capability Current-Truth Refresh

## Goal

把 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md` 里关于 capability public surface 的旧结论刷新到当前真相，避免总审查报告继续把已经收口的 support-level-first capability truth 写成 live blocker。

当前更准确的状态应当是：

- capability runtime/source truth
  已经收口到
  support-level-first
- legacy `Supports*`
  仍然存在，
  但主要是
  compatibility projection baggage
- 真正还留在主路线上的，
  更偏向
  `ISSLConnection`
  /
  `TSSLConfig`
  /
  facade
  这些 public API shape debt

## Scope

- 只刷新：
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 新增 focused contract：
  - `tests/scripts/test_interface_audit_capability_current_truth_contract.sh`
- 不修改 runtime 实现
- 不重写历史 verification 报告正文

## Architecture Truth

- `TSSLBackendCapabilities`
  的 paired feature 当前以：
  - `SNISupport`
  - `ALPNSupport`
  - `OCSPStaplingSupport`
  - `CertTransparencySupport`
  - `SessionTicketsSupport`
  - `SessionCacheSupport`
  作为 source/runtime truth
- legacy `Supports*`
  当前主要是
  compatibility projection
  ，由
  `NormalizeLegacyCapabilityBooleans(...)`
  回填
- serializer / deserializer / diff / docs entry
  当前都已经收平到
  support-level-first precedence
- 因而 audit 不应再把 capability 写成
  “仍然存在系统性双真相、尚未决定信哪套字段”

## Verification

```bash
bash -n tests/scripts/test_interface_audit_capability_current_truth_contract.sh
bash tests/scripts/test_interface_audit_capability_current_truth_contract.sh
git diff --check
```

## Expected Outcome

- audit 报告不再把 capability 写成当前 live dual-truth blocker
- capability 残余被更准确分类为 compatibility baggage，而不是未收口的主真相冲突
- 总路线图判断会更集中到真正还在阻碍接口整洁度的 public API shape debt
