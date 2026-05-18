# TSSLConfig Option-Bridge Surface Truth Freeze

## Goal

把 `TSSLConfig` 中三个 option-bridge 布尔字段的 active public truth 收紧为明确的 `compatibility-only` surface，避免它们继续在普通文档/活跃测试里看起来像推荐主路径。

- `EnableCompression`
- `EnableSessionTickets`
- `EnableOCSPStapling`

## Architecture / Boundary

- 不改 runtime normalization 行为
- 不改 `Options vs legacy booleans` 已冻结的 precedence truth
- 只收口 active source/doc/test guidance：
  - source comment
  - API reference wording
  - dedicated compatibility tests
  - non-compat active test guidance

## Files

- `src/fafafa.ssl.base.pas`
- `docs/reference/API_REFERENCE.md`
- `tests/test_factory_logic.pas`
- `tests/test_data_structures.pas`
- `tests/test_tsslconfig_option_bridge_default_truth.pas`
- `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
- `tests/test_direct_library_default_config_parity.pas`
- `tests/security/test_session_security.pas`
- `tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Steps

1. Tighten `TSSLConfig` field comments so the three booleans are explicitly `compatibility-only` and point new code back to `Options`.
2. Rewrite the active API reference wording so the preferred write surface is `Options`, while the `v1.x` compatibility contract remains explicit.
3. Mark dedicated compatibility tests accordingly and remove option-bridge boolean writes from non-compat active security coverage.
4. Add a focused shell contract that fails if source/docs/tests drift back toward treating these fields as ordinary primary inputs.
5. Run focused verification only.

## Verification Commands

```bash
bash -n tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh
bash tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh
mkdir -p tmp/test_tsslconfig_option_bridge_default_truth
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas
./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth
mkdir -p tmp/test_tsslconfig_option_bridge_precedence_freeze
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_precedence_freeze -FEtmp/test_tsslconfig_option_bridge_precedence_freeze -otmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze tests/test_tsslconfig_option_bridge_precedence_freeze.pas
./tmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze
mkdir -p tmp/test_session_security
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_session_security -FEtmp/test_session_security -otmp/test_session_security/test_session_security tests/security/test_session_security.pas
./tmp/test_session_security/test_session_security
git diff --check
```

## Expected Outputs

- contract script PASS
- option-bridge default/prececence focused tests PASS
- session security test PASS while no longer writing `EnableSessionTickets := ...`
- no whitespace / patch-format issues
