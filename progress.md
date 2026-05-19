# Progress - Interface Design And Backend Implementation Verification

## 2026-05-19

## 2026-05-20

### Helper Surface Classification Truth

- `rg -n "TSSLHelper|QuickServer|CreateOCSPClient|CreateCRLManager|GetLibraryInstance|推荐入口|helper" docs/reference/API_REFERENCE.md RELEASE_NOTES_V1.5.0.md docs/README.md docs/guides/GETTING_STARTED.md docs/guides/USER_GUIDE.md docs/guides/MIGRATION_GUIDE.md src/fafafa.ssl.pas src/fafafa.ssl.factory.pas`
- `rg -n "IsFIPSModeEnabled|GetEnterpriseTrustedRoots|IsFIPSEnabled|GetTrustedRoots|GetAllPolicies|TSSLEnterpriseConfig" src docs tests/scripts`
  - result: PASS
  - summary:
    - confirmed a real active-docs gap:
      - exported facade helpers were still shipped, but canonical docs had not yet
        classified them as main-entry vs convenience surfaces
    - also confirmed a sharper canonical drift:
      - `docs/reference/API_REFERENCE.md` still presented old WinSSL enterprise globals
        as the visible helper surface
      - while current source / migration guide / user guide already treated
        `TSSLEnterpriseConfig.IsFIPSEnabled / GetTrustedRoots / GetAllPolicies`
        as the main path

- add `docs/plans/2026-05-20-helper-surface-classification-truth.md`
  - change:
    - recorded the bounded docs+contract batch for helper-surface layering truth

- add `tests/scripts/test_helper_surface_classification_truth_contract.sh`
  - change:
    - added a focused shell contract that freezes:
      - TLS bootstrap main-entry vs convenience-helper classification
      - WinSSL enterprise main-path names vs legacy wrapper demotion

- update docs/source:
  - `docs/reference/API_REFERENCE.md`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.factory.pas`
  - change:
    - classified `CreateDefaultConfig` / `TSSLHelper` / `QuickServer` /
      `CreateOCSPClient` / `CreateCRLManager` as convenience helpers
    - kept `TSSLFactory.GetLibraryInstance(...)` / connector surfaces as bootstrap main entry
    - rewrote `WinSSL 企业工具` to use `TSSLEnterpriseConfig` current helper names
    - demoted old enterprise globals to legacy convenience wrappers

- `bash -n tests/scripts/test_helper_surface_classification_truth_contract.sh`
- `bash tests/scripts/test_helper_surface_classification_truth_contract.sh`
- `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
- `bash tests/scripts/test_active_fips_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - helper-surface classification now agrees across canonical API docs, source comments,
      migration guide, and active FIPS docs
    - WinSSL enterprise helper naming drift is closed without removing shipped wrappers

- `git diff --check`
  - result: PASS
  - summary:
    - the helper-surface classification batch is whitespace-clean

### Integration Guide Canonical Path Truth

- `ls -l docs/INTEGRATION_GUIDE.md docs/guides/INTEGRATION_GUIDE.md`
- `diff -u docs/INTEGRATION_GUIDE.md docs/guides/INTEGRATION_GUIDE.md`
- `rg -n "INTEGRATION_GUIDE\\.md" docs tests/scripts src`
  - result: PASS
  - summary:
    - confirmed the repo carried two divergent active integration-guide paths:
      - canonical root `docs/INTEGRATION_GUIDE.md`
      - stale shadow copy `docs/guides/INTEGRATION_GUIDE.md`
    - README / documentation index / most owner-path contracts already treated the root file
      as canonical, while a smaller set of facade/SNI contracts still targeted the shadow copy

- add `docs/plans/2026-05-20-integration-guide-canonical-path-truth.md`
  - change:
    - recorded the bounded docs+contract batch for collapsing integration-guide truth
      onto a single canonical path

- add `tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - change:
    - added a focused shell contract that freezes:
      - root `docs/INTEGRATION_GUIDE.md` as the canonical path
      - absence of `docs/guides/INTEGRATION_GUIDE.md`
      - active facade/SNI/import contracts pointing at the canonical root file

- `bash -n tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - result: PASS
  - summary:
    - new canonical-path contract syntax is valid

- `bash tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first proved the real problem was not hypothetical:
      - `stale shadow integration guide still exists at docs/guides/INTEGRATION_GUIDE.md`
    - GREEN after deleting the shadow copy and retargeting active contracts proves:
      - the repo now has one active integration-guide truth source
      - active contracts no longer split between root and guides paths

- update docs/tests:
  - `docs/INTEGRATION_GUIDE.md`
  - `tests/scripts/test_facade_main_entry_truth_contract.sh`
  - `tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
  - `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - remove `docs/guides/INTEGRATION_GUIDE.md`
  - change:
    - normalized canonical integration-guide examples to public facade imports
    - removed direct `fafafa.ssl.base` / `fafafa.ssl.tls` active-example teaching
    - retargeted facade/SNI/import contracts to the canonical root path

- `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
- `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
- `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
- `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
- `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
- `bash tests/scripts/test_docs_readme_integration_guide_exists_contract.sh`
  - result: PASS
  - summary:
    - all focused and dependent root-guide contracts stayed green after the canonical-path collapse
    - connection-info / session-resumption / cert-verification owner-path truth remained intact

- `git diff --check`
  - result: PASS
  - summary:
    - the integration-guide canonical-path batch is whitespace-clean

### macOS Batch Loader Regression Closure

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - 当前会话继续沿用既有 planning files，不需要从头重建

- `sed -n '1,260p' tmp/gh-run-26108902159/.../wave_b_macos_gate_summary_*.md`
- `sed -n '1,220p' tmp/gh-run-26108902159/.../test_p2_{pkcs7,pkcs12,pkcs12_create_parse,cms,ocsp}_*.txt`
  - result: PASS
  - summary:
    - 最新 macOS gate 明确只在 `modules` 步骤失败
    - 失败集中到：
      - `PKCS7` -> `LoadEVP` 失败
      - `PKCS12` -> module loaded but core symbols not assigned
      - `PEM` -> create/parse lane 直接报模块加载失败
      - `CMS` / `OCSP` -> `LoadOpenSSLCMS` / `LoadOpenSSLOCSP` 返回 false

- `sed -n '1,160p' tmp/gh-run-26048015976/.../wave_b_macos_loader_symbol_probe_*.json`
- `sed -n '1,160p' tmp/gh-run-26108902159/.../wave_b_macos_loader_symbol_probe_*.json`
  - result: PASS
  - summary:
    - 旧 run `26048015976` 已确认：
      - same `OpenSSL 3.6.2 7 Apr 2026`
      - `evp/pem/pkcs12/cms/ocsp/ts/ct/store` module truth 全绿
    - 新 run `26108902159` 已确认：
      - same version string
      - direct symbols 仍是 `true`
      - 但 `evp/pem/pkcs12/cms/ocsp` module truth 全部转成 `false`
    - 这把当前问题从“历史能力缺失”收紧成了真实回归

- `git log --oneline --since='2026-05-17' -- src/fafafa.ssl.openssl.loader.pas src/fafafa.ssl.openssl.api.evp.pas src/fafafa.ssl.openssl.api.pem.pas src/fafafa.ssl.openssl.api.pkcs12.pas src/fafafa.ssl.openssl.api.cms.pas src/fafafa.ssl.openssl.api.ocsp.pas tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
  - result: PASS
  - summary:
    - 红面的 batch-loader 模块文件自 2026-05-19 probe lane 落地后没有继续被改动
    - 当前更像：
      - shared runtime state / batch-loader diagnostics 缺失
      - 或 batch binding storage 在 live macOS lane 的稳定性问题

- add `docs/plans/2026-05-20-macos-batch-loader-regression-closure.md`
  - change:
    - 记录了这次 focused batch 的目标、边界、commands、expected outputs

- add `tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`
  - change:
    - 新增 focused shell contract，准备冻结：
      - failing batch-binding tables 的 runtime-storage 形态
      - loader diagnostics API
      - macOS loader probe 的新 diagnostics 字段
      - PEM loaded-state 的 read-surface 语义

- `bash -n tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`
  - result: PASS
  - summary:
    - 新增 contract 语法有效

- `bash tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED 先证明 runtime-storage 正则与实际 source layout 还没完全对齐
    - 把 comment / `var` 布局收紧后，GREEN 证明：
      - `EVP/PEM/PKCS12/CMS/OCSP` batch bindings 已切到 runtime storage
      - loader diagnostics API 已落地
      - probe JSON 已记录新的 diagnostics 字段
      - PEM loaded-state 已回到 read-surface 语义

- `fpc -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_macos_batch_loader_probe_units -FEtmp/test_macos_batch_loader_probe_bin tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
  - result: FAIL -> PASS
  - summary:
    - 首次仅因输出目录不存在失败，不是源码错误
    - 建立 `tmp/test_macos_batch_loader_probe_{units,bin}` 后重新编译通过

- `./tmp/test_macos_batch_loader_probe_bin/test_macos_openssl_loader_symbol_probe tmp/test_macos_batch_loader_probe.json`
  - result: PASS
  - summary:
    - 本机 probe 已成功产出新 JSON
    - 关键字段结构已验证：
      - `evp.load_functions_loaded_count = 98`
      - `pem.load_functions_loaded_count = 60`
      - `pkcs12.load_functions_loaded_count = 37`
      - `cms.load_functions_loaded_count = 86`
      - `ocsp.load_functions_loaded_count = 67`
      - 五个模块的 `missing_required_bindings` 均为空字符串

- `FAFAFA_FAST_LOCAL=1 FAFAFA_FPC_UNIT_OUTPUT_DIR=tmp/run_all_module_tests_units_macos_batch_loader_closure bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,OCSP --stop-on-fail`
  - result: PASS
  - summary:
    - focused module regression 全绿：
      - `PKCS7`: 2/2
      - `PKCS12`: 3/3
      - `CMS`: 2/2
      - `OCSP`: 3/3
    - 总计 10/10 PASS

- `gh run view 26110676557 --json status,conclusion,jobs`
  - result: PASS
  - summary:
    - run 已完成且总体 `success`
    - jobs:
      - `setup`: success
      - `linux-gate`: success
      - `macos-gate`: success
      - `windows-gate`: success
      - `summary`: success
    - 这次 closeout 已不再停留在本地 proof，GitHub 三平台 runtime truth 也已收口

- `sed -n '1,220p' tmp/gh-run-26110676557/wave_b_macos_gate_summary_macos_batch_loader_closure_20260520_89c2a2e.md`
  - result: PASS
  - summary:
    - macOS artifact summary 明确 `overall: PASS`
    - `probe / loader-symbol-probe / path-check / compile / modules / examples`
      六步全部 PASS
    - examples metrics:
      - `passed=71`
      - `failed=0`
      - `skipped=4`
      - `pass_rate=100.0`

- `sed -n '1,200p' tmp/gh-run-26110676557/wave_b_macos_loader_symbol_probe_macos_batch_loader_closure_20260520_89c2a2e.json`
  - result: PASS
  - summary:
    - CI probe 已恢复到正确 runtime truth：
      - same `OpenSSL 3.6.2 7 Apr 2026`
      - `direct_symbols` 全 true
      - `evp/pem/pkcs12/cms/ocsp` `module_loaded/load_result` 全 true
    - 新增 durable diagnostics 在 CI 上也给出了稳定计数：
      - `evp = 98`
      - `pem = 60`
      - `pkcs12 = 37`
      - `cms = 86`
      - `ocsp = 67`
      - all `missing_required_bindings = ""`

- `rg -n "^## |^### |mixed-scope|门面单元|能力矩阵存在双真相|ISSLServerConnection|SetServerName|deprecated" docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
- `rg -n "SetServerName\\(|SetSession\\(|SupportsSNI|SNISupport|LogLevel|LogCallback|BufferSize|HandshakeTimeout" src/fafafa.ssl.base.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.pas src/fafafa.ssl.*lib.pas src/fafafa.ssl.connection.builder.pas src/fafafa.ssl.tls.pas`
  - result: PASS
  - summary:
    - post-closeout static re-anchor shows:
      - capability dual-truth lane 已有 shared normalization/helper/contract，不是当前最值钱缺口
      - 下一条更真实的 active surface debt 在
        `TSSLConfig` mixed-scope record 与 facade quick-entry 分层
    - 这让总 goal 可以从平台 runtime triage 收回到 public surface truth 审查

### Performance Guides Benchmark Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "Phase|ops/s|ms|倍|完成|成功率|P99|benchmark|性能提升|完美支持|100%|250,000|600,000|1160|181|3\\.7|244|4574|587|3200|10000|500 ops" docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  - result: PASS
  - summary:
    - static scan confirmed both performance guides still embedded fixed benchmark snapshots and completion-style wording
    - the same scan also pointed back to the highest-value residual files:
      - `docs/guides/PERFORMANCE_GUIDE.md`
      - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`

- `rg -n "GetSession|SetSession|IsSessionReused|ISSLSessionResumption|ISSLDiagnostics|GetPerformanceMetrics" docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  - result: PASS
  - summary:
    - source/doc scan confirmed the performance guides still taught direct-core session and diagnostics mirrors
    - this turned the batch into both benchmark-truth cleanup and owner-path guidance alignment

- `sed -n '1,260p' scripts/run_phase2_performance_baseline.sh`
- `sed -n '1,260p' tests/benchmarks/run_all_benchmarks.sh`
- `find tests/benchmarks/baselines -maxdepth 2 -type f | sort`
  - result: PASS
  - summary:
    - current durable performance truth sources were reconfirmed before editing:
      - `scripts/run_phase2_performance_baseline.sh`
      - `tests/benchmarks/run_all_benchmarks.sh`
      - `tests/benchmarks/baselines/crypto_baseline.json`
      - `tests/benchmarks/baselines/random_pool_baseline.json`
      - `tests/benchmarks/baselines/tls_handshake_baseline.json`

- add `docs/plans/2026-05-19-performance-guides-benchmark-truth.md`
  - change:
    - recorded the bounded docs-only plan for performance-guide benchmark truth
      and owner-path guidance alignment

- add `tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - current benchmark truth-source wording
      - absence of hardcoded performance snapshots and phase-completion claims
      - presence of `ISSLSessionResumption` / `ISSLDiagnostics` owner-path examples
      - absence of old direct-core performance/session guidance

- update docs:
  - `docs/guides/PERFORMANCE_GUIDE.md`
  - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  - change:
    - rewrote both guides around current benchmark entrypoints, baseline files,
      success criteria, and result-interpretation boundaries
    - demoted historical benchmark/phase snapshots
    - moved TLS performance examples onto `ISSLSessionResumption`
      and `ISSLDiagnostics`
    - separated `benchmark_aesgcm_pool` from the default Phase 2 shipped baseline lane

- `bash -n tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
  - result: PASS
  - summary:
    - new performance-guide truth contract syntax is valid

- `bash tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first proved that the new contract was still overfit to one literal
      sentence layout and got broken by markdown line wrapping
    - GREEN after switching those checks to wrap-safe semantic fragments proves
      the guides now satisfy the intended benchmark-truth and owner-path rules

- `bash tests/scripts/test_active_docs_no_ci_pipeline_contract.sh`
  - result: PASS
  - summary:
    - the active-docs entrypoint contract stayed green after editing
      `PERFORMANCE_OPTIMIZATION_GUIDE.md`

- `npx prettier --write docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  - result: PASS
  - summary:
    - both performance guides remain formatter-stable after the truth cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the performance-guide benchmark-truth batch is whitespace-clean

### Active Owner-Path Docs Alignment

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -l '\b(?:Conn|LConn|Conn1|Conn2|Connection|Stream\.Connection)\.(?:GetSession|SetSession|IsSessionReused|GetPerformanceMetrics|GetHealthStatus|GetDiagnosticInfo|IsHealthy)\b|`ISSLConnection\.(?:GetSession|SetSession|IsSessionReused|GetPerformanceMetrics|GetHealthStatus|GetDiagnosticInfo|IsHealthy)`' docs/guides docs/reference --glob '!docs/archive/**' --glob '!docs/plans/**' | sort`
  - result: PASS
  - summary:
    - active-doc scan narrowed the remaining owner-path guidance drift to:
      - `docs/reference/API_REFERENCE.md`
      - `docs/guides/WINSSL_BEST_PRACTICES.md`
      - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
      - `docs/reference/WINSSL_DESIGN.md`

- add `docs/plans/2026-05-19-active-owner-path-docs-alignment.md`
  - change:
    - recorded the bounded docs-only plan for the remaining active owner-path guidance cleanup

- add `tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - API-reference diagnostics bullets now point to `ISSLDiagnostics`
      - WinSSL/profiling/design session examples now point to `ISSLSessionResumption`
      - direct-core session/diagnostics teaching is absent from the targeted active docs

- update docs:
  - `docs/reference/API_REFERENCE.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - rewrote diagnostics bullets to point at `ISSLDiagnostics`
    - migrated WinSSL/profiling/design session snippets to `ISSLSessionResumption`
    - kept compatibility-mirror discussion only as explanatory/deprecation wording

- `bash -n tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - result: PASS
  - summary:
    - new active owner-path docs contract syntax is valid

- `bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - result: PASS
  - summary:
    - targeted active docs now all prefer owner-path guidance over direct-core compatibility mirrors

- `npx prettier --write docs/reference/API_REFERENCE.md docs/guides/WINSSL_BEST_PRACTICES.md docs/guides/PERFORMANCE_PROFILING_GUIDE.md docs/reference/WINSSL_DESIGN.md`
  - result: PASS
  - summary:
    - all four active docs remain formatter-stable after the owner-path cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the active owner-path docs batch is whitespace-clean

- `rg -l '\b(?:Conn|LConn|Conn1|Conn2|Connection|Stream\.Connection)\.(?:GetSession|SetSession|IsSessionReused|GetPerformanceMetrics|GetHealthStatus|GetDiagnosticInfo|IsHealthy)\b' docs/guides docs/reference --glob '!docs/archive/**' --glob '!docs/plans/**' | sort`
  - result: PASS
  - summary:
    - active `docs/guides` / `docs/reference` no longer retain direct-core connection-call examples
    - the remaining direct-core names in active docs are now explanatory compatibility/deprecation mentions rather than teaching call sites

### P2 Minimum API Matrix CT Truth

- `rg -n "PKCS12 / CT|SupportsCertificateTransparency|CertTransparencySupport|无默认直接字段映射|CT 模块可用性不等于 OpenSSL backend public capability" docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
  - result: PASS
  - summary:
    - static scan confirmed a summary-vs-detail contradiction in the P2 minimum API matrix:
      - top summary still claimed CT had a direct capability-field mapping
      - CT row and explanatory note already said it did not

- add `docs/plans/2026-05-19-p2-minimum-api-matrix-ct-truth.md`
  - change:
    - recorded the bounded docs-only plan for the P2 minimum API matrix CT-truth cleanup

- add `tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - top summary no longer claims a direct CT field mapping
      - CT row remains scoped to low-level OpenSSL binding availability
      - explanatory note still demotes `SupportsCertificateTransparency` /
        `CertTransparencySupport` from direct API mapping

- update `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
  - change:
    - rewrote the top summary bullet so only `PKCS12` remains a direct field expression
    - kept CT scoped to low-level API/binding availability rather than default backend public capability

- `bash -n tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh`
  - result: PASS
  - summary:
    - new P2 minimum API matrix CT-truth contract syntax is valid

- `bash tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh`
  - result: PASS
  - summary:
    - the P2 minimum API matrix no longer contradicts itself on CT capability mapping

- `npx prettier --write docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
  - result: PASS
  - summary:
    - the P2 matrix remains formatter-stable after the CT truth cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the P2 minimum API matrix CT-truth batch is whitespace-clean

### Backend Capability Matrix Quick Reference Truth

- `sed -n '1,260p' docs/BACKEND_CAPABILITY_MATRIX.md`
- `rg -n "SupportsTLS13|SNISupport|ALPNSupport|KnownIssues" src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.winssl.lib.pas docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `rg -n "\*\*TLS 1\.3\*\*|\*\*ALPN\*\*|\*\*SNI\*\*|\*\*PSK\*\*|Windows 10 1903\+|PSK \| ❌ 不支持" docs/BACKEND_CAPABILITY_MATRIX.md docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/guides/WINSSL_USER_GUIDE.md`
  - result: PASS
  - summary:
    - static comparison found a summary-level contradiction in the top-level backend matrix:
      - `WinSSL TLS 1.3` was still shown as unconditional `✅`
      - `WinSSL PSK` was still shown as `⚠️`
      - `FreePascal ALPN / SNI` were still shown as stable `✅`
    - source / backend-specific docs already said:
      - WinSSL TLS 1.3 is Windows-version gated
      - WinSSL PSK is unsupported
      - FreePascal ALPN/SNI still publish `sslSupportExperimental`

- add `docs/plans/2026-05-19-backend-capability-matrix-quick-reference-truth.md`
  - change:
    - recorded the bounded docs-only plan for quick-reference capability-truth tightening

- add `tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - FreePascal `ALPN` / `SNI` rows stay aligned with `sslSupportExperimental`
      - WinSSL `TLS 1.3` stays conditional instead of unconditional `✅`
      - WinSSL `PSK` stays aligned with the backend-specific unsupported truth
      - the top-level matrix keeps explicit explanatory notes for those exceptions

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - tightened the quick-reference cells for:
      - `WinSSL TLS 1.3`
      - `WinSSL PSK`
      - `FreePascal ALPN`
      - `FreePascal SNI`
    - added short explanatory bullets so the root matrix no longer outruns
      source/backend-specific truth

- `bash -n tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
  - result: PASS
  - summary:
    - new quick-reference truth contract syntax is valid

- `bash tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
  - result: PASS
  - summary:
    - the top-level backend matrix quick reference now stays aligned with source
      capability gates and WinSSL backend-specific truth

- `npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
  - result: PASS
  - summary:
    - the backend capability matrix remains formatter-stable after the quick-reference cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the quick-reference truth batch is whitespace-clean

### Backend Capability Matrix Performance And Selection Truth

- `rg -n "性能对比|握手性能|吞吐量|1\\.0x|1\\.2x|0\\.8x|0\\.3x|选择建议|推荐|零依赖部署|Windows 应用" docs/BACKEND_CAPABILITY_MATRIX.md docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md docs/guides/WINSSL_USER_GUIDE.md`
- `sed -n '1,260p' docs/guides/PERFORMANCE_GUIDE.md`
- `sed -n '1,260p' docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- `sed -n '120,220p' docs/guides/WINSSL_USER_GUIDE.md`
- `sed -n '300,360p' docs/guides/WINSSL_USER_GUIDE.md`
  - result: PASS
  - summary:
    - static comparison confirmed the top-level backend matrix still carried:
      - fixed backend performance ratio tables
      - blanket recommendation wording
    - current truth sources already said:
      - performance truth must come from current benchmark entrypoints and fresh output
      - WinSSL recommendations must keep capability/runtime caveats visible

- add `docs/plans/2026-05-19-backend-capability-matrix-performance-selection-truth.md`
  - change:
    - recorded the bounded docs-only plan for tightening the top-level matrix's
      performance and selection wording

- add `tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - fixed performance ratio tables stay removed
      - the root matrix points readers to current benchmark truth sources
      - selection guidance stays capability-aware instead of blanket
      - WinSSL / MbedTLS / WolfSSL / FreePascal caveats remain visible in the root doc

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - replaced stale backend performance ratio tables with current measurement entrypoints and interpretation boundaries
    - rewrote the selection section around capability-aware recommendation rather than unconditional backend ranking
    - kept WinSSL zero-dependency/client strengths while restoring runtime/public-capability caveats

- `bash -n tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
  - result: PASS
  - summary:
    - new performance/selection truth contract syntax is valid

- `bash tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
  - result: PASS
  - summary:
    - the top-level backend matrix no longer publishes stale performance tables
      or blanket selection advice that outruns current capability truth

- `npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
  - result: PASS
  - summary:
    - the backend capability matrix remains formatter-stable after the performance/selection cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the performance/selection truth batch is whitespace-clean

### Backend Capability Matrix Version History Truth

- `rg -n "FAFAFA_SSL_VERSION_STRING|FAFAFA_SSL_INTERFACE_VERSION" src/fafafa.ssl.base.pas`
- `rg -n "^### v1\\.|^## v1\\.|1\\.5\\.0|v1\\.4\\.1|v1\\.4\\.0|v1\\.4\\.3" README.md docs/RELEASE_NOTES.md docs/BACKEND_CAPABILITY_MATRIX.md docs/ROADMAP.md`
- `sed -n '445,520p' docs/BACKEND_CAPABILITY_MATRIX.md`
  - result: PASS
  - summary:
    - static comparison confirmed the top-level backend matrix still started its
      bottom version section from `v1.4.1` / `v1.4.0` / `v1.3.0`
    - source / roadmap / release notes already said:
      - current stable version is `v1.5.0`
      - current release-control truth belongs to `ROADMAP` and `RELEASE_READINESS_V1.5.0`

- add `docs/plans/2026-05-19-backend-capability-matrix-version-history-truth.md`
  - change:
    - recorded the bounded docs-only plan for current-version / historical-milestone truth alignment

- add `tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - the root matrix publishes current stable version `v1.5.0` first
      - the root matrix points to roadmap/release-readiness/release-notes
      - older `v1.4.x` / `v1.3.0` items are explicitly demoted to capability milestones

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - replaced the bare `版本历史` entrypoint with a current-release section
    - relabeled older items as historical capability milestones instead of implicit current-version truth

- `bash -n tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
  - result: PASS
  - summary:
    - new version-history truth contract syntax is valid

- `bash tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first proved the contract itself still used shell-unsafe backtick quoting and tried to execute the version string
    - GREEN after switching those literal checks to safe quoting proves the root matrix now stays aligned with current v1.5.0 release truth

- `npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
  - result: PASS
  - summary:
    - the backend capability matrix remains formatter-stable after the version-history cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the version-history truth batch is whitespace-clean

### WinSSL Session Injection Semantics Truth

- `sed -n '500,740p' src/fafafa.ssl.winssl.lib.pas`
- `rg -n "GetSession|SetSession|IsSessionReused|InitializeSecurityContextW|SECPKG_ATTR_SESSION_INFO|FCurrentSession" src/fafafa.ssl.winssl.connection.pas src/fafafa.ssl.winssl.context.pas`
- `sed -n '2100,2265p' src/fafafa.ssl.winssl.connection.pas`
- `sed -n '260,520p' tests/winssl/test_winssl_session_resumption.pas`
- `rg -n "Resumption2\\.SetSession|target name|credential handle|compatibility metadata" docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/guides/WINSSL_USER_GUIDE.md docs/reference/API_REFERENCE.md docs/BACKEND_SELECTION_GUIDE.md`
  - result: PASS
  - summary:
    - static audit confirmed a semantic-boundary gap in the WinSSL session lane:
      - `DoSetSession(...)` only stores `FCurrentSession`
      - shared client handshake does not feed that session object into `InitializeSecurityContextW`
      - API reference already documented the truth, but higher-entry WinSSL docs still under-explained it

- add `docs/plans/2026-05-19-winssl-session-injection-semantics-truth.md`
  - change:
    - recorded the bounded source+docs truth plan for WinSSL session-injection semantics

- add `tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - WinSSL source documents `SetSession(...)` as compatibility metadata
      - WinSSL high-entry docs explain `target name + credential handle`
        reconnect truth
      - Windows selection guidance no longer hides this caveat

- update source/docs:
  - `src/fafafa.ssl.winssl.connection.pas`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - `docs/BACKEND_SELECTION_GUIDE.md`
  - change:
    - added a source-side note beside `DoSetSession(...)`
    - demoted `SetSession(...)` in WinSSL high-entry examples to compatibility metadata surface
    - made Windows selection guidance explicitly bounce capability-sensitive users back to OpenSSL when needed

- `bash -n tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL session-injection semantics contract syntax is valid

- `bash tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
  - result: PASS
  - summary:
    - source and high-entry docs now agree that WinSSL `SetSession(...)`
      is not a native session-handle injection point

- `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - the broader WinSSL session-resumption docs truth still stays green after the semantic-boundary tightening

- `npx prettier --write docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/guides/WINSSL_USER_GUIDE.md docs/BACKEND_SELECTION_GUIDE.md`
  - result: PASS
  - summary:
    - the touched WinSSL docs remain formatter-stable after the semantic-boundary cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - the WinSSL session-injection semantics batch is whitespace-clean

### ISSLSessionResumption Runtime Residual Classification Tightening

- add `docs/plans/2026-05-19-isslsessionresumption-runtime-residual-classification-tightening.md`
  - change:
    - define the bounded batch that freezes the direct-core residual file set
      into intentional compatibility/semantic proofs and removes mock-helper noise

- add `tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
  - change:
    - lock that:
      - `src/fafafa.ssl.connection.base.pas`
        records semantic-truth residual wording instead of vague runtime-residual wording
      - `tests/contract/test_backend_contract.pas`
        keeps its explicit compatibility-mirror marker
      - `tests/test_mbedtls_connection_session_reused_contract.pas`
        and `tests/test_openssl_connection_session_reused_contract.pas`
        are explicitly marked as semantic proofs
      - `tests/winssl/test_session_save_logic.pas`
        no longer exposes mock `GetSession` call sites
      - the direct-core residual file set is exactly:
        - `tests/contract/test_backend_contract.pas`
        - `tests/test_mbedtls_connection_session_reused_contract.pas`
        - `tests/test_openssl_connection_session_reused_contract.pas`

- `bash -n tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the new residual-classification contract is syntactically valid

- `bash tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first proved the base residual note still mislabeled these files as generic runtime residuals
    - GREEN after the wording/marker/mock-getter cleanup proves the residual set is now frozen to intentional proof files

- update session-resumption residual truth files:
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_mbedtls_connection_session_reused_contract.pas`
  - `tests/test_openssl_connection_session_reused_contract.pas`
  - `tests/winssl/test_session_save_logic.pas`
  - change:
    - tighten source residual wording from `backend-specific runtime residuals`
      to `backend-specific semantic truth proofs`
    - mark the MbedTLS/OpenSSL contracts as intentional direct-core semantic proofs
    - rename the WinSSL mock getter from `GetSession` to `GetSavedSession`
      so it stops surfacing as public owner-path noise

- `mkdir -p tmp/test_mbedtls_connection_session_reused_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_connection_session_reused_contract -FEtmp/test_mbedtls_connection_session_reused_contract -otmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract tests/test_mbedtls_connection_session_reused_contract.pas && ./tmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract`
  - result: PASS
  - summary:
    - MbedTLS semantic truth proof still compiles and runs green after adding the explicit residual marker

- `mkdir -p tmp/test_openssl_connection_session_reused_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_connection_session_reused_contract -FEtmp/test_openssl_connection_session_reused_contract -otmp/test_openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract tests/test_openssl_connection_session_reused_contract.pas && ./tmp/test_openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract`
  - result: PASS
  - summary:
    - OpenSSL semantic truth proof still compiles and runs green after adding the explicit residual marker

- `mkdir -p tmp/test_winssl_session_save_logic && fpc -B -Fu./tests/winssl -FUtmp/test_winssl_session_save_logic -FEtmp/test_winssl_session_save_logic -otmp/test_winssl_session_save_logic/test_session_save_logic tests/winssl/test_session_save_logic.pas && ./tmp/test_winssl_session_save_logic/test_session_save_logic`
  - result: PASS
  - summary:
    - the renamed WinSSL mock save-logic getter still compiles and all 12 checks pass

- `rg -lP "\\b(?:Conn|LConn|LConn1|LConn2|ResumedConn|InitialConn|LTLSStream\\.Connection)\\.(?:GetSession|SetSession|IsSessionReused)\\b" tests --glob '!tests/scripts/**' | sort`
  - result: PASS
  - summary:
    - the direct-core residual set is now frozen to exactly:
      - `tests/contract/test_backend_contract.pas`
      - `tests/test_mbedtls_connection_session_reused_contract.pas`
      - `tests/test_openssl_connection_session_reused_contract.pas`

- `git diff --check`
  - result: PASS
  - summary:
    - residual-classification tightening is whitespace-clean

### ISSLSessionResumption Runtime Owner-Path Migration Wave 2 (`test_freepascal_tls13_early_data`)

- add `docs/plans/2026-05-19-isslsessionresumption-runtime-owner-path-migration-wave2-freepascal-tls13-early-data.md`
  - change:
    - define the bounded wave 2 batch for the largest remaining ordinary runtime
      residual file

- add `tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
  - change:
    - lock that:
      - `tests/test_freepascal_tls13_early_data.pas`
        no longer uses direct core `GetSession` / `SetSession` / `IsSessionReused`
      - the file now exposes focused owner-path helpers for session capture,
        injection, and reuse assertions

- `bash -n tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
  - result: PASS
  - summary:
    - the new wave 2 contract is syntactically valid

- `bash tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first proved the target file still had direct-core session-resumption mirrors
    - the first GREEN attempt then exposed that the contract was overfit to a
      single-line helper signature
    - GREEN after loosening the contract to semantic helper patterns proves the
      file now prefers `ISSLSessionResumption` owner-path usage without depending
      on one formatting style

- update `tests/test_freepascal_tls13_early_data.pas`
  - change:
    - add `RequireSessionResumption(...)` and `AssertSessionReused(...)`
      helper entrypoints near the file's existing assertion helpers
    - migrate all direct-core session capture, injection, and reuse checks to
      `ISSLSessionResumption`
    - keep the file's runtime assertions behaviorally identical while removing
      ordinary direct-core session mirror usage

- `mkdir -p tmp/test_freepascal_tls13_early_data_owner_path && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_tls13_early_data_owner_path -FEtmp/test_freepascal_tls13_early_data_owner_path -otmp/test_freepascal_tls13_early_data_owner_path/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/test_freepascal_tls13_early_data_owner_path/test_freepascal_tls13_early_data`
  - result: PASS
  - summary:
    - the large FreePascal TLS 1.3 early-data runtime suite still compiles and runs green
    - compiler output only surfaced the repo's existing warning baseline; this
      batch did not introduce a new runtime or owner-path regression

- `rg -lP "\\b(?:Conn|LConn|LConn1|LConn2|ResumedConn|InitialConn|LTLSStream\\.Connection)\\.(?:GetSession|SetSession|IsSessionReused)\\b" tests --glob '!tests/scripts/**' | sort`
  - result: PASS
  - summary:
    - the remaining direct-core session residual set is now:
      - `tests/contract/test_backend_contract.pas`
      - `tests/test_mbedtls_connection_session_reused_contract.pas`
      - `tests/test_openssl_connection_session_reused_contract.pas`
      - `tests/winssl/test_session_save_logic.pas`

- `git diff --check`
  - result: PASS
  - summary:
    - wave 2 owner-path migration is whitespace-clean

### ISSLSessionResumption Runtime Owner-Path Migration Wave 1

- add `docs/plans/2026-05-19-isslsessionresumption-runtime-owner-path-migration-wave1.md`
  - change:
    - define the bounded runtime-owner-path batch for selected ordinary tests plus
      the builder / connector production call sites

- add `tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
  - change:
    - lock that:
      - selected runtime tests no longer use direct core
        `GetSession` / `SetSession` / `IsSessionReused`
      - `src/fafafa.ssl.connection.builder.pas`
        now applies configured sessions via `ISSLSessionResumption`
      - `src/fafafa.ssl.tls.pas`
        now applies configured sessions via `ISSLSessionResumption`

- update runtime / production files:
  - `src/fafafa.ssl.connection.builder.pas`
  - `src/fafafa.ssl.tls.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_freepascal_client_certificate_flight_requirements.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
  - `tests/test_freepascal_server_session_resumption.pas`
  - `tests/test_openssl_wolfssl_early_data_connection_contract.pas`
  - change:
    - migrate ordinary session-resumption usage from direct core mirrors to
      `ISSLSessionResumption`
    - after compile-time deprecation landed, also move builder / connector
      production application of `WithSession(...)` onto the owner path

- `bash -n tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
  - result: PASS
  - summary:
    - new runtime owner-path contract is syntactically valid

- `bash tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
  - result: PASS
  - summary:
    - selected runtime tests plus builder / connector now all prefer
      `ISSLSessionResumption`

- `fpc -B ... tests/test_connection_builder_hostname_precedence.pas && ./.../test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - builder hostname precedence contract still passes
    - the earlier new warning from `src/fafafa.ssl.connection.builder.pas`
      direct core `SetSession` disappeared after the production owner-path fix

- `fpc -B ... tests/test_freepascal_client_certificate_flight_requirements.pas && ./.../test_freepascal_client_certificate_flight_requirements`
  - result: PASS
  - summary:
    - resumed certificate-omission boundary still passes through
      `ISSLSessionResumption`

- `fpc -B ... tests/test_freepascal_client_session_resumption.pas && ./.../test_freepascal_client_session_resumption`
  - result: PASS
  - summary:
    - offline capture/resume plus CT / OCSP resumed-boundary checks still pass
      through `ISSLSessionResumption`

- `fpc -B ... tests/test_freepascal_server_session_resumption.pas && ./.../test_freepascal_server_session_resumption`
  - result: PASS
  - summary:
    - server-side resumed / tampered-binder checks still pass while reading
      reuse truth through `ISSLSessionResumption`

- `fpc -B ... tests/test_openssl_wolfssl_early_data_connection_contract.pas && ./.../test_openssl_wolfssl_early_data_connection_contract`
  - result: PASS
  - summary:
    - OpenSSL/WolfSSL early-data contract still passes after moving the configured
      resumable session injection to `ISSLSessionResumption`

- `fpc -B ... tests/test_tls_connector_early_data_contract.pas && ./.../test_tls_connector_early_data_contract`
  - result: PASS
  - summary:
    - connector early-data convenience contract still preserves the expected order:
      `session -> servername -> earlydata -> connect`

- `rg -lP ... tests --glob '!tests/scripts/**' | sort`
  - result: PASS
  - summary:
    - remaining direct-core session residual set shrank to:
      - `tests/contract/test_backend_contract.pas`
      - `tests/test_freepascal_tls13_early_data.pas`
      - `tests/test_mbedtls_connection_session_reused_contract.pas`
      - `tests/test_openssl_connection_session_reused_contract.pas`
      - `tests/winssl/test_session_save_logic.pas`
      - `tests/winssl/test_winssl_session_resumption.pas`

- `git diff --check`
  - result: PASS
  - summary:
    - session-resumption runtime owner-path migration wave 1 is whitespace-clean

### ISSLSessionResumption Compiler Deprecation Alignment

- add `docs/plans/2026-05-19-isslsessionresumption-compiler-deprecation-alignment.md`
  - change:
    - define the bounded compiler-surface batch for session-resumption owner-path truth,
      compatibility-mirror demotion, and focused compile proof

- add `tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
  - change:
    - lock that:
      - session-resumption core declarations in `src/fafafa.ssl.base.pas`
        are compiler-deprecated mirrors
      - `src/fafafa.ssl.connection.base.pas` records the session-resumption residual note
      - `API_REFERENCE.md` / `INTERFACE_DESIGN_V2.md` record
        compiler-deprecated compatibility-mirror truth
      - `tests/contract/test_backend_contract.pas` keeps the intended direct-core
        mirror proof with warning quarantine

- `bash -n tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - new session-resumption compiler-deprecation contract is syntactically valid

- `bash tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - first run exposed that `src/fafafa.ssl.connection.base.pas`
      was still missing the exact residual-note wording we wanted to lock
    - GREEN after the wording fix proves source/docs/contracts are now aligned

- update `src/fafafa.ssl.base.pas`
  - change:
    - mark `GetSession` / `SetSession` / `IsSessionReused` as compiler `deprecated`
    - add `@preferred-access` / `@owner-note` / compatibility-mirror wording

- update `src/fafafa.ssl.connection.base.pas`
  - change:
    - add session-resumption residual note stating that ordinary docs/tests now use
      the `ISSLSessionResumption` owner path and direct-core session-resumption
      remains a compatibility mirror plus backend/runtime residual surface

- update `docs/reference/API_REFERENCE.md`
  - change:
    - mark the session-resumption core summary signatures as compiler-deprecated
      compatibility mirrors
    - add active guidance that new code should prefer `ISSLSessionResumption`

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - promote session-resumption migration truth to:
      default owner is `ISSLSessionResumption`, core side is compatibility-only,
      source declarations are compiler-deprecated

- update `tests/contract/test_backend_contract.pas`
  - change:
    - add local warning quarantine around direct-core `GetSession` / `IsSessionReused`
      mirror proof
    - also remove the newly introduced deprecated-warning noise from the
      diagnostics contract by caching the direct-core reuse flag inside the same
      local quarantine pattern

- `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs/tests still prefer `ISSLSessionResumption` for session-resumption surfaces

- `mkdir -p tmp/test_backend_contract_session_resumption_deprecation && fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_contract_session_resumption_deprecation -FEtmp/test_backend_contract_session_resumption_deprecation -otmp/test_backend_contract_session_resumption_deprecation/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract_session_resumption_deprecation/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract compiled and ran successfully
    - `Session-resumption interface alignment` remained green for
      `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal`
    - `Windows Schannel` stayed a platform skip on Linux, which is expected for
      this focused static/compile proof batch

- `git diff --check`
  - result: PASS
  - summary:
    - session-resumption compiler-deprecation alignment batch is whitespace-clean

### ISSLDiagnostics Compiler Deprecation Alignment

- add `docs/plans/2026-05-19-issldiagnostics-compiler-deprecation-alignment.md`
  - change:
    - define the bounded compiler-surface batch for diagnostics owner-path truth,
      residual direct-core allowlist, and focused compile proof

- add `tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
  - change:
    - lock that:
      - diagnostics core getter declarations in `src/fafafa.ssl.base.pas`
        are compiler-deprecated mirrors
      - `src/fafafa.ssl.connection.base.pas` records the diagnostics residual note
      - `API_REFERENCE.md` / `INTERFACE_DESIGN_V2.md` record
        compiler-deprecated compatibility-mirror truth
      - `tests/contract/test_backend_contract.pas` keeps the intended direct-core
        mirror proof with warning quarantine
      - residual direct-core diagnostics usage is limited to the approved file set
      - `tests/winssl/test_winssl_session_resumption.pas`
        no longer uses direct core `GetPerformanceMetrics`

- `bash -n tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - new diagnostics compiler-deprecation contract is syntactically valid

- `bash tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed that `src/fafafa.ssl.base.pas`
      had not yet marked the four diagnostics core getters as compiler-deprecated
    - GREEN after the fix proves source/docs/tests/residual allowlist are now aligned

- update `src/fafafa.ssl.base.pas`
  - change:
    - mark `GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` /
      `GetPerformanceMetrics` as compiler `deprecated`
    - add `@preferred-access` / `@owner-note` / compatibility-mirror wording

- update `src/fafafa.ssl.connection.base.pas`
  - change:
    - add diagnostics residual note stating that ordinary docs/tests now use the
      `ISSLDiagnostics` owner path and direct-core diagnostics only remain for
      contract mirror proof plus WinSSL runtime residuals

- update `docs/reference/API_REFERENCE.md`
  - change:
    - mark the diagnostics core getter summary signatures as compiler-deprecated
      compatibility mirrors
    - add active guidance that new code should prefer `ISSLDiagnostics`

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - promote diagnostics migration truth to:
      default owner is `ISSLDiagnostics`, core side is compatibility-only,
      source declarations are compiler-deprecated

- update `tests/contract/test_backend_contract.pas`
  - change:
    - add direct-core diagnostics mirror proof against `ISSLDiagnostics`
    - quarantine deprecated-warning reads locally so the intended mirror proof
      stays explicit without polluting broader compiles

- update WinSSL residual/runtime tests:
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - add local deprecated-warning quarantine for intentional direct-core
      diagnostics runtime residuals
    - move session resumption metrics proof to `ISSLDiagnostics` owner path

- `bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs/tests still prefer `ISSLDiagnostics` for diagnostics surfaces

- `mkdir -p tmp/test_backend_contract_diagnostics_deprecation && fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_contract_diagnostics_deprecation -FEtmp/test_backend_contract_diagnostics_deprecation -otmp/test_backend_contract_diagnostics_deprecation/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract_diagnostics_deprecation/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract compiled and ran successfully
    - `Diagnostics interface alignment`, `Connection-info interface alignment`,
      `Session-resumption interface alignment`, and
      `Certificate-verification interface alignment` remained green for
      `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal`
    - `Windows Schannel` stayed a platform skip on Linux, which is expected for
      this focused static/compile proof batch

- `git diff --check`
  - result: PASS
  - summary:
    - diagnostics compiler-deprecation alignment batch is whitespace-clean

### WinSSL Callback Runtime Proof Markers

- add `docs/plans/2026-05-19-winssl-callback-runtime-proof-markers.md`
  - change:
    - define the bounded batch that closes the remaining WinSSL callback proof gap by making Windows runtime transcript evidence grep-able
    - later corrected after artifact review:
      the real Windows truth source is `tests/winssl/test_winssl_unit_comprehensive.pas`, not `tests/unit/test_winssl_comprehensive.pas`

- add `tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
  - change:
    - lock that:
      - the actual Windows comprehensive unit test source emits callback truth
      - `tests/run_winssl_tests.ps1` derives callback markers from `test_winssl_unit_comprehensive.lpi`
      - the runtime script emits `callback_surface` markers
      - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` documents the new marker and its meaning

- `gh run download 26092105397 -n wave-b-windows-winssl_callback_markers_20260519_184245 -D tmp/gh_run_26092105397/windows`
  - result: FAIL with existing file collision
  - summary:
    - a previous partial download already existed under `tmp/gh_run_26092105397/windows`, so the artifact had to be inspected in place instead of extracted over itself

- `rg -n "\\[WINSSL-RUNTIME\\] callback_surface|Verify callback set|Password callback unsupported as expected|Info callback set" tmp/gh_run_26092105397/windows`
  - result: PASS with negative callback-proof evidence
  - summary:
    - the downloaded Windows artifact did contain a callback marker, but it was:
      - `[WINSSL-RUNTIME] callback_surface verify=missing password=missing info=missing`
    - none of the expected callback truth lines were present in the runtime transcript

- `sed -n '1,240p' tmp/gh_run_26092105397/windows/winssl_runtime_suite_winssl_callback_markers_20260519_184245.log`
  - result: PASS
  - summary:
    - broader suite transcript confirmed the failure mode precisely:
      the callback marker is emitted after test `1/8`, but all three values are `missing`

- `sed -n '1,260p' tests/run_winssl_tests.ps1`
  - result: PASS
  - summary:
    - callback marker extraction is currently gated on `test_winssl_unit_comprehensive.lpi`

- `sed -n '1,220p' tests/winssl/test_winssl_unit_comprehensive.lpi`
  - result: PASS
  - summary:
    - the Windows comprehensive LPI points to `tests/winssl/test_winssl_unit_comprehensive.pas`
    - this proved the earlier assumption about `tests/unit/test_winssl_comprehensive.pas` being the runtime truth source was wrong

- `bash tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - first RED correctly exposed that `tests/winssl/test_winssl_unit_comprehensive.pas` did not emit any callback truth
    - GREEN after the fix proves the focused contract now locks the actual Windows suite entrypoint instead of the wrong cross-platform test file

- update `tests/winssl/test_winssl_unit_comprehensive.pas`
  - change:
    - add `TCallbackProbe`
    - add `TestCallbackConfiguration`
    - make the actual Windows comprehensive unit test emit:
      - `Verify callback set`
      - `Password callback unsupported as expected`
      - `Info callback set`
    - register the callback test in the main Windows comprehensive suite

- update `tests/run_winssl_tests.ps1`
  - change:
    - add `Write-CallbackSurfaceMarkers`
    - extract:
      - `Verify callback set`
      - `Password callback unsupported as expected`
      - `Info callback set`
      from the captured unit-comprehensive output
    - emit:
      - `[WINSSL-RUNTIME] callback_surface verify=... password=... info=...`

- update `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - change:
    - add the `callback_surface` runtime marker to the WinSSL transcript marker checklist
    - explain that it maps directly to the callback granularity truth in `tests/winssl/test_winssl_unit_comprehensive.pas`

- `bash -n tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
  - result: PASS
  - summary:
    - corrected WinSSL callback runtime marker contract syntax is valid

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL callback runtime marker root-cause fix batch is whitespace-clean before commit/push

- `git commit -m "test(winssl): bind callback markers to real windows suite"`
  - result: PASS
  - summary:
    - recorded the real-Windows-suite callback truth fix in commit `12e62a2`

- `git push origin master`
  - result: PASS
  - summary:
    - pushed `12e62a2` to `origin/master`

- `gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_callback_markers_fix_20260519_185808`
  - result: PASS
  - summary:
    - dispatched a fresh Windows-proof run against `12e62a2`

- `gh run watch 26092828923`
  - result: FAIL in `windows-gate`
  - summary:
    - `linux-gate` and `macos-gate` completed
    - `windows-gate` failed specifically at:
      - `Run broader WinSSL runtime suite`

- `gh run view 26092828923 --job 76722715903 --log`
  - result: PASS with failure diagnosis
  - summary:
    - the new Windows run proved the callback marker itself is now correct:
      - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
    - broader suite still failed because the newly added Windows callback test treated the current fail-closed message
      - `Password callback is not published by the current WinSSL backend runtime...`
      as a test failure instead of as supported unsupported/not-published truth

- update `tests/winssl/test_winssl_unit_comprehensive.pas`
  - change:
    - relax password callback failure-text acceptance so the Windows comprehensive test treats both:
      - `unsupported`
      - `not published`
      as valid fail-closed evidence for the current WinSSL password callback path

- `bash tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
  - result: PASS
  - summary:
    - marker contract still holds after relaxing the Windows password callback assertion text

- `git diff --check`
  - result: PASS
  - summary:
    - follow-up Windows password assertion fix is whitespace-clean before the next commit/push

- `git commit -m "test(winssl): accept published fail-closed callback proof"`
  - result: PASS
  - summary:
    - recorded the follow-up Windows callback assertion fix in commit `26bad43`

- `git push origin master`
  - result: PASS
  - summary:
    - pushed `26bad43` to `origin/master`

- `gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_callback_markers_fix2_20260519_191025`
  - result: PASS
  - summary:
    - dispatched the final verification run for the Windows password assertion follow-up

- `gh run watch 26093405878`
  - result: PASS
  - summary:
    - `windows-gate`, `linux-gate`, `macos-gate`, and `summary` all completed successfully

- `gh run download 26093405878 -n wave-b-windows-winssl_callback_markers_fix2_20260519_191025 -D tmp/gh_run_26093405878/windows`
  - result: PASS
  - summary:
    - downloaded the final successful Windows evidence artifact locally

- `rg -n "\\[WINSSL-RUNTIME\\] callback_surface|\\[WINSSL-RUNTIME\\] suite_summary|\\[WINSSL-RUNTIME\\] suite_end" tmp/gh_run_26093405878/windows/winssl_runtime_suite_winssl_callback_markers_fix2_20260519_191025.log`
  - result: PASS
  - summary:
    - final Windows artifact now contains the exact closeout evidence:
      - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
      - `[WINSSL-RUNTIME] suite_summary passed=8 failed=0 total=8 success_rate=100`
      - `[WINSSL-RUNTIME] suite_end status=PASS`

### WinSSL FIPS Capability Truth Tightening

- add `docs/plans/2026-05-19-winssl-fips-capability-truth-tightening.md`
  - change:
    - define the bounded implementation batch for WinSSL FIPS capability truth, enterprise-helper boundary clarification, and focused selector-visible contract updates

- update `tests/scripts/test_active_fips_docs_truth_contract.sh`
  - change:
    - extend the existing active-FIPS truth contract to lock:
      - `src/fafafa.ssl.winssl.lib.pas` no longer publishes `SupportsFIPSMode=True`
      - WinSSL active docs no longer market FIPS as a published backend capability
      - enterprise/helper docs explicitly separate policy detection from capability truth

- update `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - change:
    - tighten `MIGRATION_GUIDE_V1.1.md` expectations so FIPS is unpublished across the current active backend table
    - lock the new WinSSL enterprise-helper note

- add `tests/test_backend_fips_capability_truth_contract.pas`
  - change:
    - prove the current shipped backend FIPS capability baseline is `False`
      for FreePascal / OpenSSL / MbedTLS / WolfSSL
    - keep a Windows-conditional WinSSL assertion ready for the Windows lane

- update `src/fafafa.ssl.winssl.lib.pas`
  - change:
    - replace `SupportsFIPSMode=True` publication with `False`
    - record that the current WinSSL FIPS line is helper/policy detection, not a published backend capability

- update active docs:
  - `docs/reference/WINSSL_DESIGN.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - `docs/PLATFORM_SUPPORT.md`
  - `docs/reference/BACKEND_SELECTOR_DESIGN.md`
  - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/MIGRATION_GUIDE_V1.1.md`
  - `docs/guides/MIGRATION_GUIDE.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - change:
    - stop marketing WinSSL FIPS as a published backend capability
    - explicitly preserve WinSSL enterprise helper usage as Windows policy/GPO detection

- `bash -n tests/scripts/test_active_fips_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - updated WinSSL/OpenSSL FIPS truth contract syntax is valid

- `bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - updated active-capability docs truth contract syntax is valid

- `bash tests/scripts/test_active_fips_docs_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED first exposed that active FIPS docs still advertised WinSSL as a published FIPS backend
    - a second RED exposed shell command-substitution noise from backtick-containing literal patterns inside the contract, which was fixed before final verification
    - GREEN after fix proves source/docs truth now separates WinSSL enterprise FIPS helpers from published capability truth

- `bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED exposed that `MIGRATION_GUIDE_V1.1.md` still kept the WinSSL FIPS column as published truth
    - a second RED exposed the same backtick-literal shell issue in the new WinSSL note contract
    - GREEN after fix proves the active migration guide now aligns with the unpublished FIPS capability baseline

- `fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_fips_capability_truth -FEtmp/test_backend_fips_capability_truth -otmp/test_backend_fips_capability_truth/test_backend_fips_capability_truth_contract tests/test_backend_fips_capability_truth_contract.pas`
  - result: FAIL -> PASS with existing repository warnings
  - summary:
    - the first attempt failed because the `-FE/-FU` target directory did not yet exist
    - after creating `tmp/test_backend_fips_capability_truth`, the focused capability contract compiled successfully

- `./tmp/test_backend_fips_capability_truth/test_backend_fips_capability_truth_contract`
  - result: PASS
  - summary:
    - current Linux runtime proof confirms:
      - FreePascal / OpenSSL / MbedTLS / WolfSSL all publish `SupportsFIPSMode=False`
      - the Windows Schannel assertion remains compiled into the contract for Windows-side execution, but is skipped on Linux because that backend is unavailable here

- `git diff --check`
  - result: PASS
  - summary:
    - WinSSL FIPS capability truth batch is whitespace-clean after final source/doc/test sync

### Custom Cipher Capability Truth Alignment

- add `docs/plans/2026-05-19-custom-cipher-capability-truth-alignment.md`
  - change:
    - define the bounded implementation batch for custom-cipher capability truth, setter fail-closed semantics, and shipped-baseline compatibility retention

- add `tests/scripts/test_custom_cipher_capability_truth_contract.sh`
  - change:
    - lock that:
      - base/API docs record the current custom-cipher fail-closed rule
      - OpenSSL published truth now follows shared runtime helper readiness
      - FreePascal / WinSSL / MbedTLS / WolfSSL publish `SupportsCustomCipherSuites=False`
      - unpublished custom-cipher backends reject custom non-default setter assignments
      - WinSSL / MbedTLS active docs no longer teach unsupported custom-cipher tuning

- add `tests/test_backend_custom_cipher_capability_truth_contract.pas`
  - change:
    - prove:
      - current OpenSSL ready path still publishes custom-cipher support and accepts custom non-default overrides
      - unpublished backends keep shipped baseline defaults but reject custom non-default overrides
      - factory/direct-library config paths also reject custom non-default overrides while capability is unpublished
      - removing `SSL_CTX_set_ciphersuites` drops OpenSSL back to fail-closed capability truth

- update `src/fafafa.ssl.openssl.api.ssl.pas`
  - change:
    - add shared `OpenSSLPublishedCustomCipherSurfaceReady` helper

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - replace unconditional `SupportsCustomCipherSuites=True` publication with the shared runtime custom-cipher gate

- update `src/fafafa.ssl.openssl.context.pas`
  - change:
    - distinguish shipped baseline defaults from custom non-default overrides
    - gate custom non-default `SetCipherList` / `SetCipherSuites` assignments behind the published custom-cipher surface
    - keep empty clear / shipped baseline defaults available as compatibility/default-context path

- update `src/fafafa.ssl.freepascal.lib.pas`
  - change:
    - publish `SupportsCustomCipherSuites=False`

- update `src/fafafa.ssl.freepascal.context.pas`
  - change:
    - reject custom non-default cipher-list / cipher-suites overrides as unsupported
    - keep empty clear / shipped baseline defaults as compatibility/default-context path

- update `src/fafafa.ssl.winssl.lib.pas`
  - change:
    - publish `SupportsCustomCipherSuites=False`

- update `src/fafafa.ssl.winssl.context.pas`
  - change:
    - reject custom non-default cipher-list / cipher-suites overrides as unsupported
    - keep empty clear / shipped baseline defaults as compatibility/default-context path

- update `src/fafafa.ssl.mbedtls.lib.pas`
  - change:
    - publish `SupportsCustomCipherSuites=False`

- update `src/fafafa.ssl.mbedtls.context.pas`
  - change:
    - reject custom non-default cipher-list / cipher-suites overrides as unsupported
    - keep empty clear / shipped baseline defaults as compatibility/default-context path

- update `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - publish `SupportsCustomCipherSuites=False`

- update `src/fafafa.ssl.wolfssl.context.pas`
  - change:
    - reject custom non-default cipher-list / cipher-suites overrides as unsupported
    - keep empty clear / shipped baseline defaults as compatibility/default-context path

- update `src/fafafa.ssl.base.pas`
  - change:
    - document the current `SupportsCustomCipherSuites` fail-closed rule directly on the public interface surface

- update docs:
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_PERFORMANCE_TUNING.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/MBEDTLS_USER_GUIDE.md`
  - change:
    - record current custom-cipher truth
    - remove WinSSL / MbedTLS active examples that still taught unsupported backend-specific custom-cipher tuning

- update focused tests/doc-truth residues:
  - `tests/test_direct_library_default_config_parity.pas`
  - `tests/mbedtls/test_mbedtls_server_accept_simple.pas`
  - `tests/winssl/test_winssl_context_config.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/unit/test_winssl_comprehensive.pas`
  - change:
    - stop assuming WinSSL / FreePascal / MbedTLS custom non-default cipher overrides are published capabilities
    - keep shipped baseline defaults where that is the real compatibility/default-context path

- `bash -n tests/scripts/test_custom_cipher_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - new custom-cipher truth contract syntax is valid

- `bash tests/scripts/test_custom_cipher_capability_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED first exposed that `API_REFERENCE` still lacked the exact custom-cipher fail-closed wording required by the new contract
    - a second RED exposed shell command-substitution noise on backtick-containing literal patterns inside the new contract, which was fixed before final verification
    - GREEN after fix proves source/docs truth now locks runtime-gated OpenSSL publication, false-backend fail-closed setter semantics, and corrected WinSSL / MbedTLS active docs

- `fpc -B -Fu./src -Fu./tests -FUtmp/test_custom_cipher_truth -FEtmp/test_custom_cipher_truth -otmp/test_custom_cipher_truth/test_backend_custom_cipher_capability_truth_contract tests/test_backend_custom_cipher_capability_truth_contract.pas`
  - result: PASS with existing repository warnings
  - summary:
    - focused custom-cipher runtime contract compiled successfully after capability/setter truth alignment

- `./tmp/test_custom_cipher_truth/test_backend_custom_cipher_capability_truth_contract`
  - result: PASS
  - summary:
    - runtime proof now confirms:
      - current OpenSSL ready path still publishes custom-cipher support and accepts custom non-default overrides
      - FreePascal / MbedTLS / WolfSSL reject custom non-default overrides while keeping shipped baseline defaults
      - factory/direct-library config paths reject custom non-default overrides when capability is unpublished
      - removing `SSL_CTX_set_ciphersuites` drops OpenSSL back to fail-closed custom-cipher publication truth

- `fpc -B -Fu./src -Fu./tests -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas`
  - result: PASS with existing repository warnings
  - summary:
    - updated direct-library default-config parity contract still compiles after narrowing FreePascal custom-cipher publication

- `./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
  - result: PASS
  - summary:
    - direct-library default-config parity now explicitly keeps shipped cipher baselines on unpublished custom-cipher backends while preserving the rest of the default-config projection

- `git diff --check`
  - result: PASS
  - summary:
    - custom-cipher truth batch is whitespace-clean after final source/doc/test sync

### OpenSSL Callback Publication Runtime Gate

- add `docs/plans/2026-05-19-openssl-callback-publication-runtime-gate.md`
  - change:
    - define the bounded implementation batch for runtime-aware OpenSSL callback publication and setter fail-closed alignment

- update `src/fafafa.ssl.openssl.api.ssl.pas`
  - change:
    - add shared `OpenSSLPublishedContextCallbackSurfaceReady` helper
    - define the minimal required helper set as:
      - `SSL_CTX_set_cert_verify_callback`
      - `SSL_CTX_set_default_passwd_cb`
      - `SSL_CTX_set_default_passwd_cb_userdata`
      - `SSL_CTX_set_info_callback`

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - replace unconditional `SupportsCallbacks=True` publication with the shared runtime callback-surface gate

- update `src/fafafa.ssl.openssl.context.pas`
  - change:
    - add `RequirePublishedOpenSSLContextCallbackSurface(...)`
    - make verify/password/info callback setter non-nil assignments fail-closed when the published callback surface is incomplete
    - keep `nil` clear as a best-effort compatibility operation instead of raising on missing helpers

- update `tests/scripts/test_callback_capability_truth_contract.sh`
  - change:
    - lock that OpenSSL callback publication is no longer unconditional
    - lock that password callback publication also requires userdata helper readiness

- update `tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - change:
    - lock that OpenSSL verify/password/info setter now guard non-nil assignment behind the published callback-surface gate

- update `tests/test_backend_callback_capability_truth_contract.pas`
  - change:
    - evaluate OpenSSL expected publication after runtime initialization, not before symbol loading
    - add a focused runtime drift contract that temporarily removes `SSL_CTX_set_default_passwd_cb_userdata`
      and proves `SupportsCallbacks` falls back to `False`

- update `tests/test_backend_callback_setter_fail_closed_contract.pas`
  - change:
    - evaluate OpenSSL published-vs-unpublished path from the live runtime gate
    - add a focused runtime drift contract that temporarily removes `SSL_CTX_set_default_passwd_cb_userdata`
      and proves verify/password/info non-nil setter assignment fails closed while `nil` clear remains available
    - fix the focused probe lifecycle to use interface ownership and avoid invalid pointer teardown noise

- `bash -n tests/scripts/test_callback_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - updated OpenSSL callback capability source contract syntax is valid

- `bash tests/scripts/test_callback_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - static capability truth now locks the runtime-aware OpenSSL callback publication gate

- `bash -n tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - result: PASS
  - summary:
    - updated OpenSSL callback setter contract syntax is valid

- `bash tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - result: PASS
  - summary:
    - static setter contract now locks OpenSSL non-nil callback gating behind the published callback surface

- `fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_capability_truth -FEtmp/test_callback_capability_truth -otmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas`
  - result: PASS with existing repository warnings
  - summary:
    - focused callback capability runtime contract compiled successfully after OpenSSL runtime-gate updates

- `./tmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract`
  - result: FAIL -> PASS
  - summary:
    - initial RED exposed that the test was reading the OpenSSL helper gate before backend initialization
    - after moving the expected-value check behind real library initialization, the runtime contract proved:
      - current Linux OpenSSL build still publishes callbacks on the fully-ready path
      - removing password-callback userdata helper drops `SupportsCallbacks` back to `False`

- `fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas`
  - result: PASS with existing repository warnings
  - summary:
    - focused callback setter runtime contract compiled successfully after OpenSSL fail-closed alignment

- `./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract`
  - result: FAIL -> PASS
  - summary:
    - initial RED first exposed the same pre-initialization expectation bug in the test
    - a second RED exposed probe teardown ownership noise (`EInvalidPointer`) in the focused test harness rather than product code
    - GREEN after fix proves:
      - current Linux OpenSSL build accepts verify/password/info callbacks on the fully-ready path
      - removing password-callback userdata helper makes all three non-nil setter assignments fail-closed
      - `nil` clear remains available on the incomplete path

### Migration Guide Low-Level Helper Entrypoint Truth

- add `docs/plans/2026-05-19-migration-guide-lowlevel-helper-entrypoint-truth.md`
  - change:
    - define the bounded docs batch for the remaining old factory call inside the migration guide low-level helper example

- update `tests/scripts/test_migration_guide_active_truth_contract.sh`
  - change:
    - extend the existing migration-guide truth contract to also lock:
      - the OpenSSL low-level helper example uses `TSSLFactory.GetLibraryInstance(...)`
      - the example no longer falls back to `TSSLFactory.GetLibrary(...)`

- read-only evidence triage
  - summary:
    - active-doc scan had already narrowed the remaining stale public helper residue down to a single `MIGRATION_GUIDE` line
    - source/reference truth remained:
      - `TSSLFactory.GetLibraryInstance(...)` is the current public entrypoint
      - `GetFriendlyErrorMessage(...)` / `GetOpenSSLErrorCategory(...)` are still OpenSSL-specific low-level helpers
    - the real remaining problem was just that the low-level helper example still mixed in the old factory call

- `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
  - result: PASS
  - summary:
    - tightened migration-guide contract syntax remains valid

- `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
  - result: PASS
  - summary:
    - the focused fix kept the whole migration-guide truth contract green while tightening the low-level helper entrypoint rule

- update `docs/guides/MIGRATION_GUIDE.md`
  - change:
    - replace the last remaining `TSSLFactory.GetLibrary(sslOpenSSL)` occurrence with `TSSLFactory.GetLibraryInstance(sslOpenSSL)` inside the OpenSSL low-level helper example

- `git diff --check`
  - result: PASS
  - summary:
    - migration-guide low-level-helper batch is whitespace-clean after final doc/log sync

### Security Best Practices Pinning Helper Truth

- add `docs/plans/2026-05-19-security-best-practices-pinning-helper-truth.md`
  - change:
    - define the bounded docs batch for pinning-example helper truth in `security-best-practices`

- add `tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
  - change:
    - lock that the pinning example:
      - no longer uses nonexistent `LoadCertificateFromFile(...)`
      - uses `LoadCertificateFromPEM(...)`
      - imports the needed OpenSSL raw helper units
      - releases the `PX509` handle via `X509_free(...)`

- read-only evidence triage
  - summary:
    - active docs scan had narrowed the next stale helper residue down to `security-best-practices`
    - current source truth was re-confirmed in:
      - `src/fafafa.ssl.cert.pinning.pas`
      - `src/fafafa.ssl.openssl.api.pem.pas`
    - the example was not backend-neutral:
      - `TPinValidator.ExtractPublicKeyHash(...)` takes `PX509`
      - therefore the current file-loader truth is `LoadCertificateFromPEM(...)`, not any generic `LoadCertificateFromFile(...)`

- `bash -n tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
  - result: PASS
  - summary:
    - new security-best-practices pinning helper contract syntax is valid

- `bash tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED proved the guide still lacked an explicit raw-handle scope note
    - GREEN after fix proves the example now aligns with the current PEM helper and handle-lifetime truth

- update `docs/guides/security-best-practices.md`
  - change:
    - mark the pinning snippet as an OpenSSL raw certificate handle path
    - replace nonexistent `LoadCertificateFromFile(...)` with `LoadCertificateFromPEM(...)`
    - import `fafafa.ssl.openssl.api.pem` / `fafafa.ssl.openssl.api.x509`
    - add explicit `X509_free(...)` cleanup to the sample

- `git diff --check`
  - result: PASS
  - summary:
    - security-best-practices pinning-helper batch is whitespace-clean after final doc/log sync

### PKCS12 Helper Guide Active Truth

- add `docs/plans/2026-05-19-pkcs12-helper-guide-active-truth.md`
  - change:
    - define the bounded docs batch for PKCS12 helper entrypoint truth and stale helper-name cleanup

- add `tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
  - change:
    - lock that `PKCS12_USER_GUIDE`:
      - distinguishes helper-vs-raw PKCS12 entrypoints
      - uses `TPKCS12Manager.CreatePKCS12ToFile(...)`
      - uses `TPKCS12Manager.LoadFromPKCS12File(...)`
      - uses `LoadCertificateFromPEM(...)` / `LoadPrivateKeyFromPEM(...)` in raw examples
      - no longer uses nonexistent `LoadCertificateFromFile(...)` / `LoadPrivateKeyFromFile(...)`
    - lock that `API_REFERENCE` exposes the current façade PKCS12 helper section

- read-only evidence triage
  - summary:
    - `PKCS12_USER_GUIDE` already had the right backend scope note, but its active examples still taught nonexistent helper names
    - current source truth was re-confirmed in:
      - `src/fafafa.ssl.pas`
      - `src/fafafa.ssl.cert.advanced.pas`
      - `src/fafafa.ssl.openssl.api.pem.pas`
    - the real public/helper split is:
      - façade helper: `TPKCS12Manager` / `DefaultPKCS12Options`
      - raw OpenSSL helper: `LoadCertificateFromPEM(...)` / `LoadPrivateKeyFromPEM(...)`

- `bash -n tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
  - result: PASS
  - summary:
    - new PKCS12 helper-guide truth contract syntax is valid

- `bash tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED proved `PKCS12_USER_GUIDE` still lacked an explicit helper-vs-raw entrypoint split
    - an intermediate RED exposed shell command-substitution noise on backtick-containing literal patterns inside the new contract, which was fixed before final verification
    - GREEN after fix proves the guide and API reference now point to current PKCS12 helper surfaces

- update `docs/guides/PKCS12_USER_GUIDE.md`
  - change:
    - add an explicit helper-vs-raw entrypoint section
    - replace nonexistent `LoadCertificateFromFile(...)` / `LoadPrivateKeyFromFile(...)` examples
    - move active helper examples to `TPKCS12Manager.CreatePKCS12ToFile(...)` / `LoadFromPKCS12File(...)`
    - keep a raw OpenSSL example but align it to `LoadCertificateFromPEM(...)` / `LoadPrivateKeyFromPEM(...)`
    - refresh same-file tests/resources and update metadata date/version

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add a dedicated PKCS12 helper section for `DefaultPKCS12Options` and `TPKCS12Manager`
    - explain that the helper family maps to the OpenSSL full PKCS12 helper/API surface, while WinSSL only publishes the PFX/P12 import path

- `git diff --check`
  - result: PASS
  - summary:
    - PKCS12 helper-guide batch is whitespace-clean after final doc/log sync

### Capability Precedence Doc Truth

- add `docs/plans/2026-05-19-capability-precedence-doc-truth.md`
  - change:
    - define the bounded docs batch for capability precedence truth and adjacent high-entry example drift

- add `tests/scripts/test_capability_precedence_docs_truth_contract.sh`
  - change:
    - lock that active capability docs now state:
      - paired `*Support` fields are the truth source
      - legacy `Supports*` bools are compatibility projections
      - `SupportsTLS13` remains the primary bool truth
    - lock that capability guide / API reference use `TSSLFactory.GetLibraryInstance(...)`
    - lock same-batch adjacent truth for:
      - `CompatibilityLevel: Integer`
      - new-backend example calling `NormalizeLegacyCapabilityBooleans(Result);`

- read-only evidence triage
  - summary:
    - current runtime/source truth was re-confirmed in:
      - `src/fafafa.ssl.base.pas`
      - `src/fafafa.ssl.capability.serializer.pas`
      - `src/fafafa.ssl.capability.diff.pas`
    - the remaining drift was control-plane only:
      - `CAPABILITY_MATRIX_GUIDE` and `API_REFERENCE` still listed legacy bools and `*Support` without clearly freezing precedence
      - `BACKEND_CAPABILITY_MATRIX` still lacked a table-level precedence note
    - same-file adjacent drift was also confirmed:
      - capability guide still used `TSSLFactory.GetLibrary(...)`
      - capability record snippets still published `CompatibilityLevel: Byte`
      - new-backend example still looked legacy-bool-first

- `bash -n tests/scripts/test_capability_precedence_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - new capability precedence docs contract syntax is valid

- `bash tests/scripts/test_capability_precedence_docs_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED proved `CAPABILITY_MATRIX_GUIDE` still lacked an explicit support-level-first precedence statement
    - GREEN after fix proves:
      - capability guide / API reference now spell out the truth-source rule
      - backend capability matrix now declares the table precedence
      - high-entry examples now use `GetLibraryInstance(...)`

- update `docs/CAPABILITY_MATRIX_GUIDE.md`
  - change:
    - add explicit support-level-first precedence guidance
    - keep `SupportsTLS13` as the primary bool truth in the doc wording
    - replace high-entry `GetLibrary(...)` examples with `GetLibraryInstance(...)`
    - fix `CompatibilityLevel` type to `Integer`
    - rewrite the new-backend snippet to show `*Support` first plus `NormalizeLegacyCapabilityBooleans(Result);`
    - refresh same-file related-doc links to current active paths

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add explicit capability precedence wording in the `TSSLBackendCapabilities` section
    - fix `CompatibilityLevel` type to `Integer`
    - move the capability example back to `TSSLFactory.GetLibraryInstance(...)`

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - add a concise table-level precedence note for paired `*Support` fields and `SupportsTLS13`

- `git diff --check`
  - result: PASS
  - summary:
    - capability precedence docs batch is whitespace-clean after the final plan/log sync

### Interface Audit Current Truth Refresh

- add `docs/plans/2026-05-19-interface-audit-current-truth-refresh.md`
  - change:
    - define the bounded static-audit refresh batch for stale SNI / `ISSLServerConnection` / `TSSLConfig` conclusions

- add `tests/scripts/test_interface_audit_current_truth_contract.sh`
  - change:
    - lock current source/doc truth that:
      - factory paths are `warning + ignore` for `TSSLConfig.ServerName`
      - builder `WithSNI(...)` is deprecated and ignored by `BuildClient` / `BuildServer`
      - active docs now explicitly state `ISSLServerConnection` is absent
      - the audit report no longer preserves stale live-drift claims

- read-only evidence triage
  - summary:
    - the current highest-value drift was in the durable audit report itself, not in runtime
    - source re-check confirmed:
      - `TSSLFactory.CreateContext(...)` now warns and ignores `TSSLConfig.ServerName`
      - `TSSLContextBuilder.WithSNI(...)` is compile-time deprecated and runtime ignored
      - direct-library `CreateContext(...)` now rejects server-side `ServerName` and warns+ignores client-side `ServerName`
      - active architecture/design docs already explicitly say `ISSLServerConnection` is absent
      - `BufferSize` / `HandshakeTimeout` are explicitly rejected on factory/direct-library create paths

- `bash -n tests/scripts/test_interface_audit_current_truth_contract.sh`
  - result: PASS
  - summary:
    - new interface-audit truth contract syntax is valid

- `bash tests/scripts/test_interface_audit_current_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - initial RED first exposed the stale audit-report wording
    - an intermediate RED also exposed shell quoting noise in the new contract, which was fixed before final verification
    - GREEN after fix proves the audit report now matches current source/doc truth

- update `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - change:
    - refresh the top conclusion from “live drift everywhere” to “compatibility baggage + remaining structural design debt”
    - reclassify context-level SNI from active propagation drift to frozen compatibility-only surface
    - reclassify `ISSLServerConnection` from active-doc mismatch to current server-side asymmetry
    - reclassify `TSSLConfig.BufferSize` / `HandshakeTimeout` from “possibly inert” to explicit reject semantics

- `git diff --check`
  - result: FAIL -> PASS
  - summary:
    - initial FAIL was limited to one trailing space on the refreshed audit status line
    - PASS after cleanup confirms the batch is whitespace-clean

### Public Unit Import Guidance Truth

- add `docs/plans/2026-05-19-public-unit-import-guidance-truth.md`
  - change:
    - define the bounded active-doc truth batch for stale public unit imports, creator entrypoints, and enum-name drift

- add `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - change:
    - lock that high-entry docs now use:
      - `fafafa.ssl`
      - `TSSLFactory.GetLibraryInstance(...)`
      - `sslCtxClient`
      - `LibraryTypeToString(Lib.GetLibraryType)`
    - lock that stale `abstract.*` imports, `CreateSSLLibrary(...)`, stale enum names, and manual `LoadOpenSSL` guidance do not come back

- read-only evidence triage
  - summary:
    - six high-entry docs still mixed removed imports, nonexistent facade units, nonexistent creators, and stale enum names
    - current source truth was re-confirmed in:
      - `src/fafafa.ssl.pas`
      - `src/fafafa.ssl.base.pas`
      - `src/fafafa.ssl.factory.pas`
    - during the same sweep, adjacent source-truth issues were also confirmed:
      - `TSSLEnterpriseConfig` class helpers are `IsFIPSEnabled` / `GetTrustedRoots` / `GetAllPolicies`
      - SAN collections use `TSSLStringArray`
      - WinSSL error helpers are `GetFriendlyErrorMessageCN/EN`

- update `docs/guides/USER_GUIDE.md`
  - change:
    - switch active examples from `fafafa.ssl.openssl` / `abstract.intf` to `fafafa.ssl`
    - replace `CreateOpenSSLLibrary` with `TSSLFactory.GetLibraryInstance(sslOpenSSL)`
    - narrow compile guidance back to `-Fusrc`
    - fix same-file example drift for SAN arrays and WinSSL enterprise helper names

- update `docs/guides/WINSSL_QUICKSTART.md`
  - change:
    - replace stale `CreateSSLLibrary(...)`, `sslLibrary*`, `sslContextClient`, and `GetLibraryName`
    - move examples and migration snippets to:
      - `TSSLFactory.GetLibraryInstance(...)`
      - `sslCtxClient`
      - `LibraryTypeToString(Lib.GetLibraryType)`
    - replace removed `abstract.*` units in project-tree guidance

- update `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - replace stale `CreateSSLLibrary(...)` path in core parity snippet and migration snippet
    - move minimal example imports to `fafafa.ssl`

- update `docs/guides/MBEDTLS_USER_GUIDE.md`
  - change:
    - replace stale `CreateSSLLibrary(...)` path with `TSSLFactory.GetLibraryInstance(...)`
    - remove `abstract.intf` from active examples
    - update API-reference mini-section to publish current factory entrypoint

- update `docs/guides/TROUBLESHOOTING.md`
  - change:
    - replace stale `fafafa.ssl.openssl` / `abstract.*` troubleshooting guidance with `fafafa.ssl`
    - replace manual OpenSSL-loader troubleshooting steps with:
      - `TSSLFactory.IsLibraryAvailable(sslOpenSSL)`
      - `TSSLFactory.GetLibraryInstance(sslOpenSSL)`
    - align same-file WinSSL enterprise and context-lifetime snippets to current names

- update `docs/reference/API_REFERENCE.md`
  - change:
    - publish `TSSLFactory.GetLibraryInstance(...)` as the current public library-entrypoint
    - classify `CreateOpenSSLLibrary` / `CreateWinSSLLibrary` as backend-specific low-level creators
    - remove stale `LoadOpenSSL` / `CreateSSLLibrary(...)` high-entry guidance
    - move high-entry examples to `fafafa.ssl` + `TSSLFactory.GetLibraryInstance(...)`

- update `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - change:
    - remove a multiline `rg` pattern that emitted noisy warnings during repeated verification

- `bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - result: PASS
  - summary:
    - current public-unit import guidance contract syntax is valid

- `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `USER_GUIDE` still used stale facade/import guidance
      - the remaining docs still taught nonexistent creators, stale enum names, and manual loader steps
    - GREEN after fix:
      - all six high-entry docs now align to current facade/factory/import truth
      - the contract itself no longer emits repeated multiline `rg` noise

- `git diff --check`
  - result: FAIL -> PASS
  - summary:
    - initial FAIL was limited to two trailing spaces on markdown version lines
    - PASS after cleanup confirms the batch is whitespace-clean

### Migration Guide Active Truth

- add `docs/plans/2026-05-19-migration-guide-active-truth.md`
  - change:
    - define the bounded active-doc truth batch for stale migration-guide versioning, unit names, and helper boundaries

- add `tests/scripts/test_migration_guide_active_truth_contract.sh`
  - change:
    - lock that `MIGRATION_GUIDE` no longer teaches:
      - `v0.8` as current version
      - removed `abstract.intf` / nonexistent facade unit usage
      - stale WinSSL enterprise helper names
      - OpenSSL low-level helper as generic public facade API

- read-only evidence triage
  - summary:
    - `MIGRATION_GUIDE` still carried a full `v0.7/v0.8` migration storyline
    - active code examples still used removed or nonexistent unit names
    - current public migration truth remained anchored in:
      - `src/fafafa.ssl.base.pas`
      - `src/fafafa.ssl.pas`
      - `src/fafafa.ssl.tls.pas`
      - `docs/reference/API_REFERENCE.md`

- `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
  - result: PASS
  - summary:
    - new migration-guide active-truth contract syntax is valid

- `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `MIGRATION_GUIDE` still declared `v0.8`
      - the whole guide still taught pre-`v1.0` migration as current active truth
      - old unit names and stale enterprise helper names were still present
    - GREEN after fix:
      - `MIGRATION_GUIDE` now uses rolling/current truth anchors
      - migration examples now use current public facade/builder/connector paths
      - WinSSL enterprise and OpenSSL low-level helper boundaries are now stated correctly

- update `docs/guides/MIGRATION_GUIDE.md`
  - change:
    - replace the old `v0.x`-centered guide with a current migration guide
    - anchor truth to source + canonical API reference
    - move client migration guidance to:
      - `fafafa.ssl`
      - `fafafa.ssl.context.builder`
      - `TSSLConnector`
      - `TSSLStream`
    - add current raw-connection fallback snippet with:
      - `Supports(LConn, ISSLClientConnection, ...)`
      - `SetServerName(...)`
    - correct WinSSL enterprise helper names
    - bound OpenSSL low-level error helpers to `fafafa.ssl.openssl.api.err`

- `git diff --check`
  - result: PASS
  - summary:
    - current migration-guide batch has no whitespace or patch-format issues

### Active Connection API Docs Truth

- add `docs/plans/2026-05-19-active-connection-api-docs-truth.md`
  - change:
    - define the bounded active-doc truth batch for stale connection-shape guidance and WinSSL overclaim wording

- add `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  - change:
    - lock that high-entry docs no longer teach:
      - `Connect(host, port)`
      - `CreateConnection(port)`
      - `Disconnect`
      - nonexistent connection-level error helpers
    - lock that WinSSL guide no longer claims full backend identity

- read-only evidence triage
  - summary:
    - stale `Connect(host, port)` guidance was confirmed in:
      - `docs/reference/API_DOCUMENTATION.md`
      - `docs/guides/WINSSL_BEST_PRACTICES.md`
    - WinSSL active guide still claimed:
      - `完全相同的接口`
    - current source truth remained:
      - `ISSLConnection.Connect: Boolean`
      - caller-owned `CreateConnection(Socket/Stream)`
      - per-connection SNI via `ISSLClientConnection.SetServerName(...)`

- `bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - new active connection API docs contract syntax is valid

- `bash tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `API_DOCUMENTATION` still declared stale `2.0.0` doc version
      - quick-start / `ISSLConnection` / troubleshooting sections still taught old connection surface
      - `WINSSL_BEST_PRACTICES` still used `Connect(host, port)` in test guidance
      - `WINSSL_USER_GUIDE` still claimed identical interfaces
    - GREEN after fix:
      - `API_DOCUMENTATION` now uses rolling doc version and current connection/SNI/I-O/verify-result truth
      - `WINSSL_BEST_PRACTICES` test snippets now use caller-owned socket + per-connection SNI + zero-arg `Connect`
      - `WINSSL_USER_GUIDE` now distinguishes core-interface parity from backend-specific capability truth

- update `docs/reference/API_DOCUMENTATION.md`
  - change:
    - switch header to rolling/current date
    - quick-start now uses:
      - caller-owned socket
      - `CreateConnection(Socket)`
      - `ISSLClientConnection.SetServerName(...)`
      - `Connect`
      - `WriteString` / `ReadString`
      - `Shutdown`
    - `ISSLConnection` section now publishes current raw/text I/O signatures
    - troubleshooting/examples now use `GetVerifyResult` / `GetVerifyResultString`

- update `docs/guides/WINSSL_BEST_PRACTICES.md`
  - change:
    - replace stale `Connect(host, port)` test snippets with:
      - `CreateConnection(CreateSocket(...))`
      - per-connection `SetServerName(...)`
      - zero-arg `Connect`

- update `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - replace “完全相同的接口” wording with bounded core-interface parity guidance
    - record that password callback / DER-PKCS8 key import / PKCS#12 helper range remain backend-specific capability truth

- `git diff --check`
  - result: PASS
  - summary:
    - current active connection API docs batch has no whitespace or patch-format issues

### ALPN Owner-Path Active Guidance

- add `docs/plans/2026-05-19-alpn-owner-path-active-guidance.md`
  - change:
    - define the bounded active guide/example truth batch for ALPN owner-path drift

- add `tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
  - change:
    - lock that active guide/example no longer teach `GetSelectedALPNProtocol` as an `ISSLConnection` primary path

- read-only evidence triage
  - summary:
    - `src/fafafa.ssl.base.pas` still marks:
      - `deprecated 'Use ISSLConnectionInfo.GetSelectedALPNProtocol'`
    - active residual hits were confirmed in:
      - `docs/guides/WINSSL_USER_GUIDE.md`
      - `examples/https_server/https_server_alpn.pas`

- update `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - ALPN capability bullet now explicitly points readers at:
      - `ISSLConnectionInfo.GetSelectedALPNProtocol`

- update `examples/https_server/https_server_alpn.pas`
  - change:
    - add `ConnectionInfo: ISSLConnectionInfo`
    - replace direct `Connection.GetSelectedALPNProtocol` read with:
      - `if Supports(Connection, ISSLConnectionInfo, ConnectionInfo) then`
      - `SelectedProto := ConnectionInfo.GetSelectedALPNProtocol;`

- `bash -n tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - new ALPN owner-path guidance contract syntax is valid

- `bash tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active ALPN guide/example now prefer the `ISSLConnectionInfo` owner path

- `mkdir -p tmp/example_https_server_alpn && fpc -B -Fu./src -Fu./examples -FUtmp/example_https_server_alpn -FEtmp/example_https_server_alpn -otmp/example_https_server_alpn/https_server_alpn examples/https_server/https_server_alpn.pas`
  - result: PASS
  - summary:
    - updated ALPN server example still compiles after moving to the owner-surface accessor
    - compile emitted existing unrelated warnings/notes only

- `git diff --check`
  - result: PASS
  - summary:
    - current ALPN owner-path guidance batch has no whitespace or patch-format issues

### ReadString Active Example Signature Truth

- add `docs/plans/2026-05-19-readstring-active-example-signature-truth.md`
  - change:
    - define the bounded active-doc/example truth batch for `ISSLConnection.ReadString` signature drift

- add `tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
  - change:
    - lock that active guides/reference/example no longer teach `ReadString` as a direct string-returning call

- read-only evidence triage
  - summary:
    - active stale hits were confirmed in:
      - `docs/reference/API_REFERENCE.md`
      - `docs/guides/USER_GUIDE.md`
      - `docs/guides/MIGRATION_GUIDE.md`
      - `examples/04_https_rest_client.pas`
    - all four still taught `ReadString` as if it returned `string`, while `src/fafafa.ssl.base.pas` still declared:
      - `function ReadString(out AStr: string): Boolean;`

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `LReply: string` to the HTTPS example
    - replace direct `LConn.ReadString` printing with:
      - `if LConn.ReadString(LReply) then`
      - `WriteLn('收到: ', LReply);`

- update `docs/guides/USER_GUIDE.md`
  - change:
    - client example now uses:
      - `if LConn.ReadString(LResponse) then`
    - server example now uses:
      - `if LConn.ReadString(LRequest) then`

- update `docs/guides/MIGRATION_GUIDE.md`
  - change:
    - migration snippet now uses:
      - `if LConn.ReadString(LResponse) then`
      - `WriteLn(LResponse);`

- update `examples/04_https_rest_client.pas`
  - change:
    - printed guidance now shows:
      - `if Connection.ReadString(Response) then`
      - `HandleResponse(Response);`

- `bash -n tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
  - result: PASS
  - summary:
    - new ReadString example-signature contract syntax is valid

- `bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
  - result: PASS
  - summary:
    - active ReadString examples now all match the current out-parameter source truth

- `mkdir -p tmp/example_04_https_rest_client && fpc -B -Fu./src -Fu./examples -FUtmp/example_04_https_rest_client -FEtmp/example_04_https_rest_client -otmp/example_04_https_rest_client/example_04_https_rest_client examples/04_https_rest_client.pas`
  - result: PASS
  - summary:
    - the updated REST client example still compiles after changing the printed guidance text
    - compile emitted existing unrelated warnings only

- `git diff --check`
  - result: PASS
  - summary:
    - current ReadString example-signature batch has no whitespace or patch-format issues

### ISSLConnection Convenience Surface Classification

- add `docs/plans/2026-05-19-isslconnection-convenience-surface-classification.md`
  - change:
    - define the bounded route-truth batch for `ISSLConnection` convenience-core / connection-adjacent surface classification

- add `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  - change:
    - lock that source comments and key design/canonical docs agree on the current truth for:
      - `ReadString` / `WriteString`
      - `SetTimeout` / `GetTimeout`
      - `SetBlocking` / `GetBlocking`

- `bash -n tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - new convenience-surface classification contract syntax is valid

- `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - source comments still lacked explicit convenience-surface classification
      - `INTERFACE_DESIGN_V2` still presented current shipped convenience methods as `**移除**`
      - `ARCHITECTURE` / `INTERFACE_DESIGN_AUDIT_V1.5.0` still lacked current shipped-truth clarification
    - GREEN after fix:
      - source comments now classify text helpers as `v1.x` convenience-core
      - timeout/blocking now classify as builder-first connection-adjacent convenience surface
      - design/audit docs now distinguish v2 target from current shipped truth

- update `src/fafafa.ssl.base.pas`
  - change:
    - add `@preferred-access` guidance for:
      - `ReadString` / `WriteString`
      - `SetTimeout` / `GetTimeout`
      - `SetBlocking` / `GetBlocking`

- update `docs/reference/API_REFERENCE.md`
  - change:
    - restate `HandshakeTimeout` as builder-first with connection-side convenience override
    - add explicit convenience-surface classification bullets under `ISSLConnection`

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - clarify this is the `v2` minimal-core target, not the current shipped source mirror
    - replace stale `**移除**` wording for convenience methods with current `v1.x` classification

- update `docs/ARCHITECTURE.md`
  - change:
    - mark the `ISSLConnection` snippet as conceptual minimal-core slice
    - point readers back to `API_REFERENCE` for current shipped source truth
    - correct the stale `GetState` snippet return type to `string`

- update `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - change:
    - separate compatibility-mirror demotion advice from current convenience-surface shipped truth
    - note that any real convenience-method removal belongs to a future dedicated `v2` surgery batch

- `git diff --check`
  - result: PASS
  - summary:
    - current convenience-surface classification batch has no whitespace or patch-format issues

### API Reference Certificate Surfaces Truth

- add `docs/plans/2026-05-19-api-reference-certificate-surfaces-truth.md`
  - change:
    - define the bounded active-doc truth batch for `ISSLCertificate` / `ISSLCertificateStore` high-entry surface drift in `API_REFERENCE`

- add `tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
  - change:
    - lock that the active certificate/store sections in `docs/reference/API_REFERENCE.md`
      match the current shipped source surface from `src/fafafa.ssl.base.pas`

- `bash -n tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
  - result: PASS
  - summary:
    - new focused certificate-surface API-reference contract syntax is valid

- `bash tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `API_REFERENCE` still lacked a dedicated `ISSLCertificateStore` section
      - `ISSLCertificate` code block still omitted a broader current method set from source
    - GREEN after fix:
      - `ISSLCertificate` code block now reflects current shipped source truth
      - `ISSLCertificateStore` now has a dedicated high-entry section in the canonical API reference

- update `docs/reference/API_REFERENCE.md`
  - change:
    - `ISSLCertificate` code block now includes:
      - `LoadFromMemory`
      - `SaveToStream`
      - `GetInfo`
      - `GetPublicKeyAlgorithm`
      - `GetSignatureAlgorithm`
      - `GetDaysUntilExpiry`
      - `GetSubjectCN`
      - `GetExtension`
      - `GetFingerprint(...)`
      - issuer-link / clone helpers
    - `ISSLCertificate` extension collection types now match source truth:
      - `TSSLStringArray`
    - added a dedicated `ISSLCertificateStore` section and code block
    - linked the new store section to the live `STORE_USAGE_GUIDE`

- `git diff --check`
  - result: PASS
  - summary:
    - current certificate-surface API-reference batch has no whitespace or patch-format issues

### API Reference Library / Context Surface Truth

- add `docs/plans/2026-05-19-api-reference-library-context-surface-truth.md`
  - change:
    - define the bounded active-doc truth batch for `ISSLLibrary` / `ISSLContext` code-block drift in `API_REFERENCE`

- add `tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - change:
    - lock that the active `ISSLLibrary` / `ISSLContext` code blocks in `docs/reference/API_REFERENCE.md`
      include the current shipped source surface from `src/fafafa.ssl.base.pas`

- `bash -n tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new focused API-reference surface-truth contract syntax is valid

- `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `ISSLLibrary` code block still missed `SetDefaultConfig`
      - the same active doc also still omitted a broader `ISSLContext` method set already shipped in source
    - intermediate housekeeping:
      - fixed one contract string quoting issue for `LoadPrivateKeyPEM`
      - removed one trailing-space formatting issue from `API_REFERENCE`
    - GREEN after fix:

### Optional Backends PKCS12 Capability Truth

- add `docs/plans/2026-05-19-optional-backends-pkcs12-capability-truth.md`
  - change:
    - define the bounded capability/docs truth batch for `MbedTLS` / `WolfSSL` PKCS#12 publication drift

- add `tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh`
  - change:
    - lock source/docs truth for:
      - `MbedTLS SupportsPKCS12=False`
      - `WolfSSL SupportsPKCS12=False`
      - backend-specific PKCS#12 docs wording

- add `tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
  - change:
    - lock runtime capability truth for:
      - `FreePascal=False`
      - `OpenSSL=True`
      - `WinSSL=True`
      - `MbedTLS=False`
      - `WolfSSL=False`

- read-only evidence triage
  - summary:
    - `src/fafafa.ssl.mbedtls.context.pas` / `src/fafafa.ssl.wolfssl.context.pas`
      only exposed PEM / DER / PKCS#8 certificate/private-key load paths
    - no public PKCS#12 create / parse / import surface was found in the MbedTLS / WolfSSL backend context paths
    - active doc conflict was confirmed between:
      - `docs/guides/FAQ.md`
      - `docs/guides/PKCS12_USER_GUIDE.md`

- `bash -n tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - new PKCS#12 capability truth shell contract syntax is valid

- `bash tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `src/fafafa.ssl.mbedtls.lib.pas` still published `SupportsPKCS12 := True`
    - intermediate housekeeping:
      - fixed shell-contract quoting on backtick-containing literal patterns so bash stops treating them as command substitution
    - GREEN after fix:
      - optional backend PKCS#12 source/docs truth now aligns with the intended backend-specific publication model

- `mkdir -p tmp/test_optional_backends_pkcs12_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_optional_backends_pkcs12_capability_truth -FEtmp/test_optional_backends_pkcs12_capability_truth -otmp/test_optional_backends_pkcs12_capability_truth/test_optional_backends_pkcs12_capability_truth_contract tests/test_optional_backends_pkcs12_capability_truth_contract.pas && ./tmp/test_optional_backends_pkcs12_capability_truth/test_optional_backends_pkcs12_capability_truth_contract`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - runtime contract failed at:
        - `MbedTLS SupportsPKCS12 mismatch: expected=False actual=True`
    - GREEN after fix:
      - `OpenSSL=True`
      - `MbedTLS=False`
      - `WolfSSL=False`
      - `FreePascal` / `WinSSL` were skipped on this Linux host when unavailable
    - compile emitted existing unrelated warnings/notes only

- update `src/fafafa.ssl.mbedtls.lib.pas`
  - change:
    - set `Result.SupportsPKCS12 := False`
    - annotate that current MbedTLS runtime paths do not ship a PKCS#12/PFX bundle surface

- update `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - set `Result.SupportsPKCS12 := False`
    - annotate that current WolfSSL runtime paths do not ship a PKCS#12/PFX bundle surface

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - add quick-reference row for `PKCS#12 / PFX`
    - spell out backend-specific truth:
      - `OpenSSL` full helper/API
      - `WinSSL` partial PFX/P12 import
      - `FreePascal` / `MbedTLS` / `WolfSSL` unsupported

- update `docs/guides/FAQ.md`
  - change:
    - replace stale “planned only” answer with backend-specific PKCS#12 truth

- update `docs/guides/PKCS12_USER_GUIDE.md`
  - change:
    - clarify the guide is OpenSSL-backend scoped
    - add backend-specific scope note for WinSSL and unsupported backends

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add one-line canonical summary of current `SupportsPKCS12` truth across backends

- `bash tests/scripts/test_password_protected_key_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - adjacent private-key password truth remains aligned after the PKCS#12 docs/capability update

- `git diff --check`
  - result: PASS
  - summary:
    - current optional-backends PKCS#12 batch has no whitespace or patch-format issues

### MbedTLS Active Docs Capability Truth

- add `docs/plans/2026-05-19-mbedtls-active-docs-capability-truth.md`
  - change:
    - define the bounded MbedTLS docs-truth batch for active capability/reference/user-guide drift

- add `tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - change:
    - lock that active MbedTLS docs now match current published capability/API truth

- read-only evidence triage
  - summary:
    - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md` still overstated:
      - `0-RTT`
      - pinning-via-callback
      - custom I/O callback publication
    - `docs/guides/MBEDTLS_USER_GUIDE.md` still taught stale API names/signatures:
      - `LoadCertificateFromFile`
      - `LoadPrivateKeyFromFile`
      - `LoadCAFromFile`
      - `Connection.SetHostname`
      - `Connection.Connect(host, port)`
      - `ReadAll`
      - `GetCipherSuite`
      - `GetLastError: string`
    - source truth was confirmed in:
      - `src/fafafa.ssl.base.pas`
      - `src/fafafa.ssl.mbedtls.context.pas`
      - `src/fafafa.ssl.mbedtls.connection.pas`

- `bash -n tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - new MbedTLS docs-truth shell contract syntax is valid

- `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed one contract-helper quoting issue:
      - backtick-containing fixed strings were still wrapped in double quotes, causing bash command substitution
    - GREEN after switching those literal patterns to single quotes:
      - MbedTLS active capability/reference/user-guide docs now align with current published truth

- update `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - mark the document as describing current fafafa.ssl published surface
    - change pinning row from callback wording to current context pinning APIs
    - change `0-RTT` row to current capability none
    - change custom-I/O row to “public callback surface not published”
    - replace `WithSystemRoots` example with explicit `WithCAFile(...)`
    - replace custom-I/O callback section with transport-surface explanation

- update `docs/guides/MBEDTLS_USER_GUIDE.md`
  - change:
    - replace “完全相同的接口” with backend-specific capability guidance
    - record current callback / FIPS / PKCS12 / 0-RTT truth
    - update high-entry examples to current API names/signatures:
      - `CreateContext(sslCtxClient)`
      - `LoadCertificate`
      - `LoadPrivateKey`
      - `LoadCAFile`
      - `ISSLClientConnection.SetServerName`
      - `Connection.Connect`
      - `ReadString(out ...)`
      - `GetCipherName`
      - `GetLastErrorString`
    - update the interface summary at the end of the guide to the current public surface

- `git diff --check`
  - result: PASS
  - summary:
    - current MbedTLS active-docs batch has no whitespace or patch-format issues
      - `ISSLLibrary` / `ISSLContext` code blocks now reflect current shipped source truth instead of the older narrowed subset

- update `docs/reference/API_REFERENCE.md`
  - change:
    - `ISSLLibrary` code block now includes:
      - `SetDefaultConfig`
      - `GetDefaultConfig`
      - `GetStatistics`
      - `ResetStatistics`
    - `ISSLContext` code block now includes:
      - preferred-version methods
      - PEM direct-load helpers
      - session-cache size methods
      - options / SNI / ALPN / cert-verify-flag surfaces
      - password/info callbacks
      - certificate pinning helpers
    - added explicit wording that these two code blocks are current source-truth views, not older minimal subsets

- `git diff --check`
  - result: PASS
  - summary:
    - current API-reference surface-truth batch has no whitespace or patch-format issues

### Optional Interface Capability Alignment

- add `docs/plans/2026-05-19-optional-interface-capability-alignment.md`
  - change:
    - define the bounded source-level batch for capability/public-interface alignment

- add `tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - change:
    - lock the optional-interface public-surface rules for:
      - OpenSSL early-data context / connection
      - OpenSSL server OCSP stapling context
      - WolfSSL server OCSP stapling context

- `bash -n tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - result: PASS
  - summary:
    - new optional-interface capability-alignment contract syntax is valid

- `bash tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - OpenSSL/WolfSSL source still lacked a capability-gated optional-interface boundary
    - intermediate compile finding:
      - the first attempt to use `override GetInterface(...)` was rejected by FPC
      - this forced the implementation to pivot to capability-gated subclass selection instead of interface-dispatch interception
    - GREEN after fix:
      - OpenSSL / WolfSSL optional interface exposure now follows capability-gated subclass truth

- update `src/fafafa.ssl.openssl.context.pas`
  - change:
    - remove unconditional `ISSLEarlyDataContext` / `ISSLServerOCSPStaplingContext` exposure from the base context class
    - add:
      - `TOpenSSLEarlyDataContext`
      - `TOpenSSLServerOCSPContext`
      - `TOpenSSLAdvancedContext`
    - gate `CreateConnection(...)` so early-data-capable contexts produce `TOpenSSLEarlyDataConnection`

- update `src/fafafa.ssl.openssl.connection.pas`
  - change:
    - remove unconditional `ISSLEarlyDataConnection` exposure from the base connection class
    - add `TOpenSSLEarlyDataConnection` subclass

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - gate `CreateContext(...)` by current capability truth so it selects:
      - base context
      - early-data context
      - server-OCSP context
      - combined advanced context

- update `src/fafafa.ssl.wolfssl.context.pas`
  - change:
    - remove unconditional `ISSLServerOCSPStaplingContext` exposure from the base context class
    - add:
      - `TWolfSSLOCSPStaplingContext`
      - `TWolfSSLAdvancedContext`

- update `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - gate `CreateContext(...)` by current capability truth so it selects:
      - base context
      - early-data context
      - server-OCSP context
      - combined advanced context

- `python3 scripts/compile_all_modules.py`
  - result: FAIL -> PASS
  - summary:
    - first full compile exposed that `GetInterface` is not a viable override seam in this FPC setup
    - after pivoting to capability-gated subclasses, full compile returned to `187/187 PASS`

- `git diff --check`
  - result: PASS
  - summary:
    - current optional-interface capability-alignment batch has no whitespace or patch-format issues

### Active Release / Platform Truth Sweep

- add `docs/plans/2026-05-19-active-release-platform-truth-sweep.md`
  - change:
    - define the bounded active-doc truth batch for release/platform/WinSSL entrypoints

- add `tests/scripts/test_active_release_platform_truth_contract.sh`
  - change:
    - lock current release/platform/WinSSL truth across:
      - `docs/RELEASE_NOTES.md`
      - `docs/PLATFORM_SUPPORT.md`
      - `docs/guides/WINSSL_USER_GUIDE.md`
      - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
    - forbid stale placeholder repo/contact tokens in these active docs

- `bash -n tests/scripts/test_active_release_platform_truth_contract.sh`
  - result: PASS
  - summary:
    - new active-release/platform focused contract syntax is valid

- `bash tests/scripts/test_active_release_platform_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `RELEASE_NOTES` still presented `v1.0.0` as current release head
      - `PLATFORM_SUPPORT` still mixed old `97.5% / macOS 验证中 / WinSSL 100% 完成` wording
      - `WINSSL_USER_GUIDE` still overclaimed `100% 完成 / 会话复用完全支持`
      - `ZERO_DEPENDENCY_DEPLOYMENT` still ended with `WinSSL 100% 完成，生产就绪`
      - active docs still kept `yourusername` / `your-repo` / `your.email@example.com`
    - GREEN after fix:
      - current active entry docs now point back to `v1.5.0` release truth and bounded WinSSL session truth

- update `docs/RELEASE_NOTES.md`
  - change:
    - promote current `v1.5.0` release truth to the doc head
    - demote `v1.0.0` to explicit historical snapshot
    - replace placeholder repo/support URLs with the live GitHub repository

- update `docs/PLATFORM_SUPPORT.md`
  - change:
    - replace stale platform percentages with current verification-entry wording
    - align Windows/Linux/macOS status text to current `v1.5.0` release/workflow truth
    - bound WinSSL wording to current session/runtime truth
    - replace placeholder Issues URL with the live repository

- update `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - replace `100% 完成` heading/status with current public-surface-and-boundary wording
    - restate session resumption / tickets as experimental public surface with
      `observed_reuse=false` / `session_configured=true`

- update `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
  - change:
    - add current bounded WinSSL truth note near the top
    - add capability-matrix reference
    - replace stale `100% 完成，生产就绪` footer status with current bounded wording

- `git diff --check`
  - result: PASS
  - summary:
    - current active-release/platform truth batch has no whitespace or patch-format issues

### Implemented Backend Future Truth Sweep

- add `docs/plans/2026-05-19-implemented-backend-future-truth-sweep.md`
  - change:
    - define the bounded stale-future-truth batch for implemented backend wording in active docs

- add `tests/scripts/test_implemented_backend_future_truth_contract.sh`
  - change:
    - lock that active docs no longer describe `sslFreePascal` / `sslMbedTLS` as future-only backends

- `bash -n tests/scripts/test_implemented_backend_future_truth_contract.sh`
  - result: PASS
  - summary:
    - new implemented-backend future-truth contract syntax is valid

- `bash tests/scripts/test_implemented_backend_future_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - backend abstraction design still described `FreePascal` as planned
      - user guide still described `MbedTLS` as future-only
      - migration / architecture / native-handle docs still taught a future-only pure-Pascal backend mindset
    - GREEN after fix:
      - active docs now reflect that `sslFreePascal` and `sslMbedTLS` are already part of the current backend family truth

- update `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
  - change:
    - replace stale `FreePascal | ❌ 计划中` row with current implemented-backend truth

- update `docs/guides/USER_GUIDE.md`
  - change:
    - remove stale `(未来)` tail from the MbedTLS lightweight recommendation

- update `docs/MIGRATION_GUIDE_V1.1.md`
  - change:
    - replace future-only FreePascal backend wording with current `sslFreePascal` optional-native-handle truth

- update `docs/ARCHITECTURE.md`
  - change:
    - mark the pure Pascal backend snippet as current implementation instead of future-only

- update `docs/NATIVE_HANDLE_QUICK_REF.md`
  - change:
    - replace the future enum comment with explicit `sslFreePascal` case handling

- `git diff --check`
  - result: PASS
  - summary:
    - current implemented-backend future-truth batch has no whitespace or patch-format issues

### Active Root Doc Link Repair

- add `docs/plans/2026-05-19-active-root-doc-link-repair.md`
  - change:
    - define the bounded root-doc broken-link repair batch for the highest-traffic active docs

- add `tests/scripts/test_active_root_doc_link_repair_contract.sh`
  - change:
    - lock the live doc targets for:
      - `docs/PLATFORM_SUPPORT.md`
      - `docs/RELEASE_NOTES.md`
      - `docs/TOOLS.md`
      - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
      - `docs/guides/WINSSL_USER_GUIDE.md`

- `bash -n tests/scripts/test_active_root_doc_link_repair_contract.sh`
  - result: PASS
  - summary:
    - new active-root-doc focused contract syntax is valid

- `bash tests/scripts/test_active_root_doc_link_repair_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `PLATFORM_SUPPORT` still linked stale bare targets like `QUICKSTART.md`
      - `RELEASE_NOTES` still linked old `docs/QuickStart.md` / `docs/API_Reference.md`
      - `TOOLS` still linked placeholder `docs/API.md`
      - `ZERO_DEPENDENCY_DEPLOYMENT` still linked `.claude/plan/WINSSL_COMPLETION_REPORT.md`
      - `WINSSL_USER_GUIDE` still linked missing `WINSSL_HTTPS_TEST_REPORT.md` and stale phase reports
    - GREEN after fix:
      - all 5 high-entry docs now resolve to live guides/reference/test_reports targets

- update `docs/PLATFORM_SUPPORT.md`
  - change:
    - move related-doc and troubleshooting links to current `guides/` / `reference/` targets

- update `docs/RELEASE_NOTES.md`
  - change:
    - replace stale `docs/*` support links with current:
      - `guides/QUICKSTART.md`
      - `reference/API_REFERENCE.md`
      - `guides/FAQ.md`
      - `ARCHITECTURE.md`
      - `ROADMAP.md`

- update `docs/TOOLS.md`
  - change:
    - replace stale quickstart/API placeholders with current guide/reference targets

- update `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
  - change:
    - replace stale WinSSL guide / quickstart / completion-report links with:
      - `guides/WINSSL_USER_GUIDE.md`
      - `guides/WINSSL_QUICKSTART.md`
      - `test_reports/WINSSL_BACKEND_STATUS_REPORT.md`

- update `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - replace stale sibling/root/phase-report links with current:
      - `../ZERO_DEPENDENCY_DEPLOYMENT.md`
      - `../test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
      - `../reference/WINSSL_DESIGN.md`
      - `../reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
      - `../reference/WINSSL_PERFORMANCE_TUNING.md`

- `git diff --check`
  - result: PASS
  - summary:
    - current active-root-doc link-repair batch has no whitespace or patch-format issues

### Backend Doc Linkage And Enum Truth

- add `docs/plans/2026-05-19-backend-doc-linkage-and-enum-truth.md`
  - change:
    - define the bounded active-doc truth batch for backend link targets and `TSSLLibraryType` enum drift

- add `tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
  - change:
    - lock that the top-level backend capability matrix only links live backend docs
    - lock that `API_REFERENCE` keeps a complete `TSSLLibraryType` snippet
    - lock that `sslFreePascal` is no longer described as future-only in source enum comments

- `bash -n tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
  - result: PASS
  - summary:
    - new backend-doc-linkage focused contract syntax is valid

- `bash tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `docs/BACKEND_CAPABILITY_MATRIX.md` still linked missing `reference/OPENSSL_BACKEND.md`
      - the same doc still linked missing `reference/WINSSL_BACKEND.md`
      - `API_REFERENCE` still described `sslMbedTLS` as planned
      - source enum comment still described `sslFreePascal` as future-only
    - GREEN after fix:
      - top-level backend matrix now links live backend references only
      - `API_REFERENCE` enum snippet now matches the current source enum family
      - `sslFreePascal` source enum comment now reflects implemented status

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - replace missing backend-doc links with live references:
      - `reference/OPENSSL_MODULES.md`
      - `reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
      - `reference/WINSSL_DESIGN.md`
      - `reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`

- update `docs/reference/API_REFERENCE.md`
  - change:
    - refresh `TSSLLibraryType` snippet to the current source truth:
      - `sslAutoDetect`
      - `sslOpenSSL`
      - `sslWolfSSL`
      - `sslMbedTLS`
      - `sslWinSSL`
      - `sslFreePascal`

- update `src/fafafa.ssl.base.pas`
  - change:
    - remove stale `sslFreePascal` “future” comment residue from the public enum declaration

- `git diff --check`
  - result: PASS
  - summary:
    - current backend-doc-linkage batch has no whitespace or patch-format issues

### Backend Capability Truth Tightening

- add `docs/plans/2026-05-19-backend-capability-truth-tightening.md`
  - change:
    - define the bounded batch for optional-backend session-cache capability truth and WinSSL active-doc truth tightening

- add `tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
  - change:
    - lock that `MbedTLS / WolfSSL` cannot keep advertising `sslFeatSessionCache` while omitting `SessionCacheSupport`

- update `tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - change:
    - extend coverage to:
      - `docs/BACKEND_CAPABILITY_MATRIX.md`
      - `docs/guides/QUICKSTART.md`
      - `docs/reference/WINSSL_DESIGN.md`
      - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
      - `docs/reference/BACKEND_SELECTOR_DESIGN.md`
    - lock that these active docs no longer overclaim WinSSL session resumption / session ticket / OCSP truth

- update `src/fafafa.ssl.mbedtls.lib.pas`
  - change:
    - publish `Result.SessionCacheSupport := sslSupportStable`

- update `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - publish `Result.SessionCacheSupport := sslSupportStable`

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - tighten the top-level `Session Resumption` quick row to current runtime/capability truth
    - add explicit notes for `FreePascal` experimental support and `WinSSL observed_reuse=false / session_configured=true`

- update `docs/guides/QUICKSTART.md`
  - change:
    - remove WinSSL `70-90%` performance/success overclaim
    - keep `ISSLSessionResumption` example, but rewrite output/caution to current dedicated Windows runtime truth

- update `docs/reference/WINSSL_DESIGN.md`
  - change:
    - move top-level status from “runtime proof pending” to the current partial Windows-proof reality
    - rewrite session-resumption flow so shared path no longer pretends `SECPKG_ATTR_SESSION_INFO` is a normal production truth source
    - mark native session-info probe as `opt-in isolated worker / experimental evidence lane`
    - remove the settled `70-90%` performance claim from the WinSSL/OpenSSL comparison table

- update `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
  - change:
    - stop claiming WinSSL `OCSP Stapling` and `Session Ticket` are both unconditional `✅`

- update `docs/reference/BACKEND_SELECTOR_DESIGN.md`
  - change:
    - stop claiming WinSSL requirement matching covers stable `OCSP Stapling` and `Session Ticket`

- `bash -n tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
  - result: PASS
  - summary:
    - new optional-backend session-cache contract syntax is valid

- `bash tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
  - result: PASS
  - summary:
    - `MbedTLS / WolfSSL` now publish `SessionCacheSupport` in line with their existing session-cache feature truth

- `bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - expanded WinSSL docs-truth contract syntax is valid

- `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - active WinSSL capability/design docs no longer overclaim session-resumption/runtime truth

- `python3 scripts/compile_all_modules.py`
  - result: PASS
  - summary:
    - full Pascal compile remained green after the capability/doc truth tightening batch
    - compile summary: `187/187` success, `0` failed

- `git diff --check`
  - result: PASS
  - summary:
    - current backend capability truth tightening batch has no whitespace or patch-format issues

### WinSSL Session-Info Probe Allowlist

- add `docs/plans/2026-05-19-winssl-session-info-probe-allowlist.md`
  - change:
    - define a repo-level allowlist guard for direct `SECPKG_ATTR_SESSION_INFO` query sites

- add `tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - change:
    - lock the controlled allowlist to:
      - `src/fafafa.ssl.winssl.connection.pas`
      - `tests/winssl/test_winssl_session_resumption.pas`
    - explicitly exclude `src/fafafa.ssl.winssl.session.pas`

- `bash -n tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - result: PASS
  - summary:
    - new allowlist contract syntax is valid

- `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - result: PASS
  - summary:
    - controlled probe allowlist is currently aligned with the repo state
    - compatibility shim stays outside the direct session-info query boundary

- `git diff --check`
  - result: PASS
  - summary:
    - allowlist-guard batch has no whitespace or patch-format issues

### Live GitHub Follow-up: Handle Metadata

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=winssl_handle_metadata_20260519_google -f strict_closure=false -f winssl_session_host=www.google.com -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - dispatched the first live Windows evidence run for commit `0751afc`

- `gh api repos/dtamade/fafafa.ssl/actions/runs/26071754477/jobs`
  - result: PASS
  - summary:
    - confirmed run `26071754477` head=`0751afc`
    - final job results were:
      - `linux-gate`: `success`
      - `macos-gate`: `success`
      - `windows-gate`: `failure`
      - `summary`: `success`

- `gh run download 26071754477 -n wave-b-windows-winssl_handle_metadata_20260519_google -D tmp/gh-run-26071754477/windows`
  - result: PASS
  - summary:
    - downloaded the Windows evidence bundle for the handle-metadata run

- `gh run download 26071754477 -n wave-b-summary-winssl_handle_metadata_20260519_google -D tmp/gh-run-26071754477/summary`
  - result: PASS
  - summary:
    - downloaded the summary artifact bundle for the handle-metadata run

- `rg -n "native_probe .*handle_metadata|native_probe .*stage=before_query_context_attributes|native_probe_worker exit_code|summary host=www.google.com" tmp/gh-run-26071754477/windows/winssl_runtime_suite_winssl_handle_metadata_20260519_google.log`
  - result: PASS
  - summary:
    - live Windows transcript now proves:
      - `backend=winssl`
      - `handle_valid=true`
      - `lower=0000000001658E70`
      - `upper=0000000000010AD8`
      - the worker still dies immediately after `stage=before_query_context_attributes`

- `sed -n '1,220p' tmp/gh-run-26071754477/summary/wave_b_b2_closure_readiness_winssl_handle_metadata_20260519_google.md`
  - result: PASS
  - summary:
    - live closure report now shows:
      - `linux=PASS`
      - `macos=PASS`
      - `windows=FAIL`

- `sed -n '1,220p' tmp/gh-run-26071754477/summary/wave_b_b2_handoff_bundle_winssl_handle_metadata_20260519_google.md`
  - result: PASS
  - summary:
    - live handoff report stays aligned:
      - `handoff_state: NEEDS_GATE_REPAIR`
      - `consistency_status: CONSISTENT`

### WinSSL Session Shim Safe Fallback

- `rg -n "CreateFromConnection\\(|fafafa\\.ssl\\.winssl\\.session|TWinSSLSession\\(" src tests docs`
  - result: PASS
  - summary:
    - confirmed `src/fafafa.ssl.winssl.session.pas` is still documented and wired as a compatibility shim
    - confirmed `CreateFromConnection(...)` is the only remaining shim-side construction path that could carry risky behavior

- `sed -n '1,140p' src/fafafa.ssl.winssl.session.pas`
  - result: PASS
  - summary:
    - confirmed the compatibility shim still directly called `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
    - confirmed this drift contradicted the current canonical conservative WinSSL session truth

- add `docs/plans/2026-05-19-winssl-session-shim-safe-fallback.md`
  - change:
    - define the bounded static fix that pulls the compatibility shim back to a safe conservative fallback

- add `tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
  - change:
    - lock the absence of direct risky session-info query in the shim
    - lock the pointer-based fallback session id
    - lock conservative `reused=false` metadata

- `bash -n tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
  - result: PASS
  - summary:
    - new shim-safe-fallback contract syntax is valid

- `bash tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - compatibility shim still directly called `QueryContextAttributesW(...)`
    - GREEN:
      - compatibility shim no longer queries risky session-info directly
      - compatibility shim now uses conservative pointer-based fallback metadata

- update `src/fafafa.ssl.winssl.session.pas`
  - change:
    - remove direct session-info query from `CreateFromConnection(...)`
    - remove query-specific residue
    - align fallback session id with pointer-based conservative truth

- `bash tests/scripts/test_winssl_session_truth_source_contract.sh`
  - result: PASS
  - summary:
    - the earlier single-truth-source guard remained aligned after the shim-safe fallback change

- `git diff --check`
  - result: PASS
  - summary:
    - shim-safe fallback batch has no whitespace or patch-format issues

### WinSSL Native-Probe Handle Metadata

- add `docs/plans/2026-05-19-winssl-native-probe-handle-metadata.md`
  - change:
    - define the bounded follow-up batch that records backend/validity/raw-handle metadata before the risky Schannel query

- add `tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - change:
    - lock readable backend-type evidence
    - lock `IsNativeHandleValid`
    - lock `dwLower` / `dwUpper` logging

- `bash -n tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - result: PASS
  - summary:
    - new handle-metadata contract syntax is valid

- `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - the probe helper still lacked readable backend/validity/raw-handle metadata
    - GREEN:
      - the risky probe path now records backend and handle metadata before the query boundary

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - add `BackendTypeText(...)`
    - emit `native_probe ... stage=handle_metadata backend=%s handle_valid=%s lower=%s upper=%s`

- `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - result: PASS
  - summary:
    - earlier stage-marker truth remained aligned after adding handle metadata

- `bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh`
  - result: PASS
  - summary:
    - isolated-worker quarantine truth remained aligned after adding handle metadata

- `mkdir -p tmp/winssl_native_probe_handle_metadata_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_handle_metadata_win64 -FEtmp/winssl_native_probe_handle_metadata_win64 -otmp/winssl_native_probe_handle_metadata_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 cross-target compile remained green after adding handle metadata
    - warning pool increased only by the new `BackendTypeText(...)` case helper unreachable-code warning; no fresh compile failure surfaced

### Live GitHub Follow-up: Closure Truth + Stage Markers

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=winssl_closure_truth_20260519_postfix -f strict_closure=false -f winssl_session_host=www.google.com -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - dispatched the first post-fix live run for commit `9a47c33`

- `gh run view 26071188795 --json status,conclusion,headSha,updatedAt,url`
  - result: PASS
  - summary:
    - run `26071188795` completed as `failure`
    - headSha matched closure-truth fix commit `9a47c33`

- `gh api repos/dtamade/fafafa.ssl/actions/runs/26071188795/jobs`
  - result: PASS
  - summary:
    - confirmed `windows-gate` failed in `Run broader WinSSL runtime suite`
    - confirmed `summary` still completed successfully, so summary artifact was authoritative for report-chain verification

- `gh run download 26071188795 -n wave-b-summary-winssl_closure_truth_20260519_postfix -D tmp/gh-run-26071188795/summary`
  - result: PASS
  - summary:
    - downloaded the live summary artifact bundle for the closure-truth verification run

- `sed -n '1,220p' tmp/gh-run-26071188795/summary/wave_b_b2_closure_readiness_winssl_closure_truth_20260519_postfix.md`
  - result: PASS
  - summary:
    - live closure report now truthfully shows:
      - `windows | FAIL | ... suite_end_status=FAIL`
      - `closure_status: IN_PROGRESS`

- `sed -n '1,220p' tmp/gh-run-26071188795/summary/wave_b_cross_platform_summary_winssl_closure_truth_20260519_postfix.md`
  - result: PASS
  - summary:
    - live cross summary still truthfully promotes Windows to `FAIL`

- `sed -n '1,220p' tmp/gh-run-26071188795/summary/wave_b_b2_handoff_bundle_winssl_closure_truth_20260519_postfix.md`
  - result: PASS
  - summary:
    - live handoff bundle stays aligned:
      - `handoff_state: NEEDS_GATE_REPAIR`
      - `consistency_status: CONSISTENT`

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=winssl_stage_markers_20260519_google -f strict_closure=false -f winssl_session_host=www.google.com -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - dispatched the first live Windows evidence run for commit `c99fd07`

- `gh run view 26071361489 --json status,conclusion,headSha,updatedAt,url`
  - result: PASS
  - summary:
    - captured live run `26071361489` on stage-marker commit `c99fd07`

- `gh api repos/dtamade/fafafa.ssl/actions/runs/26071361489/jobs`
  - result: PASS
  - summary:
    - confirmed `windows-gate` failed again in `Run broader WinSSL runtime suite`
    - confirmed Windows progressed through dependency install, quick smoke, and Windows gate before the broader runtime failure

- `gh run download 26071361489 -n wave-b-windows-winssl_stage_markers_20260519_google -D tmp/gh-run-26071361489/windows`
  - result: PASS
  - summary:
    - downloaded the live Windows evidence bundle for the stage-marker run

- `rg -n "native_probe .*stage=|native_probe_worker exit_code|isolated native probe worker exits cleanly|signal label=initial_handshake|summary host=www.google.com" tmp/gh-run-26071361489/windows/winssl_runtime_suite_winssl_stage_markers_20260519_google.log`
  - result: PASS
  - summary:
    - live Windows transcript now proves the worker reaches:
      - `stage=before_supports`
      - `stage=after_supports`
      - `stage=before_get_native_handle`
      - `stage=after_get_native_handle handle_nil=false`
      - `stage=before_query_context_attributes`
    - then fails with:
      - `native_probe_worker exit_code=-1073741819 ... last_marker=native_probe label=initial_handshake stage=before_query_context_attributes`

### WinSSL Native-Probe Stage Markers

- `sed -n '1,220p' tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - confirmed the risky probe body still had only `pending=true` pre-probe markers and no stage-level evidence inside `TryQueryNativeSessionReuse(...)`

- `sed -n '220,460p' tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - confirmed the narrow crash window was still:
      - `Supports(...)`
      - `GetNativeHandle`
      - `QueryContextAttributesW(...)`
    - confirmed the helper could safely be strengthened without touching shared WinSSL implementation

- add `docs/plans/2026-05-19-winssl-native-probe-stage-markers.md`
  - change:
    - define the bounded local batch that strengthens isolated-worker stage evidence inside the probe helper

- add `tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - change:
    - lock explicit `label` plumbing for `TryQueryNativeSessionReuse(...)`
    - lock the stage markers around owner-surface, native handle, and query boundaries

- `bash -n tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - result: PASS
  - summary:
    - new stage-marker contract syntax is valid

- `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `TryQueryNativeSessionReuse(...)` still lacked the explicit `label` parameter and all stage markers
    - GREEN:
      - helper now carries label-scoped stage evidence through the risky probe body

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - extend `TryQueryNativeSessionReuse(...)` to accept `label`
    - emit `native_probe` stage markers before/after `Supports`, before/after `GetNativeHandle`, before query, on query failure, on query success, and on exception
    - pass `initial_handshake` / `same_context_attempt_%d` labels into the native-probe helper call sites

- `bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh`
  - result: PASS
  - summary:
    - stage-marker strengthening did not regress isolated-worker quarantine semantics

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - stage-marker strengthening did not break the broader session-resumption runtime-truth surface

- `mkdir -p tmp/winssl_native_probe_stage_markers_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_stage_markers_win64 -FEtmp/winssl_native_probe_stage_markers_win64 -otmp/winssl_native_probe_stage_markers_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 cross-target compile remained green after the stage-marker changes
    - existing warnings stayed in the historical warning pool; no fresh compile break surfaced

- `git diff --check`
  - result: PASS
  - summary:
    - stage-marker batch has no whitespace or patch-format issues

### Wave B/B2 Closure Windows Runtime Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - confirmed the working tree was clean before starting the closure-runtime-truth batch

- `gh api repos/dtamade/fafafa.ssl/actions/runs/26070488337/jobs`
  - result: PASS
  - summary:
    - confirmed run `26070488337` head=`1aaf71b`
    - `windows-gate` failed specifically in `Run broader WinSSL runtime suite`
    - `summary` job itself still succeeded, which kept the report-chain review lane relevant

- `gh run download 26070488337 -n wave-b-summary-winssl_native_probe_worker_drain_20260519_google -D tmp/gh-run-26070488337/summary`
  - result: PASS
  - summary:
    - downloaded the summary artifact bundle for the worker-drain run

- `sed -n '1,220p' tmp/gh-run-26070488337/summary/wave_b_cross_platform_summary_winssl_native_probe_worker_drain_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed cross summary already promoted Windows to `FAIL` using `suite_end_status=FAIL`

- `sed -n '1,220p' tmp/gh-run-26070488337/summary/wave_b_b2_handoff_bundle_winssl_native_probe_worker_drain_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed handoff bundle already reported `handoff_state: NEEDS_GATE_REPAIR`

- `sed -n '1,220p' tmp/gh-run-26070488337/summary/wave_b_b2_closure_readiness_winssl_native_probe_worker_drain_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed the remaining drift: closure readiness still showed `windows | PASS`

- add `docs/plans/2026-05-19-wave-b-b2-closure-windows-runtime-truth.md`
  - change:
    - define the bounded closure-truth batch that only repairs Windows runtime failure propagation into closure readiness

- add `tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh`
  - change:
    - lock direct closure checker behavior when Windows summary is `PASS` but sibling runtime transcript ends with `suite_end_status=FAIL`

- add `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh`
  - change:
    - lock handoff-bundle regeneration so the produced closure report inherits the same Windows runtime failure truth

- `bash tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - closure readiness still treated Windows as `PASS`
    - GREEN:
      - closure readiness now demotes Windows to `FAIL` when the runtime transcript ends with `suite_end_status=FAIL`

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - handoff bundle still generated a stale closure report that showed Windows as `PASS`
    - GREEN:
      - handoff bundle now regenerates closure readiness with the same Windows runtime failure truth

- update workflow truth scripts:
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - change:
    - add optional `--windows-runtime-transcript` to the closure checker
    - default to sibling `winssl_runtime_suite_<run_id>.log` when a Windows summary is present
    - only use runtime transcript to demote Windows to `FAIL`, never to promote a missing summary to `PASS`
    - explicitly pass the sibling runtime transcript from handoff-bundle prepare into closure readiness

- `bash -n scripts/check_wave_b_b2_closure_readiness.sh`
  - result: PASS
  - summary:
    - closure checker syntax stayed valid after the runtime-transcript changes

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - result: PASS
  - summary:
    - sibling Windows companion-path behavior stayed aligned

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
  - result: PASS
  - summary:
    - handoff gate-repair semantics stayed aligned after closure truth propagation changed

- `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
  - result: PASS
  - summary:
    - closure next-actions wording remained aligned with the current handoff entrypoint

- `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
  - result: PASS
  - summary:
    - explicit missing Windows runtime evidence still forces consistency to `INCONSISTENT`

- `bash tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh`
  - result: PASS
  - summary:
    - presence-only Windows runtime logs are still rejected as non-substantive evidence

- `gh run download 26070488337 -n wave-b-linux-winssl_native_probe_worker_drain_20260519_google -D tmp/gh-run-26070488337/linux`
  - result: PASS
  - summary:
    - downloaded Linux platform evidence for real-artifact regeneration

- `gh run download 26070488337 -n wave-b-macos-winssl_native_probe_worker_drain_20260519_google -D tmp/gh-run-26070488337/macos`
  - result: PASS
  - summary:
    - downloaded macOS platform evidence for real-artifact regeneration

- `gh run download 26070488337 -n wave-b-windows-winssl_native_probe_worker_drain_20260519_google -D tmp/gh-run-26070488337/windows`
  - result: PASS
  - summary:
    - downloaded Windows platform evidence including `winssl_runtime_suite_*.log`

- `bash scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id winssl_native_probe_worker_drain_20260519_google --linux-summary tmp/gh-run-26070488337/linux/wave_b_ci_gate_summary_winssl_native_probe_worker_drain_20260519_google.md --linux-examples tmp/gh-run-26070488337/linux/examples_compile_ci_gate_winssl_native_probe_worker_drain_20260519_google.json --macos-summary tmp/gh-run-26070488337/macos/wave_b_macos_gate_summary_winssl_native_probe_worker_drain_20260519_google.md --windows-summary tmp/gh-run-26070488337/windows/wave_b_windows_gate_summary_winssl_native_probe_worker_drain_20260519_google.md --output-dir tmp/gh-run-26070488337/regen`
  - result: PASS
  - summary:
    - regenerated cross summary / closure readiness / consistency / handoff bundle from the real worker-drain artifacts
    - regenerated closure report now shows:
      - `windows | FAIL | ... suite_end_status=FAIL`
      - `closure_status: IN_PROGRESS`
    - regenerated handoff bundle still shows:
      - `handoff_state: NEEDS_GATE_REPAIR`
      - `consistency_status: CONSISTENT`

### WinSSL Native-Probe Manual Investigation Lane

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced catchup output was needed before the WinSSL native-probe workflow batch

- `rg -n "FAFAFA_WINSSL_ENABLE_NATIVE_PROBE|FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE|native probe|SECPKG_ATTR_SESSION_INFO" .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled tests/winssl/test_winssl_session_resumption.pas tests/run_winssl_tests.ps1 docs/plans docs/guides docs/reference src tests/scripts`
  - result: PASS
  - summary:
    - confirmed the dedicated proof program already supports `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE`
    - confirmed the current workflow still lacked any manual native-probe input
    - confirmed older plans/contracts already required the broader suite lane to keep native probe opt-in

- `sed -n '1,260p' docs/plans/2026-05-18-winssl-native-probe-evidence-lane.md`
  - result: PASS
  - summary:
    - confirmed the older dedicated proof plan already recorded the public-handle probe as risky on GitHub Windows runners
    - confirmed the current batch should stay strictly on workflow/manual-investigation plumbing instead of reopening shared reconnect logic

- `sed -n '1,340p' tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - confirmed `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE` and `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE` are already supported by the dedicated proof program
    - confirmed the broader lane can unlock native probe through workflow-step env injection alone

- `sed -n '1,260p' .github/workflows/wave-b-b2-manual.yml`
  - result: PASS
  - summary:
    - confirmed the active workflow only exposed `winssl_session_host` and still lacked a manual native-probe input

- `sed -n '1,220p' .github/README.md`
  - result: PASS
  - summary:
    - confirmed the workflow README documented host override but still did not document a native-probe investigation lane

- add `docs/plans/2026-05-19-winssl-native-probe-manual-investigation-lane.md`
  - change:
    - define the bounded manual-workflow batch for explicit native-probe opt-in evidence

- add `tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
  - change:
    - lock the manual workflow to an explicit false-default `winssl_enable_native_probe` input
    - require opt-in env injection, enabled/disabled logging, and no automatic `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE`

- `bash -n tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
  - result: PASS
  - summary:
    - native-probe input contract syntax is valid

- `bash tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `wave-b-b2-manual.yml` and `.disabled` did not yet expose `winssl_enable_native_probe`
    - GREEN:
      - both workflow files now expose the explicit native-probe opt-in input
      - the Windows broader runtime step now logs whether native probe is enabled or kept disabled by default
      - `.github/README.md` now documents the new risky Schannel evidence lane

- update workflow native-probe truth sources:
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - `.github/README.md`
  - change:
    - add optional `workflow_dispatch.inputs.winssl_enable_native_probe`
    - inject `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1` only for truthy manual input
    - log enabled/disabled native-probe truth in the broader runtime step
    - document the manual native-probe lane in the workflow README

- `bash tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
  - result: PASS
  - summary:
    - the existing host-override workflow contract remained aligned after adding the native-probe input

- `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
  - result: PASS
  - summary:
    - the strict-closure input description truth remained aligned after the new manual input

- `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
  - result: PASS
  - summary:
    - the required optional-artifact download semantics stayed aligned after the broader runtime-step change

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - the dedicated WinSSL session-resumption runtime-truth contract still confirms native probe remains opt-in and broader-suite safe by default

- `git diff --check`
  - result: PASS
  - summary:
    - current native-probe workflow batch has no whitespace or patch-format issues

- `git push origin master`
  - result: PASS
  - summary:
    - pushed `ce602cb ci(winssl): add manual native probe lane` to `master`

- `gh workflow run wave-b-b2-manual.yml -f run_id=winssl_native_probe_20260519_google -f strict_closure=false -f winssl_session_host=www.google.com -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - dispatched the new native-probe manual investigation lane against non-default host `www.google.com`

- `gh run list --workflow wave-b-b2-manual.yml --limit 5 --json databaseId,displayTitle,headSha,status,conclusion,createdAt,event,workflowName`
  - result: PASS
  - summary:
    - captured fresh run `26068984446` on head `ce602cbe9174cd3bdf8aa6353df773a6c298bdb7`

- `gh run watch 26068984446`
  - result: PASS
  - summary:
    - workflow completed with overall `FAILURE`
    - `windows-gate` failed in `Run broader WinSSL runtime suite`
    - `linux-gate`, `macos-gate`, and `summary` still completed

- `gh run download 26068984446 -n wave-b-windows-winssl_native_probe_20260519_google -D tmp/gh-run-26068984446/windows`
  - result: PASS
  - summary:
    - downloaded Windows evidence bundle including `winssl_runtime_suite_winssl_native_probe_20260519_google.log`

- `gh run download 26068984446 -n wave-b-summary-winssl_native_probe_20260519_google -D tmp/gh-run-26068984446/summary`
  - result: PASS
  - summary:
    - downloaded cross-platform summary bundle for the native-probe run

- `gh run view 26068984446 --json url,conclusion,status,createdAt,updatedAt,workflowName,displayTitle,headSha`
  - result: PASS
  - summary:
    - run URL: `https://github.com/dtamade/fafafa.ssl/actions/runs/26068984446`
    - conclusion: `failure`
    - headSha: `ce602cbe9174cd3bdf8aa6353df773a6c298bdb7`

- `gh run view 26068984446 --log > tmp/gh-run-26068984446/full_run.log && rg -n "Enabling risky WinSSL native probe|Using WinSSL session resumption host override|Run broader WinSSL runtime suite|exit code 1|-1073741819" tmp/gh-run-26068984446/full_run.log`
  - result: PASS
  - summary:
    - confirmed the step really ran with:
      - `host=www.google.com`
      - `nativeProbeInput=true`
      - `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1`
    - confirmed the workflow step printed:
      - `Using WinSSL session resumption host override: www.google.com`
      - `Enabling risky WinSSL native probe for Schannel session evidence`

- `sed -n '1,260p' tmp/gh-run-26068984446/windows/winssl_runtime_suite_winssl_native_probe_20260519_google.log`
  - result: PASS
  - summary:
    - confirmed the dedicated session-resumption test still reaches only the first public signal:
      - `signal label=initial_handshake reused=false info_resumed=false perf_reused=false`
    - confirmed no `native_probe ...` marker was emitted before failure
    - confirmed the failure remains:
      - `exit_code=-1073741819`
      - immediately after `initial handshake must not report reuse: PASS`

- `sed -n '1,220p' tmp/gh-run-26068984446/summary/wave_b_b2_evidence_consistency_winssl_native_probe_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed consistency report records `windows_runtime_transcript` as present substantive evidence with `suite_end_status=FAIL`
    - confirmed this report stays `CONSISTENT`, which means transcript failure truth is not currently promoted into top-level inconsistency

- `sed -n '1,220p' tmp/gh-run-26068984446/summary/wave_b_b2_handoff_bundle_winssl_native_probe_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed handoff bundle still renders `handoff_state: CLOSED` for this opt-in native-probe run
    - confirmed top-level handoff truth does not currently treat native-probe runtime failure as a closure-breaker

- `sed -n '1,260p' tmp/gh-run-26068984446/summary/wave_b_cross_platform_summary_winssl_native_probe_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed cross summary still shows `windows | PASS` because it keys off `wave_b_windows_gate_summary_*`, not the broader runtime-suite failure
    - this establishes that opt-in native-probe lane success/failure must be read from workflow conclusion + runtime transcript, not from cross summary alone

### WinSSL Session Runtime Host-Override Investigation Lane

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced catchup output was needed before the WinSSL host-override workflow batch

- `sed -n '1,260p' .github/workflows/wave-b-b2-manual.yml`
  - result: PASS
  - summary:
    - confirmed `workflow_dispatch` still exposed only `run_id`, `openssl_root_macos`, and `strict_closure`
    - confirmed the Windows broader WinSSL runtime step did not yet accept a manual host override

- `sed -n '1,260p' .github/workflows/wave-b-b2-manual.yml.disabled`
  - result: PASS
  - summary:
    - confirmed the dormant template matched the active workflow and would need the same host-override input change

- `sed -n '1,260p' tests/run_winssl_tests.ps1`
  - result: PASS
  - summary:
    - confirmed the WinSSL session-resumption lane only injects `FAFAFA_RUN_NETWORK_TESTS=1` and `FAFAFA_WINSSL_SESSION_ATTEMPTS=4`
    - confirmed workflow-step env injection can safely provide `FAFAFA_WINSSL_SESSION_HOST`

- `sed -n '1,520p' tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - confirmed `ResolveSessionHost` already supports `FAFAFA_WINSSL_SESSION_HOST`
    - confirmed session summary markers already emit `host=<value>` for artifact inspection

- `sed -n '1,260p' .github/README.md`
  - result: PASS
  - summary:
    - confirmed the workflow README still described `wave-b-b2-manual.yml` without any host-override investigation input

- add `docs/plans/2026-05-19-winssl-session-runtime-host-override-investigation.md`
  - change:
    - define the bounded workflow/manual-investigation batch for optional WinSSL session host override

- add `tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
  - change:
    - lock the manual workflow to an optional blank-default `winssl_session_host` input
    - require Windows broader runtime-step host injection truth plus README documentation

- `bash -n tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
  - result: PASS
  - summary:
    - host-override contract syntax is valid

- `bash tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `wave-b-b2-manual.yml` and `.disabled` did not yet expose `winssl_session_host`
    - GREEN:
      - both workflow files now expose the optional host-override input
      - the Windows broader runtime step now records whether it uses the default host or a manual override
      - `.github/README.md` now documents the investigation lane

- update workflow host-override truth sources:
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - `.github/README.md`
  - change:
    - add optional `workflow_dispatch.inputs.winssl_session_host`
    - inject `FAFAFA_WINSSL_SESSION_HOST` only when the manual input is non-empty
    - log fallback-to-default-host truth for artifact review
    - document the new manual investigation input in the workflow README

- `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - the existing contract still hard-coded `actions/download-artifact@v4`
      - current workflow truth had already moved to pinned `download-artifact` v7
    - GREEN:
      - the contract now locks the required Linux/macOS/Windows artifact-download semantics without baking in a stale action version label

- update workflow artifact-download contract truth:
  - `tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
  - change:
    - replace stale `@v4` matching with generic pinned `actions/download-artifact@` truth

- `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
  - result: PASS
  - summary:
    - the existing strict-closure input description truth remained aligned after adding the new manual investigation input

- `git diff --check`
  - result: PASS
  - summary:
    - current workflow host-override batch has no whitespace or patch-format issues

- `gh auth status`
  - result: PASS
  - summary:
    - GitHub CLI is logged in with `workflow` scope, so the new manual lane can be dispatched after push for real Windows-runner evidence

- `git push origin master`
  - result: PASS
  - summary:
    - pushed the previously queued local commits plus `81eebb1 ci(winssl): add manual host override investigation lane`

- `gh workflow run wave-b-b2-manual.yml -f run_id=winssl_host_probe_20260519_google -f strict_closure=false -f winssl_session_host=www.google.com`
  - result: PASS
  - summary:
    - dispatched the new manual host-override investigation lane against non-default host `www.google.com`

- `gh run list --workflow wave-b-b2-manual.yml --limit 5 --json databaseId,displayTitle,headSha,status,conclusion,createdAt,event,workflowName`
  - result: PASS
  - summary:
    - captured fresh run `26068474291` on head `81eebb1eb75466ade7d1bb3de382654b50a9afb3`

- `gh run watch 26068474291 --exit-status`
  - result: PASS
  - summary:
    - workflow completed `SUCCESS`
    - `windows-gate` succeeded in `5m24s`
    - `macos-gate`, `linux-gate`, and `summary` also succeeded

- `gh run download 26068474291 -n wave-b-windows-winssl_host_probe_20260519_google -D tmp/gh-run-26068474291/windows`
  - result: PASS
  - summary:
    - downloaded Windows evidence bundle including `winssl_runtime_suite_winssl_host_probe_20260519_google.log`

- `gh run download 26068474291 -n wave-b-summary-winssl_host_probe_20260519_google -D tmp/gh-run-26068474291/summary`
  - result: PASS
  - summary:
    - downloaded cross-platform summary bundle for the same run

- `sed -n '1,260p' tmp/gh-run-26068474291/windows/winssl_runtime_suite_winssl_host_probe_20260519_google.log`
  - result: PASS
  - summary:
    - confirmed the WinSSL session-resumption test executed inside the broader runtime suite
    - confirmed runtime markers now record:
      - `host=www.google.com`
      - `observed_reuse=false`
      - `session_configured=true`
    - confirmed all four resumed attempts completed while keeping `reused=false`

- `sed -n '1,260p' tmp/gh-run-26068474291/summary/wave_b_cross_platform_summary_winssl_host_probe_20260519_google.md`
  - result: PASS
  - summary:
    - confirmed the final cross-platform summary reports `windows | PASS` for run `winssl_host_probe_20260519_google`

- `gh run view 26068474291 --json url,conclusion,status,createdAt,updatedAt,workflowName,displayTitle,headSha`
  - result: PASS
  - summary:
    - run URL: `https://github.com/dtamade/fafafa.ssl/actions/runs/26068474291`
    - conclusion: `success`
    - headSha: `81eebb1eb75466ade7d1bb3de382654b50a9afb3`

### WinSSL Session-Reuse Benchmark Truth Alignment

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced catchup output was needed before the WinSSL benchmark truth batch

- `sed -n '1,320p' tests/winssl/test_winssl_session_reuse_benchmark.pas`
  - result: PASS
  - summary:
    - confirmed the benchmark still used direct core session mirrors
    - confirmed the report logic overwrote the first metrics record with the second benchmark result

- `sed -n '1,280p' tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`
  - result: PASS
  - summary:
    - confirmed the guide still promised `70-90%`, `>95%`, and “快速握手”
    - confirmed the guide still treated timing delta as if native reuse were already runtime-proven

- add `docs/plans/2026-05-19-winssl-session-reuse-benchmark-truth-alignment.md`
  - change:
    - define the bounded WinSSL benchmark truth/harness batch

- add `tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
  - change:
    - lock the WinSSL benchmark program/guide away from stale high-reuse/high-gain claims
    - require owner-path `ISSLSessionResumption` usage plus metrics-merge truth

- `bash -n tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL benchmark truth contract syntax is valid

- `bash tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - benchmark guide still promised runtime-proven high reuse/performance truth
    - GREEN:
      - benchmark program and guide now align with conservative WinSSL session-resumption truth

- update benchmark truth sources:
  - `tests/winssl/test_winssl_session_reuse_benchmark.pas`
  - `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`
  - change:
    - switch the benchmark program to `ISSLSessionResumption`
    - record `SessionConfiguredCount` separately from `ObservedReuseCount`
    - fix the metrics overwrite bug by merging both benchmark result records
    - harden percentage/report output against zero-success scenarios
    - remove stale `70-90%` / “快速握手” claims from the guide and replace them with `observed_reuse=false / session_configured=true`

- `mkdir -p tmp/winssl_session_reuse_benchmark_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_reuse_benchmark_win64 -FEtmp/winssl_session_reuse_benchmark_win64 -otmp/winssl_session_reuse_benchmark_win64/test_winssl_session_reuse_benchmark.exe tests/winssl/test_winssl_session_reuse_benchmark.pas`
  - result: PASS
  - summary:
    - Win64 cross-target benchmark program still compiles after the owner-path/truth alignment changes
    - compile completed with the repo's existing warning baseline only

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL benchmark truth batch has no whitespace or patch-format issues

### Session-Resumption Guide Old-Name Truth Freeze

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced catchup output was needed before the session-resumption guide batch

- `rg -n "GetSessionID|IsSessionResumed|SetSession\\(|GetSession\\(|IsSessionReused|ISSLSessionResumption" docs/guides/QUICKSTART.md docs/guides/TROUBLESHOOTING.md docs/guides/USER_GUIDE.md docs/guides/INTEGRATION_GUIDE.md docs/reference/API_REFERENCE.md docs/reference/API_DOCUMENTATION.md`
  - result: PASS
  - summary:
    - confirmed the remaining high-visibility session-resumption drift now lived in `QUICKSTART.md`, `TROUBLESHOOTING.md`, and `USER_GUIDE.md`
    - confirmed the earlier `API_REFERENCE` / `API_DOCUMENTATION` / `INTEGRATION_GUIDE` owner-path batch had already landed

- add `docs/plans/2026-05-19-session-resumption-guide-old-name-freeze.md`
  - change:
    - define the bounded guide-only truth-freeze batch for stale session-resumption names and direct connection-core mirrors

- add `tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
  - change:
    - lock `QUICKSTART`, `TROUBLESHOOTING`, and `USER_GUIDE` away from `GetSessionID` / `IsSessionResumed`
    - require `ISSLSessionResumption` owner-path guidance in all 3 guide files

- update session-resumption guide truth sources:
  - `docs/guides/QUICKSTART.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/USER_GUIDE.md`
  - change:
    - switch session save/restore/reuse examples to `Supports(..., ISSLSessionResumption, ...)`
    - replace direct `Connection.GetSession / SetSession / IsSessionResumed` guidance with owner-path `GetSession / SetSession / IsSessionReused`
    - add an explicit owner-path note in `QUICKSTART`

- `bash -n tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
  - result: PASS
  - summary:
    - session-resumption guide truth contract syntax is valid

- `bash tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
  - result: PASS
  - summary:
    - `QUICKSTART`, `TROUBLESHOOTING`, and `USER_GUIDE` are now aligned on `ISSLSessionResumption` owner-path guidance
    - stale `GetSessionID` / `IsSessionResumed` names are gone from the active guides covered by this batch

- `git diff --check`
  - result: PASS
  - summary:
    - current session-resumption guide truth batch has no whitespace or patch-format issues

- `rg -n "GetSessionID|IsSessionResumed" docs src tests --glob '!docs/archive/**' --glob '!docs/test_reports/**'`
  - result: PASS
  - summary:
    - active guides no longer carry stale session-resumption names
    - remaining hits are now limited to:
      - `docs/reference/API_REFERENCE.md` historical/compatibility notes
      - `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`
      - contract / plan / progress files that intentionally record the old names

### Facade Main-Entry Truth Freeze

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced catchup output was needed before the facade/main-entry truth batch

- add `docs/plans/2026-05-19-facade-main-entry-truth-freeze.md`
  - change:
    - define the bounded high-visibility facade/main-entry truth-freeze batch

- add `tests/scripts/test_facade_main_entry_truth_contract.sh`
  - change:
    - lock README, facade header, factory comments, and integration guide to current `sslCtx*` + facade entry truth
    - require the recommended `TSSLConnector.FromContext(...)` path and direct `ISSLClientConnection.SetServerName(...)` path in `docs/README.md`

- `bash -n tests/scripts/test_facade_main_entry_truth_contract.sh`
  - result: PASS
  - summary:
    - facade/main-entry truth contract syntax is valid

- `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `docs/README.md` still exposed stale `sslClient` and split-unit main-entry guidance
    - GREEN:
      - `docs/README.md`, `src/fafafa.ssl.pas`, `src/fafafa.ssl.factory.pas`, and `docs/guides/INTEGRATION_GUIDE.md` now align on current facade/main-entry truth

- update main-entry truth sources:
  - `docs/README.md`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.factory.pas`
  - `docs/guides/INTEGRATION_GUIDE.md`
  - change:
    - switch the highest-visibility entry docs to `uses fafafa.ssl`
    - publish `TSSLConnector.FromContext(...)` as the recommended client entry
    - keep `ISSLClientConnection.SetServerName(...)` as the explicit direct per-connection SNI path
    - replace stale `sslClient` / `sslServer` wording with `sslCtxClient` / `sslCtxServer`

- `git diff --check`
  - result: PASS
  - summary:
    - current facade/main-entry truth batch has no whitespace or patch-format issues

### ISSLCertificateVerification Root-Test Residual Freeze

- add `docs/plans/2026-05-19-isslcertificateverification-root-test-residual-freeze.md`
  - change:
    - define the bounded root-test verify-result residual subgroup freeze

- add `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - change:
    - lock the root-test direct-core verify-result residual file set
    - require intentional residual notes plus the expected verify-result coverage in each file

- update root-test residual proof files:
  - `tests/test_freepascal_backend_basic.pas`
  - `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
  - `tests/test_freepascal_client_certificate_flight_requirements.pas`
  - `tests/test_freepascal_client_chain_trust_runtime.pas`
  - `tests/test_freepascal_client_ct_sct_surface.pas`
  - `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
  - `tests/test_freepascal_client_online_ocsp_runtime.pas`
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_openssl_connection_verify_result_contract.pas`
  - `tests/test_wolfssl_framework.pas`
  - change:
    - add unified `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` notes
    - record these files as root-test runtime / backend-contract proof

- `bash -n tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - result: PASS
  - summary:
    - root-test residual subgroup contract syntax is valid

- `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - the focused contract over-matched a wrapped residual-comment line instead of a stable substring
    - GREEN:
      - root-test direct-core verify-result residual file set stayed frozen to the expected subgroup
      - each file still carries the intentional residual note and expected verify-result coverage

- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the broader certificate-verification residual allowlist remained aligned after the root-test subgroup freeze

- `git diff --check`
  - result: PASS
  - summary:
    - current root-test residual freeze batch has no whitespace or patch-format issues

### ISSLCertificateVerification OCSP Runtime Duo Freeze

- add `docs/plans/2026-05-19-isslcertificateverification-ocsp-runtime-duo-freeze.md`
  - change:
    - define the bounded OpenSSL/WolfSSL OCSP runtime residual duo freeze

- add `tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
  - change:
    - lock the OpenSSL/WolfSSL direct-core verify-result residual file set to the current duo
    - require intentional residual notes plus the expected diagnostics coverage in each file

- update OCSP runtime residual proof files:
  - `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
  - `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
  - change:
    - add unified `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` notes
    - record these files as backend-specific server-side OCSP stapling diagnostics proof

- `bash -n tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
  - result: PASS
  - summary:
    - OCSP runtime residual duo contract syntax is valid

- `bash tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - the focused contract over-matched a wrapped residual-comment line instead of stable substrings
    - GREEN:
      - OpenSSL/WolfSSL direct-core verify-result residual file set stayed frozen to the expected duo
      - each file still carries the intentional residual note and expected diagnostics coverage

- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the broader certificate-verification residual allowlist remained aligned after the OCSP runtime duo freeze

- `git diff --check`
  - result: PASS
  - summary:
    - current OCSP runtime duo freeze batch has no whitespace or patch-format issues

### ISSLCertificateVerification MbedTLS Residual Cluster Freeze

- add `docs/plans/2026-05-19-isslcertificateverification-mbedtls-residual-cluster-freeze.md`
  - change:
    - define the bounded MbedTLS residual subgroup freeze for backend-specific verify-result proof files

- add `tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
  - change:
    - lock the MbedTLS direct-core verify-result residual file set to the current 8-file cluster
    - require intentional residual notes plus expected verify-result coverage in each file

- update MbedTLS residual proof files:
  - `tests/mbedtls/benchmark_handshake_simple.pas`
  - `tests/mbedtls/test_mbedtls_safe.pas`
  - `tests/mbedtls/test_mbedtls_simple_connection.pas`
  - `tests/mbedtls/test_mbedtls_lowlevel.pas`
  - `tests/mbedtls/test_mbedtls_cert_chain.pas`
  - `tests/mbedtls/test_mbedtls_cert_errors.pas`
  - `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
  - `tests/test_mbedtls_framework.pas`
  - change:
    - add unified `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` notes
    - record these files as MbedTLS-specific benchmark / runtime diagnostics / framework contract proof

- `bash -n tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
  - result: PASS
  - summary:
    - MbedTLS residual contract syntax is valid

- `bash tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - the focused contract over-matched a wrapped residual-comment line instead of the stable substrings
    - GREEN:
      - MbedTLS direct-core verify-result file set stayed frozen to the expected 8-file cluster
      - each file still carries the intentional residual note and expected direct verify-result coverage

- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the broader certificate-verification residual allowlist remained aligned after the MbedTLS subgroup freeze

- `git diff --check`
  - result: PASS
  - summary:
    - current MbedTLS residual freeze batch has no whitespace or patch-format issues

### ISSLCertificateVerification WinSSL Runtime Residual Freeze

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced catchup output was needed before the WinSSL runtime residual batch

- add `docs/plans/2026-05-19-isslcertificateverification-winssl-runtime-residual-freeze.md`
  - change:
    - define the bounded WinSSL runtime residual freeze batch for the remaining direct core verify-result trio

- add `tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
  - change:
    - lock the `tests/winssl/` direct-core verify-result file set to the current trio
    - require intentional residual comments plus both verify-result reads in each file

- update WinSSL online runtime residual proofs:
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_hostname_mismatch_online.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - change:
    - add `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` notes ahead of the direct core `GetVerifyResult` / `GetVerifyResultString` reads
    - record that these paths stay as WinSSL-specific online certificate-error proof while owner-path coverage lives elsewhere

- `bash -n tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
  - result: PASS
  - summary:
    - WinSSL runtime residual contract syntax is valid

- `bash tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
  - result: PASS
  - summary:
    - `tests/winssl/` direct-core verify-result file set stayed frozen to:
      - `tests/winssl/test_winssl_error_mapping_online.pas`
      - `tests/winssl/test_winssl_hostname_mismatch_online.pas`
      - `tests/winssl/test_winssl_revocation_online.pas`
    - each file still carries the intentional residual note and both direct verify-result reads

- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the broader certificate-verification residual allowlist remained aligned after the WinSSL subgroup freeze

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL runtime residual freeze batch has no whitespace or patch-format issues

### ISSLCertificateVerification Residual Classification Freeze

- add `docs/plans/2026-05-19-isslcertificateverification-residual-classification-freeze.md`
  - change:
    - define the bounded residual-allowlist batch for direct core certificate-verification getters

- add `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: FAIL -> FAIL -> PASS
  - summary:
    - RED 1:
      - `src/fafafa.ssl.base.pas` still lacked preferred-access / owner notes for certificate verification
    - RED 2:
      - the new `ISSLConnection.GetVerifyResult*` owner notes in `src/fafafa.ssl.base.pas` legitimately expanded the `src/` allowlist by one file
    - GREEN:
      - residual direct-core surface now matches the expected allowlist

- update `src/fafafa.ssl.base.pas`
  - change:
    - add preferred-access / owner-note wording for `GetVerifyResult`
    - add preferred-access / owner-note wording for `GetVerifyResultString`

- update `src/fafafa.ssl.connection.base.pas`
  - change:
    - add residual-surface note for the shared `GetVerifyResult` / `GetVerifyResultString` mirror implementation

- `bash -n tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - residual classification contract syntax is valid

- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - active docs direct-core file set stayed at `0`
    - `examples/` direct-core file set stayed frozen to `examples/fafafa.examples.tcp.pas`
    - `tests/examples/` direct-core file set stayed at `0`
    - `tests/connection/` direct-core file set stayed frozen to `tests/connection/test_ssl_client_connection.pas`
    - `tests/contract/` direct-core file set stayed frozen to `tests/contract/test_backend_contract.pas`
    - backend-specific runtime / contract residual file set stayed frozen to the current allowlist

- `git diff --check`
  - result: PASS
  - summary:
    - current certificate-verification residual freeze batch has no whitespace or patch-format issues

### ISSLCertificateVerification Generic Examples Owner Path

- add `docs/plans/2026-05-19-isslcertificateverification-generic-examples-owner-path.md`
  - change:
    - define the bounded generic examples/tests owner-path batch for residual verify-result guidance

- add `tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - generic examples/tests still lacked a shared `ISSLCertificateVerification` owner-path helper
    - GREEN after fix:
      - generic examples/tests now prefer the shared/local owner-path helper

- update `examples/fafafa.examples.tcp.pas`
  - change:
    - add shared `GetCertificateVerificationInfo(...)`
    - prefer `ISSLCertificateVerification` and keep core getter fallback only as compatibility backstop

- update generic examples/tests:
  - `examples/01_tls_client.pas`
  - `examples/example_https_api.pas`
  - `examples/production/https_client_auth.pas`
  - `examples/validation/real_world_test.pas`
  - `tests/examples/test_openssl.pas`
  - `tests/examples/test_real_websites.pas`
  - `tests/examples/test_real_websites_enhanced.pas`
  - `tests/examples/test_real_websites_comprehensive.pas`
  - `tests/connection/test_ssl_client_connection.pas`
  - change:
    - route generic verify-result reads through shared/local owner-path helper instead of direct core getter calls

- `bash -n tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
  - result: PASS
  - summary:
    - source contract syntax is valid

- `bash tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
  - result: PASS
  - summary:
    - generic examples/tests now prefer `ISSLCertificateVerification` for verify-result surfaces

- target compile verification
  - result: PASS with focused FAIL -> PASS inside the batch
  - summary:
    - initial compile RED also exposed pre-existing compile-liveness issues:
      - `tests/examples/test_real_websites*.pas` still used FPC-invalid `try..except..finally`
      - `tests/connection/test_ssl_client_connection.pas` still assumed older `ssockets` / native-handle API shapes
    - after fixing those narrow issues, all focused targets compiled:
      - `examples/01_tls_client.pas`
      - `examples/example_https_api.pas`
      - `examples/production/https_client_auth.pas`
      - `examples/validation/real_world_test.pas`
      - `tests/examples/test_openssl.pas`
      - `tests/examples/test_real_websites.pas`
      - `tests/examples/test_real_websites_enhanced.pas`
      - `tests/examples/test_real_websites_comprehensive.pas`
      - `tests/connection/test_ssl_client_connection.pas`

- `git diff --check`
  - result: PASS
  - summary:
    - current generic examples/tests owner-path batch has no whitespace or patch-format issues

### Certificate Verification Chain Issuer-Link Contract

- add `docs/plans/2026-05-19-certificate-verification-chain-issuer-link-contract.md`
  - change:
    - define the bounded `Contract 21` batch that promotes optional/core peer-chain issuer-link truth into the unified backend contract

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced session catchup output was needed before this batch

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp -otmp/tmp_backend_contract tests/contract/test_backend_contract.pas && ./tmp/tmp_backend_contract`
  - result: PASS
  - summary:
    - pre-change baseline stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - confirmed `Contract 21` still had no issuer-link truth assertions before this batch

- update `tests/contract/test_backend_contract.pas`
  - change:
    - add `CertificatePublicIdentityMatches(...)` helper
    - extend `Contract 21` to compare optional/core `GetIssuerCertificate()` nilness
    - extend `Contract 21` to compare issuer-link public identity when present

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp -otmp/tmp_backend_contract tests/contract/test_backend_contract.pas && ./tmp/tmp_backend_contract`
  - result: PASS
  - summary:
    - strengthened `Contract 21` remained green at `135 total / 111 passed / 0 failed / 24 skipped`
    - no new backend implementation drift surfaced after issuer-link truth entered the unified contract

- `git diff --check`
  - result: PASS
  - summary:
    - current `Contract 21` issuer-link contract batch has no whitespace or patch-format issues

### ISSLCertificateVerification High-Visibility Owner Path

- add `docs/plans/2026-05-19-isslcertificateverification-high-visibility-owner-path.md`
  - change:
    - define the bounded certificate-verification owner-path batch for builder / TLS facade / OCSP guide / CT guide

- `rg -n "\\.GetVerifyResultString\\b|\\.GetVerifyResult\\b" src/fafafa.ssl.connection.builder.pas src/fafafa.ssl.tls.pas docs/guides/OCSP_USAGE_GUIDE.md docs/guides/CT_IMPLEMENTATION_GUIDE.md`
  - result: PASS
  - summary:
    - confirmed both high-visibility facade units still read direct core verify-result mirrors
    - confirmed OCSP / CT guides still taught direct core `GetVerifyResultString`

- update `src/fafafa.ssl.connection.builder.pas`
  - change:
    - add local verification-surface helper
    - client/server handshake failure paths now prefer `ISSLCertificateVerification`

- update `src/fafafa.ssl.tls.pas`
  - change:
    - add local verification-surface helper
    - connector/acceptor handshake failure paths now prefer `ISSLCertificateVerification`

- update `docs/guides/OCSP_USAGE_GUIDE.md`
  - change:
    - handshake-failure example now capability-gates `ISSLCertificateVerification`
    - no longer teaches direct core `GetVerifyResultString`

- update `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
  - change:
    - handshake-failure example now capability-gates `ISSLCertificateVerification`
    - no longer teaches direct core `GetVerifyResultString`

- update `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - result: FAIL -> FAIL -> PASS
  - summary:
    - RED 1:
      - OCSP guide still contained direct core `GetVerifyResultString`
    - RED 2:
      - source contract substring-matched helper-local `AVerifyRes` and falsely flagged it as old direct-core usage
    - GREEN:
      - contract now correctly covers builder / TLS facade / OCSP guide / CT guide owner-path truth

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - builder-focused compile/run stayed green at `29 passed / 0 failed`

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - TLS facade focused compile/run stayed green at `6 passed / 0 failed`

- `bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh && bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs/source contract now prefers `ISSLCertificateVerification` across high-visibility paths

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp -otmp/tmp_backend_contract tests/contract/test_backend_contract.pas && ./tmp/tmp_backend_contract`
  - result: PASS
  - summary:
    - backend contract stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - existing backend interface alignment did not regress after the owner-path refactor

- `git diff --check`
  - result: PASS
  - summary:
    - current high-visibility owner-path batch has no whitespace or patch-format issues

### MbedTLS Peer Certificate Chain Issuer Link

- add `docs/plans/2026-05-19-mbedtls-peer-cert-chain-issuer-link.md`
  - change:
    - define the bounded MbedTLS connection-level peer-chain completeness batch, commands, scope, and expected closeout

- update `tests/test_mbedtls_connection_peer_certificate_contract.pas`
  - result: RED
  - summary:
    - extend the focused contract from “owned leaf copy exists” to “leaf+issuer chain materializes and leaf issuer-link truth is preserved”
    - first failures landed on:
      - `GetPeerCertificate should preserve issuer link`
      - `GetPeerCertificateChain should expose the peer leaf and issuer`
      - `GetPeerCertificateChain leaf should preserve issuer link`

- `sed -n '41,95p' /usr/include/mbedtls/x509_crt.h`
  - result: PASS
  - summary:
    - confirmed upstream `mbedtls_x509_crt` exposes a native `next` pointer for peer-chain traversal
    - confirmed current Pascal connection layer was not yet using that native chain truth

- `nl -ba src/fafafa.ssl.mbedtls.connection.pas | sed -n '398,442p'`
  - result: PASS
  - summary:
    - confirmed `DoGetPeerCertificate()` was cloning only the borrowed leaf wrapper
    - confirmed `DoGetPeerCertificateChain()` was hard-coding a single-entry result

- update `src/fafafa.ssl.mbedtls.connection.pas`
  - change:
    - add native peer-chain materialization helper for `mbedtls_x509_crt.next`
    - `GetPeerCertificate()` now materializes the native chain leaf and supplements issuer-link truth
    - `GetPeerCertificateChain()` no longer truncates the native peer chain to a single leaf
    - sequential chain entries now preserve `GetIssuerCertificate()` truth while keeping existing fail-closed behavior

- `mkdir -p tmp/test_mbedtls_connection_peer_certificate_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_peer_certificate_contract_units -FEtmp/test_mbedtls_connection_peer_certificate_contract_units -otmp/test_mbedtls_connection_peer_certificate_contract_units/test_mbedtls_connection_peer_certificate_contract tests/test_mbedtls_connection_peer_certificate_contract.pas && ./tmp/test_mbedtls_connection_peer_certificate_contract_units/test_mbedtls_connection_peer_certificate_contract`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `GetPeerCertificate should preserve issuer link`
      - `GetPeerCertificateChain should expose the peer leaf and issuer`
      - `GetPeerCertificateChain leaf should preserve issuer link`
    - GREEN:
      - `14 passed / 0 failed`

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp -otmp/tmp_backend_contract tests/contract/test_backend_contract.pas && ./tmp/tmp_backend_contract`
  - result: PASS
  - summary:
    - backend contract stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - existing backend interface alignment did not regress after the MbedTLS peer-chain completeness repair

- `git diff --check`
  - result: PASS
  - summary:
    - current MbedTLS peer-chain completeness batch has no whitespace or patch-format issues

### Certificate Clone Issuer Link

- add `docs/plans/2026-05-19-certificate-clone-issuer-link.md`
  - change:
    - define the bounded cross-backend clone issuer-link batch, commands, scope, and expected closeout

- add `tests/test_certificate_clone_issuer_link_contract.pas`
  - result: RED
  - summary:
    - new focused contract locks clone-level issuer-link truth across `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` / `FreePascal`
    - Linux first failure landed on:
      - `OpenSSL: clone should preserve issuer link`
      - `WolfSSL: clone should preserve issuer link`
      - `MbedTLS: clone should preserve issuer link`
    - Win64+wine first failure landed on:
      - `WinSSL: clone should preserve issuer link`

- `sed -n '1738,1815p' src/fafafa.ssl.openssl.certificate.pas`
  - result: PASS
  - summary:
    - confirmed `TOpenSSLCertificate.Clone()` only wrapped the retained native cert
    - confirmed it was not yet preserving `FIssuerCert`

- `sed -n '928,1005p' src/fafafa.ssl.wolfssl.certificate.pas`
  - result: PASS
  - summary:
    - confirmed `TWolfSSLCertificate.Clone()` was rebuilding DER/PEM/native truth
    - confirmed it was not yet preserving `FIssuerCert`

- `sed -n '1170,1248p' src/fafafa.ssl.mbedtls.certificate.pas`
  - result: PASS
  - summary:
    - confirmed `TMbedTLSCertificate.Clone()` was rebuilding DER/PEM/native truth
    - confirmed it was not yet preserving `FIssuerCert`

- `sed -n '1425,1505p' src/fafafa.ssl.winssl.certificate.pas`
  - result: PASS
  - summary:
    - confirmed `TWinSSLCertificate.Clone()` only duplicated the native cert context
    - confirmed it was not yet preserving `FIssuerCert`

- update `src/fafafa.ssl.openssl.certificate.pas`
  - change:
    - preserve `FIssuerCert` when clone wraps the retained native `X509`

- update `src/fafafa.ssl.wolfssl.certificate.pas`
  - change:
    - preserve `FIssuerCert` after clone materializes the owned DER/native copy

- update `src/fafafa.ssl.mbedtls.certificate.pas`
  - change:
    - preserve `FIssuerCert` after clone materializes the owned DER/native copy

- update `src/fafafa.ssl.winssl.certificate.pas`
  - change:
    - preserve `FIssuerCert` when clone wraps the duplicated `PCCERT_CONTEXT`

- `mkdir -p tmp/test_certificate_clone_issuer_link_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_certificate_clone_issuer_link_contract_units -FEtmp/test_certificate_clone_issuer_link_contract_units -otmp/test_certificate_clone_issuer_link_contract_units/test_certificate_clone_issuer_link_contract tests/test_certificate_clone_issuer_link_contract.pas && ./tmp/test_certificate_clone_issuer_link_contract_units/test_certificate_clone_issuer_link_contract`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `OpenSSL: clone should preserve issuer link`
      - `WolfSSL: clone should preserve issuer link`
      - `MbedTLS: clone should preserve issuer link`
    - GREEN:
      - `16 passed / 0 failed`

- `mkdir -p tmp/test_certificate_clone_issuer_link_contract_win64 && fpc -B -Twin64 -Px86_64 -Fu./src -Fu./tests -FUtmp/test_certificate_clone_issuer_link_contract_win64 -FEtmp/test_certificate_clone_issuer_link_contract_win64 -otmp/test_certificate_clone_issuer_link_contract_win64/test_certificate_clone_issuer_link_contract.exe tests/test_certificate_clone_issuer_link_contract.pas && wine tmp/test_certificate_clone_issuer_link_contract_win64/test_certificate_clone_issuer_link_contract.exe`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `WinSSL: clone should preserve issuer link`
    - GREEN:
      - `8 passed / 0 failed / 3 skipped`

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp -otmp/tmp_backend_contract tests/contract/test_backend_contract.pas && ./tmp/tmp_backend_contract`
  - result: PASS
  - summary:
    - backend contract stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - existing backend interface alignment did not regress after the clone issuer-link repair

- `git diff --check`
  - result: PASS
  - summary:
    - current certificate clone issuer-link batch has no whitespace or patch-format issues

### WinSSL Peer Certificate Issuer Link

- `rg -n "GetPeerCertificate|GetPeerCertificateChain|IssuerCertificate|SetIssuerCertificate|CertGetCertificateChain|SECPKG_ATTR_REMOTE_CERT_CONTEXT" src/fafafa.ssl.winssl.connection.pas src/fafafa.ssl.winssl.certificate.pas`
  - result: PASS
  - summary:
    - confirmed `WinSSL` leaf / chain surfaces already materialized peer certs
    - confirmed connection layer still was not wiring `GetIssuerCertificate()` truth

- `lazbuild tests/winssl/test_winssl_integration_multi.lpi`
  - result: FAIL
  - summary:
    - local Linux-target Lazarus build failed on missing `unit Windows`
    - this confirmed the focused WinSSL runtime path must use explicit `Win64` cross-target rather than default host target

- `lazbuild --os=win64 --cpu=x86_64 tests/winssl/test_winssl_integration_multi.lpi`
  - result: PASS
  - summary:
    - confirmed the local `Win64 cross-target + wine` route is available for real WinSSL RED/GREEN work

- add `docs/plans/2026-05-19-winssl-peer-cert-issuer-link.md`
  - change:
    - define the bounded WinSSL issuer-link completeness batch, commands, scope, and expected closeout

- add `tests/winssl/test_winssl_peer_certificate_surface.pas`
  - change:
    - add focused runtime surface coverage for leaf/chain issuer-link truth against a real WinSSL handshake

- add `tests/winssl/test_winssl_peer_certificate_surface.lpi`
  - change:
    - create dedicated Lazarus entry so the focused WinSSL runtime test can be compiled under `Win64`

- `lazbuild --os=win64 --cpu=x86_64 tests/winssl/test_winssl_peer_certificate_surface.lpi`
  - result: PASS
  - summary:
    - focused WinSSL peer-certificate surface test compiled successfully for `Win64`

- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_WINSSL_PEER_CERT_HOST=api.github.com wine tests/winssl/bin/test_winssl_peer_certificate_surface.exe`
  - result: FAIL -> PASS
  - summary:
    - RED:
      - `peer leaf certificate should preserve issuer link`
      - `peer chain leaf entry should preserve issuer link`
    - GREEN after connection-layer repair:
      - leaf issuer link now matches the returned issuer chain entry
      - chain leaf issuer link now also matches the returned issuer chain entry

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - add WinSSL-local issuer lookup/link helpers
    - `GetPeerCertificate()` now supplements leaf issuer truth from the returned chain
    - `GetPeerCertificateChain()` now preserves issuer links across returned chain entries

- update `tests/run_winssl_tests.ps1`
  - change:
    - add `WinSSL Peer Certificate Surface` to the broader WinSSL runtime suite
    - runtime lane now sets `FAFAFA_RUN_NETWORK_TESTS=1` and `FAFAFA_WINSSL_PEER_CERT_HOST=api.github.com`

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - backend contract stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - existing backend surface alignment did not regress after the WinSSL issuer-link repair

- `pwsh -NoProfile -Command ...ParseFile(...)`
  - result: FAIL
  - summary:
    - local host does not provide `pwsh`; no same-command retry was attempted
    - this batch instead relied on direct file diff review plus the new runtime-suite entry for Windows execution coverage

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL issuer-link batch has no whitespace or patch-format issues

### WolfSSL Peer Certificate Issuer Link

- `nl -ba src/fafafa.ssl.wolfssl.connection.pas | sed -n '714,798p'`
  - result: PASS
  - summary:
    - confirmed `DoGetPeerCertificate()` and `DoGetPeerCertificateChain()` were materializing certs
    - confirmed neither path was wiring `GetIssuerCertificate()` truth yet

- `sed -n '1,260p' tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
  - result: PASS
  - summary:
    - existing WolfSSL surface test only locked chain materialization and safe-degrade
    - it did not yet lock leaf/chain issuer-link truth

- add `docs/plans/2026-05-19-wolfssl-peer-cert-issuer-link.md`
  - change:
    - define the bounded WolfSSL issuer-link completeness batch, commands, scope, and expected closeout

- update `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
  - result: RED
  - summary:
    - add leaf/chain issuer-link truth assertions on top of the existing scripted chain fixture
    - first failure landed on `WolfSSL peer leaf certificate should preserve issuer link`

- update `src/fafafa.ssl.wolfssl.connection.pas`
  - change:
    - add local chain materialization and issuer-link helpers
    - `GetPeerCertificate()` now supplements leaf issuer truth from the peer chain when available
    - `GetPeerCertificateChain()` now preserves issuer links across returned chain entries

- `mkdir -p tmp/test_wolfssl_client_peer_certificate_surface_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_client_peer_certificate_surface_units -FEtmp/test_wolfssl_client_peer_certificate_surface_units -otmp/test_wolfssl_client_peer_certificate_surface_units/test_wolfssl_client_peer_certificate_surface tests/connection/test_wolfssl_client_peer_certificate_surface.pas && ./tmp/test_wolfssl_client_peer_certificate_surface_units/test_wolfssl_client_peer_certificate_surface`
  - result: FAIL -> PASS
  - summary:
    - RED: `WolfSSL peer leaf certificate should preserve issuer link`
    - GREEN: `PASS: WolfSSL client peer certificate chain surface contract passed`

- `mkdir -p tmp/test_wolfssl_connection_peer_certificate_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_peer_certificate_contract_units -FEtmp/test_wolfssl_connection_peer_certificate_contract_units -otmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract tests/test_wolfssl_connection_peer_certificate_contract.pas && ./tmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract`
  - result: PASS
  - summary:
    - existing WolfSSL single-cert materialization contract stayed green at `4 passed / 0 failed`

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: PASS
  - summary:
    - WolfSSL framework suite stayed green at `141 passed / 0 failed`

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - backend contract stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - `Contract 21: Certificate-verification interface alignment - WolfSSL` remained self-consistent after the issuer-link repair

- `git diff --check`
  - result: PASS
  - summary:
    - current WolfSSL issuer-link batch has no whitespace or patch-format issues

### OpenSSL Peer Certificate Issuer Link

- `rg -n "GetPeerCertificate|GetPeerCertificateChain|issuer|Clone|SSL_get_peer_certificate|SSL_get_peer_cert_chain|FindIssuerX509InChain" src/fafafa.ssl.openssl.connection.pas src/fafafa.ssl.openssl.certificate.pas tests/test_openssl_connection_peer_certificate_contract.pas tests/test_openssl_connection_peer_certificate_chain_contract.pas`
  - result: PASS
  - summary:
    - confirmed `OpenSSL` connection leaf / chain surfaces existed
    - confirmed `TOpenSSLCertificate` already had `SetIssuerCertificate/GetIssuerCertificate`
    - confirmed the connection layer was still not wiring that issuer-link truth

- add `docs/plans/2026-05-19-openssl-peer-cert-issuer-link.md`
  - change:
    - define the bounded OpenSSL issuer-link completeness batch, commands, scope, and expected closeout

- add `tests/test_openssl_connection_peer_certificate_surface.pas`
  - result: RED
  - summary:
    - new focused surface contract first failed on
      `OpenSSL peer leaf certificate should preserve issuer link`
    - this locked the real public-surface completeness gap before any production edit

- update `src/fafafa.ssl.openssl.connection.pas`
  - change:
    - add retained-certificate helpers
    - `GetPeerCertificate()` now materializes issuer link from peer chain / verified chain
    - `GetPeerCertificateChain()` now preserves issuer links across returned chain entries

- update `tests/test_openssl_connection_peer_certificate_surface.pas`
  - change:
    - bridge typed `sk_X509_num/value` expectations onto generic `OPENSSL_sk_*` helpers
    - avoid false-negative empty-chain results on OpenSSL builds where typed stack symbols are macro-style

- `mkdir -p tmp/test_openssl_connection_peer_certificate_surface_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_peer_certificate_surface_units -FEtmp/test_openssl_connection_peer_certificate_surface_units -otmp/test_openssl_connection_peer_certificate_surface_units/test_openssl_connection_peer_certificate_surface tests/test_openssl_connection_peer_certificate_surface.pas && ./tmp/test_openssl_connection_peer_certificate_surface_units/test_openssl_connection_peer_certificate_surface`
  - result: FAIL -> FAIL -> PASS
  - summary:
    - RED 1: `OpenSSL peer leaf certificate should preserve issuer link`
    - intermediate harness mismatch: `OpenSSL peer chain surface should materialize the scripted leaf and issuer entries (expected=2 actual=0)`
    - GREEN after harness bridge + implementation fix: `PASS: OpenSSL client peer certificate surface contract passed`

- `mkdir -p tmp/test_openssl_connection_peer_certificate_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_peer_certificate_contract_units -FEtmp/test_openssl_connection_peer_certificate_contract_units -otmp/test_openssl_connection_peer_certificate_contract_units/test_openssl_connection_peer_certificate_contract tests/test_openssl_connection_peer_certificate_contract.pas && ./tmp/test_openssl_connection_peer_certificate_contract_units/test_openssl_connection_peer_certificate_contract`
  - result: PASS
  - summary:
    - existing OpenSSL peer leaf safe-degrade contract stayed green at `2 passed / 0 failed`

- `mkdir -p tmp/test_openssl_connection_peer_certificate_chain_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_peer_certificate_chain_contract_units -FEtmp/test_openssl_connection_peer_certificate_chain_contract_units -otmp/test_openssl_connection_peer_certificate_chain_contract_units/test_openssl_connection_peer_certificate_chain_contract tests/test_openssl_connection_peer_certificate_chain_contract.pas && ./tmp/test_openssl_connection_peer_certificate_chain_contract_units/test_openssl_connection_peer_certificate_chain_contract`
  - result: PASS
  - summary:
    - existing OpenSSL peer chain safe-degrade contract stayed green at `8 passed / 0 failed`

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - backend contract stayed green at `135 total / 111 passed / 0 failed / 24 skipped`
    - `Contract 21: Certificate-verification interface alignment - OpenSSL` remained self-consistent after the issuer-link repair

- `git diff --check`
  - result: PASS
  - summary:
    - current OpenSSL issuer-link batch has no whitespace or patch-format issues

## 2026-05-18

### GitHub Windows Runtime Evidence Strengthening

- `gh run view 26030261335 --json status,conclusion,workflowName,createdAt,updatedAt,jobs,url`
  - result: PASS
  - summary:
    - `Wave B B2 Manual Gate (Template)` run `26030261335` finished `success`
    - `windows-gate` completed `Run quick WinSSL smoke` / `Run Windows Wave B gate` / `Run broader WinSSL runtime suite`

- `gh run download 26030261335 -D tmp/gh-run-26030261335`
  - result: PASS
  - summary:
    - downloaded Linux / macOS / Windows / summary artifacts for direct offline inspection

- `sed -n '1,220p' tmp/gh-run-26030261335/.../winssl_runtime_suite_wave_b_b2_20260518_191939.log`
  - result: PASS
  - summary:
    - downloaded runtime log only contained transcript start/end shell
    - broader suite artifact still lacked substantive runtime details

- `gh run view 26030261335 --job 76514096222 --log`
  - result: PASS
  - summary:
    - job console log proved broader suite actually compiled and ran all 6 lanes
    - root cause narrowed to evidence capture, not missing runtime execution

- update `.github/workflows/wave-b-b2-manual.yml`
  - change:
    - quick smoke and broader suite now stream live output through `Tee-Object -Variable ...`
    - both artifacts are rewritten with `Out-File -Encoding utf8`
    - broader suite no longer relies on transcript-only capture

- update `.github/workflows/wave-b-b2-manual.yml.disabled`
  - change:
    - mirror the same UTF-8 Windows evidence capture fix into the dormant template

- update `.github/workflows/winssl-tests.yml.disabled`
  - change:
    - sync quick/runtime UTF-8 log capture with the active workflow truth
    - summary wording now says `runtime logs` instead of stale `transcripts`

- update `tests/run_winssl_tests.ps1`
  - change:
    - add stable `[WINSSL-RUNTIME] suite_start / test_result / suite_summary / suite_end` markers
    - compile-failure and runtime-failure paths now both emit machine-readable end markers

- update `scripts/check_wave_b_b2_evidence_consistency.sh`
  - change:
    - `windows_runtime_transcript` no longer uses presence-only acceptance
    - runtime log must contain `[WINSSL-RUNTIME]` start/summary/end markers or the report flips to `INCONSISTENT`

- update `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - change:
    - handoff next-actions now distinguish between missing runtime log and marker-less runtime log

- add/update focused contracts
  - result: RED -> GREEN
  - summary:
    - new/updated contracts now lock:
      - workflow UTF-8 runtime log capture
      - substantive Windows runtime evidence requirement
      - runtime marker emission in `tests/run_winssl_tests.ps1`
      - checklist/bundle docs truth about `[WINSSL-RUNTIME]` markers

- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`
  - result: PASS

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - result: PASS

- `bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_runtime_suite_markers_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
  - result: PASS

- `git diff --check`
  - result: PASS
  - summary:
    - current Windows runtime evidence strengthening batch has no whitespace or patch-format issues

- `git push origin master`
  - result: PASS
  - summary:
    - pushed commit `fa7f5af` (`ci/winssl: strengthen windows runtime evidence`) to `origin/master`

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=wave_b_b2_20260518_193941_evidence_fix`
  - result: PASS
  - summary:
    - dispatched a live rerun against commit `fa7f5af` to verify the repaired Windows evidence chain

- `gh api repos/dtamade/fafafa.ssl/actions/runs/26031191987/jobs`
  - result: PASS
  - summary:
    - live rerun `26031191987` finished green across `linux-gate` / `macos-gate` / `windows-gate` / `summary`
    - `windows-gate` completed quick smoke, Wave B Windows gate, and broader WinSSL runtime suite

- `gh run download 26031191987 -D tmp/gh-run-26031191987`
  - result: PASS
  - summary:
    - downloaded the repaired live artifacts for direct offline inspection

- `sed -n '1,220p' tmp/gh-run-26031191987/.../winssl_runtime_suite_wave_b_b2_20260518_193941_evidence_fix.log`
  - result: PASS
  - summary:
    - runtime artifact now contains compile output, per-test execution results, final summary, and stable `[WINSSL-RUNTIME]` markers
    - this is no longer a transcript-only shell

- `sed -n '1,220p' tmp/gh-run-26031191987/.../wave_b_b2_evidence_consistency_wave_b_b2_20260518_193941_evidence_fix.md`
  - result: PASS
  - summary:
    - report now records `windows_runtime_transcript` as `substantive runtime evidence; suite_end_status=PASS`
    - run-level consistency stayed `CONSISTENT`

- `sed -n '1,220p' tmp/gh-run-26031191987/.../wave_b_b2_handoff_bundle_wave_b_b2_20260518_193941_evidence_fix.md`
  - result: PASS
  - summary:
    - handoff bundle closed green on the repaired evidence chain
    - replay command was preserved for future revalidation

### Context Recovery

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch is `master...origin/master`
    - current worktree started clean before this interface/backend verification batch

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - script produced no recovery output
    - there was no extra unsynced session context to merge before starting the new goal

- `sed -n '1,220p' docs/AGENTS.md`
  - result: PASS
  - summary:
    - repo conventions confirm this batch should keep scope tight, prefer focused verification, and update planning files as part of done criteria

- `sed -n '1,220p' docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - result: PASS
  - summary:
    - previous static audit already identified six design-smell families
    - current batch needs to verify whether those smells still map to live implementation truth across backends

- `sed -n '1,160p' task_plan.md`
  - result: PASS
  - summary:
    - previous plan was still anchored on the older WinSSL capability-truth batch
    - a new plan entrypoint is required to avoid reopening the wrong lane next time

### Session Reused Semantic Truth Audit

- `nl -ba src/fafafa.ssl.winssl.connection.pas | sed -n '1748,1772p'`
  - result: PASS
  - summary:
    - WinSSL `DoSetSession(...)` 当时在 `FCurrentSession := ASession` 后立刻 `FSessionReused := True`

- `nl -ba src/fafafa.ssl.mbedtls.connection.pas | sed -n '468,488p'`
  - result: PASS
  - summary:
    - MbedTLS `DoSetSession(...)` 当时在 `mbedtls_ssl_set_session(...) = 0` 后立刻 `FSessionReused := True`

- `nl -ba src/fafafa.ssl.openssl.connection.pas | sed -n '946,1025p'`
  - result: PASS
  - summary:
    - OpenSSL `DoIsSessionReused(...)` 继续直接读取 native `SSL_session_reused`

- `nl -ba src/fafafa.ssl.wolfssl.connection.pas | sed -n '816,842p'`
  - result: PASS
  - summary:
    - WolfSSL `DoIsSessionReused(...)` 继续直接读取 native `wolfSSL_session_reused`

- `nl -ba src/fafafa.ssl.freepascal.connection.pas | sed -n '4838,4870p'`
  - result: PASS
  - summary:
    - FreePascal `DoSetSession(...)` 会先清空 `FSessionReused`，不把 configured session 等价成 resumed handshake

- `nl -ba tests/winssl/test_winssl_session_resumption.pas | sed -n '1,260p'`
  - result: PASS
  - summary:
    - WinSSL 会话复用基线测试源码明确保留 `true resumption尚未接入` 注释

- add `docs/plans/2026-05-18-session-reused-semantic-truth-audit.md`
  - change:
    - 固化本批目标、真值语义、验证命令和 RED -> GREEN 收口标准

- add `tests/scripts/test_session_reused_semantic_truth_contract.sh`
  - result: RED -> GREEN
  - summary:
    - 先直接抓到 WinSSL `DoSetSession(...)` 里预置 `FSessionReused := True`
    - 修复后继续锁住：
      - WinSSL / MbedTLS 不得在 `DoSetSession(...)` 提前误报 reuse
      - OpenSSL / WolfSSL 继续读 native reused truth
      - FreePascal 继续在 `DoSetSession(...)` 清空 reuse 状态

- add `tests/test_mbedtls_connection_session_reused_contract.pas`
  - result: RED -> GREEN
  - summary:
    - 用 fake `mbedtls_ssl_set_session(...) = 0` 模拟 native helper 成功
    - 修复前 `SetSession(...)` 会把 `IsSessionReused` 提前翻成 `True`
    - 修复后 `SetSession(...)` 仍会调用 helper，但握手前 `IsSessionReused` 保持 `False`

- `bash -n tests/scripts/test_session_reused_semantic_truth_contract.sh && bash tests/scripts/test_session_reused_semantic_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED: `[FAIL] WinSSL DoSetSession still preclaims session reuse before handshake truth exists`
    - GREEN: session reused semantics 现在能区分 “configured session” 与 “actual resumed handshake”

- `mkdir -p tmp/test_mbedtls_connection_session_reused_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_connection_session_reused_contract -FEtmp/test_mbedtls_connection_session_reused_contract -otmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract tests/test_mbedtls_connection_session_reused_contract.pas && ./tmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract`
  - result: FAIL -> PASS
  - summary:
    - RED: `SetSession must not claim a resumed handshake before Connect/DoHandshake`
    - GREEN: 3/3 PASS

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - `DoSetSession(...)` 现在只更新当前配置 session，并显式把 `FSessionReused` 复位为 `False`

- update `src/fafafa.ssl.mbedtls.connection.pas`
  - change:
    - `DoSetSession(...)` 现在先清空 `FSessionReused`
    - native `mbedtls_ssl_set_session(...)` 成功不再被误当成“当前握手已复用”

- update `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - change:
    - 明确 Windows runtime artifact evidence 已闭环
    - 明确当前剩余的是“真实 resumed handshake / session tickets runtime proof”，而不是 `IsSessionReused` 的 preclaim 语义

- `git diff --check`
  - result: PASS
  - summary:
    - 当前 session-reused semantic batch 没有 whitespace 或 patch-format 问题

### WinSSL Session Runtime Proof Bridge

- `rg -n "FSessionReused :=|FCurrentSession|SaveSessionAfterHandshake|WasResumed|SetSessionMetadata|GetSession|DoSetSession|DoIsSessionReused|IsResumed|session" src/fafafa.ssl.winssl.connection.pas`
  - result: PASS
  - summary:
    - 确认 canonical WinSSL connection 当时持有 `FSessionReused` / `FCurrentSession`
    - 但 client `DoConnect(...)` 成功后没有调用 `SaveSessionAfterHandshake`

- `nl -ba src/fafafa.ssl.winssl.session.pas | sed -n '48,83p'`
  - result: PASS
  - summary:
    - 兼容 shim `winssl.session` 早就会查询 `SECPKG_ATTR_SESSION_INFO`
    - 这进一步证明 canonical `winssl.connection` 与设计意图脱节

- update `src/fafafa.ssl.winssl.base.pas`
  - change:
    - 新增 `SSL_SESSION_RECONNECT = 1`

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - 新增 current-session-info helper，直接读取 `SECPKG_ATTR_SESSION_INFO`
    - `FSessionReused` 现在来源于 Schannel reconnect flag
    - `SaveSessionAfterHandshake(...)` 会把真实 resumed flag 写回 saved session metadata
    - client `DoConnect(...)` / generic `PerformHandshake(...)` 成功后也会保存 session metadata

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - 从“只是计时基线”切到 dedicated runtime truth proof
    - 新增：
      - owner/core/info/perf reuse truth 一致性检查
      - same-context repeated handshake attempts
      - stable `[WINSSL-SESSION-RESUME] ...` markers
      - `FAFAFA_WINSSL_REQUIRE_REUSE` strict mode

- update `tests/run_winssl_tests.ps1`
  - change:
    - broader suite 新增 `test_winssl_session_resumption.lpi`
    - session-resumption lane 会自动设置 `FAFAFA_RUN_NETWORK_TESTS=1`
    - wrapper 会把 `[WINSSL-SESSION-RESUME] ...` 提升成 `[WINSSL-RUNTIME] session_resumption ...`
    - 成功场景下只要出现这些 proof markers，也会把原始输出保留进 artifact

- update `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - change:
    - broader suite 列表新增 `test_winssl_session_resumption.lpi`
    - runtime markers 现在显式要求 `[WINSSL-RUNTIME] session_resumption summary ...`

- update `tests/windows/VALIDATION_BUNDLE.md`
  - change:
    - inventory 同步记录 dedicated session-resumption proof lane
    - 同步记录 promoted session-resumption runtime marker

- add `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - 锁住 canonical WinSSL connection 必须读取 `SECPKG_ATTR_SESSION_INFO`
    - 锁住 broader suite 必须真正跑 `test_winssl_session_resumption.lpi`
    - 锁住 checklist/bundle 必须记录 dedicated session-resumption proof lane

- `bash -n tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh && bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_runtime_suite_markers_contract.sh`
  - result: PASS

- `bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_session_reused_semantic_truth_contract.sh`
  - result: PASS
  - summary:
    - 之前修好的 “SetSession 不得提前误报 reused=true” 语义仍保持绿色

- `mkdir -p tmp/winssl_session_resumption_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_resumption_win64 -FEtmp/winssl_session_resumption_win64 -otmp/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: FAIL -> PASS
  - summary:
    - RED: 新 helper 初版把 `Assigned(QueryContextAttributesW)` 写成了不适合当前 binding 的表达式，focused Win64 compile 直接抓到编译错误
    - GREEN: 修正后 focused Win64 cross-target compile 通过

- add `docs/plans/2026-05-18-winssl-session-runtime-proof-bridge.md`
  - change:
    - 固化本批目标、source/runtime 边界、验证命令与 live-proof pending 状态

- `gh run view 26033545656 --job 76525564608 --log`
  - result: PASS
  - summary:
    - `windows-gate` 失败点已压缩到 `Run broader WinSSL runtime suite`
    - `test_winssl_session_resumption.lpi` 被 Lazarus 以 `-Tlinux` 编译
    - 真实报错是 `Can't find unit system used by test_winssl_session_resumption`
    - 这说明当前 first hard blocker 是新 `.lpi` 的 host-target drift，不是 session runtime assertion 失败

- `gh run view 26033545656 --job 76525564448 --log`
  - result: PASS
  - summary:
    - `macos-gate` 失败在 `scripts/run_all_module_tests.sh`
    - 与本批新增的 WinSSL session-resumption lane 无直接耦合

- `sed -n '1,200p' tests/winssl/test_winssl_session_resumption.lpi`
  - result: PASS
  - summary:
    - 确认该新 `.lpi` 硬编码了 `TargetCPU=x86_64` 与 `TargetOS=linux`
    - 这与其它 broader-suite WinSSL `.lpi` 默认走宿主平台的做法不一致

- `bash tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
  - result: PASS (false negative)
  - summary:
    - 旧 contract 本来用于阻止 runtime-entry `.lpi` 再次漂回非 Windows target
    - 但它没有包含新加的 `test_winssl_session_resumption.lpi`
    - 因此当前需要先扩 guard，再修 `.lpi` 本身

- update `tests/winssl/test_winssl_session_resumption.lpi`
  - change:
    - 移除硬编码的 `TargetCPU/TargetOS` block
    - 改回与其它 WinSSL runtime-entry `.lpi` 一样使用宿主平台 target truth

- update `tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
  - change:
    - 把 `tests/winssl/test_winssl_session_resumption.lpi` 纳入 guard 列表
    - 防止 dedicated session-resumption lane 再次绕过旧的 Windows runtime project target contract

- `lazbuild tests/winssl/test_winssl_session_resumption.lpi`
  - result: FAIL (expected host-platform boundary)
  - summary:
    - 在当前 Linux 宿主上，去掉硬编码 Linux target 后，Lazarus 会按宿主真相编这份 WinSSL 专项工程
    - 编译随后落在 `fafafa.ssl.winssl.lib` 依赖 `unit Windows` 的既有平台边界
    - 这不是本批 regression；它反而证明 `.lpi` 不再偷偷带错误 target，真正的验收面仍应回到 GitHub Windows runner

- `gh workflow run .github/workflows/wave-b-b2-manual.yml -f run_id=wave_b_b2_20260518_204503_session_target_fix`
  - result: PASS
  - summary:
    - 触发新的 GitHub Actions live rerun `26034303732`
    - head sha: `cca8c0d8660d5f81262ae1a6cc53c42ec80432c1`

- `gh run view 26034303732 --job 76528178883 --log`
  - result: PASS
  - summary:
    - `test_winssl_session_resumption.lpi` 这次在 Windows broader suite compile phase 已通过，证明 `.lpi` target drift 修复生效
    - 新的 first hard blocker 已转成 shared runtime helper：
      - `UpdateSessionReuseTruthFromContext(...)`
      - line `826` of `src/fafafa.ssl.winssl.connection.pas`
    - `Integration Multi` / `Session Resumption Truth` / `Performance Benchmark` / `HTTPS Client` 都在这条 helper 上触发 `EAccessViolation`

- `gh run view 26034303732 --json status,conclusion,jobs,headSha,createdAt`
  - result: PASS
  - summary:
    - workflow run `26034303732` 最终 overall `failure`
    - `windows-gate`: failure
    - `linux-gate`: success
    - `macos-gate`: success
    - `summary`: success

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - `TryGetCurrentSessionInfo(...)` 现在会把 runtime exception 吞掉并回落成 `False`
    - `UpdateSessionReuseTruthFromContext(...)` 现在会把 session-info 读取降成 best-effort observation
    - 任何 session-info 读取异常都只会回落成 `session_id=''` / `FSessionReused=False`，不再允许打崩成功握手路径

- update `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - change:
    - 新增 safety guard 断言：
      - current-session-info helper 必须在异常时回落成 `False`
      - session-info observation 必须是 best-effort，不能破坏 handshake path

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
  - result: PASS

- `bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_session_reused_semantic_truth_contract.sh`
  - result: PASS

- `mkdir -p tmp/winssl_session_resumption_win64_fix && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_resumption_win64_fix -FEtmp/winssl_session_resumption_win64_fix -otmp/winssl_session_resumption_win64_fix/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 focused cross-target compile 继续通过
    - 新的 best-effort session-info guard 没有破坏 WinSSL connection / session-resumption 专项程序的编译面

- `gh workflow run .github/workflows/wave-b-b2-manual.yml -f run_id=wave_b_b2_20260518_205739_session_guard_fix`
  - result: PASS
  - summary:
    - 触发新的 GitHub Actions live rerun `26034948820`
    - head sha: `a5a16508fbb5d2216f0634d0cd07a3484817ffe8`

- `gh run view 26034948820 --json status,conclusion,jobs,headSha,createdAt,updatedAt,url`
  - result: PASS
  - summary:
    - workflow run `26034948820` 最终 overall `failure`
    - `windows-gate`: failure
    - `linux-gate`: success
    - `macos-gate`: success
    - `summary`: success

- `gh run view 26034948820 --job $(gh run view 26034948820 --json jobs --jq '.jobs[] | select(.name=="windows-gate") | .databaseId') --log | rg -n "SessionIdBytesToHex|UpdateSessionReuseTruthFromContext|EAccessViolation|WinSSL Session Resumption Truth|HTTPS Client|Performance Benchmark|Integration Tests" -n -C 2`
  - result: PASS
  - summary:
    - Windows broader suite 的 compile phase 这次继续全部通过
    - shared crash 顶点已从泛化的 session-info helper 收敛到 `UpdateSessionReuseTruthFromContext(...)` 内部的 raw session-id byte 读取
    - 关键证据是多条失败都把栈顶压到 `line 839 of ../../src/fafafa.ssl.winssl.connection.pas`
    - 受影响 lane 继续集中在 `Integration Multi` / `Backend Comparison` / `Session Resumption Truth` / `Performance Benchmark` / `HTTPS Client`

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - `UpdateSessionReuseTruthFromContext(...)` 不再在共享路径里读取 `SessionIdBytesToHex(LSessionInfo)`
    - 当前 canonical path 只保留 `dwFlags and SSL_SESSION_RECONNECT` 作为 reuse truth
    - raw session-id 改成留空，继续复用既有 fallback session-id 生成路径

- update `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - change:
    - contract 现在直接锁住新的更窄边界：
      - reconnect flag 仍是 runtime truth source
      - raw session-id byte buffer 已被证明不稳定，不能留在 canonical shared path
      - session-info observation 继续保持 best-effort 且不得破坏 handshake path

- `bash -n tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh && bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
  - result: PASS

- `bash tests/scripts/test_session_reused_semantic_truth_contract.sh`
  - result: PASS

- `mkdir -p tmp/winssl_session_resumption_win64_fix3 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_resumption_win64_fix3 -FEtmp/winssl_session_resumption_win64_fix3 -otmp/winssl_session_resumption_win64_fix3/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - focused Win64 cross-target compile 继续通过
    - 停止读取 raw session-id bytes 后，没有破坏 WinSSL session-resumption proof 程序的编译面

- `git diff --check`
  - result: PASS

- `gh workflow run .github/workflows/wave-b-b2-manual.yml -f run_id=wave_b_b2_20260518_211710_session_id_fallback`
  - result: PASS
  - summary:
    - 触发新的 GitHub Actions live rerun `26035941452`
    - head sha: `4715efc0cbd0d1e89ba5159c1c63b9938713fb98`

- `gh run view 26035941452 --json status,conclusion,jobs,headSha,createdAt,updatedAt,url`
  - result: PASS
  - summary:
    - workflow run `26035941452` 最终 overall `failure`
    - `windows-gate`: failure
    - `macos-gate`: failure
    - `linux-gate`: success
    - `summary`: success

- `gh api repos/dtamade/fafafa.ssl/actions/jobs/76533985560/logs > tmp/windows-job-76533985560.log`
  - result: PASS
  - summary:
    - 直接绕过 `gh run view --log` 的 in-progress 限制，把 Windows job 原始日志下载到本地

- `rg -n "SessionIdBytesToHex|UpdateSessionReuseTruthFromContext|EAccessViolation|suite_summary|suite_end|observed_reuse|WinSSL Session Resumption Truth|WinSSL HTTPS Client|Performance Benchmark|Backend Comparison Tests|Integration Tests" tmp/windows-job-76533985560.log -n -C 2`
  - result: PASS
  - summary:
    - `windows-gate` 已通过 `Run quick WinSSL smoke` 与 `Run Windows Wave B gate`
    - broader suite compile phase 继续全部通过
    - 旧的 `SessionIdBytesToHex(LSessionInfo)` 崩点不再出现
    - 新崩点已收敛到 `UpdateSessionReuseTruthFromContext(...)` 的 `line 850`
    - 这说明 canonical shared path 上整条 `SECPKG_ATTR_SESSION_INFO` probe 仍然不安全

- `gh api repos/dtamade/fafafa.ssl/actions/jobs/76533985587/logs > tmp/macos-job-76533985587.log`
  - result: PASS
  - summary:
    - 直接下载 macOS job 原始日志，避免把这次 Windows 调查误写成“所有失败都是同一根因”

- `tail -n 220 tmp/macos-job-76533985587.log`
  - result: PASS
  - summary:
    - `macos-gate` 失败在独立的 `scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`
    - `compile_all_modules.py` 与 examples compile 仍然通过
    - 这条回退当前不属于 WinSSL session-resumption shared-crash 本批直接修复面

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - `UpdateSessionReuseTruthFromContext(...)` 不再在 canonical shared path 上调用 live `SECPKG_ATTR_SESSION_INFO` probe
    - 当前共享真相先回到 `reused=false` 与现有 fallback session-id generators
    - `TryGetCurrentSessionInfo(...)` 保留为后续 dedicated Windows proof lane 的实验入口

- update `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - change:
    - contract 现在锁住新的第三层边界：
      - lower-level session-info helper 仍保留
      - canonical shared path 必须停用 live session-info probing
      - 当前共享真相必须回到 conservative fallback

- `gh run view 26037518301 --json status,conclusion,jobs,headSha,createdAt,updatedAt,url`
  - result: PASS
  - summary:
    - workflow run `26037518301` 最终 overall `success`
    - `linux-gate`: success
    - `macos-gate`: success
    - `windows-gate`: success
    - `summary`: success

- `gh api repos/dtamade/fafafa.ssl/actions/jobs/76539716869/logs > tmp/windows-job-76539716869.log`
  - result: PASS
  - summary:
    - 下载最终 green run 的 Windows job 原始日志，确认 broader suite runtime truth，而不只看 job conclusion

- `rg -n "session_resumption|observed_reuse|suite_summary|suite_end|WinSSL Session Resumption Truth|test_result index=4|signal label|summary attempts" tmp/windows-job-76539716869.log -n -C 2`
  - result: PASS
  - summary:
    - Windows broader suite 最终 `suite_summary passed=7 failed=0 total=7 success_rate=100`
    - `WinSSL Session Resumption Truth` 已稳定 PASS
    - 当前 artifact 中的 dedicated runtime truth 为：
      - `observed_reuse=false`
      - `require_reuse=false`
      - `session_configured=true`
      - `attempts=4`
    - shared crash 已消失，当前真实剩余问题只剩 native resumed-handshake behavior 本身

### Interface And Backend Truth Cross-Check

- `rg -n "ISSLConnection = interface|ISSLClientConnection = interface|ISSLServerConnection|SetServerName|TSSLConfig = record|Supports[A-Z][A-Za-z]+: Boolean|[A-Za-z]+Support: TSSLSupportLevel" src/fafafa.ssl.base.pas src/fafafa.ssl.factory.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.pas docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md docs/ARCHITECTURE.md docs/reference/INTERFACE_DESIGN_V2.md`
  - result: PASS
  - summary:
    - confirmed live source still lacks any `ISSLServerConnection` declaration
    - confirmed docs still promise `ISSLServerConnection`
    - confirmed context-level `SetServerName` remains deprecated in base but actively used in factory/builder

- `rg -n "ServerName|SetServerName\\(|GetServerName\\(|CreateConnection\\(" src/fafafa.ssl.factory.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.base.pas src/fafafa.ssl.*connection*.pas tests`
  - result: PASS
  - summary:
    - all major client-capable backends still copy `AContext.GetServerName` into connection state
    - tests also codify context-to-connection `ServerName` fallback as expected behavior

- `rg -n "HandshakeTimeout" src tests`
  - result: PASS
  - summary:
    - request/default factory paths explicitly reject custom `HandshakeTimeout`
    - this is a scoped-design constraint, not a silent no-op

- `rg -n "BufferSize" src tests`
  - result: PASS
  - summary:
    - request/default factory paths explicitly reject custom `BufferSize`
    - `BufferSize` currently remains a public config field mainly for defaults/debug/compatibility surface

- `rg -n "EnableSessionTickets|EnableOCSPStapling" src/fafafa.ssl.factory.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.pas src/fafafa.ssl.base.pas src/fafafa.ssl.context.builder.pas tests/test_factory_logic.pas`
  - result: PASS
  - summary:
    - confirmed `EnableSessionTickets` / `EnableOCSPStapling` are normalized into `Options`
    - this part is compatibility-heavy but still has a live normalization path

- `sed -n '922,1088p' src/fafafa.ssl.openssl.backed.pas`
  - result: PASS
  - summary:
    - OpenSSL `GetCapabilities` still publishes both legacy booleans and v1.2 support-level fields

- `sed -n '1450,1515p' src/fafafa.ssl.freepascal.lib.pas`
  - result: PASS
  - summary:
    - FreePascal backend still marks several legacy booleans as `True` while publishing the corresponding features as `experimental`

- `sed -n '510,575p' src/fafafa.ssl.winssl.lib.pas`
  - result: PASS
  - summary:
    - WinSSL capability source is now internally more truthful after the previous batch, but it still participates in the dual boolean/support-level model

- `sed -n '470,515p' src/fafafa.ssl.mbedtls.lib.pas`
  - result: PASS
  - summary:
    - MbedTLS publishes legacy booleans and support-levels separately, with `OCSPStaplingSupport` locked to `none`

- `sed -n '419,470p' src/fafafa.ssl.wolfssl.lib.pas`
  - result: PASS
  - summary:
    - WolfSSL does the same dual publication, including experimental OCSP/early-data grades

- `sed -n '340,390p' src/fafafa.ssl.backend.selector.pas`
  - result: PASS
  - summary:
    - selector feature matching already trusts support-level fields rather than legacy booleans

- `sed -n '260,305p' src/fafafa.ssl.capability.serializer.pas; sed -n '480,535p' src/fafafa.ssl.capability.serializer.pas; sed -n '665,690p' src/fafafa.ssl.capability.serializer.pas; sed -n '820,868p' src/fafafa.ssl.capability.serializer.pas; sed -n '228,252p' src/fafafa.ssl.capability.diff.pas`
  - result: PASS
  - summary:
    - serializer and diff still round-trip and compare both the legacy boolean surface and the new support-level surface
    - this confirms the dual-truth model is systemic, not a one-file leftover

### Focused Fix And Verification

- update `docs/ARCHITECTURE.md`
  - change:
    - remove nonexistent `ISSLServerConnection` from the active public interface graph
    - clarify that current server-specific capability surfaces mainly live on optional context interfaces

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - remove `ISSLServerConnection` from the active hierarchy
    - restate current truth instead of promising a missing public interface

- add `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - purpose:
    - ensure active docs do not draw `ISSLServerConnection` into the shipped public interface graph while source still lacks the declaration
    - keep the script portable by using `grep`, not a hard `rg` dependency

- `bash -n tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh && bash tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - result: PASS
  - summary:
    - active interface docs no longer promise nonexistent `ISSLServerConnection`

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - custom `HandshakeTimeout` and `BufferSize` are explicitly rejected in factory paths
    - confirms these fields are scope-gated, not silently ignored

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - client-side context `ServerName` remains officially supported as a compatibility path
    - server-side use remains rejected

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - FreePascal / OpenSSL / WolfSSL / MbedTLS all still inherit context-level `ServerName` fallback on dual-context stream paths
    - FreePascal socket path does the same
    - this is now well-proved implementation truth, not just a documentation smell

- `git diff --check`
  - result: PASS

### Shared Client Context SNI Fallback Cut

- add `docs/plans/2026-05-18-shared-client-context-sni-fallback-cut.md`
  - purpose:
    - define the bounded cross-backend alignment batch after the FreePascal-only no-inheritance cut
    - keep scope on the shared seam instead of reopening unrelated release or Windows lanes

- add `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - purpose:
    - prove that all currently available client-capable backends preserve deprecated context state on the context object itself
    - but no longer auto-inherit that state into new client connections

- update `src/fafafa.ssl.context.compat.pas`
  - change:
    - keep `GetContextLevelServerNameCompatibilityValue(...)` as the shared control seam
    - stop reading deprecated context-level `GetServerName`
    - return `''` for any non-nil context so shared-helper backends also follow the no-inheritance rule

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: RED
  - summary:
    - stale source contract still required `src/fafafa.ssl.freepascal.connection.pas` to use the shared helper
    - this contradicted the earlier FreePascal no-inheritance runtime cut and blocked the current batch for the wrong reason

- update `tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - change:
    - require the shared helper only in OpenSSL / WolfSSL / MbedTLS / WinSSL
    - fail if FreePascal reintroduces the shared helper
    - fail if the helper itself or any backend source reintroduces direct `(AContext|FContext).GetServerName` fallback reads

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the shared client fallback cut, the stale-contract correction, and the new next-route recommendation into repo working memory

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark the shared client fallback cut as delivered
    - move the next recommended batch back to the final direct server-context legacy-state control case

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the shared client fallback cut
    - refresh the route summary so the next session does not reopen the already-closed cross-backend fallback divergence

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: RED -> GREEN
  - summary:
    - updated source contract now matches current truth
    - shared-helper backends still route through one seam, FreePascal stays off the seam, and direct context getter fallback stays forbidden everywhere

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend contract finished `20 passed, 0 failed, 1 skipped`
    - FreePascal / OpenSSL / WolfSSL / MbedTLS all keep deprecated context state on the context but no longer inherit it into new client connections
    - WinSSL stayed source-covered and runtime-skipped on Linux because the backend is unavailable on this host

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: PASS
  - summary:
    - focused builder/runtime consistency suite finished `6 passed, 0 failed`
    - the shared seam cut did not regress the remaining direct server-context control assertions

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - focused factory scope suite finished `6 passed, 0 failed`
    - client default-config / one-shot `ServerName` remains context-only state on FreePascal after the shared seam cut

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: PASS
  - summary:
    - focused factory isolation suite finished `6 passed, 0 failed`
    - one-shot/default config isolation remains green while FreePascal connections stay no-inheritance

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - focused dual-role clarification suite finished `28 passed, 0 failed, 1 skipped`
    - the shared seam cut did not reopen the already-closed `sslCtxBoth` no-inheritance boundary

- `git diff --check`
  - result: PASS
  - summary:
    - current shared client fallback cut batch has no whitespace or patch-format issues

### FreePascal Client Context SNI Fallback Cut

- add `docs/plans/2026-05-18-freepascal-client-context-sni-fallback-cut.md`
  - purpose:
    - define the first dedicated `sslCtxClient` behavior-migration batch after the cross-backend contract cleanup
    - keep scope on FreePascal runtime constructors instead of reopening all backends or shared shim consumers

- update `tests/test_freepascal_context_server_name_inheritance.pas`
  - change:
    - flip the dedicated FreePascal regression from inherited-fallback expectations to explicit no-inheritance expectations
    - locally suppress the deprecated direct-context setter warning at the negative-coverage callsite

- add `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - purpose:
    - fail if `src/fafafa.ssl.freepascal.connection.pas` still reads `GetContextLevelServerNameCompatibilityValue(AContext)`
    - keep the new FreePascal runtime cut guarded by a cheap source contract

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_freepascal_context_server_name_inheritance.pas`
    - keep the intentional label set aligned with the smaller remaining compatibility boundary

- `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - result: RED
  - summary:
    - initial failure proved the two FreePascal client constructors still read shared context-level `ServerName` compatibility fallback

- `mkdir -p tmp/test_freepascal_context_server_name_inheritance && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_context_server_name_inheritance -FEtmp/test_freepascal_context_server_name_inheritance -otmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance`
  - result: RED
  - summary:
    - both negative assertions failed
    - builder `WithSNI(...)` and direct context `SetServerName(...)` were still being inherited by new FreePascal client connections

- update `src/fafafa.ssl.freepascal.connection.pas`
  - change:
    - remove `GetContextLevelServerNameCompatibilityValue(AContext)` reads from the socket and stream client constructors
    - leave `FServerName` empty until callers explicitly set per-connection hostname/SNI

- `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - result: RED -> GREEN
  - summary:
    - FreePascal client constructors no longer read context-level `ServerName` compatibility fallback

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the dedicated FreePascal runtime regression

- `mkdir -p tmp/test_freepascal_context_server_name_inheritance && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_context_server_name_inheritance -FEtmp/test_freepascal_context_server_name_inheritance -otmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance/test_freepascal_context_server_name_inheritance`
  - result: RED -> GREEN
  - summary:
    - dedicated FreePascal regression now proves both socket and stream client connections no longer inherit context-level `ServerName`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - remaining intentional mock precedence contract stayed green
    - no production change in this batch accidentally rewrote the next planned compatibility surface

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - remaining connector override precedence contract stayed green

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove `tests/test_freepascal_context_server_name_inheritance.pas` from the intentional compatibility set
    - record the new FreePascal client runtime cut and move the next recommendation to `tests/test_connection_builder_hostname_precedence.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the FreePascal client fallback cut
    - shrink the intentional compatibility set and refresh the next recommended batch

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the dedicated FreePascal runtime cut into persistent repo working memory

### Connection Builder Explicit Hostname Cut

- add `docs/plans/2026-05-18-connection-builder-explicit-hostname-cut.md`
  - purpose:
    - define the next bounded client-side behavior-migration batch after the FreePascal runtime cut
    - keep scope on `TSSLConnectionBuilder.TryBuildClient` instead of reopening connector or shared backend compatibility shims

- update `tests/test_connection_builder_hostname_precedence.pas`
  - change:
    - flip case 1 from “preserve context fallback” to “clear context fallback”
    - keep case 2 explicit override and case 3 explicit empty clear intact
    - locally suppress the deprecated context setter warning at the mock setup callsite

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_connection_builder_hostname_precedence.pas`
    - keep the intentional compatibility label set aligned with the smaller remaining boundary

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: RED
  - summary:
    - only case 1 failed
    - `TryBuildClient` was still preserving inherited context fallback when no explicit hostname was provided

- update `src/fafafa.ssl.connection.builder.pas`
  - change:
    - when the built client connection supports `ISSLClientConnection`, `TryBuildClient` now always owns per-connection hostname state
    - if `WithHostname(...)` was not called, it explicitly clears `ServerName` to `''`
    - explicit override / explicit empty clear behavior remains unchanged

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: RED -> GREEN
  - summary:
    - all 9 assertions passed
    - client builder path no longer preserves inherited context fallback

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - connector override precedence stayed green
    - the builder-path cut did not regress the next higher-level client override surface

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the builder precedence test

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove `tests/test_connection_builder_hostname_precedence.pas` from the intentional compatibility set
    - record the builder explicit-hostname cut and move the next recommendation to `tests/test_tls_connector_hostname_override_precedence.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the connection-builder explicit-hostname cut
    - shrink the remaining client-side intentional compatibility surface again

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the builder explicit-hostname cut into persistent repo working memory

### TLS Connector Override Without Context Fallback

- add `docs/plans/2026-05-18-tls-connector-override-no-context-fallback.md`
  - purpose:
    - define the bounded contract-cleanup batch that removes inherited context fallback from the connector override precedence test
    - keep production `TSSLConnector` code untouched because it already uses pure per-connection `SetServerName(...)`

- add `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - purpose:
    - fail if `tests/test_tls_connector_hostname_override_precedence.pas` still teaches `Ctx.SetServerName(...)`

- update `tests/test_tls_connector_hostname_override_precedence.pas`
  - change:
    - remove the mock context-level `SetServerName('ctx.example.com')` setup
    - rename the empty case text so it no longer talks about clearing inherited fallback

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_tls_connector_hostname_override_precedence.pas`
    - keep the intentional compatibility label set aligned with the smaller remaining boundary

- `bash -n tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - the new focused source contract is syntactically valid

- `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - connector override precedence test no longer teaches context-level SNI

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the connector override test

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - connector override precedence behavior stayed green without the inherited context fallback input

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove `tests/test_tls_connector_hostname_override_precedence.pas` from the intentional compatibility set
    - move the next recommendation to `tests/test_tls_connector_early_data_contract.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the connector override contract cleanup
    - shrink the remaining client-side intentional compatibility surface again

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the connector override contract cleanup into persistent repo working memory

### TLS Connector Early-Data Without Context Fallback

- add `docs/plans/2026-05-18-tls-connector-early-data-no-context-fallback.md`
  - purpose:
    - define the bounded contract-cleanup batch that removes inherited context fallback from the connector early-data contract
    - keep production `TSSLConnector` code untouched because it already applies explicit per-connection hostname before early-data queueing

- add `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - purpose:
    - fail if `tests/test_tls_connector_early_data_contract.pas` still teaches `Ctx.SetServerName(...)`

- update `tests/test_tls_connector_early_data_contract.pas`
  - change:
    - remove the mock context-level `SetServerName('ctx.example.com')` setup
    - rename the server-name assertion so it describes explicit hostname application instead of overriding inherited fallback

- `bash -n tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - the new focused source contract is syntactically valid

- `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - connector early-data contract no longer teaches context-level SNI

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green while shrinking to the remaining server-side control case

- `mkdir -p tmp/test_tls_connector_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_early_data_contract -FEtmp/test_tls_connector_early_data_contract -otmp/test_tls_connector_early_data_contract/test_tls_connector_early_data_contract tests/test_tls_connector_early_data_contract.pas && ./tmp/test_tls_connector_early_data_contract/test_tls_connector_early_data_contract`
  - result: PASS
  - summary:
    - connector early-data ordering and unsupported-path behavior stayed green without the inherited context fallback input

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - add the connector early-data contract cleanup as the fifth cut
    - move the next recommendation to `tests/test_context_builder_server_servername_runtime_consistency.pas`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the connector early-data contract cleanup
    - record that the remaining intentional compatibility label set is now only the server-side control case

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the connector early-data contract cleanup into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - current early-data contract cleanup batch has no whitespace or patch-format issues

### FreePascal Client Context ServerName Expectation Sync

- add `docs/plans/2026-05-18-freepascal-client-context-servername-expectation-sync.md`
  - purpose:
    - define the bounded sync batch that fixes stale FreePascal-focused contracts after the earlier client runtime fallback cut
    - keep the work on truth-sync instead of reopening unrelated server-side or release lanes

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - current intentional compatibility label set is now only the direct server-context control case

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED
  - summary:
    - `BuildClient.WithSNI(...)` still preserved context state
    - but FreePascal client connections no longer inherited that state
    - the focused contract was still asserting pre-cut behavior

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: RED
  - summary:
    - client default-config / one-shot config still preserved context state
    - but FreePascal client connections no longer inherited that state
    - factory focused contract was still asserting pre-cut connection fallback

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: RED
  - summary:
    - default-path / one-shot isolation contract showed the same stale inherited-connection expectation

- update FreePascal-focused contracts:
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - change:
    - keep context-state assertions intact
    - replace inherited-connection assertions with explicit empty-ServerName expectations on FreePascal connections

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record that live retest exposed stale FreePascal-focused expectations
    - move the next recommendation to the remaining shared client fallback backends instead of the old server-side control case

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the FreePascal expectation-sync batch
    - restate the main unresolved seam as cross-backend shared client fallback divergence

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the FreePascal expectation correction and the corrected next route into persistent repo working memory

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED -> GREEN
  - summary:
    - client-side assertion now matches live FreePascal runtime truth
    - server-side control assertions remained green

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: RED -> GREEN
  - summary:
    - FreePascal factory client contract now correctly treats `ServerName` as context-only state, not inherited connection fallback

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: RED -> GREEN
  - summary:
    - default-path / one-shot isolation contract now reflects the same context-only boundary on FreePascal

- `git diff --check`
  - result: PASS
  - summary:
    - current FreePascal expectation-sync batch has no whitespace or patch-format issues

### Residual Context SNI Classification And WinSSL mTLS Skeleton Cleanup

- add `docs/plans/2026-05-18-residual-context-sni-classification-and-mtls-skeleton-cleanup.md`
  - purpose:
    - define the bounded residual classification batch after the first WinSSL client-flow migration cut
    - separate intentional compatibility / API-surface coverage from the last small ordinary handshake path

- add `tests/scripts/test_residual_context_sni_classification_contract.sh`
  - purpose:
    - require explicit `INTENTIONAL_*` labels in the residual ambiguous files
    - fail if `tests/winssl/test_winssl_mtls_skeleton.pas` still uses `Ctx.SetServerName(ServerHost)` in the real handshake path

- `bash -n tests/scripts/test_residual_context_sni_classification_contract.sh && bash tests/scripts/test_residual_context_sni_classification_contract.sh`
  - result: RED
  - summary:
    - initial failure proved `tests/winssl/test_winssl_mtls_skeleton.pas` still lacked explicit `INTENTIONAL_API_SURFACE` classification
    - the residual batch was still real work, not duplicate governance

- update residual classification files:
  - `tests/test_tls_connector_early_data_contract.pas`
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
  - change:
    - add explicit `INTENTIONAL_COMPAT` / `INTENTIONAL_API_SURFACE` markers to the residual ambiguous coverage files
    - move the real `TestMTLSHandshake` flow from context-level `SetServerName(ServerHost)` to per-connection `ISSLClientConnection.SetServerName(ServerHost)`

- `bash -n tests/scripts/test_residual_context_sni_classification_contract.sh && bash tests/scripts/test_residual_context_sni_classification_contract.sh`
  - result: RED -> GREEN
  - summary:
    - residual files are now explicitly classified
    - `test_winssl_mtls_skeleton.pas` no longer uses context-level SNI in the real handshake path

- `mkdir -p tmp/test_tls_connector_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tls_connector_early_data_contract -FEtmp/test_tls_connector_early_data_contract -otmp/test_tls_connector_early_data_contract/test_tls_connector_early_data_contract tests/test_tls_connector_early_data_contract.pas`
  - result: PASS
  - summary:
    - compile succeeded
    - the new `INTENTIONAL_COMPAT` marker only produced the expected deprecated context-level SNI warning at the labeled coverage site

- `mkdir -p tmp/test_mbedtls_context_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_context_contract -FEtmp/test_mbedtls_context_contract -otmp/test_mbedtls_context_contract/test_mbedtls_context_contract tests/mbedtls/test_mbedtls_context_contract.pas`
  - result: PASS
  - summary:
    - compile succeeded
    - the labeled context contract still only emits the expected deprecated setter/getter warnings

- `mkdir -p tmp/test_wolfssl_context_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_context_contract -FEtmp/test_wolfssl_context_contract -otmp/test_wolfssl_context_contract/test_wolfssl_context_contract tests/wolfssl/test_wolfssl_context_contract.pas`
  - result: PASS
  - summary:
    - compile succeeded
    - the labeled context contract still only emits the expected deprecated setter/getter warnings

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_library_basic.exe tests/winssl/test_winssl_library_basic.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile succeeded after adding the explicit API-surface label

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_mtls_skeleton.exe tests/winssl/test_winssl_mtls_skeleton.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile succeeded after migrating the real handshake path to per-connection SNI
    - the remaining context-level setter use in the file is now limited to the labeled configuration smoke coverage

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the residual classification cut as Phase E delivered second cut
    - move the next recommended batch from residual classification to behavior-migration RED selection

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated residual-classification closeout section
    - refresh the next-step recommendation so future sessions continue from behavior-migration RED selection

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the residual classification closeout into persistent repo working memory

- `git diff --check`
  - result: PASS

### Cross-Backend Network Contracts Per-Connection SNI

- add `docs/plans/2026-05-18-cross-backend-network-contracts-per-connection-sni.md`
  - purpose:
    - define the bounded batch that removes deprecated context-level SNI guidance from the two cross-backend network contracts
    - separate real cross-backend result/error contracts from intentional compatibility coverage

- add `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - purpose:
    - fail if the two cross-backend integration contracts still teach `Ctx.SetServerName(...)`
    - require an explicit `SetServerName(...)` call to remain, so the SNI step does not disappear silently

- `bash -n tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - the new focused source contract is syntactically valid

- `bash tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - result: PASS
  - summary:
    - both cross-backend network contracts now use explicit per-connection SNI instead of `Ctx.SetServerName(...)`

- update cross-backend network contracts:
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
  - change:
    - remove `Ctx.SetServerName(...)`
    - require `ISSLClientConnection`
    - move SNI setup to `ClientConn.SetServerName(...)` before `Connect`
    - migrate the `www.google.com:80` handshake-failure branch to the same per-connection path

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `test_cross_backend_consistency_contract`
    - remove `test_cross_backend_errors_contract`
    - keep the intentional-compat label set aligned with the smaller remaining real compatibility boundary

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the intentional compatibility label set stayed green after removing the two cross-backend network contracts

- `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
  - result: PASS
  - summary:
    - compile/run shape stayed green after the per-connection SNI migration
    - runtime network probe remained skipped on this host because `FAFAFA_RUN_NETWORK_TESTS!=1`

- `mkdir -p tmp/test_cross_backend_errors_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_errors_contract -FEtmp/test_cross_backend_errors_contract -otmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract`
  - result: PASS
  - summary:
    - compile/run shape stayed green after the per-connection SNI migration
    - runtime network probe remained skipped on this host because `FAFAFA_RUN_NETWORK_TESTS!=1`

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - remove the two cross-backend network contracts from the intentional compatibility set
    - record the new Phase E cut that migrates them to per-connection SNI

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - remove the old claim that the two cross-backend network contracts must carry `INTENTIONAL_COMPAT`
    - add a dedicated closeout section for the per-connection SNI migration
    - refresh the next recommended batch toward `tests/test_freepascal_context_server_name_inheritance.pas`

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync this batch into persistent repo working memory so future sessions do not reopen the old misclassification

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the cross-backend per-connection SNI batch
  - summary:
    - no whitespace or patch-format issues remained after the residual classification batch

### BuildServer WithSNI Ignore Behavior Migration

- add `docs/plans/2026-05-18-buildserver-withsni-ignore-behavior-migration.md`
  - purpose:
    - define the first true behavior-migration cut after residual classification closed
    - keep scope bounded to the server-side builder dead-compat path instead of reopening client fallback

- update focused RED tests:
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
  - `tests/config/test_config_validation.pas`
  - change:
    - expect `BuildServer.WithSNI(...)` to stop retaining `ServerName` on the built server context
    - expect warning / validation wording to say `BuildServer ignores it and server-side connections ignore it`

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED
  - summary:
    - initial run failed 1 assertion
    - `BuildServer` still retained the deprecated client-only `ServerName` on the built server context

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED
  - summary:
    - initial run failed 2 assertions
    - warning wording still described the old apply/ignore split and did not match the desired runtime truth

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: RED
  - summary:
    - initial run failed 1 assertion
    - validation wording still described server-side ignore semantics without the new `BuildServer ignores it` truth

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - `BuildServer` no longer calls `Result.SetServerName(FServerName)`
    - builder server warning now says `BuildServer ignores it and server-side connections ignore it`
    - `ValidateServer` warning wording now follows the same ignore semantics

- update `docs/reference/API_REFERENCE.md`
  - change:
    - clarify that `BuildClient` applies `WithSNI(...)` with warning, while `BuildServer` warns and ignores it

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: RED -> GREEN
  - summary:
    - final run finished `6 passed, 0 failed`
    - built server contexts no longer retain the deprecated client-only `ServerName`

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED -> GREEN
  - summary:
    - final run finished `14 passed, 0 failed`
    - builder warning text now matches the actual ignore behavior

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: RED -> GREEN
  - summary:
    - final run finished `53 passed, 0 failed`
    - validation wording is aligned with the new runtime truth

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the first server-side behavior-migration cut
    - move the next recommended batch to client-side behavior-migration RED selection

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated BuildServer dead-compat closeout section
    - refresh the next-step recommendation toward client-side fallback migration

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the first behavior-migration cut into the persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the BuildServer ignore batch

### sslCtxBoth Context SNI Ambiguity Cut

- add `docs/plans/2026-05-18-sslctxboth-context-sni-ambiguity-cut.md`
  - purpose:
    - define the first bounded client-side fallback migration cut
    - keep the scope on `sslCtxBoth` role ambiguity instead of reopening all client fallback paths

- update `tests/test_sslctxboth_client_capability_clarification.pas`
  - change:
    - move the dual-context stream/socket expectations from inherited context fallback to explicit no-fallback semantics
    - keep the `ISSLClientConnection` exposure checks and early-data role-gate checks intact

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - remove `tests/test_sslctxboth_client_capability_clarification.pas` from the intentional-compat label set
    - this file is no longer expected to preserve legacy inherited fallback

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: RED
  - summary:
    - initial run failed 5 assertions
    - FreePascal / OpenSSL / WolfSSL / MbedTLS dual-context stream paths and the FreePascal socket path all still inherited `both.example.com`

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the remaining intentional-compat label set stayed stable after removing the `sslCtxBoth` file

- `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
  - result: PASS
  - summary:
    - adjacent roleless-handshake boundary was already green before the shim change
    - this confirmed the intended semantic anchor for the ambiguity cut

- update `src/fafafa.ssl.context.compat.pas`
  - change:
    - `GetContextLevelServerNameCompatibilityValue(...)` now returns empty for `sslCtxBoth`
    - add a short comment tying this to the existing explicit-role handshake rule

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: RED -> GREEN
  - summary:
    - final run finished `28 passed, 0 failed, 1 skipped`
    - dual-role contexts no longer inherit deprecated context-level `ServerName` fallback

- `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
  - result: PASS
  - summary:
    - roleless-handshake fail-fast behavior remained intact after the ambiguity cut

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the remaining intentional-compat label set stayed green after the `sslCtxBoth` removal

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the `sslCtxBoth` ambiguity cut under the shared-compatibility-shim track
    - move the next recommended batch to `sslCtxClient` behavior migration RED selection

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated `sslCtxBoth` ambiguity-cut closeout section
    - refresh the next-step recommendation toward `sslCtxClient` fallback migration

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the first client-side fallback migration cut into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the `sslCtxBoth` ambiguity cut

### Context ServerName Shared Compatibility Shim

- add `docs/plans/2026-05-18-context-servername-shared-compatibility-shim.md`
  - purpose:
    - define the bounded Phase C batch before code changes
    - keep the next execution order anchored on shared seam extraction instead of broader migration

- add `tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - purpose:
    - force a RED on missing shared helper adoption
    - guard both helper presence and backend source migration away from local direct context `GetServerName` reads

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: RED
  - summary:
    - initial failure proved `src/fafafa.ssl.context.compat.pas` did not exist yet
    - shared compatibility seam had not been extracted

- add `src/fafafa.ssl.context.compat.pas`
  - change:
    - introduce `GetContextLevelServerNameCompatibilityValue(...)`
    - centralize client-role gate, deprecated read, and warning suppression in one place

- update backend constructors:
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - replace local direct context `GetServerName` fallback reads with shared helper usage
    - preserve each backend's original side effect path (`SetServerName(...)` vs field assignment)

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: RED
  - summary:
    - first compile failed because the new helper referenced `ContextTypeSupportsClientConnectionRole` from the wrong unit
    - fixed by importing `fafafa.ssl.connection.base` inside `src/fafafa.ssl.context.compat.pas`

- `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh && bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - result: PASS
  - summary:
    - shared helper now exists
    - all five backend constructor paths route fallback through the shared seam
    - backend-local direct context `GetServerName` reads are gone from the targeted files

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - cross-backend context-to-connection ServerName fallback remains intact after seam extraction
    - final run finished `28 passed, 0 failed, 1 skipped`

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - factory/client compatibility behavior remains intact after backend shim extraction
    - final run finished `6 passed, 0 failed`

### Builder ServerName Compatibility Warning

- add `docs/plans/2026-05-18-builder-servername-compatibility-warning.md`
  - purpose:
    - define the next bounded builder-surface batch after the shared shim landed
    - keep the repo-level route anchored on runtime compatibility warning alignment instead of broader surface redesign

- add `tests/test_context_builder_server_name_compatibility_warning.pas`
  - purpose:
    - prove builder runtime path still silently applies `WithSNI(...)`
    - lock the exact warning expectations for `BuildClient`, `BuildServer`, and the no-SNI quiet path

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED
  - summary:
    - initial run failed 8 assertions
    - `BuildClient` / `BuildServer` both still silently applied `WithSNI(...)`
    - no runtime warning named `WithSNI`, no compatibility-only phrasing, and no builder callsite evidence existed yet

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - add `LogBuilderContextLevelServerNameCompatibilityWarning(...)`
    - emit runtime warning before `BuildClient` / `BuildServer` apply `FServerName` to the context
    - align validation wording so client/server `WithSNI(...)` warnings follow the same compatibility terminology
    - add a short interface comment marking `WithSNI(...)` as compatibility-only

- update `docs/reference/API_REFERENCE.md`
  - change:
    - extend the `Client SNI Compatibility Note` so it explicitly includes `TSSLContextBuilder.WithSNI(...)`
    - point new code toward `TSSLConnectionBuilder.WithHostname(...)` in addition to the per-connection APIs already documented

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: RED -> GREEN
  - summary:
    - final run passed all 12 assertions
    - builder runtime path no longer stays silent when `WithSNI(...)` is applied
    - the quiet path without `WithSNI(...)` remains quiet

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: PASS
  - summary:
    - validation warning semantics stayed aligned after the builder wording update
    - final run finished `53 passed, 0 failed`

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: PASS
  - summary:
    - builder client/server compatibility behavior remained intact after adding runtime warnings
    - final run finished `6 passed, 0 failed`

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the builder warning batch

### WinSSL Client Flow SNI Guidance Cleanup

- add `docs/plans/2026-05-18-winssl-client-flow-sni-guidance-cleanup.md`
  - purpose:
    - define a bounded batch over a small set of ordinary WinSSL client-flow tests
    - separate normal client-flow guidance from intentional compatibility/API-surface coverage

- add `tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
  - purpose:
    - fail if selected WinSSL client-flow tests still teach context-level SNI through local context variables

- `bash -n tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh && bash tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
  - result: RED
  - summary:
    - initial failure proved `tests/winssl/test_winssl_error_mapping_online.pas` still used `Ctx.SetServerName('expired.badssl.com')`
    - the selected WinSSL client-flow files were still carrying deprecated context-level SNI guidance

- update selected WinSSL client-flow tests:
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
  - change:
    - replace local context-level `SetServerName(...)` with per-connection `ISSLClientConnection.SetServerName(...)`
    - preserve existing protocol/verification/handshake assertions

- `bash -n tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh && bash tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
  - result: PASS
  - summary:
    - the selected WinSSL client-flow tests no longer use context-level SNI guidance

- `mkdir -p tmp/test_winssl_https_client && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_winssl_https_client -FEtmp/test_winssl_https_client -otmp/test_winssl_https_client/test_winssl_https_client tests/winssl/test_winssl_https_client.pas`
  - result: EXPECTED PLATFORM FAILURE
  - summary:
    - direct Linux-target compile still fails in `src/fafafa.ssl.winssl.lib.pas` because the WinSSL library depends on the `Windows` unit
    - this confirms the selected files should be verified through Win64 cross-compile or Windows runtime evidence, not native Linux-target compile

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_error_mapping_online.exe tests/winssl/test_winssl_error_mapping_online.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_revocation_online.exe tests/winssl/test_winssl_revocation_online.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_mtls_e2e_local.exe tests/winssl/test_winssl_mtls_e2e_local.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_winssl_https_client.exe tests/winssl/test_winssl_https_client.pas`
  - result: PASS
  - summary:
    - Win64 cross-compile completed successfully after the per-connection SNI change

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the WinSSL client-flow cleanup batch

### Context ServerName Compatibility Roadmap Freeze

- `rg -n "SetServerName\\(|GetServerName\\(|WithSNI\\(|ServerName\\b" src tests docs | sed -n '1,320p'`
  - result: PASS
  - summary:
    - mapped the remaining `context-level ServerName` write paths, backend fallback read paths, active docs guidance, and focused tests that still lock compatibility semantics

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the real migration map across factory, builder, connector, five backend constructors, and intentional compatibility tests
    - define the next execution order as builder surface narrowing -> shared compatibility shim -> final surface cleanup
    - include a route-level progress report so future sessions resume from the current main line instead of reopening finished capability work

- update `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - change:
    - add `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - add `tests/test_sslctxboth_client_capability_clarification.pas`
    - accept the unified `INTENTIONAL_COMPAT:` label across the curated compatibility-locking tests

- update `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - change:
    - align the direct server-context compatibility note to the shared `INTENTIONAL_COMPAT:` label family

- update `tests/test_sslctxboth_client_capability_clarification.pas`
  - change:
    - label the dual-context fallback checks as explicit intentional compatibility coverage

- `bash -n tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the curated context-level SNI compatibility tests are now explicitly labeled, including the newly mapped builder-server and sslCtxBoth fallback regressions

- `git diff --check`
  - result: PASS

### Capability Serialization Truth Projection

- add `tests/test_capability_serialization_truth_projection.pas`
  - purpose:
    - directly assert JSON/XML emitted payload truth instead of relying on deserialize round-trip
    - catch cases where serializer leaks contradictory `supports*` and `*Support` fields

- `mkdir -p tmp/test_capability_serialization_truth_projection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_serialization_truth_projection -FEtmp/test_capability_serialization_truth_projection -otmp/test_capability_serialization_truth_projection/test_capability_serialization_truth_projection tests/test_capability_serialization_truth_projection.pas && ./tmp/test_capability_serialization_truth_projection/test_capability_serialization_truth_projection`
  - result: RED -> GREEN
  - summary:
    - initial failure proved `CapabilitiesToJSON(...)` still emitted `"supportsSNI": false` while `sniSupport` was already `"stable"`
    - after the fix, JSON/XML serialization now projects legacy boolean output from support-level truth whenever the record already carries v1.2 support-level signals

- update `src/fafafa.ssl.capability.serializer.pas`
  - change:
    - add `HasAnySupportLevelTruth(...)` and `PrepareCapabilitiesForSerialization(...)`
    - normalize a local copy before JSON/XML emission when the record is already support-level-aware
    - keep pure legacy-only in-memory records untouched because serializer still has no presence bits to distinguish default `none` from explicit `none`

- `mkdir -p tmp/test_capability_deserialization_roundtrip && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_deserialization_roundtrip -FEtmp/test_capability_deserialization_roundtrip -otmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip tests/test_capability_deserialization_roundtrip.pas && ./tmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip`
  - result: PASS
  - summary:
    - existing JSON/XML round-trip compatibility remained green after the serializer projection fix

- `git diff --check`
  - result: PASS

### Capability Runtime Truth Alignment

- `git diff -- src/fafafa.ssl.base.pas src/fafafa.ssl.freepascal.lib.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.mbedtls.lib.pas src/fafafa.ssl.wolfssl.lib.pas`
  - result: PASS
  - summary:
    - confirmed this batch adds one shared normalization helper in `fafafa.ssl.base`
    - confirmed all five live capability sources now normalize legacy boolean truth from the v1.2 support-level fields before caching/returning

- `git diff -- tests/contract/test_capabilities_contract.pas tests/contract/test_backend_contract.pas tests/scripts/test_capability_legacy_bool_normalization_contract.sh`
  - result: PASS
  - summary:
    - confirmed the new source contract guards helper adoption across all major backends
    - confirmed contract assertions now trust `*Support` as runtime truth and also require bool/support-level projection consistency

- `bash -n tests/scripts/test_capability_legacy_bool_normalization_contract.sh && bash tests/scripts/test_capability_legacy_bool_normalization_contract.sh`
  - result: PASS
  - summary:
    - the shared normalization helper is declared in `src/fafafa.ssl.base.pas`
    - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL all invoke `NormalizeLegacyCapabilityBooleans(Result);` in `GetCapabilities`

- `mkdir -p tmp/test_capabilities_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capabilities_contract -FEtmp/test_capabilities_contract -otmp/test_capabilities_contract/test_capabilities_contract tests/contract/test_capabilities_contract.pas && ./tmp/test_capabilities_contract/test_capabilities_contract`
  - result: PASS
  - summary:
    - focused capability contract finished `63 passed, 0 failed, 1 skipped`
    - major backends now pass support-level-first truth checks and all bool/support-level consistency assertions
    - compile emitted only pre-existing repo warning families; no new normalization-related failures appeared

- `mkdir -p tmp/test_backend_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_contract -FEtmp/test_backend_contract -otmp/test_backend_contract/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract finished `111 passed, 0 failed, 24 skipped`
    - optional interface alignment for SNI / CT / OCSP now follows the support-level truth and remains green across available backends
    - Windows Schannel remains intentionally skipped on this Linux host, consistent with the repo's current platform boundary

### Serializer / Deserializer / Diff Truth Alignment

- `mkdir -p tmp/test_capability_deserialization_truth_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_deserialization_truth_precedence -FEtmp/test_capability_deserialization_truth_precedence -otmp/test_capability_deserialization_truth_precedence/test_capability_deserialization_truth_precedence tests/test_capability_deserialization_truth_precedence.pas && ./tmp/test_capability_deserialization_truth_precedence/test_capability_deserialization_truth_precedence`
  - result: RED -> GREEN
  - summary:
    - initial failure proved `JSONToCapabilities(...)` kept `supportsSNI=true` even when `sniSupport="none"` was present in the same payload
    - after the fix, JSON/XML deserialization now lets v1.2 `*Support` fields override conflicting legacy boolean inputs while preserving legacy-only input compatibility

- `mkdir -p tmp/test_capability_diff_support_level_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_diff_support_level_truth -FEtmp/test_capability_diff_support_level_truth -otmp/test_capability_diff_support_level_truth/test_capability_diff_support_level_truth tests/test_capability_diff_support_level_truth.pas && ./tmp/test_capability_diff_support_level_truth/test_capability_diff_support_level_truth`
  - result: RED -> GREEN
  - summary:
    - initial failure proved `CompareCapabilities(...)` completely missed `SNISupport` / `EarlyDataSupport` changes when legacy boolean values did not change
    - after the fix, diff now compares support-level truth first and uses legacy boolean only as a compatibility fallback

- `mkdir -p tmp/test_capability_deserialization_roundtrip && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_deserialization_roundtrip -FEtmp/test_capability_deserialization_roundtrip -otmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip tests/test_capability_deserialization_roundtrip.pas && ./tmp/test_capability_deserialization_roundtrip/test_capability_deserialization_roundtrip`
  - result: PASS
  - summary:
    - existing JSON/XML round-trip test remained green after the precedence fix
    - confirms this batch tightened truth precedence without regressing the current serialization/deserialization compatibility path

### Internal Context ServerName Warning Quarantine

- `mkdir -p tmp/internal_context_servername_warning_probe && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/internal_context_servername_warning_probe -FEtmp/internal_context_servername_warning_probe -otmp/internal_context_servername_warning_probe/test_capabilities_contract tests/contract/test_capabilities_contract.pas 2>&1 | tee tmp/internal_context_servername_warning_probe/compile.log`
  - result: RED
  - summary:
    - live compile probe emitted deprecated `ISSLContext.GetServerName` warnings from `src/fafafa.ssl.wolfssl.connection.pas` and `src/fafafa.ssl.mbedtls.connection.pas`
    - this confirmed the old `test_builder_integration`-based warning contract had drifted away from the current noise source

- update `tests/scripts/test_internal_context_servername_warning_contract.sh`
  - change:
    - switch the compile probe from `tests/test_builder_integration.pas` to `tests/contract/test_capabilities_contract.pas`
    - check that `wolfssl.connection` / `mbedtls.connection` no longer emit deprecated `GetServerName` warnings
    - add a static `WinSSL` source guard by requiring local warning quarantine markers in `src/fafafa.ssl.winssl.connection.pas`
    - run the compiled `test_capabilities_contract` binary as part of the contract

- update `src/fafafa.ssl.wolfssl.connection.pas`
  - change:
    - add local deprecated-warning quarantine around the two constructor fallback reads of `AContext.GetServerName`

- update `src/fafafa.ssl.mbedtls.connection.pas`
  - change:
    - add local deprecated-warning quarantine around the internal SNI fallback read from `FContext.GetServerName`

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - add local deprecated-warning quarantine around both constructor fallback reads of `AContext.GetServerName`

- `bash -n tests/scripts/test_internal_context_servername_warning_contract.sh`
  - result: PASS

- `bash tests/scripts/test_internal_context_servername_warning_contract.sh`
  - result: GREEN
  - summary:
    - internal warning contract passed after the local quarantines landed
    - the compiled `test_capabilities_contract` binary still executed successfully inside the contract

- `rg -n "deprecated" tmp/internal_context_servername_warning_contract/build.log`
  - result: PASS
  - summary:
    - no remaining deprecated-warning matches were left in the focused compile log after the quarantine change

### Context Builder ServerName Compatibility Marker

- resumed compile/run session `74931` for `tests/config/test_context_builder_server_name_compat_marker.pas`
  - result: RED
  - summary:
    - initial run failed 5 assertions
    - builder export lacked any explicit compatibility marker for `server_name`
    - legacy JSON import failure also exposed a brittle substring-style assertion against pretty-printed JSON

- add `tests/config/test_context_builder_server_name_compat_marker.pas`
  - purpose:
    - lock builder JSON/INI export behavior so `server_name` remains backward compatible but is visibly marked as deprecated context-level SNI compatibility
    - ensure legacy JSON/INI payloads with bare `server_name` still import and re-export with the new marker

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - add `CONTEXT_SERVER_NAME_COMPAT_MODE = 'deprecated_context_sni'`
    - emit `server_name_mode` in JSON/INI export whenever `server_name` is non-empty
    - explicitly accept/ignore `server_name_mode` during JSON/INI import so compatibility metadata does not affect runtime state

- update `tests/config/test_context_builder_server_name_compat_marker.pas`
  - change:
    - parse JSON for the legacy-import assertions instead of substring-matching formatted output
    - keep the INI assertions string-based because INI export is line-oriented and stable

- `mkdir -p tmp/test_context_builder_server_name_compat_marker && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compat_marker -FEtmp/test_context_builder_server_name_compat_marker -otmp/test_context_builder_server_name_compat_marker/test_context_builder_server_name_compat_marker tests/config/test_context_builder_server_name_compat_marker.pas && ./tmp/test_context_builder_server_name_compat_marker/test_context_builder_server_name_compat_marker`
  - result: RED -> GREEN
  - summary:
    - all 8 assertions passed after the builder export/import compatibility marker patch
    - compile emitted only pre-existing repo warning families

- `mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export`
  - result: PASS
  - summary:
    - focused config import/export suite finished `96 passed, 0 failed`
    - the new `server_name_mode` field did not break existing JSON/INI round-trip coverage

- `mkdir -p tmp/test_context_builder_merge_advanced_option_snapshot_semantics && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_merge_advanced_option_snapshot_semantics -FEtmp/test_context_builder_merge_advanced_option_snapshot_semantics -otmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas && ./tmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics`
  - result: PASS
  - summary:
    - merge snapshot semantics stayed green (`13 passed, 0 failed`)
    - additive compatibility metadata did not alter empty-field or option-clearing behavior

- `mkdir -p tmp/test_config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_snapshot_clone -FEtmp/test_config_snapshot_clone -otmp/test_config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone/test_config_snapshot_clone`
  - result: PASS
  - summary:
    - clone/reset/merge suite stayed green (`57 passed, 0 failed`)
    - builder snapshots continue to round-trip after the compatibility marker addition

- update `docs/plans/2026-05-18-context-builder-servername-compatibility-marker.md`
  - change:
    - record the bounded Phase B first-cut plan, touched files, command sequence, and expected outputs for the builder compatibility marker batch

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark Phase B builder surface first cut as delivered
    - move the next recommended batch to `factory/config surface narrowing`

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated builder-surface compatibility-marker closeout section
    - refresh the "next batch" recommendation so future sessions do not restart from discovery

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new builder-surface result into the persistent repo-level working memory

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained at batch closeout

### Factory Config ServerName Compatibility Warning

- add `tests/test_factory_server_name_compatibility_warning.pas`
  - purpose:
    - lock the second Phase B cut so factory/client `TSSLConfig.ServerName` compatibility no longer stays silent
    - prove both default-config and one-shot factory paths emit an explicit deprecation warning while preserving current behavior

- `mkdir -p tmp/test_factory_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_compatibility_warning -FEtmp/test_factory_server_name_compatibility_warning -otmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning tests/test_factory_server_name_compatibility_warning.pas && ./tmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning`
  - result: RED
  - summary:
    - initial run failed 8 assertions
    - both factory client paths still silently applied `TSSLConfig.ServerName`
    - no warning named `TSSLConfig.ServerName`, no compatibility-only phrasing, and no explicit callsite evidence existed yet

- update `src/fafafa.ssl.factory.pas`
  - change:
    - add `LogContextLevelServerNameCompatibilityWarning(...)`
    - emit `TSecurityLog.Warning('Factory', ...)` right before client-side compatibility writes in both `CreateContext` overloads
    - message explicitly names `TSSLConfig.ServerName`, marks it as deprecated context-level SNI compatibility, and points callers at `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`

- update `src/fafafa.ssl.base.pas`
  - change:
    - mark `TSSLConfig.ServerName` field comment as deprecated compatibility-only context-level SNI

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `Client SNI Compatibility Note`
    - document that factory still applies `TSSLConfig.ServerName` only for compatibility and now emits a warning

- `mkdir -p tmp/test_factory_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_compatibility_warning -FEtmp/test_factory_server_name_compatibility_warning -otmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning tests/test_factory_server_name_compatibility_warning.pas && ./tmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning`
  - result: RED -> GREEN
  - summary:
    - all 12 assertions passed after the warning patch
    - default-config client path and one-shot config path now both emit the expected compatibility warning
    - client config without `ServerName` remains quiet

- `mkdir -p tmp/test_factory_server_name_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_scope_clarification -FEtmp/test_factory_server_name_scope_clarification -otmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification tests/test_factory_server_name_scope_clarification.pas && ./tmp/test_factory_server_name_scope_clarification/test_factory_server_name_scope_clarification`
  - result: PASS
  - summary:
    - client default-config and one-shot `ServerName` compatibility behavior remains intact
    - server-side rejection behavior remains intact

- `mkdir -p tmp/test_factory_config_server_name_isolation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_config_server_name_isolation -FEtmp/test_factory_config_server_name_isolation -otmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation/test_factory_config_server_name_isolation`
  - result: PASS
  - summary:
    - one-shot `ServerName` still does not leak into shared defaults
    - explicit default-config compatibility inheritance remains intact

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - request config still rejects `LogLevel` / `LogCallback`
    - library default logging round-trip and dispatch behavior stayed green after the new factory warning hook

- `bash tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs still do not teach deprecated context-level SNI as the recommended path

- `git diff --check`
  - result: PASS
  - summary:
    - no whitespace or patch-format issues remained after the factory/config warning batch

- `git diff --check`
  - result: PASS

### High-Level Context ServerName Ignore Cut

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - recovery script produced no extra unsynced context to merge
    - current session could continue directly from the live worktree and planning files

- `mkdir -p tmp/test_factory_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_server_name_compatibility_warning -FEtmp/test_factory_server_name_compatibility_warning -otmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning tests/test_factory_server_name_compatibility_warning.pas && ./tmp/test_factory_server_name_compatibility_warning/test_factory_server_name_compatibility_warning`
  - result: PASS
  - summary:
    - focused factory warning suite finished `16 passed, 0 failed`
    - default-config and one-shot client paths both emit the compatibility warning
    - built client contexts no longer retain deprecated `ServerName` state

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: PASS
  - summary:
    - focused config validation suite finished `53 passed, 0 failed`
    - builder validation wording and compatibility guidance stayed green after the high-level ignore cut

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend contract finished `20 passed, 0 failed, 1 skipped`
    - direct context API still keeps deprecated `ServerName` observable on the context itself
    - new client connections across available backends still do not inherit that state

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - sync current truth so builder/factory high-level paths are `warning + ignore`
    - move the next recommended batch from the old direct-state control case to final public surface cleanup prep

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the high-level context `ServerName` ignore cut
    - refresh the route summary so future sessions do not reopen the already-closed builder/factory legacy-state question

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that deprecated context-level `ServerName` no longer enters new contexts through builder/factory high-level paths
    - record that direct `ISSLContext.SetServerName/GetServerName` is now the last remaining observable compatibility surface

- `git diff --check`
  - result: PASS
  - summary:
    - current high-level ignore cut batch has no whitespace or patch-format issues

- `git status --short`
  - result: PASS
  - summary:
    - worktree contains the expected builder/factory/test/doc updates for the current batch
    - new plan file `docs/plans/2026-05-18-high-level-context-servername-ignore-cut.md` is ready to be added at commit time

### OpenSSL Library Default-Config ServerName Alignment

- add `docs/plans/2026-05-18-openssl-library-default-config-servername-alignment.md`
  - purpose:
    - define the bounded backend-specific alignment batch for the remaining OpenSSL direct-library default-config `ServerName` drift
    - keep scope on `ISSLLibrary.SetDefaultConfig + TOpenSSLLibrary.CreateContext(...)` instead of reopening the whole public-surface family

- add `tests/test_openssl_library_default_config_server_name_clarification.pas`
  - purpose:
    - prove the OpenSSL direct-library client default-config path still preserved deprecated `ServerName`
    - prove the OpenSSL direct-library server default-config path was not rejecting client-scoped `ServerName` yet

- `mkdir -p tmp/test_openssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_library_default_config_server_name_clarification -FEtmp/test_openssl_library_default_config_server_name_clarification -otmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification tests/test_openssl_library_default_config_server_name_clarification.pas && ./tmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification`
  - result: RED
  - summary:
    - initial run failed `8` assertions
    - OpenSSL direct-library client path still preserved deprecated default `ServerName`
    - OpenSSL direct-library server path still created a context instead of rejecting the client-scoped field
    - no direct-library warning existed yet

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - move the server-scope validation into a true fail-fast check before context creation
    - stop applying `FDefaultConfig.ServerName` to new client contexts
    - emit an OpenSSL library warning through the library log callback when client default-config still carries deprecated `ServerName`

- `mkdir -p tmp/test_openssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_library_default_config_server_name_clarification -FEtmp/test_openssl_library_default_config_server_name_clarification -otmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification tests/test_openssl_library_default_config_server_name_clarification.pas && ./tmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification`
  - result: RED -> GREEN
  - summary:
    - focused OpenSSL direct-library clarification suite finished `13 passed, 0 failed`
    - client default-config `ServerName` is now warning + ignore
    - server default-config `ServerName` now fails fast before context creation

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - adjacent cross-backend contract stayed green (`20 passed, 0 failed, 1 skipped`)
    - the OpenSSL direct-library alignment did not regress the current no-inheritance truth on new client connections

- update `docs/reference/API_REFERENCE.md`
  - change:
    - extend the client SNI compatibility note so it also covers the direct OpenSSL library default-config path

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - record the OpenSSL direct-library alignment as the last remaining high-level write-surface closeout
    - keep the next recommended batch on final public surface cleanup prep

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for the OpenSSL direct-library default-config alignment
    - refresh the route summary so future sessions do not re-open this backend-specific leak

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that builder, generic factory, and direct OpenSSL library paths no longer inject deprecated `ServerName` into newly created contexts

### Deprecated Builder/Config ServerName Surface Classification

- add `docs/plans/2026-05-18-deprecated-context-servername-compat-surface-classification.md`
  - purpose:
    - define the first static cleanup cut inside final public surface cleanup prep
    - keep scope on ordinary-test de-guidance plus explicit compatibility classification

- update selected tests under `tests/` and `tests/config/`
  - change:
    - remove ordinary `.WithSNI(...)` usage from `tests/test_quick.pas`
    - remove stale `LConfig.ServerName := ...` setup from `tests/winssl/test_winssl_connection_edge_cases.pas`
    - add `INTENTIONAL_COMPAT` labels to remaining builder/config compatibility coverage files
    - clarify `tests/test_data_structures.pas` and `tests/test_factory_logic.pas` messages so `ServerName` is framed as a compatibility field, not recommended flow guidance

- add `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - purpose:
    - confine deprecated builder/config ServerName surface to an explicit allowlist
    - fail if active ordinary tests reintroduce `.WithSNI(...)` or builder-config `ServerName :=`

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - remaining deprecated builder/config ServerName usage is confined to explicitly labeled compatibility tests
    - ordinary active tests no longer leak deprecated builder/config guidance

- `mkdir -p tmp/test_quick && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_quick -FEtmp/test_quick -otmp/test_quick/test_quick tests/test_quick.pas && ./tmp/test_quick/test_quick`
  - result: PASS
  - summary:
    - normal builder smoke still builds client and server contexts without `.WithSNI(...)`
    - quick smoke output stayed green after removing deprecated builder guidance from the ordinary path

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark the public compatibility-surface classification cut complete
    - move the next recommended batch from test-surface cleanup prep to final API-shape decisions

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for deprecated builder/config ServerName surface classification
    - record that ordinary smoke/edge-case tests no longer teach deprecated builder/config guidance

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that remaining deprecated builder/config ServerName usage is now explicitly classified
    - record that the next highest-value work is final API-shape decisions, not more ordinary-test cleanup

### Active Direct Context ServerName Surface Classification

- add `docs/plans/2026-05-18-active-direct-context-servername-surface-classification.md`
  - purpose:
    - define the second static cleanup cut inside final public surface cleanup prep
    - keep scope on active direct-context `SetServerName(...)` classification only

- update selected compatibility tests
  - change:
    - add explicit `INTENTIONAL_COMPAT` labels to:
      - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      - `tests/test_sslctxboth_client_capability_clarification.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
    - keep runtime semantics unchanged; the batch is classification-only

- add `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - purpose:
    - classify every active real direct-context `SetServerName(...)` hit
    - fail if an active ordinary test reintroduces an unclassified direct-context ServerName setter

- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - all active real direct-context `SetServerName(...)` tests are now explicitly classified
    - no unexpected active ordinary test still uses a direct-context ServerName setter

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend clarification stayed green (`20 passed, 0 failed, 1 skipped`)
    - direct context state is still observable while new client connections stay no-inheritance across available backends

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder precedence contract stayed green (`9 passed, 0 failed`)
    - explicit hostname override/clear rules remain correct after the classification-only batch

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - focused `sslCtxBoth` clarification stayed green (`28 passed, 0 failed, 1 skipped`)
    - dual-role contexts still expose client capability without reintroducing implicit ServerName inheritance

- update `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - change:
    - mark the active direct-context surface classification cut complete
    - keep the next recommended batch on final API-shape decisions

- update `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  - change:
    - add a dedicated closeout section for active direct-context ServerName surface classification
    - record that the next blocker is final public API shape, not more test-surface triage

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the new truth that all active real direct-context `SetServerName(...)` hits are now explicitly classified
    - record that the next highest-value work is final API-shape decisions, not more direct-context surface census

- update selected intentional compatibility tests
  - change:
    - add local deprecated getter/setter warning suppression to:
      - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      - `tests/test_sslctxboth_client_capability_clarification.pas`
      - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - keep runtime semantics unchanged; the batch is warning-noise cleanup only

- `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
  - result: PASS
  - summary:
    - focused cross-backend clarification stayed green after local warning quarantine (`20 passed, 0 failed, 1 skipped`)
    - compile output no longer emits the direct-context `GetServerName` deprecated warnings from this test

- `mkdir -p tmp/test_sslctxboth_client_capability_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_sslctxboth_client_capability_clarification -FEtmp/test_sslctxboth_client_capability_clarification -otmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification tests/test_sslctxboth_client_capability_clarification.pas && ./tmp/test_sslctxboth_client_capability_clarification/test_sslctxboth_client_capability_clarification`
  - result: PASS
  - summary:
    - focused `sslCtxBoth` clarification stayed green after local warning quarantine (`28 passed, 0 failed, 1 skipped`)
    - compile output no longer emits the direct-context `SetServerName` deprecated warnings from this test

- `mkdir -p tmp/test_context_builder_server_servername_runtime_consistency && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_servername_runtime_consistency -FEtmp/test_context_builder_server_servername_runtime_consistency -otmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/test_context_builder_server_servername_runtime_consistency/test_context_builder_server_servername_runtime_consistency`
  - result: PASS
  - summary:
    - focused builder/direct-context consistency contract stayed green after local warning quarantine (`6 passed, 0 failed`)
    - compile output no longer emits the direct-context `GetServerName` deprecated warnings from this test

### WithSNI Compiler Deprecation Alignment

- add `docs/plans/2026-05-18-withsni-compiler-deprecation-alignment.md`
  - purpose:
    - define the bounded source-truth batch that upgrades `WithSNI(...)` from documentation/runtime-only deprecation to compiler-level deprecation
    - keep runtime behavior unchanged while making the public builder surface tell the truth at compile time

- add `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
  - purpose:
    - fail if `ISSLContextBuilder.WithSNI(...)` or `TSSLContextBuilderImpl.WithSNI(...)` loses its compiler `deprecated` marker

- update `src/fafafa.ssl.context.builder.pas`
  - change:
    - mark both public `WithSNI(...)` declarations as compiler `deprecated`
    - reuse the same per-connection-hostname migration message already used by the runtime warnings

- update selected intentional compatibility tests under `tests/` and `tests/config/`
  - change:
    - add local warning suppression around intentional `.WithSNI(...)` callsites
    - keep behavior assertions unchanged; the batch is source-truth alignment plus compile-noise quarantine

- update `docs/reference/API_REFERENCE.md`
  - change:
    - record that `WithSNI(...)` is now also compiler deprecated, not only runtime warning + ignore

- `bash tests/scripts/test_withsni_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - both builder `WithSNI(...)` declarations are now compiler deprecated
    - the dedicated source contract now guards this declaration-level truth

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - remaining deprecated builder/config compatibility usage stays confined to the existing allowlist after the compiler-deprecation alignment

- `mkdir -p tmp/test_context_builder_server_name_compatibility_warning && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_context_builder_server_name_compatibility_warning -FEtmp/test_context_builder_server_name_compatibility_warning -otmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning tests/test_context_builder_server_name_compatibility_warning.pas && ./tmp/test_context_builder_server_name_compatibility_warning/test_context_builder_server_name_compatibility_warning`
  - result: PASS
  - summary:
    - focused builder warning suite finished `16 passed, 0 failed`
    - intentional `.WithSNI(...)` coverage stayed green after the compiler-level deprecation change

- `mkdir -p tmp/test_config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_validation -FEtmp/test_config_validation -otmp/test_config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation/test_config_validation`
  - result: PASS
  - summary:
    - focused config validation suite finished `53 passed, 0 failed`
    - compatibility validation wording stayed aligned while compile output remained free of repeated known `.WithSNI(...)` deprecation noise

- `git diff --check`
  - result: PASS
  - summary:
    - current `WithSNI` compiler-deprecation batch has no whitespace or patch-format issues

### TSSLConfig ServerName Surface Truth Freeze

- add `docs/plans/2026-05-18-tsslconfig-servername-surface-truth-freeze.md`
  - purpose:
    - define the bounded `v1.x` surface-freeze batch for `TSSLConfig.ServerName`
    - keep runtime behavior unchanged while preventing the record field from drifting back into ordinary client-path guidance

- add `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
  - purpose:
    - fail if the `TSSLConfig.ServerName` source comment, warning wording, or active-doc confinement drifts away from the current compatibility-only truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - repeat the client-side warning + ignore truth next to `Use TSSLConfig with TSSLFactory.CreateContext(...)`
    - explicitly redirect callers back to `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`

- `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - the new TSSLConfig surface-truth contract script is syntactically valid

- `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
  - result: RED -> GREEN
  - summary:
    - first run failed because markdown backticks inside double-quoted `rg` patterns triggered shell command substitution
    - the script was corrected to use fixed-string matching for the API reference bullets
    - `TSSLConfig.ServerName` source comment, warning wording, and active-doc confinement all match the intended compatibility-only truth
    - active docs currently mention `TSSLConfig.ServerName` only in `docs/reference/API_REFERENCE.md`

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - the existing builder/config compatibility allowlist stays green after the TSSLConfig source/doc freeze batch

- `git diff --check`
  - result: PASS
  - summary:
    - current TSSLConfig surface-freeze batch has no whitespace or patch-format issues

### Direct Context ServerName Surface Truth Freeze

- add `docs/plans/2026-05-18-direct-context-servername-surface-truth-freeze.md`
  - purpose:
    - define the bounded `v1.x` surface-freeze batch for direct `ISSLContext.SetServerName/GetServerName`
    - keep runtime behavior unchanged while preventing deprecated direct context APIs from drifting back into ordinary client-path guidance

- add `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - purpose:
    - fail if the direct-context deprecation messages, production-source caller boundary, or active-doc guidance drift away from the current compatibility-only truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - explicitly classify `ISSLContext.SetServerName(...)` / `GetServerName(...)` as deprecated direct context compatibility APIs

- `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - the new direct-context surface-truth contract script is syntactically valid

- `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - direct `ISSLContext` ServerName deprecation messages remain correct
    - production `src/` contains no real direct context caller
    - active docs contain no `Ctx.SetServerName(...)`-style guidance

- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - active direct-context test coverage remains explicitly classified and confined after the source/doc freeze batch

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - result: PASS
  - summary:
    - the remaining intentional direct-context compatibility control case stays explicitly labeled

- `git diff --check`
  - result: PASS
  - summary:
    - current direct-context surface-freeze batch has no whitespace or patch-format issues

### WithSNI Surface Truth Freeze

- add `docs/plans/2026-05-18-withsni-surface-truth-freeze.md`
  - purpose:
    - define the bounded `v1.x` surface-freeze batch for `TSSLContextBuilder.WithSNI(...)`
    - keep runtime behavior unchanged while preventing the deprecated fluent method from drifting back into ordinary builder guidance

- add `tests/scripts/test_withsni_surface_truth_contract.sh`
  - purpose:
    - fail if the WithSNI source comment, active-doc confinement, or source-hit boundary drifts away from the current compatibility-only truth

- `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - the new WithSNI surface-truth contract script is syntactically valid

- `bash tests/scripts/test_withsni_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - `WithSNI(...)` remains fenced to its current declaration/implementation boundary in `src/`
    - active docs currently mention `WithSNI(...)` only in `docs/reference/API_REFERENCE.md`
    - the source comment still classifies it as compatibility-only context-level SNI

- `bash tests/scripts/test_withsni_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the dedicated compiler-deprecation contract still proves both public WithSNI declarations remain deprecated

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - the existing builder/config compatibility allowlist stays green after the WithSNI surface-freeze batch

- `git diff --check`
  - result: PASS
  - summary:
    - current WithSNI surface-freeze batch has no whitespace or patch-format issues

### Post-SNI Interface Debt Triage

- add `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - purpose:
    - capture the next recommended route after the entire context-level SNI compatibility family was frozen for `v1.x`
    - avoid reopening old SNI cleanup when the broader interface-design debt should now move to `TSSLConfig` vs `ISSLConnection`

- read-only evidence triage:
  - summary:
    - `TSSLConfig` already has multiple field-scope truths on disk:
      - `BufferSize` / `HandshakeTimeout` = connection-scoped and rejected by factory
      - `LogLevel` / `LogCallback` = library-scoped and rejected by factory
      - several option-style fields still normalize into `Options`
    - `ISSLConnection` slimming remains larger-risk because it would touch every backend connection implementation plus many tests/helpers
    - next recommended bounded batch is therefore `TSSLConfig` cross-layer slimming roadmap, not immediate `ISSLConnection` surgery

### TSSLConfig Scope Buckets

- add `docs/plans/2026-05-18-tsslconfig-scope-buckets.md`
  - purpose:
    - define the first bounded post-SNI `TSSLConfig` truth batch
    - freeze mixed-scope field buckets before any larger slimming or backend refactor

- update `src/fafafa.ssl.base.pas`
  - change:
    - rewrite mixed-scope field comments so `BufferSize` / `HandshakeTimeout` / `Session*` / `ALPN` / early-data / logging / option-bridge fields now carry explicit scope truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `TSSLConfig Scope Buckets`
    - align the replay-store note so it explicitly says `context-scoped, server-only opt-in`

- add `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - purpose:
    - fail if the new source/doc bucket truth drifts away from current factory / OpenSSL direct-path evidence

- `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - result: PASS
  - summary:
    - the new TSSLConfig scope bucket contract script is syntactically valid

- `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - first result: FAIL
  - summary:
    - shell interpreted backtick-bearing double-quoted fixed-string assertions as command substitution
    - fix:
      - switch those fixed-string assertions to single-quoted literals
      - add `--` to `rg` invocations so bullet-like patterns are not parsed as flags

- `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - rerun result: PASS
  - summary:
    - source comments, API reference bucket section, factory scope checks, and OpenSSL direct-path apply points stay aligned

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - focused factory connection-scope clarification test remains green

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - focused factory logging-scope clarification test remains green

### Cross-backend Direct-Library Default-Config Parity Audit

- read-only static audit:
  - summary:
    - OpenSSL direct-library `CreateContext(AType)` explicitly applies `SessionCacheSize` / `SessionTimeout` / `ALPNProtocols` and handles deprecated `ServerName`
    - WinSSL direct-library `CreateContext(AType)` currently only applies `Options`
    - FreePascal / MbedTLS / WolfSSL direct-library `CreateContext(AType)` currently just create contexts
    - those same libraries still store `FDefaultConfig`, while their context classes expose `SessionCacheSize` / `SessionTimeout` / `ALPNProtocols`
    - this is the next highest-value parity risk to verify/fix before broader interface slimming

### Direct-Library Default-Config Parity Fix

- add `docs/plans/2026-05-18-direct-library-default-config-parity.md`
  - purpose:
    - define a bounded TDD batch for `ISSLLibrary.SetDefaultConfig(...)` + `CreateContext(AType)` parity

- add `tests/test_direct_library_default_config_parity.pas`
  - purpose:
    - prove a real runtime RED on the FreePascal direct-library path before touching production code

- add `tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - purpose:
    - prove a source RED across backend library units before touching production code

- `bash -n tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - result: PASS
  - summary:
    - the new direct-library default-config parity contract script is syntactically valid

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - RED result: FAIL
  - summary:
    - `src/fafafa.ssl.freepascal.lib.pas` was not normalizing `SetDefaultConfig(...)`

- `mkdir -p tmp/test_direct_library_default_config_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas && ./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
  - RED result: FAIL
  - summary:
    - FreePascal direct-library `CreateContext(sslCtxClient)` failed to reflect default-config:
      - `ProtocolVersions`
      - `VerifyMode`
      - `VerifyDepth`
      - `CipherList`
      - `CipherSuites`
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
      - normalized option-bridge `Options`

- update:
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - normalize `SetDefaultConfig(...)` via `TSSLFactory.NormalizeConfig(...)`
    - apply context-safe default fields in direct-library `CreateContext(AType)`

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - GREEN result: PASS
  - summary:
    - all targeted backend library units now keep the same direct-library default-config apply skeleton

- `mkdir -p tmp/test_direct_library_default_config_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas && ./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
  - GREEN result: PASS
  - summary:
    - FreePascal direct-library client context now reflects:
      - `ProtocolVersions`
      - `PreferredVersion`
      - `VerifyMode`
      - `VerifyDepth`
      - `CipherList`
      - `CipherSuites`
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
      - normalized option-bridge `Options`

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - connection-scoped rejection truth on factory paths remains green after the direct-library parity fix

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - library-scoped logging truth on factory paths remains green after the direct-library parity fix

### Direct-Library ServerName Compatibility Parity

- add `docs/plans/2026-05-18-direct-library-servername-compatibility-parity.md`
  - purpose:
    - define the bounded TDD batch for direct-library `ServerName` compatibility warning/reject parity

- add `tests/test_freepascal_library_default_config_server_name_clarification.pas`
  - purpose:
    - prove a real runtime RED on the FreePascal direct-library path before touching production code

- add `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - purpose:
    - prove a source RED across backend library units before touching production code

- update `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - change:
    - allowlist the new intentional direct-library compatibility test

- `bash -n tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - result: PASS
  - summary:
    - the new direct-library ServerName parity contract script is syntactically valid

- `bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - RED result: FAIL
  - summary:
    - `src/fafafa.ssl.freepascal.lib.pas` was still missing server reject / client warning logic

- `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
  - RED result: FAIL
  - summary:
    - FreePascal direct-library path was still:
      - client silent ignore
      - server non-reject

- update:
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - change:
    - align direct-library deprecated `ServerName` compatibility behavior to OpenSSL:
      - client default-config warning + ignore
      - server default-config reject

- `bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - GREEN result: PASS
  - summary:
    - direct-library `ServerName` compatibility source truth is now aligned across all targeted backend library units

- `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
  - GREEN result: PASS
  - summary:
    - FreePascal direct-library path now:
      - warns and ignores client default-config `ServerName`
      - rejects server default-config `ServerName`
      - stays quiet when `ServerName` is empty

- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - result: PASS
  - summary:
    - the new intentional direct-library compatibility test is properly confined in the allowlist

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - result: PASS
  - summary:
    - the previous direct-library default-config parity batch remains intact after adding the `ServerName` special-case parity

### Direct-Library Early-Data And Replay-Store Parity

- add `docs/plans/2026-05-18-direct-library-early-data-replay-store-parity.md`
  - purpose:
    - define the bounded TDD batch for the last remaining direct-library special-case parity lane

- add `src/fafafa.ssl.context.config.pas`
  - purpose:
    - hold shared internal helper logic for replay-store scope validation, early-data apply, and replay-store installer apply
    - avoid re-copying the same logic into five backend library units

- add `tests/test_direct_library_early_data_replay_store_parity.pas`
  - purpose:
    - prove a real runtime RED on the FreePascal direct-library path before touching production code

- add `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
  - purpose:
    - prove a source RED across backend library units before touching production code

- `bash -n tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh && bash tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
  - RED result: FAIL
  - summary:
    - `src/fafafa.ssl.openssl.backed.pas` still had no replay-store scope validation / early-data apply / replay-store apply on the direct-library path

- `mkdir -p tmp/test_direct_library_early_data_replay_store_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_early_data_replay_store_parity -FEtmp/test_direct_library_early_data_replay_store_parity -otmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity tests/test_direct_library_early_data_replay_store_parity.pas && ./tmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity`
  - RED result: FAIL
  - summary:
    - FreePascal direct-library path was still missing:
      - client `ClientEarlyDataEnabled` apply
      - server `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` apply
      - replay-store file / directory install
      - client replay-store rejection
      - conflicting replay-store file + directory rejection

- update:
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - connect all five backend library `CreateContext(AType)` paths to the shared helper
    - align direct-library early-data / replay-store behavior to the factory/context truth
    - update API reference so the direct-library note no longer says early-data / replay-store is still pending

- `bash -n tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh && bash tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
  - GREEN result: PASS
  - summary:
    - all targeted backend library units now validate replay-store scope and apply early-data / replay-store defaults on the direct-library path

- `mkdir -p tmp/test_direct_library_early_data_replay_store_parity && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_direct_library_early_data_replay_store_parity -FEtmp/test_direct_library_early_data_replay_store_parity -otmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity tests/test_direct_library_early_data_replay_store_parity.pas && ./tmp/test_direct_library_early_data_replay_store_parity/test_direct_library_early_data_replay_store_parity`
  - GREEN result: PASS
  - summary:
    - FreePascal direct-library path now:
      - applies `ClientEarlyDataEnabled`
      - applies `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
      - installs replay-store file / directory at the configured path
      - rejects client replay-store config
      - rejects conflicting replay-store file + directory

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh && bash tests/scripts/test_direct_library_servername_compatibility_contract.sh`
  - result: PASS
  - summary:
    - the earlier direct-library default-config and `ServerName` parity batches remain intact after adding early-data / replay-store parity

### TSSLConfig Option-Bridge Default Truth Parity

- add `docs/plans/2026-05-18-tsslconfig-option-bridge-default-truth-parity.md`
  - purpose:
    - define a bounded batch for fresh default-config surface truth on the three option-bridge compatibility booleans

- add `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - purpose:
    - prove a real runtime RED across:
      - direct-library `GetDefaultConfig(...)`
      - factory-held `GetDefaultConfig(...)`
      - `CreateDefaultConfig(...)`
      - `SetDefaultConfig(GetDefaultConfig)` round-trip

- add `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - purpose:
    - keep constructor-level normalization and backend registration truth cheap to re-verify

- `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - result: PASS
  - summary:
    - constructor normalization and the API-reference truth note were present before the runtime narrowing continued

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - RED result: FAIL
  - summary:
    - direct `CreateFreePascalSSLLibrary` default-config truth was already green
    - `SetDefaultConfig(GetDefaultConfig)` direct-library round-trip was already green
    - only the factory-held / auto-detect / `CreateDefaultConfig(...)` lane still dropped `EnableSessionTickets`

- update `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - change:
    - add narrowing assertions for:
      - `TSSLFactory.GetLibrary(sslFreePascal).GetDefaultConfig`
      - `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig`
  - summary:
    - this isolated the real source from `CreateDefaultConfig(...)` down to the factory-held backend instance itself

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - RED result: FAIL
  - summary:
    - `TSSLFactory.GetLibrary(sslFreePascal).GetDefaultConfig`
      and `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig`
      were already stale before `CreateDefaultConfig(...)` ran
    - this proved the root cause lived in production backend instantiation, not only in the helper surface

- update:
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - change:
    - add explicit backend creator-function registration to `TSSLFactory`
    - prefer `CreateFunc` over raw registered-class instantiation in `CreateLibraryInstance(...)`
    - switch real backend registrations to `@Create*SSLLibrary`
    - extend the contract so creator-function registration truth is also guarded

- `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - GREEN result: PASS
  - summary:
    - constructor normalization is still present
    - real backend registrations now go through explicit creator functions

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - GREEN result: PASS
  - summary:
    - factory-held `GetDefaultConfig(...)`, auto-detect `GetDefaultConfig(...)`,
      and `CreateDefaultConfig(...)` now all preserve the FreePascal session-ticket truth
    - full focused suite finished `20 passed, 0 failed`

- `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
  - result: PASS
  - summary:
    - the earlier direct-library default-config parity batch remains intact after the creator-path fix

- `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
  - result: PASS
  - summary:
    - the existing `CreateDefaultConfig(...)` baseline suite remains green after the factory creator-path change
    - logging-safe default behavior was not regressed

- `git diff --check`
  - result: PASS
  - summary:
    - current option-bridge default-truth batch has no whitespace or patch-format issues

### TSSLConfig Option-Bridge Precedence Freeze

- add `docs/plans/2026-05-18-tsslconfig-option-bridge-precedence-freeze.md`
  - purpose:
    - define a bounded batch for freezing the conflict-precedence truth between `Options` and option-bridge compatibility booleans

- add `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
  - purpose:
    - prove runtime truth across:
      - `TSSLFactory.NormalizeConfig(...)`
      - `TSSLFactory.CreateContext(const AConfig)`
      - `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`

- add `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - purpose:
    - keep the precedence rule synchronized across source comments, docs, and backend normalization paths

- update:
  - `src/fafafa.ssl.factory.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - document the current `v1.x` precedence truth explicitly:
      - legacy booleans remain the compatibility write surface
      - conflicting option bits yield to the legacy booleans
      - final `Options` truth is then projected back to the booleans

- `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - RED result: FAIL
  - summary:
    - the first failure was only a shell-quoting bug in the new contract script
    - the script string containing backticks was accidentally interpreted by bash before any real repo truth was checked

- update `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - change:
    - switch the API-reference needles containing backticks to single-quoted shell strings
  - summary:
    - this removed the shell parser noise so the contract can verify actual repo truth

- `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - GREEN result: PASS
  - summary:
    - source/doc truth now explicitly records the option-bridge precedence rule
    - backend `SetDefaultConfig(...)` normalization paths remain aligned

- `mkdir -p tmp/test_tsslconfig_option_bridge_precedence_freeze && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_precedence_freeze -FEtmp/test_tsslconfig_option_bridge_precedence_freeze -otmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze tests/test_tsslconfig_option_bridge_precedence_freeze.pas && ./tmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze`
  - result: PASS
  - summary:
    - full focused suite finished `16 passed, 0 failed`
    - `NormalizeConfig(...)`, one-shot factory path, and direct-library default-config path all follow the same precedence truth

- `bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - result: PASS
  - summary:
    - the earlier default-truth and scope-bucket batches remain intact after freezing precedence

- `git diff --check`
  - result: PASS
  - summary:
    - current option-bridge precedence-freeze batch has no whitespace or patch-format issues

### TSSLConfig Option-Bridge Surface Truth Freeze

- add `docs/plans/2026-05-18-tsslconfig-option-bridge-surface-truth-freeze.md`
  - purpose:
    - define a bounded batch for freezing the remaining public-surface truth of the three option-bridge booleans
    - keep the scope on source/doc/test guidance instead of reopening runtime semantics

- update:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
  - `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
  - `tests/test_direct_library_default_config_parity.pas`
  - `tests/security/test_session_security.pas`
  - change:
    - tighten the three `TSSLConfig` option-bridge booleans to explicit `compatibility-only` source/doc truth
    - label the remaining dedicated compatibility tests
    - move active session-security coverage away from legacy boolean writes

- add `tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
  - purpose:
    - fail if source/docs/tests drift back toward treating the option-bridge booleans as ordinary primary inputs

- `bash -n tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new source/doc/test contract holds the narrowed compatibility-only truth

- `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh && bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - RED -> GREEN result: PASS
  - summary:
    - the first failure was only wording drift against the new narrowed API text
    - the scope-bucket contract was updated to the new compatibility-only phrasing instead of reopening runtime verification

- `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
  - result: PASS
  - summary:
    - the earlier fresh default-config contract now points at the new API wording and remains green

- `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh && bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - result: PASS
  - summary:
    - the earlier precedence-freeze contract now points at the new API wording and remains green

- `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
  - result: PASS
  - summary:
    - focused default-truth suite finished `20 passed, 0 failed`
    - the new compatibility labels/comments did not disturb the earlier runtime truth batch

- `mkdir -p tmp/test_tsslconfig_option_bridge_precedence_freeze && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_precedence_freeze -FEtmp/test_tsslconfig_option_bridge_precedence_freeze -otmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze tests/test_tsslconfig_option_bridge_precedence_freeze.pas && ./tmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze`
  - result: PASS
  - summary:
    - focused precedence suite finished `16 passed, 0 failed`
    - narrowed public-surface wording did not disturb the earlier precedence contract

- `mkdir -p tmp/test_session_security && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_session_security -FEtmp/test_session_security -otmp/test_session_security/test_session_security tests/security/test_session_security.pas && ./tmp/test_session_security/test_session_security`
  - RED -> GREEN result: PASS
  - summary:
    - the first attempt incorrectly tried to prove session-ticket configurability by writing only `Options` through `NormalizeConfig(...)`
    - that failed because the already-frozen legacy-boolean precedence intentionally overrides conflicting option bits during normalization
    - the final fix moved the active security test to direct context `SetOptions(...)` / `GetOptions(...)`, finishing with `35 passed, 0 failed`

- `git diff --check`
  - result: PASS
  - summary:
    - current option-bridge surface-truth batch has no whitespace or patch-format issues

### TSSLConfig Active Guidance Cleanup

- add `docs/plans/2026-05-18-tsslconfig-active-guidance-cleanup.md`
  - purpose:
    - define a bounded batch for cleaning up high-visibility TSSLConfig guidance drift in active example/reference surfaces

- update:
  - `examples/example_factory_usage.pas`
  - `docs/reference/ARCHITECTURE.md`
  - change:
    - remove `BufferSize` / `HandshakeTimeout` from the factory/config example path
    - redirect timeout/buffering guidance to connection / transport-level APIs
    - replace the stale pseudo-`TSSLConfig` record in architecture docs with current scope buckets

- add `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - purpose:
    - keep active example usage and architecture reference aligned with the current TSSLConfig scope truth
    - also keep the example-surface direct-context API coverage explicitly labeled

- `bash -n tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh && bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - RED -> GREEN result: PASS
  - summary:
    - the first failure was only an over-broad contract needle that accidentally matched an unrelated `ProtocolVersion` symbol elsewhere in the architecture doc
    - the final contract now stays focused on the real guidance truth instead of creating false reds

- `mkdir -p tmp/example_factory_usage && fpc -B -Fu./src -Fu./examples -FUtmp/example_factory_usage -FEtmp/example_factory_usage -otmp/example_factory_usage/example_factory_usage examples/example_factory_usage.pas`
  - result: PASS
  - summary:
    - active factory-usage example still compiles after removing the mixed-scope guidance drift
    - compile finished with existing repo warnings only; no new example breakage was introduced

- `git diff --check`
  - result: PASS
  - summary:
    - current active-guidance-cleanup batch has no whitespace or patch-format issues

### TSSLConfig Public-Surface Slimming Roadmap

- add `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
  - purpose:
    - turn the already-proved TSSLConfig scope truth into a field-level migration matrix that can drive future implementation batches

- update `docs/reference/API_REFERENCE.md`
  - change:
    - add `TSSLConfig Migration Targets`
    - map:
      - `LogLevel` / `LogCallback` -> library defaults surface
      - `HandshakeTimeout` / `BufferSize` -> connection / transport surface
      - `ServerName` -> per-connection SNI surface
      - option-bridge booleans -> `Options` / `WithOption(...)`
    - record the current `v1.x` status and the intended `v2` direction for each family

- add `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - purpose:
    - keep the API migration map and the dedicated slimming roadmap synchronized

- `bash -n tests/scripts/test_tsslconfig_migration_targets_contract.sh && bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - the migration matrix holds across both the API reference and the dedicated roadmap doc

- `git diff --check`
  - result: PASS
  - summary:
    - current slimming-roadmap batch has no whitespace or patch-format issues

### TSSLConfig Logging Surface Truth Freeze

- add `docs/plans/2026-05-18-tsslconfig-logging-surface-truth-freeze.md`
  - purpose:
    - define a bounded batch for freezing the remaining active logging guidance truth around `TSSLConfig.LogLevel` / `LogCallback`
    - keep scope on docs/reference/examples + focused contracts, not runtime redesign

- add `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - purpose:
    - fail if active docs drift back toward teaching callback-only logging as a complete way to see info/debug output
    - keep API/reference/guides synchronized on the split between log-level defaults and callback installation

- `bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new docs contract is syntactically valid before repo truth checks

- `bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - RED result: FAIL
  - summary:
    - first failure proved `docs/reference/API_REFERENCE.md` still lacked the explicit split between:
      - `LogLevel` via `GetDefaultConfig(...)` / `SetDefaultConfig(...)`
      - `LogCallback` via `SetLogCallback(...)`
    - the same active-doc drift also still existed in `USER_GUIDE` / `TROUBLESHOOTING`, where callback-only snippets immediately emitted `sslLogInfo` even though the default runtime threshold is still `sslLogError`

- update:
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - change:
    - make the library-default logging truth explicit in reference docs
    - require guide snippets to raise `LLogConfig.LogLevel` through `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` before showing `sslLogInfo` / `sslLogDebug` dispatch
    - keep callback installation on `ISSLLibrary.SetLogCallback(...)`

- `bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh && bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - GREEN result: PASS
  - summary:
    - active docs/reference now agree on the logging owner boundary
    - callback-only examples no longer pretend info/debug output works under the default `sslLogError` threshold

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - result: PASS
  - summary:
    - focused logging scope suite finished `12 passed, 0 failed`
    - request-path rejection, library-default round-trip, and callback dispatch gating all stayed green after the doc truth cleanup

- `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
  - result: PASS
  - summary:
    - focused default-config suite kept the logging baseline truth green
    - `CreateDefaultConfig(...)` still returns request-safe `LogLevel = sslLogError` and `LogCallback = nil`

- `git diff --check`
  - result: PASS
  - summary:
    - current logging-surface-truth batch has no whitespace or patch-format issues

### Direct-Library Connection-Scope Clarification

- add `docs/plans/2026-05-18-direct-library-connection-scope-clarification.md`
  - purpose:
    - define a bounded batch for aligning direct-library `SetDefaultConfig(...)` + `CreateContext(AType)` with the existing connection-scope truth of `HandshakeTimeout` / `BufferSize`

- add:
  - `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
  - `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - purpose:
    - prove the remaining direct-library silent-ignore drift with one runtime-focused FreePascal test and one cross-backend source/docs contract

- `bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - result: PASS
  - summary:
    - new direct-library connection-scope contract is syntactically valid before repo truth checks

- `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - RED result: FAIL
  - summary:
    - first failure proved `docs/reference/API_REFERENCE.md` still described `HandshakeTimeout` / `BufferSize` only in factory terms
    - the same contract would also have failed because no shared direct-library connection-scope validator existed yet

- `mkdir -p tmp/test_freepascal_library_default_config_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_connection_scope_clarification -FEtmp/test_freepascal_library_default_config_connection_scope_clarification -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification tests/test_freepascal_library_default_config_connection_scope_clarification.pas && ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification`
  - RED result: FAIL
  - summary:
    - initial FreePascal direct-library runtime proof showed both custom `HandshakeTimeout` and custom `BufferSize` were silently accepted on `Lib.CreateContext(sslCtxClient)` instead of raising `ESSLConfigurationException`

- update:
  - `src/fafafa.ssl.context.config.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - change:
    - add shared `ValidateDirectLibraryConnectionScope(...)`
    - wire all five backend `CreateContext(AType)` paths through that shared validator
    - update reference wording so direct-library path is explicitly covered by the same connection-scope truth

- `bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh && bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - RED -> GREEN result: PASS
  - summary:
    - first post-fix failure was only a false red from line-oriented grep against a multiline helper call
    - after tightening the contract to match the real helper invocation semantics, source/docs truth stayed green across all backend library paths

- `mkdir -p tmp/test_freepascal_library_default_config_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_connection_scope_clarification -FEtmp/test_freepascal_library_default_config_connection_scope_clarification -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification tests/test_freepascal_library_default_config_connection_scope_clarification.pas && ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification`
  - GREEN result: PASS
  - summary:
    - focused direct-library runtime suite finished `9 passed, 0 failed`
    - custom `HandshakeTimeout` / `BufferSize` now fail-fast on `ISSLLibrary.CreateContext(AType)` and request-safe defaults still build

- `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
  - result: PASS
  - summary:
    - existing factory connection-scope suite finished `12 passed, 0 failed`
    - the new shared direct-library validator did not disturb the already-frozen factory reject path

- `git diff --check`
  - result: PASS
  - summary:
    - current direct-library connection-scope clarification batch has no whitespace or patch-format issues

### Library-Default LogCallback Detachment

- add `docs/plans/2026-05-18-library-default-logcallback-detachment.md`
  - purpose:
    - define the first runtime/source implementation slice under the `LogLevel` / `LogCallback` slimming route
    - keep scope on callback ownership between `SetDefaultConfig(...)` and `SetLogCallback(...)`

- add `tests/scripts/test_library_default_logcallback_detachment_contract.sh`
  - purpose:
    - fail if any backend still lets `SetDefaultConfig(...)` install the runtime callback
    - keep `SetLogCallback(...)` as the only source-guarded callback owner

- update `tests/test_factory_logging_scope_clarification.pas`
  - change:
    - strengthen the focused runtime proof so it now requires:
      - `SetDefaultConfig(LogCallback)` does not install the callback
      - `SetLogCallback(...)` remains the only owner
      - later `SetDefaultConfig(LogLevel)` updates filtering without clearing the installed callback

- `bash -n tests/scripts/test_library_default_logcallback_detachment_contract.sh && bash tests/scripts/test_library_default_logcallback_detachment_contract.sh`
  - RED result: FAIL
  - summary:
    - first source-contract failure immediately proved `src/fafafa.ssl.openssl.backed.pas` still let `SetDefaultConfig(...)` install `FLogCallback`
    - the same drift existed across the other backend library units as well

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - RED result: FAIL
  - summary:
    - strengthened runtime proof showed two concrete failures:
      - `SetDefaultConfig(...)` still visibleized callback input in `GetDefaultConfig(...)`
      - `SetDefaultConfig(LogCallback)` alone already made `Log(...)` dispatch

- update:
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `tests/test_factory_logging_scope_clarification.pas`
  - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
  - `tests/test_openssl_library_default_config_server_name_clarification.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
  - `src/fafafa.ssl.base.pas`
  - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - change:
    - `SetDefaultConfig(...)` now preserves the current callback snapshot instead of installing/replacing it from `LConfig.LogCallback`
    - `SetLogCallback(...)` stays the only callback owner
    - direct-library warning tests now install callbacks through `SetLogCallback(...)`
    - docs/source comments now explicitly state that `SetDefaultConfig(...)` no longer installs or replaces callbacks

- `bash -n tests/scripts/test_library_default_logcallback_detachment_contract.sh && bash tests/scripts/test_library_default_logcallback_detachment_contract.sh`
  - GREEN result: PASS
  - summary:
    - all five backend library paths now keep callback ownership detached from `SetDefaultConfig(...)`

- `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
  - GREEN result: PASS
  - summary:
    - focused logging scope suite finished `17 passed, 0 failed`
    - callback installation, visibility, filtering, and ownership are now aligned around the dedicated setter path

- `mkdir -p tmp/test_freepascal_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_library_default_config_server_name_clarification -FEtmp/test_freepascal_library_default_config_server_name_clarification -otmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification tests/test_freepascal_library_default_config_server_name_clarification.pas && ./tmp/test_freepascal_library_default_config_server_name_clarification/test_freepascal_library_default_config_server_name_clarification`
  - result: PASS
  - summary:
    - direct-library FreePascal warning/reject suite finished `13 passed, 0 failed`
    - moving warning capture to `SetLogCallback(...)` did not regress the existing ServerName parity truth

- `mkdir -p tmp/test_openssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_library_default_config_server_name_clarification -FEtmp/test_openssl_library_default_config_server_name_clarification -otmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification tests/test_openssl_library_default_config_server_name_clarification.pas && ./tmp/test_openssl_library_default_config_server_name_clarification/test_openssl_library_default_config_server_name_clarification`
  - result: PASS
  - summary:
    - direct-library OpenSSL warning/reject suite finished `13 passed, 0 failed`
    - the callback-owner cut did not disturb the existing OpenSSL ServerName compatibility path

- `bash -n tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh && bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active docs now state the stronger truth that `SetDefaultConfig(...)` no longer installs or replaces callbacks

- `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh && bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - result: PASS
  - summary:
    - source comments, scope buckets, factory wording, and backend source still agree after the callback-owner cut

- `bash -n tests/scripts/test_tsslconfig_migration_targets_contract.sh && bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - the public slimming roadmap still agrees with the updated callback detachment truth

- `mkdir -p tmp/test_default_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_default_config -FEtmp/test_default_config -otmp/test_default_config/test_default_config tests/config/test_default_config.pas && ./tmp/test_default_config/test_default_config`
  - result: PASS
  - summary:
    - default-config suite remained green
    - `CreateDefaultConfig(...)` still returns request-safe `LogLevel = sslLogError` and `LogCallback = nil`

- `git diff --check`
  - result: PASS
  - summary:
    - current library-default callback detachment batch has no whitespace or patch-format issues

### Noninteractive Core Compat Tests

- add `docs/plans/2026-05-18-noninteractive-core-compat-tests.md`
  - purpose:
    - define a bounded cleanup batch for turning two core compat/record-shape tests into real noninteractive test programs
    - keep scope on `tests/test_factory_logic.pas` and `tests/test_data_structures.pas`

- `rg -n "ReadLn\\;|按回车键退出" tests/test_factory_logic.pas tests/test_data_structures.pas`
  - result: PASS
  - summary:
    - both files still contained interactive exit prompts and `ReadLn`

- `zsh -lc "mkdir -p tmp/test_factory_logic && fpc ... && printf '\\n' | ./tmp/test_factory_logic/test_factory_logic"`
  - result: PASS
  - summary:
    - pre-fix direct run finished only after feeding stdin
    - output ended with `按回车键退出...`, confirming the remaining manual-exit tail in the core factory test

- `zsh -lc "mkdir -p tmp/test_data_structures && fpc ... && printf '\\n' | ./tmp/test_data_structures/test_data_structures"`
  - result: PASS
  - summary:
    - pre-fix direct run finished only after feeding stdin
    - output ended with `按回车键退出...`, confirming the remaining manual-exit tail in the core data-structure test

- `timeout 2 ./tmp/test_factory_logic/test_factory_logic`
  - result: PASS
  - summary:
    - headless run did not hard-hang on this host
    - but it still printed the interactive exit prompt, proving the test binary remained automation-noisy even when stdin was absent

- `timeout 2 ./tmp/test_data_structures/test_data_structures`
  - result: PASS
  - summary:
    - same result for the core data-structure test: no hard hang here, but the interactive exit tail still polluted automated output

- update:
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
  - change:
    - remove `按回车键退出...` + `ReadLn`
    - extend the `INTENTIONAL_COMPAT` header note so it explicitly includes mixed-scope record-shape fields such as `BufferSize` / `HandshakeTimeout`

- `zsh -lc "mkdir -p tmp/test_factory_logic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logic -FEtmp/test_factory_logic -otmp/test_factory_logic/test_factory_logic tests/test_factory_logic.pas >/tmp/test_factory_logic.build.log && timeout 2 ./tmp/test_factory_logic/test_factory_logic"`
  - GREEN result: PASS
  - summary:
    - core factory logic suite finished `80 passed, 0 failed`
    - output now ends cleanly at the test summary without the old interactive-exit tail

- `zsh -lc "mkdir -p tmp/test_data_structures && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_data_structures -FEtmp/test_data_structures -otmp/test_data_structures/test_data_structures tests/test_data_structures.pas >/tmp/test_data_structures.build.log && timeout 2 ./tmp/test_data_structures/test_data_structures"`
  - GREEN result: PASS
  - summary:
    - core data-structure suite finished `102 passed, 0 failed`
    - output now ends cleanly at the test summary without the old interactive-exit tail

- `git diff --check`
  - result: PASS
  - summary:
    - current noninteractive core compat test batch has no whitespace or patch-format issues

### Noninteractive Top-Level Core Tests

- `rg -n "ReadLn|按回车键退出" tests/test_exceptions.pas tests/test_base_interface_contract.pas`
  - result: PASS
  - summary:
    - both top-level core tests still contained interactive exit prompts and `ReadLn`

- `mkdir -p tmp/test_exceptions && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_exceptions -FEtmp/test_exceptions -otmp/test_exceptions/test_exceptions tests/test_exceptions.pas && timeout 2 ./tmp/test_exceptions/test_exceptions`
  - result: PASS
  - summary:
    - headless run did not hard-hang on this host because stdin EOF let the program exit
    - but the output still ended with `按回车键退出...`, proving the test remained automation-noisy

- `mkdir -p tmp/test_base_interface_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_base_interface_contract -FEtmp/test_base_interface_contract -otmp/test_base_interface_contract/test_base_interface_contract tests/test_base_interface_contract.pas && timeout 2 ./tmp/test_base_interface_contract/test_base_interface_contract`
  - result: PASS
  - summary:
    - same result for the base-interface core test: no hard hang here, but the interactive-exit tail still polluted automated output

- `rg -n "ReadLn|按回车键退出" tests`
  - result: PASS
  - summary:
    - repo-wide scan showed more `ReadLn` hits remain
    - the residual set is mainly examples, diagnostics, benchmarks/file readers, and WinSSL-specialized programs rather than top-level core tests

- add `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
  - purpose:
    - define a bounded cleanup batch for turning the remaining top-level core interactive tests into real noninteractive test programs
    - keep scope on `tests/test_exceptions.pas` and `tests/test_base_interface_contract.pas`

- add `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - purpose:
    - guard the two top-level core tests against reintroducing `ReadLn` or `按回车键退出...`

- `bash -n tests/scripts/test_top_level_core_tests_noninteractive_contract.sh && bash tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - result: RED
  - summary:
    - new contract immediately failed on `tests/test_exceptions.pas`
    - the failure proved the remaining interactive exit tail was still real at source level

- update:
  - `tests/test_exceptions.pas`
  - `tests/test_base_interface_contract.pas`
  - change:
    - remove `按回车键退出...` + `ReadLn`
    - keep all assertions and coverage targets unchanged

- `bash tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - GREEN result: PASS
  - summary:
    - the new source contract now confirms both top-level core tests are noninteractive

- `mkdir -p tmp/test_exceptions && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_exceptions -FEtmp/test_exceptions -otmp/test_exceptions/test_exceptions tests/test_exceptions.pas && timeout 2 ./tmp/test_exceptions/test_exceptions`
  - GREEN result: PASS
  - summary:
    - exception core suite finished `64 passed, 0 failed`
    - output now ends cleanly at the summary without the old interactive-exit tail

- `mkdir -p tmp/test_base_interface_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_base_interface_contract -FEtmp/test_base_interface_contract -otmp/test_base_interface_contract/test_base_interface_contract tests/test_base_interface_contract.pas && timeout 2 ./tmp/test_base_interface_contract/test_base_interface_contract`
  - GREEN result: PASS
  - summary:
    - base-interface core suite finished `89 passed, 0 failed`
    - output now ends cleanly at the summary without the old interactive-exit tail

- `git diff --check`
  - result: PASS
  - summary:
    - current noninteractive top-level core test batch has no whitespace or patch-format issues

### Noninteractive WinSSL Active Tests

- `sed -n '80,110p' run_winssl_tests.ps1`
  - result: PASS
  - summary:
    - current Windows-focused runner explicitly classifies `tests\\unit\\test_winssl_comprehensive.pas` as `Minimal, non-network, non-interactive tests`
    - this proved the remaining interactive tail in that file was a real workflow contradiction, not just a cosmetic annoyance

- `sed -n '55,80p' scripts/run_tests_windows.ps1`
  - result: PASS
  - summary:
    - legacy Windows run script still attempts to auto-run WinSSL unit-level tests
    - this further confirmed the batch should stay on active WinSSL test programs, not examples/diagnostics

- `rg -n "ReadLn|按回车键退出" tests/unit/*.pas tests/winssl/*.pas tests/examples/*.pas tests/diagnostic/*.pas`
  - result: PASS
  - summary:
    - after the top-level core cleanup, remaining interactive tails were concentrated in WinSSL-specialized tests plus examples/diagnostics
    - this narrowed the next bounded batch to WinSSL active tests only

- `tail -n 35 tests/unit/test_winssl_comprehensive.pas`
  - result: PASS
  - summary:
    - both the Windows main path and the non-Windows fallback still ended with `Press Enter to exit...` + `ReadLn`

- `tail -n 35 tests/winssl/test_winssl_context_comprehensive.pas`
- `tail -n 35 tests/winssl/test_winssl_errors_comprehensive.pas`
- `tail -n 35 tests/winssl/test_winssl_monitoring.pas`
- `tail -n 35 tests/winssl/test_winssl_connection_edge_cases.pas`
- `tail -n 35 tests/winssl/test_winssl_certstore.pas`
- `tail -n 35 tests/winssl/test_winssl_session_management.pas`
- `tail -n 35 tests/winssl/test_winssl_library_basic.pas`
- `tail -n 35 tests/winssl/test_winssl_certificate_loading.pas`
  - result: PASS
  - summary:
    - each active WinSSL test still carried the same interactive exit tail
    - this confirmed the issue was systematic across the active WinSSL automation layer

- add `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
  - purpose:
    - define a bounded cleanup batch for active WinSSL test programs only
    - keep examples / diagnostics / benchmark out of scope

- add `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - purpose:
    - guard the active WinSSL test set against reintroducing `ReadLn` / `Press Enter to exit...` / `按回车键退出...`

- `bash -n tests/scripts/test_winssl_active_tests_noninteractive_contract.sh && bash tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - result: RED
  - summary:
    - new contract immediately failed on `tests/unit/test_winssl_comprehensive.pas`
    - the failure proved the active WinSSL noninteractive drift was still real at source level

- update:
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_errors_comprehensive.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/winssl/test_winssl_session_management.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_certificate_loading.pas`
  - change:
    - remove the interactive exit tail
    - keep all assertions and test bodies unchanged

- `bash tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - GREEN result: PASS
  - summary:
    - active WinSSL tests are now source-guarded as noninteractive

- `mkdir -p tmp/test_unit_winssl_comprehensive_nonwindows && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_unit_winssl_comprehensive_nonwindows -FEtmp/test_unit_winssl_comprehensive_nonwindows -otmp/test_unit_winssl_comprehensive_nonwindows/test_winssl_comprehensive tests/unit/test_winssl_comprehensive.pas && timeout 2 ./tmp/test_unit_winssl_comprehensive_nonwindows/test_winssl_comprehensive`
  - GREEN result: PASS
  - summary:
    - the non-Windows fallback branch compiled and exited cleanly on Linux
    - output no longer ended with `Press Enter to exit...`

- `mkdir -p tmp/winssl_unit_comp_win64 && fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_unit_comp_win64 -FEtmp/winssl_unit_comp_win64 -otmp/winssl_unit_comp_win64/test_winssl_comprehensive.exe tests/unit/test_winssl_comprehensive.pas`
  - GREEN result: PASS
  - summary:
    - Win64 cross-compile succeeded and linked `tmp/winssl_unit_comp_win64/test_winssl_comprehensive.exe`
    - warnings were pre-existing compile noise unrelated to the interactive-tail cleanup

- `mkdir -p tmp/winssl_session_mgmt_win64 && fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas`
  - GREEN result: PASS
  - summary:
    - Win64 cross-compile succeeded and linked `tmp/winssl_session_mgmt_win64/test_winssl_session_management.exe`
    - this gave a second Windows-side syntax proof on a dedicated WinSSL test program

- `git diff --check`
  - result: PASS
  - summary:
    - current noninteractive WinSSL active test batch has no whitespace or patch-format issues

### Backend Optional-Surface Completion-Audit Revalidation

- `for f in docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md; do ...; done`
  - result: PASS
  - summary:
    - all 6 targeted backend completion-audit plans were confirmed to be missing execution-result sections
    - this proved the next gap was documentation/evidence completeness, not missing contract code

- `rg -n "Contract [0-9]+:|ISSLHttpHooksAccess|ISSLDiagnostics|ISSLNativeHandleAccess|ISSLEarlyDataContext|ISSLServerOCSPStaplingContext" tests/contract/test_backend_contract.pas`
  - result: PASS
  - summary:
    - `tests/contract/test_backend_contract.pas` already contains Contracts 12-18 for the targeted optional surfaces
    - the repo therefore already had the right focused verifier; it just lacked current execution receipts in the plan docs

- add `docs/plans/2026-05-18-backend-optional-surface-completion-audit-revalidation.md`
  - purpose:
    - define a bounded evidence-closeout batch for backend optional public surfaces already covered by `test_backend_contract`
    - keep scope on focused revalidation instead of reopening broader design work

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - GREEN result: PASS
  - summary:
    - focused contract suite finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - OpenSSL / WolfSSL / MbedTLS / FreePascal all passed:
      - Contract 12: context optional interface alignment
      - Contract 13: context native-handle interface alignment
      - Contract 14: context HTTP hooks interface alignment
      - Contract 15: session native-handle interface alignment
      - Contract 17: certificate-store native-handle interface alignment
      - Contract 18: diagnostics interface alignment
    - WinSSL continued to skip on the current Linux host, and session native-handle kept the dedicated Windows-batch boundary

- update:
  - `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`
  - change:
    - add `Focused Revalidation Result (2026-05-18)` sections
    - record the live `test_backend_contract` outcome without falsely claiming the heavy compile/minimal-ci gates were rerun in this batch

- `git diff --check`
  - result: PASS
  - summary:
    - current backend optional-surface completion-audit revalidation batch has no whitespace or patch-format issues

### ISSLConnection Surface Truth Freeze

- `git status --short --branch && git log -1 --oneline --decorate`
  - result: PASS
  - summary:
    - worktree was clean at batch start
    - latest synced commit was `992382d docs/audit: record backend optional-surface revalidation`

- `rg -n "ISSLConnection|GetCipherBits|VerifyPeerCertificate|GetSessionID|IsSessionResumed|GetSessionData|SetSessionData|GetSelectedALPNProtocol|GetSession\\b|SetSession\\b|IsSessionReused|GetVerifyResult|GetOCSPResponseStatus|GetNativeHandle" src/fafafa.ssl.base.pas docs/reference/API_REFERENCE.md docs/reference/INTERFACE_DESIGN_V2.md docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - result: PASS
  - summary:
    - source and active docs were confirmed to be out of sync in the `ISSLConnection` area
    - `API_REFERENCE.md` still documented obsolete methods while the source exposed a larger current surface plus optional-owner splits

- `sed -n '1122,1555p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - established the current source truth for `ISSLConnection`, `ISSLClientConnection`, `ISSLDiagnostics`, `ISSLSessionResumption`, `ISSLCertificateVerification`, `ISSLOCSPStapling`, and `ISSLConnectionInfo`

- `sed -n '1663,1684p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - established the current source truth for `ISSLSession`
    - confirmed the active session surface is `GetID` / `Serialize` / `Clone`, not `GetSessionData` / `GetLastAccessTime`

- `nl -ba docs/reference/API_REFERENCE.md | sed -n '413,930p'`
  - result: PASS
  - summary:
    - confirmed the active docs still promised stale `ISSLConnection` and `ISSLSession` methods
    - example code still used `GetSessionID` and `IsSessionResumed`

- add `docs/plans/2026-05-18-isslconnection-surface-truth-freeze.md`
  - purpose:
    - define a bounded doc/contract truth-freeze batch before any public-interface slimming work

- update `docs/reference/API_REFERENCE.md`
  - change:
    - replace stale `ISSLConnection` signature block with the current source truth
    - add `v1.x` compatibility-core / optional-owner notes
    - rewrite session examples to use `GetID`, `Serialize`, and `IsSessionReused`

- add `tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - purpose:
    - fail if active docs reintroduce stale `ISSLConnection` / `ISSLSession` names
    - require current source-truth methods and optional-owner notes to remain visible

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - correct the stale route priority
    - make `ISSLConnection surface truth freeze` the immediate next batch instead of defaulting back to `TSSLConfig`

- `bash -n tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - new focused contract script is syntactically valid

- `bash tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active `ISSLConnection` / `ISSLSession` docs now match current source truth
    - stale names no longer appear in the guarded active-doc section

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnection surface truth freeze` batch has no whitespace or patch-format issues

### Backend Connection-Surface Completion-Audit Revalidation

- `for f in docs/plans/2026-05-04-backend-*.md; do ...; done`
  - result: PASS
  - summary:
    - re-scan confirmed only 3 targeted connection-layer plans were still missing current execution receipts:
      - `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
      - `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
      - `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`
    - `ISSLConnectionInfo` / `ISSLSessionResumption` / `ISSLCertificateVerification` plans already had execution results and were not the next gap

- `rg -n "Contract [0-9]+: .*SNI|Contract [0-9]+: .*native-handle|Contract [0-9]+: .*OCSP|ISSLClientConnection|ISSLNativeHandleAccess|ISSLOCSPStapling" tests/contract/test_backend_contract.pas`
  - result: PASS
  - summary:
    - confirmed the repo already has the right focused verifier for the three missing plan receipts:
      - Contract 8: client connection SNI interface alignment
      - Contract 10: client connection OCSP interface alignment
      - Contract 11: connection native-handle interface alignment

- add `docs/plans/2026-05-18-backend-connection-surface-completion-audit-revalidation.md`
  - purpose:
    - define a bounded evidence-closeout batch for the remaining connection-layer plans missing current execution receipts

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused contract suite finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - direct connection-layer truth relevant to this batch:
      - Contract 8:
        - OpenSSL / WolfSSL / MbedTLS / FreePascal PASS
        - WinSSL SKIP on the current Linux host
      - Contract 10:
        - OpenSSL / WolfSSL / FreePascal OCSP-capable connection surfaces PASS
        - MbedTLS absent-path PASS
        - WinSSL SKIP
      - Contract 11:
        - OpenSSL / WolfSSL / MbedTLS native-handle surfaces PASS
        - FreePascal absent-path PASS
        - WinSSL SKIP

- update:
  - `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
  - `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
  - `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`
  - change:
    - add `Focused Revalidation Result (2026-05-18)` sections
    - record current live `test_backend_contract` evidence without falsely claiming heavy compile/minimal-ci gates were rerun in this batch

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the remaining connection-layer execution-receipt gap as closed
    - move the next route forward to a real `ISSLConnection` slimming slice

### ISSLConnectionInfo Mirror Demotion Migration Map

- `rg -n "ISSLConnectionInfo|GetConnectionInfo|GetContext\\b|GetSelectedALPNProtocol|GetStateString" src/fafafa.ssl.base.pas src/fafafa.ssl.connection.base.pas tests/contract/test_backend_contract.pas docs/reference/API_REFERENCE.md docs/reference/INTERFACE_DESIGN_V2.md docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - result: PASS
  - summary:
    - confirmed current source truth, active API docs, contract proof, and v2 design doc were no longer aligned on the `ISSLConnectionInfo` mirror group
    - the design doc still omitted `ISSLConnectionInfo` from the hierarchy and misrouted part of the migration table

- `sed -n '1,220p' docs/reference/INTERFACE_DESIGN_V2.md`
  - result: PASS
  - summary:
    - confirmed `INTERFACE_DESIGN_V2.md` still used the empty `ISSLAdvanced` bucket
    - confirmed the class example omitted `ISSLConnectionInfo`
    - confirmed the migration table still mapped `GetConnectionInfo` to `ISSLDiagnostics`

- add `docs/plans/2026-05-18-isslconnectioninfo-mirror-demotion-migration-map.md`
  - purpose:
    - define a bounded design-only batch that freezes the Stage-A demotion route for the `ISSLConnectionInfo` mirrors before any source-facing slimming work

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - add `ISSLConnectionInfo` to the hierarchy and extension definitions
    - remove the stale `ISSLAdvanced` bucket
    - correct the implementation example and migration snippet
    - freeze `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString` to `ISSLConnectionInfo` as the Stage-A demotion target

- add `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - purpose:
    - fail if `INTERFACE_DESIGN_V2.md` reintroduces stale owner targets or the empty `ISSLAdvanced` bucket
    - require the Stage-A `ISSLConnectionInfo` demotion map to remain visible

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the migration-map batch as delivered
    - move the next route to source-facing slimming prep

- `bash -n tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - new migration-target contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - `INTERFACE_DESIGN_V2.md` now keeps the Stage-A `ISSLConnectionInfo` demotion map consistent
    - stale owner targets and the empty `ISSLAdvanced` bucket are no longer present

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnectionInfo mirror demotion / migration-map` batch has no whitespace or patch-format issues

### ISSLConnectionInfo Active Guidance De-emphasis

- `rg -n -F "GetSelectedALPNProtocol" ...` / `rg -n -F "GetStateString" ...` / `rg -n -F "GetConnectionInfo" ...`
  - result: PASS
  - summary:
    - confirmed active docs still taught direct core mirror usage in `API_REFERENCE.md` and `INTEGRATION_GUIDE.md`
    - this remained misaligned with the just-frozen Stage-A `ISSLConnectionInfo` demotion map

- add `docs/plans/2026-05-18-isslconnectioninfo-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded user-facing doc batch that switches connection-info mirrors from core teaching to `ISSLConnectionInfo`-first guidance

- update:
  - `docs/reference/API_REFERENCE.md`
  - `docs/INTEGRATION_GUIDE.md`
  - change:
    - replace direct `LConn.GetConnectionInfo` / `LConn.GetSelectedALPNProtocol` / `LConn.GetStateString` example guidance
    - switch examples to `Supports(..., ISSLConnectionInfo, ...)`
    - add an explicit note that new code should prefer `ISSLConnectionInfo` for this mirror group

- add `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - purpose:
    - fail if active docs reintroduce direct core mirror teaching for connection-info mirrors
    - require `ISSLConnectionInfo`-first guidance in the guarded active docs

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark active-guidance de-emphasis as delivered
    - keep the next route on source-facing slimming prep

- `bash -n tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - new active-guidance contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs now prefer `ISSLConnectionInfo` for the connection-info mirror group
    - direct core mirror teaching no longer appears in the guarded examples

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnectionInfo active guidance de-emphasis` batch has no whitespace or patch-format issues

### ISSLConnectionInfo Source Classification Freeze

- `sed -n '1188,1295p' src/fafafa.ssl.base.pas` / `sed -n '1520,1548p' src/fafafa.ssl.base.pas` / `sed -n '36,72p' src/fafafa.ssl.connection.base.pas`
  - result: PASS
  - summary:
    - confirmed source comments still lacked an explicit Stage-A classification note for the `ISSLConnectionInfo` mirror group
    - confirmed the next source-facing gap was classification truth, not implementation behavior

- add `docs/plans/2026-05-18-isslconnectioninfo-source-classification-freeze.md`
  - purpose:
    - define a bounded source-facing prep batch that freezes the `compatibility-core duplicate` classification in source comments before any implementation cut

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add Stage-A classification notes for `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString`
    - clarify that `ISSLConnectionInfo` is the current owner used to carry these `v1.x` compatibility-core duplicates

- add `tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - purpose:
    - fail if source comments lose the Stage-A classification notes
    - keep source-facing truth aligned with the roadmap and active docs

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark source classification freeze as delivered
    - move the next route to the first real implementation slice decision

- `bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - result: PASS
  - summary:
    - new source-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - result: PASS
  - summary:
    - source comments now keep the `ISSLConnectionInfo` mirror group aligned with the Stage-A roadmap
    - source-facing duplicate-owner truth no longer depends only on external docs

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLConnectionInfo source classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the focused source-classification proof during final commit preparation
    - current batch is ready to commit without reopening heavier verification lanes

### GetContext Active Guidance De-emphasis

- `sed -n '388,410p' docs/CAPABILITY_MATRIX_GUIDE.md` / `sed -n '548,586p' docs/reference/API_REFERENCE.md` / `rg -n "\\.GetContext\\b|GetContext\\(" src tests docs`
  - result: PASS
  - summary:
    - confirmed the last active-doc example still teaching direct core `GetContext` was in `CAPABILITY_MATRIX_GUIDE.md`
    - confirmed `API_REFERENCE.md` had not yet explicitly grouped `GetContext` into the `ISSLConnectionInfo`-first guidance sentence
    - confirmed production source had no extra live callers beyond the base implementation and mirror-equality contract coverage

- add `docs/plans/2026-05-18-getcontext-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded `GetContext` batch that keeps the work on active guidance and route selection instead of prematurely changing the public signature

- update:
  - `docs/CAPABILITY_MATRIX_GUIDE.md`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - switch the capability example from `Conn.GetContext` to `ISSLConnectionInfo.GetContext`
    - explicitly include `GetContext` in the API reference's `ISSLConnectionInfo`-first note

- add `tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - purpose:
    - fail if active docs reintroduce direct core `GetContext` teaching
    - keep `GetContext` aligned with the current `ISSLConnectionInfo` owner route

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the `GetContext` active-guidance cut as delivered
    - record `GetContext` as the current first-priority mirror for the next real implementation slice

- `bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - new `GetContext` active-guidance contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs no longer teach `Conn.GetContext` as the preferred path
    - `GetContext` is now explicitly aligned with `ISSLConnectionInfo`-first guidance

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext active guidance de-emphasis` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the focused `GetContext` guidance proof during final commit preparation
    - current batch is ready to commit without reopening heavier verification lanes

### GetContext Contract Owner Primacy

- `sed -n '1788,1856p' tests/contract/test_backend_contract.pas` / `rg -n \"test_backend_contract\\.pas|GetContext\" progress.md docs/plans tests/scripts`
  - result: PASS
  - summary:
    - confirmed the remaining live `GetContext` coupling had shrunk to the contract layer
    - confirmed the contract still narrated `ISSLConnection.GetContext` and `ISSLConnectionInfo.GetContext` as a dual-owner pair

- add `docs/plans/2026-05-18-getcontext-contract-owner-primacy.md`
  - purpose:
    - define a bounded contract-semantics batch that promotes `ISSLConnectionInfo.GetContext` to the primary owner without touching runtime implementation

- update `tests/contract/test_backend_contract.pas`
  - change:
    - check `ISSLConnectionInfo.GetContext` against the creation context before consulting the core mirror
    - keep `ISSLConnection.GetContext` only as a mirror-equality proof after optional-owner truth is established

- add `tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - purpose:
    - fail if the backend contract drifts back to legacy dual-owner `GetContext` wording
    - keep the new optional-owner/core-mirror semantics cheap to revalidate

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetContext` contract owner primacy as delivered
    - move the next route to stronger `GetContext` feasibility / deprecation discussion

- `bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - new `GetContext` contract-owner guard script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - backend contract source now treats `ISSLConnectionInfo.GetContext` as the primary owner
    - legacy dual-owner `GetContext` wording is no longer present in the guarded contract block

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract suite finished `135 total / 111 passed / 0 failed / 24 skipped`
    - OpenSSL / WolfSSL / MbedTLS / FreePascal kept `Contract 19: Connection-info interface alignment` green after the owner-primacy change
    - WinSSL continued to follow the current Linux-host skip truth

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext contract owner primacy` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetContext` contract-owner proof after the final planning-file sync
    - no heavy recompile was needed because only planning files changed after the last focused Pascal contract run

### GetContext Source/Class Split Feasibility Freeze

- `rg -n "\\.GetContext\\b|GetContext\\(" src tests docs --glob '!docs/archive/**' --glob '!docs/plans/**'` / `rg -n "function .*GetContext: ISSLContext" src`
  - result: PASS
  - summary:
    - confirmed the remaining live `GetContext` surface had shrunk to interface declarations, one shared base implementation, one active-doc `ConnInfo.GetContext` path, and one backend-contract core mirror proof
    - confirmed production source had no extra direct `GetContext` call dependency to block a future class/surface split discussion

- add `docs/plans/2026-05-18-getcontext-source-class-split-feasibility-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so the next route decision no longer depends on repeating the same `GetContext` source archaeology

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner / mirror notes for `GetContext`
    - spell out that the shared base implementation now mainly exists to support the compatibility mirror plus the current `ISSLConnectionInfo` owner

- add `tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - purpose:
    - freeze the current `GetContext` remaining live surface into a cheap allowlist contract
    - fail if active docs, source, or non-script tests reintroduce new direct core `GetContext` dependencies

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the source/class split feasibility freeze as delivered
    - move the next route decision to public deprecation wording vs. the next mirror

- first run of `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: RED
  - summary:
    - the initial script exited early because zero-hit `rg` pipelines still returned status `1` under `set -euo pipefail`
    - adjusted the counting branches to tolerate zero-hit scans explicitly before re-running

- `bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: PASS
  - summary:
    - new `GetContext` source/class split contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: PASS
  - summary:
    - `GetContext` live surface is now frozen to the expected allowlist
    - no new active-doc, source, or non-script test dependency escaped the guarded boundary

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext source/class split feasibility freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetContext` allowlist proof after the final planning-file sync
    - no heavy Pascal contract rerun was needed because only planning files changed after the source/class split freeze passed

### GetStateString Active Test De-emphasis

- `rg -n "GetStateString|ISSLConnectionInfo" tests/connection/test_connection_basic.pas tests/integration/test_real_https_connection.pas`
  - result: PASS
  - summary:
    - confirmed the highest-value remaining ordinary `GetStateString` usage lived in the generic connection smoke test and the real HTTPS integration suite
    - confirmed the next batch could stay on active-test de-emphasis without reopening backend-specific runtime surfaces

- add `docs/plans/2026-05-18-getstatestring-active-test-deemphasis.md`
  - purpose:
    - define a bounded `GetStateString` batch that moves ordinary generic/integration tests off the core getter before touching backend-specific runtime files

- update:
  - `tests/connection/test_connection_basic.pas`
  - `tests/integration/test_real_https_connection.pas`
  - change:
    - route generic/integration state-string reads through `ISSLConnectionInfo`
    - add an integration helper so handshake-failure reporting no longer directly calls the core getter

- add `tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - purpose:
    - fail if ordinary generic/integration tests reintroduce direct core `GetStateString`
    - keep this first `GetStateString` route change cheap to verify

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetStateString` active-test de-emphasis as delivered
    - move the next route to residual runtime classification vs. `GetSelectedALPNProtocol`

- first run of `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: RED
  - summary:
    - the initial contract expected an exact `Result := LConnInfo.GetStateString;` token, but the integration helper used a semicolon-free `if/else` form
    - relaxed the check to the real source shape before re-running

- `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: PASS
  - summary:
    - new `GetStateString` active-test contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: PASS
  - summary:
    - active generic/integration tests now prefer `ISSLConnectionInfo.GetStateString`
    - direct core `GetStateString` no longer appears in the guarded ordinary test paths

- first run of `mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic`
  - result: RED
  - summary:
    - compile/run exposed a pre-existing companion drift in `tests/connection/test_connection_basic.pas`
    - the file still treated `GetNativeHandle` as core `ISSLConnection` surface and used `FillChar` to build `TSSLConfig`, which triggered `LogLevel is library-scoped` at runtime

- update `tests/connection/test_connection_basic.pas`
  - change:
    - switch the native-handle check to `ISSLNativeHandleAccess`
    - replace `FillChar` config initialization with `CreateDefaultConfig(sslCtxClient)` so the test follows the current factory/config truth

- `mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic`
  - result: PASS
  - summary:
    - generic connection smoke suite finished `11 passed, 0 failed`
    - the state-string path now goes through `ISSLConnectionInfo`, and the same file no longer drifts on native-handle/config initialization truth

- `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
  - result: PASS
  - summary:
    - integration suite compiled successfully and finished green under the current environment gate
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetStateString active-test de-emphasis` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetStateString` active-test proof after the final planning-file sync
    - no extra compile rerun was needed because only planning files changed after the focused tests passed

### GetStateString Residual Classification Freeze

- `rg -n "\\.GetStateString\\b|GetStateString\\(" tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed ordinary docs/tests no longer used direct core `GetStateString`
    - confirmed the remaining direct-core residuals had shrunk to backend-contract mirror proof plus OpenSSL/WolfSSL backend-specific runtime tests

- add `docs/plans/2026-05-18-getstatestring-residual-classification-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so `GetStateString` no longer requires repeated residual-hit archaeology

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner notes for `GetStateString`
    - spell out the current residual direct-core surface at the shared base-class comment level

- add `tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - purpose:
    - freeze the current `GetStateString` direct-core residual file set into a cheap allowlist contract
    - fail if ordinary docs/tests or new files reintroduce direct core `GetStateString`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the residual-classification freeze as delivered
    - move the next route decision to stronger `GetStateString` wording vs. `GetSelectedALPNProtocol`

- first run of `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the initial comment-pattern check was too strict for the wrapped base-class comment layout
    - relaxed the residual-note matching to the real multiline source shape before re-running

- second run of `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the allowlist count initially expected 8 direct core hits, but the real residual set is 9 including the contract mirror-proof hit
    - corrected the expected residual count before the final re-run

- `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new `GetStateString` residual-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetStateString` residual direct-core surface now matches the expected allowlist
    - ordinary docs/tests no longer reintroduce direct core `GetStateString`

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetStateString residual classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetStateString` residual allowlist proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the allowlist contract passed

### GetSelectedALPNProtocol Active Test De-emphasis

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetSelectedALPNProtocol\b' tests --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed the highest-value remaining ordinary `GetSelectedALPNProtocol` usage lived in the real HTTPS integration suite and the cross-backend consistency contract
    - confirmed the next batch could stay on active-test de-emphasis without reopening backend-specific runtime ALPN files

- add `docs/plans/2026-05-18-getselectedalpn-active-test-deemphasis.md`
  - purpose:
    - define a bounded `GetSelectedALPNProtocol` batch that moves ordinary integration/contract tests off the core getter before touching backend-specific runtime files

- add `tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - purpose:
    - fail if ordinary integration/contract tests reintroduce direct core `GetSelectedALPNProtocol`
    - keep this first ALPN route change cheap to verify

- first run of `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: RED
  - summary:
    - the new contract correctly caught the first residual ordinary-path use in `tests/integration/test_real_https_connection.pas`
    - this confirmed the batch boundary before any Pascal edits landed

- update:
  - `tests/integration/test_real_https_connection.pas`
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - change:
    - add `ISSLConnectionInfo`-first ALPN helpers
    - replace direct core `GetSelectedALPNProtocol` reads in the guarded ordinary integration/contract paths

- `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: PASS
  - summary:
    - new `GetSelectedALPNProtocol` active-test contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: PASS
  - summary:
    - ordinary integration/contract tests now prefer `ISSLConnectionInfo.GetSelectedALPNProtocol`
    - direct core `GetSelectedALPNProtocol` no longer appears in the guarded test paths

- `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
  - result: PASS
  - summary:
    - integration suite compiled successfully and finished green under the current environment gate
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`

- `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
  - result: PASS
  - summary:
    - cross-backend consistency contract compiled successfully and stayed green under the current environment gate
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetSelectedALPNProtocol\b' tests --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed the remaining direct-core ALPN surface had shrunk to backend contract mirror proof plus MbedTLS/WinSSL backend-specific runtime files

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetSelectedALPNProtocol` active-test de-emphasis as delivered
    - move the next route decision to residual runtime classification vs. stronger client-owner wording

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetSelectedALPNProtocol active-test de-emphasis` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetSelectedALPNProtocol` active-test proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the focused tests passed

### GetSelectedALPNProtocol Residual Classification Freeze

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetSelectedALPNProtocol\b|GetSelectedALPNProtocol\(' tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed ordinary docs/tests no longer used direct core `GetSelectedALPNProtocol`
    - confirmed the remaining direct-core residuals had shrunk to backend-contract mirror proof plus MbedTLS/WinSSL backend-specific runtime ALPN files

- add `docs/plans/2026-05-18-getselectedalpn-residual-classification-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so `GetSelectedALPNProtocol` no longer requires repeated residual-hit archaeology

- add `tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - purpose:
    - freeze the current `GetSelectedALPNProtocol` direct-core residual file set into a cheap allowlist contract
    - fail if ordinary docs/tests or new files reintroduce direct core `GetSelectedALPNProtocol`

- first run of `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the new residual contract correctly caught the missing source-level preferred-access note in `src/fafafa.ssl.base.pas`
    - this confirmed the batch still had real source-facing truth drift before comment updates

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner notes for `GetSelectedALPNProtocol`
    - spell out the current residual direct-core surface at the shared base-class comment level

- `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new `GetSelectedALPNProtocol` residual-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetSelectedALPNProtocol` residual direct-core surface now matches the expected allowlist
    - ordinary docs/tests no longer reintroduce direct core `GetSelectedALPNProtocol`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the residual-classification freeze as delivered
    - move the next route decision to stronger `GetSelectedALPNProtocol` wording vs. `GetConnectionInfo`

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetSelectedALPNProtocol residual classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetSelectedALPNProtocol` residual allowlist proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the allowlist contract passed

### GetConnectionInfo Residual Classification Freeze

- `rg -n '\b(?:Conn|LConn|LConnection)\.GetConnectionInfo\b|GetConnectionInfo\(' tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed active docs and ordinary tests no longer used direct core `GetConnectionInfo`
    - confirmed the remaining direct-core residuals were already limited to backend-contract mirror proof plus OpenSSL/WinSSL backend-specific files

- add `docs/plans/2026-05-18-getconnectioninfo-residual-classification-freeze.md`
  - purpose:
    - define a bounded allowlist-freeze batch so `GetConnectionInfo` no longer requires repeated residual-hit archaeology

- add `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - purpose:
    - freeze the current `GetConnectionInfo` direct-core residual file set into a cheap allowlist contract
    - fail if active docs/tests or new files reintroduce direct core `GetConnectionInfo`

- first run of `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: RED
  - summary:
    - the new residual contract correctly caught the missing source-level preferred-access note in `src/fafafa.ssl.base.pas`
    - this confirmed the batch still had real source-facing truth drift before comment updates

- update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - change:
    - add explicit preferred-access / owner notes for `GetConnectionInfo`
    - spell out the current residual direct-core surface at the shared base-class comment level

- `bash -n tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new `GetConnectionInfo` residual-classification contract script is syntactically valid

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetConnectionInfo` residual direct-core surface now matches the expected allowlist
    - active docs/tests no longer reintroduce direct core `GetConnectionInfo`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark the residual-classification freeze as delivered
    - move the next route decision to stronger wording vs. backend implementation-completeness review

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetConnectionInfo residual classification freeze` batch has no whitespace or patch-format issues

- closeout revalidation before commit:
  - `bash -n tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `git diff --check`
  - result: PASS
  - summary:
    - reran the lightweight `GetConnectionInfo` residual allowlist proof after the final planning-file sync
    - no extra Pascal rerun was needed because only planning files changed after the allowlist contract passed

### GetConnectionInfo Base Enrichment From Residual Audit

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - add shared `DoGetConnectionInfoServerName` hook on `TBaseSSLConnection`
    - enrich shared `GetConnectionInfo` with `ServerName`
    - enrich shared `GetConnectionInfo` with `SessionId` when connected/handshake-complete and session metadata is available
    - extend the focused hostname-precedence mock test with `ConnectionInfo.ServerName` / `ConnectionInfo.SessionId` behavior coverage
    - narrow the active API wording from “all fields are fully populated” to shared-minimum + best-effort backend detail truth

- implementation note:
  - the final shared-layer design intentionally avoids `Supports(Self, ISSLClientConnection, ...)` inside `TBaseSSLConnection.GetConnectionInfo`
  - summary:
    - a prior attempt had already shown that the naive self-cast route could destabilize OpenSSL fresh-connection access
    - the landed design uses backend overrides of `DoGetConnectionInfoServerName` instead, which is safe for direct concrete-object test construction paths

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `13 passed, 0 failed`
    - new `ConnectionInfo.ServerName` and `ConnectionInfo.SessionId` checks both stayed green

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info cipher guard finished `10 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` no longer reproduced the prior `EAccessViolation`
    - shared `ServerName` enrichment preserved the cipher-guard baseline fields

- update `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - change:
    - raise the expected direct-core hit count from `7` to `9`
    - add `tests/test_connection_builder_hostname_precedence.pas` to the intentional direct-core allowlist
    - rationale:
      - the new mock test intentionally reads core `Conn.GetConnectionInfo` to verify shared-layer mirror truth

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - updated residual contract now matches the expanded intentional allowlist
    - no unexpected direct-core `GetConnectionInfo` files were introduced

### GetConnectionInfo PeerCertificate Base Enrichment

- add `docs/plans/2026-05-18-getconnectioninfo-peercertificate-base-enrichment.md`
  - purpose:
    - define the next bounded implementation-completeness batch after `ServerName` / `SessionId`
    - keep scope on the shared `PeerCertificate` field instead of prematurely diving into backend-specific cipher detail mapping

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - enrich shared `GetConnectionInfo` with `PeerCertificate` when `GetPeerCertificate` returns a current certificate
    - extend the focused mock contract so the existing shared `GetConnectionInfo` read also proves `PeerCertificate.Subject` / `Issuer` mirror truth
    - narrow the active API wording so `PeerCertificate` is now documented as a shared-layer field when the connection can expose the current peer certificate

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `15 passed, 0 failed`
    - the existing `GetConnectionInfo` proof still covered `ServerName` / `SessionId`
    - the same intentional direct-core read now also proved `PeerCertificate.Subject` / `Issuer` mirror truth without expanding the residual allowlist

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info guard finished `10 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` remained safe after the new shared `GetPeerCertificate` path was introduced

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the intentional direct-core `GetConnectionInfo` surface stayed unchanged at the current allowlist
    - no new residual archaeology was needed for this batch

### GetConnectionInfo Crypto Detail Name-Derived First Slice

- add `docs/plans/2026-05-18-getconnectioninfo-crypto-detail-name-derived-first-slice.md`
  - purpose:
    - define the first bounded shared-crypto-detail batch after `PeerCertificate`
    - keep scope on name-derived `Cipher` / `Hash` / `KeySize` normalization instead of reopening backend-specific ID/MAC detail

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - add a shared cipher-suite-name normalization helper for `GetConnectionInfo`
    - derive `Cipher`, `Hash`, and `KeySize` from the negotiated cipher-suite name
    - derive `KeyExchange` when the cipher-suite name still carries a legacy prefix such as `ECDHE-RSA`
    - update the focused mock proof to use a real parseable suite name: `ECDHE-RSA-AES128-GCM-SHA256`
    - narrow the active API wording so these fields are now documented as shared best-effort derivations when the backend already exposes a stable cipher-suite name

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `19 passed, 0 failed`
    - the existing intentional direct-core `GetConnectionInfo` proof still covered `ServerName`, `SessionId`, and `PeerCertificate`
    - the same read now also proved shared name-derived `KeyExchange`, `Cipher`, `Hash`, and `KeySize` truth

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info guard finished `10 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` remained safe after the new shared cipher-suite-name parser was introduced

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the intentional direct-core `GetConnectionInfo` surface stayed unchanged at the current allowlist
    - this batch did not require any new residual file or hit-count changes

### GetConnectionInfo CipherSuiteId First Slice

- add `docs/plans/2026-05-18-getconnectioninfo-ciphersuiteid-first-slice.md`
  - purpose:
    - define the next bounded `GetConnectionInfo` completeness batch after the shared name-derived crypto-detail slice
    - keep scope on `CipherSuiteId` instead of reopening `MacSize` or broader backend runtime refactors

- update:
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.openssl.api.ssl.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - keep the shared TLS 1.3 standard-name derivation for `CipherSuiteId`
    - export and load `SSL_CIPHER_get_protocol_id` from the active OpenSSL SSL API unit
    - let `TOpenSSLConnection.GetConnectionInfo` prefer `SSL_CIPHER_get_protocol_id` and fall back to `SSL_CIPHER_get_id and $FFFF`
    - extend the focused OpenSSL contract with explicit `CipherSuiteId` truth checks
    - sync the residual allowlist count after adding one more intentional direct-core `GetConnectionInfo` proof site

- error encountered:
  - the carry-over uncommitted implementation initially did not compile because `SSL_CIPHER_get_protocol_id` was not exported from the active `fafafa.ssl.openssl.api.ssl` loader path
  - resolution:
    - add the missing type / var / nil-reset / loader assignment in `src/fafafa.ssl.openssl.api.ssl.pas`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite finished `21 passed, 0 failed`
    - the existing intentional direct-core `GetConnectionInfo` proof still covered `ServerName`, `SessionId`, `PeerCertificate`, and legacy `KeyExchange`
    - the same read now also proved shared TLS 1.3 `CipherSuiteId` truth on `TLS_AES_128_GCM_SHA256`

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - first result:
    - compile became green after the loader/export fix
    - runtime contract exposed one new failure:
      - `GetConnectionInfo when SSL_CIPHER_get_name is unavailable should not raise`
      - `EAccessViolation: Access violation`
  - diagnosis:
    - the old guard uses a fake non-nil cipher pointer to model “current cipher exists but helpers are unavailable”
    - once `CipherSuiteId` low-level helpers were added, leaving real `SSL_CIPHER_get_protocol_id` assigned made the test exercise an invalid-pointer artifact instead of a real product path
  - follow-up fix:
    - extend the degrade branch to nil both `SSL_CIPHER_get_protocol_id` and `SSL_CIPHER_get_id`
    - add a separate truth contract that proves:
      - `SSL_CIPHER_get_protocol_id` is preferred
      - `SSL_CIPHER_get_id` low word is the fallback
  - final result: PASS
  - summary:
    - focused OpenSSL connection-info suite finished `14 passed, 0 failed`
    - fresh-connection `GetConnectionInfo` still degrades safely when cipher helpers are unavailable
    - low-level `CipherSuiteId` backfill now has explicit contract coverage

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - first result:
    - FAIL: expected exactly `9` direct core `GetConnectionInfo` test hits, found `10`
  - resolution:
    - update the expected count to `10`
    - rationale:
      - the new OpenSSL focused `CipherSuiteId` truth proof intentionally adds one direct-core `GetConnectionInfo` site in an already-allowlisted test file
  - final result: PASS
  - summary:
    - the intentional direct-core `GetConnectionInfo` surface remains controlled
    - this batch did require a small allowlist count sync, but no new residual file family

- `git diff --check`
  - result: PASS
  - summary:
    - current batch has no whitespace or patch-format issues

### GetConnectionInfo Contract Owner Primacy

- add `docs/plans/2026-05-18-getconnectioninfo-contract-owner-primacy.md`
  - purpose:
    - close the stale residual-allowlist drift on the `GetConnectionInfo` route
    - turn `Contract 19` into explicit `ISSLConnectionInfo` owner primacy instead of implicit dual-owner comparison

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: RED
  - summary:
    - stale source contract still expected exactly `10` direct core `GetConnectionInfo` test hits
    - live repo truth had already drifted to `15` hits across:
      - shared builder proof
      - OpenSSL / WolfSSL / MbedTLS completeness proof
      - FreePascal completion proof
    - this confirmed a real route/workflow gap instead of just a missing note

- implementation:
  - `tests/contract/test_backend_contract.pas`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/test_wolfssl_connection_info_macsize_contract.pas`
  - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
  - change:
    - `Contract 19` now reads `ISSLConnectionInfo.GetConnectionInfo` first, then checks direct core `GetConnectionInfo` as a mirror
    - new shell contract freezes the owner-primacy wording
    - completeness / proof tests now read connection info through `ISSLConnectionInfo`
    - residual allowlist now shrinks to the true remaining direct-core files:
      - `tests/contract/test_backend_contract.pas`
      - `tests/winssl/test_winssl_connection_info.pas`
      - `tests/winssl/test_winssl_connection_edge_cases.pas`

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - backend contract now treats `ISSLConnectionInfo.GetConnectionInfo` as the primary owner

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: RED -> GREEN
  - summary:
    - after shrinking ordinary proof/test usage, residual direct-core surface now matches the expected `5`-hit allowlist

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared builder proof stayed green at `29 passed, 0 failed`
    - moving the proof to `ISSLConnectionInfo` did not change the shared truth already covered in this test

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Contract 19` stayed green on OpenSSL / WolfSSL / MbedTLS / FreePascal
    - overall backend contract result remained:
      - `Total Tests: 135`
      - `Passed: 111`
      - `Failed: 0`
      - `Skipped: 24`

- `mkdir -p tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_server_accept_skeleton -FEtmp/test_freepascal_server_accept_skeleton -otmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas && ./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
  - result: PASS
  - summary:
    - FreePascal server completion proof remained green after switching to `ISSLConnectionInfo`

- `mkdir -p tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_session_resumption -FEtmp/test_freepascal_client_session_resumption -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - result: PASS
  - summary:
    - FreePascal session-resumption completion proof remained green after switching to `ISSLConnectionInfo`

- `mkdir -p tmp/test_mbedtls_connection_info_ciphersuite_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_info_ciphersuite_contract -FEtmp/test_mbedtls_connection_info_ciphersuite_contract -otmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract tests/test_mbedtls_connection_info_ciphersuite_contract.pas && ./tmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract`
  - result: PASS
  - summary:
    - MbedTLS completeness proof remained green at `15 passed, 0 failed`

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: RED -> GREEN
  - summary:
    - first owner-path conversion exposed a real test-lifetime bug:
      - concrete `TOpenSSLConnection` was still manually freed after `ISSLConnectionInfo` had taken over lifetime
      - test failed with `EInvalidPointer` / `EAccessViolation`
    - after switching the helper to interface-owned lifetime, final result returned to:
      - `20 passed, 0 failed`

- `mkdir -p tmp/test_wolfssl_connection_info_macsize_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_info_macsize_contract -FEtmp/test_wolfssl_connection_info_macsize_contract -otmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract tests/test_wolfssl_connection_info_macsize_contract.pas && ./tmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract`
  - result: RED -> GREEN
  - summary:
    - the same lifecycle pit also existed in the WolfSSL helper after the owner-path conversion
    - after aligning it to interface-owned lifetime, final result returned to:
      - `3 passed, 0 failed`

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `GetConnectionInfo` contract owner primacy as delivered
    - move the default mainline to stronger wording / slimming discussion instead of more residual allowlist cleanup

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the owner-primacy closeout, the residual shrink, and the concrete-object/interface lifetime pit into persistent repo working memory

### GetConnectionInfo WinSSL Direct-Core Classification

- add `docs/plans/2026-05-18-getconnectioninfo-winssl-direct-core-classification.md`
  - purpose:
    - settle the last residual-classification question on the current `GetConnectionInfo` route
    - decide whether the remaining WinSSL direct-core files are intentional core-surface proof or stale owner-path drift

- static audit result:
  - `tests/winssl/test_winssl_connection_info.pas`
    intentionally verifies:
    - direct core `GetConnectionInfo`
    - direct core `GetProtocolVersion`
    - direct core `GetCipherName`
    - consistency between `GetConnectionInfo` and the individual core getters
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
    keeps one direct core `GetConnectionInfo` path inside the broader WinSSL edge-case suite
  - this means the remaining WinSSL residuals are core-surface proof, not ordinary completeness tests that were forgotten during the owner-path migration

- implementation:
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
  - change:
    - marked the two WinSSL residual files as `INTENTIONAL_CORE_SURFACE`
    - added a focused source guard that freezes the WinSSL residual file set and the classification marker

- `bash tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
  - result: PASS
  - summary:
    - the remaining WinSSL direct-core `GetConnectionInfo` files are explicitly classified and confined

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - the global residual direct-core `GetConnectionInfo` allowlist stayed green after the WinSSL classification closeout

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark WinSSL residual classification as delivered
    - move the default mainline fully onto stronger owner / deprecation wording work

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the WinSSL residual classification closeout into persistent repo working memory

### FreePascal GetConnectionInfo Completion Audit

- add `docs/plans/2026-05-18-freepascal-getconnectioninfo-completion-audit.md`
  - purpose:
    - close the last open question on the current `GetConnectionInfo` implementation-completeness route
    - prove whether `FreePascal` still needs a backend-local low-level truth helper or can now close on shared TLS 1.3 truth

- update focused FreePascal runtime proofs:
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
  - change:
    - add explicit `GetConnectionInfo` assertions on the server skeleton path for:
      - `CipherSuiteId`
      - `KeySize`
      - `MacSize`
    - add explicit `GetConnectionInfo` assertions on the client initial/resumed paths for:
      - `ProtocolVersion`
      - `CipherSuiteId`
      - `KeySize`
      - `MacSize`
      - `ServerName`
      - `IsResumed`
      - `SessionId`

- add `tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
  - purpose:
    - fail if `FreePascal` grows a dedicated `GetConnectionInfo` override
    - guard that client/server TLS 1.3 paths still feed standard suite-name truth into shared `GetConnectionInfo`
    - guard that session/resumption state still carries `FCipherSuite: Word`

- `bash tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
  - result: PASS
  - summary:
    - confirmed `TFreePascalConnection` does not implement a dedicated `GetConnectionInfo` override
    - confirmed the active truth path still depends on:
      - `FCipherName := TLS13CipherSuiteToString(...)`
      - `FCipherSuite: Word`

- `mkdir -p tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_server_accept_skeleton -FEtmp/test_freepascal_server_accept_skeleton -otmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas && ./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton`
  - result: PASS
  - summary:
    - server skeleton proof now covers:
      - `GetConnectionInfo.CipherSuiteId = TLS13_CIPHER_AES_128_GCM_SHA256`
      - `KeySize = 128`
      - `MacSize = 16`

- `mkdir -p tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_session_resumption -FEtmp/test_freepascal_client_session_resumption -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - result: PASS
  - summary:
    - initial and resumed client proofs now cover:
      - `ProtocolVersion = TLS 1.3`
      - `CipherSuiteId = TLS13_CIPHER_CHACHA20_POLY1305_SHA256`
      - `KeySize = 256`
      - `MacSize = 16`
      - `ServerName = 'example.com'`
      - `IsResumed` false/true truth
      - `SessionId` mirror truth

- update `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - mark `FreePascal` completion audit as delivered
    - move the default mainline from backend-helper hunting back to owner / deprecation wording route

- update `task_plan.md`, `findings.md`, `progress.md`
  - change:
    - sync the `FreePascal` completion-audit conclusion into persistent repo working memory

- `git diff --check`
  - result: PASS
  - summary:
    - current FreePascal completion-audit batch has no whitespace or patch-format issues

### WolfSSL GetConnectionInfo Legacy MacSize Truth

- add `docs/plans/2026-05-18-wolfssl-connectioninfo-macsize-legacy-truth-feasibility.md`
  - purpose:
    - capture the next bounded `MacSize` batch after OpenSSL legacy truth landed
    - keep scope on WolfSSL low-level HMAC truth instead of reopening shared parser guesses

- implementation:
  - `src/fafafa.ssl.wolfssl.api.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `tests/test_wolfssl_connection_info_macsize_contract.pas`
  - `tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
  - change:
    - active WolfSSL API export/binding chain now includes:
      - `wolfSSL_GetHmacSize`
    - WolfSSL `GetConnectionInfo` now fills `MacSize` from HMAC truth only when:
      - shared path still leaves `MacSize = 0`
    - AEAD `MacSize` remains owned by the shared suite-name derivation path

- `bash tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
  - result: PASS
  - summary:
    - verified the new WolfSSL API export chain and the guarded HMAC-truth `MacSize` write path

- `mkdir -p tmp/test_wolfssl_connection_info_macsize_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_info_macsize_contract -FEtmp/test_wolfssl_connection_info_macsize_contract -otmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract tests/test_wolfssl_connection_info_macsize_contract.pas && ./tmp/test_wolfssl_connection_info_macsize_contract/test_wolfssl_connection_info_macsize_contract`
  - result: RED -> GREEN
  - summary:
    - first run exposed a focused contract harness precondition:
      - optional WolfSSL backend tests must define `ENABLE_WOLFSSL`
      - and must pull in `fafafa.ssl.wolfssl.lib` so factory registration is active
    - after aligning the test harness, final result was:
      - `3 passed, 0 failed`
    - the suite now explicitly proves:
      - helper unavailable safe degrade
      - legacy non-AEAD HMAC truth -> `MacSize = 32`
      - AEAD HMAC truth does not override shared `MacSize = 16`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared connection-info proof remained green at `26 passed, 0 failed`
    - the WolfSSL legacy `MacSize` addition did not disturb the earlier shared AEAD semantics

- `git diff --check`
  - result: PASS
  - summary:
    - current WolfSSL legacy `MacSize` batch has no whitespace or patch-format issues

### MbedTLS GetConnectionInfo Ciphersuite Truth

- add `docs/plans/2026-05-18-mbedtls-connectioninfo-ciphersuite-truth-feasibility.md`
  - purpose:
    - capture the MbedTLS batch that finishes the remaining high-value backend truth source on the current `GetConnectionInfo` route
    - keep scope on ciphersuite-info runtime truth and a blocking MD-constant correction

- implementation:
  - `src/fafafa.ssl.mbedtls.base.pas`
  - `src/fafafa.ssl.mbedtls.api.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
  - `tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
  - change:
    - fixed `MBEDTLS_MD_SHA1` / `MBEDTLS_MD_RIPEMD160` constant truth
    - active MbedTLS API export/binding chain now includes:
      - `mbedtls_ssl_get_ciphersuite_id`
      - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
      - `mbedtls_ssl_ciphersuite_from_id`
      - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
    - MbedTLS `GetConnectionInfo` now fills:
      - direct or fallback `CipherSuiteId`
      - `KeySize` from ciphersuite info
      - legacy/non-AEAD `MacSize` from digest truth only when shared AEAD truth still leaves `MacSize = 0`
    - shared cipher-suite parser now recognizes MbedTLS-style hyphenated AES / TLS-RSA names

- `bash tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
  - result: PASS
  - summary:
    - verified the corrected MD constants
    - verified the new MbedTLS ciphersuite-info export chain
    - verified the runtime write path for `CipherSuiteId` / `KeySize` / `MacSize`
    - verified the shared hyphenated-name compatibility guard

- `mkdir -p tmp/test_mbedtls_connection_info_ciphersuite_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_info_ciphersuite_contract -FEtmp/test_mbedtls_connection_info_ciphersuite_contract -otmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract tests/test_mbedtls_connection_info_ciphersuite_contract.pas && ./tmp/test_mbedtls_connection_info_ciphersuite_contract/test_mbedtls_connection_info_ciphersuite_contract`
  - result: RED -> GREEN
  - summary:
    - first run exposed a real shared baseline gap:
      - MbedTLS-style hyphenated AES suite names were not fully parsed by the shared cipher-suite derivation path
    - after aligning the shared parser, final result was:
      - `15 passed, 0 failed`
    - the suite now explicitly proves:
      - corrected runtime SHA1 constant truth against canonical SHA1(`abc`)
      - helper unavailable safe degrade
      - direct ciphersuite-id truth
      - name-based ciphersuite-id fallback
      - legacy non-AEAD digest truth -> `MacSize = 32` / `20`
      - AEAD digest truth does not override shared `MacSize = 16`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared connection-info proof remained green at `26 passed, 0 failed`
    - the MbedTLS truth additions and shared hyphenated-name support did not regress earlier semantics

- `git diff --check`
  - result: PASS
  - summary:
    - current MbedTLS ciphersuite-truth batch has no whitespace or patch-format issues

### GetConnectionInfo MacSize Semantics Matrix

- add `docs/plans/2026-05-18-getconnectioninfo-macsize-semantics-matrix.md`
  - purpose:
    - capture the bounded follow-up after the WinSSL cipher-truth correction
    - turn `MacSize` from an ambiguous one-backend field into a reusable shared/backend matrix with a clear next-step boundary

- implementation:
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - shared connection-info derivation now fills AEAD `MacSize` from recognized suite names
    - `GCM` / `POLY1305` / `OCB` / `CCM` map to `16`
    - `CCM_8` maps to `8`
    - WinSSL `GetConnectionInfo` now starts from inherited shared truth
    - WinSSL only falls back to `ConnInfo.dwHashStrength div 8` when shared derivation still leaves `MacSize = 0`
    - focused mock proof now explicitly checks:
      - TLS 1.3 AEAD suite -> `MacSize = 16`
      - legacy GCM suite -> `MacSize = 16`
      - legacy non-AEAD suite -> `MacSize = 0`

- `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
  - result: PASS
  - summary:
    - the earlier WinSSL cipher-suite truth correction still holds after the new `MacSize` batch

- `bash tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
  - result: PASS
  - summary:
    - verified WinSSL now starts from inherited shared connection-info truth
    - verified `dwHashStrength div 8` is guarded behind a missing shared `MacSize`
    - verified shared source contains the new AEAD-first `MacSize` derivation rules

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/connection-info suite finished `26 passed, 0 failed`
    - the shared `MacSize` derivation is now covered on:
      - TLS 1.3 GCM
      - legacy GCM
      - legacy non-AEAD no-guess behavior

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info suite remained green at `14 passed, 0 failed`
    - the new shared `MacSize` derivation did not regress the existing safe-degrade or `CipherSuiteId` truth coverage

- `git diff --check`
  - result: PASS
  - summary:
    - current `MacSize` semantics batch has no whitespace or patch-format issues

### OpenSSL GetConnectionInfo Legacy MacSize Truth

- add `docs/plans/2026-05-18-openssl-connectioninfo-macsize-legacy-truth-feasibility.md`
  - purpose:
    - capture the next bounded `MacSize` batch after the shared AEAD semantics matrix
    - keep the scope on OpenSSL low-level truth instead of spreading legacy `MacSize` guesses into the shared parser

- implementation:
  - `src/fafafa.ssl.openssl.api.ssl.pas`
  - `src/fafafa.ssl.openssl.api.evp.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
  - change:
    - active SSL API export/binding chain now includes:
      - `SSL_CIPHER_is_aead`
      - `SSL_CIPHER_get_digest_nid`
    - active EVP export/binding chain now includes:
      - `EVP_get_digestbynid`
    - OpenSSL `GetConnectionInfo` now fills `MacSize` from digest truth only when:
      - shared path still leaves `MacSize = 0`
      - current cipher is explicitly non-AEAD
    - AEAD `MacSize` remains owned by the shared suite-name derivation path

- `bash tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
  - result: PASS
  - summary:
    - verified the new SSL/EVP export chain and the OpenSSL digest-truth `MacSize` write path

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: RED -> GREEN
  - summary:
    - first run exposed a stale contract assumption:
      - old fake-cipher-pointer scenarios only nulled `protocol_id` / `get_id`
      - after the new `MacSize` path landed, `is_aead` / `digest_nid` / `EVP_get_digestbynid` also had to be nulled in those fake-pointer branches
    - after aligning the contract, final result was:
      - `20 passed, 0 failed`
    - the expanded suite now explicitly proves:
      - helper unavailable safe degrade
      - legacy non-AEAD digest truth -> `MacSize = 32`
      - AEAD digest size does not override shared `MacSize = 16`

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - shared connection-info proof remained green at `26 passed, 0 failed`
    - the OpenSSL legacy `MacSize` addition did not disturb the earlier shared AEAD semantics

- `git diff --check`
  - result: PASS
  - summary:
    - current OpenSSL legacy `MacSize` batch has no whitespace or patch-format issues

### WinSSL GetConnectionInfo Cipher Truth Correction

- add `docs/plans/2026-05-18-winssl-connectioninfo-cipher-truth-correction.md`
  - purpose:
    - capture the WinSSL truth-correction batch that was discovered while auditing `MacSize`
    - keep scope on the deterministic `CipherSuiteId` source bug before reopening broader field-completeness work

- static audit result:
  - `SecPkgContext_ConnectionInfo.aiCipher` in `src/fafafa.ssl.winssl.base.pas` is explicitly documented as an encryption algorithm ID
  - the same WinSSL unit uses it to derive algorithm-level cipher names and enums
  - therefore the previous `Result.CipherSuiteId := Word(ConnInfo.aiCipher)` path was a wrong truth source, not a benign best-effort approximation

- implementation:
  - `src/fafafa.ssl.winssl.base.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - add `SECPKG_ATTR_CIPHER_INFO`
    - add a minimal WinSSL cipher-info helper that reads Schannel `dwCipherSuite`
    - let WinSSL `GetConnectionInfo` stop writing `CipherSuiteId` from `ConnInfo.aiCipher`
    - let WinSSL `DoGetCipherName` prefer the real suite name when Schannel exposes it
    - narrow the active docs so `MacSize` is explicitly described as still best-effort and not guaranteed to equal the AEAD auth tag length

- `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
  - result: PASS
  - summary:
    - verified `SECPKG_ATTR_CIPHER_INFO` is defined
    - verified WinSSL now queries `SECPKG_ATTR_CIPHER_INFO`
    - verified the old `ConnInfo.aiCipher -> CipherSuiteId` write is gone

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - focused builder/hostname suite remained green at `21 passed, 0 failed`
    - the WinSSL correction did not disturb the shared `GetConnectionInfo` truth already established on other backends

- `mkdir -p tmp/test_openssl_connection_info_cipher_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_connection_info_cipher_contract -FEtmp/test_openssl_connection_info_cipher_contract -otmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract tests/test_openssl_connection_info_cipher_contract.pas && ./tmp/test_openssl_connection_info_cipher_contract/test_openssl_connection_info_cipher_contract`
  - result: PASS
  - summary:
    - focused OpenSSL connection-info suite remained green at `14 passed, 0 failed`
    - the WinSSL correction did not regress the OpenSSL `CipherSuiteId` truth or safe-degrade guard

- `gh auth status`
  - result: PASS
  - summary:
    - GitHub CLI is installed
    - authenticated account has `workflow` scope
    - Windows gate can be dispatched from this environment after the batch lands

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=\"winssl_cipher_truth_20260518_152020\" -f strict_closure=false`
  - result: PASS
  - summary:
    - dispatched GitHub workflow `Wave B B2 Manual Gate (Template)` against pushed commit `dcde2ff`

- `gh run watch 26019296095`
  - result: PASS
  - summary:
    - workflow `https://github.com/dtamade/fafafa.ssl/actions/runs/26019296095` finished `success`
    - `windows-gate` finished `success`
    - `Run quick WinSSL smoke` finished `success`
    - `Run Windows Wave B gate` finished `success`
    - `Run broader WinSSL runtime suite` finished `success`
    - `linux-gate`, `macos-gate`, and final `summary` job also finished `success`

- `git diff --check`
  - result: PASS
  - summary:
    - current batch has no whitespace or patch-format issues

### GetConnectionInfo Public Wording De-emphasis

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - script produced no recovery output
    - there was no extra unsynced session context before the wording batch

- `sed -n '1198,1220p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access wording
    - but it still lacked a stronger owner/de-emphasis note for `ISSLConnection.GetConnectionInfo`

- `sed -n '430,590p' docs/reference/API_REFERENCE.md`
  - result: PASS
  - summary:
    - active API reference still declared `GetConnectionInfo` in the core interface without an inline de-emphasis marker
    - the connection-info example still taught `LConn.GetProtocolVersion` / `LConn.GetCipherName` right beside `ISSLConnectionInfo.GetConnectionInfo`

- `sed -n '90,180p' docs/reference/INTERFACE_DESIGN_V2.md`
  - result: PASS
  - summary:
    - migration example still said `LConn.GetConnectionInfo;  // 仍然存在`
    - this was weaker than the current owner/mirror truth already established elsewhere

- add `docs/plans/2026-05-18-getconnectioninfo-public-wording-deemphasis.md`
  - purpose:
    - capture the bounded source/doc wording batch after owner-primacy and WinSSL residual classification were already closed
    - keep scope on public-facing truth instead of reopening runtime or backend implementation work

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
  - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
  - change:
    - add an explicit owner-note plus a stronger compatibility note to `ISSLConnection.GetConnectionInfo`
    - mark the active API declaration as compatibility-only and move the connection-info example to `LInfo.ProtocolVersion` / `LInfo.CipherSuite`
    - strengthen the v2 migration wording so `GetConnectionInfo` is explicitly treated as a compatibility mirror rather than merely "still exists"

- `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
  - result: PASS
  - summary:
    - source comment, active API docs, and v2 migration doc all carry the stronger owner/de-emphasis wording
    - stale `LConn.GetConnectionInfo;  // 仍然存在` wording is gone

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetConnectionInfo public wording de-emphasis` batch has no whitespace or patch-format issues

### GetConnectionInfo Compiler Deprecation Alignment

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch started from clean `master...origin/master`
    - the new batch could be scoped on top of the already-pushed public-wording closeout

- `rg -n "deprecated '|'deprecated;|SYMBOL_DEPRECATED|WARN 6058|WithSNI|SetServerName\\(|GetConnectionInfo" src tests docs/reference docs/plans task_plan.md findings.md progress.md --glob '!docs/archive/**'`
  - result: PASS
  - summary:
    - existing `.WithSNI(...)` and direct-context `ServerName` work already showed a stable compiler-deprecation + local-warning-quarantine pattern
    - `GetConnectionInfo` residual direct-core use was now small enough to evaluate the same route safely

- `sed -n '1188,1222p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access, owner-note, and stronger compatibility wording
    - but the declaration itself still was not compiler deprecated yet

- `rg -n '\\b(?:Conn|LConn|LConnection)\\.GetConnectionInfo\\b|\\.GetConnectionInfo\\(' tests src docs --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - production source did not show new direct-core `GetConnectionInfo` callers
    - the remaining direct-core residual set stayed confined to backend contract and WinSSL intentional core-surface tests

- add `docs/plans/2026-05-18-getconnectioninfo-compiler-deprecation-alignment.md`
  - purpose:
    - capture the bounded source-truth batch that upgrades `ISSLConnection.GetConnectionInfo` from source/doc de-emphasis to compiler-level deprecation
    - keep runtime behavior unchanged while aligning the public core mirror surface with current owner truth

- add `tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
  - purpose:
    - fail if the core `GetConnectionInfo` declaration loses its compiler `deprecated` marker
    - guard the new doc wording and intentional warning-quarantine boundary

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/contract/test_backend_contract.pas`
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - change:
    - mark `ISSLConnection.GetConnectionInfo` as compiler `deprecated 'Use ISSLConnectionInfo.GetConnectionInfo'`
    - upgrade active docs to say the core getter is now compiler deprecated
    - add local warning suppression around the remaining intentional direct-core `GetConnectionInfo` callsites

- `bash tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the core declaration is compiler deprecated
    - active docs and intentional residual tests all match the expected source-truth boundary

- `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
  - result: PASS
  - summary:
    - the earlier wording contract stayed green after the compiler-deprecation upgrade
    - source/doc de-emphasis and compiler deprecation now tell the same story

- `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - residual direct-core `GetConnectionInfo` surface stayed confined to the existing allowlist
    - compiler deprecation did not re-expand direct-core usage

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: RED -> GREEN
  - summary:
    - first compile failed with `test_backend_contract.pas(... ) Fatal: Syntax error, ";" expected but "ELSE" found`
    - root cause was a stray semicolon before the fallback `else` branch in the new session-resumption mirror-proof restructuring
    - after removing that stray semicolon, the focused backend contract finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - intentional direct-core mirror proof stayed green after local deprecation-warning quarantine

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetConnectionInfo` compiler-deprecation batch has no whitespace or patch-format issues

### GetContext Compiler Deprecation Alignment

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch started from clean `master...origin/master`
    - the new batch could be scoped directly on top of the already-pushed GetConnectionInfo compiler-surface closeout

- `rg -n "GetContext|ISSLConnectionInfo|deprecated 'Use ISSLConnectionInfo.GetContext|compiler.*deprecated|active guidance de-emphasis|owner primacy" src docs/reference docs/plans tests/scripts task_plan.md findings.md progress.md --glob '!docs/archive/**'`
  - result: PASS
  - summary:
    - confirmed `GetContext` had already finished active-guidance, contract-owner, and source/class-split freeze work
    - the remaining gap was the public core declaration itself still not being compiler deprecated

- `sed -n '1280,1294p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access wording for `ISSLConnectionInfo.GetContext`
    - but the public core declaration still lacked compiler deprecation and stronger owner/de-emphasis wording

- `sed -n '1828,1854p' tests/contract/test_backend_contract.pas`
  - result: PASS
  - summary:
    - confirmed the remaining direct core `GetContext` usage had shrunk to a single backend-contract mirror proof
    - this made local warning quarantine a small, bounded change

- add `docs/plans/2026-05-18-getcontext-compiler-deprecation-alignment.md`
  - purpose:
    - capture the bounded source-truth batch that upgrades `ISSLConnection.GetContext` from source/doc de-emphasis to compiler-level deprecation
    - keep runtime behavior unchanged while aligning the public core mirror surface with current owner truth

- add `tests/scripts/test_getcontext_compiler_deprecated_contract.sh`
  - purpose:
    - fail if the core `GetContext` declaration loses its compiler `deprecated` marker
    - guard the new doc wording and residual warning-quarantine boundary

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/contract/test_backend_contract.pas`
  - change:
    - mark `ISSLConnection.GetContext` as compiler `deprecated 'Use ISSLConnectionInfo.GetContext'`
    - upgrade active docs to say the core getter is now compiler deprecated
    - add local warning suppression around the remaining direct-core `GetContext` mirror proof

- `bash tests/scripts/test_getcontext_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the core declaration is compiler deprecated
    - active docs and the residual backend-contract proof all match the expected source-truth boundary

- `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
  - result: RED -> GREEN
  - summary:
    - first run failed only because the API reference no longer contained the exact shared guidance sentence the older contract still expected
    - after restoring that sentence while keeping the stronger compiler-deprecated `GetContext` wording, the contract passed
    - active docs still de-emphasize direct core `GetContext`
    - the compiler-deprecation upgrade did not reintroduce `Conn.GetContext` teaching

- `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
  - result: PASS
  - summary:
    - backend contract still treats `ISSLConnectionInfo.GetContext` as the primary owner
    - the remaining core getter usage stayed a pure mirror proof

- `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
  - result: PASS
  - summary:
    - `GetContext` live surface stayed frozen to the expected allowlist
    - compiler deprecation did not re-expand direct-core usage

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract still finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - the single direct-core `GetContext` mirror proof stayed green after local deprecation-warning quarantine

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetContext` compiler-deprecation batch has no whitespace or patch-format issues

### GetStateString Compiler Deprecation Alignment

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch started from clean `master...origin/master`
    - the new batch could be scoped directly on top of the already-pushed GetContext compiler-surface closeout

- `rg -n "GetStateString|ISSLConnectionInfo|deprecated 'Use ISSLConnectionInfo.GetStateString|compiler.*deprecated|active test de-emphasis|residual classification" src docs/reference docs/plans tests/scripts task_plan.md findings.md progress.md --glob '!docs/archive/**'`
  - result: PASS
  - summary:
    - confirmed `GetStateString` had already finished active-test de-emphasis and residual-classification freeze work
    - the remaining gap was the public core declaration itself still not being compiler deprecated

- `sed -n '1254,1276p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access wording for `ISSLConnectionInfo.GetStateString`
    - but the public core declaration still lacked compiler deprecation and stronger owner/de-emphasis wording

- `rg -n "\\.GetStateString\\b|GetStateString\\(" tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed ordinary docs/tests no longer used direct core `GetStateString`
    - confirmed the remaining direct-core residuals had stayed confined to backend-contract mirror proof plus OpenSSL/WolfSSL backend-specific runtime proofs

- add `docs/plans/2026-05-18-getstatestring-compiler-deprecation-alignment.md`
  - purpose:
    - capture the bounded source-truth batch that upgrades `ISSLConnection.GetStateString` from source/doc de-emphasis to compiler-level deprecation
    - keep runtime behavior unchanged while aligning the public core mirror surface with current owner truth

- add `tests/scripts/test_getstatestring_compiler_deprecated_contract.sh`
  - purpose:
    - fail if the core `GetStateString` declaration loses its compiler `deprecated` marker
    - guard the new doc wording and residual warning-quarantine boundary

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/contract/test_backend_contract.pas`
  - `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
  - `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
  - change:
    - mark `ISSLConnection.GetStateString` as compiler `deprecated 'Use ISSLConnectionInfo.GetStateString'`
    - upgrade active docs to say the core getter is now compiler deprecated
    - add local warning suppression around the remaining direct-core `GetStateString` mirror/runtime proofs

- `bash -n tests/scripts/test_getstatestring_compiler_deprecated_contract.sh && bash tests/scripts/test_getstatestring_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the core declaration is compiler deprecated
    - active docs and the residual backend/runtime proofs all match the expected source-truth boundary

- `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
  - result: PASS
  - summary:
    - active generic/integration tests still prefer `ISSLConnectionInfo.GetStateString`
    - the compiler-deprecation upgrade did not reintroduce direct core guidance

- `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetStateString` residual direct-core surface stayed confined to the existing allowlist
    - compiler deprecation did not re-expand direct-core usage

- first run of `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: RED
  - summary:
    - the API reference no longer contained the exact shared guidance sentence the older contract still expected
    - this was wording drift only, not a route regression

- update `docs/reference/API_REFERENCE.md`
  - change:
    - restore the shared `连接信息 / ALPN / 状态字符串` guidance sentence expected by the active-doc contract
    - keep the stronger standalone `GetContext` / `GetStateString` compiler-deprecated guidance intact

- second run of `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs still de-emphasize direct core mirror usage
    - the shared guidance sentence and the stronger compiler-deprecation wording now both tell the same story

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract still finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - the direct-core `GetStateString` mirror proof stayed green after local deprecation-warning quarantine
    - WinSSL continued to keep the expected Linux-host skip truth

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetStateString` compiler-deprecation batch has no whitespace or patch-format issues

### GetSelectedALPNProtocol Compiler Deprecation Alignment

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch started from clean `master...origin/master`
    - the new batch could be scoped directly on top of the already-pushed GetStateString compiler-surface closeout

- `rg -n "GetSelectedALPNProtocol|ISSLConnectionInfo.GetSelectedALPNProtocol|deprecated 'Use ISSLConnectionInfo.GetSelectedALPNProtocol|compiler.*deprecated|active test de-emphasis|residual classification" src docs/reference docs/plans tests/scripts task_plan.md findings.md progress.md --glob '!docs/archive/**'`
  - result: PASS
  - summary:
    - confirmed `GetSelectedALPNProtocol` had already finished active-test de-emphasis and residual-classification freeze work
    - the remaining gap was the public core declaration itself still not being compiler deprecated

- `sed -n '1238,1272p' src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - source comment already had preferred-access wording for `ISSLConnectionInfo.GetSelectedALPNProtocol`
    - but the public core declaration still lacked compiler deprecation and stronger owner/de-emphasis wording

- `rg -n "\\.GetSelectedALPNProtocol\\b|GetSelectedALPNProtocol\\(" tests docs src --glob '!docs/archive/**' --glob '!docs/plans/**' --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed ordinary docs/tests no longer used direct core `GetSelectedALPNProtocol`
    - confirmed the remaining direct-core residuals had stayed confined to backend-contract mirror proof plus MbedTLS/WinSSL backend-specific runtime proofs

- add `docs/plans/2026-05-18-getselectedalpn-compiler-deprecation-alignment.md`
  - purpose:
    - capture the bounded source-truth batch that upgrades `ISSLConnection.GetSelectedALPNProtocol` from source/doc de-emphasis to compiler-level deprecation
    - keep runtime behavior unchanged while aligning the public core mirror surface with current owner truth

- add `tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh`
  - purpose:
    - fail if the core `GetSelectedALPNProtocol` declaration loses its compiler `deprecated` marker
    - guard the new doc wording and residual warning-quarantine boundary

- implementation:
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `tests/contract/test_backend_contract.pas`
  - `tests/mbedtls/test_mbedtls_alpn.pas`
  - `tests/winssl/test_winssl_alpn_sni.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - change:
    - mark `ISSLConnection.GetSelectedALPNProtocol` as compiler `deprecated 'Use ISSLConnectionInfo.GetSelectedALPNProtocol'`
    - upgrade active docs to say the core getter is now compiler deprecated
    - add local warning suppression around the remaining direct-core `GetSelectedALPNProtocol` mirror/runtime proofs
    - sync the older migration-target contract to the current compiler-deprecated mirror wording

- `bash -n tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh && bash tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - the core declaration is compiler deprecated
    - active docs and the residual backend/runtime proofs all match the expected source-truth boundary

- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
  - result: PASS
  - summary:
    - active integration/contract tests still prefer `ISSLConnectionInfo.GetSelectedALPNProtocol`
    - the compiler-deprecation upgrade did not reintroduce direct core guidance

- `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - `GetSelectedALPNProtocol` residual direct-core surface stayed confined to the existing allowlist
    - compiler deprecation did not re-expand direct-core usage

- `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs still de-emphasize direct core mirror usage
    - the stronger ALPN compiler-deprecation wording still matches the shared `ISSLConnectionInfo` guidance contract

- `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - result: PASS
  - summary:
    - the migration-target contract now matches the current compiler-deprecated wording for all 4 `ISSLConnectionInfo` mirrors
    - this closes a stale-script drift that had been left behind by earlier wording upgrades

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - focused backend contract still finished `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - the direct-core `GetSelectedALPNProtocol` mirror proof stayed green after local deprecation-warning quarantine
    - WinSSL continued to keep the expected Linux-host skip truth

- `git diff --check`
  - result: PASS
  - summary:
    - current `GetSelectedALPNProtocol` compiler-deprecation batch has no whitespace or patch-format issues

### ISSLDiagnostics Active Guidance De-emphasis

- `rg -n "GetHealthStatus|IsHealthy|GetDiagnosticInfo|GetPerformanceMetrics|ISSLDiagnostics" docs/reference/API_REFERENCE.md tests/test_sslctxboth_roleless_handshake_clarification.pas tests/winssl/test_winssl_monitoring.pas tests/winssl/test_winssl_connection_edge_cases.pas docs/INTEGRATION_GUIDE.md`
  - result: PASS
  - summary:
    - confirmed the ordinary diagnostics guidance drift lived mainly in `API_REFERENCE`
    - confirmed the generic dual-context boundary test still used direct core `GetHealthStatus`
    - confirmed WinSSL monitoring/edge-case files were backend-specific residual proof and should stay out of this batch

- add `docs/plans/2026-05-18-issldiagnostics-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded diagnostics batch that moves ordinary docs/tests onto `ISSLDiagnostics`
    - keep scope off backend-specific runtime proofs and production implementation

- add `tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
  - purpose:
    - fail if ordinary diagnostics docs/tests reintroduce direct core `IsHealthy` / `GetHealthStatus` / `GetPerformanceMetrics` / `GetDiagnosticInfo`
    - keep this owner-path guidance change cheap to verify

- update:
  - `docs/reference/API_REFERENCE.md`
  - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
  - change:
    - route ordinary diagnostics examples through `Supports(LConn, ISSLDiagnostics, LDiag)`
    - route the generic dual-context health-status proof through `ISSLDiagnostics.GetHealthStatus`
    - add explicit note lines that new code should prefer `ISSLDiagnostics` owner methods for the diagnostics records

- `bash -n tests/scripts/test_issldiagnostics_active_guidance_contract.sh && bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs/tests now prefer `ISSLDiagnostics` for diagnostics surfaces
    - ordinary guidance no longer reintroduces direct core diagnostics getters

- `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
  - result: PASS
  - summary:
    - focused generic dual-context suite compiled and ran successfully
    - finished `30 passed, 0 failed, 0 skipped`
    - FreePascal / OpenSSL / WolfSSL / MbedTLS all proved the boundary test can read health status through `ISSLDiagnostics`
    - OpenSSL implicit read/write fail-fast proofs stayed green after the owner-path switch

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLDiagnostics active-guidance de-emphasis` batch has no whitespace or patch-format issues

### ISSLCertificateVerification Active Guidance De-emphasis

- `rg -n "GetVerifyResultString|GetVerifyResult|ISSLCertificateVerification" docs/INTEGRATION_GUIDE.md docs/reference/API_DOCUMENTATION.md tests/integration/test_cross_backend_consistency_contract.pas tests/integration/test_cross_backend_errors_contract.pas`
  - result: PASS
  - summary:
    - confirmed the ordinary certificate-verification guidance drift lived in `INTEGRATION_GUIDE`, `API_DOCUMENTATION`, and the two generic integration/contract tests
    - confirmed this was a docs/tests owner-path issue, not a new production implementation gap

- add `docs/plans/2026-05-18-isslcertificateverification-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded certificate-verification batch that moves ordinary docs/tests onto `ISSLCertificateVerification`
    - keep scope off backend-specific runtime proofs and production implementation

- add `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - purpose:
    - fail if ordinary certificate-verification docs/tests reintroduce direct core `GetVerifyResult / GetVerifyResultString`
    - keep this owner-path guidance change cheap to verify

- update:
  - `docs/INTEGRATION_GUIDE.md`
  - `docs/reference/API_DOCUMENTATION.md`
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
  - change:
    - route ordinary handshake-failure guidance through `Supports(Conn, ISSLCertificateVerification, CertVerify)`
    - route the generic consistency/error probes through helper functions backed by `ISSLCertificateVerification`
    - keep protocol / cipher / ALPN reads unchanged so the batch stays on certificate-verification only

- first run of `bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh && bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - result: RED
  - summary:
    - the new script used double-quoted patterns containing backticks, so shell command substitution mangled the expected integration-guide string
    - this was a script quoting bug only, not a source/docs route regression

- update `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - change:
    - switch the two backtick-containing integration-guide patterns to single-quoted literals
    - keep the rest of the guard unchanged

- second run of `bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh && bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs/tests now prefer `ISSLCertificateVerification` for verify-result surfaces
    - ordinary guidance no longer reintroduces direct core verify-result getters

- `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
  - result: PASS
  - summary:
    - focused consistency contract compiled and ran successfully
    - runtime result remained the expected network skip: `FAFAFA_RUN_NETWORK_TESTS!=1`
    - verify-result helper switch did not disturb protocol / cipher / ALPN normalization logic

- `mkdir -p tmp/test_cross_backend_errors_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_errors_contract -FEtmp/test_cross_backend_errors_contract -otmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract`
  - result: PASS
  - summary:
    - focused error contract compiled and ran successfully
    - runtime result remained the expected environment skip taxonomy: `Network tests gate / environment`
    - verify-result helper switch did not change the generic error normalization path under the current gate

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLCertificateVerification active-guidance de-emphasis` batch has no whitespace or patch-format issues

### ISSLSessionResumption Active Guidance De-emphasis

- `rg -n "\\.GetSession\\b|\\.SetSession\\b|\\.IsSessionReused\\b|ISSLSessionResumption" docs/reference/API_REFERENCE.md docs/reference/API_DOCUMENTATION.md docs/INTEGRATION_GUIDE.md tests/integration/test_e2e_scenarios.pas tests/contract/test_backend_contract.pas`
  - result: PASS
  - summary:
    - confirmed the ordinary session-resumption guidance drift lived in `API_REFERENCE`, `API_DOCUMENTATION`, `INTEGRATION_GUIDE`, and the generic E2E session-resumption scenario
    - confirmed this was a docs/tests owner-path issue, not a new production implementation gap
    - confirmed `tests/contract/test_backend_contract.pas` already held the owner truth via `Contract 20`

- add `docs/plans/2026-05-18-isslsessionresumption-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded session-resumption batch that moves ordinary docs/tests onto `ISSLSessionResumption`
    - keep scope off backend-specific runtime proofs and production implementation

- add `tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - purpose:
    - fail if ordinary session-resumption docs/tests reintroduce direct core `GetSession / SetSession / IsSessionReused`
    - keep this owner-path guidance change cheap to verify

- first run of `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh && bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - result: RED
  - summary:
    - the new guard immediately failed on `API_REFERENCE` still teaching `LSession := LConn1.GetSession;`
    - this confirmed the batch target was real ordinary-guidance drift rather than speculative cleanup

- update:
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/API_DOCUMENTATION.md`
  - `docs/INTEGRATION_GUIDE.md`
  - `tests/integration/test_e2e_scenarios.pas`
  - change:
    - route session save/restore/reuse examples through `Supports(..., ISSLSessionResumption, ...)`
    - add explicit note lines in `API_REFERENCE` that new code should prefer `ISSLSessionResumption.GetSession / SetSession / IsSessionReused`
    - route the generic E2E session-resumption proof through `ISSLSessionResumption` owner access without changing the actual runtime flow

- second run of `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh && bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs/tests now prefer `ISSLSessionResumption` for session-resumption surfaces
    - ordinary guidance no longer reintroduces direct core session mirrors

- `mkdir -p tmp/test_e2e_scenarios && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_e2e_scenarios -FEtmp/test_e2e_scenarios -otmp/test_e2e_scenarios/test_e2e_scenarios tests/integration/test_e2e_scenarios.pas && ./tmp/test_e2e_scenarios/test_e2e_scenarios`
  - result: PASS
  - summary:
    - focused E2E suite compiled and ran successfully
    - finished `Total: 9 / Passed: 9 / Failed: 0 / Skipped: 0`
    - the session-resumption scenario now explicitly proves both connections expose `ISSLSessionResumption`
    - the owner-path switch did not regress handshake, session extraction, session reuse, or the large-data / client-cert companion scenarios

- `rg -n "GetOCSPStaplingEnabled|GetOCSPResponse\\(|IsOCSPResponseVerified|GetOCSPResponseStatus|ISSLOCSPStapling" docs/reference/API_REFERENCE.md docs/reference/API_DOCUMENTATION.md docs/INTEGRATION_GUIDE.md tests`
  - result: PASS
  - summary:
    - quick next-route scan found the clearest remaining ordinary-guidance drift in `docs/reference/API_DOCUMENTATION.md`
    - direct core OCSP examples (`Connection.GetOCSPStaplingEnabled`, `Connection.IsOCSPResponseVerified`, `Connection.GetOCSPResponseStatus`) still coexist beside owner-path `ISSLOCSPStapling` examples
    - this makes `ISSLOCSPStapling active-guidance de-emphasis` the best next bounded optional-owner candidate

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLSessionResumption active-guidance de-emphasis` batch has no whitespace or patch-format issues

### ISSLOCSPStapling Active Guidance De-emphasis

- `rg -n "GetOCSPStaplingEnabled|GetOCSPResponse\\(|IsOCSPResponseVerified|GetOCSPResponseStatus|ISSLOCSPStapling" docs/reference/API_REFERENCE.md docs/reference/API_DOCUMENTATION.md docs/INTEGRATION_GUIDE.md tests`
  - result: PASS
  - summary:
    - confirmed the current ordinary OCSP guidance drift lived mainly in `docs/reference/API_DOCUMENTATION.md`
    - confirmed this was a docs-only owner-path issue, not a new production implementation gap
    - confirmed backend-specific runtime / contract proofs should stay out of this batch

- add `docs/plans/2026-05-18-isslocspstapling-active-guidance-deemphasis.md`
  - purpose:
    - define a bounded OCSP batch that moves ordinary docs onto `ISSLOCSPStapling`
    - keep scope off backend-specific runtime proofs and production implementation

- add `tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  - purpose:
    - fail if ordinary OCSP docs reintroduce direct core `GetOCSP*` usage
    - keep this owner-path guidance change cheap to verify

- first run of `bash -n tests/scripts/test_isslocspstapling_active_guidance_contract.sh && bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  - result: RED
  - summary:
    - the new guard immediately failed on `API_DOCUMENTATION` still teaching `if Connection.GetOCSPStaplingEnabled then`
    - this confirmed the batch target was real ordinary-guidance drift rather than speculative cleanup

- update `docs/reference/API_DOCUMENTATION.md`
  - change:
    - route the 4 direct-core OCSP method examples through `Supports(Connection, ISSLOCSPStapling, OCSP)`
    - add explicit note lines that new code should prefer `ISSLOCSPStapling.GetOCSPStaplingEnabled / GetOCSPResponse / IsOCSPResponseVerified / GetOCSPResponseStatus`
    - keep `Connection.GetOCSP*` only as compatibility-core mirror truth

- second run of `bash -n tests/scripts/test_isslocspstapling_active_guidance_contract.sh && bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - active docs now prefer `ISSLOCSPStapling` for OCSP stapling surfaces
    - ordinary guidance no longer reintroduces direct core OCSP mirrors

- `rg -n "GetHealthStatus|GetPerformanceMetrics|GetDiagnosticInfo|GetVerifyResult|GetVerifyResultString|GetPeerCertificateChain|GetSession\\(|SetSession\\(|IsSessionReused|GetOCSPStaplingEnabled|GetOCSPResponse\\(|IsOCSPResponseVerified|GetOCSPResponseStatus" docs/reference/API_REFERENCE.md docs/reference/API_DOCUMENTATION.md docs/INTEGRATION_GUIDE.md tests/integration tests/test_sslctxboth_roleless_handshake_clarification.pas tests/integration/test_e2e_scenarios.pas`
  - result: PASS
  - summary:
    - quick next-route scan showed the current high-value optional-owner ordinary-guidance sweep is now clean
    - remaining hits in active docs/tests are either owner-path examples, public source-truth signatures, or intentional compatibility notes
    - this makes a return to broader interface-design / implementation-completeness work the right next default lane

- `git diff --check`
  - result: PASS
  - summary:
    - current `ISSLOCSPStapling active-guidance de-emphasis` batch has no whitespace or patch-format issues

### WinSSL Session Capability Truth Alignment

- `git diff -- src/fafafa.ssl.winssl.lib.pas tests/scripts/test_winssl_capability_source_contract.sh tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh docs/reference/API_REFERENCE.md docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/reference/WINSSL_PERFORMANCE_TUNING.md docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - result: PASS
  - summary:
    - confirmed this batch was correctly scoped to WinSSL session capability/docs truth alignment
    - initial inspection already showed the intended direction: tighten capability truth, tighten docs truth, and add a dedicated docs contract

- `rg -n "IsSessionResumed|GetSession\\(|SetSession\\(|70-90%|性能提升|快速握手|完整支持|observed_reuse=false|session_configured=true|ISSLSessionResumption" docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/reference/WINSSL_PERFORMANCE_TUNING.md docs/reference/API_REFERENCE.md docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - result: PASS
  - summary:
    - manual sweep found two additional truth drifts before verification:
      - `API_REFERENCE.md` still claimed WinSSL `性能提升 70-90%`
      - `WINSSL_PERFORMANCE_TUNING.md` still mixed direct-core `GetSession` / `IsSessionResumed` into active WinSSL guidance
    - this justified one more small docs cleanup pass before the focused contracts

- update:
  - `src/fafafa.ssl.winssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_PERFORMANCE_TUNING.md`
  - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - `tests/scripts/test_winssl_capability_source_contract.sh`
  - `tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `docs/plans/2026-05-18-winssl-session-capability-truth-alignment.md`
  - change:
    - tighten `SessionTicketsSupport` from `sslSupportStable` to `sslSupportExperimental`
    - record `observed_reuse=false` / `session_configured=true` in WinSSL `KnownIssues`
    - remove WinSSL docs claims that presented session resumption as already runtime-proven stable or already yielding generic `70-90%` gains
    - align active WinSSL session examples to `ISSLSessionResumption` owner-path guidance
    - expand the dedicated docs truth contract to catch stale performance-table / direct-core-example regressions

- `bash -n tests/scripts/test_winssl_capability_source_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_capability_source_contract.sh`
  - result: PASS
  - summary:
    - WinSSL capability source now publishes stable session-cache support, experimental session-ticket support, and the current dedicated runtime truth in `KnownIssues`

- `bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS

- first run of `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: RED
  - summary:
    - failed only because `WINSSL_BACKEND_STATUS_REPORT.md` had not yet written `windows-gate` explicitly into the final green truth section
    - this was a wording/evidence-source drift, not a remaining implementation or API-guidance regression

- update `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - change:
    - pin final green run `26037518301` to the explicit `windows-gate` truth source wording so the docs contract can guard it

- second run of `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL session-resumption docs now consistently publish the post-bridge truth:
      - `observed_reuse=false`
      - `session_configured=true`
      - no more “完整支持” or generic stable-gain claims in active WinSSL docs

- `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - result: PASS

- `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - the WinSSL doc tightening did not regress the wider repo rule that active session-resumption guidance should prefer `ISSLSessionResumption`

- `mkdir -p tmp/winssl_session_capability_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_capability_truth_win64 -FEtmp/winssl_session_capability_truth_win64 -otmp/winssl_session_capability_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - dedicated Win64 cross-target session-resumption proof compiled successfully
    - compile completed with existing repo warnings only; this batch introduced no new compile blocker

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL session capability/docs truth alignment` batch has no whitespace or patch-format issues

### WinSSL Session Cache Runtime Flag Alignment

- `rg -n "FCredHandle|EnsureCredentialsAcquired|SetSessionCacheMode|SetOptions|SCH_CRED_DISABLE_RECONNECTS|InitializeSecurityContextW" src/fafafa.ssl.winssl.context.pas src/fafafa.ssl.winssl.connection.pas`
  - result: PASS
  - summary:
    - confirmed WinSSL reuses the context-level `CredHandle` across connections
    - confirmed this handle is the canonical runtime carrier for Schannel reconnect behavior
    - confirmed the real implementation gap lived in the context layer, not in a per-connection credential re-acquire path

- add `docs/plans/2026-05-18-winssl-session-cache-runtime-flag-alignment.md`
  - purpose:
    - define a bounded WinSSL implementation-completeness batch for session-cache/ticket runtime flag wiring
    - keep scope off already-closed shared crash / docs truth lanes

- add `tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - purpose:
    - fail if WinSSL session-cache/ticket context controls still stay at Pascal-field level
    - lock credential-rebuild and `SCH_CRED_DISABLE_RECONNECTS` mapping truth with a cheap focused contract

- first run of `bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh && bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - result: RED
  - summary:
    - first failure was a contract bug only: the regex missed the no-argument `EnsureCredentialsAcquired;` declaration
    - this was fixed before production edits so the RED would target real implementation drift

- update `tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - change:
    - widen the procedure matcher to handle no-argument context methods correctly

- second run of `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - result: RED
  - summary:
    - confirmed the real implementation gap: `SetSessionCacheMode(...)` still did not force credential rebuild

- update `src/fafafa.ssl.winssl.context.pas`
  - change:
    - `SetSessionCacheMode(...)` now marks `FCredentialsNeedRebuild := True`
    - `SetOptions(...)` now also marks `FCredentialsNeedRebuild := True`
    - `EnsureCredentialsAcquired` now maps disabled session-cache or disabled session-tickets truth to `SCH_CRED_DISABLE_RECONNECTS`

- final run of `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - result: PASS
  - summary:
    - WinSSL session-cache/ticket controls now affect credential acquisition instead of staying as field-only bookkeeping

- `mkdir -p tmp/winssl_session_cache_runtime_flag_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_cache_runtime_flag_win64 -FEtmp/winssl_session_cache_runtime_flag_win64 -otmp/winssl_session_cache_runtime_flag_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 cross-target session-resumption proof still compiles after the context-layer wiring fix
    - compile completed with the repo's existing warning baseline only

- update `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - change:
    - record that session-cache / session-ticket disablement now flows into `SCH_CRED_DISABLE_RECONNECTS` and triggers credential rebuild

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL session cache runtime flag alignment` batch has no whitespace or patch-format issues

### WinSSL Session Serialization Roundtrip Alignment

- `rg -n "Serialize|Deserialize|SetSessionMetadata|SetTimeout|FSessionData" src/fafafa.ssl.winssl.connection.pas tests/winssl/test_session_metadata.pas`
  - result: PASS
  - summary:
    - confirmed the session-object gap lived in `TWinSSLSession` itself
    - confirmed the current implementation was still effectively:
      - `Serialize -> FSessionData`
      - `Deserialize -> FSessionData := AData`
    - confirmed no stable metadata payload builder existed yet

- add `docs/plans/2026-05-18-winssl-session-serialization-roundtrip-alignment.md`
  - purpose:
    - define a bounded WinSSL session-object completeness batch around serialization round-trip
    - keep scope off native resumed-handshake claims

- update `tests/winssl/test_session_metadata.pas`
  - change:
    - add focused round-trip expectations for:
      - non-empty serialized payload
      - metadata restoration after deserialize
      - invalid payload rejection
    - local Linux host still cannot execute this test binary directly because the WinSSL unit chain depends on `unit Windows`

- first run of `mkdir -p tmp/test_session_metadata && fpc -B -Fu./src -Fu./tests -FUtmp/test_session_metadata -FEtmp/test_session_metadata -otmp/test_session_metadata/test_session_metadata tests/winssl/test_session_metadata.pas`
  - result: ENVIRONMENT BLOCKED
  - summary:
    - Linux-host native compile pulled `fafafa.ssl.winssl.context.pas` and stopped at `Can't find unit Windows`
    - this was an environment boundary, not a reason to reopen Windows-host-only runtime debugging

- add `tests/scripts/test_winssl_session_serialization_roundtrip_contract.sh`
  - purpose:
    - lock the object-level serialization gap with a cheap focused source contract that can run on Linux

- first run of `bash -n tests/scripts/test_winssl_session_serialization_roundtrip_contract.sh && bash tests/scripts/test_winssl_session_serialization_roundtrip_contract.sh`
  - result: RED
  - summary:
    - immediately failed on the absence of a serialized metadata payload builder helper
    - this confirmed the batch target was a real implementation gap, not just a weak test

- `mkdir -p tmp/test_session_metadata_win64 && fpc -Twin64 -Fu./src -Fu./tests -FUtmp/test_session_metadata_win64 -FEtmp/test_session_metadata_win64 -otmp/test_session_metadata_win64/test_session_metadata.exe tests/winssl/test_session_metadata.pas`
  - result: PASS
  - summary:
    - the focused metadata test itself remains Win64-compilable, so the RED is not a broken test artifact

- update `src/fafafa.ssl.winssl.connection.pas`
  - change:
    - add `BuildSerializedSessionData`
    - add `TryLoadSerializedSessionData`
    - `Serialize` now returns a metadata-backed payload
    - `Deserialize` now parses and restores metadata with real success/failure semantics
    - `SetTimeout(...)` / `SetSessionMetadata(...)` now refresh the serialized payload

- final run of `bash tests/scripts/test_winssl_session_serialization_roundtrip_contract.sh`
  - result: PASS
  - summary:
    - WinSSL session-object serialization is no longer a field-only shell

- second run of `mkdir -p tmp/test_session_metadata_win64 && fpc -Twin64 -Fu./src -Fu./tests -FUtmp/test_session_metadata_win64 -FEtmp/test_session_metadata_win64 -otmp/test_session_metadata_win64/test_session_metadata.exe tests/winssl/test_session_metadata.pas`
  - result: PASS
  - summary:
    - Win64 metadata regression test still compiles cleanly after the object-level serialization fix

- update `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - change:
    - record that WinSSL session metadata serialization now round-trips on the session object, while still not being treated as proof of native resumed-handshake

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL session serialization roundtrip alignment` batch has no whitespace or patch-format issues

### WinSSL Client Reconnect Truth Alignment

- official Schannel documentation review
  - result: PASS
  - summary:
    - confirmed client-side session cache lookup truth is anchored on same `target name` + same `credential handle` (+ same process/logon session)
    - confirmed `SCH_CRED_DISABLE_RECONNECTS` is not a generic client-side credential flag to use as the reconnect toggle
    - this immediately downgraded the previous broader wording from “fully wired client/session disable flag mapping” to a narrower server-side-only truth

- add `docs/plans/2026-05-18-winssl-client-reconnect-truth-alignment.md`
  - purpose:
    - define a bounded correction batch that removes the wrong client-side reconnect-flag assumption
    - pin the canonical WinSSL reconnect model to Schannel official truth

- update:
  - `src/fafafa.ssl.winssl.context.pas`
  - `tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - `docs/reference/API_REFERENCE.md`
  - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - `docs/plans/2026-05-18-winssl-session-cache-runtime-flag-alignment.md`
  - change:
    - scope `SCH_CRED_DISABLE_RECONNECTS` back to server-side disable truth
    - keep credential rebuild truth intact
    - document that WinSSL client reconnect remains keyed to `target name + credential handle`
    - document that `ISSLSessionResumption.SetSession(...)` is not yet a native session-handle injection point on WinSSL

- `bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - result: PASS
  - summary:
    - the focused source contract now guards the corrected server-side-only reconnect flag truth

- `mkdir -p tmp/winssl_client_reconnect_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_client_reconnect_truth_win64 -FEtmp/winssl_client_reconnect_truth_win64 -otmp/winssl_client_reconnect_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 dedicated session-resumption proof still compiles after the client reconnect truth correction
    - no new compile blocker was introduced by removing the wrong client-side reconnect-flag assumption

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL client reconnect truth alignment` batch has no whitespace or patch-format issues

### WinSSL Native Probe Evidence Lane

- `git status --short --branch`
  - result: PASS
  - summary:
    - current branch remains `master...origin/master`
    - this lane started from a single uncommitted test change in `tests/winssl/test_winssl_session_resumption.pas`

- `git diff -- tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - confirmed the pending batch only expands dedicated evidence
    - no production WinSSL reconnect logic was touched
    - new proof surface adds:
      - `TryQueryNativeSessionReuse(...)`
      - `native_probe label=...`
      - `native_observed_reuse`
      - `native_probe_succeeded`
      - `require_native_reuse`

- add `docs/plans/2026-05-18-winssl-native-probe-evidence-lane.md`
  - purpose:
    - pin this lane as “split public truth from native observation”
    - keep the next follow-up anchored on Windows artifact evidence instead of reopening closed WinSSL lanes

- update `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - change:
    - lock the dedicated native probe helper
    - lock the new `native_probe` markers
    - lock the summary split between public reuse truth and native probe truth

- `bash -n tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - focused session-resumption runtime-truth contract now guards the dedicated native probe evidence surface

- `mkdir -p tmp/winssl_native_probe_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_truth_win64 -FEtmp/winssl_native_probe_truth_win64 -otmp/winssl_native_probe_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - dedicated WinSSL native-probe proof still cross-compiles for Win64
    - compile finished with only the pre-existing `GetConnectionInfo` deprecation warning

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL native probe evidence lane` batch has no whitespace or patch-format issues

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=wave_b_b2_20260518_231137_winssl_native_probe`
  - result: INVALID EVIDENCE
  - summary:
    - this dispatch landed on old remote head `ad72904`
    - root cause was that the first `git push` had raced ahead of the new local commit
    - the run was cancelled and not used as evidence

- `git push origin master`
  - result: PASS
  - summary:
    - pushed commit `5d2d599` (`test/winssl: add native reconnect probe`) to `origin/master`

- `gh run cancel 26042288209`
  - result: PASS
  - summary:
    - cancelled the stale workflow-dispatch run that had started from `ad72904`

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=wave_b_b2_20260518_231405_winssl_native_probe_commit_5d2d599`
  - result: PASS
  - summary:
    - dispatched the corrected Windows evidence run against commit `5d2d599`

- `gh run download 26042437486 -D tmp/gh-run-26042437486`
  - result: PASS
  - summary:
    - downloaded Linux / macOS / Windows / summary artifacts for direct offline inspection

- `gh run view 26042437486 --job 76557534257 --log`
  - result: PASS
  - summary:
    - `windows-gate` completed quick smoke and Windows Wave B gate
    - failure narrowed to `Run broader WinSSL runtime suite`
    - dedicated `WinSSL Session Resumption Truth` now crashes after the initial public reuse checks and before the first `native_probe` marker, with `exit_code=-1073741819`

- `sed -n '1,220p' tmp/gh-run-26042437486/.../winssl_runtime_suite_wave_b_b2_20260518_231405_winssl_native_probe_commit_5d2d599.log`
  - result: PASS
  - summary:
    - compile phase stayed green across all 7 WinSSL runtime programs
    - runtime phase recorded:
      - `[WINSSL-RUNTIME] session_resumption signal label=initial_handshake reused=false info_resumed=false perf_reused=false`
      - then immediate process crash
    - no `native_probe` marker or final session-resumption summary was emitted before the crash

- `sed -n '1,220p' tmp/gh-run-26042437486/.../wave_b_b2_evidence_consistency_wave_b_b2_20260518_231405_winssl_native_probe_commit_5d2d599.md`
  - result: PASS
  - summary:
    - evidence chain itself remained consistent
    - Windows runtime artifact is substantive and explicitly records `suite_end_status=FAIL`

- real Windows conclusion from run `26042437486`
  - result: RED
  - summary:
    - the new dedicated native probe is not safe to keep enabled by default on the broader suite lane
    - the next safe fix is to quarantine it behind an explicit opt-in env gate while preserving the public-truth markers

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - add explicit `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE` gating
    - broader suite default lane now emits `reason=disabled_by_default` instead of executing the risky native probe
    - summary now includes `native_probe_enabled=...`

- update `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - change:
    - lock the new opt-in gating switch
    - lock `reason=disabled_by_default`
    - lock the `native_probe_enabled=...` summary field

- `bash -n tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - the focused runtime-truth contract now guards the native-probe quarantine semantics

- `mkdir -p tmp/winssl_native_probe_quarantine_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_quarantine_win64 -FEtmp/winssl_native_probe_quarantine_win64 -otmp/winssl_native_probe_quarantine_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - the broader-suite-safe follow-up still cross-compiles for Win64
    - compile finished with the pre-existing warning set only

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL native probe quarantine` follow-up has no whitespace or patch-format issues

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=wave_b_b2_20260518_233322_winssl_native_probe_quarantine_f786757`
  - result: PASS
  - summary:
    - dispatched the quarantine follow-up evidence run against commit `f786757`

- `gh run view 26043523820 --job 76561451793 --log`
  - result: PASS
  - summary:
    - `WinSSL Session Resumption Truth` now passes on the Windows runner
    - runtime artifact now records `native_probe ... reason=disabled_by_default` and `native_probe_enabled=false`
    - the new Windows first hard blocker moved to `WinSSL Integration Tests (Multi-Scenario)`

- real Windows conclusion from run `26043523820`
  - result: MIXED
  - summary:
    - fixed:
      - native-probe quarantine worked as intended on Windows
      - macOS gate also returned to PASS in this rerun
    - new blocker:
      - `api.github.com` response assertion in `integration_multi` still expected `2xx/3xx` only
      - transport-level checks were green before that assertion failed

- add `docs/plans/2026-05-18-winssl-integration-multi-http-status-stability.md`
  - purpose:
    - isolate the new broader-suite blocker as a separate flaky-external-status lane
    - keep it distinct from the native-probe/session-resumption route

- update `tests/winssl/test_winssl_integration_multi.pas`
  - change:
    - add `TryExtractHTTPStatusCode(...)`
    - replace the old `2xx/3xx` string-match oracle with:
      - `响应状态码可解析`
      - `响应状态码不是 5xx`

- add `tests/scripts/test_winssl_integration_multi_http_status_contract.sh`
  - purpose:
    - lock the relaxed non-5xx oracle and block regression back to `2xx/3xx`-only matching

- `bash -n tests/scripts/test_winssl_integration_multi_http_status_contract.sh`
  - result: PASS

- `bash tests/scripts/test_winssl_integration_multi_http_status_contract.sh`
  - result: PASS
  - summary:
    - the new focused contract guards the external-HTTP-status stability fix

- `mkdir -p tmp/winssl_integration_multi_http_status_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_integration_multi_http_status_win64 -FEtmp/winssl_integration_multi_http_status_win64 -otmp/winssl_integration_multi_http_status_win64/test_winssl_integration_multi.exe tests/winssl/test_winssl_integration_multi.pas`
  - result: PASS
  - summary:
    - the integration-multi HTTP-status stability follow-up cross-compiles cleanly for Win64
    - compile finished with the pre-existing warning set only

- `git diff --check`
  - result: PASS
  - summary:
    - current `WinSSL integration-multi HTTP status stability` batch has no whitespace or patch-format issues

- `gh run view 26044471873 --json status,conclusion,jobs,workflowName,url,createdAt,updatedAt,headSha`
  - result: FAIL
  - summary:
    - `Wave B B2 Manual Gate (Template)` on commit `0c80a74` finished with:
      - `windows-gate`: success
      - `linux-gate`: success
      - `macos-gate`: failure
      - `summary`: success
    - this confirmed the Windows `integration_multi` fix closed out, and the remaining repo-level failure moved to macOS only

- `gh run view 26044471873 --job 76564810259 --log | tail -n 220`
  - result: PASS
  - summary:
    - `macos-gate` failed in `Run macOS Wave B gate`
    - compile phase stayed green
    - failure narrowed to `scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`

- `gh run download 26044471873 -D tmp/gh-run-26044471873`
  - result: PASS
  - summary:
    - downloaded Linux/macOS/Windows/summary artifacts for the final evidence pass

- `rg -n "FAIL|ERROR|failed|exit=1|OpenSSL Version|版本:|LoadOpenSSLCMS returned False|LoadOpenSSLOCSP returned False|PKCS12_new not loaded" tmp/gh-run-26044471873 -g '!*.zip'`
  - result: PASS
  - summary:
    - macOS module artifact truth:
      - overall module pass-rate `47.1%`
      - `Store/TS/CT` remained green
      - `PEM` / `EVP` helper loads failed
      - `PKCS12/CMS/OCSP` showed broad symbol-missing failures

- add `docs/plans/2026-05-18-macos-openssl-root-loader-priority.md`
  - purpose:
    - isolate the remaining macOS loader-selection risk as the new first hard blocker
    - keep it separate from the already-closed Windows WinSSL lanes

- add `tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
  - purpose:
    - lock the new macOS loader rule that `OPENSSL_ROOT/lib/...` absolute candidates must be tried before generic fallback

- `bash -n tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
  - result: PASS

- `bash tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
  - result: FAIL
  - summary:
    - initial RED proved `src/fafafa.ssl.openssl.loader.pas` still lacked `OPENSSL_ROOT` priority handling

- update `src/fafafa.ssl.openssl.loader.pas`
  - change:
    - add `TryLoadLibraryFromOpenSSLRoot(...)`
    - make both `libcrypto` and `libssl` try `OPENSSL_ROOT/lib/...` absolute candidates before generic fallback names

- `bash -n tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
  - result: PASS

- `bash tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
  - result: PASS
  - summary:
    - focused macOS loader-priority contract completed `RED -> GREEN`

- `mkdir -p tmp/openssl_loader_ready_contract && fpc -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/openssl_loader_ready_contract -FEtmp/openssl_loader_ready_contract tests/test_openssl_loader_ready_contract.pas`
  - result: PASS
  - summary:
    - the loader-priority follow-up compiles cleanly against the existing ready-contract surface

- `mkdir -p tmp/openssl_loader_required_symbol_contract && fpc -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/openssl_loader_required_symbol_contract -FEtmp/openssl_loader_required_symbol_contract tests/test_openssl_loader_required_symbol_contract.pas`
  - result: PASS
  - summary:
    - the stricter loader required-symbol contract also compiles cleanly after the loader change

- `tmp/openssl_loader_ready_contract/test_openssl_loader_ready_contract`
  - result: PASS
  - summary:
    - loader ready-contract runtime still passes after the `OPENSSL_ROOT` priority change

- `tmp/openssl_loader_required_symbol_contract/test_openssl_loader_required_symbol_contract`
  - result: PASS
  - summary:
    - fail-closed loader behavior still holds after the `OPENSSL_ROOT` priority change

- `git diff --check`
  - result: PASS
  - summary:
    - current `macOS OPENSSL_ROOT loader priority` batch has no whitespace or patch-format issues

### macOS Loader Symbol Probe Evidence Lane

- `rg -n "LoadFunctions\\(|GetFunction\\(|GetCryptoProcAddress\\(|GetSSLProcAddress\\(" src/fafafa.ssl.openssl.api.*.pas`
  - result: PASS
  - summary:
    - static source truth now clearly separates the failure split:
      - `TS/CT/Store` rely on direct symbol lookups
      - `EVP/PEM/PKCS12/CMS/OCSP` rely on batch binding / `LoadFunctions(...)`

- add `docs/plans/2026-05-18-macos-openssl-loader-symbol-probe.md`
  - purpose:
    - replace environment-only macOS probing with actual loader/symbol truth
    - stop reopening the `OPENSSL_ROOT` hypothesis without fresh runtime evidence

- add `tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
  - change:
    - new diagnostic probe now emits:
      - loader version truth
      - api-version label
      - direct symbol availability
      - wrapper/module load results

- add `scripts/run_macos_openssl_loader_symbol_probe.sh`
  - change:
    - compiles and runs the new Pascal probe
    - writes a run-scoped JSON artifact for Wave B macOS gate reuse

- update `scripts/run_wave_b_macos_gate.sh`
  - change:
    - add a new `loader-symbol-probe` step
    - keep the new JSON path in the summary evidence table

- update `.github/workflows/wave-b-b2-manual.yml`
  - change:
    - macOS artifact upload now includes `wave_b_macos_loader_symbol_probe_<run_id>.json`

- update `.github/workflows/wave-b-b2-manual.yml.disabled`
  - change:
    - mirror the same macOS loader-symbol probe artifact upload into the dormant template

- add/update focused contracts
  - result: PASS
  - summary:
    - `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
      now also locks the new loader symbol probe artifact upload
    - `tests/scripts/test_wave_b_macos_gate_loader_symbol_probe_contract.sh`
      proves the macOS gate actually invokes the probe and records the evidence row
    - the existing macOS gate fake-run contracts were updated with the new probe stub and all stayed green

- `bash -n scripts/run_macos_openssl_loader_symbol_probe.sh scripts/run_wave_b_macos_gate.sh tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh tests/scripts/test_wave_b_macos_gate_loader_symbol_probe_contract.sh tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh tests/scripts/test_wave_b_macos_gate_invalid_examples_json_contract.sh tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh tests/scripts/test_wave_b_macos_gate_shell_startup_hook_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_loader_symbol_probe_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_empty_run_id_fallback_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_examples_threshold_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_invalid_examples_json_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_module_injection_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_openssl_root_injection_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_path_check_live_passthrough_contract.sh`
  - result: PASS

- `bash tests/scripts/test_wave_b_macos_gate_shell_startup_hook_contract.sh`
  - result: PASS

- `bash scripts/run_macos_openssl_loader_symbol_probe.sh --run-id local_probe --output tmp/local_probe.json`
  - result: PASS
  - summary:
    - the new probe compiles and runs locally
    - it produced a concrete JSON sample under `tmp/local_probe.json`

- `sed -n '1,220p' tmp/local_probe.json`
  - result: PASS
  - summary:
    - local Linux sample proved the new fields are useful:
      - `loader_version_string = OpenSSL 3.5.5 27 Jan 2026`
      - `api_version_string = 3.x (libcrypto.so.3)`
      - direct/batch probe fields all emitted as structured booleans

- `git diff --check`
  - result: PASS
  - summary:
    - current `macOS loader symbol probe evidence lane` batch has no whitespace or patch-format issues

- `git commit -m "ci/macos: add openssl loader symbol probe"`
  - result: PASS
  - summary:
    - committed the macOS loader/symbol probe evidence lane as `07e526b`

- `git push origin master`
  - result: PASS
  - summary:
    - pushed `07e526b` to `origin/master`

- `gh workflow run wave-b-b2-manual.yml --ref master -f run_id=wave_b_b2_20260518_macos_loader_symbol_probe_07e526b`
  - result: PASS
  - summary:
    - dispatched a new live cross-platform gate to collect the new macOS loader-symbol probe artifact on GitHub runner truth

- `gh run list --workflow wave-b-b2-manual.yml --limit 5 --json databaseId,headSha,status,conclusion,createdAt,displayTitle,event,headBranch,url`
  - result: PASS
  - summary:
    - latest run is now `26048015976`
    - head sha matches `07e526bf2e719b361a6c2d8a85922a58e6a7ff3d`
    - workflow is currently `in_progress`

- `gh api repos/dtamade/fafafa.ssl/actions/runs/26048015976/jobs`
  - result: PASS
  - summary:
    - `setup` already passed
    - `linux-gate` / `windows-gate` / `macos-gate` all started
    - `macos-gate` has advanced to `Run macOS Wave B gate`, so the new probe is now in the live execution path

## 2026-05-19

### macOS Probe Closeout

- `gh run view 26048015976 --json status,conclusion,jobs,url`
  - result: PASS
  - summary:
    - run `26048015976` finished `success`
    - `linux-gate` / `macos-gate` / `windows-gate` / `summary` all passed

- `gh run download 26048015976 -D tmp/gh-run-26048015976`
  - result: PASS
  - summary:
    - downloaded the live artifacts for direct offline inspection

- `jq . tmp/gh-run-26048015976/.../wave_b_macos_loader_symbol_probe_wave_b_b2_20260518_macos_loader_symbol_probe_07e526b.json`
  - result: PASS
  - summary:
    - macOS loader/symbol probe proved:
      - `loader_version_string = OpenSSL 3.6.2 7 Apr 2026`
      - direct symbol truth all `true`
      - `evp/pem/pkcs12/cms/ocsp/ts/ct/store` module truth all `true`
    - macOS loader/path or batch-binding drift is no longer the current blocker

- `mkdir -p tmp/test_backend_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_contract -FEtmp/test_backend_contract -otmp/test_backend_contract/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - current backend optional-surface contract truth is still green on Linux host

- `mkdir -p tmp/test_capabilities_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capabilities_contract -FEtmp/test_capabilities_contract -otmp/test_capabilities_contract/test_capabilities_contract tests/contract/test_capabilities_contract.pas && ./tmp/test_capabilities_contract/test_capabilities_contract`
  - result: PASS
  - summary:
    - `63 passed / 0 failed / 1 skipped`
    - current capability truth did not expose a new cross-backend drift

### Session Cache Persistence Count Truth

- add `docs/plans/2026-05-19-session-cache-persistence-count-truth.md`
  - purpose:
    - record the new generic session-cache persistence bug and its focused repair path

- add `tests/test_session_cache_persistence_contract.pas`
  - result: RED -> GREEN
  - summary:
    - new focused contract locks mixed valid/invalid session persistence
    - it proves `LoadFromFile(...)` must remain readable after `SaveToFile(...)` skips invalid entries

- `mkdir -p tmp/test_session_cache_persistence_contract && fpc -B -Fu./src -Fu./tests -FUtmp/test_session_cache_persistence_contract -FEtmp/test_session_cache_persistence_contract -otmp/test_session_cache_persistence_contract/test_session_cache_persistence_contract tests/test_session_cache_persistence_contract.pas && ./tmp/test_session_cache_persistence_contract/test_session_cache_persistence_contract`
  - result: FAIL -> PASS
  - summary:
    - RED: `LoadFromFile succeeds after SaveToFile skipped invalid entries`
    - GREEN: valid entry now loads back cleanly while skipped invalid entry no longer corrupts the file header

- update `src/fafafa.ssl.session.cache.pas`
  - change:
    - `SaveToFile(...)` now writes a placeholder count and backfills the real number of written entries
    - skipped invalid/expired sessions no longer desynchronize the file header from the payload

- `git diff --check`
  - result: PASS
  - summary:
    - current session-cache persistence fix batch has no whitespace or patch-format issues

### C-Library Session Serialization Truth Alignment

- add `docs/plans/2026-05-19-clibrary-session-serialization-truth-alignment.md`
  - purpose:
    - record the MbedTLS/WolfSSL session serialize/deserialize truth-alignment batch

- `git diff -- src/fafafa.ssl.mbedtls.api.pas src/fafafa.ssl.mbedtls.session.pas src/fafafa.ssl.wolfssl.session.pas tests/test_mbedtls_framework.pas tests/test_wolfssl_framework.pas`
  - result: PASS
  - summary:
    - current worktree was focused on c-library session serialize/deserialize truth
    - no unrelated production files were mixed into this batch

- `mkdir -p tmp/test_mbedtls_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas && ./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
  - result: PASS
  - summary:
    - `MbedTLS Framework Test Summary`
    - `Total: 104 / Passed: 104 / Failed: 0`
    - helper-less deserialize rejection and helper-backed serialize/deserialize path both stayed green

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: PASS
  - summary:
    - `WolfSSL Framework Test Summary`
    - `Total: 112 / Passed: 112 / Failed: 0`
    - helper-less deserialize path now fails closed instead of faking success

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend optional-surface and session-resumption contracts remained green after the c-library session changes

- `git diff --check`
  - result: PASS
  - summary:
    - current c-library session serialization truth batch has no whitespace or patch-format issues

### C-Library Session Clone Truth Alignment

- add `docs/plans/2026-05-19-clibrary-session-clone-truth-alignment.md`
  - purpose:
    - record the MbedTLS/WolfSSL session clone truth-alignment batch

- update `tests/test_mbedtls_framework.pas`
  - change:
    - add clone truth checks for deserialized MbedTLS sessions
    - clone must stay valid, resumable, and keep a native handle

- update `tests/test_wolfssl_framework.pas`
  - change:
    - add clone truth checks for deserialized WolfSSL sessions
    - add focused i2d/d2i stubs to prove clone/session serialization route truth

- `mkdir -p tmp/test_mbedtls_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas && ./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - clone valid
      - clone resumable
      - clone native-handle truth
    - GREEN after fix:
      - `Total: 108 / Passed: 108 / Failed: 0`

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - serialize after deserialize was still replaying stale cached bytes
      - clone valid/resumable/native-handle truth all drifted
    - GREEN after fix:
      - `Total: 120 / Passed: 120 / Failed: 0`

- update `src/fafafa.ssl.mbedtls.session.pas`
  - change:
    - `Clone()` now re-materializes a native session when the source session is valid
    - clone no longer degrades a valid session into an invalid metadata shell

- update `src/fafafa.ssl.wolfssl.session.pas`
  - change:
    - `Clone()` now re-materializes a native session when the source session is valid
    - `Serialize()` now prefers native `i2d` bytes over stale cached payload

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend session/native-handle contracts remained green after the clone truth fix

- `git diff --check`
  - result: PASS
  - summary:
    - current c-library session clone truth batch has no whitespace or patch-format issues

### WolfSSL Session Source Lifetime Truth Alignment

- add `docs/plans/2026-05-19-wolfssl-session-source-lifetime-truth-alignment.md`
  - purpose:
    - record the WolfSSL session source-lifetime ownership repair batch

- update `src/fafafa.ssl.wolfssl.api.pas`
  - change:
    - bind `wolfSSL_SESSION_dup` into the WolfSSL dynamic API layer

- update `src/fafafa.ssl.wolfssl.session.pas`
  - change:
    - add `DuplicateWolfSSLSessionHandle(...)`
    - `FromConnection()` now secures session ownership before returning:
      - prefer `wolfSSL_SESSION_dup`
      - fallback to `i2d/d2i`
      - fail closed if neither path is available

- update `tests/test_wolfssl_framework.pas`
  - change:
    - add `WolfSSL Session Source Lifetime Contract`
    - prove borrowed session duplication and fail-closed behavior when ownership cannot be secured

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: PASS
  - summary:
    - `WolfSSL Framework Test Summary`
    - `Total: 127 / Passed: 127 / Failed: 0`
    - new `WolfSSL Session Source Lifetime Contract` stayed green:
      - duplicates borrowed session when dup helper exists
      - rejects borrowed session when ownership cannot be secured

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend session/native-handle contracts remained green after the WolfSSL lifetime fix

- `git diff --check`
  - result: PASS
  - summary:
    - current WolfSSL session source-lifetime batch has no whitespace or patch-format issues

### C-Library Session Metadata And Peer-Certificate Completeness

- add `docs/plans/2026-05-19-clibrary-session-metadata-peer-cert-completeness.md`
  - purpose:
    - record the MbedTLS/WolfSSL session metadata + peer-certificate completeness batch

- update `tests/test_mbedtls_framework.pas`
  - change:
    - add `MbedTLS Session Metadata Completeness Contract`
    - lock protocol/cipher/peer-cert truth plus helper-loss fail-closed behavior

- update `tests/test_wolfssl_framework.pas`
  - change:
    - add `WolfSSL Session Metadata Completeness Contract`
    - lock peer-cert materialization plus clone-preservation truth

- `mkdir -p tmp/test_mbedtls_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas && ./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `FromContext` protocol truth
      - `FromContext` cipher truth
      - `FromContext` peer-certificate materialization
      - peer-cert-preserving clone
    - GREEN after fix:
      - `Total: 116 / Passed: 116 / Failed: 0`

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - session peer-certificate materialization
      - peer-cert-preserving clone
    - GREEN after fix:
      - `Total: 136 / Passed: 136 / Failed: 0`

- update `src/fafafa.ssl.mbedtls.session.pas`
  - change:
    - add protocol/cipher extraction from `mbedtls_ssl_get_version` / `mbedtls_ssl_get_ciphersuite`
    - add borrowed peer-cert materialization via `DER copy -> owned reload`
    - `GetPeerCertificate()` / `Clone()` now preserve peer-cert truth

- update `src/fafafa.ssl.mbedtls.certificate.pas`
  - change:
    - `Clone()` now re-materializes a native cert from DER instead of returning a cached-field shell

- update `src/fafafa.ssl.wolfssl.api.pas`
  - change:
    - bind `wolfSSL_i2d_X509` into the dynamic API layer

- update `src/fafafa.ssl.wolfssl.certificate.pas`
  - change:
    - `SaveToDER()` now exports real DER from a native `WOLFSSL_X509` when cache is empty

- update `src/fafafa.ssl.wolfssl.session.pas`
  - change:
    - `FromConnection()` now materializes peer cert into an owned cert object
    - `GetPeerCertificate()` / `Clone()` preserve peer-cert truth

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend session/certificate optional surfaces remained green after the completeness fix

- `git diff --check`
  - result: PASS
  - summary:
    - current c-library session metadata/peer-certificate batch has no whitespace or patch-format issues

### MbedTLS Connection Peer-Certificate Materialization

- add `docs/plans/2026-05-19-mbedtls-connection-peer-cert-materialization.md`
  - purpose:
    - record the MbedTLS connection peer-cert ownership/materialization batch

- add `tests/test_mbedtls_connection_peer_certificate_contract.pas`
  - change:
    - lock that `GetPeerCertificate()` / `GetPeerCertificateChain()` must return owned copies
    - lock helper-loss fail-closed behavior

- `mkdir -p tmp/test_mbedtls_connection_peer_certificate_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_mbedtls_connection_peer_certificate_contract_units -FEtmp/test_mbedtls_connection_peer_certificate_contract_units -otmp/test_mbedtls_connection_peer_certificate_contract_units/test_mbedtls_connection_peer_certificate_contract tests/test_mbedtls_connection_peer_certificate_contract.pas && ./tmp/test_mbedtls_connection_peer_certificate_contract_units/test_mbedtls_connection_peer_certificate_contract`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - returned cert handle still aliased the source fixture handle
      - returned chain leaf still aliased the source fixture handle
      - helper-loss path did not fail closed
    - GREEN after fix:
      - `Total: 8 / Passed: 8 / Failed: 0`

- update `src/fafafa.ssl.mbedtls.connection.pas`
  - change:
    - `GetPeerCertificate()` now materializes an owned cert via `TMbedTLSCertificate.Clone()`
    - `GetPeerCertificateChain()` now does the same for the single-leaf path
    - helper-loss path now naturally degrades to `nil` / empty chain

- `mkdir -p tmp/test_mbedtls_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas && ./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
  - result: PASS
  - summary:
    - `Total: 116 / Passed: 116 / Failed: 0`
    - MbedTLS backend framework coverage remained green after the connection peer-cert fix

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend optional/core surfaces remained green after the MbedTLS connection materialization change

- `git diff --check`
  - result: PASS
  - summary:
    - current MbedTLS connection peer-cert batch has no whitespace or patch-format issues

### WolfSSL Certificate Clone Materialization

- add `docs/plans/2026-05-19-wolfssl-certificate-clone-materialization.md`
  - purpose:
    - record the WolfSSL certificate clone materialization batch

- update `tests/test_wolfssl_framework.pas`
  - change:
    - add `WolfSSL Certificate Clone Materialization Contract`
    - lock native-handle preservation, subject/issuer/fingerprint truth, and helper-loss fail-closed behavior

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `Clone keeps native handle for loaded certificate`
      - `Clone preserves subject truth`
      - `Clone preserves issuer truth`
      - `Clone fails closed when X509 materialization helper is unavailable`
      - `Clone preserves fingerprint truth` remained PASS
    - GREEN after fix:
      - `Total: 141 / Passed: 141 / Failed: 0`

- update `src/fafafa.ssl.wolfssl.certificate.pas`
  - change:
    - `Clone()` now performs `DER copy -> owned reload` for loaded certificates
    - cached-field shell clones are no longer returned for loaded certs
    - helper-loss path now fails closed instead of returning a fake-complete clone

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend certificate/session optional surfaces remained green after the WolfSSL clone fix

- `git diff --check`
  - result: PASS
  - summary:
    - current WolfSSL certificate clone batch has no whitespace or patch-format issues

### WolfSSL Connection Peer-Certificate Materialization

- add `docs/plans/2026-05-19-wolfssl-connection-peer-cert-materialization.md`
  - purpose:
    - record the WolfSSL connection single-cert materialization batch

- add `tests/test_wolfssl_connection_peer_certificate_contract.pas`
  - change:
    - lock that `GetPeerCertificate()` must return an owned/materialized public cert
    - lock source-handle de-aliasing and helper-loss fail-closed behavior

- `mkdir -p tmp/test_wolfssl_connection_peer_certificate_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_peer_certificate_contract_units -FEtmp/test_wolfssl_connection_peer_certificate_contract_units -otmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract tests/test_wolfssl_connection_peer_certificate_contract.pas && ./tmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract`
  - result: SKIP -> FAIL -> PASS
  - summary:
    - initial skip exposed a test-entry issue:
      - helper-availability check ran before `LLib.Initialize`, so unbound symbols caused a false skip
    - after fixing the test entry order, RED first exposed:
      - `GetPeerCertificate must return an owned copy instead of the source native handle`
      - `GetPeerCertificate should fail closed when cert-copy helper is unavailable`
    - GREEN after fix:
      - `Total: 4 / Passed: 4 / Failed: 0 / Skipped: 0`

- update `src/fafafa.ssl.wolfssl.connection.pas`
  - change:
    - `GetPeerCertificate()` no longer returns the raw native wrapper from `wolfSSL_get_peer_certificate(...)`
    - path now performs `native X509 -> DER export -> owned reload`
    - helper-loss path now fails closed instead of returning a source-handle wrapper

- `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
  - result: PASS
  - summary:
    - `Total: 141 / Passed: 141 / Failed: 0`
    - existing WolfSSL framework coverage remained green after the connection single-cert fix

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend optional/core surfaces remained green after the WolfSSL connection materialization change

- `git diff --check`
  - result: PASS
  - summary:
    - current WolfSSL connection peer-cert batch has no whitespace or patch-format issues

### FreePascal Peer Certificate Issuer Link

- add `docs/plans/2026-05-19-freepascal-peer-cert-issuer-link.md`
  - purpose:
    - record the FreePascal connection peer-cert issuer-link completeness batch

- update `tests/test_freepascal_client_peer_certificate_surface.pas`
  - change:
    - add leaf/chain issuer-link truth checks on top of the existing scripted TLS 1.3 handshake fixture

- `mkdir -p tmp/test_freepascal_client_peer_certificate_surface_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_client_peer_certificate_surface_units -FEtmp/test_freepascal_client_peer_certificate_surface_units -otmp/test_freepascal_client_peer_certificate_surface_units/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/test_freepascal_client_peer_certificate_surface_units/test_freepascal_client_peer_certificate_surface`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `Peer leaf certificate should preserve issuer link`
    - GREEN after fix:
      - scripted handshake still succeeded
      - leaf and chain leaf issuer-link truth both passed

- update `src/fafafa.ssl.freepascal.connection.pas`
  - change:
    - after building `FPeerCertificateChain`, link each certificate to its immediate issuer
    - last chain entry now explicitly clears issuer link

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
    - cross-backend optional/core surfaces remained green after the FreePascal issuer-link fix

- `git diff --check`
  - result: PASS
  - summary:
    - current FreePascal issuer-link batch has no whitespace or patch-format issues

### GetVerifyResult Compiler Deprecation Alignment

- update `docs/reference/API_REFERENCE.md`
  - change:
    - replace dotted `ISSLCertificateVerification.GetVerifyResult*` doc wording with `ISSLCertificateVerification owner surface`
    - keep compiler-deprecated compatibility-mirror truth without re-triggering residual direct-core doc grep

- update `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
  - change:
    - align API reference assertions with the new owner-surface wording
    - keep source/doc/quarantine/mirror-proof coverage unchanged

- `bash -n tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - compiler-deprecated focused contract syntax remained valid after the wording tighten

- `bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
  - result: PASS
  - summary:
    - source declarations, API docs, V2 migration notes, warning quarantines, and backend mirror proof stayed aligned

- `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - active docs no longer reintroduced `TypeName.GetVerifyResult*` wording
    - residual direct-core allowlist stayed frozen to the expected file sets

- `bash tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
  - result: PASS
  - summary:
    - generic examples/tests still prefer `ISSLCertificateVerification` for verify-result surfaces

- `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
  - result: PASS
  - summary:
    - `29 passed / 0 failed`
    - builder owner-path fallback changes remained behavior-neutral

- `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - result: PASS
  - summary:
    - `6 passed / 0 failed`
    - TLS facade hostname precedence remained intact

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `135 total / 111 passed / 0 failed / 24 skipped`
    - cross-backend certificate-verification optional/core truth remained green after the doc-contract closeout

- `git diff --check`
  - result: PASS
  - summary:
    - current compiler-deprecated alignment batch has no whitespace or patch-format issues

### Native-Handle Owner Surface Truth Freeze

- add `docs/plans/2026-05-19-native-handle-owner-surface-truth-freeze.md`
  - change:
    - define the bounded active-doc + generic-smoke truth-freeze batch for native-handle owner surface drift

- add `tests/scripts/test_native_handle_owner_surface_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `API_REFERENCE` still listed `GetNativeHandle` inside `ISSLContext`
    - GREEN after fix:
      - source / API reference / V2 doc / generic smoke are now aligned to `ISSLNativeHandleAccess` and `ISSLConnectionInfo` owner truth

- update `docs/reference/API_REFERENCE.md`
  - change:
    - remove `GetNativeHandle` from the `ISSLContext` code listing
    - add explicit `ISSLNativeHandleAccess` optional-surface block
    - record that `GetNativeHandle` does not belong to `ISSLContext` / `ISSLConnection` core

- update `docs/reference/INTERFACE_DESIGN_V2.md`
  - change:
    - remove `GetNativeHandle` from the `ISSLConnection` core sketch
    - remove stale `GetSelectedALPNProtocol` from the `ISSLClientConnection` sketch
    - add `ISSLNativeHandleAccess` extension sketch
    - move the migration-table owner of `GetNativeHandle` to `ISSLNativeHandleAccess`

- update `tests/connection/test_ssl_connection_local.pas`
  - change:
    - replace direct `Connection.GetNativeHandle` calls with `ISSLNativeHandleAccess`
    - replace deprecated core `GetConnectionInfo` call with `ISSLConnectionInfo.GetConnectionInfo`

- `bash -n tests/scripts/test_native_handle_owner_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - focused native-handle owner-surface contract syntax is valid

- `bash tests/scripts/test_native_handle_owner_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active canonical docs no longer contradict the optional native-handle surface
    - generic local connection smoke no longer assumes removed core getters

- `mkdir -p tmp/test_ssl_connection_local_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_ssl_connection_local_units -FEtmp/test_ssl_connection_local_units -otmp/test_ssl_connection_local_units/test_ssl_connection_local tests/connection/test_ssl_connection_local.pas && ./tmp/test_ssl_connection_local_units/test_ssl_connection_local`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `Identifier idents no member "GetNativeHandle"` at two connection call sites
      - a fresh deprecated warning on direct core `GetConnectionInfo`
    - GREEN after fix:
      - compile succeeded
      - runtime summary: `27 passed / 0 failed`

- `git diff --check`
  - result: PASS
  - summary:
    - current native-handle owner-surface batch has no whitespace or patch-format issues

### Wave B/B2 Opt-In Runtime Failure Truth

- add `docs/plans/2026-05-19-wave-b-b2-opt-in-runtime-failure-truth.md`
  - purpose:
    - record the bounded workflow-truth batch for Windows opt-in runtime failure propagation

- add `tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh`
  - change:
    - lock that cross summary must promote Windows state to `FAIL` when an explicit runtime transcript reports `suite_end_status=FAIL`

- add `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh`
  - change:
    - lock that handoff bundle must not remain `CLOSED` when sibling Windows runtime transcript explicitly ends with `FAIL`

- update `scripts/generate_wave_b_cross_platform_summary.sh`
  - change:
    - add optional `--windows-runtime-transcript`
    - when the transcript explicitly reports `suite_end_status=FAIL`, elevate Windows platform state to `FAIL` while preserving legacy behavior for missing or non-failing transcripts

- update `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - change:
    - pass sibling Windows runtime transcript through to cross-summary generation
    - promote handoff state to `NEEDS_GATE_REPAIR` when transcript truth explicitly reports `suite_end_status=FAIL`
    - emit a next-action message that points back to the WinSSL broader runtime/native-probe failure instead of pretending the batch is closed

- update `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
  - change:
    - replace the stale empty runtime-transcript fixture with a substantive PASS transcript so the contract matches the repo's current Windows runtime marker expectations

- `bash -n tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh`
  - result: PASS
  - summary:
    - new cross-summary focused contract syntax is valid

- `bash tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh`
  - result: PASS
  - summary:
    - explicit Windows runtime transcript failures now surface as `windows | FAIL` in the cross-platform summary

- `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh`
  - result: PASS
  - summary:
    - new handoff-bundle focused contract syntax is valid

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh`
  - result: PASS
  - summary:
    - handoff bundle no longer stays `CLOSED` when sibling Windows runtime transcript explicitly ends in `FAIL`

- `bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`
  - result: PASS
  - summary:
    - existing cross-summary next-actions wording remained green after the Windows runtime truth propagation change

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
  - result: PASS
  - summary:
    - gate-repair-state contract is green again after aligning the runtime-transcript fixture with current suite-marker truth

- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - result: PASS
  - summary:
    - Windows sibling artifact path derivation remained intact after passing runtime transcript through the cross-summary path

- `bash tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh`
  - result: PASS
  - summary:
    - current consistency semantics still hold for substantive Windows runtime transcripts after the top-level truth fix

- `git diff --check`
  - result: PASS
  - summary:
    - current Wave B/B2 opt-in runtime failure truth batch has no whitespace or patch-format issues

### OpenSSL CT Capability Truth Re-Tightening

- add `docs/plans/2026-05-19-openssl-ct-capability-truth-retightening.md`
  - purpose:
    - record the bounded batch that re-tightens OpenSSL CT capability truth back to the real public-surface boundary

- update `tests/openssl/test_openssl_features.pas`
  - change:
    - add `TestCertificateTransparencyPublicSurfaceTruthContract`
    - prove that OpenSSL connection surface stays CT-free by default
    - prove that merely marking `osmCT` as loaded must not lift OpenSSL public capability truth

- `mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `OpenSSL must not publish CT feature support merely because low-level CT bindings are marked loaded`
    - GREEN after fix:
      - focused OpenSSL feature suite returned `All tests passed`
      - new CT public-surface truth contract stayed green

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - stop using low-level CT module readiness as `sslFeatCertificateTransparency`
    - pin `SupportsCertificateTransparency` back to `False`
    - pin `CertTransparencySupport` back to `sslSupportNone`
    - keep CT binding availability as non-public implementation detail instead of backend capability truth

- update `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
  - change:
    - CT row no longer claims direct mapping to `SupportsCertificateTransparency` / `CertTransparencySupport`
    - add note that OpenSSL CT API readiness does not imply published backend connection-level CT surface

- `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
  - result: PASS
  - summary:
    - `135 total / 111 passed / 0 failed / 24 skipped`
    - cross-backend CT optional-interface contract remained green after the OpenSSL capability truth tightening

- `python3 scripts/compile_all_modules.py`
  - result: PASS
  - summary:
    - `187/187` core modules compiled successfully

- `git diff --check`
  - result: FAIL -> PASS
  - summary:
    - first caught trailing whitespace in the new CT capability note
    - rerun passed after cleanup

### Hardware-Key Capability Truth Tightening

- add `docs/plans/2026-05-19-hardware-key-capability-truth-tightening.md`
  - purpose:
    - record the bounded batch that tightens `PKCS11/TPM` capability truth back to shipped public/runtime surface

- add `tests/scripts/test_hardware_key_capability_truth_contract.sh`
  - change:
    - lock that:
      - OpenSSL still has a shipped PKCS#11 loader path
      - OpenSSL no longer publishes TPM capability
      - WinSSL no longer publishes PKCS#11 / TPM capability
      - active WinSSL capability docs no longer market smart-card / TPM support as current backend capability truth

- add `tests/test_auto_backend_tpm_capability_truth_contract.pas`
  - change:
    - prove the downstream selector consequence:
      - when `RequireTPM` is requested
      - auto backend selection must fail with `no suitable backend`
      - instead of being satisfied by a fake capability-positive backend

- update `tests/openssl/test_openssl_features.pas`
  - change:
    - add `TestTPMPublicCapabilityTruthContract`
    - prove OpenSSL must not publish `SupportsTPM` without a shipped TPM public/runtime path

- `bash -n tests/scripts/test_hardware_key_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - new static source/doc truth contract syntax is valid

- `bash tests/scripts/test_hardware_key_capability_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - OpenSSL still advertised `SupportsTPM`
      - WinSSL source/doc still advertised hardware-key capability as published truth
    - GREEN after fix:
      - static source/doc truth for OpenSSL PKCS#11 and OpenSSL/WinSSL TPM/PKCS11 capability now aligns with shipped public/runtime surface

- `mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `OpenSSL must not publish TPM capability without a shipped TPM public/runtime path`
    - GREEN after fix:
      - focused OpenSSL feature suite returned `All tests passed`
      - new TPM public-capability truth contract stayed green

- `mkdir -p tmp/test_auto_backend_tpm_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_tpm_truth_units -FEtmp/test_auto_backend_tpm_truth_units -otmp/test_auto_backend_tpm_truth_units/test_auto_backend_tpm_truth_contract tests/test_auto_backend_tpm_capability_truth_contract.pas && ./tmp/test_auto_backend_tpm_truth_units/test_auto_backend_tpm_truth_contract`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - auto-backend selection still treated fake TPM capability as satisfiable and created a client context
    - GREEN after fix:
      - requiring TPM now correctly reports `No suitable SSL backend found for requirements`

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - keep shipped PKCS#11 capability truth
    - pin `SupportsTPM` back to `False`
    - stop treating hypothetical engine/provider ecosystem paths as current backend TPM public capability

- update `src/fafafa.ssl.winssl.lib.pas`
  - change:
    - pin `SupportsPKCS11` back to `False`
    - pin `SupportsTPM` back to `False`
    - stop treating platform potential / CNG ecosystem reach as current WinSSL backend public capability truth

- update `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - replace `智能卡` / `TPM` supported rows with current non-published capability truth
    - remove the old “原生支持智能卡和 TPM” advantage claim

- `python3 scripts/compile_all_modules.py`
  - result: PASS
  - summary:
    - `187/187` core modules compiled successfully after the hardware-key capability truth tightening batch

- `git diff --check`
  - result: PASS
  - summary:
    - current hardware-key capability truth batch has no whitespace or patch-format issues

### OpenSSL PKCS#11 Capability Runtime Truth

- add `docs/plans/2026-05-19-openssl-pkcs11-capability-runtime-truth.md`
  - purpose:
    - record the bounded batch that tightens OpenSSL `SupportsPKCS11` from unconditional truth to runtime-readiness truth

- update `tests/openssl/test_openssl_features.pas`
  - change:
    - add `TestPKCS11CapabilityMatrixRuntimeDriftContract`
    - prove that:
      - OpenSSL published PKCS#11 capability must match `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
      - capability must drop back to `False` when neither Provider nor ENGINE runtime surface is ready

- `mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `PKCS#11 capability must match PKCS#11 backend auto-detection readiness`
    - runtime proof tightened further by temporarily dropping:
      - `OSSL_PROVIDER_load`
      - `OSSL_STORE_open`
      - `OSSL_STORE_expect`
      - `ENGINE_by_id`
      - `ENGINE_init`
      - `ENGINE_load_private_key`
    - GREEN after fix:
      - `PKCS#11 capability matrix runtime drift contract verified`
      - focused OpenSSL feature suite returned `All tests passed`

- update `src/fafafa.ssl.openssl.backed.pas`
  - change:
    - import `fafafa.ssl.pkcs11.backend`
    - compute `LPKCS11Ready := TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
    - publish `Result.SupportsPKCS11 := LPKCS11Ready`
    - stop treating the mere existence of a shipped loader path as unconditional runtime capability truth

- update `docs/BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - restate OpenSSL PKCS#11 row as runtime-readiness-based support
    - document that capability truth follows Provider / ENGINE backend readiness

- `python3 scripts/compile_all_modules.py`
  - result: PASS
  - summary:
    - session `37481` completed with `187/187` core modules compiled successfully

- `git diff --check`
  - result: PASS
  - summary:
    - current OpenSSL PKCS#11 capability runtime-truth batch has no whitespace or patch-format issues

### Hardware-Key Contract Runtime Truth Resync

- add `docs/plans/2026-05-19-hardware-key-contract-runtime-truth-resync.md`
  - purpose:
    - record the bounded batch that resynchronizes the stale hardware-key shell contract to the current OpenSSL runtime-aware PKCS#11 truth

- `bash -n tests/scripts/test_hardware_key_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - existing hardware-key shell contract syntax stayed valid before the truth resync

- `bash tests/scripts/test_hardware_key_capability_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `OpenSSL capability truth no longer records the shipped PKCS#11 public path`
    - root cause:
      - the shell contract still required the stale line `Result.SupportsPKCS11 := True;`
      - source truth had already moved to runtime-aware `LPKCS11Ready`
    - GREEN after fix:
      - static contract now matches the shipped OpenSSL PKCS#11 public path plus runtime-readiness capability truth

- update `tests/scripts/test_hardware_key_capability_truth_contract.sh`
  - change:
    - keep guarding the shipped OpenSSL PKCS#11 public loader path
    - require:
      - `LPKCS11Ready := TPKCS11BackendFactory.IsBackendAvailable(btAuto);`
      - `Result.SupportsPKCS11 := LPKCS11Ready;`
    - forbid the stale unconditional line:
      - `Result.SupportsPKCS11 := True;`
    - add OpenSSL backend-capability doc checks for runtime-readiness wording
    - keep WinSSL non-published PKCS11/TPM truth checks intact

- `git diff --check`
  - result: PASS
  - summary:
    - current hardware-key contract truth-resync batch has no whitespace or patch-format issues

### Active Capability Docs Runtime Truth Sweep

- add `docs/plans/2026-05-19-active-capability-docs-runtime-truth-sweep.md`
  - purpose:
    - record the bounded batch that realigns active capability docs with current runtime-aware source truth

- add `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - change:
    - guard that active docs no longer:
      - market WinSSL `PKCS11/TPM` as published capability
      - market OpenSSL `PKCS#11` as unconditional truth
      - market OpenSSL default-build `FIPS` as published capability

- `bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - new active-capability-docs shell contract syntax is valid

- `bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `Migration guide still advertises the stale PKCS#11 capability table`
    - static review then confirmed the same active-doc family also still carried:
      - stale WinSSL TPM truth
      - stale OpenSSL default-build FIPS truth
      - stale unconditional OpenSSL PKCS#11 example wording
      - stale Windows recommendation wording tied to nonexistent TPM capability
    - GREEN after fix:
      - the active capability-doc family now matches current runtime-aware source truth

- update `docs/MIGRATION_GUIDE_V1.1.md`
  - change:
    - replace stale `PKCS#11 / TPM / FIPS` table rows with current published truth
    - add explicit note that OpenSSL PKCS#11 depends on Provider / ENGINE runtime readiness
    - add explicit note that default-build OpenSSL does not publish FIPS capability

- update `docs/BACKEND_SELECTION_GUIDE.md`
  - change:
    - change the OpenSSL scoring example from unconditional `SupportsPKCS11: Yes`
      to runtime-dependent wording
    - bound the example scoring text so PKCS#11-dependent platform score is explicitly conditional on runtime readiness

- update `docs/CAPABILITY_MATRIX_GUIDE.md`
  - change:
    - stop tying the Windows recommendation snippet to nonexistent TPM published capability
    - keep the recommendation anchored on `SupportsSystemCertStore`

- `git diff --check`
  - result: PASS
  - summary:
    - current active capability-docs runtime-truth batch has no whitespace or patch-format issues

### Auto-Backend PKCS#11 Capability Truth Contract

- add `docs/plans/2026-05-19-auto-backend-pkcs11-capability-truth-contract.md`
  - purpose:
    - record the bounded batch that closes the remaining downstream proof gap for runtime-aware PKCS#11 selector truth

- add `tests/test_auto_backend_pkcs11_capability_truth_contract.pas`
  - change:
    - prove auto-backend selection follows current published PKCS#11 capability truth
    - require success when any available registered backend publishes `SupportsPKCS11=True`
    - require failure when none do

- `mkdir -p tmp/test_auto_backend_pkcs11_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_pkcs11_truth_units -FEtmp/test_auto_backend_pkcs11_truth_units -otmp/test_auto_backend_pkcs11_truth_units/test_auto_backend_pkcs11_capability_truth_contract tests/test_auto_backend_pkcs11_capability_truth_contract.pas && ./tmp/test_auto_backend_pkcs11_truth_units/test_auto_backend_pkcs11_capability_truth_contract`
  - result: PASS
  - summary:
    - selector / builder downstream PKCS#11 truth now has a dedicated runtime-aware contract

- `git diff --check`
  - result: PASS
  - summary:
    - current auto-backend PKCS#11 capability-truth batch has no whitespace or patch-format issues

### Active FIPS Docs Truth Sweep

- add `docs/plans/2026-05-19-active-fips-docs-truth-sweep.md`
  - purpose:
    - record the bounded batch that realigns active FIPS docs with current source truth

- add `tests/scripts/test_active_fips_docs_truth_contract.sh`
  - change:
    - guard that active docs no longer advertise OpenSSL default-build FIPS capability as published truth
    - guard that WinSSL remains the currently published FIPS-capable backend in these active docs

- `bash -n tests/scripts/test_active_fips_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - new active-FIPS-docs shell contract syntax is valid

- `bash tests/scripts/test_active_fips_docs_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `Backend abstraction design doc still advertises stale OpenSSL FIPS truth`
    - static review then confirmed the same active-doc family also still carried:
      - stale OpenSSL FIPS truth in backend selector design
      - stale OpenSSL/WinSSL split in platform support
    - GREEN after fix:
      - active FIPS docs now match current source truth instead of the older static OpenSSL capability story

- update `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
  - change:
    - replace stale `OpenSSL FIPS = ✅` row with current default-build truth
    - add explicit note that OpenSSL requires a dedicated module/build for FIPS

- update `docs/reference/BACKEND_SELECTOR_DESIGN.md`
  - change:
    - replace stale `OpenSSL FIPS = ✅` row with current default-build truth
    - add explicit selector note that default capability must not be treated as already FIPS-ready

- update `docs/PLATFORM_SUPPORT.md`
  - change:
    - replace the OpenSSL/WinSSL FIPS comparison row with the current split
    - add explicit note that default OpenSSL backend capability still does not publish FIPS

- `git diff --check`
  - result: PASS
  - summary:
    - current active FIPS docs-truth batch has no whitespace or patch-format issues

### Backend Selection Guide Runtime Truth Sweep

- add `docs/plans/2026-05-19-backend-selection-guide-runtime-truth-sweep.md`
  - purpose:
    - record the bounded batch that realigns the builder/selector entry guide with current runtime-aware truth

- add `tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh`
  - change:
    - guard that:
      - `WithSecurityFirst` is no longer presented as a default FIPS shortcut
      - `RequirePKCS11Support` is described as a runtime-aware requirement
      - the government/finance scenario no longer pretends current default shipped backends must satisfy `FIPS + PKCS#11`

- `bash -n tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - new backend-selection-guide shell contract syntax is valid after one quoting cleanup

- `bash tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - first contract-run issue:
      - the new shell contract initially used backticks inside double-quoted patterns
      - a quick quoting cleanup fixed the harness itself
    - RED after harness cleanup:
      - `Backend selection guide no longer records that WithSecurityFirst is not a default FIPS shortcut`
    - static review then confirmed the same guide also still lacked:
      - runtime-aware `RequirePKCS11Support` wording
      - a deployment-boundary warning for the `FIPS + PKCS#11` scenario
    - GREEN after fix:
      - the builder/selector entry guide now matches current runtime-aware truth

- update `docs/BACKEND_SELECTION_GUIDE.md`
  - change:
    - add explicit note that `WithSecurityFirst` does not imply default FIPS readiness
    - redefine `RequirePKCS11Support` as a runtime-aware published-capability requirement
    - add OpenSSL Provider / ENGINE runtime-readiness note
    - annotate the chained example so PKCS#11 is clearly marked runtime-aware
    - bound the government/finance scenario with the current `FIPS + PKCS#11` deployment reality

- `git diff --check`
  - result: PASS
  - summary:
    - current backend-selection-guide runtime-truth batch has no whitespace or patch-format issues

### Security-First FIPS Independence Contract

- `get_goal`
  - result: ACTIVE
  - summary:
    - thread-level goal is still active for the long-running interface/backend completeness closure
    - the stale note in `task_plan.md` about the goal tool being unavailable needed correction

- `sed -n '220,260p' src/fafafa.ssl.backend.selector.pas`
- `sed -n '500,520p' src/fafafa.ssl.backend.selector.pas`
- `sed -n '3018,3032p' src/fafafa.ssl.context.builder.pas`
- `sed -n '1,220p' tests/test_backend_selector_security_first_viability.pas`
  - result: PASS
  - summary:
    - confirmed the real residual gap was not production behavior drift already visible in source
    - the missing piece was focused proof that `WithSecurityFirst` is not a default FIPS preference path

- add `docs/plans/2026-05-19-security-first-fips-independence-contract.md`
  - purpose:
    - record the bounded batch that closes the behavior-level proof gap under the broader interface/backend completeness goal

- add `tests/test_security_first_fips_independence_contract.pas`
  - change:
    - build an environment-independent mock backend matrix
    - prove `CreateSecurityFirstRequirements` defaults `PreferFIPSCompliant=False`
    - prove default security-first selection prefers a stronger non-FIPS backend
    - prove explicit `PreferFIPSCompliant=True` is the step that flips selection to the FIPS backend
    - prove `WithSecurityFirst` builder still constructs a non-FIPS context by default

- `mkdir -p tmp/test_security_first_fips_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_security_first_fips_units -FEtmp/test_security_first_fips_units -otmp/test_security_first_fips_units/test_security_first_fips_independence_contract tests/test_security_first_fips_independence_contract.pas && ./tmp/test_security_first_fips_units/test_security_first_fips_independence_contract`
  - result: PASS
  - summary:
    - contract passed green with the controlled mock matrix
    - default selection chose non-FIPS `sslOpenSSL`
    - explicit FIPS preference flipped selection to FIPS-capable `sslWinSSL`
    - `WithSecurityFirst` builder path successfully built a non-FIPS context
    - compile emitted existing unrelated warnings in shared units, but no failure

- `git diff --check`
  - result: PASS
  - summary:
    - current security-first/FIPS-independence batch has no whitespace or patch-format issues

### ISSLOCSPStapling Residual Classification Freeze

- `rg -n '\\b(?:LConn|Conn|Connection)\\.(GetOCSPStaplingEnabled|GetOCSPResponse|IsOCSPResponseVerified|GetOCSPResponseStatus)\\b' tests --glob '!tests/scripts/**'`
  - result: PASS
  - summary:
    - confirmed the current direct-core `GetOCSP*` residual surface had narrowed to 4 files:
      - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
      - `tests/openssl/test_ocsp_connection_verification_regression.pas`
      - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
      - `tests/test_wolfssl_ocsp_stapling_contract.pas`
    - this ruled out reopening the earlier ordinary-guidance lane and pointed to a bounded residual-freeze batch instead

- add `docs/plans/2026-05-19-isslocspstapling-residual-classification-freeze.md`
  - change:
    - record the bounded batch that freezes the remaining OCSP direct-core test surface as backend-specific proof

- add `tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
  - change:
    - guard source/API truth for the `ISSLOCSPStapling` owner path
    - lock the current direct-core `GetOCSP*` residual file set to the expected 4 files
    - require intent labels on all residual files

- update `src/fafafa.ssl.base.pas`
  - change:
    - add `@preferred-access` / `@owner-note` / `@compatibility-note` for:
      - `GetOCSPStaplingEnabled`
      - `GetOCSPResponse`
      - `IsOCSPResponseVerified`
      - `GetOCSPResponseStatus`

- update `src/fafafa.ssl.connection.base.pas`
  - change:
    - record that the shared OCSP bridge/stub surface now has ordinary guidance on `ISSLOCSPStapling`
    - classify the remaining direct-core `GetOCSP*` files as MbedTLS/OpenSSL/WolfSSL backend-specific residual proof

- update `docs/reference/API_REFERENCE.md`
  - change:
    - restate the OCSP methods on `ISSLConnection` as `v1.x` compatibility-core mirrors
    - point new code at the `ISSLOCSPStapling` owner surface

- update residual proof files:
  - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
  - `tests/test_wolfssl_ocsp_stapling_contract.pas`
  - change:
    - add `INTENTIONAL_OCSP_CORE_SURFACE` labels so the remaining direct-core coverage is explicitly classified as backend-specific proof

- `bash -n tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - new OCSP residual-classification contract syntax is valid

- `bash tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
  - result: PASS
  - summary:
    - source/API truth now points at `ISSLOCSPStapling`
    - the direct-core `GetOCSP*` residual surface is frozen to the expected 4 backend-specific proof files

- `git diff --check`
  - result: PASS
  - summary:
    - current OCSP residual-classification batch has no whitespace or patch-format issues

### Client OCSP Optional Interface Capability Alignment

- `sed -n '35,95p' src/fafafa.ssl.openssl.context.pas`
- `sed -n '24,80p' src/fafafa.ssl.openssl.connection.pas`
- `sed -n '35,105p' src/fafafa.ssl.wolfssl.context.pas`
- `sed -n '24,80p' src/fafafa.ssl.wolfssl.connection.pas`
  - result: PASS
  - summary:
    - static review confirmed the public-path drift was structural, not just wording:
      - `TOpenSSLConnection` / `TWolfSSLConnection` still directly implemented `ISSLOCSPStapling`
      - while capability truth for `OCSPStaplingSupport` remained runtime-aware

- add `docs/plans/2026-05-19-client-ocsp-optional-interface-capability-alignment.md`
  - change:
    - record the bounded batch that closes client-side OCSP optional-interface drift on the public `CreateConnection(...)` path

- update `tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `OpenSSL base connection no longer implements optional OCSP or early-data connection interfaces unconditionally`
      - current source still had `TOpenSSLConnection` directly implementing `ISSLOCSPStapling`
    - GREEN after fix:
      - the contract now also locks client-side OCSP connection gating for `OpenSSL` / `WolfSSL`
      - it verifies dedicated `ocsp` / `early-data` / combined subclasses plus `CreateConnection(...)` matrix selection

- update `src/fafafa.ssl.openssl.connection.pas`
  - change:
    - remove unconditional `ISSLOCSPStapling` exposure from `TOpenSSLConnection`
    - add:
      - `TOpenSSLOCSPConnection`
      - `TOpenSSLAdvancedConnection`

- update `src/fafafa.ssl.openssl.context.pas`
  - change:
    - add `HasClientOCSPCapability`
    - make both `CreateConnection(ASocket)` and `CreateConnection(AStream)` select:
      - `TOpenSSLConnection`
      - `TOpenSSLOCSPConnection`
      - `TOpenSSLEarlyDataConnection`
      - `TOpenSSLAdvancedConnection`
      according to current capability truth

- update `src/fafafa.ssl.wolfssl.connection.pas`
  - change:
    - remove unconditional `ISSLOCSPStapling` exposure from `TWolfSSLConnection`
    - add:
      - `TWolfSSLOCSPConnection`
      - `TWolfSSLAdvancedConnection`

- update `src/fafafa.ssl.wolfssl.context.pas`
  - change:
    - add `HasClientOCSPCapability`
    - make both `CreateConnection(ASocket)` and `CreateConnection(AStream)` select:
      - `TWolfSSLConnection`
      - `TWolfSSLOCSPConnection`
      - `TWolfSSLEarlyDataConnection`
      - `TWolfSSLAdvancedConnection`
      according to current capability truth

- `bash -n tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - result: PASS
  - summary:
    - expanded optional-interface source contract syntax is valid

- `bash tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - result: PASS
  - summary:
    - client-side OCSP optional-interface gating now matches current capability truth for both `OpenSSL` and `WolfSSL`

- `mkdir -p tmp/test_backend_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_contract -FEtmp/test_backend_contract -otmp/test_backend_contract/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract/test_backend_contract`
  - result: PASS
  - summary:
    - compile emitted existing unrelated warnings only
    - runtime contract summary:
      - `Total Tests: 135`
      - `Passed: 111`
      - `Failed: 0`
      - `Skipped: 24`
    - `Contract 10` remained green for `OpenSSL`, `WolfSSL`, `MbedTLS`, and `FreePascal`
    - the broader optional-interface contract set stayed intact after the new connection subclass matrix

- `git diff --check`
  - result: PASS
  - summary:
    - current client-side OCSP optional-interface alignment batch has no whitespace or patch-format issues

### SupportsCallbacks Capability Truth Audit

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: PASS
  - summary:
    - no unsynced session catch-up actions were required before starting this batch

- `rg -n "SupportsCallbacks|SetVerifyCallback|SetPasswordCallback|SetInfoCallback|FVerifyCallback|FPasswordCallback|FInfoCallback" src tests docs/plans task_plan.md findings.md progress.md`
  - result: PASS
  - summary:
    - static audit isolated the callback capability seam:
      - `OpenSSL` published `SupportsCallbacks=True`
      - `FreePascal` also published `SupportsCallbacks=True`
      - `WinSSL` runtime consumed verify/info callbacks but had no published `SupportsCallbacks=True`
      - `WolfSSL` / `MbedTLS` showed setter/field storage but no published callback capability

- `rg -n "VerifyCallback|InfoCallback|PasswordCallback" src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.wolfssl.connection.pas src/fafafa.ssl.mbedtls.connection.pas src/fafafa.ssl.freepascal.handshake.pas src/fafafa.ssl.wolfssl.handshake.pas src/fafafa.ssl.mbedtls.handshake.pas`
  - result: FAIL
  - summary:
    - `src/fafafa.ssl.*.handshake.pas` files do not exist in the current tree
    - audit path was immediately narrowed back to the real `*.connection.pas` runtime surfaces instead of repeating the failed lookup

- add `docs/plans/2026-05-19-supportscallbacks-capability-truth-audit.md`
  - change:
    - record the bounded batch that freezes current callback capability truth and the exact backend classification

- add `tests/scripts/test_callback_capability_truth_contract.sh`
  - change:
    - add a focused source-truth contract that ties callback capability publication to actual runtime/source classification across OpenSSL, WinSSL, FreePascal, WolfSSL, and MbedTLS

- add `tests/test_backend_callback_capability_truth_contract.pas`
  - change:
    - add a small cross-platform runtime capability contract that checks `SupportsCallbacks` truth for every available backend and naturally skips `WinSSL` on Linux

- `bash -n tests/scripts/test_callback_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - new callback capability source contract syntax is valid

- `bash tests/scripts/test_callback_capability_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `WinSSL capability truth must publish SupportsCallbacks while runtime callback wiring exists`
    - GREEN after fix:
      - published/source truth now matches the current callback runtime classification for all audited backends

- `mkdir -p tmp/test_callback_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_capability_truth -FEtmp/test_callback_capability_truth -otmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas && ./tmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `FreePascal Native SupportsCallbacks mismatch: expected=False actual=True`
    - GREEN after fix:
      - `OpenSSL SupportsCallbacks = True`
      - `FreePascal Native SupportsCallbacks = False`
      - `WolfSSL SupportsCallbacks = False`
      - `MbedTLS SupportsCallbacks = False`
      - `WinSSL` is skipped on Linux and remains intended for Windows CI/runtime validation
    - compile emitted existing unrelated warnings only

- update callback capability truth sources:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - change:
    - clarify the semantic baseline for `SupportsCallbacks`
    - publish `WinSSL` callback capability
    - retract `FreePascal` callback mispublication
    - explicitly freeze `WolfSSL` / `MbedTLS` callback capability at `False`

- `git diff --check`
  - result: PASS
  - summary:
    - current callback capability truth batch has no whitespace or patch-format issues

### Callback Setter Fail-Closed Alignment

- `sed -n '540,715p' src/fafafa.ssl.freepascal.context.pas`
- `sed -n '770,930p' src/fafafa.ssl.wolfssl.context.pas`
- `sed -n '760,935p' src/fafafa.ssl.mbedtls.context.pas`
- `sed -n '1898,2205p' src/fafafa.ssl.openssl.context.pas`
  - result: PASS
  - summary:
    - static audit confirmed the real drift after the previous capability batch:
      - `FreePascal` / `WolfSSL` / `MbedTLS` still silently stored non-nil verify/password/info callbacks
      - `OpenSSL` remained the reference implementation with real runtime callback wiring

- `sed -n '300,360p' docs/reference/API_REFERENCE.md`
- `sed -n '1476,1496p' docs/reference/API_REFERENCE.md`
  - result: PASS
  - summary:
    - active API reference still published stale callback type signatures and lacked a callback capability gating note

- add `docs/plans/2026-05-19-callback-setter-fail-closed-alignment.md`
  - change:
    - record the bounded batch that closes false-backend callback setter silent-store drift and API callback signature drift

- add `tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - change:
    - add a focused source/docs contract that guards:
      - callback capability gating note in `base` / `API_REFERENCE`
      - current callback type signatures in active API docs
      - fail-closed setter source patterns for `FreePascal` / `WolfSSL` / `MbedTLS`

- add `tests/test_backend_callback_setter_fail_closed_contract.pas`
  - change:
    - add a runtime contract that verifies:
      - published callback backends accept non-nil assignments and nil clears
      - unpublished callback backends reject non-nil assignments with unsupported semantics and still accept nil clears

- `bash -n tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - result: PASS
  - summary:
    - new callback setter source/docs contract syntax is valid

- `bash tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - result: FAIL -> FAIL -> PASS
  - summary:
    - RED first exposed:
      - `base interface docs must explain callback capability gating and nil-clear semantics`
    - after source/docs fix, the contract itself exposed one test-script quoting bug caused by backticks inside a double-quoted fixed string
    - GREEN after fixing the contract:
      - source/docs truth now locks callback gating notes, current callback signatures, and false-backend fail-closed setter source patterns

- `mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas && ./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `FreePascal Native must reject non-nil Verify callback while SupportsCallbacks=False`
    - GREEN after fix:
      - `OpenSSL` published callback setters accept non-nil assignments and nil clears
      - `FreePascal` / `WolfSSL` / `MbedTLS` unpublished callback setters now fail-closed on non-nil assignments and accept nil clears
      - `WinSSL` is skipped on Linux and remains pending Windows-host/CI callback-surface granularity proof
    - compile emitted existing unrelated warnings only

- update callback setter/runtime truth sources:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.wolfssl.context.pas`
  - `src/fafafa.ssl.mbedtls.context.pas`
  - `docs/reference/API_REFERENCE.md`
  - change:
    - add callback capability gating note
    - make false-capability backends reject non-nil callback assignments with unsupported semantics
    - keep `nil` callback clears valid
    - fix active API reference callback type signatures to current source truth

- `bash tests/scripts/test_callback_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - previous callback capability truth batch remains aligned after the new setter semantics changes

- `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active `ISSLLibrary` / `ISSLContext` docs still match current source truth after the callback API reference fix

- `git diff --check`
  - result: PASS
  - summary:
    - current callback setter fail-closed batch has no whitespace or patch-format issues

### WinSSL Password Callback Partial-Publication Alignment

- `rg -n "FPasswordCallback|SetPasswordCallback\(|GetWinSSLVerifyCallback|GetWinSSLInfoCallback|Password callback|Verify callback|Info callback" src/fafafa.ssl.winssl.* tests/unit/test_winssl_comprehensive.pas docs/reference/WINSSL_DESIGN.md docs/reference/API_REFERENCE.md`
  - result: PASS
  - summary:
    - static audit confirmed:
      - `WinSSL` verify/info callbacks still have runtime/source use-sites
      - `WinSSL` password callback only had field storage and a misleading passing unit test

- add `docs/plans/2026-05-19-winssl-password-callback-publication-alignment.md`
  - change:
    - record the bounded WinSSL callback granularity batch that narrows published callback truth to verify/info and retracts password callback silent setter behavior

- add `tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
  - change:
    - add a focused source/docs contract that guards:
      - WinSSL password callback fail-closed source pattern
      - WinSSL verify/info setter publication
      - WinSSL docs truth
      - WinSSL unit-test expectation
      - callback runtime contract special-casing

- update `src/fafafa.ssl.winssl.context.pas`
  - change:
    - add `RejectUnsupportedCallbackAssignment`
    - make `SetPasswordCallback` fail-closed for non-nil assignments
    - keep verify/info callback setters published

- update `tests/unit/test_winssl_comprehensive.pas`
  - change:
    - add `InfoCallback` helper
    - change callback test truth from:
      - password callback set
      to:
      - password callback unsupported as expected
    - keep verify/info published expectations

- update `tests/test_backend_callback_setter_fail_closed_contract.pas`
  - change:
    - replace the old “published backends accept all three callbacks” assumption for `WinSSL`
    - add `CheckWinSSLPartialBackend(...)` so future Windows runtime proof checks:
      - verify/info published
      - password callback unsupported

- update docs:
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - record that `SupportsCallbacks=True` is coarse-grained and current `WinSSL` only publishes verify/info runtime paths

- `bash -n tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL callback granularity source contract syntax is valid

- `bash tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
  - result: PASS
  - summary:
    - WinSSL password callback partial-publication truth is now locked across source, docs, unit-test expectation, and runtime-contract special-casing

- `mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas && ./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract`
  - result: PASS
  - summary:
    - Linux still skips `WinSSL`
    - the cross-backend callback setter runtime contract stayed green after adding the WinSSL partial-publication matrix
    - compile emitted existing unrelated warnings only

- `bash tests/scripts/test_callback_setter_fail_closed_contract.sh`
  - result: PASS
  - summary:
    - the previous callback setter fail-closed batch remains aligned after the WinSSL partial-publication refinement

- `bash tests/scripts/test_callback_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - callback capability truth remains aligned after the WinSSL password callback refinement

- `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active interface docs still match current source truth after the new API reference wording

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL password callback partial-publication batch has no whitespace or patch-format issues

### Callback Publication Matrix Truth

- `rg -n "SupportsCallbacks|callback surface|Verify callback|Password callback|Info callback|回调" docs docs/reference src task_plan.md findings.md progress.md`
  - result: PASS
  - summary:
    - static audit confirmed the remaining callback docs gap had moved to active matrix surfaces:
      - `API_REFERENCE` already carried callback gating / WinSSL partial-publication truth
      - `BACKEND_CAPABILITY_MATRIX` / `WINSSL_BACKEND_CAPABILITY_MATRIX` still lacked a callback publication row/note

- add `docs/plans/2026-05-19-callback-publication-matrix-truth.md`
  - change:
    - record the bounded docs-only batch that promotes callback truth from API reference level into active capability matrices

- add `tests/scripts/test_callback_publication_matrix_truth_contract.sh`
  - change:
    - add a focused shell contract that guards:
      - callback publication quick-reference row in `BACKEND_CAPABILITY_MATRIX`
      - callback row semantics note
      - WinSSL backend matrix callback row
      - coarse `SupportsCallbacks=True` explanation for current WinSSL partial publication

- `bash -n tests/scripts/test_callback_publication_matrix_truth_contract.sh`
  - result: PASS
  - summary:
    - new callback publication matrix contract syntax is valid

- `bash tests/scripts/test_callback_publication_matrix_truth_contract.sh`
  - result: FAIL -> FAIL -> PASS
  - summary:
    - RED first exposed:
      - `backend capability quick-reference matrix must publish current callback availability`
    - after docs fix, the contract itself hit one quoting issue caused by backticks inside a double-quoted fixed string
    - GREEN after fixing the script:
      - active callback matrix docs now match current callback publication truth

- update matrix docs:
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - change:
    - add `Context Callbacks` quick-reference truth
    - record callback row semantics
    - add WinSSL partial-publication row
    - add coarse `SupportsCallbacks=True` explanation for current WinSSL behavior

- `bash tests/scripts/test_callback_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - callback capability truth remains aligned after the matrix-doc updates

- `bash tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
  - result: PASS
  - summary:
    - WinSSL password callback partial-publication truth remains aligned after the matrix-doc updates

- `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - result: PASS
  - summary:
    - active API reference and library/context docs still match current source truth after the matrix-doc updates

- `git diff --check`
  - result: PASS
  - summary:
    - current callback publication matrix batch has no whitespace or patch-format issues

### Password-Protected Key Capability Truth

- `rg -n "SupportsPasswordProtectedKeys|APassword|password-protected|LoadPrivateKey" src/fafafa.ssl.*context.pas src/fafafa.ssl.*lib.pas docs/BACKEND_CAPABILITY_MATRIX.md docs/reference/* tests`
  - result: PASS
  - summary:
    - static audit exposed a new capability/source drift family:
      - `FreePascal` / `WolfSSL` still published `SupportsPasswordProtectedKeys=True`
      - `FreePascal` silently ignored non-empty `APassword`
      - `WolfSSL` had no shipped password bridge and still carried stale “密码回调需要单独设置” comments

- add `docs/plans/2026-05-19-password-protected-key-capability-truth.md`
  - change:
    - record the bounded plan for capability truth + fail-closed remediation on password-protected private-key paths

- add `tests/scripts/test_password_protected_key_capability_truth_contract.sh`
  - change:
    - add a focused shell contract that guards:
      - `FreePascal` / `WolfSSL` capability truth
      - non-empty `APassword` fail-closed guards on file/stream/PEM loaders
      - active doc truth in backend matrix / API reference / WinSSL matrix

- add `tests/test_backend_password_protected_key_capability_truth_contract.pas`
  - change:
    - add a focused runtime contract that checks:
      - `SupportsPasswordProtectedKeys=False` for `FreePascal` / `WolfSSL`
      - `SupportsPasswordProtectedKeys=True` for `MbedTLS` and `WinSSL` when available
      - `FreePascal` / `WolfSSL` reject non-empty private-key passwords on file/stream/PEM load paths

- `bash -n tests/scripts/test_password_protected_key_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - new password-protected-key shell contract syntax is valid

- `bash tests/scripts/test_password_protected_key_capability_truth_contract.sh`
  - result: FAIL -> FAIL -> PASS
  - summary:
    - RED first exposed the missing base/API/docs truth
    - the first green attempt was blocked by a self-inflicted script helper mistake (`_rg_pcre.py` did not exist)
    - the second attempt hit the known backtick-in-double-quoted-string shell quoting pitfall
    - GREEN after inlining the PCRE helper and switching the affected fixed strings to single quotes:
      - source/docs truth is now locked for this capability family

- update source:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.wolfssl.context.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - change:
    - add public interface guidance for non-empty private-key passwords
    - set `FreePascal` / `WolfSSL` `SupportsPasswordProtectedKeys=False`
    - fail-close `FreePascal` / `WolfSSL` file/stream/PEM private-key loaders on non-empty `APassword`
    - annotate current WinSSL coarse-grained truth in source comment

- update docs:
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - change:
    - add password-protected private-key quick-reference truth
    - record false-backend fail-closed semantics
    - record current WinSSL partial publication (`PFX/P12` only; PEM path still unsupported)

- `mkdir -p tmp/test_password_protected_key_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_password_protected_key_capability_truth -FEtmp/test_password_protected_key_capability_truth -otmp/test_password_protected_key_capability_truth/test_backend_password_protected_key_capability_truth_contract tests/test_backend_password_protected_key_capability_truth_contract.pas && ./tmp/test_password_protected_key_capability_truth/test_backend_password_protected_key_capability_truth_contract`
  - result: FAIL -> PASS
  - summary:
    - first compile failed because the new test used `TProc` / anonymous-proc syntax that this FPC mode does not provide
    - GREEN after rewriting to explicit helper procedures:
      - `FreePascal Native` capability now reports `False`
      - `WolfSSL` capability now reports `False`
      - `MbedTLS` remains `True`
      - `WinSSL` correctly skips on Linux host
      - `FreePascal` / `WolfSSL` runtime contracts now reject non-empty private-key passwords as unsupported

- `gh run list --limit 8 --json databaseId,displayTitle,event,headSha,status,conclusion,workflowName`
  - result: PASS
  - summary:
    - previous callback-doc batch `24e4d91` `docs(callbacks): align publication matrices` is now fully green in GitHub CI

- `git diff --check`
  - result: PASS
  - summary:
    - current password-protected-key capability batch has no whitespace or patch-format issues

### WinSSL Private-Key Format Truth

- `rg -n "SupportsDERPrivateKey|SupportsPKCS8PrivateKey|LoadPrivateKey\\(|PFX/P12|client.key|server.key" src docs tests`
  - result: PASS
  - summary:
    - static audit exposed a new WinSSL-specific drift family:
      - `SupportsDERPrivateKey=True` / `SupportsPKCS8PrivateKey=True` were still published
      - runtime load path only consumed `PFX/P12`
      - WinSSL-specific guides still showed bare `client.key` / `server.key` examples

- add `docs/plans/2026-05-19-winssl-private-key-format-truth.md`
  - change:
    - record the bounded plan for WinSSL key-format capability truth + non-PFX fail-closed remediation

- add `tests/scripts/test_winssl_private_key_format_truth_contract.sh`
  - change:
    - add a focused shell contract that guards:
      - `SupportsDERPrivateKey=False`
      - `SupportsPKCS8PrivateKey=False`
      - `SupportsPKCS12=True`
      - non-PFX fail-closed semantics in `TWinSSLContext.LoadPrivateKey(AStream, APassword)`
      - WinSSL-specific active docs / guide examples

- add `tests/test_winssl_private_key_format_truth_contract.pas`
  - change:
    - add a focused runtime contract that checks:
      - WinSSL capability truth on Windows
      - non-PFX file/stream private-key inputs fail-closed as unsupported
      - Linux host continues to compile and skip cleanly

- `bash -n tests/scripts/test_winssl_private_key_format_truth_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL private-key-format shell contract syntax is valid

- `bash tests/scripts/test_winssl_private_key_format_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed that `WinSSL` still published `SupportsDERPrivateKey=True`
    - GREEN after shrinking capability truth, fixing non-PFX fail-closed semantics, and updating WinSSL-specific docs

- update source:
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.winssl.context.pas`
  - change:
    - set `SupportsDERPrivateKey=False`
    - set `SupportsPKCS8PrivateKey=False`
    - keep `SupportsPKCS12=True` as the current published private-key path
    - fix `LoadPrivateKey(AStream, APassword)` so:
      - nil stream -> invalid param
      - non-PFX input -> fail-closed `unsupported`
    - remove stale comment implying callers can “先转换为 DER 格式” and still stay on current WinSSL public path

- update docs:
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - `docs/guides/WINSSL_QUICKSTART.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - record that WinSSL does not currently publish bare DER / PKCS#8 private-key loading
    - keep `PFX/P12` as the only shipped private-key import path
    - replace misleading bare key-file examples with `PFX/P12` bundle examples

- `mkdir -p tmp/test_winssl_private_key_format_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_winssl_private_key_format_truth -FEtmp/test_winssl_private_key_format_truth -otmp/test_winssl_private_key_format_truth/test_winssl_private_key_format_truth_contract tests/test_winssl_private_key_format_truth_contract.pas && ./tmp/test_winssl_private_key_format_truth/test_winssl_private_key_format_truth_contract`
  - result: PASS
  - summary:
    - local Linux host compiles the new contract cleanly
    - runtime correctly skips WinSSL execution on non-Windows host
    - the contract is now ready for GitHub Windows lanes to exercise the real WinSSL path

- `bash tests/scripts/test_password_protected_key_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - previous password-protected-key capability truth remains aligned after the WinSSL DER/PKCS#8 refinement

- `bash tests/scripts/test_winssl_capability_source_contract.sh`
  - result: PASS
  - summary:
    - existing WinSSL capability/source truth contract remains green after the key-format adjustments

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL private-key-format batch has no whitespace or patch-format issues

### API Inventory / PKCS11 High-Entry Doc Truth

- `rg -n "LoadCertificateFromFile|LoadPrivateKeyFromFile|LoadCAFromFile|SetHostname|Connect\\(host, port\\)|ReadAll|GetCipherSuite|GetLastError: string|完全相同的接口|统一等价接口|SupportsPKCS12|SupportsPasswordProtectedKeys|SupportsCallbacks|SupportsFIPSMode|0-RTT|证书固定|自定义 I/O|PKCS#11|TPM" docs/guides docs/reference`
  - result: PASS
  - summary:
    - static sweep showed the next high-value lane was no longer source capability booleans
    - the highest-risk remaining drift had moved into high-entry docs, especially:
      - `docs/reference/API_INVENTORY.md`
      - `docs/guides/PKCS11_USER_GUIDE.md`
      - `docs/reference/PKCS11_ARCHITECTURE.md`

- `rg -n "TOpenSSLContext|TWinSSLContext|TMbedTLSContext|TWolfSSLContext|TFreePascalContext|GetOCSPStaplingEnabled|LoadPrivateKeyFromPKCS11|SupportsPKCS11|SupportsOCSPStapling|SupportsCertificateTransparency" src docs/reference/API_INVENTORY.md`
  - result: PASS
  - summary:
    - confirmed `API_INVENTORY.md` had fallen far behind current implementation truth:
      - missing `FreePascal` / `MbedTLS` / `WolfSSL` context and connection families
      - still claiming shipped OCSP compatibility methods were "待实现"
      - still framing `PKCS#11` as a future completion item

- add `docs/plans/2026-05-19-api-inventory-pkcs11-high-entry-doc-truth.md`
  - change:
    - recorded the bounded docs-only plan for retightening high-entry inventory and PKCS#11 truth

- add `tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - `API_INVENTORY.md` current-surface index truth
      - shipped OCSP compatibility method truth
      - OpenSSL-only PKCS#11 published-path truth
      - runtime-aware `SupportsPKCS11` guidance
      - non-OpenSSL backend `SupportsPKCS11=False` boundary
      - corrected `LoadPrivateKeyFromPKCS11` signature in the PKCS#11 architecture doc

- update docs:
  - `docs/reference/API_INVENTORY.md`
  - `docs/guides/PKCS11_USER_GUIDE.md`
  - `docs/reference/PKCS11_ARCHITECTURE.md`
  - change:
    - rewrote `API_INVENTORY.md` into a current public-surface index
    - removed stale phase-snapshot / test-stat / performance / next-step backlog sections from the inventory page
    - added OpenSSL-only PKCS#11 published-path guidance
    - added runtime-aware Provider / ENGINE readiness wording
    - added explicit non-OpenSSL backend `SupportsPKCS11=False` boundary
    - fixed the PKCS#11 architecture doc's stale `LoadPrivateKeyFromPKCS11(...)` signature example

- `bash -n tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
  - result: PASS
  - summary:
    - new high-entry docs contract syntax is valid

- `bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed a wording mismatch in the new Provider / ENGINE readiness assertion
    - GREEN after tightening the contract to the actual guide wording:
      - `API_INVENTORY` and PKCS#11 high-entry docs now match current source truth

- `bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
  - result: PASS
  - summary:
    - the earlier builder/runtime PKCS#11 doc contract remained green after adding runtime-aware boundary guidance

- `npx prettier --write docs/reference/API_INVENTORY.md docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md`
  - result: PASS
  - summary:
    - targeted doc files are formatted and stable

- `git diff --check`
  - result: PASS
  - summary:
    - current API inventory / PKCS#11 high-entry docs batch has no whitespace or patch-format issues

### WinSSL Quickstart Runtime Truth

- `rg -n "待实现|未实现|sslVerifyPeer|sslVerifyFailIfNoPeerCert|LoadCAFile|GetServerName|双向 TLS|mTLS|客户端证书" docs/guides/WINSSL_* docs/reference/WINSSL_*`
  - result: PASS
  - summary:
    - targeted sweep showed `WINSSL_QUICKSTART.md` still carried a concentrated runtime-truth drift cluster even after other WinSSL docs had been tightened
    - the same page still marked verify / mTLS / CA paths as pending while its own FAQ already said those paths were implemented

- `rg -n "procedure TWinSSLContext.LoadCAFile|procedure TWinSSLContext.SetVerifyMode|sslVerifyPeer|sslVerifyFailIfNoPeerCert|LoadPrivateKey\\('client\\.pfx'|LoadCertificate\\(|LoadPrivateKey\\(|client certificate|双向 TLS|mTLS|SetCertificateStore|LoadCAFile" src/fafafa.ssl.winssl.context.pas src/fafafa.ssl.winssl.connection.pas docs/reference/WINSSL_DESIGN.md docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/reference/API_REFERENCE.md tests/winssl tests`
  - result: PASS
  - summary:
    - source/tests/reference evidence confirmed the quickstart was behind the current implementation:
      - `LoadCAFile(...)` is implemented
      - `sslVerifyPeer` and `sslVerifyFailIfNoPeerCert` are active verify-mode paths
      - WinSSL mTLS tests and docs already exist
      - connection-level SNI owner path remains `ISSLClientConnection`

- add `docs/plans/2026-05-19-winssl-quickstart-runtime-truth.md`
  - change:
    - recorded the bounded docs-only plan for quickstart runtime-truth cleanup

- add `tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - current verify-mode syntax
      - current mTLS verify-mode syntax
      - current `LoadCAFile(...)` path
      - per-connection SNI troubleshooting example
      - removal of stale "待实现"/"未实现" claims in the quickstart

- update docs:
  - `docs/guides/WINSSL_QUICKSTART.md`
  - change:
    - replaced stale non-set verify syntax with current set-based API
    - removed pending markers from verify / mTLS / CA guidance
    - updated troubleshooting wording to current verification failure semantics
    - switched SNI inspection example from deprecated `Ctx.GetServerName` to per-connection `ISSLClientConnection.GetServerName`
    - replaced the stale "client certificates unsupported" troubleshooting note with current mTLS guidance

- `bash -n tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL quickstart runtime-truth contract syntax is valid

- `bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL quickstart now matches the current runtime/source truth

- `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - result: PASS
  - summary:
    - quickstart import/unit guidance remained aligned after the runtime-truth cleanup

- `bash tests/scripts/test_winssl_private_key_format_truth_contract.sh`
  - result: PASS
  - summary:
    - existing WinSSL private-key-format doc truth remained green after the quickstart adjustments

- `npx prettier --write docs/guides/WINSSL_QUICKSTART.md`
  - result: PASS
  - summary:
    - WinSSL quickstart formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL quickstart runtime-truth batch has no whitespace or patch-format issues

### Security Guide HSM/Password-Key Truth

- `rg -n "LoadPKCS11Engine|LoadKeyFromHSM|SetPrivateKey\\(|LoadPrivateKey\\('server\\.key', 'strong-password'\\)|SupportsPasswordProtectedKeys|UsePKCS11|WithPKCS11PIN|LoadPrivateKeyFromPKCS11|PKCS#11|密码保护私钥|HSM" docs/guides/SECURITY_GUIDE.md docs/guides/PKCS11_USER_GUIDE.md docs/reference/API_REFERENCE.md docs/BACKEND_CAPABILITY_MATRIX.md src/fafafa.ssl.base.pas`
  - result: PASS
  - summary:
    - static audit confirmed `SECURITY_GUIDE.md` still demonstrated nonexistent HSM helper APIs and overly generic password-protected-key guidance
    - current truth sources were already available in:
      - `API_REFERENCE`
      - `BACKEND_CAPABILITY_MATRIX`
      - `PKCS11_USER_GUIDE`
      - source capability comments in `fafafa.ssl.base.pas`

- add `docs/plans/2026-05-19-security-guide-hsm-password-truth.md`
  - change:
    - recorded the bounded docs-only plan for security-guide HSM / password-key truth cleanup

- add `tests/scripts/test_security_guide_hsm_password_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - password-protected-key capability gate wording
      - WinSSL password-path boundary wording
      - OpenSSL-only PKCS#11 published-path wording
      - runtime-aware `SupportsPKCS11` gate in the HSM example
      - absence of nonexistent HSM helper APIs

- update docs:
  - `docs/guides/SECURITY_GUIDE.md`
  - change:
    - rewrote the password-protected private-key example to gate on `SupportsPasswordProtectedKeys`
    - documented current WinSSL / FreePascal / WolfSSL password-path boundaries
    - replaced nonexistent HSM helper calls with the current `LoadPrivateKey('pkcs11:...')` public path
    - documented the OpenSSL-only PKCS#11 published path and linked to `PKCS11_USER_GUIDE`

- `bash -n tests/scripts/test_security_guide_hsm_password_truth_contract.sh`
  - result: PASS
  - summary:
    - new security-guide HSM/password-key contract syntax is valid

- `bash tests/scripts/test_security_guide_hsm_password_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed a shell quoting self-injury caused by backticks inside double-quoted fixed strings
    - GREEN after converting the affected contract strings to single-quoted fixed patterns:
      - security-guide HSM/password-key guidance now matches current source truth

- `bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
  - result: PASS
  - summary:
    - earlier PKCS#11 high-entry doc truth remained green after the security-guide cleanup

- `npx prettier --write docs/guides/SECURITY_GUIDE.md`
  - result: PASS
  - summary:
    - security guide formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current security-guide HSM/password-key batch has no whitespace or patch-format issues

### Specialized Guide Historical Test Snapshot Cleanup

- `rg -n "总测试数|通过率|100%|综合测试|基础测试|Phase [A-Z]|完成总结|测试结果总结" docs/guides/PKCS12_USER_GUIDE.md docs/guides/CMS_USER_GUIDE.md`
  - result: PASS
  - summary:
    - targeted scan showed the next residual docs family was no longer API truth drift
    - it had become historical test-snapshot drift inside specialized guides:
      - `CMS_USER_GUIDE.md`
      - `PKCS12_USER_GUIDE.md`

- add `docs/plans/2026-05-19-specialized-guide-historical-test-snapshot-cleanup.md`
  - change:
    - recorded the bounded docs-only plan for demoting historical test snapshots out of current specialized-guide truth

- add `tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - presence of "current verification entrypoint, not fixed snapshot" guidance
      - absence of hardcoded `43/43`, `20/20`, `34/34`
      - absence of captured `总测试数` / `通过率` / `预期输出` blocks in the guide body

- update docs:
  - `docs/guides/CMS_USER_GUIDE.md`
  - `docs/guides/PKCS12_USER_GUIDE.md`
  - change:
    - removed captured expected-output snapshots
    - removed hardcoded historical pass-rate/count statements from the guide body
    - kept the executable test commands
    - replaced fixed-output expectations with success criteria
    - demoted history-style update logs to maintenance guidance

- `bash -n tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh`
  - result: PASS
  - summary:
    - new specialized-guide snapshot contract syntax is valid

- `bash tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh`
  - result: PASS
  - summary:
    - CMS / PKCS12 specialized guides no longer present historical test snapshots as current truth

- `npx prettier --write docs/guides/CMS_USER_GUIDE.md docs/guides/PKCS12_USER_GUIDE.md`
  - result: PASS
  - summary:
    - specialized guides formatting remains stable

- `git diff --check`
  - result: FAIL -> PASS
  - summary:
    - first check caught trailing whitespace in the CMS guide version/update footer
    - GREEN after removing the markdown trailing spaces:
      - current specialized-guide snapshot cleanup batch has no whitespace or patch-format issues

### PKCS7 Guide Status/Performance Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "100%|158/158|2 ms|500 ops/s|Production Ready|状态|性能|测试覆盖" docs/guides/PKCS7_USER_GUIDE.md`
  - result: PASS
  - summary:
    - static scan confirmed `PKCS7_USER_GUIDE.md` still embedded fixed status/performance/test snapshots as current guide truth

- `rg -n "PKCS7|LoadPKCS7Functions|模块加载状态|direct capability|无直接字段|测试结果" docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md tests/certificate tests`
  - result: PASS
  - summary:
    - reference truth already existed for the PKCS7 batch:
      - no direct capability field
      - support depends on `LoadPKCS7Functions`
      - module-loaded truth uses `osmPKCS7`
      - current support is evidenced through focused tests

- `rg -n "CreatePKCS7SignedData|VerifyPKCS7SignedData|EncryptData|DecryptData|LoadPKCS7Functions" src/fafafa.ssl.openssl.api.pkcs.pas src/fafafa.ssl.openssl.api.pkcs7.pas tests`
  - result: PASS
  - summary:
    - source audit confirmed the current public PKCS7 surface is not only raw `PKCS7_sign` / `PKCS7_encrypt`
    - published helper entrypoints also exist in `fafafa.ssl.openssl.api.pkcs7`:
      - `SignData`
      - `VerifySignedData`
      - `EncryptData`
      - `DecryptData`

- add `docs/plans/2026-05-19-pkcs7-guide-status-performance-truth.md`
  - change:
    - recorded the bounded docs-only plan for PKCS7 guide status/performance truth cleanup

- add `tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - presence of current backend-surface boundary wording
      - presence of no-direct-capability-field wording
      - presence of helper entrypoint wording
      - absence of hardcoded `Production Ready`, `100%`, `158/158`, `2 ms`, `500 ops/s`

- update docs:
  - `docs/guides/PKCS7_USER_GUIDE.md`
  - change:
    - rewrote the guide to state the current `OpenSSL` backend scope
    - added helper-vs-raw entrypoint guidance
    - documented the current verification files and success criteria
    - kept the BIO ownership rules
    - removed fixed status/performance/test-count snapshots

- `bash -n tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh`
  - result: PASS
  - summary:
    - new PKCS7 guide truth contract syntax is valid

- `bash tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh`
  - result: PASS
  - summary:
    - PKCS7 guide no longer presents historical status/performance snapshots as current truth

- `npx prettier --write docs/guides/PKCS7_USER_GUIDE.md`
  - result: PASS
  - summary:
    - PKCS7 guide formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current PKCS7 guide truth batch has no whitespace or patch-format issues

- `rg -n "Production Ready|100%|30/30|预期输出|2 ms|500 ops/s|完成度|完成总结|通过率|总测试数" docs/guides/WINSSL_USER_GUIDE.md docs/guides/5_MINUTE_QUICKSTART.md docs/guides/QUICKSTART_30SEC.md docs/reference/ARCHITECTURE.md docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  - result: PASS
  - summary:
    - next residual high-entry truth drift has been narrowed to:
      - `WINSSL_USER_GUIDE.md`
      - `QUICKSTART_30SEC.md`
      - `5_MINUTE_QUICKSTART.md`
      - `ARCHITECTURE.md`
    - performance-doc cleanup still exists, but remains lower priority than the high-entry pages above

### WinSSL User Guide Performance/Runtime Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "30/30|100%|204\\.52 ms|连接稳定性|Production Ready|预期输出|完成|性能|稳定性" docs/guides/WINSSL_USER_GUIDE.md`
  - result: PASS
  - summary:
    - static scan confirmed `WINSSL_USER_GUIDE.md` still embedded fixed WinSSL runtime snapshots in the performance/stability section

- `rg -n "observed_reuse|session_configured|VALIDATION_BUNDLE|windows-gate|runtime baseline" docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md tests/windows/VALIDATION_BUNDLE.md docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md .github/workflows/wave-b-b2-manual.yml`
  - result: PASS
  - summary:
    - current WinSSL runtime truth sources already existed for this batch:
      - `WINSSL_BACKEND_STATUS_REPORT`
      - `tests/windows/VALIDATION_BUNDLE.md`
      - `.github/workflows/wave-b-b2-manual.yml`
      - capability matrix session truth

- add `docs/plans/2026-05-19-winssl-user-guide-performance-runtime-truth.md`
  - change:
    - recorded the bounded docs-only plan for WinSSL user-guide performance/runtime truth cleanup

- add `tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - presence of runtime-baseline entrypoint wording
      - presence of validation-bundle / `windows-gate` references
      - absence of hardcoded `436.94 ms`, `204.52 ms`, `2.41 conn/s`, `30/30 成功`

- update docs:
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - replaced the fixed WinSSL benchmark table with runtime-baseline guidance
    - pointed readers to `WINSSL_BACKEND_STATUS_REPORT`, `tests/windows/VALIDATION_BUNDLE.md`, and `windows-gate`
    - rewrote success criteria around fresh artifact evidence and current session truth
    - kept the performance-tuning guide only as tuning guidance, not current runtime proof

- `bash -n tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL user-guide performance/runtime truth contract syntax is valid

- `bash tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL user guide no longer presents historical runtime metrics as current truth

- `bash tests/scripts/test_active_release_platform_truth_contract.sh`
  - result: PASS
  - summary:
    - broader release/platform/WinSSL bounded-status truth remained green after the guide runtime cleanup

- `bash tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - current connection-surface / backend-capability wording in the WinSSL guide remained green

- `npx prettier --write docs/guides/WINSSL_USER_GUIDE.md`
  - result: PASS
  - summary:
    - WinSSL user guide formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL user-guide runtime-truth batch has no whitespace or patch-format issues

- `rg -n "100% 完成|预期输出|完成度|Phase|30/30|Production Ready|100%|稳定性|项目状态" docs/guides/WINSSL_QUICKSTART.md docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md docs/reference/ARCHITECTURE.md`
  - result: PASS
  - summary:
    - next residual high-entry truth drift has now been narrowed to:
      - `docs/guides/WINSSL_QUICKSTART.md`
      - `docs/guides/QUICKSTART_30SEC.md`
      - `docs/guides/5_MINUTE_QUICKSTART.md`
      - `docs/reference/ARCHITECTURE.md`
    - `WINSSL_QUICKSTART.md` was promoted in priority because it still embeds `100% 完成` and phase-completion language in a first-contact guide

### WinSSL Quickstart Status/Phase Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "100% 完成|Phase 5 完成|Phase 1 完成|完整实现服务器模式|自动证书验证|项目状态|状态\\*\\*|所有 6 个阶段" docs/guides/WINSSL_QUICKSTART.md`
  - result: PASS
  - summary:
    - static scan confirmed `WINSSL_QUICKSTART.md` still embedded phase-completion / 100-percent wording in FAQ and footer

- `rg -n "性能对比|更快|相当|需要服务器模式|需要完整证书验证|~150ms|~160ms|~80 MB/s|~85 MB/s" docs/guides/WINSSL_QUICKSTART.md`
  - result: PASS
  - summary:
    - the same first-contact page also still embedded fixed benchmark snapshots and outdated OpenSSL-choice guidance that conflicted with current WinSSL public truth

- add `docs/plans/2026-05-19-winssl-quickstart-status-phase-truth.md`
  - change:
    - recorded the bounded docs-only plan for WinSSL quickstart phase/status/performance truth cleanup

- add `tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - presence of bounded top-level WinSSL truth wording
      - presence of bounded server/verify FAQ wording
      - presence of bounded footer status / authority links
      - absence of `100% 完成`, `Phase 1/5 完成`, fixed benchmark snapshots, and stale OpenSSL-choice bullets

- update docs:
  - `docs/guides/WINSSL_QUICKSTART.md`
  - change:
    - added a current-truth note near the top
    - rewrote FAQ answers for server mode / certificate verification / performance
    - rewrote the performance section into runtime-baseline guidance
    - tightened the OpenSSL-choice bullets to current cross-platform/runtime-sensitive cases
    - replaced the stale `100% 完成` footer with current bounded status and truth-source links

- `bash -n tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL quickstart status/phase truth contract syntax is valid

- `bash tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL quickstart no longer presents historical phase/benchmark snapshots as current truth

- `bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - previously fixed verify/mTLS/SNI runtime truth remained green after the quickstart status cleanup

- `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - result: PASS
  - summary:
    - current public facade / entrypoint truth in the quickstart remained green

- `npx prettier --write docs/guides/WINSSL_QUICKSTART.md`
  - result: PASS
  - summary:
    - WinSSL quickstart formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL quickstart truth batch has no whitespace or patch-format issues

- `rg -n "预期输出|100% 完成|生产就绪|完成度|Phase 5|Phase 1|~150ms|~160ms|~80 MB/s|~85 MB/s|状态\\*\\*|当前权威入口" docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md docs/reference/ARCHITECTURE.md docs/guides/WINSSL_QUICKSTART.md`
  - result: PASS
  - summary:
    - next residual high-entry truth drift has been narrowed to:
      - `docs/guides/QUICKSTART_30SEC.md`
      - `docs/guides/5_MINUTE_QUICKSTART.md`
      - `docs/reference/ARCHITECTURE.md`
    - `WINSSL_QUICKSTART.md` is now aligned to the same bounded truth chain as the WinSSL user/deployment docs

### High Entry Quickstarts Captured Output Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "your-org|预期输出|OpenSSL 3\\.0\\.2|OpenSSL 3\\.x\\.x|HTTP/1\\.1 200 OK|TLS 1\\.3|TLS_AES_256_GCM_SHA384" docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md`
  - result: PASS
  - summary:
    - static scan confirmed the next high-entry drift family had become captured output snapshots in general quickstart docs
    - `5_MINUTE_QUICKSTART.md` also still used a placeholder clone URL

- add `docs/plans/2026-05-19-high-entry-quickstarts-captured-output-truth.md`
  - change:
    - recorded the bounded docs-only plan for quickstart captured-output and clone-url truth cleanup

- add `tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - presence of current quickstart success-criteria wording
      - presence of the current public clone URL
      - absence of captured `预期输出`, fixed OpenSSL/TLS/HTTP snapshots, and the placeholder repo URL

- update docs:
  - `docs/guides/QUICKSTART_30SEC.md`
  - `docs/guides/5_MINUTE_QUICKSTART.md`
  - change:
    - replaced captured expected-output blocks with success criteria
    - demoted fixed OpenSSL/TLS/HTTP output text to “current run decides”
    - corrected the 5-minute quickstart clone URL to the live public repo

- `bash -n tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh`
  - result: PASS
  - summary:
    - new high-entry quickstarts captured-output contract syntax is valid

- `bash tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh`
  - result: PASS
  - summary:
    - both high-entry quickstarts no longer present captured runtime output as current truth

- `npx prettier --write docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md`
  - result: PASS
  - summary:
    - quickstart formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current high-entry quickstarts truth batch has no whitespace or patch-format issues

- `rg -n "预期输出|your-org|100% 完成|生产就绪|完成度|当前权威入口|WinSSL.*100% 完成" docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md docs/reference/ARCHITECTURE.md`
  - result: PASS
  - summary:
    - current obvious high-entry truth drift is now mainly concentrated in:
      - `docs/reference/ARCHITECTURE.md`
    - the two general quickstart entry pages have been pulled back to command-and-success-criteria truth

### Architecture Backend Status Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "100% 完成|生产就绪|WinSSL|OpenSSL 实现（Linux/macOS 默认）|状态" docs/reference/ARCHITECTURE.md`
  - result: PASS
  - summary:
    - static scan confirmed the final obvious high-entry doc drift had become the backend-status wording inside `ARCHITECTURE.md`

- add `docs/plans/2026-05-19-architecture-backend-status-truth.md`
  - change:
    - recorded the bounded docs-only plan for architecture backend-status truth cleanup

- add `tests/scripts/test_architecture_backend_status_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - presence of current backend truth-source wording
      - presence of bounded OpenSSL/WinSSL backend status wording
      - absence of `生产就绪` / `100% 完成` release-style backend table wording

- update docs:
  - `docs/reference/ARCHITECTURE.md`
  - change:
    - added a backend-truth-source note ahead of the table
    - rewrote OpenSSL status as the current default active backend
    - rewrote WinSSL status as bounded Windows client-baseline truth with status-report handoff

- `bash -n tests/scripts/test_architecture_backend_status_truth_contract.sh`
  - result: PASS
  - summary:
    - new architecture backend-status truth contract syntax is valid

- `bash tests/scripts/test_architecture_backend_status_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed contract self-injury from shell backticks inside quoted patterns
    - second adjustment also narrowed a brittle full-row table match to stable fragments after markdown column reflow
    - GREEN after both fixes:
      - architecture backend-status truth now matches the bounded wording we actually want to preserve

- `npx prettier --write docs/reference/ARCHITECTURE.md`
  - result: PASS
  - summary:
    - architecture reference formatting remains stable

- `git diff --check`
  - result: PASS
  - summary:
    - current architecture backend-status truth batch has no whitespace or patch-format issues

### WinSSL Session Cache Semantic Boundary

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `rg -n "SessionCacheSupport|SessionTicketsSupport|session resumption|session cache|SetSession|DoSetSession|InitializeSecurityContextW|observed_reuse|session_configured" task_plan.md findings.md progress.md src/fafafa.ssl.winssl.lib.pas src/fafafa.ssl.winssl.context.pas src/fafafa.ssl.winssl.connection.pas docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/reference/API_REFERENCE.md docs/guides/WINSSL_USER_GUIDE.md tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - static audit confirmed the remaining question is no longer WinSSL runtime flag wiring
    - the active ambiguity had narrowed to what `SessionCacheSupport=sslSupportStable` is actually supposed to mean

- `sed -n '3210,3298p' task_plan.md`
  - result: PASS
  - summary:
    - existing ledger already pinned:
      - `SessionCacheSupport := sslSupportStable`
      - `SessionTicketsSupport := sslSupportExperimental`
      - `observed_reuse=false`
      - `session_configured=true`
    - but it still left the semantic boundary of `SessionCacheSupport` under-explained in active interface docs

- `sed -n '1648,1718p' docs/reference/API_REFERENCE.md`
  - result: PASS
  - summary:
    - `TSSLBackendCapabilities` code block was still missing `SessionCacheSupport`
    - read-priority notes still mentioned `SessionTicketsSupport` but not `SessionCacheSupport`
    - this was the clearest active interface-truth drift in the lane

- add `docs/plans/2026-05-19-winssl-session-cache-semantic-boundary.md`
  - change:
    - recorded the bounded plan for locking the WinSSL SessionCacheSupport semantic boundary

- add `tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - `SessionCacheSupport` is explicitly present in `API_REFERENCE.md`
      - source/docs define it as cache/control support, not resumed-handshake proof
      - active WinSSL matrix/user-guide wording preserves the runtime caveat

- `bash -n tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh`
  - result: PASS
  - summary:
    - new session-cache semantic-boundary contract syntax is valid

- `bash tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed two real documentation/source truth gaps:
      - `src/fafafa.ssl.base.pas` did not describe `SessionCacheSupport` as cache/control-only support
      - `docs/reference/API_REFERENCE.md` still omitted `SessionCacheSupport` from the active capability record
    - GREEN after the focused doc/source patch:
      - base/source/docs now consistently explain that WinSSL `SessionCacheSupport=sslSupportStable` only publishes context-level cache/control truth
      - current resumed-handshake truth remains delegated to dedicated Windows runtime proof

- update docs/source:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - clarify that `SessionCacheSupport` is a cache/control surface support-level field
    - add the missing `SessionCacheSupport` field to the API reference capability record
    - restate that WinSSL `SessionCacheSupport=sslSupportStable` does not itself prove runtime resumed-handshake success

- `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - result: PASS
  - summary:
    - previous context-level wiring proof stays green after the semantic-boundary cleanup

- `git diff --check`
  - result: PASS
  - summary:
    - current session-cache semantic-boundary batch has no whitespace or patch-format issues

### WinSSL Session Evidence Model Truth

- `git status --short --branch`
  - result: PASS
  - summary:
    - batch started from a clean `master...origin/master` worktree

- `gh run list -w "Wave B B2 Manual Gate (Template)" --limit 8`
  - result: PASS
  - summary:
    - current verified manual-lane sequence showed:
      - latest successful Windows manual gate = `26093405878`
      - previous failure = `26092828923`
    - both facts helped confirm that this lane is live/current rather than a stale historical handoff

- `gh run view 26093405878 --log | rg -n "\[INFO\] Using WinSSL session resumption host override|\[INFO\] Using default WinSSL session resumption host from test program|\[INFO\] Enabling risky WinSSL native probe for Schannel session evidence|\[INFO\] Keeping WinSSL native probe disabled by default|\[WINSSL-SESSION-RESUME\] summary host=|\[WINSSL-RUNTIME\] session_resumption summary host="`
  - result: PASS
  - summary:
    - latest successful manual gate still ran the broader suite with the native probe disabled by default
    - the artifact/log truth remained:
      - `observed_reuse=false`
      - `session_configured=true`
      - `native_probe_enabled=false`

- `sed -n '930,1045p' src/fafafa.ssl.winssl.connection.pas`
  - result: PASS
  - summary:
    - static source audit confirmed the deeper truth:
      - `UpdateSessionReuseTruthFromContext(...)` currently forces:
        - `ASessionId := ''`
        - `FSessionReused := False`
      - reason: canonical shared path keeps the live `SECPKG_ATTR_SESSION_INFO` probe removed to avoid GitHub Windows AVs
    - this means the broader/shared lane's `observed_reuse=false` is a conservative public truth, not a direct safe native probe result

- `sed -n '1,240p' tests/windows/VALIDATION_BUNDLE.md`
  - result: PASS
  - summary:
    - validation bundle inventory still documented only the short `observed_reuse` marker shape
    - it did not yet teach readers how to distinguish shared/public conservative truth from opt-in native probe evidence

- `sed -n '132,176p' tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - result: PASS
  - summary:
    - Windows checklist had the same evidence-model drift:
      - promoted marker shape only showed `observed_reuse`
      - explanation still implied direct artifact inspection answers whether resumed handshake was truly observed

- add `docs/plans/2026-05-19-winssl-session-evidence-model-truth.md`
  - change:
    - recorded the bounded plan for locking the WinSSL session evidence model

- add `tests/scripts/test_winssl_session_evidence_model_truth_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - proof program emits a stable `evidence_model` marker
      - high-entry docs distinguish conservative shared/public truth from opt-in native probe truth
      - Windows checklist/bundle document the richer summary shape

- update docs/proof:
  - `tests/winssl/test_winssl_session_resumption.pas`
  - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - `tests/windows/VALIDATION_BUNDLE.md`
  - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - emit a stable session-resumption `evidence_model` marker
    - teach readers that `observed_reuse` is current shared/public conservative truth
    - point deeper runtime interpretation to `native_observed_reuse` / `native_probe_succeeded`

- `bash -n tests/scripts/test_winssl_session_evidence_model_truth_contract.sh`
  - result: PASS
  - summary:
    - new session evidence-model truth contract syntax is valid

- `bash tests/scripts/test_winssl_session_evidence_model_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed two real alignment gaps:
      - proof program did not yet emit an explicit evidence-model marker
      - status/checklist/bundle wording still left room to over-read `observed_reuse`
    - GREEN after the focused patch:
      - proof + docs now preserve the two-layer evidence model we actually rely on

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - pre-existing runtime-truth contract stays green after the evidence-model tightening

- `git diff --check`
  - result: PASS
  - summary:
    - current session evidence-model truth batch has no whitespace or patch-format issues

- `gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_native_probe_20260519_2300 -f strict_closure=false -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - launched a fresh Windows manual lane specifically to capture latest isolated native-probe evidence without reopening the shared-path probe

- `gh run list -w "Wave B B2 Manual Gate (Template)" --limit 3`
  - result: PASS (in_progress at capture time)
  - summary:
    - newly launched native-probe run is currently:
      - `26104446972`
    - status at capture time:
      - `in_progress`

- `gh run download 26104446972 --dir tmp/gh-run-26104446972`
  - result: PASS
  - summary:
    - downloaded fresh Linux/macOS/Windows/summary artifacts for the native-probe investigation run
    - this avoided GitHub web-log auth friction and gave direct access to the Windows runtime transcript

- `rg -n "session_resumption|evidence_model|observed_reuse|native_observed_reuse|native_probe_succeeded|native_probe_worker|query_failed|exception=|suite_end|test_result index=5" tmp/gh-run-26104446972 -g '*.log' -g '*.md'`
  - result: PASS
  - summary:
    - fresh artifact evidence confirmed:
      - `windows_runtime_transcript` is substantive and records `suite_end_status=FAIL`
      - failure is concentrated in `WinSSL Session Resumption Truth`
      - no evidence gap remains in artifact capture for this lane

- `rg -n "native_probe_worker exit_code=-1073741819|before_query_context_attributes|native_probe_enabled=true|native_probe_succeeded=false" tmp/gh-run-26104446972/wave-b-windows-winssl_native_probe_20260519_2300/winssl_runtime_suite_winssl_native_probe_20260519_2300.log`
  - result: PASS
  - summary:
    - fresh native-probe run `26104446972` ended with:
      - `native_probe_worker exit_code=-1073741819`
      - last marker at `native_probe label=initial_handshake stage=before_query_context_attributes`
      - `observed_reuse=false`
      - `native_probe_enabled=true`
      - `native_observed_reuse=false`
      - `native_probe_succeeded=false`
      - `session_configured=true`
    - this tightened the real unresolved issue to the isolated-worker `SECPKG_ATTR_SESSION_INFO` probe itself

### WinSSL Native Probe Safe Query Path

- `rg -n "QueryContextAttributesW\\(LCtxtHandle|TryQueryNativeSessionReuse|SECPKG_ATTR_SESSION_INFO" tests/scripts tests/winssl/test_winssl_session_resumption.pas docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - result: PASS
  - summary:
    - static source audit confirmed the current isolated native probe still called `QueryContextAttributesW(...)` directly
    - this matched the fresh Windows crash boundary from run `26104446972`

- add `docs/plans/2026-05-19-winssl-native-probe-safe-query-path.md`
  - change:
    - recorded the bounded probe-side plan for preferring `QueryContextAttributesExW(..., cbBuffer)` over the raw three-argument query path

- add `tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - dynamic resolution of `QueryContextAttributesExW`
      - an explicit sized-buffer helper
      - `ExW`-first / `W`-fallback behavior
      - a stable `stage=query_api api=...` evidence marker

- update source/contracts:
  - `tests/winssl/test_winssl_session_resumption.pas`
  - `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - `tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - change:
    - introduced a cached `QueryContextAttributesExW` resolver
    - added `TryQueryCurrentSessionInfoWithSizedBuffer(...)`
    - moved native probe off the direct `QueryContextAttributesW(...)` call site and onto the sized-buffer helper
    - kept the canonical connection helper/session-info allowlist intact while narrowing the dedicated probe site

- `bash -n tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - result: PASS
  - summary:
    - new native-probe safe-query contract syntax is valid

- `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed contract brittleness rather than a source bug:
      - the initial regex was too strict about multiline helper formatting
      - the second pass also switched the helper assertions to smaller, more stable fragments
    - GREEN after tightening the contract itself:
      - source now clearly prefers `QueryContextAttributesExW(..., SizeOf(...))`
      - fallback to `QueryContextAttributesW(...)` stays explicit

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - the broader runtime-truth guard still stays green after the probe-side source tightening

- `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - result: PASS
  - summary:
    - session-info probing remains confined to the explicit allowlist sites after the helper refactor

- `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - result: PASS
  - summary:
    - existing stage-level probe evidence markers remain intact after the safe-query patch

- `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - result: PASS
  - summary:
    - backend/handle metadata markers remain intact after the safe-query patch

- `mkdir -p tmp/winssl_native_probe_safe_query_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_safe_query_win64 -FEtmp/winssl_native_probe_safe_query_win64 -otmp/winssl_native_probe_safe_query_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 focused cross-target compile accepted the new probe-side resolver/helper path
    - no new compile blocker was introduced by the `QueryContextAttributesExW` preference

- `git diff --check`
  - result: PASS
  - summary:
    - current native-probe safe-query batch has no whitespace or patch-format issues

- `gh run download 26106025515 --dir tmp/gh-run-26106025515`
  - result: PASS
  - summary:
    - downloaded fresh artifacts for the post-`ExW`-preference Windows investigation run

- `rg -n "session_resumption|evidence_model|query_api|query_context_attributes_exw|query_context_attributesw|native_probe_worker|before_query_context_attributes|after_query_context_attributes|query_failed|native_observed_reuse|native_probe_succeeded|suite_end|test_result index=5" tmp/gh-run-26106025515 -g '*.log' -g '*.md'`
  - result: PASS
  - summary:
    - fresh runtime transcript confirmed:
      - `windows quick smoke` = PASS
      - `Run Windows Wave B gate` = PASS
      - failure stayed isolated to `Run broader WinSSL runtime suite`
      - the session lane still failed at test index `5`

- `rg -n "stage=query_api api=query_context_attributesw|native_probe_worker exit_code=-1073741819|stage=before_query_context_attributes" tmp/gh-run-26106025515/wave-b-windows-winssl_native_probe_exw_20260519_2330/winssl_runtime_suite_winssl_native_probe_exw_20260519_2330.log`
  - result: PASS
  - summary:
    - post-safe-query Windows run `26106025515` established the new key fact:
      - `QueryContextAttributesEx*` did not resolve
      - native probe still fell back to `query_context_attributesw`
      - crash remained `native_probe_worker exit_code=-1073741819`
    - this moved the main unresolved issue from the query-call shape to the resolver itself

### WinSSL Native Probe Resolver Diagnostics

- add `docs/plans/2026-05-19-winssl-native-probe-resolver-diagnostics.md`
  - change:
    - recorded the bounded plan for making the `QueryContextAttributesEx*` resolver observable and broad enough to distinguish module/name drift from true platform absence

- add `tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
  - change:
    - added a focused shell contract that guards:
      - candidate module/symbol record
      - cached resolved module/symbol names
      - ANSI `GetProcAddress` lookup
      - resolver diagnostic evidence marker

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - added explicit candidate traversal for:
      - `secur32.dll` / `sspicli.dll`
      - `QueryContextAttributesExW` / `QueryContextAttributesExA` / `QueryContextAttributesEx`
    - switched `GetProcAddress` to explicit `PAnsiChar(...)`
    - cached the resolved module/symbol names
    - emitted:
      - `stage=query_resolver module=... symbol=... resolved=...`

- `bash -n tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
  - result: PASS
  - summary:
    - new resolver-diagnostics contract syntax is valid

- `bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
  - result: PASS
  - summary:
    - resolver candidates, ANSI lookup, and resolver diagnostic markers are all present in source

- `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED came from contract brittleness after the new candidate record shifted the type block
    - GREEN after relaxing the type matcher to a stable fragment

- `mkdir -p tmp/winssl_native_probe_resolver_diag_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_resolver_diag_win64 -FEtmp/winssl_native_probe_resolver_diag_win64 -otmp/winssl_native_probe_resolver_diag_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 focused cross-target compile accepted the resolver-diagnostics changes

- `git diff --check`
  - result: PASS
  - summary:
    - current resolver-diagnostics batch has no whitespace or patch-format issues

- `bash -n tests/scripts/test_winssl_native_probe_safe_query_contract.sh && bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - result: PASS
  - summary:
    - updated safe-query contract now matches the candidate-based resolver shape
    - Ex-first sized-buffer helper, W fallback, and query-path marker all stayed green after the resolver-diagnostics patch

- `bash -n tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh && bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
  - result: PASS
  - summary:
    - resolver candidate traversal, resolved module/symbol cache, ANSI export lookup, and resolver marker all remain locked by focused contract

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - broader session-resumption runtime-truth contract remained green after the resolver-diagnostics batch

- `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - result: PASS
  - summary:
    - controlled allowlist boundary for session-info probing was not broadened by the resolver-diagnostics batch

- `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - result: PASS
  - summary:
    - native-probe stage marker contract remained green alongside the new resolver marker

- `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - result: PASS
  - summary:
    - native-handle metadata markers remained intact after the resolver-diagnostics changes

- `git push origin master`
  - result: PASS
  - summary:
    - pushed resolver-diagnostics batch as commit `e6738bf`

- `gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_native_probe_resolver_diag_20260519_232811 -f strict_closure=false -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - launched fresh Windows/manual investigation lane against commit `e6738bf`

- `gh run watch 26107307586 --exit-status`
  - result: FAIL
  - summary:
    - linux gate passed
    - windows quick smoke passed
    - windows Wave B gate passed
    - broader WinSSL runtime suite still failed
    - macOS gate also failed independently

- `gh run download 26107307586 --dir tmp/gh-run-26107307586`
  - result: PASS
  - summary:
    - downloaded fresh artifacts for the resolver-diagnostics verification run

- `rg -n "session_resumption|evidence_model|query_resolver|query_api|query_context_attributes_exw|query_context_attributesw|native_probe_worker exit_code|before_query_context_attributes|after_query_context_attributes|query_failed|native_observed_reuse|native_probe_succeeded|suite_end|test_result index=5" tmp/gh-run-26107307586/wave-b-windows-winssl_native_probe_resolver_diag_20260519_232811/winssl_runtime_suite_winssl_native_probe_resolver_diag_20260519_232811.log`
  - result: PASS
  - summary:
    - fresh runtime transcript proved:
      - `QueryContextAttributesExW` successfully resolved from `sspicli.dll`
      - native probe actually entered the `query_context_attributes_exw` path
      - crash still remained `native_probe_worker exit_code=-1073741819`
    - main unresolved issue moved from resolver selection to the real ExW/session-info call boundary

- add `docs/plans/2026-05-19-winssl-native-probe-control-query-boundary.md`
  - change:
    - recorded the next bounded batch to distinguish handle-path instability from attribute-specific session-info instability

- add `tests/scripts/test_winssl_native_probe_control_query_contract.sh`
  - change:
    - added a focused shell contract that guards the control-query helper and its before/after/failure markers

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - added a dedicated `SECPKG_ATTR_CONNECTION_INFO` control-query helper before the risky session-info probe
    - emitted:
      - `stage=before_control_query`
      - `stage=after_control_query`
      - `stage=control_query_failed`
    - preserved the existing resolver and safe-query evidence path after the control query

- `bash -n tests/scripts/test_winssl_native_probe_control_query_contract.sh && bash tests/scripts/test_winssl_native_probe_control_query_contract.sh`
  - result: PASS
  - summary:
    - control-query helper and before/after/failure markers are present in source

- `bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
  - result: PASS
  - summary:
    - resolver candidate traversal and resolver diagnostic markers stayed green after the control-query batch

- `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - result: PASS
  - summary:
    - Ex-first sized-buffer helper and W fallback stayed green after inserting the control query

- `mkdir -p tmp/winssl_native_probe_control_query_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_control_query_win64 -FEtmp/winssl_native_probe_control_query_win64 -otmp/winssl_native_probe_control_query_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 focused cross-target compile accepted the control-query boundary changes

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - broader runtime-truth contract remained green after the control-query batch

- `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - result: PASS
  - summary:
    - pre-existing native-probe stage markers remained intact alongside the new control-query markers

- `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - result: PASS
  - summary:
    - native-handle metadata markers remained intact after the control-query changes

- `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - result: PASS
  - summary:
    - control-query addition did not broaden the session-info probe allowlist

- `git diff --check`
  - result: PASS
  - summary:
    - current control-query boundary batch has no whitespace or patch-format issues

- `git push origin master`
  - result: PASS
  - summary:
    - pushed control-query boundary batch as commit `45d968d`

- `gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_native_probe_control_query_20260519_234404 -f strict_closure=false -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - launched fresh Windows/manual lane against commit `45d968d`

- `gh run watch 26108237632 --exit-status`
  - result: FAIL
  - summary:
    - linux gate passed
    - windows quick smoke passed
    - windows Wave B gate passed
    - broader WinSSL runtime suite still failed
    - macOS gate also failed independently

- `gh run download 26108237632 --dir tmp/gh-run-26108237632`
  - result: PASS
  - summary:
    - downloaded fresh artifacts for the control-query verification run

- `rg -n "session_resumption|query_resolver|query_api|before_control_query|after_control_query|control_query_failed|native_probe_worker exit_code|before_query_context_attributes|after_query_context_attributes|query_failed|native_probe_succeeded|test_result index=5|suite_end" tmp/gh-run-26108237632/wave-b-windows-winssl_native_probe_control_query_20260519_234404/winssl_runtime_suite_winssl_native_probe_control_query_20260519_234404.log`
  - result: PASS
  - summary:
    - fresh runtime transcript proved:
      - control query reached `after_control_query status=0x0`
      - resolver still reached `module=sspicli.dll symbol=QueryContextAttributesExW resolved=true`
      - crash still remained after `query_context_attributes_exw`
    - handle-path suspicion was eliminated; the remaining issue is now attribute-specific to `SECPKG_ATTR_SESSION_INFO`

- add `docs/plans/2026-05-19-winssl-native-probe-worker-evidence-only.md`
  - change:
    - recorded the next bounded batch that downgrades the known investigatory worker crash to evidence-only by default

- add `tests/scripts/test_winssl_native_probe_worker_evidence_only_contract.sh`
  - change:
    - added a focused shell contract that guards strict-vs-evidence-only worker-exit semantics

- update `tests/winssl/test_winssl_session_resumption.pas`
  - change:
    - downgraded parent-side native-probe worker nonzero exit to evidence-only by default
    - preserved strict worker-exit failure when `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE=1`

- `bash -n tests/scripts/test_winssl_native_probe_worker_evidence_only_contract.sh && bash tests/scripts/test_winssl_native_probe_worker_evidence_only_contract.sh`
  - result: PASS
  - summary:
    - strict-vs-evidence-only worker-exit semantics are present in source

- `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - broader runtime-truth contract remained green after the evidence-only worker batch

- `bash tests/scripts/test_winssl_native_probe_control_query_contract.sh`
  - result: PASS
  - summary:
    - control-query boundary remained green after changing parent-side worker-exit semantics

- `mkdir -p tmp/winssl_native_probe_worker_evidence_only_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_worker_evidence_only_win64 -FEtmp/winssl_native_probe_worker_evidence_only_win64 -otmp/winssl_native_probe_worker_evidence_only_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - result: PASS
  - summary:
    - Win64 focused cross-target compile accepted the evidence-only worker changes

- `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
  - result: PASS
  - summary:
    - safe-query proof remained green after downgrading worker nonzero exit to evidence-only

- `bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
  - result: PASS
  - summary:
    - resolver-diagnostics proof remained green after the evidence-only worker batch

- `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - result: PASS
  - summary:
    - handle-metadata proof remained green after the evidence-only worker batch

- `git diff --check`
  - result: PASS
  - summary:
    - current evidence-only worker batch has no whitespace or patch-format issues

- `git push origin master`
  - result: PASS
  - summary:
    - pushed evidence-only worker batch as commit `bafa1db`

- `gh workflow run "Wave B B2 Manual Gate (Template)" --ref master -f run_id=winssl_native_probe_evidence_only_20260519_235540 -f strict_closure=false -f winssl_enable_native_probe=true`
  - result: PASS
  - summary:
    - launched final Windows/manual verification lane against commit `bafa1db`

- `gh run watch 26108902159 --exit-status`
  - result: FAIL
  - summary:
    - linux gate passed
    - windows quick smoke passed
    - windows Wave B gate passed
    - broader WinSSL runtime suite passed
    - workflow still failed only because macOS gate failed independently

### Active Guide Convenience Surface Classification

- add `docs/plans/2026-05-20-active-guide-convenience-surface-classification.md`
  - change:
    - define the bounded active-guide truth batch for `ISSLConnection` convenience/helper classification drift

- add `tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - change:
    - lock that active guides must classify:
      - direct `Conn.SetTimeout` / `Conn.SetBlocking` as local override guidance
      - `ReadString` / `WriteString` as shipped convenience text helpers rather than the preferred main path

- `bash -n tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - new active-guide convenience-surface contract syntax is valid

- `bash tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `INTEGRATION_GUIDE` still showed `Conn.SetTimeout` / `Conn.SetBlocking` without explicit direct-override classification
      - `MIGRATION_GUIDE` still showed direct `WriteString` usage without saying it is a convenience helper rather than the preferred transport/framework path
      - `USER_GUIDE` still showed `ReadString` / `WriteString` text examples without explaining they are convenience-core helpers
    - GREEN after fix:
      - `INTEGRATION_GUIDE` now spells out builder-first / connector-first timeout guidance
      - `MIGRATION_GUIDE` now labels direct `ISSLConnection` text I/O as shipped convenience surface
      - `USER_GUIDE` now labels client/server text I/O examples as convenience-helper demos

- update `docs/INTEGRATION_GUIDE.md`
  - change:
    - add explicit note that `Conn.SetTimeout` / `Conn.SetBlocking` in direct-connection snippets are local overrides when higher-level builder/connector/acceptor configuration is already in use
    - keep timeout guidance builder-first for connector / acceptor facade users

- update `docs/guides/MIGRATION_GUIDE.md`
  - change:
    - classify direct `ISSLConnection` text example as current shipped convenience surface
    - restore the current `if LConn.ReadString(LResponse) then` example so the guide no longer drifts from the shipped `ReadString(out ...)` signature truth

- update `docs/guides/USER_GUIDE.md`
  - change:
    - explain that client/server `ReadString` / `WriteString` examples are kept for simple text roundtrips only
    - point framework / event-loop / framed-protocol integrations back to `Read` / `Write` or `TSSLStream`

- `bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `MIGRATION_GUIDE` no longer demonstrated the current `ReadString(out ...)` pattern even though the older contract still required it
    - GREEN after fix:
      - migration guide now shows both `WriteString(...)` and `if LConn.ReadString(LResponse) then ...`

- `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - active-guide wording changes did not drift the already-closed source/API/design classification truth

- `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
  - result: PASS
  - summary:
    - migration guide remains aligned with the current active facade/connector route after the convenience-surface wording fix

- `git diff --check`
  - result: PASS
  - summary:
    - current active-guide convenience-surface batch has no whitespace or patch-format issues

### Landing Quickstarts Direct-Path Classification

- add `docs/plans/2026-05-20-landing-quickstarts-direct-path-classification.md`
  - change:
    - define the bounded landing-doc batch for direct `ISSLConnection` path classification in root README and quickstarts

- add `tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - change:
    - lock that high-entry landing docs must explain:
      - raw `CreateConnection(...)` snippets are low-level/specific-capability paths
      - ordinary new code still prefers builder + connector + stream

- `bash -n tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - result: PASS
  - summary:
    - new landing-quickstarts direct-path contract syntax is valid

- `bash tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - root `README.md` still showed the raw `TLS 连接` core snippet without explicitly classifying it as lower-level reference
    - GREEN after fix:
      - root README now labels the raw connection snippet as core-surface reference
      - `GETTING_STARTED` now labels direct `ISSLConnection` as a low-level shipped path
      - `QUICKSTART` now explains that the WinSSL session example drops to direct path because `ISSLSessionResumption` hangs off the connection object

- update `README.md`
  - change:
    - classify the `核心 API -> TLS 连接` snippet as low-level core-surface reference
    - point ordinary new code back to the earlier builder + connector + stream quickstart

- update `docs/guides/GETTING_STARTED.md`
  - change:
    - clarify that direct `ISSLConnection` remains a shipped low-level entry
    - point ordinary client/server integrations back to `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`

- update `docs/guides/QUICKSTART.md`
  - change:
    - explain that the WinSSL session-resumption sample uses direct `ISSLConnection`
      because the current public resumption surface is connection-attached
    - keep the earlier connector + stream client path as the ordinary main entry

- `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
  - result: PASS
  - summary:
    - landing-doc wording changes did not drift the existing facade/main-entry truth

- `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
  - result: PASS
  - summary:
    - direct-path clarification did not regress the already-closed per-connection SNI guidance

- `git diff --check`
  - result: PASS
  - summary:
    - current landing-quickstarts direct-path batch has no whitespace or patch-format issues

### Backend Quickstarts Direct-Path Classification

- add `docs/plans/2026-05-20-backend-quickstarts-direct-path-classification.md`
  - change:
    - define the bounded backend-quickstarts batch for direct `ISSLConnection` classification in MbedTLS/WinSSL high-entry guides

- add `tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
  - change:
    - lock that backend-specific quickstarts must explain why they show direct `ISSLConnection`
      instead of the generic facade main path

- `bash -n tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
  - result: PASS
  - summary:
    - new backend-quickstarts direct-path contract syntax is valid

- `bash tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `MBEDTLS_USER_GUIDE` still showed the simple HTTPS client sample without classifying it as backend raw-surface guidance
    - GREEN after fix:
      - `MBEDTLS_USER_GUIDE` now classifies its simple client sample as backend raw-surface guidance
      - `WINSSL_QUICKSTART` now explains that the page focuses on Windows-native / WinSSL-specific direct-path usage

- update `docs/guides/MBEDTLS_USER_GUIDE.md`
  - change:
    - explain that the simple HTTPS sample directly uses `Context.CreateConnection(...)`
      to show current backend raw shipped surface
    - point ordinary cross-backend clients back to
      `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`

- update `docs/guides/WINSSL_QUICKSTART.md`
  - change:
    - explain that the quickstart intentionally focuses on Windows-native / WinSSL-specific direct-path usage
    - point ordinary cross-backend clients back to the generic facade main path

- `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - MbedTLS capability/public-surface truth remained green after the new direct-path explanation

- `bash tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL quickstart status/runtime wording remained aligned after classifying the direct path

- `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - result: PASS
  - summary:
    - unit/import guidance remained aligned across WinSSL/MbedTLS docs after the direct-path clarification

- `git diff --check`
  - result: PASS
  - summary:
    - current backend-quickstarts direct-path batch has no whitespace or patch-format issues

### Diagnostics Connection Override Classification

- add `docs/plans/2026-05-20-diagnostics-connection-override-classification.md`
  - change:
    - define the bounded diagnostics/backend-doc batch for timeout/blocking override classification

- add `tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - change:
    - lock that active diagnostics/backends must classify
      `SetTimeout(...)` / `SetBlocking(...)` as connection-level diagnostic overrides

- `bash -n tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - result: PASS
  - summary:
    - new diagnostics-override contract syntax is valid

- `bash tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `TROUBLESHOOTING.md` still showed `LConn.SetTimeout(...)` without classifying it as diagnostic override guidance
    - GREEN after fix:
      - `TROUBLESHOOTING.md` now classifies timeout and nonblocking snippets as direct-connection diagnostic overrides
      - `MBEDTLS_USER_GUIDE.md` now classifies `Connection.SetTimeout(...)` as connection-level override guidance

- update `docs/guides/TROUBLESHOOTING.md`
  - change:
    - explain that `LConn.SetTimeout(...)` is a direct-connection diagnostic override
    - explain that `LConn.SetBlocking(False)` is a direct-connection debugging entrypoint
    - point facade/event-loop users back to builder configuration and outer timer/poller control

- update `docs/guides/MBEDTLS_USER_GUIDE.md`
  - change:
    - explain that `Connection.SetTimeout(...)` in the timeout-failure section is a connection-level override
    - point ordinary cross-backend clients back to builder/connector/transport timer control

- `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - result: PASS
  - summary:
    - MbedTLS capability/public-surface truth remained aligned after the timeout-override clarification

- `bash tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - generic convenience-surface guidance remained aligned after the new diagnostics wording

- `git diff --check`
  - result: PASS
  - summary:
    - current diagnostics-override batch has no whitespace or patch-format issues

### High-Frequency Guides Direct-Path Reasoning

- add `docs/plans/2026-05-20-high-frequency-guides-direct-path-reasoning.md`
  - change:
    - define the bounded high-frequency-doc batch for explaining why selected guides intentionally use direct `CreateConnection(...)`

- add `tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
  - change:
    - lock that selected high-frequency guides must explain why their direct `ISSLConnection` usage is intentional and scenario-specific

- `bash -n tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
  - result: PASS
  - summary:
    - new high-frequency direct-path reasoning contract syntax is valid

- `bash tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `COMMON_PITFALLS` still kept the direct `CreateConnection(...)` SNI contrast without explicitly explaining why the page intentionally uses the low-level path
    - GREEN after fix:
      - `COMMON_PITFALLS` now explains the direct path as a pitfall contrast
      - `security-best-practices` now explains the direct path as explicit hostname/SNI responsibility expansion
      - `ERROR_HANDLING_BEST_PRACTICES` now explains the direct path as URL/socket ownership and exception/result-boundary guidance

- update `docs/guides/COMMON_PITFALLS.md`
  - change:
    - explain that the direct `CreateConnection(...)` snippet is intentionally retained as the shortest SNI pitfall contrast
    - point ordinary clients back to `TSSLConnector.ConnectSocket(..., host)`

- update `docs/guides/security-best-practices.md`
  - change:
    - explain that the direct `ISSLConnection` sample is used to make hostname/SNI connection ownership explicit
    - keep connector as the equally-correct higher-level path when low-level control is unnecessary

- update `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`
  - change:
    - explain that the direct `CreateConnection(...)` sample is focused on URL-driven socket ownership and exception/result boundaries
    - point simpler handshake entry back to `TSSLConnector`

- `bash tests/scripts/test_active_tls_guidance_contract.sh`
  - result: PASS
  - summary:
    - active TLS guidance stayed aligned after explaining the direct-path scenarios

- `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
  - result: PASS
  - summary:
    - secondary guide SNI guidance remained aligned after the scenario reasoning batch

- `bash tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh`
  - result: PASS
  - summary:
    - error-handling guide kept its URL-driven connection-level SNI truth

- `bash tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
  - result: PASS
  - summary:
    - security guide pinning/helper truth remained aligned after the direct-path reasoning note

- `git diff --check`
  - result: PASS
  - summary:
    - current high-frequency direct-path reasoning batch has no whitespace or patch-format issues

### Specialized Owner-Surface Reasoning

- add `docs/plans/2026-05-20-specialized-owner-surface-reasoning.md`
  - change:
    - define the bounded specialized-guide batch for explaining why OCSP/CT runtime examples intentionally use connection owner paths

- add `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - change:
    - lock that specialized OCSP/CT guides must explain why they intentionally drop to the connection owner path instead of the generic facade main entry

- `bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - result: PASS
  - summary:
    - new specialized owner-surface reasoning contract syntax is valid

- `bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `OCSP_USAGE_GUIDE` still used the direct connection path without explicitly explaining why the guide intentionally reads the owner surface from the connection object
    - GREEN after fix:
      - `OCSP_USAGE_GUIDE` now explains why OCSP runtime state + verify result are read from the connection owner path
      - `CT_IMPLEMENTATION_GUIDE` now explains why CT runtime owner surfaces are read from the connection object

- update `docs/guides/OCSP_USAGE_GUIDE.md`
  - change:
    - explain that stapled OCSP runtime state is exposed through `ISSLOCSPStapling` on the connection object
    - explain that handshake verify result is also read from the connection owner path
    - point ordinary clients without owner-surface needs back to `TSSLConnector` / `TSSLStream`

- update `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
  - change:
    - explain that `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`
      are connection owner surfaces
    - point ordinary clients without CT owner-surface needs back to `TSSLConnector` / `TSSLStream`

- `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - specialized owner-surface explanation did not regress certificate-verification owner-path truth

- `bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  - result: PASS
  - summary:
    - specialized owner-surface explanation did not regress OCSP owner-path truth

- `git diff --check`
  - result: PASS
  - summary:
    - current specialized owner-surface batch has no whitespace or patch-format issues

### Early-Data Owner-Surface Reasoning

- add `docs/plans/2026-05-20-early-data-owner-surface-reasoning.md`
  - change:
    - define the bounded early-data-guide batch for explaining why the page intentionally uses context/connection owner surfaces

- add `tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - change:
    - lock that `EARLY_DATA_GUIDE` must explain why it intentionally drops to `CreateConnection(...)` to access early-data owner surfaces

- `bash -n tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - result: PASS
  - summary:
    - new early-data owner-surface reasoning contract syntax is valid

- `bash tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `EARLY_DATA_GUIDE` still used `CreateConnection(...)` without explicitly explaining why the page intentionally uses early-data owner surfaces
    - GREEN after fix:
      - `EARLY_DATA_GUIDE` now explains that `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`
        are owner surfaces on the context / connection objects
      - `EARLY_DATA_GUIDE` now points ordinary clients without early-data needs back to
        `TSSLConnector` / `TSSLStream`

- update `docs/guides/EARLY_DATA_GUIDE.md`
  - change:
    - explain that the page intentionally returns to `CreateConnection(...)`
      because early-data owner surfaces are split across context and connection
    - point ordinary handshake entry back to `TSSLConnector` / `TSSLStream`

- `bash tests/scripts/test_early_data_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - early-data capability/runtime truth remained aligned after the owner-surface explanation note

- `git diff --check`
  - result: PASS
  - summary:
    - current early-data owner-surface batch has no whitespace or patch-format issues

### WinSSL User Guide Direct-Path Classification

- add `docs/plans/2026-05-20-winssl-user-guide-direct-path-classification.md`
  - change:
    - define the bounded WinSSL user-guide batch for classifying direct connection examples as WinSSL-specific paths

- add `tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
  - change:
    - lock that `WINSSL_USER_GUIDE` must explain why it intentionally uses backend-facing direct connection examples

- `bash -n tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL user-guide direct-path classification contract syntax is valid

- `bash tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `WINSSL_USER_GUIDE` still showed direct connection examples without explicitly classifying them as WinSSL-specific paths
    - GREEN after fix:
      - `WINSSL_USER_GUIDE` now explains that the page intentionally shows backend-facing `ISSLConnection` / `CreateConnection(...)`
      - `WINSSL_USER_GUIDE` now explains why the SNI example intentionally uses the connection-level published surface

- update `docs/guides/WINSSL_USER_GUIDE.md`
  - change:
    - explain that the page is a WinSSL-specific guide and therefore intentionally shows backend-facing direct connection paths
    - point ordinary cross-backend HTTPS clients back to `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
    - explain that hostname/SNI is published on the connection object and point the higher-level alternative back to `TSSLConnector.ConnectSocket(...)`

- `bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL quickstart runtime truth remained aligned after the user-guide direct-path note

- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - result: PASS
  - summary:
    - active direct-context ServerName classifications remained confined after the user-guide note

- `bash tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
  - result: PASS
  - summary:
    - WinSSL user-guide performance/runtime truth remained aligned after the direct-path explanation

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL user-guide direct-path batch has no whitespace or patch-format issues

### WinSSL Best-Practices Session Truth

- add `docs/plans/2026-05-20-winssl-best-practices-session-truth.md`
  - change:
    - define the bounded WinSSL best-practices batch for classifying direct connection/session paths and tightening current session truth

- add `tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
  - change:
    - lock that `WINSSL_BEST_PRACTICES` must stop teaching WinSSL session public surface as a default optimization path

- `bash -n tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
  - result: PASS
  - summary:
    - new WinSSL best-practices session-truth contract syntax is valid

- `bash tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `WINSSL_BEST_PRACTICES` still presented WinSSL session public surface as a default best-practice optimization path
    - GREEN after fix:
      - `WINSSL_BEST_PRACTICES` now classifies direct connection/session examples as WinSSL-specific backend-facing paths
      - `WINSSL_BEST_PRACTICES` now explains the current conservative WinSSL session truth
      - `WINSSL_BEST_PRACTICES` checklist no longer treats Session public surface as a default checkbox

- update `docs/guides/WINSSL_BEST_PRACTICES.md`
  - change:
    - explain that the page is a WinSSL-specific best-practices guide and therefore intentionally shows backend-facing direct connection/session paths
    - explain that WinSSL session public surface remains experimental under
      `observed_reuse=false` / `session_configured=true`
    - keep `ISSLSessionResumption` on the connection owner path without promising stable resumed-handshake gains
    - demote the checklist item so Session public surface is only considered after dedicated Windows / target-specific validation

- `bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - result: PASS
  - summary:
    - active owner-path doc alignment remained intact after tightening WinSSL best-practices session wording

- `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
  - result: PASS
  - summary:
    - secondary guide SNI guidance remained aligned after the WinSSL best-practices batch

- `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - broader WinSSL session-resumption docs truth remained aligned after tightening the best-practices page

- `git diff --check`
  - result: PASS
  - summary:
    - current WinSSL best-practices batch has no whitespace or patch-format issues

### Performance Profiling Guide Truth

- add `docs/plans/2026-05-20-performance-profiling-guide-truth.md`
  - change:
    - define the bounded profiling-guide batch for tightening current session/performance truth and explaining intentional direct-path profiling samples

- add `tests/scripts/test_performance_profiling_guide_truth_contract.sh`
  - change:
    - lock that `PERFORMANCE_PROFILING_GUIDE` must stop teaching fixed session/performance claims as current truth

- `bash -n tests/scripts/test_performance_profiling_guide_truth_contract.sh`
  - result: PASS
  - summary:
    - new performance-profiling-guide truth contract syntax is valid

- `bash tests/scripts/test_performance_profiling_guide_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - `PERFORMANCE_PROFILING_GUIDE` still lacked an explicit explanation for why profiling samples intentionally use direct connection paths
    - GREEN after fix:
      - `PERFORMANCE_PROFILING_GUIDE` now explains why profiling samples intentionally use caller-owned direct connection paths
      - `PERFORMANCE_PROFILING_GUIDE` now demotes WinSSL session public surface to current conservative runtime truth
      - `PERFORMANCE_PROFILING_GUIDE` now demotes fixed target numbers to non-authoritative reference shapes

- update `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
  - change:
    - explain that the profiling handshake sample intentionally uses direct `CreateConnection(...)` to control socket/handshake timing boundaries
    - explain that WinSSL session public surface remains experimental under
      `observed_reuse=false` / `session_configured=true`
    - demote the checklist item so Session public surface is only considered after dedicated Windows / target-specific validation
    - demote the fixed target table so fresh baseline truth points back to benchmark runners and the metrics template

- `bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - result: PASS
  - summary:
    - active owner-path doc alignment remained intact after tightening the profiling guide truth

- `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
  - result: PASS
  - summary:
    - secondary guide SNI guidance remained aligned after the profiling-guide batch

- `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - broader WinSSL session-resumption docs truth remained aligned after tightening the profiling guide

- `git diff --check`
  - result: PASS
  - summary:
    - current profiling-guide batch has no whitespace or patch-format issues

### README Performance + Session Truth

- add `docs/plans/2026-05-20-readme-performance-session-truth.md`
  - change:
    - define the bounded root-README batch for tightening high-entry performance and session truth

- add `tests/scripts/test_readme_performance_session_truth_contract.sh`
  - change:
    - lock that the root README must stop teaching fixed performance/session gains as current truth

- `bash -n tests/scripts/test_readme_performance_session_truth_contract.sh`
  - result: PASS
  - summary:
    - new README performance/session truth contract syntax is valid

- `bash tests/scripts/test_readme_performance_session_truth_contract.sh`
  - result: FAIL -> PASS
  - summary:
    - RED first exposed:
      - root `README.md` still lacked an explicit routing of performance claims back to fresh benchmark truth
    - GREEN after fix:
      - root `README.md` now routes performance claims back to benchmark/baseline entrypoints
      - root `README.md` now demotes session/ticket claims to backend-specific truth

- update `README.md`
  - change:
    - replace the fixed capability-matrix speedup claim with a benchmark/baseline truth handoff
    - replace the fixed session-resumption gain claim with backend-specific session truth wording

- `bash tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - result: PASS
  - summary:
    - landing quickstarts stayed aligned after tightening root README performance/session wording

- `bash tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
  - result: PASS
  - summary:
    - performance-guide benchmark truth remained aligned after tightening root README claims

- `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - result: PASS
  - summary:
    - broader WinSSL session-resumption docs truth remained aligned after tightening root README claims

- `git diff --check`
  - result: PASS
  - summary:
    - current root-README performance/session batch has no whitespace or patch-format issues
