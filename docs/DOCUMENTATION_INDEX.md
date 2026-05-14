# fafafa.ssl 文档索引

本索引仅链接到仓库中实际存在的文件。

- 当前文档位于 `docs/`
- 历史阶段报告/工作记录位于 `docs/archive/`

---

## 🧭 当前工程入口（release-control）

如果你是在继续当前工程收口、验证或回填证据，默认先看下面四项，再按当前命令推进：

- **[ROADMAP.md](ROADMAP.md)** - 当前稳定 roadmap / status 入口
- **[plans/2026-05-12-release-v1.5.0-formalization.md](plans/2026-05-12-release-v1.5.0-formalization.md)** - 当前 release-control 执行计划
- **[test_reports/RELEASE_READINESS_V1.5.0.md](test_reports/RELEASE_READINESS_V1.5.0.md)** - 当前 v1.5.0 release readiness / stop-here 结论
- **[../.github/README.md](../.github/README.md)** - 当前 workflow surface
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local` - FreePascal TLS 1.3 + validation runtime focused gate
- `python3 scripts/check_code_style.py src`
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
- Wave C closeout / 审批参考：`test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`、`test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`

---

## 🚀 从这里开始（推荐）

1. **[ROADMAP.md](ROADMAP.md)** - 当前稳定 roadmap / status 入口 🆕
2. **[plans/2026-05-12-release-v1.5.0-formalization.md](plans/2026-05-12-release-v1.5.0-formalization.md)** - 当前 release-control 执行计划 🆕
3. **[test_reports/RELEASE_READINESS_V1.5.0.md](test_reports/RELEASE_READINESS_V1.5.0.md)** - 当前 v1.5.0 release readiness 结论 🆕
4. **[../.github/README.md](../.github/README.md)** - 当前 workflow surface 🆕
5. **[guides/GETTING_STARTED.md](guides/GETTING_STARTED.md)** - 入门（推荐入口与最小示例）
6. **[guides/QUICKSTART.md](guides/QUICKSTART.md)** - 快速开始
7. **[INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md)** - 框架集成指南（如何把 TLS 接到你的网络框架）
8. **[reference/API_REFERENCE.md](reference/API_REFERENCE.md)** - API 参考
9. **[examples/README.md](../examples/README.md)** - 示例程序说明
10. **[examples/EXAMPLES_INDEX.md](../examples/EXAMPLES_INDEX.md)** - 示例程序完整索引（按功能分类）
11. **[reference/ARCHITECTURE.md](reference/ARCHITECTURE.md)** - 架构设计文档 🆕

---

## 📚 主题索引

### 使用与集成

- **[guides/QUICKSTART_30SEC.md](guides/QUICKSTART_30SEC.md)** - 30 秒快速示例索引 🆕
- **[guides/USER_GUIDE.md](guides/USER_GUIDE.md)**
- **[guides/DEPLOYMENT_GUIDE.md](guides/DEPLOYMENT_GUIDE.md)**
- **[guides/STORE_USAGE_GUIDE.md](guides/STORE_USAGE_GUIDE.md)** - Store 跨平台使用指南 🆕
- **[guides/OCSP_USAGE_GUIDE.md](guides/OCSP_USAGE_GUIDE.md)** - OCSP 使用指南（FreePascal stapling cryptographic verification + client online OCSP + OpenSSL helper） 🆕
- **[guides/CT_IMPLEMENTATION_GUIDE.md](guides/CT_IMPLEMENTATION_GUIDE.md)** - CT 实现指南（FreePascal runtime SCT surface: TLS / embedded / OCSP-delivered + validator） 🆕
- **[guides/TS_USAGE_GUIDE.md](guides/TS_USAGE_GUIDE.md)** - TS 使用指南（OpenSSL） 🆕
- **[ZERO_DEPENDENCY_DEPLOYMENT.md](ZERO_DEPENDENCY_DEPLOYMENT.md)**
- **[guides/MIGRATION_GUIDE.md](guides/MIGRATION_GUIDE.md)**
- **[MIGRATION_GUIDE_V1.1.md](MIGRATION_GUIDE_V1.1.md)** - v1.1/v1.2 迁移指南 🆕
- **[NATIVE_HANDLE_QUICK_REF.md](NATIVE_HANDLE_QUICK_REF.md)** - 原生句柄快速参考（v1.1.1）🆕
- **[CAPABILITY_MATRIX_GUIDE.md](CAPABILITY_MATRIX_GUIDE.md)** - 能力矩阵使用指南（v1.2.0）🆕

### 构建与依赖

- **[DEPENDENCIES.md](DEPENDENCIES.md)**
- **[FCL_DEPENDENCIES.md](FCL_DEPENDENCIES.md)**
- **[guides/LINUX_QUICKSTART.md](guides/LINUX_QUICKSTART.md)**
- **[guides/WINSSL_QUICKSTART.md](guides/WINSSL_QUICKSTART.md)**

### API / 设计 / 约定

- **[reference/API_DESIGN_GUIDE.md](reference/API_DESIGN_GUIDE.md)**
- **[reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md](reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md)** - P2 最低可用 API 与能力矩阵字段映射 🆕
- **[reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md](reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md)** - OpenSSL 1.1.1 vs 3.x 差异清单与回归策略 🆕
- **[reference/STORE_CROSS_PLATFORM_DIFFERENCES.md](reference/STORE_CROSS_PLATFORM_DIFFERENCES.md)** - Store 跨平台差异说明（Linux/macOS/Windows） 🆕
- **[reference/RETURN_TYPE_CONVENTIONS.md](reference/RETURN_TYPE_CONVENTIONS.md)**
- **[guides/ERROR_HANDLING_BEST_PRACTICES.md](guides/ERROR_HANDLING_BEST_PRACTICES.md)**
- **[reference/OPENSSL_MODULES.md](reference/OPENSSL_MODULES.md)**
- **[archive/OPENSSL_IMPLEMENTATION_PLAN.md](archive/OPENSSL_IMPLEMENTATION_PLAN.md)**（归档）
- **[archive/OPENSSL_MODULE_VALIDATION_PLAN.md](archive/OPENSSL_MODULE_VALIDATION_PLAN.md)**（归档）
- **[reference/WINSSL_DESIGN.md](reference/WINSSL_DESIGN.md)**
- **[reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md](reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md)** - MbedTLS 后端能力矩阵 🆕
- **[reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md](reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md)** - WinSSL 后端能力矩阵 🆕
- **[reference/BACKEND_SELECTOR_DESIGN.md](reference/BACKEND_SELECTOR_DESIGN.md)** - 后端选择器设计文档 🆕
- **[reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md](reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md)** - 后端抽象层设计文档 🆕
- **ADRs**: **[adr/README.md](adr/README.md)**

### 安全

- **[guides/SECURITY_GUIDE.md](guides/SECURITY_GUIDE.md)**
- **[guides/SECURITY_AUDIT.md](guides/SECURITY_AUDIT.md)**
- **[CA_CERTIFICATE_AUTO_LOADING.md](CA_CERTIFICATE_AUTO_LOADING.md)**

### 测试与验证

- **[testing/TESTING_README.md](testing/TESTING_README.md)**
- **[testing/TEST_PLAN.md](testing/TEST_PLAN.md)**
- **[testing/TEST_RESULTS.md](testing/TEST_RESULTS.md)**
- **[testing/P2_OFFLINE_FIXTURE_GUIDE.md](testing/P2_OFFLINE_FIXTURE_GUIDE.md)** - P2 离线夹具规范与目录约定 🆕
- **[testing/P2_CERT_SERVICE_OFFLINE_VALIDATION_GUIDE.md](testing/P2_CERT_SERVICE_OFFLINE_VALIDATION_GUIDE.md)** - CT/OCSP/TS 离线验证指南 🆕
- **[../tests/openssl/test_ocsp_connection_verification_regression.pas](../tests/openssl/test_ocsp_connection_verification_regression.pas)** - OpenSSL OCSP stapling 验证回归测试（含 wrapper fallback + status_request 启用 + required fail-closed） 🆕
- **[../scripts/generate_p2_ocsp_successful_basic_fixture.sh](../scripts/generate_p2_ocsp_successful_basic_fixture.sh)** - 生成 successful/basic OCSP 离线 fixture 的脚本 🆕
- **[test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md](test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md)** - Phase 2 性能指标模板（Draft） 🆕
- **[test_reports/GATE_ARCHIVE_EVIDENCE_TEMPLATE.md](test_reports/GATE_ARCHIVE_EVIDENCE_TEMPLATE.md)** - 门禁与归档证据统一模板（Draft） 🆕
- **[test_reports/CROSS_PLATFORM_GATE_SUMMARY_TEMPLATE.md](test_reports/CROSS_PLATFORM_GATE_SUMMARY_TEMPLATE.md)** - 跨平台 Gate 聚合摘要模板（Draft） 🆕
- **[test_reports/ARCHIVE_CLEANUP_EXECUTION_RECORD_TEMPLATE.md](test_reports/ARCHIVE_CLEANUP_EXECUTION_RECORD_TEMPLATE.md)** - 归档清理执行记录模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_TEMPLATE.md)** - 归档审计抽样记录模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_TEMPLATE.md)** - 归档审计抽样与 hold 到期联动模板（Draft） 🆕
- **[test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_TEMPLATE.md](test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_TEMPLATE.md)** - 发布前归档审计最小核查清单模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_TEMPLATE.md)** - 归档审计执行周报模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_TEMPLATE.md)** - 归档审计状态看板模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_TEMPLATE.md)** - 归档审计风险分级与响应模板（Draft） 🆕
- **[test_reports/PRE_RELEASE_AUDIT_BLOCKERS_TEMPLATE.md](test_reports/PRE_RELEASE_AUDIT_BLOCKERS_TEMPLATE.md)** - 发布前审计阻断项模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_TEMPLATE.md)** - 周报与发布清单一致性报告模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_TEMPLATE.md)** - 状态看板阈值策略模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_TEMPLATE.md)** - 风险响应执行回执模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_TEMPLATE.md)** - 阻断项关闭校验与豁免记录模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_TEMPLATE.md)** - 一致性偏差修复建议模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_TEMPLATE.md)** - 阈值策略回测与漂移监控模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_TEMPLATE.md)** - 执行回执签批链路模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_TEMPLATE.md)** - 阻断项重测与回归门禁模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_TEMPLATE.md)** - 多周趋势风险收敛看板模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_TEMPLATE.md)** - 签批后执行回执回写模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_TEMPLATE.md)** - 签批证据归档一致性巡检模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_TEMPLATE.md)** - 重测-签批联动回写一致性模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_TEMPLATE.md)** - 收敛看板阈值自适应策略模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_TEMPLATE.md)** - 回写载荷版本化与回滚模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_TEMPLATE.md)** - 证据巡检异常分级处置模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_TEMPLATE.md)** - 签批链路 SLA 违约预警模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_TEMPLATE.md)** - 回写变更覆盖率修复追踪模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_TEMPLATE.md)** - 联动与回滚演练计划模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_TEMPLATE.md)** - SLA 与回滚联动演练模板（Draft） 🆕
- **[test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_TEMPLATE.md](test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_TEMPLATE.md)** - 回写覆盖率修复闭环验收门禁模板（Draft） 🆕
- **[test_reports/PHASE2_BASELINE_EXECUTION_SUMMARY.md](test_reports/PHASE2_BASELINE_EXECUTION_SUMMARY.md)** - Phase 2 基线执行汇总 🆕
- **[test_reports/PHASE2_BASELINE_COMPARISON_V1.md](test_reports/PHASE2_BASELINE_COMPARISON_V1.md)** - Phase 2 首轮基线对比结论 🆕

### Wave C closeout / 审批 / 历史参考

以下条目不再是默认工程入口，仅在需要审批、closeout 或历史对照时使用。

- **[test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md](test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md)** - Wave C 收口状态：等待人工审批，当前不再继续推进主流程 🆕
- **[test_reports/WAVE_C_BASELINE_READINESS_MANIFEST_2026-03-19.md](test_reports/WAVE_C_BASELINE_READINESS_MANIFEST_2026-03-19.md)** - Wave C 当前 baseline / readiness 模块清单 🆕
- **[test_reports/WAVE_C_LIVE_EVIDENCE_MANIFEST_2026-03-19.md](test_reports/WAVE_C_LIVE_EVIDENCE_MANIFEST_2026-03-19.md)** - Wave C 当前 live evidence 模块清单（按模块导航） 🆕
- **[test_reports/WAVE_C_APPROVAL_SUBMISSION_MANIFEST_2026-03-19.md](test_reports/WAVE_C_APPROVAL_SUBMISSION_MANIFEST_2026-03-19.md)** - Wave C 当前 approval / submission 模块清单 🆕
- **[test_reports/WAVE_C_UNIFIED_BASELINE_STATUS_2026-03-15.md](test_reports/WAVE_C_UNIFIED_BASELINE_STATUS_2026-03-15.md)** - Wave C 当前统一 baseline/B101 入口状态 🆕
- **[test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md](test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md)** - Wave C 当前 local-first / pre-CI / submission 总入口 🆕
- **[test_reports/WAVE_C_READINESS_REFRESH_2026-03-15.md](test_reports/WAVE_C_READINESS_REFRESH_2026-03-15.md)** - Wave C 当前流程 readiness 刷新结论 🆕
- **[test_reports/WAVE_C_UNIFIED_THRESHOLD_REFRESH_2026-03-15.md](test_reports/WAVE_C_UNIFIED_THRESHOLD_REFRESH_2026-03-15.md)** - Wave C 新入口下的阈值刷新结论 🆕
- **[test_reports/WAVE_C_B107_THRESHOLD_EVALUATION_RESULT_2026-03-15.md](test_reports/WAVE_C_B107_THRESHOLD_EVALUATION_RESULT_2026-03-15.md)** - Wave C B107 阈值评估结果（新入口） 🆕
- **[test_reports/WAVE_C_B108_DEFAULT_ON_READINESS_RESULT_2026-03-15.md](test_reports/WAVE_C_B108_DEFAULT_ON_READINESS_RESULT_2026-03-15.md)** - Wave C B108 readiness 结果（新入口） 🆕
- **[test_reports/WAVE_C_B109_CONTROLLED_CANARY_RESULT_2026-03-15.md](test_reports/WAVE_C_B109_CONTROLLED_CANARY_RESULT_2026-03-15.md)** - Wave C B109 canary 结果（新入口） 🆕
- **[test_reports/WAVE_C_B110_ROLLBACK_DRILL_RESULT_2026-03-15.md](test_reports/WAVE_C_B110_ROLLBACK_DRILL_RESULT_2026-03-15.md)** - Wave C B110 rollback 结果（新入口） 🆕
- **[test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_2026-03-15.md](test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_2026-03-15.md)** - Wave C B113 signoff record（新入口） 🆕
- **[test_reports/WAVE_C_B113_SIGNOFF_PACK_RESULT_2026-03-15.md](test_reports/WAVE_C_B113_SIGNOFF_PACK_RESULT_2026-03-15.md)** - Wave C B113 signoff 结果（新入口） 🆕
- **[test_reports/WAVE_C_B115_WORKFLOW_ENABLE_RESULT_2026-03-15.md](test_reports/WAVE_C_B115_WORKFLOW_ENABLE_RESULT_2026-03-15.md)** - Wave C B115 enable prereq 结果（新入口） 🆕
- **[test_reports/WAVE_C_B116_ENABLEMENT_PACKET_RESULT_2026-03-15.md](test_reports/WAVE_C_B116_ENABLEMENT_PACKET_RESULT_2026-03-15.md)** - Wave C B116 enablement packet 结果（新入口） 🆕
- **[test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-03-16.md](test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-03-16.md)** - Wave C B146 submission pack 结果（新入口） 🆕
- **[test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-03-16.md](test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-03-16.md)** - Wave C B147 pack check 结果（新入口） 🆕
- **[test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-03-15.md](test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-03-15.md)** - Wave C B148 approval brief 结果（新入口） 🆕
- **[test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-03-16.md](test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-03-16.md)** - Wave C B149 submission bundle 结果（新入口） 🆕

### Other Reports

- **[archive/VALIDATION_ROADMAP.md](archive/VALIDATION_ROADMAP.md)**（归档）
- **[validation/validation_report_20251003_013646.md](validation/validation_report_20251003_013646.md)**
- **[test_reports/P2_PKCS12_TEST_REPORT.md](test_reports/P2_PKCS12_TEST_REPORT.md)**
- **[test_reports/P2_PKCS12_COMPREHENSIVE_TEST_REPORT.md](test_reports/P2_PKCS12_COMPREHENSIVE_TEST_REPORT.md)**
- **[test_reports/P2_MODULES_TEST_REPORT.md](test_reports/P2_MODULES_TEST_REPORT.md)** - P2 核心模块汇总 🆕
- **[test_reports/P2_STORE_MODULE_REPORT.md](test_reports/P2_STORE_MODULE_REPORT.md)** - Store 模块级测试报告 🆕
- **[test_reports/P2_OCSP_MODULE_REPORT.md](test_reports/P2_OCSP_MODULE_REPORT.md)** - OCSP 模块级测试报告 🆕
- **[test_reports/P2_TS_MODULE_REPORT.md](test_reports/P2_TS_MODULE_REPORT.md)** - TS 模块级测试报告 🆕
- **[test_reports/EXAMPLES_COMPILE_FIX_TRACKER.md](test_reports/EXAMPLES_COMPILE_FIX_TRACKER.md)** - 示例编译修复追踪 🆕
- **[test_reports/MBEDTLS_BACKEND_STATUS_REPORT.md](test_reports/MBEDTLS_BACKEND_STATUS_REPORT.md)** - MbedTLS 后端状态报告 🆕
- **[test_reports/WINSSL_BACKEND_STATUS_REPORT.md](test_reports/WINSSL_BACKEND_STATUS_REPORT.md)** - WinSSL 后端状态报告 🆕
- **[test_reports/ROADMAP_CLOSURE_PROGRESS_2026-02-08.md](test_reports/ROADMAP_CLOSURE_PROGRESS_2026-02-08.md)** - 路线图收口进度报告（2026-02-08） 🆕
- **[test_reports/WAVE_B_CI_GATE_PROGRESS_2026-02-08.md](test_reports/WAVE_B_CI_GATE_PROGRESS_2026-02-08.md)** - Wave B Linux CI 门禁执行记录（2026-02-08） 🆕
- **[test_reports/WAVE_B_CROSS_PLATFORM_GATE_MANIFEST_2026-02-08.md](test_reports/WAVE_B_CROSS_PLATFORM_GATE_MANIFEST_2026-02-08.md)** - Wave B 跨平台门禁执行清单（2026-02-08） 🆕

### 历史 Wave C 页面（仅归档参考）

- **[test_reports/WAVE_C_B120_POST_TRIGGER_OBSERVABILITY_RESULT_2026-02-08.md](test_reports/WAVE_C_B120_POST_TRIGGER_OBSERVABILITY_RESULT_2026-02-08.md)** - Wave C 触发后观测结果（B120） 🆕
- **[test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md](test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md)** - Wave C 一页式运行手册（B121） 🆕
- **[test_reports/WAVE_C_B122_CI_DEFERRED_LOCAL_MODE_2026-02-08.md](test_reports/WAVE_C_B122_CI_DEFERRED_LOCAL_MODE_2026-02-08.md)** - Wave C CI 暂缓与本地优先决策记录（B122） 🆕
- **[test_reports/WAVE_C_B123_LOCAL_FIRST_CONTINUITY_RESULT_2026-02-09.md](test_reports/WAVE_C_B123_LOCAL_FIRST_CONTINUITY_RESULT_2026-02-09.md)** - Wave C 本地优先连续性门禁结果（B123） 🆕
- **[test_reports/WAVE_C_B124_LOCAL_DRIFT_WATCH_RESULT_2026-02-09.md](test_reports/WAVE_C_B124_LOCAL_DRIFT_WATCH_RESULT_2026-02-09.md)** - Wave C 本地优先周期漂移检查结果（B124） 🆕
- **[test_reports/WAVE_C_B125_LOCAL_GUARD_BUNDLE_RESULT_2026-02-09.md](test_reports/WAVE_C_B125_LOCAL_GUARD_BUNDLE_RESULT_2026-02-09.md)** - Wave C 本地守护汇总结果（B125） 🆕
- **[test_reports/WAVE_C_B126_LOCAL_GUARD_HISTORY_RESULT_2026-02-09.md](test_reports/WAVE_C_B126_LOCAL_GUARD_HISTORY_RESULT_2026-02-09.md)** - Wave C 本地守护历史趋势结果（B126） 🆕
- **[test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md](test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md)** - Wave C 本地守护故障速查手册（B127） 🆕
- **[test_reports/WAVE_C_B128_LOCAL_FIRST_DOC_ENTRY_RESULT_2026-02-09.md](test_reports/WAVE_C_B128_LOCAL_FIRST_DOC_ENTRY_RESULT_2026-02-09.md)** - Wave C 本地优先文档入口收口结果（B128） 🆕
- **[test_reports/WAVE_C_B129_ONCALL_CHECK_RESULT_2026-02-09.md](test_reports/WAVE_C_B129_ONCALL_CHECK_RESULT_2026-02-09.md)** - Wave C 本地守护值班检查结果（B129） 🆕
- **[test_reports/WAVE_C_B130_ONCALL_RHYTHM_TEMPLATE_2026-02-09.md](test_reports/WAVE_C_B130_ONCALL_RHYTHM_TEMPLATE_2026-02-09.md)** - Wave C 本地守护值班节奏模板（B130） 🆕
- **[test_reports/WAVE_C_B131_LOCAL_FIRST_HANDOFF_CHECKLIST_2026-02-09.md](test_reports/WAVE_C_B131_LOCAL_FIRST_HANDOFF_CHECKLIST_2026-02-09.md)** - Wave C 本地守护最小交接清单（B131） 🆕
- **[test_reports/WAVE_C_B132_LOCAL_FIRST_STATUS_SNAPSHOT_RESULT_2026-02-09.md](test_reports/WAVE_C_B132_LOCAL_FIRST_STATUS_SNAPSHOT_RESULT_2026-02-09.md)** - Wave C 本地优先状态快照结果（B132） 🆕
- **[test_reports/WAVE_C_B133_LOCAL_GUARD_EVIDENCE_RETENTION_2026-02-09.md](test_reports/WAVE_C_B133_LOCAL_GUARD_EVIDENCE_RETENTION_2026-02-09.md)** - Wave C 本地守护证据保留策略建议（B133） 🆕
- **[test_reports/WAVE_C_B134_LOCAL_FIRST_CLOSURE_SUMMARY_2026-02-09.md](test_reports/WAVE_C_B134_LOCAL_FIRST_CLOSURE_SUMMARY_2026-02-09.md)** - Wave C 本地优先阶段收口摘要（B134） 🆕
- **[test_reports/WAVE_C_B135_PRE_CI_REENABLE_PACKET_2026-02-09.md](test_reports/WAVE_C_B135_PRE_CI_REENABLE_PACKET_2026-02-09.md)** - Wave C 恢复 CI 前检查包（B135） 🆕
- **[test_reports/WAVE_C_B136_DELIVERABLES_OVERVIEW_2026-02-09.md](test_reports/WAVE_C_B136_DELIVERABLES_OVERVIEW_2026-02-09.md)** - Wave C 当前可交付清单总览（B136） 🆕
- **[test_reports/WAVE_C_B137_PRE_CI_REENABLE_PACKET_RESULT_2026-02-09.md](test_reports/WAVE_C_B137_PRE_CI_REENABLE_PACKET_RESULT_2026-02-09.md)** - Wave C 恢复 CI 前审批包结果（B137） 🆕
- **[test_reports/WAVE_C_B138_PRE_CI_REENABLE_FULL_GATE_RESULT_2026-02-09.md](test_reports/WAVE_C_B138_PRE_CI_REENABLE_FULL_GATE_RESULT_2026-02-09.md)** - Wave C 恢复 CI 前全量门禁结果（B138） 🆕
- **[test_reports/WAVE_C_B139_LOCAL_GUARD_CLEANUP_PLAN_RESULT_2026-02-09.md](test_reports/WAVE_C_B139_LOCAL_GUARD_CLEANUP_PLAN_RESULT_2026-02-09.md)** - Wave C 本地守护清理计划结果（B139） 🆕
- **[test_reports/WAVE_C_B140_LOCAL_GUARD_CONSISTENCY_RESULT_2026-02-09.md](test_reports/WAVE_C_B140_LOCAL_GUARD_CONSISTENCY_RESULT_2026-02-09.md)** - Wave C 本地守护一致性检查结果（B140） 🆕
- **[test_reports/WAVE_C_B141_LOCAL_GUARD_OPERATIONS_SUMMARY_2026-02-09.md](test_reports/WAVE_C_B141_LOCAL_GUARD_OPERATIONS_SUMMARY_2026-02-09.md)** - Wave C 本地守护运维摘要（B141） 🆕
- **[test_reports/WAVE_C_B142_LOCAL_GUARD_STATUS_EXPORT_RESULT_2026-02-09.md](test_reports/WAVE_C_B142_LOCAL_GUARD_STATUS_EXPORT_RESULT_2026-02-09.md)** - Wave C 本地守护状态导出结果（B142） 🆕
- **[test_reports/WAVE_C_B143_LOCAL_GUARD_ALERT_THRESHOLDS_RESULT_2026-02-09.md](test_reports/WAVE_C_B143_LOCAL_GUARD_ALERT_THRESHOLDS_RESULT_2026-02-09.md)** - Wave C 本地守护告警阈值结果（B143） 🆕
- **[test_reports/WAVE_C_B144_LOCAL_GUARD_OPS_PACK_RESULT_2026-02-09.md](test_reports/WAVE_C_B144_LOCAL_GUARD_OPS_PACK_RESULT_2026-02-09.md)** - Wave C 本地守护运维打包结果（B144） 🆕
- **[test_reports/WAVE_C_B145_FAST_TRACK_BUNDLE_RESULT_2026-02-09.md](test_reports/WAVE_C_B145_FAST_TRACK_BUNDLE_RESULT_2026-02-09.md)** - Wave C 快速打包阶段结果（B145） 🆕
- **[test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-02-09.md](test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-02-09.md)** - Wave C 恢复 CI 提交包结果（B146） 🆕
- **[test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-02-09.md](test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-02-09.md)** - Wave C 提交包校验结果（B147） 🆕
- **[test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-02-09.md](test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-02-09.md)** - Wave C 恢复 CI 审批简报结果（B148） 🆕
- **[test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-02-09.md](test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-02-09.md)** - Wave C 恢复 CI 提交打包结果（B149） 🆕

### 路线图与计划

- **[ROADMAP.md](ROADMAP.md)** - 当前稳定 roadmap / status 入口 🆕
- **[plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md](plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md)** - 当前 backend completeness 主线 🆕
- **[plans/2026-03-25-freepascal-tls13-client-session-resumption-psk.md](plans/2026-03-25-freepascal-tls13-client-session-resumption-psk.md)** - FreePascal TLS 1.3 client resumption / PSK 🆕
- **[plans/2026-03-26-freepascal-tls13-server-session-resumption-psk.md](plans/2026-03-26-freepascal-tls13-server-session-resumption-psk.md)** - FreePascal TLS 1.3 server resumption / PSK 🆕
- **[plans/Q1_2026_P2_VALIDATION_PLAN.md](plans/Q1_2026_P2_VALIDATION_PLAN.md)** - Q1 P2 验证执行计划 🆕
- **[plans/PHASE3_MINIMAL_CI_GATE_DRAFT.md](plans/PHASE3_MINIMAL_CI_GATE_DRAFT.md)** - Phase 3 最小 CI 门禁草案 🆕
- **[plans/PHASE3_OPENSSL_MATRIX_COMMAND_DRAFT.md](plans/PHASE3_OPENSSL_MATRIX_COMMAND_DRAFT.md)** - Phase 3 Linux OpenSSL 矩阵命令草案 🆕
- **[plans/PHASE3_CI_ARTIFACT_ARCHIVE_STRATEGY_DRAFT.md](plans/PHASE3_CI_ARTIFACT_ARCHIVE_STRATEGY_DRAFT.md)** - Phase 3 CI 产物归档策略草案 🆕
- **[plans/PHASE3_CROSS_PLATFORM_GATE_LAYERING_STRATEGY_DRAFT.md](plans/PHASE3_CROSS_PLATFORM_GATE_LAYERING_STRATEGY_DRAFT.md)** - Phase 3 跨平台门禁分层策略草案 🆕
- **[plans/PHASE3_MACOS_OPENSSL_PATH_CHECK_DRAFT.md](plans/PHASE3_MACOS_OPENSSL_PATH_CHECK_DRAFT.md)** - Phase 3 macOS OpenSSL 路径校验命令草案 🆕
- **[plans/PHASE3_WINDOWS_WINSSL_GATE_AND_ARCHIVE_DRAFT.md](plans/PHASE3_WINDOWS_WINSSL_GATE_AND_ARCHIVE_DRAFT.md)** - Phase 3 Windows WinSSL 门禁与归档映射草案 🆕
- **[plans/PHASE3_CI_WORKFLOW_LAYERED_INTEGRATION_DRAFT.md](plans/PHASE3_CI_WORKFLOW_LAYERED_INTEGRATION_DRAFT.md)** - Phase 3 CI workflow 分层接入草案 🆕
- **[plans/PHASE4_GATE_ARCHIVE_EVIDENCE_TEMPLATE_UNIFICATION_DRAFT.md](plans/PHASE4_GATE_ARCHIVE_EVIDENCE_TEMPLATE_UNIFICATION_DRAFT.md)** - Phase 4 门禁与归档证据模板统一化草案 🆕
- **[plans/PHASE4_RELEASE_RETENTION_AND_CLEANUP_WINDOW_DRAFT.md](plans/PHASE4_RELEASE_RETENTION_AND_CLEANUP_WINDOW_DRAFT.md)** - Phase 4 发布级归档保留策略与清理窗口草案 🆕
- **[plans/PHASE4_ARCHIVE_CLEANUP_AUTOMATION_COMMAND_DRAFT.md](plans/PHASE4_ARCHIVE_CLEANUP_AUTOMATION_COMMAND_DRAFT.md)** - Phase 4 归档清理自动化命令草案 🆕
- **[plans/PHASE4_ARCHIVE_RETENTION_COMPLIANCE_CHECKLIST_DRAFT.md](plans/PHASE4_ARCHIVE_RETENTION_COMPLIANCE_CHECKLIST_DRAFT.md)** - Phase 4 归档保留策略合规核查清单草案 🆕
- **[plans/PHASE4_CROSS_PLATFORM_GATE_SUMMARY_TEMPLATE_DRAFT.md](plans/PHASE4_CROSS_PLATFORM_GATE_SUMMARY_TEMPLATE_DRAFT.md)** - Phase 4 跨平台 Gate 结果聚合摘要模板草案 🆕
- **[plans/PHASE4_ARCHIVE_HOLD_MARKING_PROCESS_DRAFT.md](plans/PHASE4_ARCHIVE_HOLD_MARKING_PROCESS_DRAFT.md)** - Phase 4 归档豁免 hold 标记流程草案 🆕
- **[plans/PHASE4_ARCHIVE_CLEANUP_EXECUTION_RECORD_TEMPLATE_DRAFT.md](plans/PHASE4_ARCHIVE_CLEANUP_EXECUTION_RECORD_TEMPLATE_DRAFT.md)** - Phase 4 归档清理执行记录模板草案 🆕
- **[plans/PHASE4_ARCHIVE_AUDIT_SAMPLING_RECORD_DRAFT.md](plans/PHASE4_ARCHIVE_AUDIT_SAMPLING_RECORD_DRAFT.md)** - Phase 4 归档审计抽样记录草案 🆕
- **[plans/PHASE4_GATE_SUMMARY_CONSISTENCY_CHECK_COMMAND_DRAFT.md](plans/PHASE4_GATE_SUMMARY_CONSISTENCY_CHECK_COMMAND_DRAFT.md)** - Phase 4 Gate 聚合摘要一致性检查命令草案 🆕
- **[plans/PHASE4_HOLD_EXPIRY_REVIEW_REMINDER_COMMAND_DRAFT.md](plans/PHASE4_HOLD_EXPIRY_REVIEW_REMINDER_COMMAND_DRAFT.md)** - Phase 4 hold 到期复核提醒命令草案 🆕
- **[plans/PHASE4_ARCHIVE_EVIDENCE_DOCS_INDEX_DEDUP_DRAFT.md](plans/PHASE4_ARCHIVE_EVIDENCE_DOCS_INDEX_DEDUP_DRAFT.md)** - Phase 4 归档与证据文档索引去重草案 🆕
- **[plans/PHASE4_ARCHIVE_AUDIT_HOLD_EXPIRY_LINKAGE_DRAFT.md](plans/PHASE4_ARCHIVE_AUDIT_HOLD_EXPIRY_LINKAGE_DRAFT.md)** - Phase 4 归档审计抽样与 hold 到期提醒联动草案 🆕
- **[plans/PHASE4_PRE_RELEASE_ARCHIVE_AUDIT_MIN_CHECKLIST_AUTOGEN_DRAFT.md](plans/PHASE4_PRE_RELEASE_ARCHIVE_AUDIT_MIN_CHECKLIST_AUTOGEN_DRAFT.md)** - Phase 4 发布前归档审计最小核查清单自动生成草案 🆕
- **[plans/PHASE4_ARCHIVE_AUDIT_WEEKLY_REPORT_TEMPLATE_DRAFT.md](plans/PHASE4_ARCHIVE_AUDIT_WEEKLY_REPORT_TEMPLATE_DRAFT.md)** - Phase 4 归档审计执行周报模板草案 🆕
- **[plans/PHASE4_ARCHIVE_AUDIT_STATUS_DASHBOARD_AUTOGEN_DRAFT.md](plans/PHASE4_ARCHIVE_AUDIT_STATUS_DASHBOARD_AUTOGEN_DRAFT.md)** - Phase 4 归档审计状态看板自动汇总草案 🆕
- **[plans/PHASE4_ARCHIVE_AUDIT_RISK_GRADING_RESPONSE_TEMPLATE_DRAFT.md](plans/PHASE4_ARCHIVE_AUDIT_RISK_GRADING_RESPONSE_TEMPLATE_DRAFT.md)** - Phase 4 归档审计风险分级与响应模板草案 🆕
- **[plans/PHASE4_PRE_RELEASE_AUDIT_BLOCKERS_EXTRACTION_DRAFT.md](plans/PHASE4_PRE_RELEASE_AUDIT_BLOCKERS_EXTRACTION_DRAFT.md)** - Phase 4 发布前审计阻断项自动提取草案 🆕
- **[plans/PHASE4_WEEKLY_CHECKLIST_CONSISTENCY_CHECK_DRAFT.md](plans/PHASE4_WEEKLY_CHECKLIST_CONSISTENCY_CHECK_DRAFT.md)** - Phase 4 周报与发布清单一致性核查草案 🆕
- **[plans/PHASE4_DASHBOARD_THRESHOLD_ESCALATION_STRATEGY_DRAFT.md](plans/PHASE4_DASHBOARD_THRESHOLD_ESCALATION_STRATEGY_DRAFT.md)** - Phase 4 状态看板阈值与升级策略草案 🆕
- **[plans/PHASE4_RISK_RESPONSE_EXECUTION_RECEIPT_TEMPLATE_DRAFT.md](plans/PHASE4_RISK_RESPONSE_EXECUTION_RECEIPT_TEMPLATE_DRAFT.md)** - Phase 4 风险响应执行回执模板草案 🆕
- **[plans/PHASE4_BLOCKER_CLOSURE_WAIVER_RECORD_DRAFT.md](plans/PHASE4_BLOCKER_CLOSURE_WAIVER_RECORD_DRAFT.md)** - Phase 4 阻断项关闭校验与豁免记录草案 🆕
- **[plans/PHASE4_CONSISTENCY_GAP_REMEDIATION_DRAFT.md](plans/PHASE4_CONSISTENCY_GAP_REMEDIATION_DRAFT.md)** - Phase 4 一致性偏差修复建议草案 🆕
- **[plans/PHASE4_THRESHOLD_POLICY_BACKTEST_DRIFT_MONITORING_DRAFT.md](plans/PHASE4_THRESHOLD_POLICY_BACKTEST_DRIFT_MONITORING_DRAFT.md)** - Phase 4 阈值策略回测与漂移监控草案 🆕
- **[plans/PHASE4_EXECUTION_RECEIPT_APPROVAL_CHAIN_DRAFT.md](plans/PHASE4_EXECUTION_RECEIPT_APPROVAL_CHAIN_DRAFT.md)** - Phase 4 执行回执签批链路草案 🆕
- **[plans/PHASE4_BLOCKER_RETEST_REGRESSION_GATE_DRAFT.md](plans/PHASE4_BLOCKER_RETEST_REGRESSION_GATE_DRAFT.md)** - Phase 4 阻断项重测与回归门禁草案 🆕
- **[plans/PHASE4_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_DRAFT.md](plans/PHASE4_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_DRAFT.md)** - Phase 4 多周趋势风险收敛看板草案 🆕
- **[plans/PHASE4_POST_APPROVAL_EXECUTION_RECEIPT_WRITEBACK_DRAFT.md](plans/PHASE4_POST_APPROVAL_EXECUTION_RECEIPT_WRITEBACK_DRAFT.md)** - Phase 4 签批后自动回写执行回执草案 🆕
- **[plans/PHASE4_APPROVAL_EVIDENCE_ARCHIVE_CONSISTENCY_AUDIT_DRAFT.md](plans/PHASE4_APPROVAL_EVIDENCE_ARCHIVE_CONSISTENCY_AUDIT_DRAFT.md)** - Phase 4 签批证据归档一致性巡检草案 🆕
- **[plans/PHASE4_RETEST_APPROVAL_WRITEBACK_LINKAGE_CONSISTENCY_DRAFT.md](plans/PHASE4_RETEST_APPROVAL_WRITEBACK_LINKAGE_CONSISTENCY_DRAFT.md)** - Phase 4 重测-签批联动回写一致性草案 🆕
- **[plans/PHASE4_CONVERGENCE_DASHBOARD_ADAPTIVE_THRESHOLD_POLICY_DRAFT.md](plans/PHASE4_CONVERGENCE_DASHBOARD_ADAPTIVE_THRESHOLD_POLICY_DRAFT.md)** - Phase 4 收敛看板阈值自适应策略草案 🆕
- **[plans/PHASE4_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_DRAFT.md](plans/PHASE4_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_DRAFT.md)** - Phase 4 回写载荷版本化与回滚草案 🆕
- **[plans/PHASE4_EVIDENCE_ANOMALY_GRADING_RESPONSE_DRAFT.md](plans/PHASE4_EVIDENCE_ANOMALY_GRADING_RESPONSE_DRAFT.md)** - Phase 4 证据巡检异常分级处置草案 🆕
- **[plans/PHASE4_APPROVAL_CHAIN_SLA_BREACH_ALERT_DRAFT.md](plans/PHASE4_APPROVAL_CHAIN_SLA_BREACH_ALERT_DRAFT.md)** - Phase 4 签批链路 SLA 违约预警草案 🆕
- **[plans/PHASE4_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKING_DRAFT.md](plans/PHASE4_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKING_DRAFT.md)** - Phase 4 回写变更覆盖率修复追踪草案 🆕
- **[plans/PHASE4_LINKAGE_ROLLBACK_DRILL_PLAN_DRAFT.md](plans/PHASE4_LINKAGE_ROLLBACK_DRILL_PLAN_DRAFT.md)** - Phase 4 联动与回滚演练计划草案 🆕
- **[plans/PHASE4_ANOMALY_RESPONSE_VALIDATION_DRILL_CHECKLIST_DRAFT.md](plans/PHASE4_ANOMALY_RESPONSE_VALIDATION_DRILL_CHECKLIST_DRAFT.md)** - Phase 4 异常处置验证演练清单草案 🆕
- **[plans/PHASE4_SLA_ROLLBACK_LINKAGE_DRILL_SCRIPT_DRAFT.md](plans/PHASE4_SLA_ROLLBACK_LINKAGE_DRILL_SCRIPT_DRAFT.md)** - Phase 4 SLA 与回滚联动演练脚本草案 🆕
- **[plans/PHASE4_WRITEBACK_COVERAGE_CLOSURE_ACCEPTANCE_GATE_DRAFT.md](plans/PHASE4_WRITEBACK_COVERAGE_CLOSURE_ACCEPTANCE_GATE_DRAFT.md)** - Phase 4 回写覆盖率修复闭环验收门禁草案 🆕
- **[plans/PHASE3_CI_GITHUB_ACTIONS_MATRIX_DRAFT.md](plans/PHASE3_CI_GITHUB_ACTIONS_MATRIX_DRAFT.md)** - Phase 3 GitHub Actions 多平台 CI 矩阵草案 🆕
- **[plans/PHASE3_EXAMPLES_COMPILE_VERIFY_DRAFT.md](plans/PHASE3_EXAMPLES_COMPILE_VERIFY_DRAFT.md)** - Phase 3 示例编译验证脚本草案 🆕
- **[plans/PHASE6_QUIC_EVALUATION_REPORT.md](plans/PHASE6_QUIC_EVALUATION_REPORT.md)** - Phase 6 QUIC 协议支持评估报告 🆕
- **[plans/PHASE5_MULTI_BACKEND_SUMMARY.md](plans/PHASE5_MULTI_BACKEND_SUMMARY.md)** - Phase 5 多后端架构总结 🆕
- **[plans/2026-02-08-roadmap-autonomous-closure-plan.md](plans/2026-02-08-roadmap-autonomous-closure-plan.md)** - 路线图长期自治收口执行计划 🆕
- **[plans/2026-02-08-roadmap-next-wave-plan.md](plans/2026-02-08-roadmap-next-wave-plan.md)** - 路线图下一波次执行计划 🆕

### 工具与维护

- **[TOOLS.md](TOOLS.md)**
- **[../tools/README.md](../tools/README.md)** - 能力矩阵可视化工具 🆕
- **[../.github/workflows/ci.yml](../.github/workflows/ci.yml)** - Linux minimal CI gate（启用）
- **[../.github/workflows/wave-b-b2-manual.yml](../.github/workflows/wave-b-b2-manual.yml)** - Wave B/B2 跨平台手动门禁（启用）
- **[../.github/workflows/tls13-signer-gate.yml](../.github/workflows/tls13-signer-gate.yml)** - TLS13 signer 专项门禁（启用）
- **[../.github/workflows/test-all-platforms.yml.disabled](../.github/workflows/test-all-platforms.yml.disabled)** - 多平台全量 workflow 模板（默认禁用）
- **[../.github/workflows/ci-matrix-draft.yml.disabled](../.github/workflows/ci-matrix-draft.yml.disabled)** - 多平台矩阵 workflow 草案（默认禁用）
- **[../scripts/detect_macos_openssl_enhanced.sh](../scripts/detect_macos_openssl_enhanced.sh)** - macOS OpenSSL 增强检测脚本 🆕
- **[../scripts/check_wave_c_local_first_continuity.sh](../scripts/check_wave_c_local_first_continuity.sh)** - Wave C 本地优先连续性检查脚本（B123） 🆕
- **[../scripts/check_wave_c_local_drift_watch.sh](../scripts/check_wave_c_local_drift_watch.sh)** - Wave C 本地优先周期漂移检查脚本（B124） 🆕
- **[../scripts/run_wave_c_local_first_guard_bundle.sh](../scripts/run_wave_c_local_first_guard_bundle.sh)** - Wave C 本地守护汇总执行脚本（B125） 🆕
- **[../scripts/summarize_wave_c_local_guard_history.sh](../scripts/summarize_wave_c_local_guard_history.sh)** - Wave C 本地守护历史趋势汇总脚本（B126） 🆕
- **[../scripts/run_wave_c_local_guard_oncall_check.sh](../scripts/run_wave_c_local_guard_oncall_check.sh)** - Wave C 本地守护值班检查脚本（B129） 🆕
- **[../scripts/generate_wave_c_local_first_status_snapshot.sh](../scripts/generate_wave_c_local_first_status_snapshot.sh)** - Wave C 本地优先状态快照脚本（B132） 🆕
- **[../scripts/run_wave_c_pre_ci_reenable_full_gate.sh](../scripts/run_wave_c_pre_ci_reenable_full_gate.sh)** - Wave C 恢复 CI 前全量门禁脚本（B138） 🆕
- **[../scripts/cleanup_wave_c_local_guard_reports.sh](../scripts/cleanup_wave_c_local_guard_reports.sh)** - Wave C 本地守护清理计划脚本（B139） 🆕
- **[../scripts/check_wave_c_local_guard_consistency.sh](../scripts/check_wave_c_local_guard_consistency.sh)** - Wave C 本地守护一致性检查脚本（B140） 🆕
- **[../scripts/export_wave_c_local_guard_status_json.sh](../scripts/export_wave_c_local_guard_status_json.sh)** - Wave C 本地守护状态导出脚本（B142） 🆕
- **[../scripts/check_wave_c_local_guard_alert_thresholds.sh](../scripts/check_wave_c_local_guard_alert_thresholds.sh)** - Wave C 本地守护告警阈值脚本（B143） 🆕
- **[../scripts/run_wave_c_local_guard_ops_pack.sh](../scripts/run_wave_c_local_guard_ops_pack.sh)** - Wave C 本地守护运维打包脚本（B144） 🆕
- **[../scripts/prepare_wave_c_ci_reenable_submission_pack.sh](../scripts/prepare_wave_c_ci_reenable_submission_pack.sh)** - Wave C 恢复 CI 提交包生成脚本（B146） 🆕
- **[../scripts/check_wave_c_ci_reenable_submission_pack.sh](../scripts/check_wave_c_ci_reenable_submission_pack.sh)** - Wave C 恢复 CI 提交包校验脚本（B147） 🆕
- **[../scripts/generate_wave_c_ci_reenable_approval_brief.sh](../scripts/generate_wave_c_ci_reenable_approval_brief.sh)** - Wave C 恢复 CI 审批简报脚本（B148） 🆕
- **[../scripts/run_wave_c_ci_reenable_submission_bundle.sh](../scripts/run_wave_c_ci_reenable_submission_bundle.sh)** - Wave C 恢复 CI 提交打包脚本（B149） 🆕
- **[archive/MAINTENANCE_PLAN.md](archive/MAINTENANCE_PLAN.md)**（归档）
- **[guides/TROUBLESHOOTING.md](guides/TROUBLESHOOTING.md)**
- **[guides/COMMON_PITFALLS.md](guides/COMMON_PITFALLS.md)** - 常见陷阱与避坑指南 🆕
- **[guides/PERFORMANCE_PROFILING_GUIDE.md](guides/PERFORMANCE_PROFILING_GUIDE.md)** - 性能热点剖析指南 🆕
- **[guides/FAQ.md](guides/FAQ.md)**

---

## 中文文档（docs/zh）

- **[快速入门.md](zh/快速入门.md)**
- **[编译指南.md](zh/编译指南.md)**
- **[安装配置.md](zh/安装配置.md)**
- **[FAQ.md](zh/FAQ.md)**

---

## 🗄️ 历史归档（docs/archive）

- **[archive/README.md](archive/README.md)**

历史归档中包含大量阶段性报告、状态快照和工作记录。下面给出几个常用入口（示例）：

- **[archive/reports/PROJECT_STATUS_2025-10-02.md](archive/reports/PROJECT_STATUS_2025-10-02.md)**
- **[archive/phase_reports/PHASE_7_FINAL_REPORT.md](archive/phase_reports/PHASE_7_FINAL_REPORT.md)**
- **[archive/reports/TDD_STATUS_AND_ROADMAP.md](archive/reports/TDD_STATUS_AND_ROADMAP.md)**

---

**最后更新**: 2026-03-18 (Wave C historical result navigation refresh)
