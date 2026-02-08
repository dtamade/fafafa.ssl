# Autonomous Iteration Protocol（路线图持续推进）

## Goal
在不打断会话的前提下，自主创建并执行任务，持续迭代项目，直到 `task_plan.md` 中路线图阶段全部完成。

## Execution Rules
1. 每轮结束前，必须在 `task_plan.md` 生成下一轮 3–5 个可执行任务。
2. 每轮至少完成 1 个可验证交付（代码、文档或测试报告之一）。
3. 每轮必须记录证据到 `findings.md` 与 `progress.md`（含命令与结果）。
4. 对失败执行 3-Strike 协议：诊断修复 → 变更方法 → 扩展重构；避免重复失败动作。
5. 优先级顺序：
   - P1：Phase 1 未完成项（失败场景、文档、示例）
   - P2：Phase 2 性能基准
   - P3：Phase 3 跨平台与 CI

## Rolling Backlog（Batch A / Batch B）

### Batch A（已完成）
- A1：PKCS12 往返用例 `create → export → parse → export`
- A2：OCSP 无效签名失败场景（离线 fixture）
- A3：TS 签名或时间无效失败场景（离线 deterministic）
- A4：CT 时间/issuer 不匹配失败场景
- A5：Store 跨平台差异文档（Linux/macOS/Windows）

### Batch B（进行中）
- B1：Store 模块级报告 + 使用指南 + 索引同步（已完成）
- B2：OCSP 模块级报告 + 使用指南（已完成）
- B3：TS 模块级报告 + 使用指南（已完成）
- B4：CT/OCSP/TS 离线验证说明补齐（已完成）
- B5：Phase 2 基准脚本与指标模板草案（已完成）
- B6：Phase 2 基线采集（握手/吞吐/会话复用）+ 首轮报告（已完成）
- B7：Phase 3 预备：CI 最小门禁命令清单与脚本草案（已完成）
- B8：Phase 4 预备：性能文档回填与索引同步（已完成）
- B9：Phase 2 扩展：指标模板回填与首轮基线对比结论（已完成）
- B10：Phase 3 扩展：Linux OpenSSL 1.1.1/3.x 矩阵命令草案（已完成）
- B11：Phase 3 扩展：CI 输出产物归档策略草案（已完成）
- B12：Phase 3 扩展：跨平台门禁分层策略草案（已完成）
- B13：Phase 3 扩展：macOS OpenSSL 路径校验命令草案（已完成）
- B14：Phase 3 扩展：Windows/WinSSL 门禁分层与归档映射草案（已完成）
- B15：Phase 3 扩展：CI workflow 分层接入草案（Linux/macOS/Windows）（已完成）
- B16：Phase 4 预备：门禁分层与归档证据模板统一化草案（已完成）
- B17：Phase 4 预备：发布级归档保留策略与清理窗口草案（已完成）
- B18：Phase 4 预备：归档清理自动化命令草案（已完成）
- B19：Phase 4 预备：归档保留策略合规核查清单草案（已完成）
- B20：Phase 4 预备：跨平台 Gate 结果聚合摘要模板草案（已完成）
- B21：Phase 4 预备：归档豁免（hold）标记流程草案（已完成）
- B22：Phase 4 预备：归档清理执行记录模板草案（已完成）
- B23：Phase 4 预备：归档审计抽样记录草案（已完成）
- B24：Phase 4 预备：Gate 聚合摘要一致性检查命令草案（已完成）
- B25：Phase 4 预备：hold 到期复核提醒命令草案（已完成）
- B26：Phase 4 预备：归档与证据文档索引去重草案（已完成）
- B27：Phase 4 预备：归档审计抽样与 hold 到期提醒联动草案（已完成）
- B28：Phase 4 预备：发布前归档审计最小核查清单自动生成草案（已完成）
- B29：Phase 4 预备：归档审计执行周报模板草案（已完成）
- B30：Phase 4 预备：归档审计状态看板自动汇总草案（已完成）
- B31：Phase 4 预备：归档审计风险分级与响应模板草案（已完成）
- B32：Phase 4 预备：发布前审计阻断项自动提取草案（已完成）
- B33：Phase 4 预备：周报与发布清单一致性核查草案（已完成）
- B34：Phase 4 预备：状态看板阈值与升级策略草案（已完成）
- B35：Phase 4 预备：风险响应执行回执模板草案（已完成）
- B36：Phase 4 预备：阻断项关闭校验与豁免记录草案（已完成）
- B37：Phase 4 预备：一致性偏差修复建议草案（已完成）
- B38：Phase 4 预备：阈值策略回测与漂移监控草案（已完成）
- B39：Phase 4 预备：执行回执签批链路草案（已完成）
- B40：Phase 4 预备：阻断项重测与回归门禁草案（已完成）
- B41：Phase 4 预备：多周趋势风险收敛看板草案（已完成）
- B42：Phase 4 预备：签批后自动回写执行回执草案（已完成）
- B43：Phase 4 预备：签批证据归档一致性巡检草案（已完成）
- B44：Phase 4 预备：重测-签批联动回写一致性草案（已完成）
- B45：Phase 4 预备：收敛看板阈值自适应策略草案（已完成）
- B46：Phase 4 预备：回写载荷版本化与回滚草案（已完成）
- B47：Phase 4 预备：证据巡检异常分级处置草案（已完成）
- B48：Phase 4 预备：签批链路 SLA 违约预警草案（已完成）
- B49：Phase 4 预备：回写变更覆盖率修复追踪草案（已完成）
- B50：Phase 4 预备：联动与回滚演练计划草案（已完成）
- B51：Phase 4 预备：异常处置验证演练清单草案（已完成）
- B52：Phase 4 预备：SLA 与回滚联动演练脚本草案（已完成）
- B53：Phase 4 预备：回写覆盖率修复闭环验收门禁草案（已完成）
- B54：Phase 4 预备：回写覆盖率自动修复脚本草案（进行中）
- B55：Phase 4 预备：SLA/回滚联动报告归档验真脚本草案
- B56：Phase 4 预备：闭环验收失败自动重试分流脚本草案
- B57：Phase 4 预备：闭环门禁周趋势与漂移复核草案

## Completion Condition
当 `task_plan.md` 中 Phase 0–6 全部标记为 `complete`，并且最新回归测试满足目标通过率后，自治循环结束。

## Iteration Addendum（2026-02-07 09:52 +0800）

- 主线代码子任务完成：`B51-M1`（OpenSSL OCSP stapling verification hardening）。
- 已完成交付：
  - `src/fafafa.ssl.openssl.connection.pas`（连接层 OCSP 读取/验证链路硬化）
  - `src/fafafa.ssl.openssl.api.ocsp.pas`（OpenSSL 3.x 小写符号别名回退）
  - `src/fafafa.ssl.openssl.api.ssl.pas`（OCSP 扩展函数加载补齐）
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`（回归测试）
- 下一批主线代码任务：
  - `B51-M2`：OCSP tlsext/status 宏回退封装统一化。
  - `B51-M3`：离线可复用 successful/basic OCSP fixture 生成链路（消除回归测试 SKIP）。

## Iteration Addendum（2026-02-07 10:04 +0800）

- 主线代码子任务完成：`B51-M2`（OCSP tlsext/status macro fallback unified wrappers）。
- 已完成交付：
  - `src/fafafa.ssl.openssl.api.ssl.pas`（统一 wrapper + loader 回退注入）
  - `src/fafafa.ssl.openssl.connection.pas`（调用侧去常量化）
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`（wrapper 可用性断言）
- 下一批主线代码任务：
  - `B51-M3`：构建可复用离线 successful/basic OCSP fixture，消除回归测试 SKIP。

## Iteration Addendum（2026-02-07 10:25 +0800）

- 主线代码子任务完成：`B51-M3`（offline successful/basic OCSP fixture pipeline）。
- 已完成交付：
  - `scripts/generate_p2_ocsp_successful_basic_fixture.sh`
  - `tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der`
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`（由 runtime 构造改为 fixture 驱动）
- 本轮结果：
  - `test_ocsp_connection_verification_regression` 从 `PASS=1, SKIP=1` 收敛为 `PASS=2, SKIP=0`。
- 下一步主线：
  - 继续 B51 主任务文档主项收口，或切换到 B52（SLA 与回滚联动演练脚本草案）。

## Iteration Addendum（2026-02-07 10:45 +0800）

- 主线代码子任务完成：`B51-M4`（`ssoEnableOCSPStapling` -> pre-handshake `status_request` enablement）。
- 已完成交付：
  - `src/fafafa.ssl.openssl.api.consts.pas`（新增 `TLSEXT_STATUSTYPE_ocsp`）
  - `src/fafafa.ssl.openssl.context.pas`（`ApplyOptions` 映射 OCSP stapling option 到 `SSL_CTX_set_tlsext_status_type`）
  - `src/fafafa.ssl.openssl.connection.pas`（新增握手前连接级 `SSL_set_tlsext_status_type` 同步）
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`（新增 option-to-handshake 启用回归）
- 本轮结果：
  - `test_ocsp_connection_verification_regression` 扩展后 `PASS=3, SKIP=0`。
  - `test_p2_ocsp_comprehensive` 保持 `55/55`。
  - 全模块编译保持 `157/157`。
- 下一批主线代码任务：
  - `B51-M5`：将 `WithOCSPStaplingRequired` 贯通到握手后 fail-closed 策略。
  - `B52`：SLA 与回滚联动演练脚本草案（在 B51 主项收口后切入）。

## Iteration Addendum（2026-02-07 11:00 +0800）

- 主线代码子任务完成：`B51-M5`（`WithOCSPStaplingRequired` fail-closed policy wiring）。
- 已完成交付：
  - `src/fafafa.ssl.base.pas`（新增 `ssoRequireOCSPStapling`）
  - `src/fafafa.ssl.context.builder.pas`（required option 写回 `TSSLOptions`）
  - `src/fafafa.ssl.openssl.connection.pas`（post-handshake required policy enforcement）
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`（required policy 回归）
- 本轮结果：
  - `test_ocsp_connection_verification_regression` 扩展后 `PASS=4, SKIP=0`。
  - `test_p2_ocsp_comprehensive` 保持 `55/55`。
  - 全模块编译保持 `157/157`。
- 下一批主线任务：
  - 收口 B51 主项（异常处置验证演练清单草案）。
  - 切换 B52（SLA 与回滚联动演练脚本草案）。

## Iteration Addendum（2026-02-07 11:25 +0800）

- 主线修复完成：`B51-M6`（builder declaration-order compile fix）。
- B52 完成交付：
  - `scripts/drill_archive_audit_sla_rollback_linkage_draft.sh`
  - `docs/plans/PHASE4_SLA_ROLLBACK_LINKAGE_DRILL_SCRIPT_DRAFT.md`
  - `docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_TEMPLATE.md`
  - `docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md`
- B51 收口文档：
  - `docs/plans/PHASE4_ANOMALY_RESPONSE_VALIDATION_DRILL_CHECKLIST_DRAFT.md`
- 本轮结果：
  - `test_ocsp_connection_verification_regression` 保持 `PASS=4, FAIL=0, SKIP=0`。
  - `test_p2_ocsp_comprehensive` 保持 `55/55`。
  - `compile_all_modules.py` 保持 `157/157`。
  - B52 strict 模式按预期 `exit 1`（`linkage_status=fail`）。
- 下一批主线任务：
  - `B53`：回写覆盖率修复闭环验收门禁草案（当前进行中）。
  - `B54`：回写覆盖率自动修复脚本草案。
  - `B55`：SLA/回滚联动报告归档验真脚本草案。
  - `B56`：闭环验收失败自动重试分流脚本草案。

## Iteration Addendum（2026-02-07 11:48 +0800）

- 主线任务完成：`B53`（writeback coverage closure acceptance gate）。
- 已完成交付：
  - `scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh`
  - `docs/plans/PHASE4_WRITEBACK_COVERAGE_CLOSURE_ACCEPTANCE_GATE_DRAFT.md`
  - `docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_TEMPLATE.md`
  - `docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md`
- 本轮结果：
  - dry-run、样例生成通过。
  - strict 模式按预期返回失败（样例数据未闭环）。
- 下一批主线任务：
  - `B54`：回写覆盖率自动修复脚本草案（当前进行中）。
  - `B55`：SLA/回滚联动报告归档验真脚本草案。
  - `B56`：闭环验收失败自动重试分流脚本草案。
  - `B57`：闭环门禁周趋势与漂移复核草案。
