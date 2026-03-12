# Repository Scan Driven Next-Wave Development Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 基于 2026-02-24 的仓库扫描结果，优先收敛 3 个高影响缺口：`Wave B/B2 Windows 实机证据链`、`Cross-Platform Summary 状态机语义`、`OpenSSL 证书验证缓存有效命中策略可配置化`。

**Architecture:** 采用“三轨并行、批次串行验收”策略：先补证据闭环（避免流程阻塞），再收敛状态机语义（避免报告歧义），最后落地 OpenSSL 缓存策略开关（性能与确定性平衡）。每个任务都执行 RED -> GREEN -> Regression，并在批次末统一跑 compile/modules/script-contract/docs-gov 门禁。

**Tech Stack:** Bash, PowerShell, FreePascal (ObjFPC), OpenSSL backend, existing script contract test suite (`tests/scripts/*.sh`)。

---

## Scan Snapshot (2026-02-24)

- 规模快照：
  - `src/`: 192 files
  - `tests/`: 1052 files
  - `scripts/`: 135 files
- 后端分布（`src/` 文件名关键字）：
  - `openssl`: 75
  - `winssl`: 13
  - `mbedtls/wolfssl/boringssl`: 16
  - `freepascal`: 4
- 高信号热点：
  - `src/fafafa.ssl.openssl.connection.pas`: valid cache hit 仍会 refresh `X509_verify_cert`。
  - `scripts/generate_wave_b_cross_platform_summary.sh`: `TODO/READY/PENDING` 混合状态，报告语义可读性一般。
  - `test-reports/` 当前无 `wave_b_windows_gate_summary_*.md`，B2 Windows 实机证据缺口仍在。
- 现有流程约束：
  - 脚本变更需合同测试（`tests/scripts/*.sh`）+ `bash -n`。
  - Pascal 核心变更需 `python3 scripts/compile_all_modules.py` + 相关 focused tests。

## Priority Queue

- **P0**: 回填 Windows 实机证据并跑通 B2 一致性 strict。
- **P0**: 收敛 cross-summary 状态机语义，消除 `TODO` 模糊态在关键 checklist 的外溢。
- **P1**: OpenSSL valid cache hit 验证策略改为可配置，保留默认兼容语义并补测试。
- **P1**: 门禁批次回归（scripts + compile + modules + docs governance strict）。

---

### Task 1: Wave B/B2 Windows Evidence Closure (P0)

**Files:**
- Use: `scripts/run_wave_b_windows_gate.ps1`
- Use: `scripts/generate_wave_b_cross_platform_summary.sh`
- Use: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Use: `scripts/check_wave_b_b2_closure_readiness.sh`
- Evidence output: `test-reports/wave_b_windows_gate_summary_<run_id>.md`
- Evidence output: `test-reports/wave_b_cross_platform_summary_<run_id>.md`
- Evidence output: `test-reports/wave_b_b2_evidence_consistency_<run_id>.md`

**Step 1: RED - 复现缺失证据导致 strict fail**

Run:
```bash
RUN_ID="wb2_live_$(date +%Y%m%d_%H%M%S)"
bash scripts/check_wave_b_b2_evidence_consistency.sh \
  --run-id "$RUN_ID" \
  --linux-summary "test-reports/wave_b_ci_gate_summary_${RUN_ID}.md" \
  --linux-examples "test-reports/examples_compile_ci_gate_${RUN_ID}.json" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --cross-summary "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md" \
  --closure-report "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md" \
  --output "test-reports/wave_b_b2_evidence_consistency_${RUN_ID}.md" \
  --strict
```
Expected:
- 非 0 退出。
- 报告包含 windows evidence 缺失行。

**Step 2: GREEN - 在 Windows Runner 生成实机 summary**

Run (Windows):
```powershell
powershell -ExecutionPolicy Bypass -File scripts/run_wave_b_windows_gate.ps1 -RunId $env:RUN_ID -OutputDir test-reports
powershell -ExecutionPolicy Bypass -File scripts/run_wave_b_windows_gate.ps1 -RunId "${env:RUN_ID}_skip" -OutputDir test-reports -SkipWinsslBlockerBatch
```
Expected:
- 生成 `wave_b_windows_gate_summary_*.md`。
- `overall` 为 `PASS` 或可解释的 `FAIL`（必须含日志证据路径）。

**Step 3: 汇总 cross-platform summary**

Run:
```bash
bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id "$RUN_ID" \
  --linux-summary "test-reports/wave_b_ci_gate_summary_${RUN_ID}.md" \
  --linux-examples "test-reports/examples_compile_ci_gate_${RUN_ID}.json" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --output "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md"
```
Expected:
- 成功生成 cross summary。
- Windows 列不再是纯缺失占位。

**Step 4: Regression - strict consistency + closure readiness**

Run:
```bash
bash scripts/check_wave_b_b2_evidence_consistency.sh --run-id "$RUN_ID" --strict \
  --linux-summary "test-reports/wave_b_ci_gate_summary_${RUN_ID}.md" \
  --linux-examples "test-reports/examples_compile_ci_gate_${RUN_ID}.json" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --cross-summary "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md" \
  --closure-report "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md" \
  --output "test-reports/wave_b_b2_evidence_consistency_${RUN_ID}.md"

bash scripts/check_wave_b_b2_closure_readiness.sh --run-id "$RUN_ID" --strict \
  --cross-summary "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md" \
  --output "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md"
```
Expected:
- 两条 strict 均通过。

**Step 5: 记录实机证据**

Run:
```bash
ls -1 test-reports/wave_b_windows_gate_summary_${RUN_ID}.md \
      test-reports/wave_b_cross_platform_summary_${RUN_ID}.md \
      test-reports/wave_b_b2_evidence_consistency_${RUN_ID}.md
```
Expected:
- 三类证据路径都存在。

---

### Task 2: Cross-Platform Summary State Model Hardening (P0)

**Files:**
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`
- Create: `tests/scripts/test_wave_b_cross_platform_summary_missing_state_contract.sh`
- Regression: `tests/scripts/test_wave_b_cross_platform_summary.sh`
- Regression: `tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`

**Step 1: RED - 新增缺失态合同**

Create contract to enforce:
- 平台证据缺失时输出 `MISSING`（或统一约定值），不再输出 `TODO`。
- 已提供平台 summary 时不得出现 `TODO`。

Run:
```bash
bash tests/scripts/test_wave_b_cross_platform_summary_missing_state_contract.sh
```
Expected:
- 初次 FAIL（当前脚本仍会输出 `TODO`）。

**Step 2: GREEN - 最小改造状态归一化**

Implement in `scripts/generate_wave_b_cross_platform_summary.sh`:
- 收敛状态集合到：`PASS|FAIL|DRY_RUN|MISSING|SKIPPED|UNKNOWN`。
- 仅在内部使用默认值，不在对外报告暴露 `TODO`。
- 兼容现有 Windows `winssl/openssl/modules` 组合逻辑。

**Step 3: 回归执行（核心合同）**

Run:
```bash
bash tests/scripts/test_wave_b_cross_platform_summary_missing_state_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh
```
Expected:
- 全部 PASS。

**Step 4: 语法门禁**

Run:
```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
```
Expected:
- exit 0。

**Step 5: 关联一致性合同补跑**

Run:
```bash
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh
```
Expected:
- PASS（确保状态模型变更未破坏 B2 一致性解析）。

---

### Task 3: OpenSSL Cert Verify Cache Valid-Hit Policy Toggle (P1)

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `tests/test_transformation_methods.pas`
- Create: `tests/openssl/test_openssl_cert_verify_cache_policy.pas`

**Step 1: RED - 先写失败测试**

- Builder 层：新增测试验证新策略开关可配置并保留默认行为。
- OpenSSL 层：新增策略测试，验证 `valid cache hit` 在“关闭 refresh”时跳过 `X509_verify_cert` 路径。

Run:
```bash
fpc -Fu./src -Fi./src tests/test_transformation_methods.pas -otmp/test_transformation_methods && ./tmp/test_transformation_methods
fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy
```
Expected:
- RED 阶段至少一条 FAIL（新选项/新语义尚未实现）。

**Step 2: GREEN - 最小实现**

Implement:
- 在 `TSSLOption` 末尾追加新选项（避免历史序号漂移）。
- 在 builder 增加对应 fluent 方法并传递到 `FOptions`。
- 在 `TOpenSSLConnection` 证书链验证处，根据选项决定 valid-hit 是否 refresh `X509_verify_cert`。

**Step 3: Focused regression**

Run:
```bash
fpc -Fu./src -Fi./src tests/test_transformation_methods.pas -otmp/test_transformation_methods && ./tmp/test_transformation_methods
fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy
fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard
```
Expected:
- 三条 PASS。

**Step 4: Compile gate**

Run:
```bash
python3 scripts/compile_all_modules.py
```
Expected:
- 全量模块 100% 通过（当前基线 179/179）。

**Step 5: Optional benchmark sanity**

Run:
```bash
fpc -Fu./src -Fu./src/openssl -Fu./tests/benchmarks -Fu./examples -Fi./src tests/benchmarks/benchmark_cert_verify_cache.pas -otmp/benchmark_cert_verify_cache && ./tmp/benchmark_cert_verify_cache
```
Expected:
- benchmark 可执行且无异常崩溃。

---

### Task 4: Batch Regression & Docs Governance Closure (P1)

**Files:**
- Use: `scripts/run_all_module_tests.sh`
- Use: `scripts/scan_active_docs_noise_draft.sh`
- Use: `scripts/check_docs_index_dedup_draft.sh`
- Use: `tests/scripts/test_*` (changed scope)

**Step 1: Script contracts (changed scope)**

Run:
```bash
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh
bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh
```
Expected:
- 全部 PASS。

**Step 2: Bash syntax gate**

Run:
```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n scripts/run_wave_b_windows_gate.ps1 2>/dev/null || true
```
Expected:
- bash 脚本语法检查通过。

**Step 3: Pascal compile + module gate**

Run:
```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```
Expected:
- compile 100% 通过。
- 指定模块测试无失败。

**Step 4: Docs governance strict batch**

Run:
```bash
bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_post_batch.md
bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_post_batch.md
```
Expected:
- `total_hits=0`，`duplicate_paths=0`，`duplicate_titles=0`。

**Step 5: Evidence writeback**

Update:
- `task_plan.md`
- `findings.md`
- `progress.md`

Expected:
- 记录本批次命令、结果、阻塞项与下一队列。

---

## Execution Order

1. Task 1 (P0)
2. Task 2 (P0)
3. Task 3 (P1)
4. Task 4 (P1)

## Blockers & Escalation Rules

- 若 Windows runner 不可用，Task 1 标记 `BLOCKED`，保留 RED 证据并先执行 Task 2/3。
- 任一任务同类错误连续 3 次，停止重试并在 `findings.md` 记录已尝试路径后再升级决策。

