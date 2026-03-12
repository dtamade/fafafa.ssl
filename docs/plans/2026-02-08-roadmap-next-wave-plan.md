# 路线图下一波次执行计划（Q2-Q3）

> **Batch**: B82  
> **Status**: updated  
> **生成时间**: 2026-02-08 01:32 +0800

## 目标

在已完成 B79/B83 基础上，继续以长期自治批次推进路线图收口：

1. 将示例编译通过率从 `74.6%` 提升到 `87.3%`（Wave A 已完成并持续推进）；
2. 固化跨平台/CI 最小门禁（Wave B）；
3. 启动 Phase 2 优化闭环（Wave C）。

---

## Wave A：示例收口冲刺（优先级 P0，已完成）

**目标阈值**：`examples pass_rate >= 80%`（已达成并提升至：`87.3%`）  
**门禁命令**：

```bash
bash scripts/verify_examples_compile.sh -f json -o docs/archive/reports/examples-compile-history/examples_compile_gate_b85.json
```

### A1：API/模块下线替换（高收益）

- 处理 `example_json_api.pas`（移除 `fafafa.ssl.http.json` 依赖，改为 `example_https_api` 同款最小 HTTP + 手工 JSON 提取）。
- 处理 `simple_ssl_connection.pas`（迁移到 `fafafa.examples.tcp` 封装）。

**验收**：完成（本轮新增转绿 7 个示例）。

### A2：AES/PKCS7 系列签名兼容

- 处理 `example_aes_gcm_aead.pas`
- 处理 `pkcs7_*` 失败组（优先 `pkcs7_sign_verify_simple.pas`）

**验收**：完成（pkcs7 失败组减少，`pkcs7_basic_example` 与 `pkcs7_sign_verify_simple` 转绿）。

### A3：密码学辅助示例对齐

- 处理 `password_hash_v2.pas`、`password_hash/password_hash.pas`
- 统一 `BytesToHex`、错误码输出与新 Result 类型适配。

**验收**：部分完成（`password_hash_v2.pas` 转绿，`password_hash/password_hash.pas` 仍待处理）。

---

## Wave B：跨平台与 CI 最小可用闭环（优先级 P1）

### B1：Linux 端最小 CI 命令闭环（已完成）

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
bash scripts/verify_examples_compile.sh -f json -o docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json
```

**验收**：已完成（新增 `scripts/run_wave_b_ci_gate.sh`，报告 `docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_022636.md`，overall PASS）。

### B2：macOS/Windows 门禁脚本核验

- macOS：`scripts/detect_macos_openssl_enhanced.sh`
- Windows：复用既有 WinSSL 草案命令并在文档内给出执行口径。

- 当前执行清单：`docs/test_reports/WAVE_B_CROSS_PLATFORM_GATE_MANIFEST_2026-02-08.md`（B2 in_progress）

**验收**：`docs/plans/` 形成可执行命令清单 + 产物路径标准。

---

## Wave C：Phase 2 性能优化闭环（优先级 P2）

### C1：基线重跑

使用既有基线脚本重新生成同机对比样本。

**验收**：新报告与 `PHASE2_BASELINE_COMPARISON_V1` 可并排对比。

### C2：一项可证明优化

从以下候选中至少实现一项：

- 证书链验证缓存命中优化
- 会话缓存策略优化
- 热点路径减少内存拷贝

**验收命令**：

- 优化前后均能通过：
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`
- 性能指标出现可解释改进。

---

## 波次节奏（建议）

- Week 1：Wave A（示例通过率冲刺，已完成）
- Week 2：Wave B（CI/跨平台门禁）
- Week 3：Wave C（性能闭环）

每周输出：

- 一份 `docs/test_reports/ROADMAP_CLOSURE_PROGRESS_YYYY-MM-DD.md`
- 一次门禁三件套证据（编译、P2模块、示例）


## 执行进展回填（2026-02-08 02:57 +0800）

- 示例通过率目标已超额完成：`87.3% -> 100.0%`。
  - 证据：`docs/archive/reports/examples-compile-history/examples_compile_gate_b87.json`
- Wave B / B1 脚本化门禁持续通过：
  - 证据：`docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_025426.md`
- 后续主线调整：
  1. 保持 Linux 门禁按批次复跑；
  2. 推进 B2（macOS/Windows runner）并回填统一格式报告。

## B88 回填（2026-02-08 03:43 +0800）

- 已完成：门禁脚本稳健性收口（`failed_files` 空数组格式修复）。
- 验证：
  - `docs/archive/reports/examples-compile-history/examples_compile_gate_b88.json`（`failed_files=[]`）
  - `docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md`（overall PASS）
- 下一步主线不变：推进 B2 实机回填（macOS/Windows）。

## B89 回填（2026-02-08 03:49 +0800）

- 已新增 `scripts/generate_wave_b_cross_platform_summary.sh` 作为 B2 汇总入口。
- 已生成样例：`docs/archive/reports/wave-b-history/wave_b_cross_platform_summary_20260208_034029.md`。
- 后续动作：在 macOS/Windows runner 回填实测报告后，复跑同一脚本得到最终三平台闭环摘要。

## B90 回填（2026-02-08 03:55 +0800）

- 已新增 runner 封装入口：
  - `scripts/run_wave_b_macos_gate.sh`
  - `scripts/run_wave_b_windows_gate.ps1`
- 现阶段可在 CI runner 直接执行并产出平台摘要，再接入 B89 汇总脚本。

## B90-B91 进展回填（2026-02-08 04:04 +0800）

- B90：已完成 runner 收口校验。
  - `scripts/run_wave_b_macos_gate.sh` dry-run 输出已确认 `overall=DRY_RUN`。
  - `scripts/run_wave_b_windows_gate.ps1` 保持待 Windows runner 实机执行。
- B91：已完成跨平台汇总状态解析修复。
  - `scripts/generate_wave_b_cross_platform_summary.sh` 已支持从平台摘要中解析 `overall` 并映射状态。
  - 样例：`docs/archive/reports/wave-b-history/wave_b_cross_platform_summary_20260208_041500.md`（`macos=DRY_RUN`）。

### 下一批（B92）

- 目标：形成“可直接粘贴到 CI Job”的 macOS/Windows 执行命令块，并等待两平台实机证据回填。
- 验收：
  1. `wave_b_macos_gate_summary_<run_id>.md`（live）
  2. `wave_b_windows_gate_summary_<run_id>.md`（live）
  3. 更新后的 `wave_b_cross_platform_summary_<run_id>.md`（三平台可横向比较）

## B92 回填（2026-02-08 04:08 +0800）

- 已新增实机命令块文档：`docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`。
- 当前 B2 最小闭环路径已明确：
  1. macOS live summary
  2. Windows live summary
  3. cross-platform final summary

## B93 回填（2026-02-08 04:18 +0800）

- 已新增：`scripts/check_wave_b_b2_closure_readiness.sh`
- 已接入：`docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`（Section F）
- 当前 B2 闭环状态可自动判定，下一批进入 CI job 模板化（B94）。

## B94 回填（2026-02-08 04:26 +0800）

- 已新增 `workflow_dispatch` 模板：`.github/workflows/wave-b-b2-manual.yml.disabled`
- 覆盖 Linux/macOS/Windows 三平台证据与最终 closure readiness 判定。
- 下一批：B95（证据完整性校验脚本接入交接包）。

## B95 回填（2026-02-08 04:30 +0800）

- 已新增：`scripts/check_wave_b_b2_evidence_consistency.sh`
- 已接入：`docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`（Section H）
- 下一批：B96（把一致性校验接入 workflow summary job）。

## B96 回填（2026-02-08 04:35 +0800）

- 已完成：将 evidence consistency 校验接入 `wave-b-b2-manual` 的 summary job。
- 下一批：B97（输出最终交接包并冻结 B2 待实机清单）。

## B97 回填（2026-02-08 04:38 +0800）

- 已新增：`scripts/prepare_wave_b_b2_handoff_bundle.sh`
- 已接入：`docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`（Section J）
- 下一批：B98（Wave C 性能闭环预备：基线脚本与最小验收链路复核）。

## B98 回填（2026-02-08 04:42 +0800）

- 已完成 Wave C baseline 预备验证（dry-run + 低迭代实跑）。
- 新增 readiness 报告：`docs/test_reports/WAVE_C_BASELINE_READINESS_2026-02-08.md`。
- 下一批：B99（输出最小优化候选清单与验收命令）。

## B99 回填（2026-02-08 04:44 +0800）

- 已新增优化候选清单：`docs/plans/WAVE_C_B99_OPTIMIZATION_SHORTLIST_2026-02-08.md`
- 已完成 cert verify cache 探针：`docs/test_reports/WAVE_C_B99_CERT_VERIFY_CACHE_PROBE_2026-02-08.md`
- 下一批：B100（执行 P1 候选的最小对照验证链路）。

## B100 回填（2026-02-08 04:49 +0800）

- 已完成 P1 候选重复对照验证：`docs/test_reports/WAVE_C_B100_P1_COMPARISON_2026-02-08.md`。
- 下一批：B101（业务链路最小接入验证计划与回归门禁绑定）。

## B101 回填（2026-02-08 04:52 +0800）

- 已新增：
  - `scripts/run_wave_c_b101_validation_playbook.sh`
  - `docs/plans/WAVE_C_B101_MINIMAL_INTEGRATION_VALIDATION_PLAN_2026-02-08.md`
- 下一批：B102（P1 单点接入实施草案与风险隔离策略）。

## B102 回填（2026-02-08 04:58 +0800）

- 已新增：`docs/plans/WAVE_C_B102_SINGLE_POINT_INTEGRATION_DRAFT_2026-02-08.md`
- 下一批：B103（按草案执行开关骨架 + 单点接入最小实现）。
