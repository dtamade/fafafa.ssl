# fafafa.ssl 开发路线图（2026）

**文档目的**：把 2026 年的开发工作拆成可以执行、可以验证的里程碑，并给出每个阶段的“完成标准”（如何证明做完了）。  
**适用人群**：维护者、贡献者（需要知道下一步做什么、怎么验收）。  
**最后更新**：2026-02-08

> 历史版本与分析材料请参考：`docs/archive/DEVELOPMENT_ROADMAP_2026.md`（归档）。

---

## 现在处于什么状态（基线快照）

以下基线在 Linux（Debian 13 / x86_64）环境验证：

- OpenSSL：3.5.4（libcrypto.so.3）
- Free Pascal：3.3.1

## 自治收口快照（2026-02-08）

本轮在长期自治“批次模式”下刷新了门禁与示例状态：

- 编译门禁：`python3 scripts/compile_all_modules.py` → `157/157`（通过）
- P2 核心模块：`bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` → `15/15`（通过）
- Wave B 门禁脚本：`bash scripts/run_wave_b_ci_gate.sh --examples-threshold 80.0` → `PASS`（`test-reports/wave_b_ci_gate_summary_20260208_022636.md`）
- 示例编译通过率：
  - 启动基线：`36/75`（`50.7%`，`test-reports/examples_compile_latest.json`）
  - 当前门禁：`62/75`（`87.3%`，`test-reports/examples_compile_gate_b85.json`）

进展报告见：`docs/test_reports/ROADMAP_CLOSURE_PROGRESS_2026-02-08.md`。

### Phase 0（已完成）：编译 + P2 核心模块冒烟验证

运行：

```bash
# 1) P2 核心模块（Q1 主线）测试集
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT

# 2) Linux 环境批量编译门禁
python3 scripts/compile_all_modules.py
```

期望：
- P2 模块测试：0 失败（含 comprehensive 测试）
- 批量编译：157/157 成功（Linux 核心模块）

这份快照的结果摘要见：`docs/test_reports/P2_MODULES_TEST_REPORT.md`。

---

## 2026 年目标（可验收）

### 目标 1：P2 模块“生产级可验证”（Q1）

把“功能存在/测试能跑”升级为“可重复验证 + 离线夹具齐全 + 失败场景覆盖 + 文档/示例齐全”。

**验收标准（必须同时满足）**
- 所有 P2 模块在 OpenSSL 3.x 环境 **100% 通过**（含 comprehensive 测试）。
- 测试不依赖外网（证书链、样本数据、日志数据全部在仓库内或可脚本生成）。
- 每个模块至少包含：成功路径 + 失败路径（例如：密码错误、签名不匹配、过期/撤销等）。
- 文档/示例能让读者在本地复现（给出命令、输入、预期输出）。

### 目标 2：性能基线与关键优化（Q1–Q2）

先“可测”，再“可优化”，最后“可证明优化有效”。

**验收标准**
- 有可重复的基准脚本（同机器、同参数可复现）。
- 每个优化点都能给出“优化前/后”对比（吞吐、延迟、CPU、内存）。
- 不引入功能回归（关键测试集通过）。

### 目标 3：跨平台验证与发布工程化（Q2）

建立一个最低成本但可信的验证矩阵（Linux/macOS/Windows）。

**验收标准**
- 能在至少 2 种 OpenSSL 版本上跑通关键测试集（1.1.1 与 3.x，或等价替代矩阵）。
- Windows 的 WinSSL 主路径（Schannel）有可执行验证步骤（零依赖部署为核心卖点）。
- CI 至少覆盖：编译门禁 + 核心测试集（避免回归）。

### 目标 4：文档与开发者体验（持续）

目标是减少“读源码才能用”的场景。

**验收标准**
- 文档索引 `docs/DOCUMENTATION_INDEX.md` 保持可用（链接有效、入口清晰）。
- Quickstart/示例“按说明一步步跑得通”。

### 目标 5：可选后端原型（Q3）

以最小成本验证“能力矩阵/selector”对新后端扩展是否顺畅。

**验收标准**
- 原型可编译、可被 selector 选中、可解释“为什么选它/为什么不能选它”。

---

## 里程碑拆分（建议节奏）

### Q1（P2 验证补齐）

详细执行计划见：`docs/plans/Q1_2026_P2_VALIDATION_PLAN.md`。

1. P2 模块测试矩阵（命令、输入、预期、结果）固化成文档（持续更新）。
2. 离线夹具：
   - 证书链、PKCS#7/PKCS#12 样本、CT/TS/OCSP 测试数据。
   - 生成脚本（或固定文件）保证可重复。
3. 失败场景覆盖（每个模块至少 2–3 个高价值失败场景）。
4. OpenSSL 1.1.1 vs 3.x 差异清单（符号、弃用、行为差异）与回归策略。

### Q2（性能 + 跨平台）

1. 性能基线跑通并落盘（报告/脚本）。
2. 关键路径优化（优先：读写、证书链验证、会话复用）。
3. Linux/macOS/Windows 验证矩阵与 CI 最小闭环。

### Q3（可选后端原型）

1. MbedTLS（或其它）最小可用原型：先 client-only / TLS1.2 起步。
2. 与能力矩阵/selector 集成验证。

### Q4+（长期演进）

1. 纯 Pascal 后端（分阶段推进）。
2. QUIC / 异步 I/O（评估 + PoC）。

---

## 当前执行入口（长期自治）

- 收口主计划：`docs/plans/2026-02-08-roadmap-autonomous-closure-plan.md`
- 下一波次计划：`docs/plans/2026-02-08-roadmap-next-wave-plan.md`

---

## 贡献者应该从哪里开始

如果你要帮忙推进路线图，优先从这些“有明确验收”的任务开始：

1. 补一个失败场景测试（并确保可离线跑通）。
2. 把某个模块的“如何验证”写成可复现的步骤（命令 + 预期输出）。
3. 为某个模块补齐 OpenSSL 版本差异说明（带可复现证据/链接到代码位置）。

## 最新执行更新（2026-02-08 02:57 +0800）

- 示例编译门禁已从 `87.3%` 提升至 `100.0%`：
  - `test-reports/examples_compile_gate_b87.json`（`passed=71, failed=0, skipped=4`）
- Wave B Linux 最小 CI 门禁继续保持 PASS：
  - `test-reports/wave_b_ci_gate_summary_20260208_025426.md`
  - 三步门禁均 `exit=0`（compile/modules/examples）
- 路线图收口状态：
  - Wave A 示例修复闭环完成（失败项清零）；
  - Wave B/B1 稳定；下一阶段聚焦 B2（macOS/Windows 实机口径回填）。

## 稳健性补丁（2026-02-08 03:43 +0800）

- 修复 `scripts/verify_examples_compile.sh` JSON 输出边界条件：
  - 当无失败示例时，`failed_files` 由错误的 `[""]` 改为标准 `[]`。
- 复核证据：
  - `test-reports/examples_compile_gate_b88.json`
  - `test-reports/examples_compile_ci_gate.json`
  - `test-reports/wave_b_ci_gate_summary_20260208_034029.md`
- 影响：
  - 提升 Wave B/B2 跨平台汇总自动化稳定性，避免下游解析误判“存在空字符串失败项”。

## B89（跨平台汇总自动化）

- 脚本新增：`scripts/generate_wave_b_cross_platform_summary.sh`
- 样例产物：`test-reports/wave_b_cross_platform_summary_20260208_034029.md`
- 状态：已具备统一汇总能力，待 macOS/Windows 实机证据回填。

## B90（B2 执行入口）

- 新增 `scripts/run_wave_b_macos_gate.sh` 与 `scripts/run_wave_b_windows_gate.ps1`。
- macOS dry-run 样例：`test-reports/wave_b_macos_gate_summary_20260208_0350.md`。
- 下一阶段：在真实 macOS/Windows runner 执行并回填平台摘要。

## 最新执行更新（2026-02-08 04:04 +0800）

- B90 runner 收口校验完成：
  - `test-reports/wave_b_macos_gate_summary_20260208_041500.md`（`mode=dry-run`, `overall=DRY_RUN`）
- B91 汇总稳健性修复完成：
  - `scripts/generate_wave_b_cross_platform_summary.sh` 现在会解析平台摘要 `overall` 字段。
  - `test-reports/wave_b_cross_platform_summary_20260208_041500.md` 已显示 `macos=DRY_RUN`（不再误判为 `READY`）。
- 当前主线：继续推进 B2 实机回填（macOS/Windows），完成三平台最终闭环摘要。

## 最新执行更新（2026-02-08 04:08 +0800）

- B92 已交付 runner 命令块：
  - `docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`
- 可直接用于 CI/runner 的执行入口已就绪，待 macOS/Windows 实机证据回填后完成 B2 最终闭环。

## 最新执行更新（2026-02-08 04:18 +0800）

- B93 已完成 B2 闭环判定自动化：
  - 新增 `scripts/check_wave_b_b2_closure_readiness.sh`
  - 当前样例状态：`test-reports/wave_b_b2_closure_readiness_20260208_041500.md` => `IN_PROGRESS`
- 该脚本支持 `--strict` 非 0 返回，可直接用于 CI gate。

## 最新执行更新（2026-02-08 04:26 +0800）

- B94 已落地 Wave B/B2 手动流水线模板：`.github/workflows/wave-b-b2-manual.yml.disabled`（历史模板保留）
- 2026-03-14：已启用版工作流为 `.github/workflows/wave-b-b2-manual.yml`（并保留 `.disabled` 备份）
- 模板内已串联 cross-summary 与 closure readiness，两者可在一次 dispatch 内完成。

## 最新执行更新（2026-02-08 04:30 +0800）

- B95 已完成 B2 证据一致性自动化：
  - 新增 `scripts/check_wave_b_b2_evidence_consistency.sh`
  - 可检测 run_id 混用与证据缺失，并在 strict 模式下直接门禁失败。

## 最新执行更新（2026-02-08 04:35 +0800）

- B96 已完成模板流水线接线：
  - summary job 同时产出 cross summary / closure readiness / evidence consistency
  - strict 模式支持 dry-run 门禁阻断

## 最新执行更新（2026-02-08 04:38 +0800）

- B97 已交付 B2 一键交接包脚本：`scripts/prepare_wave_b_b2_handoff_bundle.sh`
- 可直接输出 handoff_state 与 strict 阻断信号，便于 runner 团队接手执行。

## 最新执行更新（2026-02-08 04:42 +0800）

- B98 已完成 Wave C 基线预备验证：
  - `scripts/run_phase2_performance_baseline.sh` dry-run + 实跑通过（skip TLS）
  - 产物：`tests/benchmarks/results/benchmark_summary_20260208_042710.txt`
  - readiness：`docs/test_reports/WAVE_C_BASELINE_READINESS_2026-02-08.md`

## 最新执行更新（2026-02-08 04:44 +0800）

- B99 已完成 Wave C 优化候选清单：`docs/plans/WAVE_C_B99_OPTIMIZATION_SHORTLIST_2026-02-08.md`
- 已补 cert verify cache 探针证据，B100 可直接进入 P1 对照验证。

## 最新执行更新（2026-02-08 04:49 +0800）

- B100 完成 P1 候选对照验证：`WAVE_C_B100_P1_COMPARISON_2026-02-08.md`
- Cert verify cache 候选在重复探针下保持高收益，进入 B101 接入验证准备。

## 最新执行更新（2026-02-08 04:52 +0800）

- B101 已完成最小接入验证框架：
  - validation playbook 脚本 + integration validation plan
- B102 将在此基础上推进单点接入草案。

## 最新执行更新（2026-02-08 04:58 +0800）

- B102 已形成单点接入实施草案，明确“先单点、后扩展”的风险隔离策略。
