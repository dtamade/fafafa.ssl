# Roadmap Autonomous Closure Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在“批次模式”下持续自治推进，完成当前路线图未闭环项（优先 B79/B80/B83/B81/B82），并以可验证证据宣布阶段收口。  
**Architecture:** 采用单通道滚动执行（WIP=1）：先刷新基线，再修复核心示例与 API 漂移，随后执行编译/模块门禁，最后发布进度报告与下一阶段计划。所有“完成”结论必须来自本轮新鲜命令输出。  
**Tech Stack:** Free Pascal 3.3.1、OpenSSL 3.x、项目脚本（`scripts/verify_examples_compile.sh`、`scripts/compile_all_modules.py`、`scripts/run_all_module_tests.sh`）、规划三件套（`task_plan.md`/`findings.md`/`progress.md`）。

---

## 执行约束（长期自治）

1. **批次模式固定节奏**：每批只推进一个主目标（WIP=1），每批必须有可验证交付。
2. **证据先行**：没有当轮命令输出，不允许更新为“完成/已修复/通过”。
3. **回写纪律**：每批结束必须同步 `task_plan.md`、`findings.md`、`progress.md`。
4. **优先级顺序**：B79 → B80 → B83 → B81 → B82。
5. **风险控制**：如果连续两轮未提升示例通过率，切换为“按失败类别分治”的拆分策略。

---

### Task 1: 刷新起始基线（Batch Entry）

**Files:**
- Modify: `task_plan.md`
- Modify: `docs/test_reports/EXAMPLES_COMPILE_FIX_TRACKER.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Create: `test-reports/examples_compile_latest.json`

**Step 1: 运行示例编译基线**

Run: `bash scripts/verify_examples_compile.sh -f json -o test-reports/examples_compile_latest.json`  
Expected: 允许失败退出码；生成 `examples_compile_latest.json`。

**Step 2: 运行全模块编译门禁**

Run: `python3 scripts/compile_all_modules.py`  
Expected: 输出 `157/157` 成功（如失败则先处理阻断再进入 B79）。

**Step 3: 回写基线到追踪文档**

Action: 在 `docs/test_reports/EXAMPLES_COMPILE_FIX_TRACKER.md` 新增“本轮基线”小节（时间戳、通过率、失败数）。

**Step 4: 标记自治批次状态**

Action: 在 `task_plan.md` 明确当前为 B79（in_progress），并记录下一批候选。

---

### Task 2: B79 核心示例修复（第一轮：语法与结构）

**Files:**
- Modify: `examples/01_tls_client.pas`
- Modify: `examples/02_generate_certificate.pas`
- Modify: `examples/03_file_encryption.pas`
- Modify: `examples/06_digital_signature.pas`
- Modify: `docs/test_reports/EXAMPLES_COMPILE_FIX_TRACKER.md`

**Step 1: 逐个编译核心失败示例并记录首个错误**

Run: `fpc -Mobjfpc -Sh -Fu./src examples/02_generate_certificate.pas`（对核心失败文件逐个执行）  
Expected: 收集每个文件“第一错误”。

**Step 2: 修复内联变量/ObjFPC 不兼容写法**

Action: 把 `var x := ...` 等写法改为传统 `var` 声明块；避免引入行为变化。

**Step 3: 复跑核心示例编译**

Run: 对 `01/02/03/06` 再次逐个 `fpc` 编译。  
Expected: 至少减少一类语法错误。

**Step 4: 更新追踪状态**

Action: 在 `EXAMPLES_COMPILE_FIX_TRACKER.md` 写入“已清除错误类别 + 剩余错误类别”。

---

### Task 3: B79 核心示例修复（第二轮：类型与依赖）

**Files:**
- Modify: `examples/02_generate_certificate.pas`
- Modify: `examples/03_file_encryption.pas`
- Modify: `examples/06_digital_signature.pas`
- Modify: `examples/certificate_verification_example.pas`

**Step 1: 修复类型不匹配**

Action: 处理 `PAnsiChar`/`PByte`、指针与字符串桥接，尽量用显式转换。

**Step 2: 修复缺失单元引用**

Action: 补齐必要 `uses`，移除不存在或已下线单元依赖。

**Step 3: 核心集合回归**

Run: `bash scripts/verify_examples_compile.sh -f json -o test-reports/examples_compile_after_b79.json`  
Expected: `passed` 高于起始基线；失败项集中到 API 变更类别。

---

### Task 4: B80 API 漂移示例修复

**Files:**
- Modify: `examples/example_https_api.pas`
- Modify: `examples/example_json_api.pas`
- Modify: `examples/example_streaming_operations.pas`
- Modify: `examples/example_aes_gcm_aead.pas`
- Modify: `examples/demo_fluent_api.pas`

**Step 1: 建立 API 映射表**

Action: 从 `src/` 当前接口确认已变更 API 的替代调用方式（旧名 → 新名/新签名）。

**Step 2: 最小化适配改动**

Action: 仅改调用层，不在示例中增加新的框架依赖。

**Step 3: 目标示例编译验证**

Run: 对上述文件逐个 `fpc -Mobjfpc -Sh -Fu./src ...`。  
Expected: 目标列表中的失败数下降。

**Step 4: 全量示例回归**

Run: `bash scripts/verify_examples_compile.sh -f json -o test-reports/examples_compile_after_b80.json`  
Expected: 通过率达到当前阶段目标（建议先达到 `>= 70%`，再冲刺 `>= 80%`）。

---

### Task 5: B83 路线图回归门禁

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Create: `test-reports/examples_compile_gate_b83.json`

**Step 1: 编译门禁**

Run: `python3 scripts/compile_all_modules.py`  
Expected: `157/157`。

**Step 2: P2 核心模块门禁**

Run: `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`  
Expected: 无失败。

**Step 3: 示例门禁**

Run: `bash scripts/verify_examples_compile.sh -f json -o test-reports/examples_compile_gate_b83.json`  
Expected: 达到当轮目标阈值；输出可追踪失败列表。

**Step 4: 决策回写**

Action: 满足阈值则关闭 B79/B80/B83；不满足则将失败项按类别拆分到下一批。

---

### Task 6: B81 周期进度报告发布

**Files:**
- Create: `docs/test_reports/ROADMAP_CLOSURE_PROGRESS_2026-02-08.md`
- Modify: `docs/DEVELOPMENT_ROADMAP_2026.md`
- Modify: `docs/DOCUMENTATION_INDEX.md`

**Step 1: 汇总关键指标**

Action: 写入示例通过率变化、模块门禁状态、未完成项清单、风险项。

**Step 2: 更新路线图主文档状态**

Action: 同步 `Phase 1A/3/4` 的真实进展，不覆盖历史结论。

**Step 3: 索引挂载**

Action: 将进度报告加入 `docs/DOCUMENTATION_INDEX.md`。

---

### Task 7: B82 下一阶段执行计划

**Files:**
- Create: `docs/plans/2026-02-08-roadmap-next-wave-plan.md`
- Modify: `task_plan.md`
- Modify: `docs/DEVELOPMENT_ROADMAP_2026.md`

**Step 1: 生成 Q2-Q3 任务波次**

Action: 输出 Wave A/B/C（跨平台、CI、性能、文档体验）的优先级与依赖。

**Step 2: 定义每个波次验收门禁**

Action: 每个波次必须包含“命令 + 通过阈值 + 证据路径”。

**Step 3: 回写自治队列**

Action: 在 `task_plan.md` 更新“进行中/下一批/阻塞”。

---

## 完成定义（DoD）

只有全部满足才可声明“路线图收口阶段完成”：

1. B79、B80、B83、B81、B82 全部从 `[ ]` 变为 `[x]`。
2. `python3 scripts/compile_all_modules.py` 最新结果为 `157/157`。
3. `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT` 最新结果无失败。
4. `scripts/verify_examples_compile.sh` 达到约定通过率阈值并有报告归档。
5. `docs/DEVELOPMENT_ROADMAP_2026.md` 与 `docs/DOCUMENTATION_INDEX.md` 同步到同一轮状态。

## 失败转向策略（3-Strike）

- **Strike 1**：局部修复 + 目标文件重编译。  
- **Strike 2**：按错误类别分批拆解，缩小改动面。  
- **Strike 3**：回退到最近稳定批次，改为“文档化阻断 + 下一轮专项批次”。

