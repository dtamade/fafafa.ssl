# FreePascal Server-Side OCSP Stapling Public Closeout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把已落地的 FreePascal server-side OCSP stapling issuance 收口成正式 public API，并同步 capability truth / docs，不再继续把它表述成未实现。

**Architecture:** 本轮不再扩新的 revocation/CT/early-data 行为线，只做最后一公里闭环。实现分三层：先用 capability/builder/runtime contract 做 RED，证明 `KnownIssues` 仍然落后、public surface 仍缺；然后最小扩一个可选 public context interface 和对应 builder file-based 配置入口，FreePascal backend 直接复用当前已落地的 backend-private stapling seam；最后收紧 `KnownIssues`、roadmap 和 OCSP/API 文档，并跑 focused tests、focused gate、compile gate、diff hygiene。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.base`, `fafafa.ssl.context.builder`, `TFreePascalContext`, TLS 1.3 scripted server runtime tests, builder import/export tests, bash focused gate, file-based working memory.

---

## Scope

- 收这 4 件事：
  1. 把 server-side stapling 从 backend-private seam 提升成 public optional context interface
  2. 给 `TSSLContextBuilder` 增加 server stapled OCSP response file 配置入口，并接入 build/import/export/override
  3. 收紧 FreePascal `KnownIssues`，不再把 server-side stapling issuance 写成未实现
  4. 同步 OCSP/API/roadmap 文档到当前真相
- 明确不做：
  - 自动 online OCSP fetch / refresh / responder
  - broader early-data / CT / client revocation 继续扩线
  - 修改 focused gate inventory（当前 server runtime 已在 gate 内）

## Task 1: RED - Tighten capability truth and public-surface contracts

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/test_freepascal_server_ocsp_stapling_runtime.pas`

**Step 1: Tighten `KnownIssues` expectations**

- 收紧 FreePascal capability tests：
  - 不再允许 `KnownIssues` 包含 `server-side` / `OCSP stapling issuance is not implemented`
  - 继续要求保留 `0-RTT` / `anti-replay`
- focused RED 命令：

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

Expected:
- FAIL，失败点指向 `KnownIssues` 仍把 server-side stapling issuance 写成未实现。

**Step 2: Add builder/public API RED**

- `tests/test_transformation_methods.pas`
  - 断言新的 builder field（建议名：`server_ocsp_stapled_response_file`）能被 `Override(...)` 导出
- `tests/config/test_config_import_export.pas`
  - 断言 JSON / INI round-trip 能保留 `server_ocsp_stapled_response_file`
- `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
  - 新增 builder-driven contract：
    - `BuildServer` + public server stapling config 应可直接驱动现有 scripted accept path 发出 stapled response
    - context 应暴露 public optional server stapling interface，而不是只剩 backend-private seam

Run:

```bash
mkdir -p tmp/test_transformation_methods && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_transformation_methods -FEtmp/test_transformation_methods -otmp/test_transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods/test_transformation_methods
```

```bash
mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export
```

```bash
mkdir -p tmp/freepascal_server_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_ocsp_stapling_runtime -FEtmp/freepascal_server_ocsp_stapling_runtime -otmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime tests/test_freepascal_server_ocsp_stapling_runtime.pas && ./tmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime
```

Expected:
- FAIL，失败点分别指向：
  - builder 还没有 server stapling config field
  - import/export 还不会保留该 field
  - public optional interface / builder-driven runtime 还不存在

## Task 2: GREEN - Promote server stapling to public optional API and builder config

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/test_freepascal_server_ocsp_stapling_runtime.pas`

**Step 1: Add public optional context interface**

- 在 `fafafa.ssl.base.pas` 新增一个可选 public interface，范围只收 server stapling config：
  - clear
  - set bytes
  - load file
  - has/get bytes
- 在 `fafafa.ssl.pas` 导出该接口
- FreePascal context 实现该 public interface，并复用当前已存在的 stapling material seam
- 不扩强制性的 `ISSLContext` 核心方法

**Step 2: Add builder field + file-based config**

- 在 `TSSLContextBuilder` 增加：
  - `WithServerOCSPStapledResponseFile(const AFile: string): ISSLContextBuilder`
- 在 builder 内保存 `server_ocsp_stapled_response_file`
- 接入：
  - `BuildServer`
  - `ExportToJSON`
  - `ImportFromJSON`
  - `ExportToINI`
  - `ImportFromINI`
  - `Clone`
  - `Reset`
  - `Merge`
  - `Override`
- `BuildServer` 时：
  - 若配置了 stapled response file 且 context 支持 public optional interface，则 load file
  - 若配置了 stapled response file 但 backend 不支持该 interface，则直接给出清晰配置错误

**Step 3: Re-run GREEN**

Run Task 1 的 5 条 focused 命令。

Expected:
- PASS。

## Task 3: GREEN - Close capability truth and docs

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`
- Modify: `docs/guides/security-best-practices.md`
- Modify: `docs/reference/API_DOCUMENTATION.md`
- Modify: `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- Modify: `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md`

**Step 1: Tighten `KnownIssues`**

- 去掉 `FreePascal server-side OCSP stapling issuance is not implemented`
- `KnownIssues` 只保留当前仍真实存在的 `0-RTT / early data experimental + single-process anti-replay` 边界

**Step 2: Sync docs to public-closeout truth**

- `OCSP_USAGE_GUIDE.md`
  - 不再把 FreePascal server-side stapling 写成剩余缺口
  - 增加 public optional interface / builder file config 的最短用法
- `security-best-practices.md`
  - 更新 OCSP 剩余边界描述
- `API_DOCUMENTATION.md`
  - 补充新的 public optional interface 和 builder 方法
  - 服务端配置章节改成当前真实可用语义
- `freepascal-validation-next-wave-roadmap.md`
  - 记录原“最后剩余 gap”已关闭
- `freepascal-server-side-ocsp-stapling-issuance-next-stage.md`
  - 改成 closeout note / superseded 状态，避免继续把它当 future queue

## Task 4: Final verification and ledger write-back

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Focused verification**

Run:

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

```bash
mkdir -p tmp/test_transformation_methods && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_transformation_methods -FEtmp/test_transformation_methods -otmp/test_transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/test_transformation_methods/test_transformation_methods
```

```bash
mkdir -p tmp/test_config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_config_import_export -FEtmp/test_config_import_export -otmp/test_config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export/test_config_import_export
```

```bash
mkdir -p tmp/freepascal_server_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_ocsp_stapling_runtime -FEtmp/freepascal_server_ocsp_stapling_runtime -otmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime tests/test_freepascal_server_ocsp_stapling_runtime.pas && ./tmp/freepascal_server_ocsp_stapling_runtime/test_freepascal_server_ocsp_stapling_runtime
```

```bash
mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_ocsp_stapling_runtime -FEtmp/freepascal_client_ocsp_stapling_runtime -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime tests/test_freepascal_client_ocsp_stapling_runtime.pas && ./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime
```

**Step 2: Focused gate + compile gate**

Run:

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id fp_server_stapling_public_closeout_20260411
```

```bash
python3 scripts/compile_all_modules.py
```

**Step 3: Diff hygiene**

Run:

```bash
git diff --check -- docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-public-closeout.md docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/reference/API_DOCUMENTATION.md src/fafafa.ssl.base.pas src/fafafa.ssl.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas tests/test_transformation_methods.pas tests/config/test_config_import_export.pas tests/test_freepascal_server_ocsp_stapling_runtime.pas tests/test_freepascal_client_ocsp_stapling_runtime.pas task_plan.md findings.md progress.md
```

**Step 4: Write back ledgers**

- `task_plan.md` 顶部新增本轮批次
- `findings.md` 顶部记录：
  - 为什么选 public optional interface，而不是扩核心 `ISSLContext`
  - builder file-based config 与 runtime bytes seam 的分层
  - `KnownIssues` 收口到 0-RTT experimental truth
- `progress.md` 顶部记录 RED/GREEN 与验证命令

## Definition Of Done

- FreePascal server-side stapling 不再只是 backend-private seam，而是有 public optional context interface + builder file config。
- `KnownIssues` / docs / roadmap 不再把 server-side stapling issuance 写成未实现。
- focused builder/runtime tests、focused gate、compile gate、diff hygiene 全绿。
