# FreePascal Validation Closeout And Focused Gate Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 FreePascal validation 下一波已完成实现做成一次真实 closeout：收紧 `KnownIssues` truth、扩 focused gate、更新 roadmap / ledger、同步对外文档。

**Architecture:** 这批不再新增 validation 行为，只做 closeout truth alignment。先在 capability wording 和 focused gate 上写出会失败的 contract，证明当前 drift 仍然存在；再做最小 GREEN：`KnownIssues` 改成真实剩余边界、focused gate 纳入高价值 validation runtime tests、路线图从“待执行”改成“已完成收口”，并同步 OCSP/CT 文档。最后用 focused tests、contract script、gate dry-run、compile gate 和 diff hygiene 收尾。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalSSLLibrary.GetCapabilities`, focused bash gate script, Pascal capability tests, markdown docs, file-based working memory.

---

## Scope

- 收这 4 件事：
  1. `KnownIssues` / capability truth alignment
  2. `run_freepascal_tls13_completeness_gate.sh` focused gate 扩容
  3. roadmap / ledger closeout
  4. OCSP / CT 对外文档 truth alignment
- 明确不做：
  - 新的 revocation / CRL material plumbing
  - 新的 CT / OCSP 运行时实现
  - 任何超出 closeout truth 的 capability 升级

## Task 1: RED - Capability wording drift

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: 收紧 `KnownIssues` 断言**

- 断言 `KnownIssues` 仍然提到真实剩余边界，但不再提：
  - `OCSP-DELIVERED`
  - `TRANSPARENCY` 的旧 CT 缺口表达
  - `OCSP VALIDATION HARDENING`
  - `BROADER CERTIFICATE VALIDATION HARDENING`
- 保留要继续出现的真实边界：
  - `0-RTT`
  - `ANTI-REPLAY`
  - `OCSP`
  - 更窄的 certificate validation wording（例如 revocation / CRL material / broader revocation evidence）

**Step 2: 跑 focused RED**

Run:

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework   -FUtmp/freepascal_backend_basic   -FEtmp/freepascal_backend_basic   -otmp/freepascal_backend_basic/test_freepascal_backend_basic   tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework   -FUtmp/capability_cache   -FEtmp/capability_cache   -otmp/capability_cache/test_capability_cache   tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

Expected:
- FAIL，失败点指向过期 `KnownIssues` wording。

## Task 2: RED - Focused gate inventory drift

**Files:**
- Modify: `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`

**Step 1: 扩 contract test 期望**

- 让 dry-run 必须提到这些新增 runtime tests：
  - `tests/test_freepascal_client_chain_trust_runtime.pas`
  - `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
  - `tests/test_freepascal_client_online_ocsp_runtime.pas`
  - `tests/test_freepascal_client_ct_sct_surface.pas`
  - `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- 把 fake FPC 调用次数从 `10` 收紧到 `15`。
- 保留 summary report row 断言。

**Step 2: 跑 gate contract RED**

Run:

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

Expected:
- FAIL，失败点指向 gate inventory 还没纳入新增 runtime tests。

## Task 3: GREEN - Minimal truth alignment

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `scripts/run_freepascal_tls13_completeness_gate.sh`

**Step 1: 收紧 `KnownIssues`**

- 只保留真实剩余边界。
- 不再把已完成的：
  - OCSP-delivered CT source parity
  - broader OCSP validation hardening
  - broader certificate validation hardening
  写成 pending。

**Step 2: 扩 focused gate**

- 把 5 个高价值 validation runtime tests 直接纳入 `TEST_NAMES` / `TEST_FILES`。
- 保持脚本接口、`--fast-local` 目录策略、PATH-resolved `fpc` 行为不变。

**Step 3: 跑 GREEN**

Run:

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework   -FUtmp/freepascal_backend_basic   -FEtmp/freepascal_backend_basic   -otmp/freepascal_backend_basic/test_freepascal_backend_basic   tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework   -FUtmp/capability_cache   -FEtmp/capability_cache   -otmp/capability_cache/test_capability_cache   tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

Expected:
- PASS。

## Task 4: GREEN - Roadmap, docs, working memory closeout

**Files:**
- Modify: `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`
- Modify: `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- Modify: `docs/guides/security-best-practices.md`
- Modify: `docs/DOCUMENTATION_INDEX.md`
- Modify: `docs/README.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: 更新 roadmap 口径**

- 不再把 Batch 1-5 写成未来队列。
- 改成已完成 closeout、下一阶段另起批次的口径。

**Step 2: 更新对外文档**

- OCSP guide：
  - 不再声称 stapling / online OCSP 的 cryptographic verification parity 未完成。
  - 保留真实剩余边界：不是完整 browser-grade revocation stack、仍依赖 OpenSSL helper、没有 server-side stapling issuance。
- CT guide：
  - 明确 runtime surface 现在可从 TLS extension、embedded SCT、OCSP-delivered SCT 三处取源。
  - 不再把 `OCSP-delivered SCT source` 写成缺失。
- security best practices：
  - 调整 OCSP/CT 最佳实践描述到当前 truth。
- docs index / docs README：
  - 补 focused gate / OCSP / CT guide 的 closeout 口径，避免入口文档继续引用旧叙述。

**Step 3: 回填 working memory**

- `task_plan.md` 顶部新增本轮 closeout 批次。
- `findings.md` 顶部记录 closeout 结论。
- `progress.md` 顶部记录 RED/GREEN/验证命令与结果。

## Task 5: Verification

Run:

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --dry-run --fast-local --run-id closeout_dryrun_20260411
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check --   docs/plans/2026-04-11-freepascal-validation-closeout-and-focused-gate.md   docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md   docs/guides/OCSP_USAGE_GUIDE.md   docs/guides/CT_IMPLEMENTATION_GUIDE.md   docs/guides/security-best-practices.md   docs/DOCUMENTATION_INDEX.md   docs/README.md   scripts/run_freepascal_tls13_completeness_gate.sh   src/fafafa.ssl.freepascal.lib.pas   tests/test_freepascal_backend_basic.pas   tests/test_capability_cache.pas   tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh   task_plan.md   findings.md   progress.md
```

Expected:
- dry-run PASS
- compile gate PASS
- diff hygiene PASS

## Definition Of Done

- `KnownIssues` 只表达真实剩余边界。
- focused gate 已覆盖 validation runtime 高价值路径。
- roadmap / ledger / 对外文档不再保留已关闭批次的未来时态。
- focused tests、contract test、gate dry-run、compile gate、diff hygiene 均有真实结果。
