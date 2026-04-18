# FreePascal Stapling Truth And Fast Revocation Regression Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 FreePascal validation closeout 再收紧一层：`KnownIssues` 只保留真实剩余边界，补一个轻量 revocation/CRL 快测，并把它前置进 completeness gate。

**Architecture:** 这批不再扩新的 client-side revocation runtime 行为。先用 capability wording tests 和 gate contract tests 做 RED，锁定当前 drift 仍然存在；然后新增一个不依赖完整 TLS 握手的轻量 CRL/certchain 回归测试，最后最小更新 `KnownIssues`、相关文档和 gate 清单，把快测放到重型 runtime tests 之前。下一阶段单独立计划，只记录 server-side OCSP stapling issuance，不在本批实现。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalSSLLibrary.GetCapabilities`, `TSSLCertificateChainVerifier`, `TX509CRL`, bash gate contract, markdown plans, file-based working memory.

---

## Scope

- 收这 4 件事：
  1. `KnownIssues` / docs truth 从 `revocation evidence material` 收紧到真实剩余边界
  2. 新增轻量 revocation fast contract test
  3. 把 fast contract test 前置进 `run_freepascal_tls13_completeness_gate.sh`
  4. 单独立“下一阶段”计划文档
- 明确不做：
  - 新的 client-side revocation / CRL runtime 实现
  - 新的 OCSP/CT capability 扩展
  - server-side stapling issuance 的实现

## Task 1: RED - Tighten capability truth contracts

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: 收紧 `KnownIssues` 断言**

- 断言 `KnownIssues` 继续保留真实剩余边界：
  - `0-RTT`
  - `ANTI-REPLAY`
  - `OCSP`
  - `SERVER-SIDE`
  - `STAPLING`
  - `ISSUANCE`
- 断言 `KnownIssues` 不再保留过期 wording：
  - `REVOCATION EVIDENCE MATERIAL`
  - `CRL-BACKED`
  - `CERTIFICATE VALIDATION`

**Step 2: 跑 focused RED**

Run:

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

Expected:
- FAIL，失败点指向过期 `KnownIssues` wording。

## Task 2: Add a lightweight fast regression

**Files:**
- Create: `tests/test_freepascal_revocation_fast_contracts.pas`

**Step 1: 新增轻量 CRL / certchain contract**

- 直接复用：
  - `tests/certificate/test_certs/ca_cert.pem`
  - `tests/certificate/test_certs/ca_key.pem`
  - `tests/certificate/test_certs/revocation_revoked_crl.pem`
  - `tests/certificate/test_certs/revocation_nonmatching_crl.pem`
- 覆盖：
  - `TX509CRL.Issuer.ToString` 必须保留 `CN=` 等 attribute short names
  - matching CRL + non-revoked serial => verifier PASS
  - matching CRL + revoked serial => `RevocationStatus = 1`
  - unavailable CRL material => fail-closed 且 `RevocationStatus = 2`

**Step 2: 跑 focused test**

Run:

```bash
mkdir -p tmp/freepascal_revocation_fast_contracts && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_revocation_fast_contracts -FEtmp/freepascal_revocation_fast_contracts -otmp/freepascal_revocation_fast_contracts/test_freepascal_revocation_fast_contracts tests/test_freepascal_revocation_fast_contracts.pas && ./tmp/freepascal_revocation_fast_contracts/test_freepascal_revocation_fast_contracts
```

Expected:
- PASS；如果失败，只允许为真实 parser / certchain drift。

## Task 3: RED - Gate inventory drift

**Files:**
- Modify: `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`

**Step 1: 收紧 gate contract**

- dry-run 必须提到 `tests/test_freepascal_revocation_fast_contracts.pas`
- fake `fpc` 调用次数从 `15` 收紧到 `16`
- summary report 必须新增 fast contract PASS row

**Step 2: 跑 contract RED**

Run:

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

Expected:
- FAIL，失败点指向 gate 还没纳入新的 fast contract。

## Task 4: GREEN - Minimal truth and gate alignment

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `scripts/run_freepascal_tls13_completeness_gate.sh`
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`
- Modify: `docs/guides/security-best-practices.md`

**Step 1: 收紧 `KnownIssues` 和文档**

- `KnownIssues` 不再提 `revocation evidence material plumbing`
- 改成只保留 `FreePascal server-side OCSP stapling issuance`
- OCSP / security 文档同步 truth，不再把 client-side revocation material 当当前剩余 gap

**Step 2: 前置 fast gate lane**

- 在 `TEST_NAMES` / `TEST_FILES` 里加入 `test_freepascal_revocation_fast_contracts`
- 放在重型 validation runtime tests 前面
- 保持脚本接口、`--fast-local`、PATH-resolved `fpc` 不变

**Step 3: 跑 GREEN**

Run:

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --dry-run --fast-local --run-id fp_fast_revocation_dryrun_20260411
```

Expected:
- PASS。

## Task 5: Next-stage plan and closeout

**Files:**
- Create: `docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md`
- Modify: `docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: 单独立下一阶段计划**

- 范围只记录：
  - server-side OCSP stapling issuance
  - 风险边界
  - 建议验证方式
- 不在本批实现

**Step 2: 回填 ledger**

- `task_plan.md` 顶部新增本轮入口
- `findings.md` 顶部记录 truth 收紧结论
- `progress.md` 顶部记录 RED/GREEN/验证命令与结果

## Task 6: Verification

Run:

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

```bash
mkdir -p tmp/freepascal_revocation_fast_contracts && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_revocation_fast_contracts -FEtmp/freepascal_revocation_fast_contracts -otmp/freepascal_revocation_fast_contracts/test_freepascal_revocation_fast_contracts tests/test_freepascal_revocation_fast_contracts.pas && ./tmp/freepascal_revocation_fast_contracts/test_freepascal_revocation_fast_contracts
```

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id fp_fast_revocation_exec_20260411
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-11-freepascal-stapling-truth-and-fast-revocation-regression.md docs/plans/2026-04-11-freepascal-server-side-ocsp-stapling-issuance-next-stage.md docs/plans/2026-04-11-freepascal-validation-next-wave-roadmap.md docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md scripts/run_freepascal_tls13_completeness_gate.sh src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas tests/test_freepascal_revocation_fast_contracts.pas tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md
```

Expected:
- focused tests PASS
- gate contract PASS
- focused gate PASS
- compile gate PASS
- diff hygiene PASS

## Definition Of Done

- `KnownIssues` 只保留真实剩余边界。
- 新的 revocation fast contract 覆盖 CRL parser + certchain 基础 truth。
- completeness gate 前置运行 fast contract。
- 下一阶段单独立项，不混进本批实现。
