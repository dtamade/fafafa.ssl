# FreePascal TLS 1.3 CertificateVerify Focused Gate Promotion Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把刚完成的 FreePascal TLS 1.3 `CertificateVerify` 主线正式提升进 focused completeness gate，让默认 CI/focused verification 不再遗漏这条默认协商路径。

**Architecture:** 这批继续保持很窄的验证面边界：只更新 `run_freepascal_tls13_completeness_gate.sh` 的测试 inventory、对应 contract test，以及与 gate inventory 直接相关的说明文本。不修改 `CertificateVerify` 生产代码，不把 peer-certificate / chain-trust / OCSP / CT 额外回归一并塞进 focused gate。

**Tech Stack:** Bash, FreePascal (ObjFPC), focused gate script, gate contract tests, `.github` docs, file-based working memory.

---

## Task 1: RED - Prove focused gate still misses the CertificateVerify mainline

**Files:**
- Modify: `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`

**Step 1: Tighten the contract first**
- 在 gate contract 里新增 dry-run inventory 断言，要求 focused gate 必须提到：
  - `tests/test_tls13_clienthello_parser.pas`
  - `tests/test_tls13_servercertverify.pas`
  - `tests/test_freepascal_client_certificateverify_runtime.pas`
- 同步把 fake `fpc` invocation count 从 `7` 调整成 `10`
- 同步把 summary row 断言改成覆盖 `test_freepascal_client_certificateverify_runtime`

**Step 2: Run RED to verify current gate is still missing the new line**

**Commands (RED):**
```bash
bash -n tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

**Expected:**
- `bash -n` 通过
- contract 实际执行失败，且失败点是 dry-run output 仍缺少新的 CertificateVerify tests

## Task 2: GREEN - Promote the new tests into the focused gate

**Files:**
- Modify: `scripts/run_freepascal_tls13_completeness_gate.sh`
- Modify: `.github/README.md`

**Step 1: Minimal gate inventory update**
- 在 `scripts/run_freepascal_tls13_completeness_gate.sh` 的 `TEST_NAMES` / `TEST_FILES` 中加入：
  - `test_tls13_clienthello_parser`
  - `test_tls13_servercertverify`
  - `test_freepascal_client_certificateverify_runtime`
- 保持现有 test order 稳定，不重排旧 inventory

**Step 2: Keep documentation inventory aligned**
- 在 `.github/README.md` 的 focused gate coverage 文案里补上：
  - `test_tls13_clienthello_parser`
  - `test_tls13_servercertverify`
  - `test_freepascal_client_certificateverify_runtime`
- 不扩展到更大的 capability / roadmap 叙事

## Task 3: Verification and closeout

**Commands:**
```bash
bash -n scripts/run_freepascal_tls13_completeness_gate.sh
```

```bash
bash -n tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id certverify_gate_promotion_20260410
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-10-freepascal-tls13-certificateverify-focused-gate-promotion.md .github/README.md scripts/run_freepascal_tls13_completeness_gate.sh tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md
```

---

## Execution Result

- 待执行。

## Final Verification

- 待执行。
