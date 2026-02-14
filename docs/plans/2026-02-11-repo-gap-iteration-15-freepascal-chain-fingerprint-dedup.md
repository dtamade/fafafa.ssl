# FreePascal Certificate Chain Fingerprint Dedup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 `BuildCertificateChain` 的去环语义缺口，避免“同证书不同实例（clone）”被重复加入链。

**Architecture:** 在 `test_freepascal_backend_basic` 先增加 clone issuer 红测；再最小修改 `TFreePascalCertificateStore.BuildCertificateChain` 在环检测中加入指纹判重；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas` `BuildCertificateChain`
   - 当前仅按接口引用判断重复节点：`if Result[I] = LNext then ...`。
2. 该语义无法识别 clone 场景（相同证书指纹但不同对象引用）。
3. 现有 `Contains/AddCertificate/RemoveCertificate` 已按指纹语义收敛，链构建仍不一致。

### Priority
- **P0:** Chain 构建去重语义与 store 指纹语义一致（test-first）。

---

### Task 1 (P0): Add failing test for clone issuer chain dedup

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertion**
- 新增场景：
  - `LLoopLeaf := LCert.Clone`
  - `LLoopIssuer := LCert.Clone`
  - `LLoopLeaf.SetIssuerCertificate(LLoopIssuer)`
  - `LChain := LStore.BuildCertificateChain(LLoopLeaf)`
- 断言：`Length(LChain) = 1`（clone issuer 不应重复扩展链）。

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前会返回 2）。

---

### Task 2 (P0): Implement fingerprint-aware chain dedup

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `BuildCertificateChain` 的 `LExists` 判定中保留引用比较。
- 追加指纹比较：
  - `NormalizeFingerprint(Result[I].GetFingerprintSHA256)` vs `NormalizeFingerprint(LNext.GetFingerprintSHA256)`
  - 任一命中即视为已存在并 `Break`。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Focused regression

**Files:**
- Verify only

**Step 1: Run FreePascal server accept regression**
- Run:
  - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
  - `./tmp/test_fp_accept`
- Expected: PASS。

**Step 2: Run unit regression subset**
- Run:
  - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
  - `./tmp/run_unit_tests_simple --format=plain --all`
- Expected: PASS。

**Step 3: Update planning files**
- 更新：`task_plan.md`、`findings.md`、`progress.md`。

---

## Execution Notes
- 严格遵循：不写脚本、不改 CI/DI。
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每一步命令输出必须回报。

---

## Execution Record (2026-02-11 10:07 +0800)

### Task 1 (P0): Add failing test for clone issuer chain dedup
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertions:
  - clone leaf + clone issuer chain fixture
  - `Length(LChain) = 1` for same-fingerprint clone issuer

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ BuildCertificateChain should de-duplicate cloned issuer certificate by fingerprint`

### Task 2 (P0): Implement fingerprint-aware chain dedup
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - keep existing reference loop check
  - add fingerprint comparison inside `LExists` detection
  - break chain expansion when fingerprint already exists in chain

**GREEN command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `✅ FreePascal backend basic checks passed`

### Task 3 (P1): Focused regression

**Regression command 1**
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
```
**Output (key):**
- `✅ FreePascal server accept skeleton checks passed`

**Regression command 2**
```bash
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
**Output (key):**
- `Number of run tests: 10`
- `Number of failures: 0`
- `Number of errors: 0`
- `Number of ignored tests: 2`

### Iteration Status
- Iteration 15 P0 task: **complete**
- Contract gap (`BuildCertificateChain` clone/fingerprint dedup) closed.
