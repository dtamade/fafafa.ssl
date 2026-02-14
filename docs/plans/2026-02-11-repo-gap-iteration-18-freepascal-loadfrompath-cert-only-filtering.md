# FreePascal CertificateStore LoadFromPath Cert-Only Filtering Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收敛 FreePascal 证书仓库目录扫描行为，只对证书文件名模式尝试加载，避免把任意文本/二进制噪声当候选证书。

**Architecture:** 在 `test_freepascal_backend_basic` 增加红测（仅 `.txt` 文件时应返回 false；新增 `.pem` 后应返回 true）；再最小修改 `LoadFromPath` 引入文件名过滤；最后回归。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.freepascal.lib`, 程序级测试。

---

## Scan Summary (2026-02-11)

### High-signal gap
1. `src/fafafa.ssl.freepascal.lib.pas` `LoadFromPath` 当前对目录内所有普通文件都 `LoadFromFile`。
2. 这会把 `.txt/.log/...` 噪声文件也当作证书候选，造成不必要解析尝试。
3. 目标语义：仅扫描证书文件名模式（保留 `.pem/.crt/.cer/.der` 与系统哈希 `.0/.1...` 兼容）。

### Priority
- **P0:** cert store 扫描契约收敛（test-first）。

---

### Task 1 (P0): Add failing test for cert-only directory filtering

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Target: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Write failing assertions**
- 构造临时目录仅包含 `renamed_cert.txt`（内容为有效 PEM 证书），断言：
  - `not LoadFromPath(tempDir)`
  - `GetCount = 0`
- 同目录新增 `renamed_cert.pem` 后断言：
  - `LoadFromPath(tempDir)`
  - `GetCount = 1`

**Step 2: Run test to verify RED**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: FAIL（当前会尝试加载 `.txt` 并返回 true）。

---

### Task 2 (P0): Implement certificate filename filtering in LoadFromPath

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Minimal implementation**
- 在 `LoadFromPath` 引入文件名过滤函数，仅允许：
  - `.pem/.crt/.cer/.der`
  - OpenSSL 系统哈希风格扩展（纯数字扩展：`.0/.1/...`）
- 对不匹配文件跳过，不执行 `LoadFromFile`。

**Step 2: Run test to verify GREEN**
- Run:
  - `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic`
  - `./tmp/test_fp_basic`
- Expected: PASS。

---

### Task 3 (P1): Focused regression

**Step 1:**
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept`
- `./tmp/test_fp_accept`

**Step 2:**
- `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple`
- `./tmp/run_unit_tests_simple --format=plain --all`

---

## Execution Notes
- 严格 TDD：先 RED，再 GREEN，再回归。
- 每一步命令输出必须回报。
- 不新增脚本，不改 CI/DI。

---

## Execution Record (2026-02-11 10:31 +0800)

### Task 1 (P0): Add failing test for cert-only directory filtering
- Modified: `tests/test_freepascal_backend_basic.pas`
- Added assertions:
  - 目录仅含 `renamed_cert.txt` 时：`LoadFromPath` returns `False`, `GetCount = 0`
  - 同目录加入 `renamed_cert.pem` 后：`LoadFromPath` returns `True`, `GetCount = 1`

**RED command**
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
**Output (key):**
- `❌ LoadFromPath should ignore non-certificate extension files`

### Task 2 (P0): Implement certificate filename filtering in LoadFromPath
- Modified: `src/fafafa.ssl.freepascal.lib.pas`
- Implementation:
  - `LoadFromPath` 增加 `IsCertificateCandidateFile` 过滤
  - 允许扩展：`.pem/.crt/.cer/.der`
  - 保留系统哈希扩展兼容：纯数字扩展（`.0/.1/...`）

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
- Iteration 18 P0 task: **complete**
- Contract gap (`LoadFromPath` cert-only filtering) closed.
