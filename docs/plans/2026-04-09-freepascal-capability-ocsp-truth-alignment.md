**Goal:** 对齐 FreePascal backend capability matrix 与当前 OCSP stapling runtime truth：`SupportsOCSPStapling` / `OCSPStaplingSupport` / `sslFeatOCSPStapling` 不再停留在 `False/None`，`KnownIssues` 也不再把 OCSP stapling 整体说成未完成。

**Why This Batch:** 当前实现已经有：
- FreePascal client `status_request` request path
- `ISSLOCSPStapling` runtime surface
- bounded stapled-response parsing / status surfacing
- `ssoRequireOCSPStapling` fail-closed gate

但 capability 层仍然宣称：
- `SupportsOCSPStapling = False`
- `OCSPStaplingSupport = sslSupportNone`
- `KnownIssues` 继续写 `remaining gaps include OCSP stapling`

这会让 capability truth 和真实运行时行为发生漂移。

**Guardrails:**
- 这批只对齐 FreePascal backend OCSP capability truth 与 wording
- 不修改 OCSP runtime/request/validation 实现
- 不把 OCSP stapling 说成 stable/full-complete；目标是 `usable/experimental`，不是“完整 revocation parity”
- `KnownIssues` 只允许收紧成真实剩余缺口，比如 broader OCSP validation hardening / online fetch parity / broader certificate validation hardening

---

## Task 1: RED - Tighten capability contracts

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Add capability truth assertions**
- 断言 FreePascal:
  - `SupportsOCSPStapling = True`
  - `OCSPStaplingSupport = sslSupportExperimental`
  - `IsFeatureSupported(sslFeatOCSPStapling) = True`

**Step 2: Tighten KnownIssues wording**
- 不再允许 `KnownIssues` 笼统把 `OCSP stapling` 整体列为 remaining gap
- 改成要求：
  - 仍然提到更窄的 OCSP 剩余缺口，例如 `online fetch parity` / `validation hardening`
  - 但不再用“OCSP stapling 整体未完成”的表达

**Focused RED Commands:**
```bash
mkdir -p tmp/freepascal_backend_basic && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_backend_basic \
  -FEtmp/freepascal_backend_basic \
  -otmp/freepascal_backend_basic/test_freepascal_backend_basic \
  tests/test_freepascal_backend_basic.pas && \
./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/capability_cache \
  -FEtmp/capability_cache \
  -otmp/capability_cache/test_capability_cache \
  tests/test_capability_cache.pas && \
./tmp/capability_cache/test_capability_cache
```

---

## Task 2: GREEN - Minimal capability truth alignment

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Align runtime-advertised OCSP capability**
- 让 `GetCapabilities` 反映当前 FreePascal client OCSP stapling runtime surface：
  - `SupportsOCSPStapling := True`
  - `OCSPStaplingSupport := sslSupportExperimental`
- 让 `IsFeatureSupported(sslFeatOCSPStapling)` 与 capability matrix 一致

**Step 2: Tighten wording without overselling**
- 更新 `KnownIssues`：
  - 保留 0-RTT / anti-replay / CT source parity / validation hardening
  - OCSP 只保留真实剩余缺口，不再把整个 OCSP stapling 功能笼统列为 gap

---

## Task 3: Verification / Closeout

**Commands:**
```bash
mkdir -p tmp/freepascal_backend_basic && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_backend_basic \
  -FEtmp/freepascal_backend_basic \
  -otmp/freepascal_backend_basic/test_freepascal_backend_basic \
  tests/test_freepascal_backend_basic.pas && \
./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/capability_cache \
  -FEtmp/capability_cache \
  -otmp/capability_cache/test_capability_cache \
  tests/test_capability_cache.pas && \
./tmp/capability_cache/test_capability_cache
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-09-freepascal-capability-ocsp-truth-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md
```

---

## Execution Result

- focused RED 指向的是真实 capability drift，而不是测试误写：
  - `tests/test_freepascal_backend_basic.pas` 失败在 `SupportsOCSPStapling`
  - `tests/test_capability_cache.pas` 失败在同一 capability truth
- 最小实现只改了一处生产文件：
  - `src/fafafa.ssl.freepascal.lib.pas`
    - `IsFeatureSupported(sslFeatOCSPStapling)` => `True`
    - `SupportsOCSPStapling := True`
    - `OCSPStaplingSupport := sslSupportExperimental`
    - `KnownIssues` 改为只保留更窄的 OCSP 剩余缺口：`online OCSP fetch parity` + `OCSP stapling validation hardening`

## Final Verification

- `tests/test_freepascal_backend_basic.pas` => PASS
- `tests/test_capability_cache.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-capability-ocsp-truth-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md` => PASS
