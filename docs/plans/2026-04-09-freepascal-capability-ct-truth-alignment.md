**Goal:** 对齐 FreePascal backend capability matrix 与当前 CT runtime truth：`SupportsCertificateTransparency` / `CertTransparencySupport` / `sslFeatCertificateTransparency` 不再停留在 `False/None`，`KnownIssues` 也不再把 Certificate Transparency 笼统列为未完成。

**Why This Batch:** 当前实现已经有：
- FreePascal client SCT request + raw surface
- embedded SCT fallback
- CT validation surface
- `CT required` fail-closed gate

但 capability 层仍然宣称：
- `SupportsCertificateTransparency = False`
- `CertTransparencySupport = sslSupportNone`
- `KnownIssues` 继续写 `remaining gaps include ... Certificate Transparency ...`

这会让 capability truth 和真实运行时行为发生漂移。

**Guardrails:**
- 这批只对齐 FreePascal backend capability truth 与 wording
- 不修改 OCSP stapling truth
- 不把 CT 说成 stable/full-complete；目标是 `usable/experimental`，不是“全部收口”
- `KnownIssues` 只允许收紧成真实剩余缺口，比如 OCSP-delivered SCT / broader CT hardening / broader certificate validation hardening

---

## Task 1: RED - Tighten capability contracts

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Add capability truth assertions**
- 断言 FreePascal:
  - `SupportsCertificateTransparency = True`
  - `CertTransparencySupport = sslSupportExperimental`
  - `IsFeatureSupported(sslFeatCertificateTransparency) = True`

**Step 2: Tighten KnownIssues wording**
- 不再允许 `KnownIssues` 笼统把 `Certificate Transparency` 整体列为 remaining gap
- 改成要求：
  - 仍然提到更窄的 CT 剩余缺口，例如 `OCSP-delivered` / `validation hardening`
  - 但不再用“CT 整体未完成”的表达

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

**Step 1: Align runtime-advertised CT capability**
- 让 `GetCapabilities` 反映当前 FreePascal client CT runtime surface：
  - `SupportsCertificateTransparency := True`
  - `CertTransparencySupport := sslSupportExperimental`
- 让 `IsFeatureSupported(sslFeatCertificateTransparency)` 与 capability matrix 一致

**Step 2: Tighten wording without overselling**
- 更新 `KnownIssues`：
  - 保留 0-RTT / anti-replay / OCSP / validation hardening
  - CT 只保留真实剩余缺口，不再把整个 CT 功能笼统列为 gap

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
git diff --check -- docs/plans/2026-04-09-freepascal-capability-ct-truth-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md
```

---

## Execution Result

- focused RED 指向的是真实 capability drift，而不是测试误写：
  - `tests/test_freepascal_backend_basic.pas` 失败在 `SupportsCertificateTransparency`
  - `tests/test_capability_cache.pas` 失败在同一 capability truth
- 最小实现只改了一处生产文件：
  - `src/fafafa.ssl.freepascal.lib.pas`
    - `IsFeatureSupported(sslFeatCertificateTransparency)` => `True`
    - `SupportsCertificateTransparency := True`
    - `CertTransparencySupport := sslSupportExperimental`
    - `KnownIssues` 改为只保留更窄的 CT 剩余缺口：`OCSP-delivered ... source parity`

## Final Verification

- `tests/test_freepascal_backend_basic.pas` => PASS
- `tests/test_capability_cache.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-capability-ct-truth-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md` => PASS
