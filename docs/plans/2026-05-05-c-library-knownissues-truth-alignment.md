# C-Library KnownIssues Truth Alignment Plan

**Goal:** 把当前可在 Linux 主机验证的 C-library backend capability `KnownIssues` 收口到真实、可执行的剩余边界，不再继续发布泛泛而谈的占位文案。当前批次只覆盖 `WolfSSL` / `MbedTLS`；`WinSSL` 仍需要真实 Windows 主机 runtime proof，不在本批范围内。

**Architecture:** 先在 focused capability test 上补 RED，锁住 `WolfSSL` / `MbedTLS` 的 `KnownIssues` 必须表达当前真实 capability truth，而不是继续留着“可能缺企业特性”或“可能需要特定构建选项”这类泛化 wording。然后最小修改 `src/fafafa.ssl.wolfssl.lib.pas` 与 `src/fafafa.ssl.mbedtls.lib.pas` 的 `GetCapabilities()`，让能力对象直接输出当前可验证的剩余边界。最后跑 focused test 与 diff hygiene，回写台账。

**Files:**

- Modify: `tests/test_capability_cache.pas`
- Modify: `src/fafafa.ssl.wolfssl.lib.pas`
- Modify: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - lock concrete KnownIssues wording for C-library backends

Run:

```bash
mkdir -p tmp/capability_cache_units
fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otest_capability_cache tests/test_capability_cache.pas
./tmp/capability_cache_units/test_capability_cache
```

Expected drift:

- `WolfSSL` capability `KnownIssues` 仍是泛化的 `May require specific build options for full feature support`
- `MbedTLS` capability `KnownIssues` 仍是泛化的 `Optimized for embedded systems, may lack some enterprise features`
- 两者都没有把当前真正重要的 capability 边界写清楚

## Task 2: GREEN - align KnownIssues to verified capability truth

Change:

- `tests/test_capability_cache.pas`
  - 新增 `WolfSSL` / `MbedTLS` capability wording assertions
  - 如果 backend 在当前主机不可用，则显式 `[SKIP]`
- `src/fafafa.ssl.wolfssl.lib.pas`
  - `KnownIssues` 改成 build/runtime helper-gated truth
  - 明确 early-data 可能退化为 `none`
  - 明确 OCSP stapling 仍按 experimental 理解
- `src/fafafa.ssl.mbedtls.lib.pas`
  - `KnownIssues` 改成当前不支持 early-data / OCSP stapling / CT 的真实边界
  - 保留其 embedded-oriented 定位，但不再用泛化 wording 替代 capability truth

Constraints:

- 不修改 `src/fafafa.ssl.winssl.*`
- 不把 `WolfSSL` 当前 host 的 helper 缺失误写成 backend family 永久不支持 early-data
- 不把 `MbedTLS` 的当前 `none` 能力写成“计划很快补齐”

## Task 3: Verification

Run:

```bash
mkdir -p tmp/capability_cache_units
fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otest_capability_cache tests/test_capability_cache.pas
./tmp/capability_cache_units/test_capability_cache
git diff --check -- docs/plans/2026-05-05-c-library-knownissues-truth-alignment.md tests/test_capability_cache.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.mbedtls.lib.pas task_plan.md findings.md progress.md
```

### Definition Of Done

- `WolfSSL` capability `KnownIssues` 不再停留在泛化占位文案
- `MbedTLS` capability `KnownIssues` 不再停留在泛化占位文案
- focused capability test 覆盖这两条 runtime wording truth
- 台账更新并留出 `WinSSL` 外部 Windows runtime blocker

## Execution Result

- focused RED 命中真实 drift，而不是测试误报：
  - `tests/test_capability_cache.pas` 新增 `WolfSSL` / `MbedTLS` wording assertions 后，首次失败直接命中旧 `WolfSSL` placeholder：`May require specific build options for full feature support`
- 最小 GREEN 只改两处生产 wording：
  - `src/fafafa.ssl.wolfssl.lib.pas`
    - `KnownIssues` 收紧为 build/runtime helper-gated truth
    - 明确 early-data helper 缺失时可能退化为 `none`
    - 明确保留 `OCSP stapling remains experimental`
  - `src/fafafa.ssl.mbedtls.lib.pas`
    - `KnownIssues` 收紧为 early-data / OCSP stapling / certificate transparency 当前不支持
- focused capability test 复跑通过：
  - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otest_capability_cache tests/test_capability_cache.pas`
  - `./tmp/capability_cache_units/test_capability_cache`
  - 结果：`FreePascal KnownIssues` 继续通过，`WolfSSL KnownIssues runtime alignment verified`，`MbedTLS KnownIssues runtime alignment verified`
- hygiene 通过：
  - `git diff --check -- docs/plans/2026-05-05-c-library-knownissues-truth-alignment.md tests/test_capability_cache.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.mbedtls.lib.pas task_plan.md findings.md progress.md`
- broad completion audit 继续成立：
  - Linux 主机上 `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` public interface 合同面全绿
  - `WinSSL` 仍然因为缺少真实 Windows runtime 环境而无法在本机完成 runtime proof
