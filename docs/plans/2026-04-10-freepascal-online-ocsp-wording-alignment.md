# FreePascal Online OCSP Wording Alignment Implementation Plan

**Goal:** 对齐 FreePascal backend capability `KnownIssues` 与 OCSP 文档表述，让仓库不再把已经落地的 FreePascal client online AIA OCSP fetch / stapling validation hardening 说成未完成，同时继续明确剩余边界。

**Architecture:** 这批只做 wording / capability truth alignment，不改 online OCSP / stapling / CT 的运行时实现。实现分三层：
1. 先收紧 capability wording tests，锁定 `KnownIssues` 不再包含过期的 `online OCSP fetch parity` / `OCSP stapling validation hardening`；
2. 最小更新 `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues`，改成更真实的剩余 OCSP 边界；
3. 同步更新 `docs/guides/OCSP_USAGE_GUIDE.md`、`docs/guides/security-best-practices.md`、`docs/DOCUMENTATION_INDEX.md`，把 FreePascal client online OCSP path 写清楚，但不误表述成完整 revocation parity。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalSSLLibrary.GetCapabilities`, capability runtime tests, markdown docs, file-based working memory.

---

## Summary

- 当前代码已经落地：
  - FreePascal client `sslCertVerifyCheckOCSP` online AIA fetch
  - context-level HTTP hooks access
  - stapling validation hardening
- 但 wording 还停在旧阶段：
  - `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues` 仍写 `online OCSP fetch parity` / `OCSP stapling validation hardening`
  - `docs/guides/OCSP_USAGE_GUIDE.md` 仍把 FreePascal 路径表述成只有 stapling，且明确写 online AIA fetch 尚未支持
  - `docs/guides/security-best-practices.md` 和 `docs/DOCUMENTATION_INDEX.md` 也还没有同步这次能力闭环
- 这批最小正确动作因此是：
  - 去掉过期 wording
  - 保留真实剩余边界，例如更广的 OCSP validation hardening / CT source parity / broader certificate validation hardening
  - 不继续扩实现或 capability 等级

## Task 1: RED - Tighten capability wording contracts

**Files:**
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Lock the stale wording out**
- 断言 `KnownIssues`：
  - 仍然提到更窄的 OCSP remaining scope
  - 不再包含：
    - `online OCSP fetch parity`
    - `OCSP stapling validation hardening`
- 允许新的更窄表述，例如：
  - broader OCSP validation hardening
  - responder-signature / issuer-chain parity hardening

**Step 2: Run focused RED**
- Run:
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- Expected:
  - FAIL，指向过期 `KnownIssues` wording

## Task 2: GREEN - Align capability wording and docs

**Files:**
- Modify: `src/fafafa.ssl.freepascal.lib.pas`
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`
- Modify: `docs/guides/security-best-practices.md`
- Modify: `docs/DOCUMENTATION_INDEX.md`

**Step 1: Tighten FreePascal KnownIssues**
- 只更新 wording，不改 capability level：
  - 去掉 `online OCSP fetch parity`
  - 去掉 `OCSP stapling validation hardening`
  - 改成更真实的剩余边界，例如 broader OCSP validation hardening / CT source parity / broader certificate validation hardening

**Step 2: Document the new FreePascal online OCSP path**
- 在 `docs/guides/OCSP_USAGE_GUIDE.md`：
  - 保留 stapling path
  - 新增 FreePascal client online OCSP path：
    - `sslVerifyPeer`
    - `SetCertVerifyFlags([sslCertVerifyCheckOCSP])`
    - context HTTP hooks
    - non-resumed full-handshake boundary
  - 保持 OpenSSL helper workflow 作为更底层/手工路径
  - 不把 FreePascal wording 写成完整 revocation parity

**Step 3: Sync adjacent docs**
- `docs/guides/security-best-practices.md`
  - 去掉“online AIA fetch 仍未覆盖”的旧说法
  - 改成真实剩余边界
- `docs/DOCUMENTATION_INDEX.md`
  - 把 OCSP guide 简介从 “FreePascal stapling + OpenSSL 在线 OCSP” 收紧成能反映当前 FreePascal online path 的描述

## Task 3: Verification / Closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused wording tests**
- Run:
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
- Expected:
  - PASS

**Step 2: Run compile gate**
- Run:
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Run docs verification**
- Run:
  - `rg -n "online AIA OCSP fetch parity|online OCSP fetch parity|FreePascal client runtime|sslCertVerifyCheckOCSP|OpenSSL 在线 OCSP" docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/DOCUMENTATION_INDEX.md src/fafafa.ssl.freepascal.lib.pas`
- Expected:
  - 旧 wording 消失，新 wording 出现

**Step 4: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-10-freepascal-online-ocsp-wording-alignment.md src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas docs/guides/OCSP_USAGE_GUIDE.md docs/guides/security-best-practices.md docs/DOCUMENTATION_INDEX.md task_plan.md findings.md progress.md`
- Expected:
  - PASS

## Notes

- 这批不升级 capability 等级；FreePascal OCSP / CT 仍然是 `experimental/usable` 语义。
- 这批不把 FreePascal wording 写成“完整在线 revocation stack”；仍然保留 broader OCSP validation / CT source parity 等剩余边界。
