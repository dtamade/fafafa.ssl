# Review Remediation Batch 1 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复后端上下文与系统证书存储可能不一致的行为风险，并把 README / 测试文档收敛到当前可验证的事实表述。

**Architecture:** 先在 builder 路径增加一个最小失败测试，锁定“所选 backend 必须同时用于 context 和 certificate store”的契约；然后对 `TSSLContextBuilderImpl.BuildClient` 做最小实现。文档方面不再硬编码易漂移的“100% / 415 files”等营销数字，而是改为引用当前可执行验证命令和更稳态的项目描述。

**Tech Stack:** FreePascal (ObjFPC), shell-based contract tests, markdown docs.

---

### Task 1: Lock backend/store consistency with a failing test

**Files:**
- Test: `tests/test_context_builder_backend_store_consistency.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing test**
- Add a fake `ISSLLibrary` implementation that records whether `CreateContext` and `CreateCertificateStore` were both requested from the same backend.
- Use `TSSLFactory.RegisterLibrary` to register two fake backends with different priorities.
- Build a client context through `TSSLContextBuilder.Create.WithBackend(...).WithSystemRoots.BuildClient`.
- Assert the selected backend created both the context and the store.

**Step 2: Run test to verify it fails**
- Run: `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_ctx_builder_store_consistency && ./tmp/test_ctx_builder_store_consistency`
- Expected: FAIL because the store path still uses `sslAutoDetect`.

**Step 3: Write minimal implementation**
- Track the effective backend chosen in `BuildClient`.
- Use that backend when creating the certificate store.

**Step 4: Run test to verify it passes**
- Run the same command as Step 2.
- Expected: PASS.

### Task 2: Make README truthful and easier to trust

**Files:**
- Modify: `README.md`

**Step 1: Remove stale claims**
- Remove duplicated “30 秒示例” section.
- Replace fragile hard-coded counts / pass-rate claims with durable statements.
- Add a short “验证方式” section pointing to `python3 scripts/compile_all_modules.py` and `bash scripts/run_minimal_ci_gate.sh --fast-local`.

**Step 2: Verify markdown reads cleanly**
- Re-read only touched sections.

### Task 3: Refresh testing docs around current workflows

**Files:**
- Modify: `docs/testing/TESTING_README.md`
- Optional modify: `.github/workflows/ci.yml`

**Step 1: Replace stale historical stats with current guidance**
- Remove 2025-era fixed module/test counts and obsolete Windows/`examples/`-based instructions.
- Document the current recommended Linux verification commands and note that some broader workflows are staged/conditional.

**Step 2: Tighten CI messaging if needed**
- If a minimal edit helps, clarify workflow scope instead of implying broad platform coverage.

### Task 4: Verify the batch

**Files:**
- Verify only

**Step 1: Focused regression**
- Run: `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_ctx_builder_store_consistency && ./tmp/test_ctx_builder_store_consistency`
- Expected: PASS.

**Step 2: Existing lightweight gates**
- Run: `python3 scripts/compile_all_modules.py`
- Run: `bash scripts/run_minimal_ci_gate.sh --fast-local`
- Expected: PASS.
