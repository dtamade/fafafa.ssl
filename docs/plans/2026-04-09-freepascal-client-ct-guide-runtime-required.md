**Goal:** 更新 `docs/guides/CT_IMPLEMENTATION_GUIDE.md`，让文档覆盖当前真实可用的 FreePascal client CT surface、`CT required` 配置方式，以及 verify-peer / resumed 的 runtime 边界，避免文档仍停留在底层 OpenSSL validator 视角。

**Why This Batch:** 代码和测试已经支持：
- `ISSLCertificateTransparency`
- `ISSLCertificateTransparencyValidation`
- `TSSLContextBuilder.WithCertificateTransparencyRequired(...)`
- `certificate_transparency_required` override / export

但公开指南还没有告诉使用者：
- 在连接对象上如何读 CT / SCT 状态
- `required` 何时会 fail-closed
- 哪些路径不会触发 enforcement

**Guardrails:**
- 只写已经实现并有测试/代码证据的行为
- 不把文档说成“所有 backend 都支持 CT required”
- 不承诺 OCSP-delivered SCT source 或自定义 policy 配置
- 以 FreePascal client 为中心，保留现有底层 OpenSSL CT validator 内容

---

## Task 1: Update the guide

**Files:**
- Modify: `docs/guides/CT_IMPLEMENTATION_GUIDE.md`

**Step 1: Add a FreePascal client runtime section**
- 说明 `sslFreePascal` client 在 `sslVerifyPeer` 下会请求并 surface SCT
- 给出 builder + connection 示例：
  - `WithVerifyPeer`
  - `WithSystemRoots`
  - `WithCertificateTransparencyRequired`
  - `Supports(..., ISSLCertificateTransparency, ...)`
  - `Supports(..., ISSLCertificateTransparencyValidation, ...)`

**Step 2: Document actual runtime boundaries**
- 明确写出：
  - `required` 只在 verify-peer、non-resumed full-handshake 上 fail-closed
  - verify-none 时不会请求 SCT，也不会触发 `required`
  - resumed session 时不会因为缺少 certificate/SCT flight 而被 `required` 阻断
- 明确当前 negative fail-closed 条件：
  - missing SCT list
  - validation unavailable
  - policy failed

**Step 3: Keep the scope honest**
- 在 guide 中注明当前不覆盖：
  - OCSP-delivered SCT source
  - custom CT policy / custom log-store runtime wiring for client enforcement
  - 所有 backend 的一致支持声明

---

## Task 2: Verification

**Commands:**
```bash
rg -n "WithCertificateTransparencyRequired|ISSLCertificateTransparency|verify-peer|resumed|validation unavailable|policy failed" docs/guides/CT_IMPLEMENTATION_GUIDE.md
```

```bash
git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-guide-runtime-required.md docs/guides/CT_IMPLEMENTATION_GUIDE.md task_plan.md findings.md progress.md
```

**Optional Formatting:**
```bash
yarn prettier --write docs/guides/CT_IMPLEMENTATION_GUIDE.md
```

**Done When:**
- CT guide 覆盖当前真实可用的 FreePascal client runtime CT API
- `required` 的生效边界与 fail-closed 条件写清楚
- diff hygiene 为绿

---

## Execution Result

- guide 已补到当前真实实现，而不是停留在底层 OpenSSL validator 视角：
  - 新增 FreePascal client runtime CT 用法示例
  - 写清 `WithCertificateTransparencyRequired(...)` 的生效边界
  - 写清当前 fail-closed 条件与未覆盖范围
- 本批只改文档，没有改任何生产代码或测试。

## Final Verification

- `rg -n "WithCertificateTransparencyRequired|ISSLCertificateTransparency|verify-peer|resumed|validation unavailable|policy failed|embedded SCT" docs/guides/CT_IMPLEMENTATION_GUIDE.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/CT_IMPLEMENTATION_GUIDE.md` => PASS
  - 注：`yarn prettier --write ...` 在当前环境下因路径匹配失败未能直接工作，改用同一 prettier 二进制完成格式化
- `git diff --check -- docs/plans/2026-04-09-freepascal-client-ct-guide-runtime-required.md docs/guides/CT_IMPLEMENTATION_GUIDE.md task_plan.md findings.md progress.md` => PASS
