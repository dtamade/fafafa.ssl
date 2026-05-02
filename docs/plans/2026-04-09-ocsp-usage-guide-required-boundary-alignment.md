**Goal:** 更新 `docs/guides/OCSP_USAGE_GUIDE.md`，把 FreePascal client runtime `required OCSP` 的两个关键边界写实：`verify-none` 不触发 fail-closed，resumed path 不因缺少新的 certificate / stapled-response flight 被阻断。

**Why This Batch:** 代码和测试现在已经把 OCSP required 的主路径、`verify-none` guard、resumed guard 都补齐了，但当前 guide 仍然把 `WithOCSPStaplingRequired(True)` 写成一个无条件 fail-closed 结论，没有把这两个 boundary 说出来。这会让文档再次跑在 runtime truth 前面。

**Guardrails:**
- 这批只改 `docs/guides/OCSP_USAGE_GUIDE.md`
- 只写已经被 fresh code/tests 证明的行为
- 不重写 OpenSSL 在线 OCSP 工作流
- 不顺手扩到 `security-best-practices` / `API_DOCUMENTATION.md`

---

## Task 1: Align the guide wording to the current runtime boundaries

**Files:**
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`

**Step 1: Tighten the required-mode wording**
- 在 FreePascal client runtime section 里，把 `WithOCSPStaplingRequired(True)` 的描述收紧成：
  - verify-peer 的 non-resumed full-handshake path 上：
    - missing stapled response => fail-closed
    - unaccepted stapled response => fail-closed
- 明确补上当前两个边界：
  - `verify-none` 时不会因为 `required` 被 fail-closed
  - resumed TLS 1.3 path 不会因为 resumed flight 缺少新的 stapled response 被 `required` 阻断

**Step 2: Keep the rest of the guide honest**
- 保留当前已经对齐好的范围描述：
  - `ISSLOCSPStapling`
  - optional surface
  - online AIA OCSP fetch / responder-signature / server-side issuance 仍未覆盖
- 不新增新的能力承诺

## Task 2: Verify and close out

**Commands:**
```bash
rg -n "verify-none|resumed|WithOCSPStaplingRequired|ISSLOCSPStapling|fail-closed|full-handshake" docs/guides/OCSP_USAGE_GUIDE.md
```

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/OCSP_USAGE_GUIDE.md
```

```bash
git diff --check -- docs/plans/2026-04-09-ocsp-usage-guide-required-boundary-alignment.md docs/guides/OCSP_USAGE_GUIDE.md task_plan.md findings.md progress.md
```

---

## Execution Result

- `docs/guides/OCSP_USAGE_GUIDE.md` 已补齐 FreePascal client runtime `required OCSP` 的两个 boundary：
  - `verify-none` 不触发 required fail-closed
  - resumed TLS 1.3 path 不因缺少新的 certificate / stapled response 被 `required` 阻断
- 原有已对齐内容保持不变：
  - `WithOCSPStapling(True)` request path
  - `ISSLOCSPStapling` surface
  - online AIA OCSP fetch / responder-signature / server-side issuance 仍未覆盖

## Final Verification

- `rg -n "verify-none|resumed|WithOCSPStaplingRequired|ISSLOCSPStapling|fail-closed|full-handshake" docs/guides/OCSP_USAGE_GUIDE.md` => PASS
- `/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/guides/OCSP_USAGE_GUIDE.md` => PASS (`unchanged`)
- `git diff --check -- docs/plans/2026-04-09-ocsp-usage-guide-required-boundary-alignment.md docs/guides/OCSP_USAGE_GUIDE.md task_plan.md findings.md progress.md` => PASS
