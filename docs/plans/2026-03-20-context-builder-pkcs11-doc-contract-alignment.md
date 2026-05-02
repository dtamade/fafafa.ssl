# Context Builder PKCS#11 Docs Contract Alignment Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align the PKCS#11 docs with the current `TSSLContextBuilder` runtime contract after the recent builder fixes. The docs should clearly show that builder callers can use direct PINs, environment variables, and files, while `pmCallback` and `pmInteractive` remain lower-level backend paths rather than builder-supported flows.

**Architecture:** Keep this batch docs-only. Update `docs/guides/PKCS11_USER_GUIDE.md` with actionable builder examples for `pmValue`, `pmEnvironment`, and `pmFile`, plus an explicit lower-level callback example that matches the real `TPKCS11PINCallback` type. Update `docs/reference/PKCS11_ARCHITECTURE.md` with a concise builder support-boundary note and correct the stale backend interface/class snippets so the reference material does not imply broader or older API support than the runtime actually provides. Verify with formatting, focused `rg` checks, and diff hygiene.

**Tech Stack:** Markdown docs, Pascal code snippets, focused `rg` verification, Prettier formatting

---

### Task 1: Fix PKCS#11 user-guide contract drift

**Files:**
- Modify: `docs/guides/PKCS11_USER_GUIDE.md`
- Reference: `src/fafafa.ssl.context.builder.pas`
- Reference: `src/fafafa.ssl.pkcs11.types.pas`
- Reference: `src/fafafa.ssl.pkcs11.backend.pas`

**Step 1: Clarify builder-supported PIN sources**

- Keep the current builder example on the supported fluent path:
  - `WithCertificate(...)`
  - `UsePKCS11(...)`
  - `WithPKCS11PIN(...)`
  - `BuildServer`
- Add explicit guidance that builder callers may use:
  - direct PIN value (`pmValue`, implicit when calling `WithPKCS11PIN(...)`)
  - environment variable source via `WithPKCS11PINMethod(pmEnvironment)`
  - file source via `WithPKCS11PINMethod(pmFile)`
- Add a short note that empty source values fail validation and missing env/file sources fail deterministically during build

**Step 2: Separate lower-level callback guidance**

- Rename the callback section so it is clearly a lower-level/backend API example, not builder guidance
- Replace the free-function callback example with an object-bound callback example that matches `TPKCS11PINCallback = function(...) of object`
- Use `TPKCS11ConfigDefault` + `TPKCS11BackendFactory.CreateBackend` so the example matches the current public surface
- State explicitly that `pmCallback` and `pmInteractive` are not builder runtime paths

### Task 2: Tighten the architecture reference boundary

**Files:**
- Modify: `docs/reference/PKCS11_ARCHITECTURE.md`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add builder runtime boundary note**

- In the builder API section, state that builder runtime currently supports:
  - `pmNone`
  - `pmValue`
  - `pmEnvironment`
  - `pmFile`
- State that `pmCallback` and `pmInteractive` remain lower-level `TPKCS11Config` / backend integrations
- Correct the backend abstraction snippets to match the current public API:
  - `IPKCS11Backend` includes `LoadCertificate`, `GetName`, and `GetVersion`
  - backend classes are `TProviderBackend` / `TEngineBackend`
- Keep the example small and aligned with the current builder API

### Task 3: Format and verify docs

**Files:**
- `docs/guides/PKCS11_USER_GUIDE.md`
- `docs/reference/PKCS11_ARCHITECTURE.md`

**Step 1: Run formatter**

Run:
`npx prettier --write docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md`

Expected:
- PASS

**Step 2: Run focused searches**

Run:
`rg -n "WithPKCS11PINMethod\\(pmEnvironment\\)|WithPKCS11PINMethod\\(pmFile\\)|pmCallback|pmInteractive|TPKCS11Config|IPKCS11Backend" docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md`

Run:
`rg -n "WithPKCS11Key|\\.ForServer\\b|\\.Build;" docs/guides/PKCS11_USER_GUIDE.md`

Expected:
- first search finds the updated contract language and examples
- second search finds no stale PKCS#11 builder API names

**Step 3: Run whitespace / patch hygiene**

Run:
`git diff --check -- docs/plans/2026-03-20-context-builder-pkcs11-doc-contract-alignment.md docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md task_plan.md findings.md progress.md`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and evidence**

- Note that builder/runtime behavior changed in recent batches, but user-facing docs still implied an older or broader contract
- Record that the callback example previously used the wrong callback shape for the current `of object` type

**Step 2: Roll next queue**

- Decide whether to add a lightweight docs contract check for PKCS#11 builder guidance
- Keep callback/interactive builder support as a separate API/design decision, not a docs-only follow-up
