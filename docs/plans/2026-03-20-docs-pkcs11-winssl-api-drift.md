# PKCS#11 And WinSSL Docs API Drift Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Update the project docs so PKCS#11 builder examples and WinSSL SNI guidance match the current public API and deprecation guidance.

**Architecture:** Keep this batch docs-only. Replace nonexistent PKCS#11 builder methods with the current `UsePKCS11(...)` / `WithPKCS11PIN(...)` / `BuildServer` chain, and replace deprecated context-level `SetServerName(...)` guidance in WinSSL docs with connection-level `ISSLClientConnection.SetServerName(...)`. Do not redesign the wider WinSSL guides in this batch.

**Tech Stack:** Markdown docs, Pascal code snippets, focused `rg` verification, Prettier formatting

---

### Task 1: Fix PKCS#11 builder example drift

**Files:**
- Modify: `docs/guides/PKCS11_USER_GUIDE.md`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Replace outdated builder API**

- Update the `Context Builder` example so it uses the current chain:
  - `WithCertificate(...)`
  - `UsePKCS11(...)`
  - `WithPKCS11PIN(...)`
  - `BuildServer`
- Remove nonexistent calls:
  - `.ForServer`
  - `.WithPKCS11Key(...)`
  - `.Build`

**Step 2: Verify the old API names are gone**

Run:
`rg -n "WithPKCS11Key|\\.ForServer\\b|\\.Build;" docs/guides/PKCS11_USER_GUIDE.md`

Expected:
- No matches

### Task 2: Fix WinSSL SNI guidance drift

**Files:**
- Modify: `docs/guides/WINSSL_USER_GUIDE.md`
- Modify: `docs/guides/WINSSL_QUICKSTART.md`
- Reference: `docs/INTEGRATION_GUIDE.md`
- Reference: `docs/guides/GETTING_STARTED.md`

**Step 1: Replace deprecated context-level SNI examples**

- Update WinSSL examples so SNI is configured on `ISSLClientConnection`, not on shared context.
- Where the example already creates a connection, show:
  - `Conn := Ctx.CreateConnection(Socket);`
  - `ClientConn := Conn as ISSLClientConnection;`
  - `ClientConn.SetServerName(...)`
- Where the example is schematic and has no real socket yet, still demonstrate the connection-level pattern in comments/snippets.

**Step 2: Update prose guidance**

- Replace text that instructs people to call `Ctx.SetServerName(...)`.
- Keep the message short and explicit: SNI/hostname is a connection-level setting.

**Step 3: Verify deprecated guidance is gone from the targeted docs**

Run:
`rg -n "Ctx\\.SetServerName|LCtx\\.SetServerName" docs/guides/WINSSL_USER_GUIDE.md docs/guides/WINSSL_QUICKSTART.md`

Expected:
- No matches

### Task 3: Format and verify docs

**Files:**
- `docs/guides/PKCS11_USER_GUIDE.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `docs/guides/WINSSL_QUICKSTART.md`

**Step 1: Run formatter**

Run:
`npx prettier --write docs/guides/PKCS11_USER_GUIDE.md docs/guides/WINSSL_USER_GUIDE.md docs/guides/WINSSL_QUICKSTART.md`

Expected:
- PASS

**Step 2: Run focused searches**

Run:
- `rg -n "UsePKCS11|WithPKCS11PIN|BuildServer|connection-level|ISSLClientConnection.SetServerName|ClientConn.SetServerName" docs/guides/PKCS11_USER_GUIDE.md docs/guides/WINSSL_USER_GUIDE.md docs/guides/WINSSL_QUICKSTART.md`
- `rg -n "WithPKCS11Key|\\.ForServer\\b|\\.Build;|Ctx\\.SetServerName|LCtx\\.SetServerName" docs/guides/PKCS11_USER_GUIDE.md docs/guides/WINSSL_USER_GUIDE.md docs/guides/WINSSL_QUICKSTART.md`

Expected:
- First search finds updated guidance
- Second search finds no stale patterns

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record drift and fixes**

- Note that the docs had drifted in two different ways:
  - nonexistent PKCS#11 builder APIs
  - deprecated context-level SNI guidance

**Step 2: Roll the next queue**

- Move the queue to the next review-worthy contract gap after docs cleanup, likely the remaining builder/deprecation boundary in `BuildClient`.
