# OpenSSL Server OCSP Runtime Doc Truth Plan

**Goal:** 把 `OpenSSL` 服务端 OCSP stapling 的对外文档更新到刚完成的 runtime-proof 真值，避免调用方继续把能力理解成“只有 callback contract 接通”。

**Architecture:** 这批只改文档，不改实现。重点是把 `OpenSSL` server stapling 的 runtime 证据、builder file-load path，以及 `WithVerifyNone` 这条最小 server smoke 基线写清楚。

- `docs/BACKEND_CAPABILITY_MATRIX.md`：升级 OpenSSL server OCSP stapling 条目的 truth 描述。
- `docs/guides/OCSP_USAGE_GUIDE.md`：更新最小 server 代码与说明文字。
- `task_plan.md` / `findings.md` / `progress.md`：记录这批 docs truth 收口和验证结果。

**Files:**

- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/OCSP_USAGE_GUIDE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Sync the runtime truth

Update:

- OpenSSL server stapling is runtime-verified, not only callback-wired
- builder file-load path is included in that focused proof
- the path still only handles caller-provided stapled DER material
- builder examples that are meant to be non-mTLS server smoke should show `WithVerifyNone`

## Task 2: Verification

Run:

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `BACKEND_CAPABILITY_MATRIX` and `OCSP_USAGE_GUIDE` reflect the new OpenSSL runtime proof truth
- builder verify baseline is explicit in the server-side example
- repo compile gate passes
- minimal CI gate passes

## Result

- 状态：已完成
- `docs/BACKEND_CAPABILITY_MATRIX.md` 已明确 OpenSSL server stapling 现在是 runtime-verified，而不只是 callback-wired
- `docs/guides/OCSP_USAGE_GUIDE.md` 已把 `.WithVerifyNone` 写进最小 server 示例，并补上 runtime-proof 说明
- 验证结果：
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => compile gate `185/185`，模块测试 `17/17`，phase2 baseline dry-run PASS
