# WolfSSL Server OCSP Runtime Proof Plan

**Goal:** 为 `WolfSSL` 服务端 OCSP stapling issuance 补当前真实 TLS 1.3 运行时证据：在本机先证明 scripted `TStream` baseline handshake 和 public surface 已经闭合；对 `configured + requested => stapled DER` 这条 emission 场景，则只在 `wolfSSL >= 5.9.1` 主机上执行 runtime proof，旧版本主机明确 skip 并记录上游边界。

**Architecture:** 这批不扩新接口、不重开 online OCSP、也不改 OpenSSL / FreePascal 其他主线。只在 `WolfSSL` 上加 focused runtime proof，并修正现有 focused contract 的 backend bring-up 方式，避免当前主机继续误报 `[SKIP]`。如果本机动态库证据表明 configured-emission 失败来自旧版 WolfSSL，上游边界直接体现在测试 skip 和文档中，不继续硬拗成本地生产代码缺口。
- `tests/test_wolfssl_ocsp_stapling_contract.pas`：修正 backend bring-up，让现有 contract 真跑。
- `tests/wolfssl/*`：新增 runtime contract，用 scripted `TStream` 建立 `WolfSSL server <-> scripted TLS 1.3 client` 握手。
- `src/fafafa.ssl.wolfssl.*`：只有在 runtime proof 暴露真实缺口时才做最小修复。
- `task_plan.md` / `findings.md` / `progress.md`：记录这批真正的 runtime 结论和 host/runtime 边界。

**Files:**
- Modify: `tests/test_wolfssl_ocsp_stapling_contract.pas`
- Add: `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
- Modify: `src/fafafa.ssl.wolfssl.*` (only if runtime proof exposes a real bug)
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Status

- 状态：已完成（host-gated closeout）
- 当前主机：`wolfSSL 5.7.2`
- 真实结果：
  - baseline `WolfSSL server + scripted TStream` TLS 1.3 handshake 已通过
  - `tests/test_wolfssl_ocsp_stapling_contract.pas` 已不再误报 `[SKIP]`
  - `configured + requested => stapled DER` 与 builder file-load emission 场景在 `5.7.2` 上不进入 `wolfSSL_set_tlsext_status_ocsp_resp(...)`，现按 `wolfSSL >= 5.9.1` 做显式门控
  - `python3 scripts/compile_all_modules.py` 与 `bash scripts/run_minimal_ci_gate.sh --fast-local` 已通过

## Task 1: RED - prove the missing WolfSSL runtime coverage

Run:

```bash
fpc -Fu./src tests/test_wolfssl_ocsp_stapling_contract.pas -otmp/test_wolfssl_ocsp_stapling_contract
./tmp/test_wolfssl_ocsp_stapling_contract
fpc -Fu./src tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas -otmp/test_wolfssl_server_ocsp_stapling_runtime
./tmp/test_wolfssl_server_ocsp_stapling_runtime
```

Cover:
- direct `configured + requested => client receives stapled DER`（仅在 `wolfSSL >= 5.9.1` 主机执行）
- direct `configured + not requested => absent`
- direct `no material + requested => absent`
- builder `WithServerOCSPStapledResponseFile(...) => server loads bytes and emits them when requested`（仅在 `wolfSSL >= 5.9.1` 主机执行）

## Task 2: GREEN - fix only if runtime proof fails on a supported host

Constraint:
- keep fixes bounded to `WolfSSL` server stapling runtime seam
- do not widen into client verification policy, CT, online responder fetch, or other backend work
- if the failure is limited to `wolfSSL < 5.9.1`, close out with a version-gated skip instead of widening repo-side changes

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/test_wolfssl_ocsp_stapling_contract.pas -otmp/test_wolfssl_ocsp_stapling_contract && ./tmp/test_wolfssl_ocsp_stapling_contract
fpc -Fu./src tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas -otmp/test_wolfssl_server_ocsp_stapling_runtime && ./tmp/test_wolfssl_server_ocsp_stapling_runtime
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `WolfSSL` server stapling baseline path has real handshake evidence, not only source contract / focused capability contract
- direct / builder configured emission scenes run only on `wolfSSL >= 5.9.1`, and older hosts produce explicit skip truth instead of false failure or false green
- current host no longer reports a false `[SKIP]` for the existing WolfSSL contract
- repo compile gate passes
- minimal CI gate passes
