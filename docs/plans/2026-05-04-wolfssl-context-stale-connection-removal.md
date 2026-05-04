# WolfSSL Context Stale Connection Removal Plan

**Goal:** 删除 `src/fafafa.ssl.wolfssl.context.pas` 里仍保留的旧 `TWolfSSLConnection` 私有残留实现，避免它继续和 `src/fafafa.ssl.wolfssl.connection.pas` 的现代连接类分叉。

**Architecture:** 这批不改 `TWolfSSLContext.CreateConnection(...)` 已经收口好的工厂路径，也不改 WolfSSL handshake、OCSP、early-data 逻辑。只做两件事：
- 用一个 focused source contract 锁住 `wolfssl.context` 不再自己实现旧 `TWolfSSLConnection` 私有类；
- 删除 `implementation` 区里的旧类声明与整段旧实现，保留现代 `wolfssl.connection` 路径作为唯一 truth source。

**Files:**
- Modify: `src/fafafa.ssl.wolfssl.context.pas`
- Add: `tests/scripts/test_wolfssl_context_stale_connection_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove stale private connection implementation still exists

Run:

```bash
bash -n tests/scripts/test_wolfssl_context_stale_connection_contract.sh
bash tests/scripts/test_wolfssl_context_stale_connection_contract.sh
```

Add checks:
- `wolfssl.context` 不应再保留 `TWolfSSLConnection = class(TInterfacedObject, ...)`
- `wolfssl.context` 不应再保留旧的 `constructor TWolfSSLConnection.Create(...)` / `destructor TWolfSSLConnection.Destroy`
- `TWolfSSLContext.CreateConnection(...)` 仍应继续走 `fafafa.ssl.wolfssl.connection.TWolfSSLConnection`

Expected possibilities:
- 如果直接 RED，说明私有残留仍真实存在，这批进入最小 GREEN
- 如果直接全绿，说明残留已提前被清掉，这批只作为 completion audit 收口

## Task 2: GREEN - remove the stale implementation

Change:
- `src/fafafa.ssl.wolfssl.context.pas`
  - 删除旧类声明与整段旧实现

Constraints:
- 不修改 `src/fafafa.ssl.wolfssl.connection.pas` 的运行时逻辑
- 不把 `implementation` 私有残留误判成公开 API 再额外扩 surface
- 不顺手重构无关的 WolfSSL context 配置代码

## Task 3: Verification

Run:

```bash
bash -n tests/scripts/test_wolfssl_context_stale_connection_contract.sh
bash tests/scripts/test_wolfssl_context_stale_connection_contract.sh
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `wolfssl.context` 不再保留旧 `TWolfSSLConnection` 私有实现
- focused source contract、compile gate、minimal CI gate 全绿
