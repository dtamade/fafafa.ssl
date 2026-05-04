# Early-Data Host-Gated Truth Alignment Plan

**Goal:** 把当前仓库里 `early-data` 的 contract 和文档收口到真实 host/runtime truth：`OpenSSL` 保持 stable，`FreePascal` 保持 experimental，`WinSSL` / `MbedTLS` 保持 none，`WolfSSL` 则明确改成“按 build/runtime helper 门控；helper 缺失时 capability 发布为 `none`，context/connection 不暴露 early-data 接口”。

**Architecture:** 这批不重开 `FreePascal` 0-RTT 实现线，也不扩 `WolfSSL` 新功能。先把 focused Pascal contract 改成真正能在当前主机验证 `WolfSSL` 的 runtime helper truth，而不是继续因为未显式链接 backend unit 而 `[SKIP]`。再新增 docs contract，锁住 `README.md`、`docs/BACKEND_CAPABILITY_MATRIX.md`、`docs/guides/EARLY_DATA_GUIDE.md` 的表述必须和当前实现/主机证据一致。若 Pascal contract 直接全绿，则本批只做文档 truth alignment，不改 `src/`。

**Files:**

- Add: `tests/scripts/test_early_data_docs_truth_contract.sh`
- Modify: `tests/test_openssl_wolfssl_early_data_connection_contract.pas`
- Modify: `README.md`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/EARLY_DATA_GUIDE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove current contract/docs drift

Run:

```bash
mkdir -p tmp/openssl_wolfssl_early_data_units
fpc -B -Fu./src -Fu./tests -FUtmp/openssl_wolfssl_early_data_units -FEtmp/openssl_wolfssl_early_data_units -otest_openssl_wolfssl_early_data_connection_contract tests/test_openssl_wolfssl_early_data_connection_contract.pas
./tmp/openssl_wolfssl_early_data_units/test_openssl_wolfssl_early_data_connection_contract
bash -n tests/scripts/test_early_data_docs_truth_contract.sh
bash tests/scripts/test_early_data_docs_truth_contract.sh
```

Expected drift:

- 旧 focused contract 还会把 `WolfSSL` 固定期待成 `experimental`，或因为未显式链接 backend unit 而在当前主机继续 `[SKIP]`
- `README.md` 仍把 `FreePascal` 早数据写成 production ready
- `docs/guides/EARLY_DATA_GUIDE.md` 仍把 `FreePascal` 写成完整支持，并把 `WolfSSL` 写成无条件实验性
- `docs/BACKEND_CAPABILITY_MATRIX.md` 的 `WolfSSL` early-data 段落还缺少 “helper 缺失 => `sslSupportNone` + interface absent” 的 host-gated truth

## Task 2: GREEN - align contracts first, docs second

Change:

- `tests/test_openssl_wolfssl_early_data_connection_contract.pas`
  - 显式链接 `OpenSSL` / `WolfSSL` backend unit
  - `WolfSSL` 的 expected support 改按 runtime helper presence 推导
  - `EarlyDataSupport = none` 时，显式断言 context / connection interface absent
- `README.md`
  - Early Data 顶部摘要和表格收口到当前真相
- `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `WolfSSL` early-data 段落补 helper-gated truth
  - `FreePascal` 继续保持 experimental wording
- `docs/guides/EARLY_DATA_GUIDE.md`
  - 支持表格、快速开始前提、以及 backend 说明全部收口到同一真相

Constraints:

- 不修改 `src/fafafa.ssl.openssl.*` / `src/fafafa.ssl.wolfssl.*`
- 不把 `WolfSSL` 的 host-gated `none` 曲解成 backend family 永久不支持
- 不把 `FreePascal` experimental/local-persistent caveat 擅自提升为 production ready

## Task 3: Verification

Run:

```bash
mkdir -p tmp/openssl_wolfssl_early_data_units
fpc -B -Fu./src -Fu./tests -FUtmp/openssl_wolfssl_early_data_units -FEtmp/openssl_wolfssl_early_data_units -otest_openssl_wolfssl_early_data_connection_contract tests/test_openssl_wolfssl_early_data_connection_contract.pas
./tmp/openssl_wolfssl_early_data_units/test_openssl_wolfssl_early_data_connection_contract
bash -n tests/scripts/test_early_data_docs_truth_contract.sh
bash tests/scripts/test_early_data_docs_truth_contract.sh
git diff --check -- docs/plans/2026-05-05-early-data-host-gated-truth-alignment.md tests/scripts/test_early_data_docs_truth_contract.sh tests/test_openssl_wolfssl_early_data_connection_contract.pas README.md docs/BACKEND_CAPABILITY_MATRIX.md docs/guides/EARLY_DATA_GUIDE.md task_plan.md findings.md progress.md
```

Formatting:

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/plans/2026-05-05-early-data-host-gated-truth-alignment.md /home/dtamade/projects/fafafa.ssl/README.md /home/dtamade/projects/fafafa.ssl/docs/BACKEND_CAPABILITY_MATRIX.md /home/dtamade/projects/fafafa.ssl/docs/guides/EARLY_DATA_GUIDE.md /home/dtamade/projects/fafafa.ssl/task_plan.md /home/dtamade/projects/fafafa.ssl/findings.md /home/dtamade/projects/fafafa.ssl/progress.md
```

## Definition Of Done

- focused Pascal contract 不再把当前 `WolfSSL` host truth 跳过或误判为固定 experimental
- 文档和 README 不再把 `FreePascal` early-data 写成 production ready
- `WolfSSL` early-data 的 helper-gated truth 被明确记录
- focused contract、docs contract、diff hygiene 全绿
- broad objective 的剩余 blocker 被进一步收口到实现层真正未完成的 family，而不是文档/contract 漂移
