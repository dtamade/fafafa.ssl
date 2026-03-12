# Runtime Contract Cleanup PR Summary (March 7, 2026)

This page is a PR-ready summary for the March 7, 2026 runtime-contract cleanup.

## English

### Summary
- Added explicit runtime or compile-only contract coverage for all active Pascal `program` entrypoints under `tests/integration` and `tests/certificate`.
- Introduced a top-level runtime regression batch at `tests/scripts/test_active_program_runtime_contract_batch.sh` plus a coverage guard at `tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh`.
- Consolidated overlapping runtime-contract scripts to reduce maintenance cost while preserving coverage.
- Added stable ASCII completion markers to heavy smoke and workflow-style programs so shell contracts can assert success reliably across terminals and locales.
- Fixed several real compatibility drifts uncovered during the sweep, including OpenSSL serial-number helper loading, X.509 test assumptions, certificate metadata array semantics, and legacy native-handle usage in tests.

### Key outcomes
- Active Pascal `program` entrypoints left without contract coverage: `0`
- Deleted runtime-contract script stale references left in docs and memory files: `0`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` passes
- `python3 scripts/compile_all_modules.py` passes with `231/231`

### Notable implementation details
- `src/fafafa.ssl.openssl.certificate.pas` now loads the required BN / ASN.1 helpers before formatting certificate serial numbers.
- `tests/integration/test_x509_basic.pas` now matches current OpenSSL X.509 name encoding and version semantics.
- `tests/certificate/test_cert_store.pas` now uses the canonical native-handle helper instead of an outdated direct interface member.
- `tests/certificate/test_certificate_unit.pas` now aligns with the current `TSSLStringArray` metadata API.
- Environment-specific certificate debug coverage remains explicit and isolated via `tests/scripts/test_cert_load_debug_contract.sh`.

### Current entrypoints
- Broad active-program sweep:
  - `bash tests/scripts/test_active_program_runtime_contract_batch.sh`
- Fast default gate:
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `python3 scripts/compile_all_modules.py`
- Current script map:
  - `docs/plans/2026-03-07-runtime-contracts-current-index.md`
- Historical cleanup trail:
  - `docs/plans/2026-03-07-runtime-contracts-historical-index.md`

## 中文

### 摘要
- 为 `tests/integration` 与 `tests/certificate` 下所有 active Pascal `program` 入口补齐了 runtime 或 compile-only 合同覆盖。
- 新增顶层 runtime 回归批处理入口 `tests/scripts/test_active_program_runtime_contract_batch.sh`，并配套 coverage 守护 `tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh`。
- 合并了多份重叠的 runtime 合同脚本，在不降低覆盖率的前提下减少维护成本。
- 为重型 smoke / workflow 程序补了稳定 ASCII 完成标记，使 shell 合同可以跨终端、跨 locale 稳定断言成功。
- 顺手修复了本轮扫描暴露出的几类真实漂移问题，包括 OpenSSL 序列号依赖加载、X.509 测试假设、证书元数据数组语义以及旧 native-handle 调用方式。

### 关键结果
- 未纳管的 active Pascal `program` 入口数：`0`
- 文档与记忆文件中已删除 runtime 合同脚本的残留引用数：`0`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` 通过
- `python3 scripts/compile_all_modules.py` 通过，结果为 `231/231`

### 值得注意的实现点
- `src/fafafa.ssl.openssl.certificate.pas` 现在会在格式化证书序列号前按需加载 BN / ASN.1 helper。
- `tests/integration/test_x509_basic.pas` 已对齐当前 OpenSSL 的 X.509 名称编码与版本语义。
- `tests/certificate/test_cert_store.pas` 已改用规范的 native-handle helper，而不是旧的直接接口成员调用。
- `tests/certificate/test_certificate_unit.pas` 已对齐当前 `TSSLStringArray` 证书元数据 API。
- 环境耦合较强的证书调试探针仍保持显式隔离，通过 `tests/scripts/test_cert_load_debug_contract.sh` 管理。

### 当前入口
- 广覆盖 active-program sweep：
  - `bash tests/scripts/test_active_program_runtime_contract_batch.sh`
- 默认快速门禁：
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `python3 scripts/compile_all_modules.py`
- 当前脚本索引：
  - `docs/plans/2026-03-07-runtime-contracts-current-index.md`
- 历史收口轨迹：
  - `docs/plans/2026-03-07-runtime-contracts-historical-index.md`
