# 2026-05-23 FreePascal Embedded SCT Fail-Closed Alignment

## Goal

把 FreePascal client CT/SCT surface 中 embedded X.509 SCT fallback 的错误语义收口到既有契约：只要证书里真的带了 malformed `signed_certificate_timestamp`，连接就必须 fail-closed，而不是吞错后退化成“好像没有 SCT”。

## Scope

- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- FreePascal client CT surface 允许从多个来源 materialize SCT：
  - TLS `signed_certificate_timestamp` 扩展
  - embedded X.509 SCT 扩展
  - OCSP-delivered SCT
- 当 CT surface 已经发现某个来源存在 SCT，但该来源内容 malformed 时，语义必须 fail-closed；否则 runtime proof 会把“坏数据”误降级成“无数据”。
- 这批不重设计 parser，也不扩新的 CT policy，只把 embedded SCT fallback 的错误传播对齐到现有 malformed TLS SCT / malformed OCSP SCT 语义。

## Steps

1. 复现 `tests/test_freepascal_client_ct_sct_surface.pas` 中 malformed embedded SCT list 的 fail-open 回归。
2. 在 `TFreePascalConnection.TryCachePeerCertificatesFromHandshake` 中保留 embedded SCT helper 的错误，不再把 `AError` 清空。
3. 在该错误路径上清理 peer cert cache 并提前退出，让握手按 CT surface 契约 fail-closed。
4. 重新运行 focused CT/SCT surface 测试与 fast-local completeness gate，确认没有新的扩散回归。

## Verification

- `mkdir -p tmp/test_freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_ct_sct_surface -FEtmp/test_freepascal_client_ct_sct_surface -otmp/test_freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/test_freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- `python3 -u scripts/compile_all_modules.py`
- `git diff --check`

## Outcome

- malformed embedded SCT list 现在会和 malformed TLS SCT list 一样 fail-closed。
- focused CT/SCT surface 运行通过。
- fast-local completeness gate `run_id=20260523_191741` 为 `18/18 PASS`，summary 位于 `tmp/test-reports/freepascal_tls13_completeness_20260523_191741.md`。
- `python3 -u scripts/compile_all_modules.py` 结果仍是历史已知边界：`185/186` 通过，仅 `fafafa.ssl.pkcs11.engine.pas` 失败。
