# MbedTLS Protocol Capability Doc Truth Alignment

## Goal

收口 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
里协议支持表的活跃文档漂移，
把当前 dedicated page
重新拉回 source truth：

- `TLS 1.0` / `TLS 1.1` 当前不发布
- `DTLS 1.0` / `DTLS 1.2` 当前不发布

## Scope

- 新增 focused shell contract，冻结 MbedTLS source / test / doc truth
- 最小修正 `MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`
- 不补做新的 MbedTLS 协议实现
- 不扩到 cipher / Ed25519 / async / OCSP 等其他 MbedTLS 话题

## Architecture Truth

- `src/fafafa.ssl.mbedtls.lib.pas`
  当前明确发布：
  - `sslProtocolTLS10: Result := False;`
  - `sslProtocolTLS11: Result := False;`
  - `sslProtocolDTLS10, sslProtocolDTLS12: Result := False;`
  - `Result.MinTLSVersion := sslProtocolTLS12;`
  - `Result.SupportsDTLS := False`
- `tests/test_mbedtls_framework.pas`
  已冻结：
  - `DTLS 1.0 not supported`
  - `DTLS 1.2 not supported`
  - `SupportsDTLS` 必须与 runtime protocol support 一致
- dedicated MbedTLS page
  目前仍把：
  - `TLS 1.0` / `TLS 1.1`
    写成 `⚠️ 可选`
  - `DTLS 1.0`
    写成 `⚠️ 可选`
  - `DTLS 1.2`
    写成 `✅ 支持`

## Files

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_mbedtls_protocol_capability_doc_truth_contract.sh`
- `docs/plans/2026-05-20-mbedtls-protocol-capability-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认 dedicated MbedTLS matrix 先 RED
3. 最小修正协议表：
   - `TLS 1.0`
   - `TLS 1.1`
   - `DTLS 1.0`
   - `DTLS 1.2`
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_mbedtls_protocol_capability_doc_truth_contract.sh
bash tests/scripts/test_mbedtls_protocol_capability_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- MbedTLS 专页不再把
  `TLS 1.0 / 1.1 / DTLS 1.0 / DTLS 1.2`
  写成当前已发布或可选支持
- dedicated page
  会明确：
  - 当前 shipped public/runtime capability
    从 `TLS 1.2+` 起步
  - 当前不发布 DTLS protocol support
