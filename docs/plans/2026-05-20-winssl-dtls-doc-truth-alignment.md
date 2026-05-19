# WinSSL DTLS Doc Truth Alignment

## Goal

收口 `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
里对 `DTLS 1.0 / 1.2`
的活跃文档漂移，
把 dedicated backend page
重新拉回当前 source truth：

- `SupportsDTLS = False`
- 当前 WinSSL / Schannel backend
  不发布 DTLS public/runtime capability

## Scope

- 新增 focused shell contract，冻结 WinSSL source / doc truth
- 最小修正 `WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`
- 不补做新的 WinSSL DTLS 实现
- 不扩到 TLS 1.3 / session / callback / private-key 等其他 WinSSL 话题

## Architecture Truth

- `src/fafafa.ssl.winssl.lib.pas`
  当前明确发布：
  - `Result.SupportsDTLS := False;  // Schannel 不支持 DTLS`
- 活跃 dedicated WinSSL matrix
  目前仍把：
  - `DTLS 1.0`
  - `DTLS 1.2`
  写成不同 Windows 版本下的
  `✅ / ⚠️ / ❌`
  组合，
  这和当前库层 published capability
  已经直接冲突
- 这批只收专页真值，
  不把它扩成新的 WinSSL DTLS 支持路线

## Files

- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_winssl_dtls_doc_truth_contract.sh`
- `docs/plans/2026-05-20-winssl-dtls-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认 dedicated WinSSL matrix 先 RED
3. 最小修正 WinSSL dedicated matrix 的 `DTLS 1.0 / 1.2` 两行
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_winssl_dtls_doc_truth_contract.sh
bash tests/scripts/test_winssl_dtls_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- WinSSL 专页不再把 `DTLS 1.0 / 1.2`
  写成当前已发布或部分支持
- dedicated backend page
  会明确：
  - 当前 `SupportsDTLS=False`
  - WinSSL / Schannel backend
    当前不发布 DTLS public/runtime path
