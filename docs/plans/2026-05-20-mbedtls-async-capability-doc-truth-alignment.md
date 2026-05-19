# MbedTLS Async Capability Doc Truth Alignment

## Goal

收口 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
里
`异步操作`
这一行过于宽泛的活跃文档表述，
让专页重新回到当前 source / API reference
已经明确表达的
public surface truth：

- 当前有
  `WantRead / WantWrite`
  这类非阻塞重试语义
- 但没有
  dedicated async callback / job / event-loop
  public capability

## Scope

- 新增 focused shell contract，锁住当前 source / API / dedicated-doc truth
- 最小修正 `MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`

不做：

- 不改 MbedTLS 实现
- 不扩到 `Ed25519` / curves / session / Windows 等其他话题
- 不重开 broader async 架构设计

## Architecture Truth

- `src/fafafa.ssl.base.pas`
  当前 `ISSLConnection`
  已发布：
  - `WantRead: Boolean`
  - `WantWrite: Boolean`
- `src/fafafa.ssl.mbedtls.connection.pas`
  当前明确用：
  - `MBEDTLS_ERR_SSL_WANT_READ`
  - `MBEDTLS_ERR_SSL_WANT_WRITE`
  驱动：
  - `DoWantRead`
  - `DoWantWrite`
  - `sslHsInProgress`
- `tests/test_mbedtls_framework.pas`
  当前已冻结：
  - `ERR_SSL_WANT_READ -> sslErrWantRead`
  - `ERR_SSL_WANT_WRITE -> sslErrWantWrite`
- 但 dedicated MbedTLS page
  现在仍只写：
  - `异步操作 | ⚠️ 部分 | 非阻塞 I/O`
  这会把
  “有重试语义”
  和
  “对外发布专门 async capability”
  混成一个模糊结论

## Files

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `tests/test_mbedtls_framework.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_mbedtls_async_capability_doc_truth_contract.sh`
- `docs/plans/2026-05-20-mbedtls-async-capability-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认当前 dedicated doc 先 RED
3. 最小修正 MbedTLS dedicated matrix 的 `异步操作` 行
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_mbedtls_async_capability_doc_truth_contract.sh
bash tests/scripts/test_mbedtls_async_capability_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- MbedTLS 专页不再只写模糊的
  `非阻塞 I/O`
- 专页会明确：
  - 当前 public surface
    通过
    `WantRead / WantWrite`
    暴露非阻塞重试语义
  - 当前没有 dedicated async callback / job public capability

