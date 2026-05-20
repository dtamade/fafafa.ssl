# Optional Backends Certificate Stream/Memory Truth

## Goal

把 `MbedTLS` / `WolfSSL`
证书对象对外发布的
stream / memory load-save surface：

- `LoadFromStream`
- `LoadFromMemory`
- `SaveToStream`

收紧成与当前仓库其他 backend
一致的 content-aware public truth，
避免调用方继续遇到：

- 文件能加载，
  同样内容的 `TStream`
  却加载失败
- valid PEM memory
  在 optional backends
  上被错误拒绝
- `SaveToStream`
  虽然能写出 PEM，
  但 `LoadFromStream`
  无法把它 roundtrip 回来

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/connection/test_wolfssl_metadata_accuracy.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重开 broader certificate redesign
  - 不改 `OpenSSL` / `FreePascal` / `WinSSL`
  - 不引入新的 native X509 binding

## Architecture Truth

- `OpenSSL` / `FreePascal`
  当前都已经对
  `LoadFromStream`
  / `LoadFromMemory`
  走内容感知：
  - PEM -> `LoadFromPEM`
  - DER -> `LoadFromDER`
- `MbedTLS`
  当前 `LoadFromStream`
  直接把原始流字节交给
  `LoadFromMemory`，
  但 PEM memory path
  没有补
  `#0`
  终止，
  这会让 PEM stream/memory
  成为弱语义
- `WolfSSL`
  当前 `LoadFromStream`
  只是转调
  `LoadFromMemory`，
  而后者只接受 DER，
  结果 public stream/memory
  surface 比 file/PEM surface
  更窄

## Steps

1. 在 framework tests 中增加 RED：
   - valid PEM memory load
   - `SaveToStream -> LoadFromStream`
     roundtrip
   - fingerprint truth 保持一致
2. 最小修复：
   - `MbedTLS.LoadFromMemory`
     对 PEM path
     做 content-aware parse
   - `WolfSSL.LoadFromMemory`
     / `LoadFromStream`
     改成 content-aware dispatch
   - `WolfSSL.LoadFromDER`
     改成 direct DER parse，
     避免和 `LoadFromMemory`
     互相递归
3. focused verification：
   - `tests/test_mbedtls_framework.pas`
   - `tests/test_wolfssl_framework.pas`
   - `tests/connection/test_wolfssl_metadata_accuracy.pas`
   - `git diff --check`

## Expected Result

- optional backends
  的 certificate stream/memory surface
  不再比 file surface
  更窄
- `SaveToStream`
  写出的 public PEM truth
  能被 `LoadFromStream`
  直接 roundtrip
- WolfSSL 旧的
  “PEM memory 必须失败”
  残留被收回到
  “malformed PEM 仍失败”
  的正确边界

## Result

- `MbedTLS`
  现在已经补齐：
  - PEM memory
    content-aware parse
  - `LoadFromStream`
    roundtrip
  - nil/zero input
    fail-closed state reset
- `WolfSSL`
  现在已经补齐：
  - PEM memory / stream
    content-aware dispatch
  - direct DER parse path
  - malformed PEM
    `False` + 清空状态，
    不再把
    `EBase64Error`
    向外逃逸
- focused verification：
  - `tests/connection/test_wolfssl_metadata_accuracy.pas`
    - PASS
  - `tests/test_wolfssl_framework.pas`
    - PASS
    - `245 passed / 0 failed`
  - `tests/test_mbedtls_framework.pas`
    - PASS
    - `231 passed / 0 failed`
  - `git diff --check`
    - PASS
