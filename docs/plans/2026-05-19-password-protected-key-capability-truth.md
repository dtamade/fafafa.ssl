# 2026-05-19 Password-Protected Key Capability Truth

## Goal

继续沿着“接口设计 / 各 backend capability 真值”主线推进，收口 password-protected private key 这条残余：

- `FreePascal` / `WolfSSL` 当前把 `SupportsPasswordProtectedKeys` 发布为 `True`
- 但现有实现没有真正消费 `LoadPrivateKey(..., APassword)` / `LoadPrivateKeyPEM(..., APassword)` 的非空密码
- 同时 `WolfSSL` 代码里还保留了“密码回调单独设置”的旧注释，会继续误导后续维护和调用方判断

这会形成和 callback 批次很像的两类风险：

- capability 假阳性
- non-empty `APassword` 被 silent-ignore 或落到错误的晚期失败点

## Scope

- 只处理 password-protected private key 相关的 capability / runtime / active docs truth：
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.wolfssl.context.pas`
  - `src/fafafa.ssl.base.pas`
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- 用 focused shell contract + Pascal runtime contract 锁住真相
- 不扩展到真正补做 FreePascal/WolfSSL 的 encrypted private key runtime
- 不重做 capability 结构

## Files

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `src/fafafa.ssl.wolfssl.context.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_password_protected_key_capability_truth_contract.sh`
- `tests/test_backend_password_protected_key_capability_truth_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `FreePascal`
  - 当前没有 published password-protected private key runtime path
  - `LoadPrivateKey*` 的非空 `APassword` 不应再 silent-ignore
  - `SupportsPasswordProtectedKeys` 应回收到 `False`
- `WolfSSL`
  - 当前没有 shipped password callback bridge，也没有 published password-protected private key runtime path
  - `LoadPrivateKey*` 的非空 `APassword` 不应再继续假装“可单独设置 callback”
  - `SupportsPasswordProtectedKeys` 应回收到 `False`
- `WinSSL`
  - 仍保留 coarse-grained `SupportsPasswordProtectedKeys=True`
  - 但 active docs 需要写清当前只是 password-protected PFX/P12 import path，PEM private-key password path 仍不发布
- `MbedTLS`
  - 继续保留 `SupportsPasswordProtectedKeys=True`
- `OpenSSL`
  - 继续沿用当前 runtime-aware capability 逻辑

## Steps

1. 先补 focused contract，让 capability 假阳性和 silent-ignore 先 RED。
2. 把 `FreePascal` / `WolfSSL` capability 收回，并让 non-empty `APassword` fail-closed。
3. 同步 active docs，把 WinSSL 的 partial password-protected path 和 false-backend fail-closed 真相写回矩阵/API 参考。
4. 跑 focused contracts，回写台账并提交。

## Commands

```bash
bash -n tests/scripts/test_password_protected_key_capability_truth_contract.sh
bash tests/scripts/test_password_protected_key_capability_truth_contract.sh
mkdir -p tmp/test_password_protected_key_capability_truth && \
  fpc -B -Fu./src -Fu./tests \
    -FUtmp/test_password_protected_key_capability_truth \
    -FEtmp/test_password_protected_key_capability_truth \
    -otmp/test_password_protected_key_capability_truth/test_backend_password_protected_key_capability_truth_contract \
    tests/test_backend_password_protected_key_capability_truth_contract.pas && \
  ./tmp/test_password_protected_key_capability_truth/test_backend_password_protected_key_capability_truth_contract
git diff --check
```

## Expected Result

- `FreePascal` / `WolfSSL` 不再把 password-protected private key support 发布成已实现 capability
- 非空 `APassword` 在未发布 backend 上会 fail-closed，而不是 silent-ignore
- active docs 不再把 WinSSL / false-backend 的私钥密码路径说得比真实实现更宽
