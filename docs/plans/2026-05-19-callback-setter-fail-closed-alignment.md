# 2026-05-19 Callback Setter Fail-Closed Alignment

## Goal

继续沿着 callback interface/completeness 主线推进，把 `SupportsCallbacks=False` backend 的
`SetVerifyCallback` / `SetPasswordCallback` / `SetInfoCallback` 从当前 setter-only / silent-store
状态收回到 fail-closed truth，同时修正 active API reference 里的 callback 类型签名漂移。

## Scope

- 只处理：
  - `FreePascal`
  - `WolfSSL`
  - `MbedTLS`
  - active callback docs / API reference truth
- 不改：
  - `OpenSSL` / `WinSSL` 已发布 callback runtime path
  - callback 运行时实现设计本身
  - 其它无关 capability

## Files

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.wolfssl.context.pas`
- `src/fafafa.ssl.mbedtls.context.pas`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_callback_setter_fail_closed_contract.sh`
- `tests/test_backend_callback_setter_fail_closed_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 前一批已经收口：
  - `OpenSSL` / `WinSSL` 发布 `SupportsCallbacks=True`
  - `FreePascal` / `WolfSSL` / `MbedTLS` 发布 `SupportsCallbacks=False`
- 但当前 `False` backend 的 verify/password/info callback setter 仍然只是：
  - 接收 non-nil callback
  - 存进字段
  - runtime 永远不消费
- 这会把“未发布 capability”重新弱化成“接口看起来能设”，属于 silent no-op / misleading setter drift
- active API reference 里的 callback 类型示例签名还停留在旧形态，已与源码不一致

## Target Semantics

- `SupportsCallbacks=True`
  - non-nil callback assignment 允许继续工作
  - `nil` 允许清除并回到默认行为
- `SupportsCallbacks=False`
  - non-nil callback assignment 必须 fail-closed 为 `unsupported`
  - `nil` 允许作为清除/保持默认行为的 compatibility operation

## Steps

1. 补 source/docs contract 与 runtime contract，先 RED。
2. 让 `FreePascal` / `WolfSSL` / `MbedTLS` 的 callback setter 对 non-nil 赋值 fail-closed。
3. 把 `base` / `API_REFERENCE` 的 callback gating 与 callback type signatures 写回当前真相。
4. 跑 focused proof，更新台账并提交。

## Commands

```bash
bash -n tests/scripts/test_callback_setter_fail_closed_contract.sh
bash tests/scripts/test_callback_setter_fail_closed_contract.sh
mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas
./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract
git diff --check
```

## Expected Result

- `SupportsCallbacks=False` backend 不再 silently accept non-nil callback setter
- `nil` callback clear path 继续可用
- active API reference callback 类型签名与源码一致
- callback interface/completeness 路线继续从“能力真相”推进到“setter 语义真相”
