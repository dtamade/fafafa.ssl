# 2026-05-19 OpenSSL Callback Publication Runtime Gate

## Goal

继续沿着 interface/backend completeness 主线推进，收口 `OpenSSL` callback capability 的一个真实实现漂移：

- `GetCapabilities` 当前无条件发布 `SupportsCallbacks=True`
- 但 `SetVerifyCallback` / `SetPasswordCallback` / `SetInfoCallback` 实际上仍依赖 runtime symbol/helper 是否真的存在

这会造成对外 contract 撒谎：

- capability 说支持
- 某些 build 上实际安装 callback 时却抛 `unsupported`

本批把 OpenSSL callback publication 改成 runtime-aware strict gate，并让 setter 在 callback surface 不完整时统一 fail-closed。

## Scope

- 只处理：
  - `OpenSSL` callback capability 发布
  - `OpenSSL` verify/password/info setter 的 fail-closed 语义
  - 对应 focused contract / runtime drift proof
- 不重做：
  - `SupportsCallbacks` 粒度设计
  - WinSSL partial callback 设计
  - 其它 backend callback 实现

## Files

- `src/fafafa.ssl.openssl.api.ssl.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.openssl.context.pas`
- `tests/scripts/test_callback_capability_truth_contract.sh`
- `tests/scripts/test_callback_setter_fail_closed_contract.sh`
- `tests/test_backend_callback_capability_truth_contract.pas`
- `tests/test_backend_callback_setter_fail_closed_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `SupportsCallbacks` 是 coarse-grained published flag，不表达每个 callback kind 的独立粒度
- 对 `OpenSSL` 来说，在没有更细 capability 字段之前，最安全的 truth 是：
  - 只有当 verify/password/info callback 所需 runtime helper 全部就绪时，才发布 `SupportsCallbacks=True`
- 尤其 password callback 不只依赖：
  - `SSL_CTX_set_default_passwd_cb`
  - 还依赖：
    - `SSL_CTX_set_default_passwd_cb_userdata`
  否则 thunk 无法拿到 `Self`，等于 published path 不完整
- 因而 callback surface 不完整时：
  - `GetCapabilities.SupportsCallbacks` 必须回落为 `False`
  - 三个 non-nil setter 都必须 fail-closed 为 unsupported
  - `nil` clear 继续允许作为 compatibility clear/no-op

## Steps

1. 先补 shell contract 与 focused Pascal runtime drift contract，让当前 unconditional publication 先 RED。
2. 提供共享的 OpenSSL callback-surface readiness helper。
3. 让 `GetCapabilities` 与三个 setter 统一跟随这条 helper。
4. 跑 focused verification，更新台账并提交。

## Commands

```bash
bash -n tests/scripts/test_callback_capability_truth_contract.sh
bash tests/scripts/test_callback_capability_truth_contract.sh
bash -n tests/scripts/test_callback_setter_fail_closed_contract.sh
bash tests/scripts/test_callback_setter_fail_closed_contract.sh
mkdir -p tmp/test_callback_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_capability_truth -FEtmp/test_callback_capability_truth -otmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas
./tmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract
mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas
./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract
git diff --check
```

## Expected Result

- `OpenSSL` 不再无条件发布 `SupportsCallbacks=True`
- callback publication 与 setter runtime truth 重新一致
- callback surface 缺 helper 的 build 不再对外“先说支持，再在 setter 时翻车”
