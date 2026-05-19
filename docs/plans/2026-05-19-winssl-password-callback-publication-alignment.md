# 2026-05-19 WinSSL Password Callback Publication Alignment

## Goal

继续沿着 callback setter/completeness 主线推进，收口 `WinSSL` 当前 callback surface 的 partial-publication drift：

- verify callback 有 runtime use-site
- info callback 有 runtime use-site
- password callback 只有 silent setter / field store

把 password callback 从静默 setter 收回到 fail-closed truth，并同步 WinSSL active docs、WinSSL unit test 预期、以及跨 backend callback setter contract。

## Scope

- 只处理：
  - `WinSSL` password callback
  - `WinSSL` active docs / unit test 预期
  - callback setter runtime contract 的 WinSSL 矩阵
- 不改：
  - `WinSSL` verify/info callback runtime path
  - `SupportsCallbacks` bool 结构
  - 其它 backend

## Files

- `src/fafafa.ssl.winssl.context.pas`
- `tests/unit/test_winssl_comprehensive.pas`
- `tests/test_backend_callback_setter_fail_closed_contract.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_DESIGN.md`
- `tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `SupportsCallbacks=True` 当前只代表至少一条 published callback runtime path 存在
- 对 `WinSSL` 来说，当前静态证据已确认：
  - verify callback 被连接验证路径真实消费
  - info callback 被 connection info-notify 路径真实消费
  - password callback 没有 use-site，也没有 access seam
- 因而当前正确语义不是“WinSSL 三种 callback 都支持”
- 而是“WinSSL 发布了 partial callback surface，其中 password callback 仍应 fail-closed”

## Steps

1. 补 focused source contract，让 WinSSL password callback drift 先 RED。
2. 让 `TWinSSLContext.SetPasswordCallback` 对 non-nil 赋值 fail-closed，`nil` 保持 clear/no-op。
3. 同步：
   - `test_winssl_comprehensive`
   - callback setter runtime contract
   - `API_REFERENCE`
   - `WINSSL_DESIGN`
4. 跑 focused proof，回写台账并提交。

## Commands

```bash
bash -n tests/scripts/test_winssl_password_callback_partial_publication_contract.sh
bash tests/scripts/test_winssl_password_callback_partial_publication_contract.sh
mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas
./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract
bash tests/scripts/test_callback_setter_fail_closed_contract.sh
git diff --check
```

## Expected Result

- `WinSSL` password callback 不再 silently accept non-nil assignment
- `WinSSL` verify/info callback 继续保持 published runtime path
- WinSSL active docs / unit test / callback runtime contract 同步到 partial-publication truth
