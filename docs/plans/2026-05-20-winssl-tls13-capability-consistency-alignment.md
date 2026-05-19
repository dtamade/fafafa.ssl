# WinSSL TLS 1.3 Capability Consistency Alignment

## Goal

收口 `WinSSL` backend
当前在同一 capability 主题下
自相矛盾的 source truth：

- `GetCapabilities.SupportsTLS13`
  当前按
  `Windows 10 1903+`
  发布
- 但
  `IsProtocolSupported(sslProtocolTLS13)`
  却仍按
  `Build 20348+`
  判定

这会直接导致：

- capability record
  说
  `TLS 1.3=True`
- runtime protocol query
  却返回
  `False`

从而破坏
`ISSLLibrary.GetCapabilities`
与
`ISSLLibrary.IsProtocolSupported(...)`
之间最基本的一致性。

## Scope

- 新增 focused shell contract，冻结当前正确的一致性 truth
- 最小修正 `src/fafafa.ssl.winssl.lib.pas`
- 最小修正 `tests/winssl/test_winssl_unit_comprehensive.pas`
  里的旧平台叙事
- 更新 `task_plan.md` / `findings.md` / `progress.md`

不做：

- 不重开 WinSSL 新功能实现
- 不改 session / early-data / OCSP 线
- 不做 Windows 本地手工运行

## Architecture Truth

- `docs/BACKEND_CAPABILITY_MATRIX.md`
  当前已经把
  `WinSSL TLS 1.3`
  记为：
  - 条件 capability truth
  - 例子直接写
    `Windows 10 1903+`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  当前 dedicated page
  也写：
  - `TLS 1.3`
    对
    `Windows 10`
    的门槛是
    `1903+`
- `src/fafafa.ssl.winssl.lib.pas`
  当前却分叉成两套门槛：
  - `Result.SupportsTLS13 := ... Build >= 18362`
  - `IsProtocolSupported(sslProtocolTLS13) := ... Build >= 20348`
- `tests/winssl/test_winssl_unit_comprehensive.pas`
  还保留：
  - `Windows 11`
    才支持
    `TLS 1.3`
    的旧叙事

## Files

- `src/fafafa.ssl.winssl.lib.pas`
- `tests/winssl/test_winssl_unit_comprehensive.pas`
- `tests/scripts/test_winssl_tls13_capability_consistency_contract.sh`
- `docs/plans/2026-05-20-winssl-tls13-capability-consistency-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认当前 source / test truth 先 RED
3. 最小修正：
   - `IsProtocolSupported(sslProtocolTLS13)`
   - WinSSL unit test 的 TLS 1.3 平台说明
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_winssl_tls13_capability_consistency_contract.sh
bash tests/scripts/test_winssl_tls13_capability_consistency_contract.sh
git diff --check
```

## Expected Outcome

- `WinSSL` source 不再同时发布两套不同的 TLS 1.3 门槛
- `SupportsTLS13`
  与
  `IsProtocolSupported(sslProtocolTLS13)`
  回到同一版本条件
- WinSSL unit test
  不再继续暗示
  “只有 Windows 11 才支持 TLS 1.3”
