# WinSSL VerifyEx Closure Writeback And Store Interface Contract

## Goal

把 `WinSSL certificate.VerifyEx` 这条已经在远端闭环的 follow-up
正式写回当前工作记录，并补一条本地可跑 contract，
锁住 focused WinSSL test 对 memory-backed store 的接口持有语义。

## Scope

- Modify:
  - `docs/plans/2026-05-20-winssl-certificate-verifyex-flag-parity.md`
  - `docs/plans/2026-05-20-winssl-cert-verifyex-custom-trust-engine.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `tests/scripts/test_winssl_verifyex_store_interface_contract.sh`

## Why This Batch

- 远端 `WinSSL Runtime Gate`
  run `26159931322`
  已经对
  commit `f0be85a`
  给出完整成功证明：
  - `Run quick WinSSL smoke`
  - `Run Windows Wave B gate`
  - `Run broader WinSSL runtime suite`
  全部 `success`
- 但现有两份 WinSSL `VerifyEx` 计划文档仍停在
  `FOLLOW-UP IN PROGRESS`
  的旧状态
- 最后一层 `EAccessViolation`
  已被确认是 focused test 里的
  `TInterfacedObject`
  生命周期洞：
  - store 被类引用持有
  - 调用边界发生临时 class-to-interface 转换
  - `_Release`
    后可留下悬空类指针
- 这条根因值得被锁成一个本地 contract，
  防止同类回归再次只在 Windows runtime 才暴露

## Steps

1. 用 `gh run view 26159931322 --json ...`
   重新确认远端 closure 证据。
2. 新增 shell contract，
   锁住 `tests/winssl/test_winssl_cert_verify_ex.pas`
   必须以 `ISSLCertificateStore`
   持有 memory-backed store。
3. 更新两份 WinSSL 计划文档，
   把最终 closure 与真实根因写回。
4. 更新当前 `task_plan.md` / `findings.md` / `progress.md`。
5. 验证：
   - `bash tests/scripts/test_winssl_verifyex_store_interface_contract.sh`
   - `git diff --check`

## Expected Result

- WinSSL `VerifyEx` 不再在当前工作记录里表现为未闭环 residual
- focused WinSSL test 的 store ownership 语义被本地 contract 锁住
- 下一步可以把精力放回真正仍未解决的 backend / TLS 主线问题，
  而不是继续重开已闭环的 WinSSL `VerifyEx` 追踪
