# Task Plan - WolfSSL Client Peer Certificate Chain Surface

## Goal
让 WolfSSL client connection 在成功的 full handshake 后真正暴露 peer leaf certificate 与 peer certificate chain，收掉当前 `GetPeerCertificateChain` 永远返回空数组的核心 verification surface 缺口。

## Current Batch
1. 探索性 RED 已完成：
   - 先尝试复用 scripted TLS 1.3 server harness 做真实 full-handshake runtime proof
   - 当前主机 `wolfSSL 5.7.2` 上该路径只得到 `Connect=False / verify=OK` 这类不可信号，不能作为收口证据
2. 最终 GREEN 已改为 deterministic contract：
   - 修正 `wolfSSL_get_error()` 常量符号方向
   - 修正 `wolfSSL_X509_d2i` loader 绑定，避免 DER 导入 AV
   - 增加 peer-chain API bindings，并让 `TWolfSSLConnection.DoGetPeerCertificateChain` 用 DER bytes materialize 证书链
   - 顺手把 `Renegotiate` 的静默失败收成显式 unsupported 语义，避免 framework test 再漂
3. 验证与收口：
   - focused peer-chain contract
   - `tests/test_wolfssl_framework.pas`
   - `python3 scripts/compile_all_modules.py`
   - `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Status
- [completed] 计划与 RED 探索
- [completed] WolfSSL peer-chain 与 DER 导入修复
- [completed] Verification
- [completed] Review and commit ready

## Risks
- 当前主机上的 scripted full-handshake runtime proof 仍不可靠，因此本批真相源是 deterministic native-surface contract，不把 `wolfSSL 5.7.2` 上的模糊 runtime 信号包装成“已证实”。
- peer-chain native pointers 的 ownership 仍需保持只读复制；当前实现按 DER bytes materialize，并在 helper 缺失或 entry 损坏时 fail-closed 为 `[]`。
- 这批只修 peer-certificate verification surface 和相邻的 renegotiate 诊断语义，不扩大到 trust policy、OCSP、CT 或其它握手主线。

## Follow-up Queue
1. 如果后续继续补 WolfSSL verification/runtime surface，优先挑不受当前 host `5.7.2` 模糊行为干扰、能做 deterministic contract 或稳定 runtime proof 的缺口。
2. 真实 full-handshake peer-certificate runtime proof 仍值得后续在更稳定的 WolfSSL host/runtime 上补回，但不阻塞当前 peer-chain surface 收口。
