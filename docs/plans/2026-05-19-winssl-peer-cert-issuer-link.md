# WinSSL Peer Certificate Issuer Link

## Goal

补齐 `WinSSL` 连接态 public peer-certificate surface 的 `issuer-link truth`，确保：

- `ISSLConnection.GetPeerCertificate().GetIssuerCertificate()` 不再丢失真实 issuer
- `ISSLConnection.GetPeerCertificateChain()` 返回的链条条目会接上相邻 issuer link
- 这条 truth 进入可重复的 WinSSL runtime 覆盖，避免后续再反复从静态审查重新拉起

## Scope

- `src/fafafa.ssl.winssl.connection.pas`
- `tests/winssl/test_winssl_peer_certificate_surface.pas`
- `tests/winssl/test_winssl_peer_certificate_surface.lpi`
- `tests/run_winssl_tests.ps1`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Steps

1. 新增 focused WinSSL runtime surface test
2. 在本机走 `Win64 cross-target + wine` 先观察 RED
3. 对 `WinSSL` 连接层做最小 issuer-link 修复
4. 重新跑 focused test 到 GREEN
5. 跑相关回归并同步记录

## Commands

```bash
lazbuild --os=win64 --cpu=x86_64 tests/winssl/test_winssl_peer_certificate_surface.lpi
FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_WINSSL_PEER_CERT_HOST=api.github.com \
  wine tests/winssl/bin/test_winssl_peer_certificate_surface.exe

fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units \
  -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract

git diff --check
```

## Expected Closeout

- focused WinSSL runtime surface test 先 RED 后 GREEN
- backend contract 继续 green
- 计划/发现/进度文件记录本批结论
