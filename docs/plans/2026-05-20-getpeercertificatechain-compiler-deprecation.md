# `GetPeerCertificateChain` Compiler Deprecation Alignment

## Goal

把 `ISSLConnection.GetPeerCertificateChain` 从“当前仍像普通 core surface”继续收成真正的 compatibility-only owner mirror：

- shipped surface 继续保留
- ordinary docs / examples 优先改走 `ISSLCertificateVerification`
- 源码声明进入编译期 `deprecated`
- residual direct-core proof 只保留在明确的 backend/runtime/contract 文件里

## Scope

本批只处理 public-surface truth 对齐，不改 backend 取链逻辑：

- `src/fafafa.ssl.base.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `docs/guides/TROUBLESHOOTING.md`
- `tests/examples/test_certchain.pas`
- `tests/contract/test_backend_contract.pas`
- `tests/test_openssl_connection_peer_certificate_surface.pas`
- `tests/test_mbedtls_connection_peer_certificate_contract.pas`
- `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
- `tests/test_openssl_connection_peer_certificate_chain_contract.pas`
- `tests/test_freepascal_client_peer_certificate_surface.pas`
- `tests/winssl/test_winssl_connection_info.pas`
- `tests/winssl/test_winssl_peer_certificate_surface.pas`
- `tests/scripts/test_getpeercertificatechain_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 `ISSLCertificateVerification` 的 runtime owner 实现
- 不重开 `GetVerifyResult*` 已闭环路线
- 不把 `GetPeerCertificateChain` 从 public API 里删除

## Why This Batch

当前 repo 的真相已经出现了典型“半收口”：

- `ISSLCertificateVerification` 已经暴露 `GetPeerCertificateChain`
- `TBaseSSLConnection` 的残余说明也已经把普通 docs/tests 判断为 owner-path
- `GetVerifyResult*` 这组相邻 surface 已经进入 compiler `deprecated`

但 `ISSLConnection.GetPeerCertificateChain` 自己仍未进入 compiler-deprecated，也还有普通文档/示例在继续直接教学 `LConn.GetPeerCertificateChain`。

因此这批要解决的是：

1. ordinary guidance 先切 owner path
2. core declaration 再进入 compiler-deprecated
3. residual direct-core proof 显式 quarantine

## Planned Changes

1. 新增 focused shell contract，先锁住：
   - core declaration 的 compiler-deprecated 形态
   - API / V2 文档的 compiler-deprecated 叙述
   - ordinary doc/example 已切到 `ISSLCertificateVerification`
   - direct-core residual file set 与 warning quarantine
2. 在 `src/fafafa.ssl.base.pas` 中给 `GetPeerCertificateChain` 补：
   - `@preferred-access`
   - `@owner-note`
   - `@compatibility-note`
   - `@deprecated`
   - `deprecated 'Use ISSLCertificateVerification.GetPeerCertificateChain';`
3. 更新 active docs：
   - `docs/reference/API_REFERENCE.md`
   - `docs/reference/INTERFACE_DESIGN_V2.md`
   - `docs/guides/TROUBLESHOOTING.md`
4. 更新普通 example：
   - `tests/examples/test_certchain.pas`
5. 对 intentional residual tests 加 local/file-scoped warning quarantine，
   保留 direct-core proof，但明确它们是 deprecated compatibility usage

## Verification

```bash
bash -n tests/scripts/test_getpeercertificatechain_compiler_deprecated_contract.sh
bash tests/scripts/test_getpeercertificatechain_compiler_deprecated_contract.sh

mkdir -p tmp/test_peer_chain_backend_contract_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_peer_chain_backend_contract_units \
  -FEtmp/test_peer_chain_backend_contract_units \
  -otmp/test_peer_chain_backend_contract_units/test_backend_contract \
  tests/contract/test_backend_contract.pas

mkdir -p tmp/test_peer_chain_openssl_surface_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_peer_chain_openssl_surface_units \
  -FEtmp/test_peer_chain_openssl_surface_units \
  -otmp/test_peer_chain_openssl_surface_units/test_openssl_connection_peer_certificate_surface \
  tests/test_openssl_connection_peer_certificate_surface.pas

mkdir -p tmp/test_peer_chain_mbedtls_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_peer_chain_mbedtls_units \
  -FEtmp/test_peer_chain_mbedtls_units \
  -otmp/test_peer_chain_mbedtls_units/test_mbedtls_connection_peer_certificate_contract \
  tests/test_mbedtls_connection_peer_certificate_contract.pas

git diff --check
```

## Expected Outcome

- `GetPeerCertificateChain` 在 source/doc/compiler 三层都被明确为
  `ISSLCertificateVerification` owner-path 的 compatibility mirror
- active guidance 不再把 direct-core getter 当普通入口
- direct-core residual 只剩显式、可解释、带 quarantine 的 backend/runtime proof
