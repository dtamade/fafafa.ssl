# Helper-Linked HTTPS Examples Public Import Truth

## Goal

收口一组通过 helper unit
串起来的 HTTPS client/server example
里仍保留的历史
`fafafa.ssl.base`
导入，
让它们回到当前真实入口：

- 普通 SSL public surface
  直接来自
  `fafafa.ssl`
- socket / network helper
  继续由
  `https_server_common`
  /
  `fafafa.examples.tcp`
  提供
- 不再因为 helper-linked 示例残留，
  继续误导调用方拆分导入

## Scope

- Update:
  - `examples/https_server/https_server_common.pas`
  - `examples/https_server/https_server_simple.pas`
  - `examples/https_server/https_server_alpn.pas`
  - `examples/https_server/https_server_mtls.pas`
  - `examples/https_client/https_client_session.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-helper-linked-https-examples-public-import-truth.md`
  - `tests/scripts/test_helper_linked_https_examples_public_import_truth_contract.sh`

不做：

- 不改 runtime 实现
- 不扩大到 `examples/production/*`
- 不重开 ALPN owner-path / session-resumption behavior 设计

## Why This Batch

当前 `fafafa.ssl` 主门面已经 re-export：

- `ISSLConnection`
- `ISSLConnectionInfo`
- `ISSLContext`
- `ISSLClientConnection`
- `ISSLSessionResumption`
- `ISSLSession`
- `TSSLFactory`
- `sslOpenSSL`
- `sslCtxServer`
- `sslCtxClient`
- `sslProtocolTLS12`
- `sslProtocolTLS13`
- `sslVerifyPeer`
- `sslVerifyFailIfNoPeerCert`

而这组 helper-linked 示例
里的 socket/network 边界
本来就由专属 helper 提供：

- `https_server_common`
  提供
  `TSocketHandle`
  /
  `INVALID_SOCKET_HANDLE`
  /
  `CreateListeningSocket`
  /
  `AcceptClient`
- `fafafa.examples.tcp`
  提供
  `TSocketHandle`
  /
  `INVALID_SOCKET`
  /
  `ConnectTCP`
  /
  `CloseSocket`

这说明当前残余：

- `https_server_common`
  为了
  `ISSLConnection`
  继续带入
  `fafafa.ssl.base`
- `https_server_simple`
  /
  `https_server_alpn`
  /
  `https_server_mtls`
  继续保留
  `fafafa.ssl.base`
- `https_client_session`
  虽然真正依赖的是
  `fafafa.examples.tcp`
  的 socket 常量，
  仍额外带着
  `fafafa.ssl.base`

都已经不再是实现能力缺口，
而是 helper-linked active examples
继续教学旧入口的 guidance drift。

## Minimal Fix

1. 为目标 helper/example
   新增 focused import contract
2. 将普通 SSL public surface
   收回到
   `fafafa.ssl`
3. 保留真正的 helper unit
   `https_server_common`
   /
   `fafafa.examples.tcp`
4. 跑 focused contract
   与最小 compile proof，
   确认当前入口真相在 helper-linked examples 上真实成立

## Verification

```bash
bash -n tests/scripts/test_helper_linked_https_examples_public_import_truth_contract.sh
bash tests/scripts/test_helper_linked_https_examples_public_import_truth_contract.sh
bash tests/scripts/test_alpn_owner_path_active_guidance_contract.sh

mkdir -p tmp/example_import_truth_https_server_simple
fpc -B -Fu./src -Fu./examples/https_server \
  -FUtmp/example_import_truth_https_server_simple \
  -FEtmp/example_import_truth_https_server_simple \
  -otmp/example_import_truth_https_server_simple/https_server_simple \
  examples/https_server/https_server_simple.pas

mkdir -p tmp/example_import_truth_https_server_alpn
fpc -B -Fu./src -Fu./examples/https_server \
  -FUtmp/example_import_truth_https_server_alpn \
  -FEtmp/example_import_truth_https_server_alpn \
  -otmp/example_import_truth_https_server_alpn/https_server_alpn \
  examples/https_server/https_server_alpn.pas

mkdir -p tmp/example_import_truth_https_server_mtls
fpc -B -Fu./src -Fu./examples/https_server \
  -FUtmp/example_import_truth_https_server_mtls \
  -FEtmp/example_import_truth_https_server_mtls \
  -otmp/example_import_truth_https_server_mtls/https_server_mtls \
  examples/https_server/https_server_mtls.pas

mkdir -p tmp/example_import_truth_https_client_session
fpc -B -Fu./src -Fu./examples -Fu./examples/https_client \
  -FUtmp/example_import_truth_https_client_session \
  -FEtmp/example_import_truth_https_client_session \
  -otmp/example_import_truth_https_client_session/https_client_session \
  examples/https_client/https_client_session.pas

git diff --check
```

## Expected Outcome

- `https_server_common`
  不再为了
  `ISSLConnection`
  带入
  `fafafa.ssl.base`
- `https_server_simple`
  /
  `https_server_alpn`
  /
  `https_server_mtls`
  不再继续教学
  split import
- `https_client_session`
  明确通过
  `fafafa.examples.tcp`
  获取 socket 常量和 helper，
  不再把调用方带回
  `fafafa.ssl.base`

## Execution Result

- PASS
- focused contract:
  - `bash -n tests/scripts/test_helper_linked_https_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_helper_linked_https_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
    - PASS
- focused compile proof:
  - `examples/https_server/https_server_simple.pas`
    - PASS
  - `examples/https_server/https_server_alpn.pas`
    - PASS
  - `examples/https_server/https_server_mtls.pas`
    - PASS
  - `examples/https_client/https_client_session.pas`
    - PASS
- hygiene:
  - `git diff --check`
    - PASS
- note:
  - 这批只收口
    helper-linked HTTPS
    活跃示例的
    public import truth，
    未改动 runtime 行为
  - compile 输出
    仍有仓库既有
    warning/note，
    但没有新的失败信号
