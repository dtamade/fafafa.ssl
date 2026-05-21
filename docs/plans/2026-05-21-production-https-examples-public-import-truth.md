# Production HTTPS Examples Public Import Truth

## Goal

收口
`examples/production/*`
这组仍然活跃可见的
production HTTPS client/server
示例里残留的
`fafafa.ssl.base`
导入，
让它们回到当前真实入口：

- 普通 TLS public surface
  直接来自
  `fafafa.ssl`
- socket / network helper
  继续由
  `fafafa.examples.tcp`
  或本地 socket 实现
  提供
- 不再因为 production
  级示例残留，
  继续误导调用方拆分导入

## Scope

- Add:
  - `docs/plans/2026-05-21-production-https-examples-public-import-truth.md`
  - `tests/scripts/test_production_https_examples_public_import_truth_contract.sh`
- Update:
  - `examples/production/https_client_auth.pas`
  - `examples/production/https_client_simple.pas`
  - `examples/production/https_client_post.pas`
  - `examples/production/https_server_simple.pas`
  - `examples/production/https_client_session.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不改 runtime 实现
- 不扩大到 `.lpi` project metadata
- 不重开 production example
  的 verify-policy /
  session semantics /
  socket architecture 设计

## Why This Batch

当前
`fafafa.ssl`
主门面已经 re-export：

- `ISSLContext`
- `ISSLConnection`
- `ISSLClientConnection`
- `ISSLCertificateStore`
- `ISSLSessionResumption`
- `TSSLFactory`
- `sslCtxClient`
- `sslCtxServer`
- `sslOpenSSL`
- `sslVerifyPeer`
- `sslVerifyNone`

而这组 production
示例里的 socket / network
边界本来就已经有
自己的 owner：

- `fafafa.examples.tcp`
  提供
  `TSocketHandle`
  /
  `INVALID_SOCKET`
  /
  `ConnectTCP`
  /
  `CloseSocket`
  /
  `InitNetwork`
- `https_server_simple`
  自身通过
  `Sockets`
  提供本地 server-side
  socket 语义

这说明当前残余
不是实现能力缺口，
而是 production
示例还在继续保留
历史
`fafafa.ssl.base`
导入。

## Minimal Fix

1. 新增 focused contract，
   冻结 production HTTPS
   示例的 import truth
2. 把普通 TLS public surface
   收回到
   `fafafa.ssl`
3. 保留真正的
   helper / socket owner
4. 跑 focused contract
   与最小 compile proof，
   确认修复只影响 guidance，
   不影响 production 示例
   的能力边界

## Verification

```bash
bash -n tests/scripts/test_production_https_examples_public_import_truth_contract.sh
bash tests/scripts/test_production_https_examples_public_import_truth_contract.sh

mkdir -p tmp/example_import_truth_production_https_client_auth
fpc -B -Fu./src -Fu./examples -Fu./examples/production \
  -FUtmp/example_import_truth_production_https_client_auth \
  -FEtmp/example_import_truth_production_https_client_auth \
  -otmp/example_import_truth_production_https_client_auth/https_client_auth \
  examples/production/https_client_auth.pas

mkdir -p tmp/example_import_truth_production_https_client_simple
fpc -B -Fu./src -Fu./examples -Fu./examples/production \
  -FUtmp/example_import_truth_production_https_client_simple \
  -FEtmp/example_import_truth_production_https_client_simple \
  -otmp/example_import_truth_production_https_client_simple/https_client_simple \
  examples/production/https_client_simple.pas

mkdir -p tmp/example_import_truth_production_https_client_post
fpc -B -Fu./src -Fu./examples -Fu./examples/production \
  -FUtmp/example_import_truth_production_https_client_post \
  -FEtmp/example_import_truth_production_https_client_post \
  -otmp/example_import_truth_production_https_client_post/https_client_post \
  examples/production/https_client_post.pas

mkdir -p tmp/example_import_truth_production_https_server_simple
fpc -B -Fu./src -Fu./examples/production \
  -FUtmp/example_import_truth_production_https_server_simple \
  -FEtmp/example_import_truth_production_https_server_simple \
  -otmp/example_import_truth_production_https_server_simple/https_server_simple \
  examples/production/https_server_simple.pas

mkdir -p tmp/example_import_truth_production_https_client_session
fpc -B -Fu./src -Fu./examples -Fu./examples/production \
  -FUtmp/example_import_truth_production_https_client_session \
  -FEtmp/example_import_truth_production_https_client_session \
  -otmp/example_import_truth_production_https_client_session/https_client_session \
  examples/production/https_client_session.pas

git diff --check
```

## Expected Outcome

- `https_client_auth`
  /
  `https_client_simple`
  /
  `https_client_post`
  /
  `https_client_session`
  明确通过
  `fafafa.examples.tcp`
  获取 socket helper，
  不再额外带入
  `fafafa.ssl.base`
- `https_server_simple`
  通过
  `Sockets`
  与
  `fafafa.ssl`
  表达当前边界，
  不再继续教学
  `fafafa.ssl.base`
- production examples
  source-level
  public import truth
  与前面 active/helper
  批次保持一致

## Execution Result

- PASS
- focused contract history:
  - `bash -n tests/scripts/test_production_https_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_production_https_examples_public_import_truth_contract.sh`
    - RED -> PASS
    - initial RED:
      - `https_server_simple`
        still retained
        the historical
        `fafafa.ssl.base`
        import
- focused compile proof:
  - `examples/production/https_client_auth.pas`
    - PASS
  - `examples/production/https_client_simple.pas`
    - PASS
  - `examples/production/https_client_post.pas`
    - PASS
  - `examples/production/https_server_simple.pas`
    - PASS
  - `examples/production/https_client_session.pas`
    - PASS
- `git diff --check`
  - PASS
- note:
  - compile logs still contain
    repo pre-existing
    warnings / notes,
    but this batch
    introduced no new
    compile failure
