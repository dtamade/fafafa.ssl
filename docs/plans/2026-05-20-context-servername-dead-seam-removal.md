# Context ServerName Dead Seam Removal

## Goal

把 `context-level ServerName`
迁移主线里
最后一层已经失效的
shared compatibility seam
彻底移除：

- 删除
  `src/fafafa.ssl.context.compat.pas`
- 删除
  OpenSSL / WolfSSL / MbedTLS / WinSSL
  connection constructor
  里对该 helper 的 dead fallback read
- 把 source / roadmap / focused contract
  一起切到
  “不再保留 no-op seam”
  的新真相

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 删除：
  - `src/fafafa.ssl.context.compat.pas`

## TDD Steps

1. 把
   `tests/scripts/test_context_server_name_compat_shim_contract.sh`
   翻成新 truth：
   - helper file 必须不存在
   - 所有 backend
     都不得再调用
     `GetContextLevelServerNameCompatibilityValue(...)`
   - 所有 backend
     都不得 direct read
     `(AContext|FContext).GetServerName`
2. 删除 helper file，
   并从四个 backend constructor
   移除 dead fallback read
3. Focused verification：
   - `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh`
   - `bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
   - `mkdir -p tmp/test_cross_backend_client_context_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_client_context_server_name_clarification -FEtmp/test_cross_backend_client_context_server_name_clarification -otmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification tests/test_cross_backend_client_context_server_name_clarification.pas && ./tmp/test_cross_backend_client_context_server_name_clarification/test_cross_backend_client_context_server_name_clarification`
   - `git diff --check`

## Closeout

- 删除后的 focused verification 已全部通过：
  - `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - `bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `git diff --check`
- 当前最终源码真相：
  - `src/fafafa.ssl.context.compat.pas` 已不存在
  - OpenSSL / WolfSSL / MbedTLS / WinSSL constructor 已不再调用 shared helper
  - 所有 backend constructor 都不再 direct read deprecated context-level `ServerName`
- 这批不改变当前 runtime compatibility 边界，只是把“已经切断行为、却还残留在源码里的过渡 seam”彻底收干净
