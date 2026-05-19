# ISSLOCSPStapling Compiler Deprecation Alignment

## Goal

把 `ISSLConnection` 上这组 OCSP compatibility-core mirrors

- `GetOCSPStaplingEnabled`
- `GetOCSPResponse`
- `IsOCSPResponseVerified`
- `GetOCSPResponseStatus`

收口到与当前 owner-path truth 一致的状态：

- 继续 shipped
- 继续兼容保留
- 但源码声明进入编译期 `deprecated`
- 新代码明确优先走 `ISSLOCSPStapling`

## Architecture

这批不改 runtime 行为，不动 backend OCSP 实现，只做 public-surface truth 对齐：

1. 新增 focused shell contract
   - 冻结四个 core OCSP 方法的 compiler-deprecated 声明
   - 冻结 API / v2 设计文档中的 compiler-deprecated 表述
   - 冻结 intentional residual tests 的 deprecation-warning quarantine
2. 在 `src/fafafa.ssl.base.pas` 上把四个 core OCSP mirrors 标为
   `deprecated 'Use ISSLOCSPStapling....'`
3. 更新活跃文档
   - `docs/reference/API_REFERENCE.md`
   - `docs/reference/INTERFACE_DESIGN_V2.md`
4. 对四个 intentional residual tests 加 warning quarantine
   - 不移除 direct-core residual proof
   - 只显式标成 intentional deprecated-compat usage

## Files

- Add: `docs/plans/2026-05-20-isslocspstapling-compiler-deprecation.md`
- Add: `tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh`
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Modify: `docs/reference/INTERFACE_DESIGN_V2.md`
- Modify: `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`
- Modify: `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
- Modify: `tests/test_wolfssl_ocsp_stapling_contract.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 OCSP 这组 surface 已经完成了大部分 truth 收口：

- active docs 已转向 `ISSLOCSPStapling`
- source comments 已把 core `GetOCSP*` 标成 compatibility-core mirrors
- residual direct-core test set 已缩到 4 个 intentional backend/runtime proofs

但源码声明本身还没进入编译期 `deprecated`。
这会让：

- docs 说“仅兼容保留”
- source comments 说“owner 是 ISSLOCSPStapling”
- 但 public declaration 仍像普通 core surface

因此这批的价值是把 source declaration 也真正拉到同一条线上。

## Verification

```bash
bash -n tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh
bash tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh
bash tests/scripts/test_isslocspstapling_residual_classification_contract.sh
bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh

mkdir -p tmp/test_isslocspstapling_units && \
fpc -B -Fu./src -Fu./tests -FUtmp/test_isslocspstapling_units \
  -FEtmp/test_isslocspstapling_units \
  -otmp/test_isslocspstapling_units/test_mbedtls_ocsp_capability \
  tests/mbedtls/test_mbedtls_ocsp_capability.pas

mkdir -p tmp/test_isslocspstapling_units && \
fpc -B -Fu./src -Fu./tests -FUtmp/test_isslocspstapling_units \
  -FEtmp/test_isslocspstapling_units \
  -otmp/test_isslocspstapling_units/test_wolfssl_ocsp_stapling_contract \
  tests/test_wolfssl_ocsp_stapling_contract.pas

git diff --check
```

## Expected Outcome

- core `GetOCSP*` shipped surface 继续保留，但会明确成为 compiler-deprecated compatibility mirrors
- active docs / v2 design / source declaration 三者对齐
- intentional residual tests 继续保留 direct-core proof，同时显式隔离 deprecation warning
