# 2026-05-18 Interface Design And Backend Implementation Verification

## Goal

对 `fafafa.ssl` 做一轮“公共接口设计 + 各 backend 实现真相”的综合验证，确认文档、接口、factory/builder/config、capability matrix、selector、backend source 是否仍保持同一套真相源；把发现和必要修复记录下来，作为后续继续工作的稳定入口，避免重复拉起。

## Why Now

- `v1.5.0` 发布链已经闭环，继续围绕 release / old runtime blocker 打转只会偏离产品主线。
- 现有 `INTERFACE_DESIGN_AUDIT_V1.5.0.md` 已指出多处接口设计异味，但还没有把这些异味与各 backend 的 live 实现真相做全面对照。
- 用户明确怀疑“接口设计和实现（各个后端）这个部分有问题”，这比继续做历史 closeout 更贴近当前产品推进。

## Architecture Focus

1. 核心公共接口：
   - `ISSLConnection`
   - `ISSLContext`
   - capability record / support-level fields
2. 高层入口：
   - `fafafa.ssl`
   - `TSSLFactory`
   - context builder
   - `TSSLConfig`
3. backend 真相源：
   - OpenSSL
   - WinSSL
   - FreePascal
   - MbedTLS
   - WolfSSL
4. 对齐面：
   - docs
   - source
   - serializer / diff / selector
   - tests / contracts

## Files In Scope

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.context.builder.pas`
- `src/fafafa.ssl.backend.selector.pas`
- `src/fafafa.ssl.capability.serializer.pas`
- `src/fafafa.ssl.capability.diff.pas`
- `src/fafafa.ssl.openssl.lib.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
- `tests/test_capability_matrix_v12.pas`
- `tests/contract/test_backend_contract.pas`

## Execution Steps

1. 固化本轮 goal 与记录入口。
2. 复核旧 interface audit 的六类设计问题是否仍存在于 live 源码。
3. 横向对照各 backend 的 capability truth 发布与最小 API/selector 语义。
4. 识别“设计问题已变成实现/合同漂移”的高价值问题。
5. 选择边界清晰的问题做最小修复。
6. 跑 focused contract / focused Pascal test。
7. 把结论写回 plan / findings / progress。

## Expected Outputs

- 一份新的综合审查记录，能作为下次继续工作的入口。
- 一组带证据的 findings，区分：
  - 设计债
  - 实现真 bug
  - 文档/合同漂移
- 若边界允许，则包含至少一批 focused 修复和验证证据。

## Verification Commands

优先窄验证，不重跑重型全量门禁：

```bash
git status --short --branch
rg -n "ISSLConnection|ISSLServerConnection|SetServerName|TSSLConfig|Supports[A-Z]|Support :" src docs tests
rg -n "GetCapabilities|IsCipherSupported|SessionCacheSupport|EarlyDataSupport|ZeroRTTSupport" src/fafafa.ssl.*.pas tests
```

若发生修复，再补 focused 合同或 focused 编译/运行：

```bash
bash tests/scripts/<new_contract>.sh
fpc -B -Fu./src -Fu./tests -Fu./tests/framework ...
git diff --check
```
