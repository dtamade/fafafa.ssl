# 2026-05-19 Public Unit Import Guidance Truth

## Goal

继续沿着 interface/backend completeness 主线推进，收口高入口文档里仍在教授旧导入路径、旧 facade 单元名、旧创建入口和旧枚举名的问题：

- `docs/guides/USER_GUIDE.md`
- `docs/guides/WINSSL_QUICKSTART.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `docs/guides/TROUBLESHOOTING.md`
- `docs/reference/API_REFERENCE.md`

当前这些文档仍混用：

- 已删除的 `fafafa.ssl.abstract.intf` / `fafafa.ssl.abstract.types`
- 不存在的 `fafafa.ssl.openssl` facade unit
- 不存在的 `CreateSSLLibrary(...)`
- 不存在的 `GetLibraryName`
- 旧枚举名：
  - `sslLibraryWinSSL`
  - `sslLibraryOpenSSL`
  - `sslLibraryAutoDetect`
- 旧/不推荐的高入口创建心智：
  - 让新代码从 backend-specific `CreateOpenSSLLibrary` 开始
  - 让普通调用方手动 `LoadOpenSSL`

## Scope

- 只处理当前高入口 public import / creation guidance truth
- 用 focused shell contract 锁住：
  - 当前公开单元导入路径
  - 当前高入口库/上下文创建路径
  - 当前推荐枚举名
  - `LoadOpenSSL` / backend-specific creator 的边界说明
- 不修改 runtime 实现
- 不扩到 PKCS12/security-best-practices 等低层 helper 讨论页

## Files

- `docs/guides/USER_GUIDE.md`
- `docs/guides/WINSSL_QUICKSTART.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `docs/guides/TROUBLESHOOTING.md`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
- `docs/plans/2026-05-19-public-unit-import-guidance-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前高入口 public facade 是：
  - `fafafa.ssl`
  - `fafafa.ssl.context.builder`
  - `fafafa.ssl.tls`
- 当前常见 public 创建入口是：
  - `TSSLFactory.GetLibraryInstance(...)`
  - `TSSLFactory.CreateContext(...)`
  - `TSSLContextBuilder.Create...`
  - `TSSLConnector.FromContext(...)`
- backend-specific `CreateOpenSSLLibrary` / `CreateWinSSLLibrary`
  仍然存在于 backend units，但不应被高入口普通示例教成首选路径
- `CreateSSLLibrary(...)` 不是当前 shipped source public function
- 当前推荐枚举名是：
  - `sslOpenSSL`
  - `sslWinSSL`
  - `sslAutoDetect`
  - `sslCtxClient`
  - `sslCtxServer`
- 当前高入口普通文档不应再要求调用方手动 `LoadOpenSSL`
  - 普通路径优先通过 `TSSLFactory.IsLibraryAvailable(...)` / `GetLibraryInstance(...)`
  - 低层 OpenSSL API loader 只应放在 backend-specific/low-level 语境

## Steps

1. 补 focused shell contract，让旧导入/旧创建入口先 RED。
2. 把六份高入口 docs 改回当前 public facade / enum / creation truth。
3. 同步台账，避免后续继续把这些旧导入路径当成 current source truth。
4. 跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
git diff --check
```

## Expected Result

- 高入口 docs 不再继续教授 `abstract.intf` / `fafafa.ssl.openssl` / `CreateSSLLibrary(...)`
- WinSSL / MbedTLS / generic guide 的示例重新回到当前公开门面和枚举名
- `API_REFERENCE` / `TROUBLESHOOTING` 对 backend-specific creator / loader 的边界重新说清楚

## Result

- 已完成。
- 六份高入口文档现已统一回到：
  - `fafafa.ssl`
  - `TSSLFactory.GetLibraryInstance(...)`
  - `TSSLFactory.IsLibraryAvailable(...)`
  - `sslCtxClient`
  - `LibraryTypeToString(Lib.GetLibraryType)`
- `API_REFERENCE` 现已把 `TSSLFactory.GetLibraryInstance(...)` 标成当前 public library-entrypoint，并把 `CreateOpenSSLLibrary` / `CreateWinSSLLibrary` 归类为 backend-specific low-level creators。
- `TROUBLESHOOTING` 现已去掉手动底层 OpenSSL loader 的普通应用入口指导。
- 额外同文件收口：
  - `USER_GUIDE` 的 SAN 类型改回 `TSSLStringArray`
  - `USER_GUIDE` / `TROUBLESHOOTING` 的 WinSSL enterprise class helper 名称改回源码真相
  - 新 contract 的 multiline `rg` 噪音已修掉

## Verification

```bash
bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
git diff --check
```

- 结果：全部通过
