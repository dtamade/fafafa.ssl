# FAQ And Common Pitfalls Entrypoint And Backend Truth Alignment

## Goal

修复
`docs/guides/FAQ.md`
与
`docs/guides/COMMON_PITFALLS.md`
里的活跃入口漂移，
让这两份最常被先打开的指南重新对齐当前 shipped truth：

- OpenSSL 不是所有平台/所有 backend 的唯一前提
- 普通新代码优先走
  `fafafa.ssl`
  /
  `TSSLContextBuilder`
  /
  `TSSLConnector`
- 自定义动态库路径属于
  OpenSSL-specific
  fallback，
  不再继续教学
  `TSSLLibrary.Instance.*`
  旧单例入口

这批不改 runtime，
只做：

- active docs truth repair
- 一个静态 contract，
  防止旧入口再次回流
- 账本同步

## Why This Batch

继续沿
“接口设计 / backend completeness / 活跃文档真相”
主线扫描时，
当前最明显的新 residual
已经收窄到两份高入口指南：

- `docs/guides/FAQ.md`
  - 仍写
    `唯一要求：系统安装OpenSSL 1.1.1+或3.x。`
  - 仍教学
    `TSSLLibrary.Instance.Initialize;`
  - 仍教学
    `TSSLLibrary.Instance.SetCustomLibraryPath(...)`
- `docs/guides/COMMON_PITFALLS.md`
  - 仍把
    `TSSLLibrary.Instance.SetCustomLibraryPath(...)`
    当成 macOS brew OpenSSL 的当前建议

这类 drift
不会只影响阅读体验，
而是会直接把：

- 新用户接入路径
- 后端依赖判断
- 后续 focused 审查入口

继续带回旧心智模型。

## Scope

- Add:
  - `docs/plans/2026-05-21-faq-and-common-pitfalls-entrypoint-and-backend-truth-alignment.md`
  - `tests/scripts/test_faq_and_common_pitfalls_entrypoint_truth_contract.sh`
- Update:
  - `docs/guides/FAQ.md`
  - `docs/guides/COMMON_PITFALLS.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把
   `FAQ`
   顶部平台/依赖说明
   改回 backend-specific truth：
   - Windows 可直接使用 WinSSL
   - FreePascal backend 不要求系统 OpenSSL
   - OpenSSL 依赖只属于相应 backend
2. 把
   `FAQ`
   里的旧单例 loader 示例
   改为：
   - 统一入口：
     `fafafa.ssl`
     +
     `TSSLContextBuilder`
     /
     `TSSLConnector`
   - OpenSSL-specific fallback：
     `fafafa.ssl.openssl.backed.SetCustomLibraryPaths(...)`
3. 把
   `COMMON_PITFALLS`
   的 macOS brew OpenSSL fallback
   改成当前真实的
   `SetCustomLibraryPaths(libcrypto, libssl)`
   形式，
   并明确它不是通用初始化步骤
4. 顺手修掉
   `FAQ`
   里的活跃入口噪音：
   - placeholder GitHub URL
   - 错误/过期文档链接
   - 版本号漂移

## Verification

```bash
bash -n tests/scripts/test_faq_and_common_pitfalls_entrypoint_truth_contract.sh
bash tests/scripts/test_faq_and_common_pitfalls_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- `FAQ`
  与
  `COMMON_PITFALLS`
  不再把 OpenSSL 发布成全局唯一前提
- 活跃指南不再教学
  `TSSLLibrary.Instance.*`
  旧入口
- OpenSSL-specific fallback
  被明确压回 backend-specific 边界
- 这两份高入口指南重新成为可信的当前 truth source
