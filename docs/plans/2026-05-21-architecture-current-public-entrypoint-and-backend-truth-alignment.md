# 2026-05-21 ARCHITECTURE 当前 public entrypoint 与 backend 真相对齐

## Goal

修复 `docs/ARCHITECTURE.md` 中仍会误导接口设计、
backend 实现路线、
以及调用方入口选择的高价值漂移，
让这份架构文档回到当前 `v1.5.0` public truth。

## Why Now

当前这页同时残留了几类关键 drift：

1. 入口叙事仍在教学
   `Factory.CreateContext(...)`
   与
   `CreateLibrary(...)`
2. 工厂段仍把
   `CreateLibrary`
   当作公开入口，
   但当前 active truth
   应是：
   - 普通新代码：
     `fafafa.ssl`
     +
     `TSSLContextBuilder`
     /
     `TSSLConnector`
   - fixed-backend / advanced：
     `TSSLFactory.GetLibraryInstance(...)`
     +
     `Lib.CreateContext(...)`
3. backend 优先级仍停在旧时代：
   - `OpenSSL=10`
   - `MbedTLS=7`
   - `WolfSSL=5`
   - `WinSSL=10`
4. 文件组织与 backend 状态也漂移：
   - 仍写不存在的
     `fafafa.ssl.openssl.lib.pas`
   - 漏掉
     `fafafa.ssl.freepascal.*`
   - “未来架构演进” 仍把纯
     `FreePascal`
     backend 写成未来态

## Scope

- Add:
  - `docs/plans/2026-05-21-architecture-current-public-entrypoint-and-backend-truth-alignment.md`
  - `tests/scripts/test_architecture_current_public_entrypoint_truth_contract.sh`
- Update:
  - `docs/ARCHITECTURE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把入口叙事统一切回当前公开推荐面：
   - `fafafa.ssl`
   - `TSSLContextBuilder`
   - `TSSLConnector`
   - `TSSLFactory.GetLibraryInstance(...)`
   - `TSSLFactory.CreateContext(...)`
     仅保留为 core/factory surface
2. 把工厂段签名与说明切回当前 source truth：
   - 去掉
     `CreateLibrary`
   - 补回
     `GetLibraryInstance`
     /
     `GetAvailableLibraries`
     /
     `IsLibraryAvailable`
3. 把 backend / priority / file-layout 段切回当前事实：
   - `WinSSL=200`
   - `MbedTLS=175`
   - `WolfSSL=150`
   - `OpenSSL=100`
   - `FreePascal=50`
   - `fafafa.ssl.openssl.backed.pas`
   - `fafafa.ssl.freepascal.*.pas`
   - `fafafa.ssl.native_handle`
4. 把“未来架构演进”里
   已 shipped 的
   `FreePascal`
   backend
   改成当前能力/完整度继续推进，
   而不是“未来才有”

## Verification

```bash
bash -n tests/scripts/test_architecture_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_architecture_current_public_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- 架构文档不再继续教授旧 public entrypoint
- backend priority / file layout / shipped status
  与当前源码和活跃文档一致
- 接口设计、后端实现与文档主线再次收敛到同一套 truth
