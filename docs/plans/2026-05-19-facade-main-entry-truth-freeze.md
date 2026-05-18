# Facade Main-Entry Truth Freeze

## Goal

把当前最高可见的一批门面/主入口 truth source 收到同一套 public 真相：

- `docs/README.md` 的快速开始不再使用旧的 `sslClient` / core-only 路径
- `src/fafafa.ssl.pas` 的头部示例改成当前推荐入口
- `src/fafafa.ssl.factory.pas` 的头部示例和参数说明改成当前 `sslCtxClient` / `sslCtxServer` 真相
- `docs/guides/INTEGRATION_GUIDE.md` 不再继续教学旧的 `sslClient` 枚举名

## Scope

- `docs/README.md`
- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.factory.pas`
- `docs/guides/INTEGRATION_GUIDE.md`
- `tests/scripts/test_facade_main_entry_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不删除 `TSSLHelper` / `QuickServer` / `CreateOCSPClient` / `CreateCRLManager` export
- 不改生产实现逻辑
- 不重开 broader facade slimming / compatibility removal 设计

## Why This Batch

这批不是“文档 polish”，而是当前最容易让新读者直接走错入口的一层 public drift：

- `docs/guides/GETTING_STARTED.md` 已明确 `TSSLConnector` 是当前推荐入口
- 但 `docs/README.md` 仍用旧的 `sslClient` + core-only 路径
- `src/fafafa.ssl.factory.pas` 的头部示例和参数注释仍写 `sslClient` / `sslServer`
- `docs/guides/INTEGRATION_GUIDE.md` 也还在教旧枚举名

所以这批最小正确动作，是先把这些高可见 truth source 全部对齐到：

- `sslCtxClient` / `sslCtxServer`
- `TSSLConnector` / per-connection SNI
- `uses fafafa.ssl` 的门面入口

## Planned Changes

1. 新增 focused shell contract，锁住 facade/main-entry truth。
2. 更新 `docs/README.md`：
   - 快速开始改成 facade + connector 推荐入口
   - 去掉旧的 `sslClient` 用法
3. 更新 `src/fafafa.ssl.pas` 头部示例：
   - 使用当前门面 + builder/connector 入口
4. 更新 `src/fafafa.ssl.factory.pas` 注释示例与参数说明：
   - 统一改成 `sslCtxClient` / `sslCtxServer`
5. 更新 `docs/guides/INTEGRATION_GUIDE.md`：
   - 把旧的 `sslClient` 枚举名改成 `sslCtxClient`

## Verification

```bash
bash -n tests/scripts/test_facade_main_entry_truth_contract.sh
bash tests/scripts/test_facade_main_entry_truth_contract.sh
git diff --check
```

## Expected Outcome

- highest-visibility facade entry docs and source comments now use current enum truth
- `docs/README.md` no longer lags behind `GETTING_STARTED.md` on the recommended connector path
- future drift back to `sslClient` / stale main-entry examples will trip a focused contract
