# 2026-05-21 EARLY_DATA_GUIDE 当前 public import 真相对齐

## Goal

修复 `docs/guides/EARLY_DATA_GUIDE.md`
里两个 active early-data
示例仍在使用
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
split import
的问题，
让这份 guide
继续保留
owner-surface
理由与
early-data
边界判断，
但不再偏离
当前 façade 已公开的 import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-early-data-guide-current-public-import-truth.md`
- Update:
  - `docs/guides/EARLY_DATA_GUIDE.md`
  - `tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 early-data runtime 实现
- 不重开 replay-store / anti-replay / capability 功能线
- 不把 early-data 示例改写成 builder 风格

## Architecture Truth

- `EARLY_DATA_GUIDE`
  当前仍应保留：
  - 为什么
    early-data owner surface
    直接挂在
    context / connection
  - 为什么
    普通客户端
    若不需要
    early-data owner surface
    仍可停留在
    `TSSLConnector` / `TSSLStream`
- 这不等于
  active 示例
  还要继续 split：
  - `fafafa.ssl.base`
  - `fafafa.ssl.factory`
- 当前 façade 已公开：
  - `ISSLLibrary`
  - `ISSLContext`
  - `ISSLConnection`
  - `ISSLEarlyDataContext`
  - `ISSLEarlyDataConnection`
  - `TSSLFactory`
  都可直接来自：
  - `fafafa.ssl`
- 当前 high-entry
  public library-entrypoint
  仍是：
  - `TSSLFactory.GetLibraryInstance(...)`

## Steps

1. 收紧现有
   `tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`：
   - 继续冻结
     owner-surface
     理由
   - 新增冻结：
     - 两段 active 示例
       都必须使用：
       - `uses fafafa.ssl;`
     - 不得继续出现：
       - `fafafa.ssl.base`
       - `fafafa.ssl.factory`
     - 示例继续保持：
       - `TSSLFactory.GetLibraryInstance(...)`
2. 跑 contract，拿到 RED。
3. 最小修改 `EARLY_DATA_GUIDE.md` 的两段导入示例。
4. 重跑 contract 与 diff hygiene。

## Verification

```bash
bash -n tests/scripts/test_early_data_owner_surface_reasoning_contract.sh
bash tests/scripts/test_early_data_owner_surface_reasoning_contract.sh
git diff --check
```

## Expected Result

- `EARLY_DATA_GUIDE`
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
  这组旧 split import
- owner-surface
  解释
  与
  `CreateConnection(...)`
  直连理由
  继续保留
- high-entry
  early-data 示例
  统一回到
  `uses fafafa.ssl;`

## Execution Result

- PASS
- focused contract
  先补齐了：
  - `EARLY_DATA_GUIDE_DOC`
    覆盖入口，
    允许同一条
    focused contract
    对
    `HEAD`
    旧版 guide
    做 RED
  - facade-import
    计数
    改成
    `wc -l`
    路径，
    避免
    0 命中时
    因
    `set -e`
    提前静默退出
- focused RED
  通过
  `HEAD`
  snapshot
  真实暴露：
  - 两段 active
    early-data
    示例
    都还在使用
    旧 split import
  - 合同输出：
    - `expected 2 facade import lines, found: 0`
- 最小修复后：
  - 客户端 / 服务端
    两段 active 示例
    已统一回到：
    - `uses fafafa.ssl;`
  - owner-surface
    理由、
    `CreateConnection(...)`
    直连路径、
    `TSSLFactory.GetLibraryInstance(...)`
    入口
    全部保留
- focused verification：
  - `bash -n tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
    - PASS
  - `EARLY_DATA_GUIDE_DOC=/tmp/fafafa_ssl_early_data_guide_head.md bash tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
    - FAIL
  - `bash tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
