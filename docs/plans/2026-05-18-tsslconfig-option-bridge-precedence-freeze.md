# TSSLConfig Option-Bridge Precedence Freeze

## Goal

把 `TSSLConfig` 三个 option-bridge compatibility 字段在“冲突输入”场景下的优先级冻结成明确 truth：

- `EnableCompression`
- `EnableSessionTickets`
- `EnableOCSPStapling`

本批聚焦：

- `TSSLFactory.NormalizeConfig(...)`
- `TSSLFactory.CreateContext(const AConfig)`
- `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`

不在这一批直接删掉 legacy booleans，也不把 builder/import-export 一并改成新语义。

## Architecture

- 当前 live behavior 已经存在，但还没有被正式记录成 contract：
  - legacy booleans 是当前 `v1.x` public record 中仍可写的 compatibility bridge
  - 当调用方同时提供冲突的 `Options` 和 legacy booleans 时：
    - factory normalization 会先让 legacy booleans 覆盖冲突的 option bits
    - 再把最终 `Options` truth 回投回 legacy booleans
- 当前风险：
  - 行为存在，但用户和后续维护者只能从源码里推断
  - 现有测试主要只覆盖“单独写 boolean 会不会折叠进 `Options`”，没有正式覆盖“冲突输入谁赢”

## TDD

### RED

1. 新增 focused Pascal test：
   - `NormalizeConfig(...)` 在 `Options` 与 legacy booleans 冲突时，legacy booleans 必须赢
   - `TSSLFactory.CreateContext(const AConfig)` 必须把冲突输入收成与 legacy booleans 一致的最终 context option truth
   - `ISSLLibrary.SetDefaultConfig(...)` / `CreateContext(AType)` 也必须保持同一条 precedence truth
2. 新增 focused contract：
   - API 文档必须明确记录 precedence rule
   - `NormalizeConfigOptions(...)` 必须保留：
     - legacy boolean -> `Options`
     - final `Options` -> legacy booleans
   - 各 backend `SetDefaultConfig(...)` 继续统一走 `TSSLFactory.NormalizeConfig(...)`

### GREEN

- 不改 runtime precedence 本身
- 只把当前 live behavior 冻结成：
  - focused tests
  - source comments
  - API reference

### REGRESSION

- 复用现有 default-config / scope-bucket focused contracts
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-18-tsslconfig-option-bridge-precedence-freeze.md`
- Add: `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
- Add: `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
- Update: `src/fafafa.ssl.factory.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Commands

1. `bash -n tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
2. `bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
3. `mkdir -p tmp/test_tsslconfig_option_bridge_precedence_freeze && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_precedence_freeze -FEtmp/test_tsslconfig_option_bridge_precedence_freeze -otmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze tests/test_tsslconfig_option_bridge_precedence_freeze.pas && ./tmp/test_tsslconfig_option_bridge_precedence_freeze/test_tsslconfig_option_bridge_precedence_freeze`
4. `bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
5. `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
6. `git diff --check`

## Expected Outputs

- RED 时：
  - focused Pascal test 失败，证明 precedence 还没被正式固定
  - contract 失败，证明 source/doc truth 还没把 precedence 写清楚
- GREEN 后：
  - 冲突输入优先级不再需要靠读源码猜
  - factory path 与 direct-library path 对 option-bridge precedence 保持同一条 truth
  - 文档、测试、源码注释对这条规则达成一致

## Closeout

- 这批没有改 runtime precedence 本身，而是把当前 live behavior 冻结成公开 contract：
  - legacy booleans 仍是 `v1.x` compatibility write surface
  - conflict input 时，legacy booleans 赢
  - final `Options` truth 再回投进 compatibility booleans

- 当前收口结果：
  - `TSSLFactory.NormalizeConfig(...)` 已被 focused runtime test 直接覆盖
  - `TSSLFactory.CreateContext(const AConfig)` 与 direct-library default-config path 也已证明遵守同一条 precedence truth
  - API reference / source comments / contract scripts 现在对这条规则说的是同一句话
