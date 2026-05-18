# TSSLConfig Option-Bridge Default Truth Parity

## Goal

把 `TSSLConfig` 三个 option-bridge compatibility 字段在“fresh default-config surfaces”上的真相收口一致：

- `EnableCompression`
- `EnableSessionTickets`
- `EnableOCSPStapling`

本批聚焦：

- `ISSLLibrary.GetDefaultConfig(...)`
- `CreateDefaultConfig(...)`
- `Lib.SetDefaultConfig(Lib.GetDefaultConfig)` round-trip

不在这一批改动更大的 `Options vs legacy booleans` 冲突优先级规则，也不把 builder/import-export 全部混进来。

## Architecture

- 当前 source truth：
  - public record 仍同时暴露：
    - modern `Options`
    - legacy option-bridge booleans
  - factory normalization 会把 booleans 折叠进 `Options`
- 当前问题：
  - 多个 backend library constructor 的 `FDefaultConfig` 仍是未归一化的 mixed truth
  - 这会导致：
    - `GetDefaultConfig(...)` 暴露 stale bridge booleans
    - `CreateDefaultConfig(...)` 在某些默认库下丢失既有 option truth
    - `Lib.SetDefaultConfig(Lib.GetDefaultConfig)` round-trip 可能把 session-ticket default 意外冲掉

## TDD

### RED

1. 新增 FreePascal runtime test：
   - `GetDefaultConfig(...)` 返回的 option-bridge truth 不得 stale
   - `SetDefaultConfig(GetDefaultConfig)` 不得丢 session-ticket / compression default
   - `CreateDefaultConfig(...)` 在强制 FreePascal 默认库下也要保持 option-bridge truth
2. 新增跨 backend source contract：
   - 所有 backend library constructor 都必须 normalize `FDefaultConfig`
   - FreePascal library constructor 必须显式保留 session-ticket default truth

### GREEN

- 在 constructor-level default config path 上补齐 missing normalization / bridge-default truth
- 必要时补一条小的 public doc 说明，避免以后再把 fresh default-config surface 当“未归一化原样记录”

### REGRESSION

- 复用 direct-library default-config parity contract
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-18-tsslconfig-option-bridge-default-truth-parity.md`
- Add: `tests/test_tsslconfig_option_bridge_default_truth.pas`
- Add: `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
- Update: `src/fafafa.ssl.freepascal.lib.pas`
- Update: `src/fafafa.ssl.winssl.lib.pas`
- Update: `src/fafafa.ssl.mbedtls.lib.pas`
- Update: `src/fafafa.ssl.wolfssl.lib.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`

## Commands

1. `bash -n tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
2. `bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
3. `mkdir -p tmp/test_tsslconfig_option_bridge_default_truth && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_tsslconfig_option_bridge_default_truth -FEtmp/test_tsslconfig_option_bridge_default_truth -otmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth tests/test_tsslconfig_option_bridge_default_truth.pas && ./tmp/test_tsslconfig_option_bridge_default_truth/test_tsslconfig_option_bridge_default_truth`
4. `bash tests/scripts/test_direct_library_default_config_parity_contract.sh`
5. `git diff --check`

## Expected Outputs

- RED 时：
  - FreePascal runtime test 失败，证明 fresh default-config surface 仍暴露 stale option-bridge truth
  - source contract 失败，证明某些 backend constructor 仍未 normalize `FDefaultConfig`
- GREEN 后：
  - fresh default-config surface 不再自相矛盾
  - FreePascal round-trip 不再意外丢 `ssoEnableSessionTickets`
  - source contract 证明 constructor-level default truth 已在 backend 间收平

## Closeout

- 这批最后确认的真实根因，不只是 constructor-level default config 未归一化：
  - direct-library `CreateFreePascalSSLLibrary` path 当时已经是绿的
  - 真正失真的是 factory-held backend instance 的 `GetDefaultConfig(...)`
  - 因而 downstream `CreateDefaultConfig(...)` 只是症状，不是源头

- 当前最终修法：
  - `TSSLFactory` 新增 explicit creator-function registration path
  - `CreateLibraryInstance(...)` 优先走 registration 里的 `CreateFunc`
  - real backends (`openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`)
    的注册统一改成 `@Create*SSLLibrary`

- 当前收口结果：
  - factory-held `ISSLLibrary.GetDefaultConfig(...)` 不再丢 `EnableSessionTickets`
  - `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig(...)` 在强制 FreePascal 默认库下恢复正确
  - `CreateDefaultConfig(...)` fresh surface 恢复与最终 `Options` truth 对齐
  - 既有 `tests/config/test_default_config.pas` 继续保持绿色
