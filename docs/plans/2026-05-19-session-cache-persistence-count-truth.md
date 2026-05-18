# 2026-05-19 Session Cache Persistence Count Truth

## Goal

修复 `TSSLSessionCache.SaveToFile(...)` 的文件头计数漂移，确保缓存里混有 invalid/expired session 时，持久化文件仍能被 `LoadFromFile(...)` 正常读取。

## Why now

- 当前主线已经从 macOS / Windows workflow blocker 收回到“接口设计与实现完整性”审查。
- 在继续审查 generic session/persistence seam 时，发现 `SaveToFile(...)` 会先写 `FCache.Count`，但随后又跳过 invalid/expired session。
- 这会让文件头记录的条目数大于真实写入条目数，`LoadFromFile(...)` 读取时直接读坏文件。

## Files

- `src/fafafa.ssl.session.cache.pas`
- `tests/test_session_cache_persistence_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Approach

1. 先加 focused RED，构造“一个 valid + 一个 invalid session”的缓存文件。
2. 证明旧实现下：
   - `SaveToFile(...)` 仍返回 `True`
   - 但 `LoadFromFile(...)` 因头部计数撒谎而失败
3. 最小修复 `SaveToFile(...)`：
   - 先写占位计数
   - 只对真实写出的条目递增
   - 最后回填真实写入数
4. 重新跑 focused test 与 `git diff --check`。

## Commands

```bash
mkdir -p tmp/test_session_cache_persistence_contract
fpc -B -Fu./src -Fu./tests \
  -FUtmp/test_session_cache_persistence_contract \
  -FEtmp/test_session_cache_persistence_contract \
  -otmp/test_session_cache_persistence_contract/test_session_cache_persistence_contract \
  tests/test_session_cache_persistence_contract.pas
./tmp/test_session_cache_persistence_contract/test_session_cache_persistence_contract
git diff --check
```

## Expected Output

- RED:
  - `LoadFromFile succeeds after SaveToFile skipped invalid entries` 失败
- GREEN:
  - valid session 可以正常恢复
  - skipped invalid session 不再污染文件结构

## Execution Result

- PASS
- 新增 `tests/test_session_cache_persistence_contract.pas`，直接覆盖：
  - valid + invalid 混合缓存
  - Save/Load round-trip
  - invalid entry 不应 materialize
- 旧实现已被 fresh RED 证明：
  - `SaveToFile(...)` 返回 `True`
  - 但 `LoadFromFile(...)` 因文件头计数漂移而失败
- 修复后：
  - `SaveToFile(...)` 会回填真实写入条目数
  - `LoadFromFile(...)` 恢复正常
  - focused test 与 `git diff --check` 已通过
