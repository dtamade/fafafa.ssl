# Task Plan - WinSSL Context And Library Access Alignment

## Goal
收口 WinSSL connection 对 context/library 的内部访问路径，不再把 `FContext: ISSLContext` 和 `ISSLLibrary` 直接硬转成 `TWinSSLContext` / `TWinSSLLibrary`，改成显式内部 access interface，消掉 Win64 交叉编译中的不安全类型告警。

## Current Batch
1. 先补 focused RED：
   - 新增 `tests/scripts/test_winssl_connection_context_access_contract.sh`
   - 先锁住 `src/fafafa.ssl.winssl.connection.pas` 不再出现 `TWinSSLContext(FContext)` 和 `TWinSSLLibrary(...)` 这类硬转
   - 运行脚本契约，先让旧代码在 source-level 失败
2. 然后做最小生产修复：
   - 在 `src/fafafa.ssl.winssl.context.pas` 增加内部 `IWinSSLContextAccess`
   - 在 `src/fafafa.ssl.winssl.lib.pas` 增加内部 `IWinSSLLibraryStatsAccess`
   - `src/fafafa.ssl.winssl.connection.pas` 改为用 `Supports(...)` 查询内部 access interface，不再做类硬转
3. 跑 focused source contract、Win64 交叉编译、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，再写回台账并提交。

## Status
- [completed] 计划与 RED 测试
- [completed] WinSSL 内部 access interface 修复
- [completed] Verification
- [completed] Review and commit

## Verification Summary
- focused source contract:
  - `bash -n tests/scripts/test_winssl_connection_context_access_contract.sh`
  - `bash tests/scripts/test_winssl_connection_context_access_contract.sh`
  - 初次 RED 命中：`winssl connection no longer hard-casts ISSLContext to TWinSSLContext`
  - 修复后 GREEN：脚本全部 `[PASS]`
- Win64 交叉编译:
  - `mkdir -p tmp/winssl_session_mgmt_win64`
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas > tmp/winssl_session_mgmt_win64/build.log 2>&1`
  - `! rg 'Class types "ISSLContext" and "TWinSSLContext" are not related|Class types "ISSLLibrary" and "TWinSSLLibrary" are not related' tmp/winssl_session_mgmt_win64/build.log`
  - 结果：Win64 编译成功，且两类不安全类型告警已消失
- compile gate:
  - `python3 scripts/compile_all_modules.py`
  - 结果：`185/185` 核心模块编译成功
- minimal CI gate:
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - 结果：最终 `[PASS] minimal CI gate finished`

## Risks
- 这批只收口 WinSSL 内部访问路径，不改握手、证书验证、SNI/ALPN/session 行为。
- 修复必须保持 public `ISSLContext` / `ISSLLibrary` 不扩张；新增接口只用于 WinSSL 内部协作。
- 不能把 Linux 上的 Win64 交叉编译结果写成 Windows runtime 已证实；当前真实剩余 blocker 仍是 Windows runtime proof。

## Follow-up Queue
1. 如果环境恢复，下一步优先补 WinSSL Windows runtime proof；Linux 主机上的 cross-target compile 只能证明 compile surface。
2. 在 runtime 环境缺失前，只继续做能直接减少 WinSSL compile/public-contract 漂移的静态批次。
3. 更广的 backend completeness 仍要继续批次化推进，但每次只锁一组 capability/interface truth。
