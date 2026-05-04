# Task Plan - WinSSL Session Truth-Source Collapse

## Goal
收敛 WinSSL session 的重复 truth source，让 `src/fafafa.ssl.winssl.connection.pas` 成为唯一真实实现；`src/fafafa.ssl.winssl.session.pas` 只保留兼容 shim；同时去掉 WinSSL session 的假 native-handle surface，并用 source contract 锁住这条边界。

## Current Batch
1. 先补一个 focused source contract，锁住三件事：
   - `winssl.connection.pas` 里的 `TWinSSLSession` 不再实现 `ISSLNativeHandleAccess`
   - `winssl.session.pas` 不再保留独立 `TInterfacedObject` 实现，而是转成兼容 shim
   - WinSSL 测试不再把 `ISSLSession` 当成有 `GetNativeHandle` 的旧接口
2. 然后做最小生产改动：
   - `src/fafafa.ssl.winssl.connection.pas` 去掉 session 级假 native-handle surface
   - `src/fafafa.ssl.winssl.session.pas` 改成兼容 shim，避免外部直接引用该 unit 时断裂
   - 收紧 WinSSL 相关测试/文档的旧 truth
3. 跑 `bash -n`、focused source contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后更新台账并提交。

## Status
- [completed] 现状重载与风险收敛
- [completed] RED/source contract + 最小修复
- [completed] Verification
- [completed] Review and commit

## Risks
- 这批在 Linux 主机上无法做 WinSSL runtime proof，只能做 source-contract 收口，不能把结果包装成 Windows 运行时已验证。
- `winssl.session.pas` 可能被仓库外代码直接引用，因此删除文件过于激进；兼容 shim 比直接删文件更保守。
- WinSSL 旧文档里还保留了过时的 `ISSLSession` 形状，如果不一起收紧，后续还会继续误导。

## Follow-up Queue
1. 如果这批 source contract 收口完成，下一批应转到 Windows 主机上的 focused compile/runtime audit。
2. 如果这批暴露出更多 WinSSL 旧测试依赖旧 session 形状，再按同一 truth-source 边界继续清理，而不是重开整个 WinSSL backend。
