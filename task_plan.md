# Task Plan - MbedTLS Renegotiate Explicit Unsupported Semantics

## Goal
让 MbedTLS connection 的 `Renegotiate` 不再是静默 `False`，而是给出显式 `sslErrUnsupported` 错误分类和稳定诊断文案，收掉当前 public method 的语义缺口。

## Current Batch
1. 先补 focused RED：
   - 在 `tests/test_mbedtls_framework.pas` 增加 `Renegotiate` 的显式 unsupported contract
   - 断言 `Renegotiate=False` 之外，还要断言 `GetError(-1)=sslErrUnsupported`
   - 断言 `GetVerifyResultString` 至少包含 `renegotiation`
2. 然后做最小生产修复：
   - `src/fafafa.ssl.mbedtls.connection.pas` 的 `DoRenegotiate` 记录 `sslErrUnsupported`
   - `DoGetError` 在没有 native error 但已有语义错误时优先返回 `FLastErrorCode`
   - `DoGetVerifyResultString` 在已有语义错误文案时优先返回 `FLastErrorString`
3. 跑 focused framework test、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，再写回台账并提交。

## Status
- [completed] 计划与 RED 测试
- [completed] MbedTLS renegotiate 语义修复
- [completed] Verification
- [completed] Review and commit ready

## Risks
- 这批只收 public method 的错误语义，不实现真正的 renegotiation，也不扩大到 capability 矩阵或完整握手主线。
- `GetVerifyResultString` 当前既承载 verify 结果，也承载若干语义错误文案；修复时只能优先返回已有语义错误，不能把正常 verify 路径打坏。
- MbedTLS framework test 需要在 runtime 可用时稳定复现 RED；如果 host 依赖缺失，必须保持结构化 skip，不把 dependency 问题当成行为失败。

## Follow-up Queue
1. 如果这批收口完成，下一步继续看其它后端仍然存在的静默 `False` / 空 surface / 假 capability 漂移。
2. WolfSSL / MbedTLS 的 capability-vs-runtime 漂移仍值得单开 focused contract 批次处理，但不和这批的 `Renegotiate` 语义修复混在一起。
