# minimal ci gate openssl cache runtime option (2026-03-04)

## Goal
将 OpenSSL 证书验证缓存运行时策略回归测试接入最小门禁入口，提供显式可选开关，避免仅靠源码字符串合同判断策略语义。

## Scope
- `scripts/run_minimal_ci_gate.sh`
- `tests/scripts/test_minimal_ci_gate_openssl_cache_runtime_option_contract.sh`

最小改动原则：
- 默认行为不变（不开启网络运行时测试）。
- 仅新增一个可选项和一条 gate 命令链。

## Files
- Modify: `scripts/run_minimal_ci_gate.sh`
- Add: `tests/scripts/test_minimal_ci_gate_openssl_cache_runtime_option_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## RED -> GREEN -> Regression
1. RED
   - 新增合同：默认不触发 runtime 测试；启用新开关时应出现编译+运行命令，并带 `FAFAFA_RUN_NETWORK_TESTS=1`。
   - 初次运行应失败（脚本暂无该开关）。
2. GREEN
   - 在 `run_minimal_ci_gate.sh` 增加：
     - `WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=false`
     - CLI 选项：`--with-openssl-cert-verify-cache-runtime`
     - 可选 gate 步骤：编译并运行 `tests/integration/test_openssl_cert_verify_cache_policy_runtime.pas`
3. Regression
   - 新合同
   - 既有合同：
     - `tests/scripts/test_minimal_ci_gate_compile_module_isolation_passthrough.sh`
     - `tests/scripts/test_minimal_ci_gate_platform_path_checks_integration.sh`
     - `tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh`
     - `tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`
     - `tests/scripts/test_minimal_ci_gate_fpc_host_passthrough_contract.sh`
   - `bash -n`：脚本 + 新合同

## Expected
- 最小门禁可按需执行运行时缓存策略回归（真实网络握手分支），默认流程保持轻量。
