# Cert Verify Cache Concurrency Contract and Global Lock Init Hardening (2026-03-03)

## Goal
补齐 `TCertVerifyCache` 的并发压力合同，并消除 `GetGlobalCertVerifyCache` 的懒初始化竞态窗口，确保多线程访问下无崩溃、无锁错配、行为稳定。

## Architecture
- 代码层：将全局锁改为单元初始化期创建，避免并发首访时 `Enter/Leave` 锁对象不一致风险。
- 测试层：新增并发合同测试覆盖两条路径：
  - 全局缓存访问器并发一致性；
  - 同证书 `TryGet/Put` 高并发读写稳定性。

## Files
- Modify: `src/fafafa.ssl.cert.verify.cache.pas`
- Add: `tests/test_cert_verify_cache_concurrency.pas`

## Steps
1. 修改全局锁初始化策略：移除 `GetGlobalCertVerifyCache` 中懒创建逻辑，改为 `initialization` 阶段初始化。
2. 新增并发合同测试：
   - 多线程并发调用 `GetGlobalCertVerifyCache`，断言返回实例指针一致且无异常。
   - 多线程并发执行 `TryGet/Put`，断言线程无异常、缓存大小稳定（单证书场景保持 1）。
3. 运行聚焦回归：
   - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_concurrency.pas -otmp/test_cert_verify_cache_concurrency && ./tmp/test_cert_verify_cache_concurrency`
   - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/test_cert_verify_cache_nil_guard.pas -otmp/test_cert_verify_cache_nil_guard && ./tmp/test_cert_verify_cache_nil_guard`
   - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_cert_verify_cache_policy.pas -otmp/test_openssl_cert_verify_cache_policy && ./tmp/test_openssl_cert_verify_cache_policy`
4. 运行主干编译门禁：
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- 新并发合同测试通过。
- nil-guard 与 cache policy 既有回归保持通过。
- `compile_all_modules` 保持 179/179 通过。
