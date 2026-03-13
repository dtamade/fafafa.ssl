# Denetworkize CT/OCSP: HTTP transport via hooks (no internal networking)

## Goal
- `fafafa.ssl` 保持 **不实现网络通信** 的边界（不依赖 sockets/fphttpclient，也不在 OpenSSL OCSP 中实现 BIO connect 传输）。
- 为需要 HTTP 的模块（OCSP 在线检查、CT log list 下载）提供 **可注入** 的 HTTP GET/POST hooks。
- 保持尽量多的向后兼容：旧入口仍存在，但传输由上层提供。

## Architecture
- 新增 `src/fafafa.ssl.net.hooks.pas`：
  - 线程局部（`threadvar`）HTTP hooks：`TSSLHTTPHooks`（GET/POST）。
  - `TSSLHTTPHooksScope`：push/pop，并在 record finalize 时自动恢复。
  - `SSLHTTPGet/SSLHTTPPost`：统一入口；当 hooks 缺失时返回 `sslErrUnsupported`。
- `src/fafafa.ssl.base.pas`：
  - 增加 `TSSLHTTPGetCallback` / `TSSLHTTPPostCallback`。
  - 增加可选接口 `ISSLHttpHooksAccess`（context/connection 可实现，用于注入 hooks）。
- OpenSSL backend wiring：
  - `src/fafafa.ssl.openssl.context.pas` 实现 `ISSLHttpHooksAccess`，保存 hooks。
  - `src/fafafa.ssl.openssl.connection.pas` 在执行 OCSP 在线检查前，将 context hooks push 到线程局部（scope guard）。
- 模块去网络化：
  - `src/fafafa.ssl.openssl.api.ocsp.pas`：`SendOCSPRequest` 改为通过 `SSLHTTPPost` 发送 OCSP request（不再 BIO connect）。
  - `src/fafafa.ssl.ct.log.pas`：`DownloadCTLogList` 改为 `SSLHTTPGet`（移除 fphttpclient/ssockets 依赖）。
  - `src/fafafa.ssl.http.client.pas`：保留为兼容桥接层，内部改用 `SSLHTTPPost`（不再 sockets）。

## Files
- Add: `src/fafafa.ssl.net.hooks.pas`
- Modify:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.openssl.context.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.openssl.api.ocsp.pas`
  - `src/fafafa.ssl.ct.log.pas`
  - `src/fafafa.ssl.http.client.pas`
  - `src/fafafa.ssl.ocsp.stapling.pas`
  - `scripts/run_all_module_tests.sh`
- Add tests:
  - `tests/ct/test_p2_ct_http_hooks.pas`
  - `tests/ocsp/test_p2_ocsp_http_hooks.pas`
- Add contracts:
  - `tests/scripts/test_denetworkize_ct_log_no_fphttpclient_contract.sh`
  - `tests/scripts/test_denetworkize_http_client_no_sockets_contract.sh`
  - `tests/scripts/test_denetworkize_openssl_ocsp_no_internal_net_contract.sh`

## Step-by-step
1) 交付 hooks 抽象与可选接口
   - 期望：无网络依赖引入；基础类型/接口可用。

2) OpenSSL 上下文/连接注入 hooks
   - 期望：仅在需要 HTTP 的路径 push hooks；其余逻辑不受影响。

3) CT/OCSP 去网络化改造
   - 期望：CT log 下载与 OCSP 在线请求均不再直接实现网络传输。

4) 补齐测试与契约
   - 期望：hooks 正确被调用；缺失 hooks 的行为可预测；契约脚本能阻止回归（重新引入网络依赖）。

5) 验证（按最小门禁优先）
   - `python3 -u scripts/compile_all_modules.py`
     - 期望：PASS（全部模块可编译）
   - `bash scripts/run_all_module_tests.sh --modules OCSP,CT --fast-local`
     - 期望：PASS（包含新增 hooks 测试）
   - `bash tests/scripts/test_denetworkize_ct_log_no_fphttpclient_contract.sh`
     - 期望：PASS
   - `bash tests/scripts/test_denetworkize_http_client_no_sockets_contract.sh`
     - 期望：PASS
   - `bash tests/scripts/test_denetworkize_openssl_ocsp_no_internal_net_contract.sh`
     - 期望：PASS

## Done Criteria
- hooks 抽象与 OpenSSL wiring 合并后，CT/OCSP 相关改动可独立审查。
- 编译门禁 + OCSP/CT 模块测试 + 契约脚本全部通过。
- `git status` 干净（无未提交改动；不引入不必要的构建产物）。

