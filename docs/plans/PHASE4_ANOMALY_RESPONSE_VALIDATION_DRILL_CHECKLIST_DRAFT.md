# Phase 4 异常处置验证演练清单草案（Draft）

**目标**：将 B51 的 OCSP stapling 代码主线（M1-M5）转化为可执行演练清单，用于异常处置闭环验证与回归门禁。  
**阶段**：Batch B51

---

## 1. 演练范围

- OpenSSL OCSP stapled response 读取与验证链路。
- status_request enablement（context option -> connection handshake）。
- required stapling fail-closed 策略（缺失 / 验证失败）。
- fixture 驱动回归（successful/basic 与 malformed 场景）。

---

## 2. 前置条件

- OpenSSL 1.1.1+ 或 3.x 可用。
- 已生成夹具：`tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der`。
- 编译输出目录：`./bin`。

---

## 3. 验证清单（执行顺序）

1. **编译回归用例**
   - `fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/openssl/test_ocsp_connection_verification_regression.pas -o./bin/test_ocsp_connection_verification_regression`
2. **执行回归用例**
   - `./bin/test_ocsp_connection_verification_regression`
   - 期望：`PASS=4, FAIL=0, SKIP=0`
3. **编译 P2 OCSP 综合用例**
   - `fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib tests/certificate/test_p2_ocsp_comprehensive.pas -o./bin/test_p2_ocsp_comprehensive`
4. **执行 P2 OCSP 综合用例**
   - `echo "" | ./bin/test_p2_ocsp_comprehensive`
   - 期望：`55/55`
5. **全模块编译门禁（可选但建议）**
   - `python3 scripts/compile_all_modules.py`
   - 期望：`157/157`

---

## 4. 异常演练条目

- 场景 A：OpenSSL 缺失 `SSL_get_tlsext_status_ocsp_resp` 符号（验证 `SSL_ctrl` fallback）。
- 场景 B：启用 `ssoEnableOCSPStapling` 时 context/connection 均应设置 OCSP `status_type=ocsp`。
- 场景 C：`ssoRequireOCSPStapling` + 无 stapled response -> fail-closed（`X509_V_ERR_OCSP_VERIFY_NEEDED`）。
- 场景 D：`ssoRequireOCSPStapling` + 无法验证 stapled response -> fail-closed（`X509_V_ERR_OCSP_VERIFY_FAILED`）。

---

## 5. 完成标准（B51）

- B51-M1~M5 对应回归断言全部可复现。
- 关键结果满足：`PASS=4`（回归）+ `55/55`（综合）+ `157/157`（编译门禁）。
- 失败场景均具备可定位诊断信息（verify_result 与日志链路）。

---

## 6. 后续任务

- B52：SLA 与回滚联动演练脚本草案。
- B53：回写覆盖率修复闭环验收门禁草案。
