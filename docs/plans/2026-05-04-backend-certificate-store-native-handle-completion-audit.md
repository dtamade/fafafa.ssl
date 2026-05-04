# Backend Certificate And Store Native-Handle Completion Audit Plan

**Goal:** 把 `ISSLCertificate` / `ISSLCertificateStore` 上尚未纳入 completion audit 的 `ISSLNativeHandleAccess` public surface 锁成跨后端契约：基于 C 库 / OS-native 的 backend 需要暴露真实句柄，纯 Pascal backend 继续保持 absent。

**Architecture:** 这批继续走 completion audit 模式，不预设一定有生产代码变更。先在 `tests/contract/test_backend_contract.pas` 增加两个跨后端契约：
- certificate-level：用已加载 fixture 的证书对象验证 optional interface、backend type、handle validity 和 helper round-trip。
- certificate-store-level：用 `CreateCertificateStore()` 返回的公开对象验证 optional interface、backend type、handle validity 和 helper round-trip。

只有 focused RED 证明真实漂移时，才做最小修复，优先收口在对应 backend 的 certificate / certstore 实现内。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.mbedtls.certificate.pas` (only if RED proves store/cert drift)
- Modify: `src/fafafa.ssl.wolfssl.certificate.pas` (only if RED proves store/cert drift)
- Modify: `src/fafafa.ssl.winssl.certificate.pas` (only if RED proves store/cert drift)
- Modify: `src/fafafa.ssl.winssl.certstore.pas` (only if RED proves store/cert drift)
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove certificate/store native-handle drift

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
```

Add checks:
- `Contract 16`: loaded certificate native-handle interface alignment
- `Contract 17`: certificate-store native-handle interface alignment

Expected audit rules:
- `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 这类 C-library / OS-native backend：
  - `Supports(..., ISSLNativeHandleAccess, ...)` 必须为真
  - `GetBackendType` 必须与 backend 枚举一致
  - `IsNativeHandleValid` 必须为真
  - `GetNativeHandle` 不应返回 `nil`
  - `TryGetNativeHandle(...)` 必须成功并 round-trip 到同一 handle
- `FreePascal`：
  - `ISSLCertificate` / `ISSLCertificateStore` 都必须保持 `ISSLNativeHandleAccess` absent
  - `TryGetNativeHandle(...)` 必须返回 `False`

Constraints:
- certificate contract 必须使用已加载 fixture，不能拿空 certificate wrapper 当真值
- store contract 不向 WinSSL 系统 store 注入 fixture，避免把测试变成系统证书写入
- 不扩大到 diagnostics / session resumption / verify / connection info

## Task 2: GREEN - only if the new contracts expose real drift

Possible change directions:
- 类声明漂移：补/移除 `ISSLNativeHandleAccess`
- getter/validity 漂移：修正 `GetBackendType` / `GetNativeHandle` / `IsNativeHandleValid`
- store 初始化漂移：在 constructor 或公开创建路径里分配真实 native store handle

## Task 3: Verification

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- certificate/store native-handle truth 已被 cross-backend contract 锁住
- 若 RED 暴露真实漂移，最小修复已经完成并验证通过
- `task_plan.md` / `findings.md` / `progress.md` 已写回本批证据
