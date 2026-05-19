# API Reference Certificate Surfaces Truth Plan

**Goal:** 把 `docs/reference/API_REFERENCE.md` 里的 `ISSLCertificate` 高入口代码块补回当前 shipped source truth，并补上当前缺失的 `ISSLCertificateStore` 独立小节，避免 active canonical API doc 继续把证书 public surface 写得比源码更窄。

**Architecture:** 这批继续保持在 active-doc truth 范围内，不改 runtime，不改 public Pascal source：
- `tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`：先补 focused RED，证明当前 `API_REFERENCE` 的证书相关高入口 surface 仍然不完整。
- `docs/reference/API_REFERENCE.md`：把 `ISSLCertificate` 代码块扩到当前源码真相，并新增 `ISSLCertificateStore` 的独立代码块小节。

**Files:**
- Add: `tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
- Modify: `docs/reference/API_REFERENCE.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove current certificate-surface doc drift

Run:

```bash
bash -n tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh
bash tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh
```

Expected RED before doc fix:
- `ISSLCertificate` code block still misses shipped methods such as:
  - `LoadFromMemory`
  - `SaveToStream`
  - `GetInfo`
  - `GetPublicKeyAlgorithm`
  - `GetSignatureAlgorithm`
  - `GetDaysUntilExpiry`
  - `GetSubjectCN`
  - `GetExtension`
  - `GetFingerprint(...)`
  - issuer-link / clone helpers
- `ISSLCertificateStore` high-entry section is currently absent from `API_REFERENCE`

## Task 2: GREEN - restore certificate/store source truth in API reference

Change:
- keep `ISSLCertificate` / `ISSLCertificateStore` as active source-truth views
- add the currently shipped methods back into `ISSLCertificate`
- add a dedicated `ISSLCertificateStore` section and code block
- do not widen scope to runtime certificate verification behavior or backend-native-handle audits

## Task 3: Verification

Run:

```bash
bash -n tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh
bash tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh
git diff --check
```

## Definition Of Done

- `API_REFERENCE` no longer understates `ISSLCertificate` public surface
- `ISSLCertificateStore` has a real high-entry section in the canonical API reference
- focused contract stays green
- planning files record the new canonical-doc baseline so this gap is not reopened later

## Execution Result

- focused RED 先直接压实了两个活跃入口缺口：
  - `ISSLCertificateStore` 小节在 `API_REFERENCE` 里根本不存在
  - `ISSLCertificate` 代码块也仍停在旧的窄化 surface，上面缺少一批当前 shipped methods
- 最小 GREEN 没有改 runtime：
  - 只把 `ISSLCertificate` 代码块扩回 current source truth
  - 新增 `ISSLCertificateStore` 独立小节与代码块
  - 并把新增的 store guide 导航接到正确相对路径
- 验证结果：
  - `bash -n tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`：PASS
  - `bash tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`：PASS
  - `git diff --check`：PASS
