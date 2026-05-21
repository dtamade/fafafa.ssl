# 2026-05-21 WINSSL_BACKEND_CAPABILITY_MATRIX 当前 public import 真相对齐

## Goal

修复 `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
里系统证书存储
那段 active
WinSSL 示例
仍在使用
`fafafa.ssl.base`
导入的问题，
让这份专用矩阵页
继续保留：

- WinSSL store helper
  的当前 public
  路径
- 证书枚举
  的 shipped
  public API
- WinSSL capability
  专题矩阵

但不再偏离
当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-winssl-backend-capability-matrix-current-public-import-truth.md`
- Update:
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 WinSSL capability 语义
- 不改 certificate-store 实现
- 不重开 broader WinSSL matrix 真相线

## Architecture Truth

- `ISSLCertificateStore`
  /
  `ISSLCertificate`
  当前都可直接来自：
  - `fafafa.ssl`
- `OpenSystemStore(...)`
  /
  `SSL_STORE_MY`
  仍然来自：
  - `fafafa.ssl.winssl.certstore`
- 因此：
  - active
    WinSSL
    证书存储示例
    当前应使用：
    - `fafafa.ssl`
    - `fafafa.ssl.winssl.certstore`
  - 不应继续导入：
    - `fafafa.ssl.base`

## Steps

1. 收紧现有
   `tests/scripts/test_winssl_store_active_docs_truth_contract.sh`：
   - 继续冻结
     WinSSL
     store helper
     示例真相
   - 新增冻结：
     - active
       matrix 示例
       必须使用：
       - `fafafa.ssl`
     - 不得继续出现：
       - `fafafa.ssl.base`
2. 用 `HEAD`
   matrix snapshot
   跑同一条合同，
   先拿到 RED。
3. 最小修改
   `WINSSL_BACKEND_CAPABILITY_MATRIX.md`
   的导入。
4. 重跑 focused contract
   与
   `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_winssl_store_active_docs_truth_contract.sh
WINSSL_MATRIX_DOC=/tmp/fafafa_ssl_winssl_backend_capability_matrix_head.md bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh
bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh
git diff --check
```

## Expected Result

- `WINSSL_BACKEND_CAPABILITY_MATRIX`
  不再继续教学
  `fafafa.ssl.base`
- active
  WinSSL
  cert-store
  示例
  继续保留
  current helper
  / public API
  真相

## Execution Result

- PASS
- focused contract
  先补齐了：
  - `WINSSL_MATRIX_DOC`
    覆盖入口，
    允许同一条
    focused contract
    对
    `HEAD`
    旧版 matrix
    做 RED
- focused RED
  通过
  `HEAD`
  snapshot
  真实暴露：
  - active
    cert-store
    示例
    仍在使用
    `fafafa.ssl.base`
  - 合同输出：
    - `WinSSL backend matrix must stop teaching fafafa.ssl.base in the active certificate-store example`
- 最小修复后：
  - active
    cert-store
    示例
    已统一回到：
    - `fafafa.ssl`
    - `fafafa.ssl.winssl.certstore`
  - WinSSL
    helper /
    证书枚举
    public API
    语义
    全部保留
- focused verification：
  - `bash -n tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
    - PASS
  - `WINSSL_MATRIX_DOC=/tmp/fafafa_ssl_winssl_backend_capability_matrix_head.md bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
    - FAIL
  - `bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
