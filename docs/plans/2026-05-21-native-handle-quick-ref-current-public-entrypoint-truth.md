# 2026-05-21 Native Handle Quick Ref 当前 public entrypoint 真相对齐

## Goal

修复 `docs/NATIVE_HANDLE_QUICK_REF.md`
里仍会把高级用户带回旧入口心智的内容，
让这份 native-handle 快速参考继续聚焦
`ISSLNativeHandleAccess`
与
`fafafa.ssl.native_handle`
的实际用法，
但不再继续发布：

- split `uses fafafa.ssl.base`
  的旧入口心智
- `TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL)`
  这类没把当前 library-entrypoint 讲清的例子
- `CreateLibrary`
  这类已不再是当前 public truth 的旧工厂说法

## Scope

- Add:
  - `docs/plans/2026-05-21-native-handle-quick-ref-current-public-entrypoint-truth.md`
  - `tests/scripts/test_native_handle_quick_ref_current_public_entrypoint_truth_contract.sh`
- Update:
  - `docs/NATIVE_HANDLE_QUICK_REF.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 `src/` runtime 实现
- 不重写 native-handle helper API
- 不重开 capability / backend completeness 主线

## Architecture Truth

- 当前普通调用方若只做 capability / native-handle 查询，
  不必再拆回
  `uses fafafa.ssl.base`
  /
  `fafafa.ssl.factory`；
  `fafafa.ssl`
  已 re-export
  `ISSLContext`
  /
  `ISSLNativeHandleAccess`
  /
  `TSSLFactory`
- 当前 fixed-backend + native-handle 高级场景，
  library-entrypoint
  优先应写成：
  `TSSLFactory.GetLibraryInstance(...)`
  +
  `Lib.CreateContext(...)`
- 普通 TLS 建立流程
  仍应优先回到：
  `TSSLContextBuilder`
  /
  `TSSLConnector`
  /
  `TSSLStream`
- `CreateLibrary`
  不应再出现在这页活跃 quick ref 中

## Steps

1. 新增 focused contract：
   - 当前 quick ref 必须明确 `fafafa.ssl` + `fafafa.ssl.native_handle` 的主导入面
   - OpenSSL-specific 示例必须走
     `TSSLFactory.GetLibraryInstance(...)`
     +
     `Lib.CreateContext(...)`
   - 文档不得继续出现
     `CreateLibrary`
     或
     `TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL)`
     这类旧示例
2. 跑 focused contract，拿到 RED。
3. 用最小文档改动修正 quick ref。
4. 重跑 focused contract 与相关回归 contract。

## Verification

```bash
bash -n tests/scripts/test_native_handle_quick_ref_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_native_handle_quick_ref_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_implemented_backend_future_truth_contract.sh
git diff --check
```

## Expected Result

- native-handle quick ref
  不再继续教授
  split base import
  和旧工厂入口
- 高级 fixed-backend / native-handle 场景
  回到当前 library-entrypoint 真相
- 普通 TLS 主入口
  与 native-handle 高级路径
  的边界在同一页里说清楚
