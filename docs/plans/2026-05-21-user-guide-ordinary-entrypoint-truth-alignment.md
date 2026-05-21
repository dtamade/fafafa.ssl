# 2026-05-21 USER_GUIDE 普通用户主入口真相对齐

## Goal

修复 `docs/guides/USER_GUIDE.md`
里对普通用户最先看到的
HTTPS client/server
主场景入口漂移，
避免这份用户指南继续把
low-level fixed-backend /
direct `ISSLLibrary` /
`ISSLContext` /
`CreateConnection(...)`
路径教成普通新代码默认入口。

## Scope

- Add:
  - `docs/plans/2026-05-21-user-guide-ordinary-entrypoint-truth-alignment.md`
  - `tests/scripts/test_user_guide_ordinary_entrypoint_truth_contract.sh`
- Update:
  - `docs/guides/USER_GUIDE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 runtime 实现
- 不重写证书管理 / WinSSL 企业 helper 场景
- 不删 low-level direct path，只把它从普通主场景降回次级路径

## Architecture Truth

- 普通新代码当前主入口：
  - `uses fafafa.ssl, fafafa.ssl.context.builder;`
  - `TSSLContextBuilder`
  - `TSSLConnector`
  - `TSSLAcceptor`
  - `TSSLStream`
- fixed-backend /
  low-level owner-surface /
  direct connection path
  仍然 shipped，
  但应放在：
  - 高级场景
  - backend-specific guide
  - specialized owner-surface reasoning
- `USER_GUIDE`
  作为普通用户指南，
  前两个
  HTTPS client/server
  主场景
  不应再默认教学：
  - `TSSLFactory.GetLibraryInstance(sslOpenSSL)`
  - `Lib.CreateContext(...)`
  - `Ctx.CreateConnection(...)`
  - 手工 `SetServerName(...)`

## Steps

1. 新增 focused contract：
   - `USER_GUIDE`
     必须显式声明
     前两个主场景优先展示普通新代码主入口
   - client/server
     场景
     必须使用
     builder +
     connector/acceptor
     path
   - 前两个主场景
     不得再保留
     low-level fixed-backend
     初始化与 direct `CreateConnection(...)`
2. 跑 focused contract，拿到 RED。
3. 用最小改动修正 `USER_GUIDE`：
   - 顶部补当前主入口说明
   - 场景 1 / 2 改回 builder 主路径
   - 明确 ordinary one-way TLS server 与 mTLS 的 verify intent
4. 重跑 focused contract 与相关回归 contract。

## Verification

```bash
bash -n tests/scripts/test_user_guide_ordinary_entrypoint_truth_contract.sh
bash tests/scripts/test_user_guide_ordinary_entrypoint_truth_contract.sh
bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_active_server_example_verify_intent_truth_contract.sh
git diff --check
```

## Expected Result

- `USER_GUIDE`
  前两个主场景
  重新回到当前普通用户主入口
- low-level fixed-backend /
  direct path
  仍保留，
  但不再占据普通用户第一屏主路径
- 接口设计、调用入口、文档主线
  继续向同一套 public truth 收敛
