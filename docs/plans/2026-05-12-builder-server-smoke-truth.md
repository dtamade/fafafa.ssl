# Builder Server Smoke Truth

## Goal

修复 `tests/test_builder_integration.pas` 里的 server smoke 误导，让它不再把一个缺证书的 `BuildServer` 失败路径当成集成演示输出。

## Architecture

- 不改 runtime：`BuildServer` 缺证书失败仍然是正确语义。
- 先核对 source/docs/contract：
  - `BuildServer` / `ValidateServer` 是否都要求证书
  - 现有 builder contract 是否已经覆盖了无证书失败 / 有证书成功
- 最小修法只改 smoke：
  - 为 server case 临时生成自签名证书和私钥
  - 再调用 `WithPerformanceFirst` + `WithCertificatePEM(...)` + `WithPrivateKeyPEM(...)` + `BuildServer`

## Files

- Add: `docs/plans/2026-05-12-builder-server-smoke-truth.md`
- Modify: `tests/test_builder_integration.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Verification

1. `fpc -Fu./src -Fu./tests tests/test_builder_integration.pas -otmp/test_builder_integration && ./tmp/test_builder_integration`
2. `git diff --check`
3. `git status --short`

## Risks

- 不要把 runtime 正确的“server context requires a certificate”语义误改掉。
- 不要让 smoke 依赖仓库外部证书文件；测试应自给自足生成临时 PEM。
