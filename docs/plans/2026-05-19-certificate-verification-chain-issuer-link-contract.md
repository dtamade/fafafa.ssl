# Certificate Verification Chain Issuer-Link Contract

## Goal

把 `ISSLCertificateVerification.GetPeerCertificateChain()` 与 `ISSLConnection.GetPeerCertificateChain()` 之间的 issuer-link truth 正式纳入统一 backend contract，确保：

- optional/core 两条 getter 返回的 peer chain 长度、entry nilness、subject/issuer/serial 继续保持一致
- 每个 chain entry 的 `GetIssuerCertificate()` nil/non-nil truth 也必须一致
- 如果 issuer-link 存在，optional/core 对应 issuer cert 的 public identity 也必须一致

## Scope

- `tests/contract/test_backend_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Steps

1. 扩展 `Contract 21`，先把 optional/core chain issuer-link truth 写进统一 contract
2. 跑 backend contract，观察是否有 backend 在新断言下转红
3. 若转红，则按最小修法修对应 backend 或公共路径
4. 再跑 backend contract、`git diff --check`，完成记账

## Commands

```bash
mkdir -p tmp/backend_contract_units && \
  fpc -B -Fu./src -Fu./tests \
  -FUtmp/backend_contract_units \
  -FEtmp \
  -otmp/tmp_backend_contract \
  tests/contract/test_backend_contract.pas && \
  ./tmp/tmp_backend_contract

git diff --check
```

## Expected Closeout

- `Contract 21` 对 optional/core peer-chain issuer-link truth 有明确断言
- 如果没有残余 backend 漏口，backend contract 直接 green
- 如果有残余 backend 漏口，必须由同一批次最小修法收掉后再 green
