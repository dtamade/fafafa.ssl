# ReadString Active Example Signature Truth

## Goal

收口活跃 guide / reference / example 中对 `ISSLConnection.ReadString` 的旧签名误导，明确当前 shipped source truth：

- `ReadString` 不是“返回字符串”的函数
- 当前真实签名是：
  - `function ReadString(out AStr: string): Boolean;`
- 活跃示例应统一改成：
  - `if Conn.ReadString(LData) then ...`

## Why This Batch

前面已经完成了 `ISSLConnection` convenience surface 的分类真相冻结，但重新扫活跃文档后发现：

- `docs/reference/API_REFERENCE.md`
- `docs/guides/USER_GUIDE.md`
- `docs/guides/MIGRATION_GUIDE.md`
- `examples/04_https_rest_client.pas`

仍残留把 `ReadString` 当成“直接返回字符串”的旧示例写法。这不是 archive 噪音，而是高入口文档 / 示例漂移，会直接误导调用方写出与当前 public signature 不匹配的代码。

## Scope

- 不改 runtime 行为
- 不改 public Pascal signature
- 只修活跃 docs/example guidance 与 focused contract

## Files

- Add: `docs/plans/2026-05-19-readstring-active-example-signature-truth.md`
- Add: `tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/guides/USER_GUIDE.md`
- Update: `docs/guides/MIGRATION_GUIDE.md`
- Update: `examples/04_https_rest_client.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 写 focused shell contract，先命中活跃文档中的旧签名用法。
2. 修正 4 份高入口文件：
   - `API_REFERENCE`
   - `USER_GUIDE`
   - `MIGRATION_GUIDE`
   - `04_https_rest_client`
3. 统一改成 `if ...ReadString(LData) then ...` 风格。
4. 重新跑合同与最小语法/patch 验证。
5. 同步 planning files，提交并推送。

## Verification

1. `bash -n tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
2. `bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
3. `fpc -B -Fu./src -Fu./examples -FUtmp/example_04_https_rest_client -FEtmp/example_04_https_rest_client -otmp/example_04_https_rest_client/example_04_https_rest_client examples/04_https_rest_client.pas`
4. `git diff --check`

## Risks

- 不要把 scope 扩大成整个 `ReadString` runtime 行为或 `ISSLConnection` redesign。
- 不要去改 archive 历史文档；这批只修活跃入口。
- 不要让 contract 只守住一个文件，而漏掉另外几份同类高入口示例。
