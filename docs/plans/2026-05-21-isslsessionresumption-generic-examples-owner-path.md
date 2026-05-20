# 2026-05-21 ISSLSessionResumption Generic Examples Owner Path

## Goal

把当前仍在普通活跃 examples 里继续教学 direct core
`GetSession / SetSession / IsSessionReused`
的一批入口收口到
`ISSLSessionResumption`
owner path，并顺手收掉示例里的 session semantics 过强承诺。

## Scope

- `examples/session_reuse_example.pas`
- `examples/session_resumption_example.pas`
- `examples/https_client/https_client_session.pas`
- `examples/production/https_client_session.pas`
- `docs/guides/USER_GUIDE.md`
- `tests/scripts/test_isslsessionresumption_generic_examples_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改生产实现
- 不重跑全量 examples gate
- 不重开已经完成的 API reference / API documentation owner-path 批次

## Why This Batch

当前 active docs/tests 的 ordinary guidance 大多已经切到
`ISSLSessionResumption`
owner path。

但普通活跃示例里还有一组 residual：

- 直接 `Conn.SetSession(...)`
- 直接 `Conn.IsSessionReused`
- 直接 `Conn.GetSession`
- 把 session resumption 写成“已完美支持”或“显著提升性能”的固定结论

这会把读者重新带回 compatibility mirrors，也会冲淡前面已经收口好的接口设计方向。

## Planned Changes

1. 新增 focused shell contract，冻结普通活跃示例优先走
   `ISSLSessionResumption`
   owner path。
2. 更新 4 个普通活跃 session 示例：
   - session 获取 / 注入 / reuse 读取都改走 owner path
   - 去掉与当前 source/runtime truth 不符的过强承诺
3. 收紧 `USER_GUIDE.md` 的 generic wording：
   - 不再把 `IsSessionReused=True`
     直接写成“握手更快”

## Verification

```bash
bash -n tests/scripts/test_isslsessionresumption_generic_examples_contract.sh
bash tests/scripts/test_isslsessionresumption_generic_examples_contract.sh

mkdir -p tmp/example_session_reuse_example
fpc -B -Fu./src -Fu./examples \
  -FUtmp/example_session_reuse_example \
  -FEtmp/example_session_reuse_example \
  -otmp/example_session_reuse_example/session_reuse_example \
  examples/session_reuse_example.pas

mkdir -p tmp/example_session_resumption_example
fpc -B -Fu./src -Fu./examples \
  -FUtmp/example_session_resumption_example \
  -FEtmp/example_session_resumption_example \
  -otmp/example_session_resumption_example/session_resumption_example \
  examples/session_resumption_example.pas

mkdir -p tmp/example_https_client_session
fpc -B -Fu./src -Fu./examples \
  -FUtmp/example_https_client_session \
  -FEtmp/example_https_client_session \
  -otmp/example_https_client_session/https_client_session \
  examples/https_client/https_client_session.pas

mkdir -p tmp/example_production_https_client_session
fpc -B -Fu./src -Fu./examples \
  -FUtmp/example_production_https_client_session \
  -FEtmp/example_production_https_client_session \
  -otmp/example_production_https_client_session/https_client_session \
  examples/production/https_client_session.pas

git diff --check
```

