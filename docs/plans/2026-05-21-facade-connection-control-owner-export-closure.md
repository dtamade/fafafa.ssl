# Facade Connection-Control Owner Export Closure

## Goal

修复 `fafafa.ssl` 主门面在 timeout/blocking 迁移主线上的最后一个
public completeness gap：

- `API_REFERENCE` 已经把 runtime owner 指向 `ISSLConnectionControl`
- 但 `uses fafafa.ssl;`
  当前还不能直接声明 / `Supports(...)`
  这条接口

也就是说，
文档已经把调用方导向了新 owner path，
但主门面入口还没有把这个 owner path 真正发布完整。

## Scope

- Update:
  - `src/fafafa.ssl.pas`
  - `docs/reference/API_REFERENCE.md`
  - `tests/contract/test_facade_optional_owner_surface_entry.pas`
  - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开 timeout / blocking runtime owner-path 设计
- 不重开 broader facade slimming / unit reorganization
- 不重跑大门禁，除非 focused proof 暴露了编译层回归

## Why This Batch

当前已经完成：

- `ISSLConnectionControl` source owner-path adoption
- builder / connector / acceptor owner-path-first
- active docs / v2 / audit truth 同步

但主门面 `src/fafafa.ssl.pas`
仍然漏掉：

- `ISSLConnectionControl = fafafa.ssl.base.ISSLConnectionControl;`

这会造成一个非常具体的 compile gap：

```pascal
uses
  fafafa.ssl;

var
  Conn: ISSLConnection;
  Control: ISSLConnectionControl;
begin
  if Supports(Conn, ISSLConnectionControl, Control) then
    Control.SetTimeout(15000);
end;
```

当前 migration docs 已经推荐这种写法，
但 facade-only caller 还得退回 `fafafa.ssl.base`，
所以这是一个真实的 public truth 失配，而不是文档措辞问题。

## Minimal Fix

1. 把现有 facade optional-owner contract 扩展到：
   - `ISSLConnectionControl`
   - facade-only compile proof
2. 在 `src/fafafa.ssl.pas` 补齐：
   - `ISSLConnectionControl = fafafa.ssl.base.ISSLConnectionControl;`
3. 在 `API_REFERENCE` 补一句：
   - 主门面 `fafafa.ssl` 也 re-export 这组 connection-side owner interfaces
4. 跑 focused contract + `git diff --check`

## Verification

```bash
bash -n tests/scripts/test_facade_optional_owner_surface_export_contract.sh
bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh
git diff --check
```

## Expected Outcome

- `uses fafafa.ssl;` 足以访问
  `ISSLConnectionControl`
- timeout migration 主路径
  `Supports(..., ISSLConnectionControl, ...)`
  不再需要 split `uses fafafa.ssl.base`
- facade truth 与当前 migration docs / owner-path docs 重新一致

## Execution Result

- PASS
- focused RED 首轮直接证明：
  - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    失败在
    `ISSLConnectionControl = fafafa.ssl.base.ISSLConnectionControl;`
    缺失
- 最小修复后：
  - `src/fafafa.ssl.pas`
    现已补齐：
    - `ISSLConnectionControl`
  - `docs/reference/API_REFERENCE.md`
    现已明确记录：
    - 主门面 `fafafa.ssl`
      也 re-export
      connection-side owner interfaces
- focused verification：
  - `bash -n tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    - PASS
  - `bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
