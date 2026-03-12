# Legacy Class-Only Registration Governance (2026-03-09)

## Goal
- 清掉仓库内剩余的 class-only `RegisterLibrary(...)` 调用，让 repo-local registration 全部显式传入 `ALibraryFactory`。
- 给 `TSSLFactory.RegisterLibrary` 增加兼容治理信号：保留 class-only fallback，但明确标成 deprecated。
- 用 focused contract 锁定“仓库内不再新增 class-only registration”这条规则。

## Scope
- `src/fafafa.ssl.factory.pas`
- `tests/test_factory.pas`
- `tests/test_context_builder_backend_store_consistency.pas`
- `tests/scripts/test_register_library_explicit_factory_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
1. 把 `RegisterLibrary(...)` 拆成两个 overload：
   - 兼容 overload：只收 `ALibraryClass`，标记 deprecated，并内部转发到 5 参数版本
   - 推荐 overload：显式要求 `ALibraryFactory`
2. 仓库内剩余 repo-local call site 全部改成显式 `@Create...` 工厂函数。
3. 新增 shell contract，扫描 `src/` 与 `tests/` 下的 `TSSLFactory.RegisterLibrary(...)` 调用，要求 repo-local call site 一律是 5 参数显式 factory 形式。

## RED
1. 新增 `tests/scripts/test_register_library_explicit_factory_contract.sh`
2. RED run:
   - `bash tests/scripts/test_register_library_explicit_factory_contract.sh`
3. 预期失败点：
   - `tests/test_context_builder_backend_store_consistency.pas`
   - `tests/test_factory.pas`
   - 这些文件里仍有 class-only `RegisterLibrary(...)` 调用

## GREEN
1. 修改 `src/fafafa.ssl.factory.pas`
   - 增加 deprecated 4 参数 overload，作为兼容 wrapper
   - 保留 5 参数 overload 作为推荐路径
   - 在接口注释里明确 `ALibraryFactory` 是首选
2. 修改 repo-local remaining call sites
   - `tests/test_factory.pas`
   - `tests/test_context_builder_backend_store_consistency.pas`
3. 让 contract 变绿
   - `tests/scripts/test_register_library_explicit_factory_contract.sh`

## Verification
- `bash -n tests/scripts/test_register_library_explicit_factory_contract.sh`
- `bash tests/scripts/test_register_library_explicit_factory_contract.sh`
- `fpc -Fu./src -otmp/test_factory tests/test_factory.pas && ./tmp/test_factory`
- `fpc -Fu./src -otmp/test_context_builder_backend_store_consistency tests/test_context_builder_backend_store_consistency.pas && ./tmp/test_context_builder_backend_store_consistency`
- `fpc -Fu./src -otmp/test_factory_backend_default_config_initialization tests/test_factory_backend_default_config_initialization.pas && ./tmp/test_factory_backend_default_config_initialization`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- src/fafafa.ssl.factory.pas tests/test_factory.pas tests/test_context_builder_backend_store_consistency.pas tests/scripts/test_register_library_explicit_factory_contract.sh docs/plans/2026-03-09-legacy-class-only-registration-governance.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md`

## Notes
- 这波之后，仓库内 repo-local registration 已全部切到显式 factory；class-only path 只剩对第三方/遗留代码的兼容入口。
- 若后续要进一步收紧，可以考虑为 backend author 文档单独补一段“何时必须传 `ALibraryFactory`”。
