# Repository Guidelines

## 项目结构与模块组织
- `src/` 存放全部 Pascal 单元（`fafafa.ssl.*`），按提供方（`openssl`、`winssl`）与共享抽象分层；新增单元时需延续现有命名并按后端归类。
- `tests/` 保存自动化套件：`unit/` 进行模块级验证，`integration/` 覆盖端到端场景，`performance/` 聚焦性能，`bin/` 存放构建产物；新增夹具应与目标功能相邻。
- `examples/` 提供文档配套示例程序；扩展核心 API 时同步更新相关示例。
- `scripts/` 收录 PowerShell 维护脚本（模块校验、`uses` 归并等）；`build/` 与 `out/` 仅用于临时输出，禁止将新增目录纳入版本控制。
- `docs/` 汇总阶段报告与技术说明；功能成熟后请在此编写深入文档，而非直接修改根目录 README。

## 构建、测试与开发命令
- `lazbuild --build-mode=Release fafafa_ssl.lpk` 构建 Lazarus 包；调试阶段可改用 `--build-mode=Debug`。
- `fpc -Fu%PROJECT_ROOT%\src -Fu%PROJECT_ROOT%\src\openssl your_program.pas` 编译临时诊断程序；若依赖特定后端，请追加对应 `-Fu` 路径。
- `powershell -ExecutionPolicy Bypass -File run_all_tests.ps1` 运行端到端回归并刷新 `tests\bin` 内的摘要。
- `pwsh scripts\validate_all_modules.ps1` 在提交前校验 `uses` 列表与接口导出是否一致。

## 代码风格与命名规范
- 统一使用 `{$mode objfpc}{$H+}` 与两个空格缩进；避免制表符，确保 `begin`/`end` 垂直对齐。
- 类型命名采用 PascalCase，并为类/记录添加 `T` 前缀（如 `TOpenSSLVersion`），接口以 `I` 开头，常量使用全大写加下划线，过程/函数保持 PascalCase 动词。
- 单元命名遵循 `fafafa.ssl.<层级>.<功能>`；后端适配层分别位于 `fafafa.ssl.openssl.*` 与 `fafafa.ssl.winssl.*`。
- `uses` 子句按标准库、共享抽象、后端单元的顺序分组，各组之间留出空行。

## 测试指南
- 优先使用 PowerShell 驱动脚本 (`run_all_tests.ps1`) 执行全量测试；脚本会在 `tests\bin` 生成可执行文件并输出汇总结果。
- 单模块测试遵循 `test_<领域>.pas` 命名；新建测试可复制 `test_openssl_1_1_compatibility.pas` 作为模板。
- 涉及多层交互的诊断用例放入 `tests\integration`，后端特有场景集中在 `tests\unit\lib`。
- 失败日志需保持精炼，缺失依赖库时应在初始化阶段安全退出。

## 提交与合并请求规范
- 遵循 Conventional Commits（如 `feat:`、`fix:`、`docs:`），范围标签需匹配受影响的子系统（示例：`feat(winssl): ...`）。
- 每个 Pull Request 须概述功能改动、列出受影响的单元与测试、附上 `run_all_tests.ps1` 输出，并关联相关 issue 或 `docs/` 设计记录。
- 当行为发生变化时，请同步更新对应文档（`docs/` 或 `examples/`），确保使用者掌握最新信息。

## 安全与配置提示
- 禁止提交 OpenSSL 库文件或私钥；依赖系统安装并在文档中说明所需路径。
- 在新增代码中使用 `LoadOpenSSLCoreWithVersion` 验证目标版本，并通过显式能力探测处理平台特性。
- 针对环境差异请使用本地 Lazarus/FPC 配置文件，不要将个性化设置写入仓库源代码。
