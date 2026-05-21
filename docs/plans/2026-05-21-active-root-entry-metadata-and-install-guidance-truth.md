# Active Root Entry Metadata And Install Guidance Truth

## Goal

收口当前最外层入口
还在发布旧 truth
的两类残差：

- `examples/`
  根目录里
  已经失效的
  `test_winssl.lpi`
  /
  `test_openssl.lpi`
- 普通安装 / 编译入口文档
  仍在教学
  `uses fafafa.ssl, fafafa.ssl.base`

让调用方从
`README.md`
与中文安装/编译入口
学到当前真实入口：

- 普通 public surface
  优先来自
  `fafafa.ssl`
- `fafafa.ssl.base`
  是 source truth /
  supporting-type owner，
  不是普通安装示例
  的默认导入
- `examples/`
  根目录不再保留
  指向缺失主文件的
  失效 Lazarus 项目入口

## Scope

- Delete:
  - `examples/test_winssl.lpi`
  - `examples/test_openssl.lpi`
- Update:
  - `README.md`
  - `docs/zh/安装配置.md`
  - `docs/zh/编译指南.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-active-root-entry-metadata-and-install-guidance-truth.md`
  - `tests/scripts/test_active_root_entry_metadata_and_install_guidance_truth_contract.sh`

不做：

- 不改
  `tests/examples/*`
  canonical test project
- 不重写
  2025 年历史测试报告
- 不扩大到
  `WINSSL_QUICKSTART`
  /
  `LINUX_QUICKSTART`
  这类当前未证明有误导的
  tree snippet

## Why This Batch

当前扫描确认：

- `examples/test_winssl.lpi`
  与
  `examples/test_openssl.lpi`
  的主文件
  `test_winssl.pas`
  /
  `test_openssl.pas`
  在
  `examples/`
  根目录并不存在
- 仓库里真正存在的
  canonical test project
  位于：
  - `tests/examples/test_winssl.lpi`
  - `tests/examples/test_openssl.lpi`
- `README.md`
  里的源码树
  仍把
  `fafafa.ssl.factory.pas`
  /
  `fafafa.ssl.base.pas`
  放在最显眼入口位置，
  却没有把
  `fafafa.ssl.pas`
  标成当前普通入口
- `docs/zh/安装配置.md`
  /
  `docs/zh/编译指南.md`
  仍直接教学：
  `uses fafafa.ssl, fafafa.ssl.base`

这说明当前残差
已经不是 runtime
或 backend 缺口，
而是最外层入口
还在继续发布
失效 metadata
和旧 public-entry guidance。

## Minimal Fix

1. 先加 focused contract，
   锁定根目录 metadata
   与安装 guidance truth
2. 删除
   `examples/`
   根目录里
   两个失效 `.lpi`
3. 把
   `README.md`
   的架构入口
   收回到
   `fafafa.ssl`
   为主的当前 truth
4. 把中文安装 / 编译入口
   的普通 `uses`
   改回
   `fafafa.ssl`
5. 跑 focused contract
   与
   `git diff --check`

## Verification

```bash
bash -n tests/scripts/test_active_root_entry_metadata_and_install_guidance_truth_contract.sh
bash tests/scripts/test_active_root_entry_metadata_and_install_guidance_truth_contract.sh
git diff --check
```

## Expected Outcome

- `examples/`
  根目录不再保留
  指向缺失主文件的
  失效 `.lpi`
- `README.md`
  明确发布：
  - `fafafa.ssl.pas`
    是当前普通入口
  - `fafafa.ssl.context.builder.pas`
    是推荐 builder 入口
  - `fafafa.ssl.factory.pas`
    /
    `fafafa.ssl.base.pas`
    是更底层的
    factory / source-truth
    owner
- 中文安装 / 编译入口
  不再继续教学
  普通调用方
  split import

## Execution Result

- PASS
- focused contract history:
  - `bash -n tests/scripts/test_active_root_entry_metadata_and_install_guidance_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_active_root_entry_metadata_and_install_guidance_truth_contract.sh`
    - RED -> PASS
    - initial RED:
      - `examples/test_winssl.lpi`
        still existed as
        a stale root
        example project
- root metadata closeout:
  - retired
    `examples/test_winssl.lpi`
  - retired
    `examples/test_openssl.lpi`
- doc guidance closeout:
  - `README.md`
    now publishes
    `fafafa.ssl.pas`
    as the ordinary
    public entry
  - `docs/zh/安装配置.md`
    now teaches
    ordinary
    `uses fafafa.ssl;`
  - `docs/zh/编译指南.md`
    now keeps
    `ProtocolVersionToString`
    on
    `fafafa.ssl`
- `git diff --check`
  - PASS
