# Findings - CI Runtime Gate Repair

## 2026-05-15

- GitHub Actions 账户额度不再是当前 blocker：
  - 仓库公开后，远端 workflows 已经真正执行
  - 旧的 “billing/quota startup failure” 叙述已经过时，不能继续作为当前停滞原因

- CI run `25893971783`（head `2eb563f`）的真实失败点是 completeness job 依赖缺口：
  - `Minimal Gate (Linux)` PASS
  - `Code Quality (Light)` PASS
  - `FreePascal TLS 1.3 Completeness` FAIL
  - failed log 明确落在 `WolfSSL KnownIssues 运行时对齐测试`
  - 关键错误：`Failed to load WolfSSL library: libwolfssl.so`
  - 结论：`.github/workflows/ci.yml` 的 completeness job 缺 `libwolfssl-dev`

- 同一依赖缺口也存在 release workflow：
  - `release.yml` / `release.yml.disabled` 也会调用 `run_freepascal_tls13_completeness_gate.sh`
  - 如果不一起补，未来 tag/release 路线会重现同样的 WolfSSL runtime 失败

- TLS13 signer workflow run `25901035350`（head `2eb563f`）有两个独立问题：
  - `Run TLS13 signer gate bundle` 失败：
    - 内层 `signer_gate_ci` exit=1
    - bundle 最终 `overall_state=ATTENTION`
  - `Append step summary` 失败：
    - `.github/workflows/tls13-signer-gate.yml` 的 here-doc terminator `PY` 多缩进了 2 个空格
    - shell 实际报 `warning: here-document ... wanted 'PY'` 和 `syntax error: unexpected end of file`

- `scripts/run_freepascal_tls13_servercertverify_bench.sh` 的主问题在 bench 构建层，而不是 signer 实现层：
  - 编译时强加 `-Criot`，会在 `src/fafafa.ssl.crypto.hash.pas` 触发 compile-time constant range-check errors
  - 编译输出被重定向到 `/dev/null`，把真实错误吞掉，导致远端只看到“bench step exit=1”
  - 去掉该旗标后，本地 bench 可以稳定产出指标

- 修复后本地验证已经闭环：
  - focused contract tests PASS
  - `run_freepascal_tls13_servercertverify_bench.sh` PASS
  - `run_tls13_signer_gate_ci.sh` PASS（run_id=`20260515_131250`）
  - `run_tls13_signer_gate_bundle.sh --strict` PASS（run_id=`local_bundle_repair_20260515`）

- 首次修复推送 `d3ebeee` 后，远端真相又进一步收敛：
  - signer run `25901775672`
    - `Run TLS13 signer gate bundle` 已经 PASS
    - `Append step summary` 仍 FAIL
    - 新错误不再是 shell EOF，而是 Python heredoc 正文带 2 个前导空格导致 `IndentationError`
  - CI run `25901775676`
    - completeness job 仍 FAIL
    - 关键错误仍是 `Failed to load WolfSSL library: libwolfssl.so`
    - 这说明“只补 workflow apt 依赖”还不够，WolfSSL loader 本身也需要 fallback

- 因此第二批根因修法应分两条落地：
  - workflow 层：
    - signer summary 的 heredoc terminator 与 Python body 都必须输出成真正可执行的脚本
  - Pascal runtime 层：
    - `src/fafafa.ssl.wolfssl.api.pas` 不能只赌 `LoadLibrary('libwolfssl.so')`
    - 在 Linux 上需要回退扫描常见系统库目录与版本化 `libwolfssl.so*`

- Windows/WinSSL 仍保持 `static-only`：
  - 用户明确不要走 Windows 条件
  - 这批只处理 Linux / GitHub Actions 可直接复核的 CI/runtime blocker

- 第二次推送 `18f154f` 后，远端状态继续收敛：
  - signer run `25902255923`（head `18f154f`）已经 SUCCESS
  - CI run `25902255941` 仍 FAIL，且仍落在 `WolfSSL KnownIssues 运行时对齐测试`

- 但这次不能再把 `25902255941` 直接归因为 “loader fallback 仍未生效”：
  - 当前仓库里的 `.github/workflows/ci.yml` 真实内容显示：
    - `Minimal Gate (Linux)` 的 install step 包含 `libwolfssl-dev`
    - `freepascal-tls13-completeness` 的 install step 却仍是 `fpc libssl-dev python3`
  - 旧版 `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` 只对整份 `ci.yml` 做 `grep -Fq libwolfssl-dev`
  - 这会产生假绿：只要别的 job 装了 `libwolfssl-dev`，contract 就会放行，即使 completeness job 自己没装

- 因此第三批修复的真实根因是“workflow + contract 双重盲区”：
  - workflow 漏装：completeness job 确实缺 `libwolfssl-dev`
  - contract 漏检：没有把断言限定在 `freepascal-tls13-completeness` job 的 install step

- 本地长跑 `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id local_ci_runtime_repair_20260515` 已 PASS：
  - FreePascal / WolfSSL / MbedTLS KnownIssues 运行时对齐全部通过
  - 这至少证明：在“依赖存在”的前提下，当前 loader fallback 与 completeness 脚本链路是可工作的

- 第三次推送 `8d052dd` 后，远端 CI run `25902644127` 的真相继续收敛：
  - `WolfSSL KnownIssues 运行时对齐测试` 已经 PASS
  - 新的唯一失败点变成 `MbedTLS KnownIssues 运行时对齐测试`
  - 关键错误：`Failed to initialize MbedTLS library (LastError=-1, Details=Failed to load MbedTLS libraries)`

- 这说明当前主问题不再是 WolfSSL loader：
  - 远端 runner 已经真正装到了 `libwolfssl-dev` 并成功跑过 WolfSSL runtime alignment
  - 当前 completeness lane 下一层真实依赖缺口是 `libmbedtls-dev`

- release 路线必须和 completeness lane 同步补上 `libmbedtls-dev`：
  - `release.yml` / `release.yml.disabled` 同样会执行 `run_freepascal_tls13_completeness_gate.sh`
  - 如果只修 CI，不修 release，未来 tag/release 会原样复现 MbedTLS 缺库红灯

- workflow 结构重复会让“看似正确的补丁”打偏到相邻 job：
  - `ci.yml` 里两个 install step 文本高度相似
  - 这批第一次补 `libmbedtls-dev` 时误命中了 `Minimal Gate (Linux)` 而不是 completeness job
  - 新的 job-local contract 立即把这个误命中抓了出来，说明当前 contract 粒度是有效的

- 最新远端 CI run `25902932655`（head `30467e4`）说明依赖缺口已经补齐：
  - `FreePascal KnownIssues runtime alignment` PASS
  - `WolfSSL KnownIssues runtime alignment` PASS
  - `MbedTLS KnownIssues runtime alignment` PASS
  - 失败发生在打印 `所有测试完成！` 之后，而不是任何一个测试主体内部

- 因此当前最可信的根因不是新的 capability/runtime 回归，而是进程退出期清理：
  - `src/fafafa.ssl.mbedtls.lib.pas` / `src/fafafa.ssl.wolfssl.lib.pas` 的 `finalization` 会调用 `Unregister...Backend`
  - 这会走到 `src/fafafa.ssl.factory.pas` 的 `TSSLFactory.UnregisterLibrary -> ReleaseLibrary -> ISSLLibrary.Finalize`
  - 在 GitHub runner 的退出期再次触发 backend `Finalize/Unload`，高度可疑会导致双清理或失序清理下的 `EAccessViolation`

- 这批修法选择的是最小 shutdown-safe 路径，而不是扩大正常运行时语义：
  - `TSSLFactory` 新增 `UnregisterLibraryForProcessShutdown`
  - `TMbedTLSLibrary` / `TWolfSSLLibrary` 新增 `GSkipFinalizeOnDestroy`
  - backend unit 的 `finalization` 改走 shutdown-safe 注销，仅移除工厂持有引用与注册项，避免在进程退出期再进入 backend `Finalize`

- 本地验证支持这条修法，但还不等于远端已证实：
  - `python3 scripts/compile_all_modules.py` 185/185 编译成功
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id local_shutdown_unregister_20260515` PASS
  - 本地 long-run gate 在打印 `所有测试完成！` 后未再出现退出期异常
  - 是否彻底消除 GitHub runner 上的 `EAccessViolation` 仍需第五次 push 复核

- 第五次提交 `45dabb4` 后，远端 CI run `25903921296` 已经证明 shutdown-time 崩溃真正消失：
  - `FreePascal TLS 1.3 Completeness` SUCCESS
  - `Minimal Gate (Linux)` SUCCESS
  - `Code Quality (Light)` SUCCESS

- 运行时主阻塞解除后，当前最高价值的小批次变成 workflow hygiene：
  - GitHub Actions 远端日志提示 `actions/checkout@v4` 仍运行在 Node20 兼容层，并带来 deprecation annotation
  - 仓库内不只活跃 workflow，多个 dormant template 也仍保留 `actions/checkout@v3/v4`
  - 如果只修活跃 workflow，未来重启旧模板时还会把同类告警重新带回仓库

- 这批选择 `actions/checkout@v5` 而不是更激进地跳到 `v6`：
  - `v5` 已明确切到 Node24 runtime，足以消除当前 deprecation 来源
  - 对现有 workflow 语义变化最小，适合做低风险收口
  - `v6` 还带有凭据持久化路径变化，当前批次没有必要顺手扩大

- 新增 focused contract 后，checkout 升级可以被持续守护：
  - `tests/scripts/test_workflow_checkout_node24_contract.sh` 会拒绝 `.github/workflows` 下残留的 `actions/checkout@v3/v4`
  - 同时强制当前活跃 workflow 与需同步模板显式使用 `actions/checkout@v5`

- 第六次提交 `d56637f` 后，远端 runs 说明 checkout 方向是对的，但 workflow hygiene 还没完全结束：
  - `TLS13 Signer Gate` run `25904745243` SUCCESS
  - `CI` run `25904745247` SUCCESS
  - 新 annotation 不再指向 `actions/checkout@v4`
  - 但 signer run 新暴露出 `actions/upload-artifact@v4` 仍运行在 Node20 兼容层

- 这意味着当前最优策略不是停下，而是顺着同一条 hygiene 主线继续收第二层：
  - 先用 focused contract 抓出 `.github/workflows` 里残留的 `actions/upload-artifact@v4`
  - 再统一升级 workflow / template，避免未来启用旧模板时重新带回 artifact 侧告警

- 这批选择 `actions/upload-artifact@v6` 而不是 `v5`：
  - `v6` 已默认切到 Node24 runtime
  - `v5` 需要额外启用 Node24 切换，不适合作为仓库默认基线
  - 因此仓库级卫生标准直接锁到 `v6` 更稳妥

- 新增 artifact contract 后，workflow runtime hygiene 扩展成双合同面：
  - `tests/scripts/test_workflow_checkout_node24_contract.sh` 负责 checkout
  - `tests/scripts/test_workflow_upload_artifact_node24_contract.sh` 负责 artifact
  - 两者组合后，当前活跃 workflow 与同步模板都能被持续守护在 Node24 默认线

- 重新审查 workflow hygiene 路线后，旧的 `download-artifact@v5` 目标已被今天的官方 release 真相推翻：
  - 截至 `2026-05-16`，`actions/download-artifact` 最新 release 是 `v8.0.1`
  - 官方 `v7.0.0` release 明确写明：这是默认跑在 `node24` 的第一条主线
  - 旧计划里的 `v5` 不仅不是最新，也不是 `download-artifact` 的 Node24 默认线，继续照它执行会把错误基线固化进仓库

- 对这个仓库来说，`download-artifact` 最小安全修法应锁到 `v7` 而不是直接跳 `v8`：
  - `v7` 已经满足 Node24 默认 runtime
  - `v8` 还引入了 ESM、digest mismatch 默认报错、非 zip 下载解压策略变化
  - 当前 workflow 只需要收 Node20 弃用源，不需要顺手扩大 artifact 语义变化

- 这批暴露出的流程问题不是代码问题，而是验证路径问题：
  - 当前仓库里实际使用 `actions/download-artifact` 的活跃 workflow 是 `wave-b-b2-manual.yml`
  - 它是 `workflow_dispatch`，不会随着 push 自动运行
  - 其他残留点都在 `.disabled` 模板
  - 因此拿 `CI` / `TLS13 Signer Gate` 的绿灯来证明 `download-artifact` 升级是错误的验证代理

- 新增 `tests/scripts/test_workflow_download_artifact_node24_contract.sh` 后，第三波 hygiene 的静态边界终于完整：
  - 它会拒绝 `.github/workflows` 下残留的 `actions/download-artifact@v3` 到 `@v6`
  - 同时强制当前活跃 manual workflow 与相关 dormant templates 使用 `actions/download-artifact@v7`

- 第八批结束后，`task_plan.md` 再次暴露出一个流程问题：
  - 代码、push、远端 CI 都已经完成
  - 但计划文件仍停在 “准备第八次 commit/push”
  - 这说明我们的 workflow hygiene 收口除了修代码，还要把 planning files 当成真实交付物；否则后续 continuation 会被过期 queue 误导

- 继续核对剩余 action 家族后，当前仍有直接升级路径的 Node20 风险只剩三类：
  - `softprops/action-gh-release@v2`
  - `actions/setup-python@v5`
  - `actions/cache@v4`
  - 它们都不是“看起来旧但其实安全”的假问题

- `softprops/action-gh-release` 的官方真相非常直接：
  - `v2` 的 `action.yml` 仍是 `runs.using: "node20"`
  - 官方 `v3.0.0` release note 明确写明：runtime 从 Node20 迁到 Node24
  - 因此 `release.yml` / `release.yml.disabled` 里的 `@v2` 是活跃 release 路径上的真实 hygiene 缺口

- `actions/setup-python` 与 `actions/cache` 也都存在明确的 Node24 默认线：
  - `actions/setup-python@v6.0.0` release note 明确写明 upgrade to node 24，且 `action.yml` 为 `node24`
  - `actions/cache@v5.0.0` release note 明确写明 `@v5` runs on Node.js 24，而 `v4.3.0` 的 `action.yml` 仍是 `node20`
  - 因此 `code-quality.yml.disabled` 里的 `setup-python@v5` 与 dormant Windows workflows 里的 `cache@v4` 都应继续收掉

- 这批和 `download-artifact` 不同，验证边界更简单：
  - `softprops/action-gh-release@v2` 命中活跃 `release.yml`
  - `setup-python@v5` 与 `cache@v4` 虽在 dormant workflow，但都有明确官方 Node24 后继线
  - 所以它们不需要先等远端 annotation 点名，再做被动修复

- `gcarreno/setup-lazarus@v3.4.1` 是当前唯一不能被我们直接收掉的剩余 Node20 项：
  - 上游最新观察到的 release 仍在 `v3.4.1`
  - 其 `action.yml` 仍声明 `runs.using: 'node20'`
  - 当前未发现 `v4` 或其它官方 Node24 线
  - 这应被记录为“上游阻塞”，而不是继续在本仓库里盲目改版本号
