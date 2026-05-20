# API Documentation ISSLConnection Slice And OCSP Mirror Classification

## Goal

收掉 `docs/reference/API_DOCUMENTATION.md` 里最后一层仍会误导读者的 section-level 语义漂移：

- `### ISSLConnection` 小节还像在描述完整 shipped source truth
- `GetOCSP*` 这组条目虽然示例已走 `ISSLOCSPStapling` owner path，但 section 级语义仍容易被理解成核心主接口

本批目标是把 active reference 入口和 `docs/reference/API_REFERENCE.md` / owner-path truth 重新说成一张图，避免后续会话再从这一页把旧心智拉起来。

## Scope

只处理 active docs、focused contract 与台账：

- `docs/reference/API_DOCUMENTATION.md`
- `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
- `tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/` 下 public signature
- 不改 backend runtime / capability 实现
- 不重跑重型 compile-all / CI gate

## Architecture Truth

- `API_DOCUMENTATION` 的 `ISSLConnection` 小节应该被归类为：
  - 当前常用连接方法切片
  - 不是 `v1.5.0` shipped source 的完整逐行镜像
- 完整 shipped source truth 应该回到：
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
- `Connection.GetOCSP*` 目前仍存在于 shipped source，但语义是：
  - `ISSLConnection` 上的 compatibility-core mirrors
  - 新代码优先通过 `ISSLOCSPStapling` owner surface 访问

## Planned Changes

1. 先补 focused contract：
   - 要求 `API_DOCUMENTATION` 明确声明 `ISSLConnection` 小节只是 slice，不是 full truth
   - 要求同页明确把 full truth 路由回 `API_REFERENCE`
   - 要求 `GetOCSP*` 条目被标记成 compatibility-core mirrors，新代码优先 `ISSLOCSPStapling`
2. 运行 focused contract，拿到预期 RED。
3. 最小修正文档，不扩写到其它 guide/reference。
4. 同步台账并提交。

## Verification

```bash
bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash -n tests/scripts/test_isslocspstapling_active_guidance_contract.sh
bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh
git diff --check
```

## Expected Outcome

- `API_DOCUMENTATION` 不再把 `ISSLConnection` section 发布成完整 source truth
- `GetOCSP*` 在 active reference 里被明确分类为 compatibility-core mirrors
- 新读者看到的是一条一致路线：
  - active docs 的常用切片
  - optional owner interfaces 的推荐访问路径
  - `API_REFERENCE` 的完整 shipped truth

## Result

- `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  先新增：
  - `ISSLConnection` section 是 slice 不是 full truth
  - full truth 回到 `docs/reference/API_REFERENCE.md`
  - `GetOCSP*` 在 section 级被标记为 compatibility-core mirrors
- `tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  先新增：
  - `GetOCSP*` compatibility-mirror section wording
  - owner-first section guidance
- 预期 RED 已捕获：
  - `API_DOCUMENTATION ISSLConnection section must classify itself as a current slice instead of full shipped truth`
  - `API documentation missing ISSLOCSPStapling-first guidance: 下面这组 \`GetOCSP*\` 条目之所以仍保留在 \`ISSLConnection\` 小节，是因为当前 shipped source 仍向后兼容这些 compatibility-core mirrors。`
- `docs/reference/API_DOCUMENTATION.md`
  现已明确：
  - `ISSLConnection` 小节只是当前常用连接方法切片
  - 完整 shipped truth 需要回到 `docs/reference/API_REFERENCE.md`
  - `GetOCSP*` 在这一层只是 compatibility-core mirrors
  - 新代码优先通过 `ISSLOCSPStapling` 访问 stapling state / response / verify status / status string

## Route Impact

- 这批把 `README` / `ARCHITECTURE` / `API_DOCUMENTATION` 三个高可见入口重新收成同一套 `ISSLConnection` 语义：
  - slice 入口
  - optional owner path
  - `API_REFERENCE` full truth
- `ISSLOCSPStapling` 这组能力不再只是在示例层 owner-first，而是连 section 级分类也与源码和 canonical reference 对齐
- 默认下一批不该再重复清这条 `ISSLConnection + GetOCSP*` active reference 语义线，而应继续寻找尚未被 contract 守住的其它 active slice / owner-path 漂移
