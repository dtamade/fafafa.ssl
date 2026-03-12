# API Contract Current Index

这是 fafafa.ssl 当前 API contract 的导航页。

如果你只想知道“现在什么是真的”，优先看三份文档：
- 路线图入口：`docs/plans/2026-03-10-api-canon-and-implementation-roadmap.md`
- API canon：`docs/reference/ARCHITECTURE.md`
- 月度执行真相：`docs/plans/2026-03-current-summary.md`

## How To Use

- 先读 `ARCHITECTURE.md`，理解当前 API canon、主入口和层次结构。
- 再用本页找到某一类 contract 的高信号计划文件。
- 如果需要 RED → GREEN 过程或更细粒度证据，再回到对应 `docs/plans/*.md`。

## Core API Contracts

这些 contract 决定普通业务开发者默认会遇到的用户面行为。

- **主入口与层次**
  - `TSSLContextBuilder` 是唯一推荐主入口
  - `TSSLFactory + TSSLConfig` 仅保留为兼容/底层入口
  - `TSSLConnector` / `TSSLStream` 作为快捷消费层
  - 当前真相：`docs/reference/ARCHITECTURE.md`
  - 入口治理：`docs/reference/API_ENTRYPOINT_GOVERNANCE.md`

- **backend resolution**
  - 单次解析 concrete backend
  - `CreateContext(...)` 与 `CreateCertificateStore(...)` 共享同一 resolved backend
  - 关键计划：
    - `docs/plans/2026-03-09-builder-server-default-backend-store-consistency.md`
    - `docs/plans/2026-03-09-builder-implicit-default-backend-resolution-consistency.md`
    - `docs/plans/2026-03-09-builder-backend-resolution-helper.md`

- **library-scope vs request/context-scope**
  - `library-scope vs request/context-scope` 已显式分离
  - logging/defaults 属于 library scope
  - cert/key/CA 材料属于 request/context scope
  - 关键计划：
    - `docs/plans/2026-03-08-library-create-context-default-config-consistency.md`
    - `docs/plans/2026-03-09-factory-default-config-boundary-followup.md`
    - `docs/plans/2026-03-09-factory-default-config-owner-fields-contract.md`

- **ServerName**
  - 推荐路径：per-connection SNI
  - precedence：`connection override > context default > empty`
  - 关键计划：
    - `docs/plans/2026-03-09-server-name-migration-policy.md`
    - `docs/plans/2026-03-09-context-builder-server-name-context-parity.md`
    - `docs/plans/2026-03-09-connection-builder-hostname-override-precedence.md`

- **file / PEM / PKCS11 precedence**
  - 证书：`certificate_pem > certificate_file`
  - 私钥：`PKCS#11 > private_key_pem > private_key_file`
  - `UsePKCS11(...)` 只替代私钥来源，不替代 server 证书要求
  - 关键计划：
    - `docs/plans/2026-03-10-builder-private-key-pem-precedence-alignment.md`
    - `docs/plans/2026-03-10-builder-certificate-pem-precedence-alignment.md`
    - `docs/plans/2026-03-10-builder-pkcs11-mixed-key-warning-alignment.md`
    - `docs/plans/2026-03-10-builder-pkcs11-certificate-required-doc-truth.md`

## Advanced API Contracts

这些 contract 面向框架作者和高阶调用方，描述 Core API 之外仍需稳定的高级行为。

- **builder advanced option parity**
  - `server_name` / `alpn_protocols` / `session_cache_enabled` 的 option-sync
  - 关键计划：
    - `docs/plans/2026-03-09-builder-override-advanced-option-parity.md`
    - `docs/plans/2026-03-09-builder-import-advanced-option-parity.md`
    - `docs/plans/2026-03-09-builder-advanced-option-empty-value-contract.md`
    - `docs/plans/2026-03-09-builder-merge-advanced-option-snapshot-semantics.md`

- **builder snapshot / import / merge**
  - `private_key_password`
  - backend-selection snapshot/mode normalization
  - string-field empty-value semantics
  - 关键计划：
    - `docs/plans/2026-03-09-builder-private-key-password-snapshot-semantics.md`
    - `docs/plans/2026-03-09-builder-backend-selection-snapshot-semantics.md`
    - `docs/plans/2026-03-09-builder-backend-selection-mode-normalization.md`
    - `docs/plans/2026-03-09-builder-merge-string-field-empty-value-snapshot-semantics.md`

- **PKCS11 / OCSP / cert-verify-cache**
  - PKCS11 JSON / INI parity
  - OCSP stapling override parity
  - cert-verify-cache override parity
  - 关键计划：
    - `docs/plans/2026-03-09-builder-override-pkcs11-json-parity.md`
    - `docs/plans/2026-03-09-builder-override-pkcs11-ini-parity.md`
    - `docs/plans/2026-03-09-builder-override-ocsp-parity.md`
    - `docs/plans/2026-03-09-builder-override-cert-verify-cache-parity.md`

- **capability / fallback / support semantics**
  - capability 字段代表当前 backend runtime 真相
  - unsupported / unavailable / skipped 语义需可发现
  - 当前入口：
    - `docs/reference/API_CAPABILITY_STRATEGY.md`
    - `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
    - `docs/CAPABILITY_MATRIX_GUIDE.md`

- **错误模型 / warning / observability**
  - `TSSLOperationResult` / `TSSLDataResult`
  - `ESSLException` 家族
  - `warning` / `unsupported` / `configuration` 边界
  - 当前入口：
    - `docs/reference/API_ERROR_MODEL.md`
    - `docs/reference/API_CANCELLATION_MODEL.md`
    - `docs/reference/RETURN_TYPE_CONVENTIONS.md`
    - `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`

- **日志与观测**
  - logging scope
  - dry-run/report-surface observability
  - 关键计划：
    - `docs/plans/2026-03-09-request-config-logging-scope-visibleization.md`
    - `docs/plans/2026-03-08-test-reports-output-policy-and-march-summary.md`
    - `docs/plans/2026-03-07-runtime-contracts-current-index.md`

## Backend-Specific Contracts

这些 contract 不是普通业务代码默认入口，但仍是当前设计真相的一部分。

- **OpenSSL**
  - 作为 Linux 现实基线
  - 相关 canonical docs：
    - `docs/reference/OPENSSL_MODULES.md`
    - `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`

- **WinSSL**
  - 作为 Windows 现实基线
  - 当前 contract 重点仍在平台边界与 summary/report surface

- **WolfSSL shim policy**
  - standalone connection 单元是兼容桥接层，不是第二套 runtime
  - 关键计划：
    - `docs/plans/2026-03-09-wolfssl-standalone-shim-policy.md`

- **纯 Pascal**
  - `纯 Pascal` 后端是战略重点，不是教学样例
  - 目标角色：无原生依赖的可移植后端
  - 第一里程碑：`HTTPS/TLS 客户端生产可用`
  - 当前总路线图：
    - `docs/plans/2026-03-10-api-canon-and-implementation-roadmap.md`
  - 当前 M1 checklist：
    - `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`

## Historical Notes

- 本页只列当前 contract 入口，不承担完整历史记录职责。
- 需要 RED → GREEN 历史时，回到对应 `docs/plans/*.md`。
- 大量 2026-03 单波次计划文件仍保留，但应优先通过本页进入，而不是逐个盲扫。
