# 2026-05-21 security-best-practices 当前 public import 真相对齐

## Goal

修复 `docs/guides/security-best-practices.md`
里 OCSP stapling
那段 active
builder 示例
仍在使用
`fafafa.ssl.base`
导入的问题，
让这份安全最佳实践指南继续保留：

- 当前真实
  builder fluent
  surface
- client stapled-response
  request/required
  边界
- server-side
  stapled-response
  caller-provided
  叙事

但不再偏离
当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-security-best-practices-current-public-import-truth.md`
- Update:
  - `docs/guides/security-best-practices.md`
  - `tests/scripts/test_active_builder_guides_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改安全语义
- 不改 OCSP / early-data / session-cache 运行时实现
- 不重开更大范围的安全文档重写

## Architecture Truth

- `TSSLContextBuilder`
  当前来自：
  - `fafafa.ssl.context.builder`
- `ISSLContext`
  /
  `ISSLOCSPStapling`
  当前都可直接来自：
  - `fafafa.ssl`
- 因此：
  - active builder
    OCSP 示例
    当前不应再导入：
    - `fafafa.ssl.base`
- 这份文档
  更关键的真相
  仍然是：
  - builder fluent
    高入口
  - client request
    /
    required
    行为边界
  - server-side
    caller-provided
    OCSP stapled
    response

## Steps

1. 收紧现有
   `tests/scripts/test_active_builder_guides_truth_contract.sh`：
   - 继续冻结
     security-best-practices
     的 current
     builder surface
   - 新增冻结：
     - active builder
       示例
       不得继续出现：
       - `fafafa.ssl.base`
2. 用 `HEAD`
   guide snapshot
   跑同一条合同，
   先拿到 RED。
3. 最小修改 `security-best-practices.md`
   的 OCSP
   示例导入。
4. 重跑 focused contract
   与
   `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_active_builder_guides_truth_contract.sh
SECURITY_BEST_PRACTICES_DOC=/tmp/fafafa_ssl_security_best_practices_head.md bash tests/scripts/test_active_builder_guides_truth_contract.sh
bash tests/scripts/test_active_builder_guides_truth_contract.sh
git diff --check
```

## Expected Result

- `security-best-practices`
  不再继续教学
  `fafafa.ssl.base`
- 安全最佳实践文档
  继续保留
  current builder
  / OCSP
  语义真相

## Execution Result

- PASS
- focused contract
  先补齐了：
  - `SECURITY_BEST_PRACTICES_DOC`
    覆盖入口，
    允许同一条
    focused contract
    对
    `HEAD`
    旧版 guide
    做 RED
- focused RED
  通过
  `HEAD`
  snapshot
  真实暴露：
  - OCSP stapling
    那段 active
    builder 示例
    仍在使用
    `fafafa.ssl.base`
  - 合同输出：
    - `security best practices guide must stop teaching fafafa.ssl.base in active builder examples`
- 最小修复后：
  - active OCSP
    builder 示例
    已统一回到：
    - `fafafa.ssl`
  - 当前
    builder/OCSP
    语义边界
    全部保留
- focused verification：
  - `bash -n tests/scripts/test_active_builder_guides_truth_contract.sh`
    - PASS
  - `SECURITY_BEST_PRACTICES_DOC=/tmp/fafafa_ssl_security_best_practices_head.md bash tests/scripts/test_active_builder_guides_truth_contract.sh`
    - FAIL
  - `bash tests/scripts/test_active_builder_guides_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
