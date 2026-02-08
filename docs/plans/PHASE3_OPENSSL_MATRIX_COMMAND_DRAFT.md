# Phase 3 Linux OpenSSL 矩阵命令草案（Draft）

**目标**：在 Linux 下形成 OpenSSL 1.1.1 / 3.x 的可执行验证命令矩阵。  
**阶段**：Batch B10

---

## 1. 脚本入口

- `scripts/run_linux_openssl_matrix_draft.sh`

该脚本按 profile 执行同一套门禁链路：
1. `python3 scripts/compile_all_modules.py`（可选）
2. `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`
3. `bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50`（可选）

---

## 2. 默认 Profile

- `system-default`：不指定 `LD_LIBRARY_PATH`，使用系统默认动态库。
- `openssl3`：若检测到 `libcrypto.so.3`，则注入对应 `LD_LIBRARY_PATH`。
- `openssl111`：若检测到 `libcrypto.so.1.1`，则注入对应 `LD_LIBRARY_PATH`。

---

## 3. 常用命令

```bash
# 仅检查命令链（推荐先跑）
bash scripts/run_linux_openssl_matrix_draft.sh --dry-run

# 指定 1.1.1 / 3.x 库目录并执行
bash scripts/run_linux_openssl_matrix_draft.sh \
  --openssl111-lib-dir /opt/openssl-1.1/lib \
  --openssl3-lib-dir /opt/openssl-3/lib

# 跳过 compile，聚焦模块回归
bash scripts/run_linux_openssl_matrix_draft.sh --skip-compile --verbose
```

---

## 4. 验收口径（Draft）

- 至少 `system-default` profile 可跑通。
- 若主机安装了 1.1.1 / 3.x，对应 profile 不应报“库缺失”。
- 模块回归需满足：`failed: 0`。

---

## 5. 与 Phase 3 的关系

- 本草案解决“Linux OpenSSL 版本矩阵命令化”问题。
- 后续 B11/B12 将补齐：
  - CI 产物归档策略；
  - 跨平台门禁分层策略（Linux/macOS/Windows）。
