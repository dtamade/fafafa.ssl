# Phase 3 macOS OpenSSL 路径校验命令草案（Draft）

**目标**：为 macOS 明确 OpenSSL 3 路径探测与环境注入命令，降低 Apple Silicon / Intel 环境差异导致的 CI 漂移。  
**阶段**：Batch B13

---

## 1. 脚本入口

- `scripts/run_macos_openssl_path_check_draft.sh`

该脚本聚焦以下检查链路：

1. OpenSSL 根路径探测（默认候选：`/opt/homebrew/opt/openssl@3`、`/usr/local/opt/openssl@3`）。
2. 校验 `libcrypto.dylib` / `libssl.dylib` / `ssl.h` 是否存在。
3. 注入 `OPENSSL_ROOT` / `DYLD_LIBRARY_PATH` / `PKG_CONFIG_PATH` / `PATH`。
4. 执行模块回归入口（可选）与 Phase2 baseline dry-run（可选）。

---

## 2. 常用命令

```bash
# 建议先跑 dry-run（即使在非 macOS 也可联调命令链）
bash scripts/run_macos_openssl_path_check_draft.sh --dry-run

# 指定 OpenSSL 根目录执行
bash scripts/run_macos_openssl_path_check_draft.sh \
  --openssl-root /opt/homebrew/opt/openssl@3

# 仅验证路径与环境，不跑模块测试
bash scripts/run_macos_openssl_path_check_draft.sh \
  --skip-module-tests --skip-phase2-dryrun
```

---

## 3. 验收口径（Draft）

- `openssl version` 在注入环境后可执行。
- `libcrypto.dylib` / `libssl.dylib` / `ssl.h` 路径校验通过。
- （默认链路）模块回归命令可执行；Phase2 dry-run 命令可执行。

---

## 4. 与门禁分层策略关系

- 对应 B12 的 `macOS L0/L2/L3`：
  - `L0`：路径与依赖预检。
  - `L2`：模块回归入口。
  - `L3`：Phase2 baseline dry-run 作为深度门禁前置。

---

## 5. 后续任务

- B14：Windows WinSSL/OpenSSL 对照门禁草案。
- B15：将 Linux/macOS/Windows 分层门禁转成 CI workflow 草案。
