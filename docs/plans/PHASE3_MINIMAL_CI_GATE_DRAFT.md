# Phase 3 最小 CI 门禁草案（Draft）

**目标**：在进入跨平台矩阵前，先建立一条“成本低、可重复、能及时阻断回归”的最小门禁链路。  
**范围**：编译门禁 + P2 核心模块回归 + Phase2 基准入口可执行性检查。

---

## 1. 门禁命令（建议顺序）

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50
```

说明：
- 第 1 步验证模块编译完整性；
- 第 2 步验证 P2 核心功能回归；
- 第 3 步确保 Phase2 基准入口可用（不引入长时性能负载）。

---

## 2. 脚本入口（已提供）

统一脚本：`scripts/run_minimal_ci_gate.sh`

示例：

```bash
# 默认门禁
bash scripts/run_minimal_ci_gate.sh

# 仅做命令检查（CI 配置联调阶段）
bash scripts/run_minimal_ci_gate.sh --dry-run

# 自定义模块列表
bash scripts/run_minimal_ci_gate.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT --verbose

# 追加 TLS13 CertificateVerify（纯 Pascal signer）基准
bash scripts/run_minimal_ci_gate.sh --with-tls13-sign-bench

# 仅运行 TLS13 signer 基准（快速模式）
bash scripts/run_minimal_ci_gate.sh --only-tls13-sign-bench

# 仅运行 TLS13 signer 基准并自定义参数
bash scripts/run_minimal_ci_gate.sh --only-tls13-sign-bench \
  --tls13-sign-bench-scheme rsa_pss_rsae_sha256 \
  --tls13-sign-bench-iterations 3 \
  --tls13-sign-bench-warmup 1
```

可用参数补充：
- `--skip-compile`：跳过 `compile_all_modules.py`；
- `--skip-modules`：跳过 `run_all_module_tests.sh`；
- `--skip-phase2-dryrun`：跳过 Phase2 baseline dry-run；
- `--with-tls13-sign-bench`：在原门禁后追加 signer 基准；
- `--only-tls13-sign-bench`：等价于 `--skip-compile --skip-modules --skip-phase2-dryrun --with-tls13-sign-bench`；
- `--tls13-sign-bench-scheme`：`rsa_pkcs1_sha256` / `rsa_pss_rsae_sha256` / `rsa_pss_pss_sha256`；
- `--tls13-sign-bench-iterations`：基准迭代次数；
- `--tls13-sign-bench-warmup`：基准预热次数；
- `--tls13-sign-bench-key`：私钥路径（默认 `tests/certificate/test_certs/signer_key.pem`）。

---

## 3. Wave B Gate（可选追加 signer 基准）

统一脚本：`scripts/run_wave_b_ci_gate.sh`

示例：

```bash
# 默认 Wave B 门禁
bash scripts/run_wave_b_ci_gate.sh

# Wave B 门禁 + TLS13 signer 基准
bash scripts/run_wave_b_ci_gate.sh --with-tls13-sign-bench

# Wave B 门禁 + 指定 signer 基准参数
bash scripts/run_wave_b_ci_gate.sh \
  --with-tls13-sign-bench \
  --tls13-sign-bench-scheme rsa_pss_rsae_sha256 \
  --tls13-sign-bench-iterations 1 \
  --tls13-sign-bench-warmup 0
```

说明：
- 当启用 `--with-tls13-sign-bench` 时，基准失败会将 Wave B gate 判定为失败；
- summary 会追加 `tls13_servercertverify_bench` 步骤和 `TLS13 Signer Bench Metrics` 小节。

---

## 4. 验收标准（Draft）

1. `compile_all_modules`：无编译失败。
2. `run_all_module_tests`：指定模块集合 `failed: 0`。
3. `run_phase2_performance_baseline --dry-run`：exit code = 0。

---

## 5. 后续扩展（B8/B9）

- B8：将 Phase2 指标模板回填到首轮 baseline 报告，形成“可审阅性能结论”。
- B9：扩展跨平台矩阵（Linux/macOS/Windows）并增加平台差异门禁策略。
