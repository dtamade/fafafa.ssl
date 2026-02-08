# Wave C B102 Single-Point Integration Draft（2026-02-08）

## 目标

在不扩散风险的前提下，把 cert verify cache 以“单点接入”方式挂入 OpenSSL 连接验证链路，形成最小可回滚实现草案。

---

## 接入范围（仅单点）

- 首选接入点：`src/fafafa.ssl.openssl.connection.pas:1400`
  - `VerifyCertificateOCSP` 分支中的 `X509_verify_cert` 调用点。
- 暂不改动：`src/fafafa.ssl.openssl.connection.pas:853` 路径（保留作为对照路径）。

---

## 风险隔离策略

1. **开关控制**：新增显式选项（默认关闭），避免默认行为变化。
2. **结果语义保持**：缓存仅复用“验证结果读取”，不改变错误码映射逻辑。
3. **TTL 限制**：缓存命中必须受 TTL 约束，过期后回退原始验证。
4. **单点接入**：本批只改一条业务路径，降低问题定位成本。

---

## 实施清单（草案）

1. 新增可选开关（建议在 context option 层），默认 `off`。
2. 在 `VerifyCertificateOCSP` 中：
   - 先按证书指纹尝试读取缓存；
   - 未命中时执行 `X509_verify_cert` 并写回缓存；
   - 命中时沿用现有错误码/成功路径语义。
3. 增加最小回归测试：
   - 命中路径不改变验证结论；
   - 关闭开关时行为与当前版本一致。

---

## 验证命令（实施后）

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
bash scripts/run_wave_c_b101_validation_playbook.sh --run-id <RUN_ID> --strict --full-gate
```

---

## 回滚策略

- 若出现行为偏差：
  1. 先关闭开关回退；
  2. 保留探针脚本与报告用于复盘；
  3. 不在同批次扩展第二接入点。

---

## B103 入口

- 在本草案基础上，优先执行“开关骨架 + 单点读写 + 回归证明”。
