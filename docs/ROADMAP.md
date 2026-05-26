# fafafa.ssl 路线图

> **更新**: 2026-05-26
> **状态**: RELEASED v1.5.0

---

## 当前状态

- **版本**: v1.5.0 (已发布)
- **工程状态**: 生产就绪，持续优化中
- **默认构建**: `python3 scripts/compile_all_modules.py`
- **默认门禁**: `bash scripts/run_minimal_ci_gate.sh --fast-local`
- **TLS 1.3 门禁**: `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`

## 后端成熟度

| 后端 | 状态 | 备注 |
|------|------|------|
| OpenSSL | 生产就绪 | 完整 TLS 1.2/1.3, OCSP, CT, Early Data |
| MbedTLS | 生产就绪 | TLS 1.2/1.3 核心功能 |
| WolfSSL | 生产就绪 | TLS 1.2/1.3, Early Data (需 helper) |
| WinSSL | 生产就绪 | Windows 原生 Schannel |
| FreePascal | 实验性 | 纯 Pascal TLS 1.3 客户端/服务端 |

## FreePascal 后端注意事项

- 0-RTT / Early Data 为实验性功能
- 依赖本地持久化 replay-store 路径
- 路径不可用时 fail-closed 拒绝

## 下一步方向

1. FreePascal 后端 AES-256-GCM-SHA384 纯 Pascal 实现
2. 跨后端一致性测试覆盖扩展
3. 性能基准持续优化
4. CI/CD 完善

## 构建验证

```bash
# 编译所有模块
python3 scripts/compile_all_modules.py

# 本地最小门禁
bash scripts/run_minimal_ci_gate.sh --fast-local

# P2 模块回归
bash scripts/run_all_module_tests.sh --fast-local --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT

# FreePascal TLS 1.3 完整性
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local

# 代码风格
python3 scripts/check_code_style.py src
```
