# fafafa.ssl 文档索引

## 入门

- [架构概览](ARCHITECTURE.md)
- [平台支持](PLATFORM_SUPPORT.md)
- [依赖说明](DEPENDENCIES.md)
- [路线图](ROADMAP.md)

## 用户指南

- [5 分钟快速入门](guides/5_MINUTE_QUICKSTART.md)
- [用户指南](guides/USER_GUIDE.md)
- [部署指南](guides/DEPLOYMENT_GUIDE.md)
- [FAQ](guides/FAQ.md)
- [常见陷阱](guides/COMMON_PITFALLS.md)
- [错误处理最佳实践](guides/ERROR_HANDLING_BEST_PRACTICES.md)

## 后端指南

- [后端选择指南](BACKEND_SELECTION_GUIDE.md)
- [后端能力矩阵](BACKEND_CAPABILITY_MATRIX.md)
- [MbedTLS 指南](guides/MBEDTLS_USER_GUIDE.md)
- [WinSSL 指南](guides/WINSSL_USER_GUIDE.md)
- [DANE 指南](guides/DANE_USER_GUIDE.md)

## 功能指南

- [Early Data 指南](guides/EARLY_DATA_GUIDE.md)
- [OCSP 使用指南](guides/OCSP_USAGE_GUIDE.md)
- [CT 实现指南](guides/CT_IMPLEMENTATION_GUIDE.md)
- [CMS 用户指南](guides/CMS_USER_GUIDE.md)
- [性能优化指南](guides/PERFORMANCE_GUIDE.md)

## 参考

- [API 参考](reference/API_REFERENCE.md)
- [Native Handle 快速参考](NATIVE_HANDLE_QUICK_REF.md)
- [迁移指南](MIGRATION_GUIDE_V1.1.md)

## 构建与测试

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
bash scripts/run_all_module_tests.sh --fast-local
python3 scripts/check_code_style.py src
```
