# fafafa.ssl 文档中心

fafafa.ssl 是 Free Pascal 的高性能 SSL/TLS 库，支持 OpenSSL、WinSSL、FreePascal，以及可选的 MbedTLS / WolfSSL 后端。

## 文档导航

详见 [DOCUMENTATION_INDEX.md](DOCUMENTATION_INDEX.md)。

## 快速链接

- [架构概览](ARCHITECTURE.md)
- [后端选择指南](BACKEND_SELECTION_GUIDE.md)
- [后端能力矩阵](BACKEND_CAPABILITY_MATRIX.md)
- [路线图](ROADMAP.md)

## 构建命令

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```
