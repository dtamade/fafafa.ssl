# macOS OpenSSL Loader Symbol Probe（2026-05-18）

## Goal
- 为 `wave-b-b2-manual.yml` 的 `macos-gate` 增加一条真正可复用的 loader/symbol 证据链，直接产出：
  - `TOpenSSLLoader.GetVersionInfo.VersionString`
  - 关键 direct symbol 命中情况
  - batch-wrapper 模块加载结果
- 让后续 macOS 调试不再停留在“环境看起来像对的”或“日志里出现了 3.x 请求名”的模糊层面。

## Why now
- 旧的 `OPENSSL_ROOT` 优先级修法已经落地并通过 focused contract，但 live macOS rerun 没有改变失败面。
- 当前现象更像：
  - `TS/CT/Store` 这类 direct `GetCryptoProcAddress(...)` 路线仍可工作
  - `EVP/PEM/PKCS12/CMS/OCSP` 这类 `LoadFunctions(...)`/batch-binding 路线持续失败
- 现有 `wave_b_macos_gate_probe_*.json` 只覆盖环境探测，不覆盖“实际加载到哪个库、关键符号能否被直接解析”。

## Non-Goals
- 本批不直接重写 `PEM/PKCS12/CMS/OCSP` 的批量绑定表。
- 本批不再次把 `OPENSSL_ROOT` 优先级当成主根因继续深挖。
- 本批不改动 Windows/WinSSL 已收口 lane。

## Files
- `tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
- `scripts/run_macos_openssl_loader_symbol_probe.sh`
- `scripts/run_wave_b_macos_gate.sh`
- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- `tests/scripts/test_wave_b_macos_gate_loader_symbol_probe_contract.sh`

## Approach
1. 静态确认失败模块与绑定路径的共性：
   - `TS/CT/Store` 走 direct `GetCryptoProcAddress(...)`
   - `EVP/PEM/PKCS12/CMS/OCSP` 走 `LoadFunctions(...)` 或 batch-binding
2. 新增 Pascal probe 程序，输出：
   - loader 实际版本字符串
   - direct symbols: `PEM_read_bio_X509`, `PKCS12_new`, `CMS_sign`, `OCSP_REQUEST_new`, `TS_REQ_new`, `CTLOG_STORE_new`, `OSSL_STORE_open`
   - wrapper/module results: `LoadEVP`, `LoadOpenSSLPEM`, `LoadPKCS12Module`, `LoadOpenSSLCMS`, `LoadOpenSSLOCSP`
3. 新增脚本入口，把 probe 编译并产出 JSON。
4. 把 probe 接到 macOS gate 和 workflow artifact。
5. 补 focused shell contracts，防止 workflow/gate 以后把这条证据链删掉。

## Commands
```bash
bash -n scripts/run_macos_openssl_loader_symbol_probe.sh
bash -n tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh
bash -n tests/scripts/test_wave_b_macos_gate_loader_symbol_probe_contract.sh

bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh
bash tests/scripts/test_wave_b_macos_gate_loader_symbol_probe_contract.sh

bash scripts/run_macos_openssl_loader_symbol_probe.sh \
  --run-id local_probe \
  --output tmp/local_probe.json

git diff --check
```

## Expected Outputs
- workflow contract 证明：
  - macOS artifact 会上传新的 loader symbol probe JSON
- gate contract 证明：
  - `run_wave_b_macos_gate.sh` 会实际调用新 probe，并在 summary 里保留 evidence row
- local probe 至少要能产出一份 JSON，让我们验证字段结构和 triage 信号。

## Execution Result
- GREEN:
  - `tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas` 已落地
  - `scripts/run_macos_openssl_loader_symbol_probe.sh` 已落地
  - `run_wave_b_macos_gate.sh` 已新增 `loader-symbol-probe` step
  - workflow active + disabled template 已上传新的 JSON artifact
  - focused contracts 通过：
    - `test_wave_b_b2_macos_probe_workflow_contract.sh`
    - `test_wave_b_macos_gate_loader_symbol_probe_contract.sh`
    - 以及受影响的 macOS gate 合同回归集
  - 本地 `local_probe.json` 已成功产出，并证明 probe 结构可读
- NEXT:
  - push 后重跑 `wave-b-b2-manual.yml`
  - 重点观察新的 `wave_b_macos_loader_symbol_probe_<run_id>.json`
  - 用它判断：
    - 是 loader 仍然拿错库
    - 还是 direct symbols 本身存在、但 batch-wrapper/binding table 漂移
