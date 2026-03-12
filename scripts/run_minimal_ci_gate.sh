#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUN_ID="${FAFAFA_MINIMAL_CI_GATE_RUN_ID:-$(date +%Y%m%d_%H%M%S)_$$}"
COMPILE_UNIT_OUTPUT_DIR="${FAFAFA_MINIMAL_CI_GATE_COMPILE_UNIT_OUTPUT_DIR:-tmp/minimal_ci_gate_compile_units_${RUN_ID}}"
MODULE_UNIT_OUTPUT_DIR="${FAFAFA_MINIMAL_CI_GATE_MODULE_UNIT_OUTPUT_DIR:-tmp/minimal_ci_gate_module_units_${RUN_ID}}"
MODULE_BIN_OUTPUT_DIR="${FAFAFA_MINIMAL_CI_GATE_MODULE_BIN_OUTPUT_DIR:-tmp/minimal_ci_gate_module_bin_${RUN_ID}}"
FPC_EXE="${FAFAFA_FPC_EXE:-fpc}"

DRY_RUN=false
VERBOSE=false
WITH_PHASE2_DRYRUN=true
WITH_COMPILE=true
WITH_MODULES=true
WITH_PLATFORM_PATH_CHECKS_DRYRUN=true
WITH_DOCS_GOVERNANCE_STRICT_BATCH=false
WITH_WARNING_NOISE_GOVERNANCE_BATCH=true
WITH_MINIMAL_GATE_CONTRACT_BATCH=false
WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH=false
ONLY_PLATFORM_PATH_CHECK_DRYRUN=false
MODULE_SET="PKCS7,PKCS12,CMS,Store,OCSP,TS,CT"
WITH_TLS13_SIGN_PURITY_CHECK=false
WITH_TLS13_SIGN_BENCH=false
WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=false
TLS13_SIGN_BENCH_ITERATIONS="3"
TLS13_SIGN_BENCH_WARMUP="1"
TLS13_SIGN_BENCH_SCHEME="rsa_pkcs1_sha256"
TLS13_SIGN_BENCH_KEY="tests/certificate/test_certs/signer_key.pem"
TLS13_SIGN_BENCH_TIMEOUT="120"
TLS13_SIGN_BENCH_JSON_OUT=""

usage() {
  cat <<'USAGE'
最小 CI 门禁脚本（Draft）

目标：
  以最低成本覆盖“可编译 + P2 核心回归 + Phase2 基准入口可用性”。

用法：
  scripts/run_minimal_ci_gate.sh [options]

选项：
  --modules LIST                     指定模块列表（默认: PKCS7,PKCS12,CMS,Store,OCSP,TS,CT）
  --skip-compile                     跳过 compile_all_modules 阶段
  --skip-modules                     跳过 run_all_module_tests 阶段
  --skip-phase2-dryrun               跳过 Phase2 baseline 脚本 dry-run 检查
  --fast-local                       快速本地模式：仅保留 warning/noise 治理批次
  --pre-commit-minimal               提交前最小回归：fast-local + skip-warning + contract-batch
  --skip-platform-path-checks-dryrun 跳过四平台路径检查 dry-run batch
  --skip-warning-noise-governance-batch 跳过 warning/noise 治理合同批次
  --only-platform-path-check-dryrun  快速模式：仅运行四平台路径检查 dry-run batch
  --with-docs-governance-strict-batch 追加运行 docs 噪声与索引一致性 strict batch
  --with-warning-noise-governance-batch 追加运行 warning/noise 治理合同批次
  --with-minimal-gate-contract-batch 追加运行 minimal gate 合同批次
  --with-pre-commit-triplet-contract-batch 追加运行 pre-commit 三合同批次
  --with-tls13-sign-purity-check     追加运行 TLS13 signer 纯 Pascal 依赖静态检查
  --with-tls13-sign-bench            追加运行 TLS13 CertificateVerify 纯 Pascal 签名基准
  --with-openssl-cert-verify-cache-runtime 追加运行 OpenSSL 证书验证缓存运行时策略回归（需网络）
  --only-tls13-sign-bench            快速模式：仅运行 TLS13 签名基准（自动启用 skip + with）
  说明：多个 preset 按参数顺序解析，后出现的 preset 覆盖前者（last-flag-wins）
  --tls13-sign-bench-iterations N    TLS13 签名基准迭代次数（默认: 3）
  --tls13-sign-bench-warmup N        TLS13 签名基准预热次数（默认: 1）
  --tls13-sign-bench-scheme NAME     基准算法（默认: rsa_pkcs1_sha256）
  --tls13-sign-bench-key PATH        私钥路径（默认: tests/certificate/test_certs/signer_key.pem）
  --tls13-sign-bench-timeout N       基准超时时间（秒，默认: 120）
  --tls13-sign-bench-json-out PATH   基准 JSON 输出路径（可选）
  --verbose                          模块测试启用 verbose
  --dry-run                          仅打印命令，不执行
  --help                             显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --modules)
      MODULE_SET="$2"
      shift 2
      ;;
    --skip-compile)
      WITH_COMPILE=false
      shift
      ;;
    --skip-modules)
      WITH_MODULES=false
      shift
      ;;
    --skip-phase2-dryrun)
      WITH_PHASE2_DRYRUN=false
      shift
      ;;
    --fast-local)
      ONLY_PLATFORM_PATH_CHECK_DRYRUN=false
      WITH_COMPILE=false
      WITH_MODULES=false
      WITH_PHASE2_DRYRUN=false
      WITH_PLATFORM_PATH_CHECKS_DRYRUN=false
      WITH_DOCS_GOVERNANCE_STRICT_BATCH=false
      WITH_WARNING_NOISE_GOVERNANCE_BATCH=true
      WITH_MINIMAL_GATE_CONTRACT_BATCH=false
      WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH=false
      WITH_TLS13_SIGN_PURITY_CHECK=false
      WITH_TLS13_SIGN_BENCH=false
      WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=false
      shift
      ;;
    --pre-commit-minimal)
      ONLY_PLATFORM_PATH_CHECK_DRYRUN=false
      WITH_COMPILE=false
      WITH_MODULES=false
      WITH_PHASE2_DRYRUN=false
      WITH_PLATFORM_PATH_CHECKS_DRYRUN=false
      WITH_DOCS_GOVERNANCE_STRICT_BATCH=false
      WITH_WARNING_NOISE_GOVERNANCE_BATCH=false
      WITH_MINIMAL_GATE_CONTRACT_BATCH=true
      WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH=false
      WITH_TLS13_SIGN_PURITY_CHECK=false
      WITH_TLS13_SIGN_BENCH=false
      WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=false
      shift
      ;;
    --skip-platform-path-checks-dryrun)
      WITH_PLATFORM_PATH_CHECKS_DRYRUN=false
      shift
      ;;
    --skip-warning-noise-governance-batch)
      WITH_WARNING_NOISE_GOVERNANCE_BATCH=false
      shift
      ;;
    --only-platform-path-check-dryrun)
      ONLY_PLATFORM_PATH_CHECK_DRYRUN=true
      WITH_COMPILE=false
      WITH_MODULES=false
      WITH_PHASE2_DRYRUN=false
      WITH_PLATFORM_PATH_CHECKS_DRYRUN=true
      WITH_DOCS_GOVERNANCE_STRICT_BATCH=false
      WITH_WARNING_NOISE_GOVERNANCE_BATCH=false
      WITH_MINIMAL_GATE_CONTRACT_BATCH=false
      WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH=false
      WITH_TLS13_SIGN_PURITY_CHECK=false
      WITH_TLS13_SIGN_BENCH=false
      WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=false
      shift
      ;;
    --with-docs-governance-strict-batch)
      WITH_DOCS_GOVERNANCE_STRICT_BATCH=true
      shift
      ;;
    --with-warning-noise-governance-batch)
      WITH_WARNING_NOISE_GOVERNANCE_BATCH=true
      shift
      ;;
    --with-minimal-gate-contract-batch)
      WITH_MINIMAL_GATE_CONTRACT_BATCH=true
      shift
      ;;
    --with-pre-commit-triplet-contract-batch)
      WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH=true
      shift
      ;;
    --with-tls13-sign-purity-check)
      WITH_TLS13_SIGN_PURITY_CHECK=true
      shift
      ;;
    --with-tls13-sign-bench)
      WITH_TLS13_SIGN_BENCH=true
      shift
      ;;
    --with-openssl-cert-verify-cache-runtime)
      WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=true
      shift
      ;;
    --only-tls13-sign-bench)
      WITH_COMPILE=false
      WITH_MODULES=false
      WITH_PHASE2_DRYRUN=false
      WITH_PLATFORM_PATH_CHECKS_DRYRUN=false
      WITH_DOCS_GOVERNANCE_STRICT_BATCH=false
      WITH_WARNING_NOISE_GOVERNANCE_BATCH=false
      WITH_MINIMAL_GATE_CONTRACT_BATCH=false
      WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH=false
      WITH_TLS13_SIGN_PURITY_CHECK=false
      WITH_TLS13_SIGN_BENCH=true
      WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME=false
      shift
      ;;
    --tls13-sign-bench-iterations)
      TLS13_SIGN_BENCH_ITERATIONS="$2"
      shift 2
      ;;
    --tls13-sign-bench-warmup)
      TLS13_SIGN_BENCH_WARMUP="$2"
      shift 2
      ;;
    --tls13-sign-bench-scheme)
      TLS13_SIGN_BENCH_SCHEME="$2"
      shift 2
      ;;
    --tls13-sign-bench-key)
      TLS13_SIGN_BENCH_KEY="$2"
      shift 2
      ;;
    --tls13-sign-bench-timeout)
      TLS13_SIGN_BENCH_TIMEOUT="$2"
      shift 2
      ;;
    --tls13-sign-bench-json-out)
      TLS13_SIGN_BENCH_JSON_OUT="$2"
      shift 2
      ;;
    --verbose)
      VERBOSE=true
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ ! "$TLS13_SIGN_BENCH_ITERATIONS" =~ ^[0-9]+$ ]] || [[ "$TLS13_SIGN_BENCH_ITERATIONS" -le 0 ]]; then
  echo "Invalid --tls13-sign-bench-iterations: $TLS13_SIGN_BENCH_ITERATIONS" >&2
  exit 1
fi

if [[ ! "$TLS13_SIGN_BENCH_WARMUP" =~ ^[0-9]+$ ]] || [[ "$TLS13_SIGN_BENCH_WARMUP" -lt 0 ]]; then
  echo "Invalid --tls13-sign-bench-warmup: $TLS13_SIGN_BENCH_WARMUP" >&2
  exit 1
fi

if [[ ! "$TLS13_SIGN_BENCH_TIMEOUT" =~ ^[0-9]+$ ]] || [[ "$TLS13_SIGN_BENCH_TIMEOUT" -le 0 ]]; then
  echo "Invalid --tls13-sign-bench-timeout: $TLS13_SIGN_BENCH_TIMEOUT" >&2
  exit 1
fi

run_cmd() {
  local cmd="$1"
  echo "[GATE] $cmd"
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi
  eval "$cmd"
}

now_millis() {
  if date +%s%3N >/dev/null 2>&1; then
    date +%s%3N
    return 0
  fi
  python3 - <<'PY'
import time
print(int(time.time() * 1000))
PY
}

echo "========================================"
echo "fafafa.ssl Minimal CI Gate (Draft)"
echo "========================================"
echo "[INFO] run_id: $RUN_ID"
echo "[INFO] compile unit output dir: $COMPILE_UNIT_OUTPUT_DIR"
echo "[INFO] module unit output dir: $MODULE_UNIT_OUTPUT_DIR"
echo "[INFO] module bin output dir: $MODULE_BIN_OUTPUT_DIR"

if [[ "$WITH_COMPILE" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && python3 scripts/compile_all_modules.py --unit-output-dir '$COMPILE_UNIT_OUTPUT_DIR' --fpc-exe '$FPC_EXE'"
fi

if [[ "$WITH_MODULES" == "true" ]]; then
  module_cmd="cd '$PROJECT_ROOT' && FAFAFA_FPC_EXE='$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='$MODULE_BIN_OUTPUT_DIR' bash scripts/run_all_module_tests.sh --modules $MODULE_SET"
  if [[ "$VERBOSE" == "true" ]]; then
    module_cmd="$module_cmd --verbose"
  fi
  run_cmd "$module_cmd"
fi

if [[ "$WITH_PHASE2_DRYRUN" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50"
fi

if [[ "$WITH_PLATFORM_PATH_CHECKS_DRYRUN" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh"
fi

if [[ "$WITH_DOCS_GOVERNANCE_STRICT_BATCH" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh"
fi

if [[ "$WITH_WARNING_NOISE_GOVERNANCE_BATCH" == "true" ]]; then
  WARNING_NOISE_START_MS="$(now_millis)"
  run_cmd "cd '$PROJECT_ROOT' && bash tests/scripts/test_warning_noise_governance_contract_batch.sh"
  WARNING_NOISE_END_MS="$(now_millis)"
  WARNING_NOISE_ELAPSED_MS=$((WARNING_NOISE_END_MS - WARNING_NOISE_START_MS))
  if [[ "$WARNING_NOISE_ELAPSED_MS" -lt 0 ]]; then
    WARNING_NOISE_ELAPSED_MS=0
  fi
  echo "[INFO] warning-noise governance elapsed_ms=$WARNING_NOISE_ELAPSED_MS"
fi

if [[ "$WITH_MINIMAL_GATE_CONTRACT_BATCH" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash tests/scripts/test_minimal_ci_gate_contract_batch.sh"
fi

if [[ "$WITH_PRE_COMMIT_TRIPLET_CONTRACT_BATCH" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh"
fi

if [[ "$WITH_TLS13_SIGN_PURITY_CHECK" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && bash scripts/check_tls13_signer_pure_pascal.sh"
fi

if [[ "$WITH_TLS13_SIGN_BENCH" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && FAFAFA_TLS13_SIGN_BENCH_ITERATIONS='$TLS13_SIGN_BENCH_ITERATIONS' FAFAFA_TLS13_SIGN_BENCH_WARMUP='$TLS13_SIGN_BENCH_WARMUP' FAFAFA_TLS13_SIGN_BENCH_SCHEME='$TLS13_SIGN_BENCH_SCHEME' FAFAFA_TLS13_SIGN_BENCH_KEY='$TLS13_SIGN_BENCH_KEY' FAFAFA_TLS13_SIGN_BENCH_TIMEOUT='$TLS13_SIGN_BENCH_TIMEOUT' FAFAFA_TLS13_SIGN_BENCH_JSON_OUT='$TLS13_SIGN_BENCH_JSON_OUT' bash scripts/run_freepascal_tls13_servercertverify_bench.sh"
fi

if [[ "$WITH_OPENSSL_CERT_VERIFY_CACHE_RUNTIME" == "true" ]]; then
  run_cmd "cd '$PROJECT_ROOT' && \"$FPC_EXE\" -Fu./src -Fu./src/openssl -Fu./tests/framework -Fi./src tests/integration/test_openssl_cert_verify_cache_policy_runtime.pas -otmp/test_openssl_cert_verify_cache_policy_runtime && FAFAFA_RUN_NETWORK_TESTS=1 ./tmp/test_openssl_cert_verify_cache_policy_runtime"
fi

echo "[PASS] minimal CI gate finished"
