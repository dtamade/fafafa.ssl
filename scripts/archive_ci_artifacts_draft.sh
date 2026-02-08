#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

OUTPUT_ROOT="$PROJECT_ROOT/artifacts/ci"
RUN_ID=""
PROFILE="pr"
INCLUDE_BINARIES=false
COMPRESS=true
DRY_RUN=false

RETENTION_CORE=30
RETENTION_PERF=14
RETENTION_DOC=30
RETENTION_DEBUG=7
RETENTION_BIN=7

CORE_COUNT=0
PERF_COUNT=0
DOC_COUNT=0
DEBUG_COUNT=0
BIN_COUNT=0
TOTAL_COUNT=0

TMP_DIR=""
MANIFEST_CSV=""
MANIFEST_MD=""
RUN_DIR=""
ARCHIVE_FILE=""

usage() {
  cat <<'USAGE'
CI 产物归档脚本（Draft）

目标：
  将测试与性能相关输出按“产物类别 + 保留天数”归档到统一目录，
  便于接入 GitHub Actions upload-artifact。

用法：
  scripts/archive_ci_artifacts_draft.sh [options]

选项：
  --profile NAME         归档策略（pr|nightly|release，默认: pr）
  --run-id ID            指定归档批次 ID（默认: yyyyMMdd_HHmmss）
  --output-root DIR      归档输出根目录（默认: artifacts/ci）
  --include-binaries     额外归档二进制产物（默认关闭）
  --no-compress          不生成 tar.gz 压缩包
  --dry-run              仅打印计划，不执行拷贝与打包
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --profile)
      PROFILE="$2"
      shift 2
      ;;
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --output-root)
      OUTPUT_ROOT="$2"
      shift 2
      ;;
    --include-binaries)
      INCLUDE_BINARIES=true
      shift
      ;;
    --no-compress)
      COMPRESS=false
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

if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +"%Y%m%d_%H%M%S")"
fi

case "$PROFILE" in
  pr)
    RETENTION_CORE=30
    RETENTION_PERF=14
    RETENTION_DOC=30
    RETENTION_DEBUG=7
    RETENTION_BIN=7
    ;;
  nightly)
    RETENTION_CORE=14
    RETENTION_PERF=30
    RETENTION_DOC=30
    RETENTION_DEBUG=7
    RETENTION_BIN=7
    ;;
  release)
    RETENTION_CORE=90
    RETENTION_PERF=90
    RETENTION_DOC=90
    RETENTION_DEBUG=14
    RETENTION_BIN=14
    ;;
  *)
    echo "[FAIL] unsupported profile: $PROFILE (expected: pr|nightly|release)" >&2
    exit 1
    ;;
esac

RUN_DIR="$OUTPUT_ROOT/$RUN_ID"
ARCHIVE_FILE="$OUTPUT_ROOT/${RUN_ID}_${PROFILE}_ci_artifacts.tar.gz"

log_info() {
  echo "[INFO] $1"
}

log_warn() {
  echo "[WARN] $1"
}

setup_runtime_files() {
  TMP_DIR="$(mktemp -d)"
  trap 'rm -rf "$TMP_DIR"' EXIT

  mkdir -p "$TMP_DIR"
  : > "$TMP_DIR/seen_files.txt"

  if [[ "$DRY_RUN" == "false" ]]; then
    mkdir -p "$RUN_DIR"
    MANIFEST_CSV="$RUN_DIR/manifest.csv"
    MANIFEST_MD="$RUN_DIR/manifest.md"

    cat > "$MANIFEST_CSV" <<'EOF_CSV'
class,retention_days,relative_path
EOF_CSV
  fi
}

glob_files() {
  local pattern="$1"

  (
    cd "$PROJECT_ROOT"
    shopt -s nullglob
    local old_ifs="$IFS"
    IFS=$'\n'
    for rel in $pattern; do
      if [[ -f "$rel" ]]; then
        printf '%s\n' "$rel"
      fi
    done
    IFS="$old_ifs"
  )
}

increment_class_counter() {
  local class_name="$1"
  case "$class_name" in
    core-reports) CORE_COUNT=$((CORE_COUNT + 1)) ;;
    perf-baseline) PERF_COUNT=$((PERF_COUNT + 1)) ;;
    docs-evidence) DOC_COUNT=$((DOC_COUNT + 1)) ;;
    debug-logs) DEBUG_COUNT=$((DEBUG_COUNT + 1)) ;;
    binaries) BIN_COUNT=$((BIN_COUNT + 1)) ;;
    *) ;;
  esac
  TOTAL_COUNT=$((TOTAL_COUNT + 1))
}

already_seen() {
  local class_name="$1"
  local rel="$2"

  if grep -Fqx "$class_name|$rel" "$TMP_DIR/seen_files.txt"; then
    return 0
  fi

  echo "$class_name|$rel" >> "$TMP_DIR/seen_files.txt"
  return 1
}

copy_one_file() {
  local class_name="$1"
  local retention_days="$2"
  local rel="$3"

  if already_seen "$class_name" "$rel"; then
    return 0
  fi

  increment_class_counter "$class_name"

  log_info "[$class_name][${retention_days}d] $rel"

  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi

  local src="$PROJECT_ROOT/$rel"
  local dst="$RUN_DIR/$class_name/$rel"
  mkdir -p "$(dirname "$dst")"
  cp -f "$src" "$dst"

  echo "$class_name,$retention_days,$rel" >> "$MANIFEST_CSV"
}

collect_class_artifacts() {
  local class_name="$1"
  local retention_days="$2"
  shift 2

  local matched=false
  local pattern

  for pattern in "$@"; do
    while IFS= read -r rel; do
      matched=true
      copy_one_file "$class_name" "$retention_days" "$rel"
    done < <(glob_files "$pattern")
  done

  if [[ "$matched" == "false" ]]; then
    log_warn "[$class_name] no files matched"
  fi
}

write_manifest_markdown() {
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi

  cat > "$MANIFEST_MD" <<EOF_MD
# CI Artifact Manifest (Draft)

- run_id: $RUN_ID
- profile: $PROFILE
- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')
- project_root: $PROJECT_ROOT

## Retention Policy

| class | retention_days | files |
|-------|----------------|-------|
| core-reports | $RETENTION_CORE | $CORE_COUNT |
| perf-baseline | $RETENTION_PERF | $PERF_COUNT |
| docs-evidence | $RETENTION_DOC | $DOC_COUNT |
| debug-logs | $RETENTION_DEBUG | $DEBUG_COUNT |
| binaries | $RETENTION_BIN | $BIN_COUNT |

- total files: $TOTAL_COUNT
- compressed bundle: $(basename "$ARCHIVE_FILE")
EOF_MD
}

compress_archive_if_needed() {
  if [[ "$DRY_RUN" == "true" ]]; then
    return 0
  fi

  if [[ "$COMPRESS" == "false" ]]; then
    log_info "compression disabled (--no-compress)"
    return 0
  fi

  tar -czf "$ARCHIVE_FILE" -C "$OUTPUT_ROOT" "$RUN_ID"
  log_info "archive generated: $ARCHIVE_FILE"
}

setup_runtime_files

log_info "========================================"
log_info "fafafa.ssl CI Artifact Archive (Draft)"
log_info "========================================"
log_info "profile: $PROFILE"
log_info "run_id: $RUN_ID"
log_info "output_root: $OUTPUT_ROOT"
log_info "dry_run: $DRY_RUN"
log_info "include_binaries: $INCLUDE_BINARIES"

collect_class_artifacts "core-reports" "$RETENTION_CORE" \
  "test-reports/test_report_*.txt" \
  "test-reports/*_result.txt" \
  "test-reports/*_compile.log"

collect_class_artifacts "perf-baseline" "$RETENTION_PERF" \
  "tests/benchmarks/results/benchmark_summary_*.txt" \
  "tests/benchmarks/results/*.log" \
  "tests/benchmarks/results/*baseline*.json"

collect_class_artifacts "docs-evidence" "$RETENTION_DOC" \
  "docs/test_reports/PHASE2_*.md" \
  "docs/plans/PHASE3_*.md"

collect_class_artifacts "debug-logs" "$RETENTION_DEBUG" \
  "tests/benchmarks/bin/*_compile.log"

if [[ "$INCLUDE_BINARIES" == "true" ]]; then
  collect_class_artifacts "binaries" "$RETENTION_BIN" \
    "bin/test_*" \
    "tests/benchmarks/bin/*"
fi

write_manifest_markdown
compress_archive_if_needed

log_info "summary: core=$CORE_COUNT perf=$PERF_COUNT docs=$DOC_COUNT debug=$DEBUG_COUNT bin=$BIN_COUNT total=$TOTAL_COUNT"

if [[ "$DRY_RUN" == "false" ]]; then
  log_info "artifact directory: $RUN_DIR"
  log_info "manifest csv: $MANIFEST_CSV"
  log_info "manifest md: $MANIFEST_MD"
fi

log_info "[PASS] CI artifact archive draft finished"
