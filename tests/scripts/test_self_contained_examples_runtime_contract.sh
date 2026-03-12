#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile_and_run() {
  local src="$1"
  local out_name="$2"
  local token="$3"
  local log_name="$4"
  local run_dir="$5"

  rm -rf "$run_dir"
  mkdir -p "$run_dir"
  local out_path="$ROOT_DIR/$run_dir/$out_name"
  local log_path="$ROOT_DIR/$run_dir/$log_name"
  local compile_log="$ROOT_DIR/$run_dir/$log_name.compile"

  if ! (
    cd "$run_dir"
    fpc -Fu"$ROOT_DIR/src" "$ROOT_DIR/$src" -o"$out_path" >"$compile_log" 2>&1
  ); then
    echo "[INFO] compile output for $src:"
    sed -n '1,220p' "$compile_log" || true
    fail "$src should compile"
  fi

  if ! (
    cd "$run_dir"
    "$out_path" >"$log_path" 2>&1
  ); then
    echo "[INFO] runtime output for $src:"
    sed -n '1,220p' "$log_path" || true
    fail "$src should run"
  fi

  if ! rg -F --quiet -- "$token" "$log_path"; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,220p' "$log_path" || true
    fail "$src should print completion marker"
  fi
}

compile_and_run examples/pkcs7_sign_verify_simple.pas pkcs7_sign_verify_simple_contract '[PASS] pkcs7 sign/verify simple example completed' pkcs7_sign_verify_simple.log tmp/runtime_contracts/pkcs7_sign_verify_simple
compile_and_run examples/02_generate_certificate.pas generate_certificate_contract '[PASS] generate certificate example completed' generate_certificate.log tmp/runtime_contracts/generate_certificate

echo '[PASS] self-contained example programs stay green at runtime'
