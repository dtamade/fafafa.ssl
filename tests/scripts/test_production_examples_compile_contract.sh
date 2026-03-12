#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile_one() {
  local src="$1"
  local out="$2"
  local log="/tmp/$(basename "$out").compile.log"

  if ! fpc -Fu"$ROOT_DIR/src" -Fu"$ROOT_DIR/examples" "$ROOT_DIR/$src" -o"$ROOT_DIR/$out" >"$log" 2>&1; then
    echo "[INFO] compile output for $src:"
    sed -n '1,220p' "$log" || true
    fail "$src should compile with example helper search path"
  fi
}

compile_one examples/production/https_server_simple.pas tmp/production_https_server_simple_contract
compile_one examples/production/https_client_post.pas tmp/production_https_client_post_contract
compile_one examples/production/https_client_session.pas tmp/production_https_client_session_contract
compile_one examples/production/https_client_simple.pas tmp/production_https_client_simple_contract
compile_one examples/production/https_client_auth.pas tmp/production_https_client_auth_contract

echo '[PASS] production HTTPS examples compile with example helper units'
