#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$repo_root"

fail() {
  printf '[FAIL] %s\n' "$1" >&2
  exit 1
}

factory_file="src/fafafa.ssl.factory.pas"
api_ref="docs/reference/API_REFERENCE.md"
contract_src="tests/contract/test_server_helper_verifymode_default_entry.pas"
build_root="tmp/test_server_helper_verifymode_default_entry"
units_dir="$build_root/units"
bin_dir="$build_root/bin"
binary="$bin_dir/test_server_helper_verifymode_default_entry"

printf '[TEST] server helper verify-mode default alignment contract\n'

if rg -F -n --quiet 'Result.SetVerifyMode([sslVerifyNone]); // 服务端默认不验证客户端' "$factory_file"; then
  fail "CreateServerContext must stop silently forcing server helpers into no-verify mode"
fi

if ! rg -F -n --quiet '`CreateServerContext(...)` / `QuickServer(...)` 当前不会再隐式切到 no-verify；' "$api_ref"; then
  fail "API reference must record the current helper verify-mode truth"
fi

mkdir -p "$units_dir" "$bin_dir"
fpc -B -Fu./src -Fu./tests -FU"$units_dir" -FE"$bin_dir" -o"$binary" "$contract_src" >/dev/null
if [[ ! -x "$binary" ]]; then
  fail "server helper verify-mode contract source must compile"
fi

"$binary"

printf '[PASS] server helper verify-mode default alignment contract passed\n'
