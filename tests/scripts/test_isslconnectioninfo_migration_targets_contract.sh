#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

doc_file="docs/reference/INTERFACE_DESIGN_V2.md"

declare -a forbidden_patterns=(
  'ISSLAdvanced'
  '| GetConnectionInfo | **移除** | 使用 ISSLDiagnostics |'
  '| GetStateString | **移除** | 合并到 GetState |'
  '| GetContext | **移除** | 通常不需要 |'
  '| GetSelectedALPNProtocol | ISSLClientConnection | 客户端特有 |'
)

for pattern in "${forbidden_patterns[@]}"; do
  if grep -F -q "$pattern" "$doc_file"; then
    echo "[FAIL] ISSLConnectionInfo migration doc still contains stale target: $pattern"
    exit 1
  fi
done

declare -a required_patterns=(
  '├── ISSLConnectionInfo (连接信息 mirrors)'
  '### ISSLConnectionInfo (连接信息 mirrors)'
  'ISSLConnectionInfo = interface'
  'function GetConnectionInfo: TSSLConnectionInfo;'
  'function GetContext: ISSLContext;'
  'function GetSelectedALPNProtocol: string;'
  'function GetStateString: string;'
  'if Supports(LConn, ISSLConnectionInfo, LInfoExt) then'
  'ISSLConnectionInfo,'
  '| GetConnectionInfo | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |'
  '| GetStateString | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |'
  '| GetContext | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |'
  '| GetSelectedALPNProtocol | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |'
  '当前 `v1.x` source truth 里'
  '1. 先把这 4 个 mirrors 的默认 owner 统一成 `ISSLConnectionInfo`'
)

for pattern in "${required_patterns[@]}"; do
  if ! grep -F -q "$pattern" "$doc_file"; then
    echo "[FAIL] ISSLConnectionInfo migration doc missing required truth: $pattern"
    exit 1
  fi
done

echo "[PASS] ISSLConnectionInfo migration targets match the current slimming roadmap"
