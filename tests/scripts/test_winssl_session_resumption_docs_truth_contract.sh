#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

matrix_doc="$ROOT_DIR/docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md"
perf_doc="$ROOT_DIR/docs/reference/WINSSL_PERFORMANCE_TUNING.md"
api_ref="$ROOT_DIR/docs/reference/API_REFERENCE.md"
status_report="$ROOT_DIR/docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] WinSSL session-resumption docs truth contract"

for pattern in \
  "Session 复用 | ✅ 支持 | 完整支持" \
  "Session Ticket | ✅ 支持 | TLS 1.2+" \
  "Conn1.GetSession" \
  "Conn2.SetSession(Session)" \
  "Conn2.IsSessionResumed"
do
  if grep -F -q -- "$pattern" "$matrix_doc"; then
    fail "WinSSL backend capability matrix still overclaims or uses stale session-resumption guidance: $pattern"
  fi
done

for pattern in \
  "ISSLSessionResumption" \
  "observed_reuse=false" \
  "session_configured=true"
do
  if ! grep -F -q -- "$pattern" "$matrix_doc"; then
    fail "WinSSL backend capability matrix missing current session-resumption truth: $pattern"
  fi
done

for pattern in \
  "减少握手时间 70-90%" \
  "Result.SessionReused := AConn.IsSessionResumed;" \
  "LSession := LConn.GetSession;" \
  "LConn2.IsSessionResumed" \
  "快速握手" \
  "性能提升"
do
  if grep -F -q -- "$pattern" "$perf_doc"; then
    fail "WinSSL performance tuning guide still overclaims runtime-proven session resumption: $pattern"
  fi
done

for pattern in \
  "ISSLSessionResumption" \
  "observed_reuse=false" \
  "当前 dedicated Windows CI runtime truth"
do
  if ! grep -F -q -- "$pattern" "$perf_doc"; then
    fail "WinSSL performance tuning guide missing current runtime caution: $pattern"
  fi
done

for pattern in \
  "✓ Session 复用成功 - 握手时间大幅减少"
do
  if grep -F -q -- "$pattern" "$api_ref"; then
    fail "API reference still presents WinSSL session resumption as a stable success path: $pattern"
  fi
done

for pattern in \
  "| 性能提升       | 70-90%               | 70-90%             |"
do
  if grep -F -q -- "$pattern" "$api_ref"; then
    fail "API reference still overclaims WinSSL session-resumption performance truth: $pattern"
  fi
done

for pattern in \
  "observed_reuse=false" \
  "session_configured=true" \
  "26037518301"
do
  if ! grep -F -q -- "$pattern" "$api_ref"; then
    fail "API reference missing current WinSSL runtime truth note: $pattern"
  fi
done

for pattern in \
  "26037518301" \
  "observed_reuse=false" \
  "session_configured=true" \
  "windows-gate"
do
  if ! grep -F -q -- "$pattern" "$status_report"; then
    fail "WinSSL backend status report missing final green runtime bridge truth: $pattern"
  fi
done

echo "[PASS] WinSSL session-resumption docs truth contract passed"
