#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TARGET="$ROOT_DIR/src/fafafa.ssl.freepascal.connection.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal post-handshake send pending structure contract"

rg -F --quiet -- "fpPendingWriteKeyUpdate" "$TARGET" || {
  fail "missing dedicated pending-write kind for KeyUpdate"
}

rg -F --quiet -- "fpPendingWriteSessionTicket" "$TARGET" || {
  fail "missing dedicated pending-write kind for NewSessionTicket"
}

rg -F --quiet -- "SendBufferedRecord(LRecord, fpPendingWriteKeyUpdate)" "$TARGET" || {
  fail "KeyUpdate path should use SendBufferedRecord"
}

rg -F --quiet -- "SendBufferedRecord(LRecord, fpPendingWriteSessionTicket)" "$TARGET" || {
  fail "NewSessionTicket path should use SendBufferedRecord"
}

echo "[PASS] pure Pascal post-handshake send pending structure contract passed"
