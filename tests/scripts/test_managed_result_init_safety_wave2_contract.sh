#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WIRE_FILE="$ROOT_DIR/src/fafafa.ssl.tls13.wire.pas"
SESSION_FILE="$ROOT_DIR/src/fafafa.ssl.freepascal.session.pas"

echo "[TEST] managed result init safety wave2 contract"

python3 - "$WIRE_FILE" "$SESSION_FILE" <<'PY'
from pathlib import Path
import re
import sys

wire = Path(sys.argv[1]).read_text(encoding="utf-8")
session = Path(sys.argv[2]).read_text(encoding="utf-8")

def require(condition: bool, message: str) -> None:
    if not condition:
        print(f"[FAIL] {message}")
        raise SystemExit(1)
    print(f"[PASS] {message}")

def extract_function(text: str, signature: str) -> str:
    m = re.search(rf"{re.escape(signature)}.*?^end;", text, re.S | re.M)
    require(m is not None, f"function exists: {signature}")
    return m.group(0)

extract_function(
    wire, "function BuildTLSPlaintext(AContentType: Byte; const APayload: TBytes): TBytes;"
)
read_vector16 = extract_function(
    session, "function ReadVector16(const AData: TBytes; var AOffset: Integer): TBytes;"
)
serialize = extract_function(
    session, "function TFreePascalSession.Serialize: TBytes;"
)

require(re.search(r"function BuildTLSPlaintext\(AContentType: Byte; const APayload: TBytes\): TBytes;.*?Result := nil;.*?SetLength\(Result, 5 \+ LLen\);",
                  wire, re.S) is not None,
        "BuildTLSPlaintext initializes empty TBytes result with nil before SetLength and still builds the exact-sized TLSPlaintext buffer")

require(re.search(r"function ReadVector16\(const AData: TBytes; var AOffset: Integer\): TBytes;.*?Result := nil;.*?SetLength\(Result, LLen\);",
                  session, re.S) is not None,
        "ReadVector16 initializes empty TBytes result with nil before SetLength and still allocates the requested vector length")
require("SetLength(Result, LLen);" in read_vector16,
        "ReadVector16 still allocates the requested vector length")

require("Result := nil;" in serialize,
        "TFreePascalSession.Serialize initializes empty TBytes result with nil")
require("SetLength(Result, 0);" not in serialize,
        "TFreePascalSession.Serialize no longer uses SetLength(Result, 0) on an uninitialized managed result")
PY

echo "[PASS] managed result init safety wave2 contract passed"
