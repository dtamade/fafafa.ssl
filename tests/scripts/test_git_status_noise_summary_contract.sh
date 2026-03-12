#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/summarize_git_status_noise_draft.sh"
WORK_REL="tmp/test_git_status_noise_summary_contract"
REPO_REL="$WORK_REL/repo"
OUT_REL="$WORK_REL/status_noise_summary.md"
REPO_DIR="$ROOT_DIR/$REPO_REL"
OUT_FILE="$ROOT_DIR/$OUT_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,260p' "$file" || true
    fail "expected pattern not found"
  fi
}

setup_repo_fixture() {
  rm -rf "$ROOT_DIR/$WORK_REL"
  mkdir -p "$REPO_DIR/.github/workflows" "$REPO_DIR/docs" "$REPO_DIR/src" "$REPO_DIR/bin" "$REPO_DIR/scripts" "$REPO_DIR/test-reports"

  git -C "$REPO_DIR" init -q
  git -C "$REPO_DIR" config user.name "codex"
  git -C "$REPO_DIR" config user.email "codex@example.com"

  cat > "$REPO_DIR/.github/workflows/ci.yml" <<'EOF_WF'
name: ci
EOF_WF
  cat > "$REPO_DIR/docs/guide.md" <<'EOF_DOC'
# guide
EOF_DOC
  cat > "$REPO_DIR/src/unit.pas" <<'EOF_SRC'
unit unit1;
EOF_SRC
  cat > "$REPO_DIR/bin/generated" <<'EOF_BIN'
generated
EOF_BIN
  cat > "$REPO_DIR/scripts/tool.sh" <<'EOF_SCRIPT'
#!/usr/bin/env bash
echo tool
EOF_SCRIPT
  cat > "$REPO_DIR/test-reports/sample.md" <<'EOF_REPORT'
# sample
EOF_REPORT
  chmod +x "$REPO_DIR/scripts/tool.sh"

  git -C "$REPO_DIR" add .
  git -C "$REPO_DIR" commit -qm "init"

  echo "wf change" >> "$REPO_DIR/.github/workflows/ci.yml"
  echo "doc change" >> "$REPO_DIR/docs/guide.md"
  echo "src change" >> "$REPO_DIR/src/unit.pas"
  rm "$REPO_DIR/bin/generated"
  echo "script change" >> "$REPO_DIR/scripts/tool.sh"
  echo "report change" >> "$REPO_DIR/test-reports/sample.md"
  echo "misc" > "$REPO_DIR/notes.txt"
}

echo "[TEST] git status noise summary contract"
[[ -f "$SCRIPT" ]] || fail "summary script should exist"

setup_repo_fixture
rm -f "$OUT_FILE"

(cd /tmp && bash "$SCRIPT" --repo-root "$REPO_REL" --output "$OUT_REL" >/dev/null) || fail "summary script should succeed"

[[ -f "$OUT_FILE" ]] || fail "summary report should be written under project root"

assert_contains "$OUT_FILE" "| generated_artifacts_root_bin | 1 |"
assert_contains "$OUT_FILE" "| workflow_drift | 1 |"
assert_contains "$OUT_FILE" "| docs_drift | 1 |"
assert_contains "$OUT_FILE" "| source_edits | 1 |"
assert_contains "$OUT_FILE" "| scripts_drift | 1 |"
assert_contains "$OUT_FILE" "| test_reports_drift | 1 |"
assert_contains "$OUT_FILE" "| other | 1 |"
assert_contains "$OUT_FILE" "bin/generated"
assert_contains "$OUT_FILE" ".github/workflows/ci.yml"
assert_contains "$OUT_FILE" "docs/guide.md"
assert_contains "$OUT_FILE" "src/unit.pas"
assert_contains "$OUT_FILE" "test-reports/sample.md"
assert_contains "$OUT_FILE" "notes.txt"

echo "[PASS] git status noise summary contract passed"
