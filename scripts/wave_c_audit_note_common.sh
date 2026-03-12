#!/usr/bin/env bash

# Shared Wave C audit-note semantics for B147/B148/B149 scripts.

wave_c_map_alert_state_to_audit_note() {
  local alert_state="${1:-}"
  case "$alert_state" in
    WARN)
      echo "B148_ALERT_WARN_REVIEW_REQUIRED"
      ;;
    CLEAR)
      echo "B148_ALERT_CLEAR"
      ;;
    MISSING)
      echo "B148_ALERT_MISSING"
      ;;
    *)
      echo "B148_ALERT_UNKNOWN"
      ;;
  esac
}

wave_c_is_allowed_audit_note() {
  local audit_note="${1:-}"
  case "$audit_note" in
    B148_ALERT_WARN_REVIEW_REQUIRED|B148_ALERT_CLEAR|B148_ALERT_MISSING|B148_ALERT_UNKNOWN)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

wave_c_allowed_audit_notes_csv() {
  echo "B148_ALERT_WARN_REVIEW_REQUIRED, B148_ALERT_CLEAR, B148_ALERT_MISSING, B148_ALERT_UNKNOWN"
}

wave_c_is_missing_or_unknown_note() {
  local audit_note="${1:-}"
  [[ "$audit_note" == "MISSING" || "$audit_note" == "UNKNOWN" ]]
}

wave_c_compute_audit_note_sync_state() {
  local projected_note="${1:-UNKNOWN}"
  local actual_note="${2:-UNKNOWN}"

  if wave_c_is_missing_or_unknown_note "$projected_note"; then
    echo "MISSING"
  elif [[ "$projected_note" == "$actual_note" ]]; then
    echo "MATCH"
  else
    echo "MISMATCH"
  fi
}

wave_c_compute_audit_note_chain_consistency() {
  local b147_projected_note="${1:-UNKNOWN}"
  local b148_preview_note="${2:-UNKNOWN}"
  local b149_actual_note="${3:-UNKNOWN}"

  if wave_c_is_missing_or_unknown_note "$b147_projected_note" \
    || wave_c_is_missing_or_unknown_note "$b148_preview_note" \
    || wave_c_is_missing_or_unknown_note "$b149_actual_note"; then
    echo "MISSING"
  elif [[ "$b147_projected_note" == "$b148_preview_note" && "$b148_preview_note" == "$b149_actual_note" ]]; then
    echo "MATCH"
  else
    echo "MISMATCH"
  fi
}
