# Progress - Release Control Plane Realignment

## 2026-05-15

### Context Recovery

- `git status --short --branch`
  - result: `## master...origin/master [ahead 93]`
- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py /home/dtamade/projects/fafafa.ssl`
  - result: no additional catchup output
- `mcp__ace_tool__.search_context`
  - result: failed because `ACE_TOKEN` was invalid
  - action: switched to direct file inspection

### RED Verification

- `bash -n tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
  - result: PASS
- `bash tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
  - result: FAIL before doc edits
  - failure: `README is missing the current release-control navigation line`
- `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - result: FAIL before roadmap/doc edits
  - failure: missing new `current_execution_control_plane` roadmap truth
- `bash tests/scripts/test_platform_support_guidance_convergence_contract.sh`
  - result: FAIL before platform doc edits
  - failure: missing `当前发布与平台验证入口`
- `bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
  - result: FAIL before index edits
  - failure: missing the new Wave C closeout / historical section label

### Implementation

- Replaced the old Wave C canonical-entry contract with:
  - `tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
- Updated contracts:
  - `tests/scripts/test_active_roadmap_references_contract.sh`
  - `tests/scripts/test_platform_support_guidance_convergence_contract.sh`
  - `tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
- Realigned active docs:
  - `README.md`
  - `docs/README.md`
  - `docs/DOCUMENTATION_INDEX.md`
  - `docs/PLATFORM_SUPPORT.md`
  - `docs/ROADMAP.md`
  - `.github/README.md`
- Refreshed release-control plan truth:
  - `docs/plans/2026-05-12-release-v1.5.0-formalization.md`
- Added current batch plan:
  - `docs/plans/2026-05-15-release-control-plane-realignment.md`
- Reset root working-memory files to current-control-plane format:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

### GREEN Verification

- `bash tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
  - result: PASS
- `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - result: PASS
- `bash tests/scripts/test_platform_support_guidance_convergence_contract.sh`
  - result: PASS
- `bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
  - result: PASS
- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS
- `git diff --check`
  - result: PASS

### Pending

- final review conclusion
- git commit for this batch
