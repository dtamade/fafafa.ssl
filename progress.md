# Progress - v1.5.0 Direct Merge

## 2026-05-15

### Direct Merge Decision

- user decision:
  - skip the PR route
  - close `#13`
  - merge `release/v1.5.0-prep-2026-05-15` directly into local `master`

### Context Recovery

- `git status --short --branch`
  - result: `## release/v1.5.0-prep-2026-05-15...origin/release/v1.5.0-prep-2026-05-15`
- `git rev-parse --short HEAD`
  - result: `2b31832`
- `git branch --show-current`
  - result: `release/v1.5.0-prep-2026-05-15`

### Working-Memory Recovery

- `python3 /home/dtamade/.codex/skills/planning-with-files/scripts/session-catchup.py "$(pwd)"`
  - result: no output
- `sed -n '1,220p' docs/test_reports/RELEASE_PREP_HANDOFF_V1.5.0_2026-05-15.md`
  - result: confirmed `PUSHED_READY_FOR_APPROVAL`
- `sed -n '1,220p' docs/test_reports/RELEASE_READINESS_V1.5.0.md`
  - result: confirmed `READY_FOR_MAIN_MERGE`
- `sed -n '1,180p' docs/ROADMAP.md`
  - result: confirmed `CLOSED_OUT_PENDING_APPROVAL` and current control plane

### GitHub PR Reality

- `gh pr list --head release/v1.5.0-prep-2026-05-15 --state all --json number,title,state,baseRefName,headRefName,url`
  - result: `[]`
- `.github/pull_request_template.md` / `.github/PULL_REQUEST_TEMPLATE/`
  - result: no PR template found
- `gh api repos/dtamade/fafafa.ssl/branches/master/protection -H 'Accept: application/vnd.github+json'`
  - result: `403`
  - note: branch protection cannot be auto-discovered via current API access level

### Tooling Constraint

- `mcp__ace_tool__.search_context`
  - result: FAIL
  - error: `ACE_TOKEN` 失效或无效

### PR Closure

- `gh pr close 13 --comment "Superseded by the user-approved direct merge route. We are closing this PR and merging the release-prep branch locally into master instead."`
  - result: PASS
- `gh pr view 13 --json number,title,state,url,mergeStateStatus,reviewDecision,headRefName,baseRefName`
  - result: PASS
  - summary: state=`CLOSED`, base=`master`, head=`release/v1.5.0-prep-2026-05-15`

### In Progress

- add direct-merge plan doc
- rewrite approval packet to historical closed state
- rewrite root working-memory to direct merge batch
- commit direct-merge metadata
- push `master` and verify remote status

### Direct Merge Focused Verification

- `bash tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
  - result: PASS
- `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - result: PASS
- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS
- `git diff --check`
  - result: PASS
- `git status --short`
  - result: only expected direct-merge doc and working-memory changes are present

### Direct Merge Metadata Commit

- review conclusion:
  - no production-code behavior changed in this batch
  - this batch only switched delivery route from PR approval to direct merge
  - focused release-control contracts remained green
- `git commit -m "docs: switch v1.5.0 to direct merge route"`
  - result: `2de9ded`
- `git push`
  - result: PASS
  - remote update: `34a83c6..2de9ded`

### Merge To master

- review conclusion:
  - this merge does not introduce a new implementation batch
  - it only brings the finalized release-prep control-plane history back to `master`
  - the real remaining risk stays external to the repo: GitHub Actions billing/startup failure
- `git switch master`
  - result: PASS
- `git merge --no-ff release/v1.5.0-prep-2026-05-15 -m "merge: finalize v1.5.0 direct merge route"`
  - result: PASS
  - merge commit: `ddd475b`
- `git branch -vv`
  - result: local `master` is now `ahead 100` vs `origin/master`

### Focused Verification

- `bash tests/scripts/test_release_control_entrypoint_convergence_contract.sh`
  - result: PASS
- `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - result: PASS
- `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - result: PASS
- `git diff --check`
  - result: PASS
- `git status --short`
  - result: only expected doc and working-memory changes are present

### Commit And Push

- review conclusion:
  - no production-code behavior changed in this batch
  - focused release-control contracts remained green
  - this batch only added PR approval assets and moved the branch into a reviewable approval state
- `git commit -m "docs: prepare v1.5.0 PR approval packet"`
  - result: `9c8ce1c`
- `git push`
  - result: PASS
  - remote update: `2b31832..9c8ce1c`

### PR Creation

- `gh pr create --base master --head release/v1.5.0-prep-2026-05-15 --title "release: request v1.5.0 merge approval" --body-file docs/test_reports/PR_APPROVAL_PACKET_V1.5.0_2026-05-15.md`
  - result: PASS
  - PR: `#13`
  - URL: `https://github.com/dtamade/fafafa.ssl/pull/13`
- `gh pr view release/v1.5.0-prep-2026-05-15 --json number,title,state,url,mergeStateStatus,reviewDecision,headRefName,baseRefName`
  - result: FAIL
  - error: `no pull requests found for branch "release/v1.5.0-prep-2026-05-15"`
- `gh pr view 13 --json number,title,state,url,mergeStateStatus,reviewDecision,headRefName,baseRefName`
  - result: PASS
  - summary: base=`master`, head=`release/v1.5.0-prep-2026-05-15`, state=`OPEN`, mergeStateStatus=`UNSTABLE`

### GitHub-side Blocker

- `gh pr checks 13`
  - result: FAIL
  - note: all affected jobs failed before startup, not after running branch code
  - failing jobs:
    - `Minimal Gate (Linux)`
    - `FreePascal TLS 1.3 Completeness`
    - `Code Quality (Light)`
    - `tls13-signer-gate`
  - shared annotation: recent account payments failed or spending limit needs to be increased

### PR Body Refresh

- `gh pr edit 13 --title "release: request v1.5.0 merge approval" --body-file docs/test_reports/PR_APPROVAL_PACKET_V1.5.0_2026-05-15.md`
  - result: FAIL
  - error: GraphQL classic Projects deprecation on `repository.pullRequest.projectCards`
- `gh api repos/dtamade/fafafa.ssl/pulls/13 --method PATCH --raw-field title='release: request v1.5.0 merge approval' --raw-field body=\"$BODY\"`
  - result: PASS
  - note: REST API workaround successfully updated the PR title/body
- `gh api repos/dtamade/fafafa.ssl/pulls/13 --jq '{updated_at: .updated_at, title: .title, body: .body}'`
  - result: PASS
  - summary: remote PR body now matches the checked-in approval packet including PR metadata and billing blocker note
