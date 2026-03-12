# Task Plan Next-Queue Dedup Finalization

## Goal
Reduce queue noise in `task_plan.md` by clearly marking historical queue sections as archived and keeping a single active queue section at the tail.

## Scope
- Modify: `task_plan.md`
- Evidence writeback:
  - `findings.md`
  - `progress.md`

## Steps
1. Replace all historical `### Next Queue ...` headings with `### Historical Next Queue ...`.
2. Mark all non-latest `### Active Queue ...` headings as `### Historical Active Queue ...`.
3. Keep only the latest active queue section as authoritative.
4. Verify heading distribution via:
   - `rg -n "^### Next Queue|^### Active Queue|^### Historical Next Queue|^### Historical Active Queue" task_plan.md`
