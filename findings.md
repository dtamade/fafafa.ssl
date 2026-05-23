# Findings

## Conclusions
- `scripts/compile_all_modules.py` was tripping an FPC internal exception because all batch compiles shared one `-FU` directory.
- Per-unit output isolation fixed the instability without changing the actual compile command semantics for any single module.
- The batch compile gate is now fully green: `186/186 PASS`.
- The isolation contract now locks the behavior so future changes do not reintroduce the shared-output failure mode.

## Notes
- `fafafa.ssl.pkcs11.engine.pas` is not the root cause by itself; it was the first file that exposed the shared-output collision pattern.
