# Benchmark HKDF Activation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 `benchmark_crypto` 的 HKDF 基准从 placeholder 提示改为真实执行路径，直接使用 `TCryptoUtils.HKDF` 统计性能。

**Architecture:** 先用当前基准程序作为 RED 证据（输出仍含 `not yet implemented`），再最小修改 `BenchmarkHKDF`，最后复跑基准确认 HKDF 性能条目产出。

**Tech Stack:** FreePascal benchmark program, `tests/benchmarks/benchmark_crypto.pas`, `TCryptoUtils.HKDF`.

---

## Scan Summary (2026-02-12)
- 高信号缺口：`tests/benchmarks/benchmark_crypto.pas` 仍输出 `HKDF not yet implemented in TCryptoUtils`。
- 前置条件：Iteration 85 已实现 `TCryptoUtils.HKDF/TryHKDF`，可直接接入基准。

---

### Task 1 (P1): RED benchmark evidence

**Files:**
- Inspect: `tests/benchmarks/benchmark_crypto.pas`

**Step 1: Verify RED output**
- Run:
  - `fpc -Fu./src -Fu./tests/benchmarks tests/benchmarks/benchmark_crypto.pas -otmp/benchmark_crypto && ./tmp/benchmark_crypto`
- Expected:
  - HKDF 段输出 `HKDF not yet implemented in TCryptoUtils`。

---

### Task 2 (P1): Minimal benchmark activation

**Files:**
- Modify: `tests/benchmarks/benchmark_crypto.pas`

**Step 1: Minimal implementation**
- 将 `BenchmarkHKDF` 改为真实循环：
  - 构造 `salt/info`（UTF-8 bytes）
  - 调用 `TCryptoUtils.HKDF(LData1KB, LSalt, LInfo, 32, HASH_SHA256)`
  - 使用 `TBenchmark.Report` 输出 ops/sec

**Step 2: Verify GREEN**
- 重跑 Task 1 命令，期望：
  - HKDF 段输出 `HKDF-SHA256 (32 bytes x 1000)` 性能结果。

---

## Acceptance
- 基准输出不再出现 `not yet implemented`。
- HKDF 性能项可稳定执行并输出 ops/sec。

## Execution Record (2026-02-12 14:10 CST)

### RED
- `fpc -Fu./src -Fu./tests/benchmarks tests/benchmarks/benchmark_crypto.pas -otmp/benchmark_crypto && ./tmp/benchmark_crypto`
- 关键输出：
  - `HKDF not yet implemented in TCryptoUtils`

### GREEN
- 修改：`tests/benchmarks/benchmark_crypto.pas`
  - `BenchmarkHKDF` 改为真实 HKDF 循环压测（`TCryptoUtils.HKDF`）
- 复跑通过：
  - 输出 `HKDF-SHA256 (32 bytes x 1000) : ... ops/sec`
  - 不再出现 `not yet implemented` 文案
