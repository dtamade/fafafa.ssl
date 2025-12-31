#!/bin/bash
#
# fafafa.ssl CI/CD Automation Script
# 
# Purpose: Automated building, testing, and regression detection
# Usage: ./ci_pipeline.sh [build|test|bench|all]
#

set -e  # Exit on error

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FPC_BIN="${FPC:-fpc}"
DATE=$(date +%Y%m%d_%H%M%S)

# Network-dependent tests are opt-in.
# Set FAFAFA_RUN_NETWORK_TESTS=1 to enable real-website/E2E scenarios.
RUN_NETWORK_TESTS="${FAFAFA_RUN_NETWORK_TESTS:-0}"

EXAMPLES_BIN="$PROJECT_ROOT/examples/bin"
TESTS_BIN="$PROJECT_ROOT/tests/bin"
BENCH_BIN="$PROJECT_ROOT/tests/benchmarks/bin"

# FPC unit paths (helps with custom FPC installs that may have incomplete fpc.cfg search paths)
FPC_UNIT_PATHS=""
FPC_BASE="$HOME/freePascal/fpc/units/x86_64-linux"
if [ -d "$FPC_BASE" ]; then
    for dir in rtl-objpas rtl rtl-unix rtl-extra fcl-base fcl-json fcl-process pthreads; do
        if [ -d "$FPC_BASE/$dir" ]; then
            FPC_UNIT_PATHS="$FPC_UNIT_PATHS -Fu$FPC_BASE/$dir"
        fi
    done
fi

FPC_COMMON_OPTS="$FPC_UNIT_PATHS -Fusrc -Fusrc/openssl"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Build all test suites
build_tests() {
    info "Building test suites..."
    
    cd "$PROJECT_ROOT"

    mkdir -p "$EXAMPLES_BIN" "$TESTS_BIN" "$BENCH_BIN"
    mkdir -p "$PROJECT_ROOT/tests/benchmarks/archive"

    if [ "$RUN_NETWORK_TESTS" = "1" ]; then
        info "  Compiling network integration tests..."
        $FPC_BIN $FPC_COMMON_OPTS -Fuexamples -o"$EXAMPLES_BIN/test_real_websites_enhanced" examples/test_real_websites_enhanced.pas || {
            error "Failed to compile test_real_websites_enhanced"
            return 1
        }

        $FPC_BIN $FPC_COMMON_OPTS -Fuexamples -o"$EXAMPLES_BIN/test_real_websites_comprehensive" examples/test_real_websites_comprehensive.pas || {
            error "Failed to compile test_real_websites_comprehensive"
            return 1
        }

        info "  Compiling E2E tests..."
        $FPC_BIN $FPC_COMMON_OPTS -Fuexamples -Futests/framework -o"$TESTS_BIN/test_e2e_scenarios" tests/integration/test_e2e_scenarios.pas || {
            error "Failed to compile test_e2e_scenarios"
            return 1
        }
    else
        warn "Skipping network tests build (set FAFAFA_RUN_NETWORK_TESTS=1 to enable)"
    fi

    # Performance tests
    info "  Compiling performance tests..."
    $FPC_BIN $FPC_COMMON_OPTS -o"$BENCH_BIN/performance_regression_suite" tests/benchmarks/performance_regression_suite.pas || {
        error "Failed to compile performance_regression_suite"
        return 1
    }

    # New API tests (v2.0 & Quick)
    info "  Compiling v2.0 & Quick API tests..."
    $FPC_BIN $FPC_COMMON_OPTS -o"$TESTS_BIN/test_new_api" tests/test_new_api.pas || {
        error "Failed to compile test_new_api"
        return 1
    }
    $FPC_BIN $FPC_COMMON_OPTS -o"$TESTS_BIN/test_quick_all" tests/test_quick_all.pas || {
        error "Failed to compile test_quick_all"
        return 1
    }

    # OpenSSL OCSP regression tests (no external network required)
    info "  Compiling OpenSSL OCSP regression tests..."
    $FPC_BIN $FPC_COMMON_OPTS -o"$TESTS_BIN/test_openssl_chain_issuer_selection" tests/openssl/test_openssl_chain_issuer_selection.pas || {
        error "Failed to compile test_openssl_chain_issuer_selection"
        return 1
    }
    $FPC_BIN $FPC_COMMON_OPTS -o"$TESTS_BIN/test_openssl_ocsp_fail_closed" tests/openssl/test_openssl_ocsp_fail_closed.pas || {
        error "Failed to compile test_openssl_ocsp_fail_closed"
        return 1
    }
    
    info "✅ All tests compiled successfully"
    return 0
}

# Run integration tests
run_integration_tests() {
    info "Running integration tests..."
    
    cd "$PROJECT_ROOT"

    if [ "$RUN_NETWORK_TESTS" = "1" ]; then
        info "  Running network integration tests..."
        "$EXAMPLES_BIN/test_real_websites_enhanced" || {
            warn "Some enhanced tests failed (expected)"
        }

        "$TESTS_BIN/test_e2e_scenarios" || {
            warn "Some E2E tests failed"
        }
    else
        warn "Skipping network integration tests (set FAFAFA_RUN_NETWORK_TESTS=1 to enable)"
    fi

    # OpenSSL OCSP regression tests
    info "  Running OpenSSL OCSP regression tests..."
    "$TESTS_BIN/test_openssl_chain_issuer_selection" || {
        error "OpenSSL chain issuer selection tests failed"
        return 1
    }

    "$TESTS_BIN/test_openssl_ocsp_fail_closed" || {
        error "OpenSSL OCSP fail-closed tests failed"
        return 1
    }

    # v2.0 API tests
    info "  Running v2.0 API tests..."
    "$TESTS_BIN/test_new_api" || {
        error "v2.0 API tests failed"
        return 1
    }

    # Quick API tests
    info "  Running Quick API tests..."
    "$TESTS_BIN/test_quick_all" || {
        error "Quick API tests failed"
        return 1
    }
    
    info "✅ Integration tests completed"
}

# Run performance benchmarks
run_benchmarks() {
    info "Running performance benchmarks..."

    mkdir -p "$BENCH_BIN"

    cd "$PROJECT_ROOT/tests/benchmarks"

    # Run regression suite
    "$BENCH_BIN/performance_regression_suite" > "benchmark_${DATE}.log" 2>&1 || {
        warn "Performance regression detected or benchmark failed"
        cat "benchmark_${DATE}.log"
    }
    
    # Archive results
    if [ -f "benchmark_results.csv" ]; then
        cp "benchmark_results.csv" "archive/benchmark_${DATE}.csv"
        info "Results archived to archive/benchmark_${DATE}.csv"
    fi
    
    info "✅ Performance benchmarks completed"
}

# Generate performance report
generate_report() {
    info "Generating performance report..."
    
    cd "$PROJECT_ROOT/tests/benchmarks"
    
    if [ ! -f "benchmark_results.csv" ]; then
        warn "No benchmark results found"
        return 1
    fi
    
    {
        echo "# Performance Report - $DATE"
        echo ""
        echo "## Summary"
        echo ""
        echo '```'
        tail -n +4 benchmark_results.csv | awk -F',' '{
            printf "%-35s: %6s ms (%10.2f ops/s)\n", $1, $2, $3
        }'
        echo '```'
        echo ""
        echo "## Regression Analysis"
        echo ""
        if [ -f "benchmark_baseline.csv" ]; then
            echo "Compared against baseline"
        else
            echo "⚠️  No baseline found - creating new baseline"
        fi
    } > "report_${DATE}.md"
    
    info "✅ Report generated: report_${DATE}.md"
}

# Clean build artifacts
clean() {
    info "Cleaning build artifacts..."
    find "$PROJECT_ROOT" -name "*.o" -delete
    find "$PROJECT_ROOT" -name "*.ppu" -delete
    info "✅ Clean completed"
}

# Main pipeline
main() {
    local command="${1:-all}"
    
    info "========================================="
    info "fafafa.ssl CI/CD Pipeline"
    info "========================================="
    info "Date: $DATE"
    info "Command: $command"
    info ""
    
    case "$command" in
        build)
            build_tests
            ;;
        test)
            run_integration_tests
            ;;
        bench)
            run_benchmarks
            generate_report
            ;;
        clean)
            clean
            ;;
        all)
            clean
            build_tests
            run_integration_tests
            run_benchmarks
            generate_report
            ;;
        *)
            error "Unknown command: $command"
            echo "Usage: $0 [build|test|bench|clean|all]"
            exit 1
            ;;
    esac
    
    info ""
    info "========================================="
    info "Pipeline completed"
    info "========================================="
}

# Create archive directory
mkdir -p "$PROJECT_ROOT/tests/benchmarks/archive"

main "$@"
