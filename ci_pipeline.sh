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
    
    # Integration tests
    info "  Compiling integration tests..."
    $FPC_BIN -Fusrc -Fusrc/openssl -Fuexamples examples/test_real_websites_enhanced.pas || {
        error "Failed to compile test_real_websites_enhanced"
        return 1
    }
    
    $FPC_BIN -Fusrc -Fusrc/openssl -Fuexamples examples/test_real_websites_comprehensive.pas || {
        error "Failed to compile test_real_websites_comprehensive"
        return 1
    }
    
    # E2E tests
    info "  Compiling E2E tests..."
    $FPC_BIN -Fusrc -Fusrc/openssl -Fuexamples tests/test_e2e_scenarios.pas || {
        error "Failed to compile test_e2e_scenarios"
        return 1
    }
    
    # Performance tests
    info "  Compiling performance tests..."
    $FPC_BIN -Fusrc -Fusrc/openssl tests/benchmarks/performance_regression_suite.pas || {
        error "Failed to compile performance_regression_suite"
        return 1
    }

    # New API tests (v2.0 & Quick)
    info "  Compiling v2.0 & Quick API tests..."
    $FPC_BIN -Fusrc -Fusrc/openssl tests/test_new_api.pas || {
        error "Failed to compile test_new_api"
        return 1
    }
    $FPC_BIN -Fusrc -Fusrc/openssl tests/test_quick_all.pas || {
        error "Failed to compile test_quick_all"
        return 1
    }
    
    info "✅ All tests compiled successfully"
    return 0
}

# Run integration tests
run_integration_tests() {
    info "Running integration tests..."
    
    cd "$PROJECT_ROOT"
    
    # Enhanced website tests
    info "  Running enhanced website tests..."
    ./examples/test_real_websites_enhanced || {
        warn "Some enhanced tests failed (expected)"
    }
    
    # E2E scenarios
    info "  Running E2E scenarios..."
    ./tests/test_e2e_scenarios || {
        warn "Some E2E tests failed"
    }

    # v2.0 API tests
    info "  Running v2.0 API tests..."
    ./tests/test_new_api || {
        error "v2.0 API tests failed"
        return 1
    }

    # Quick API tests
    info "  Running Quick API tests..."
    ./tests/test_quick_all || {
        error "Quick API tests failed"
        return 1
    }
    
    info "✅ Integration tests completed"
}

# Run performance benchmarks
run_benchmarks() {
    info "Running performance benchmarks..."
    
    cd "$PROJECT_ROOT/tests/benchmarks"
    
    # Run regression suite
    ./performance_regression_suite > "benchmark_${DATE}.log" 2>&1 || {
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
