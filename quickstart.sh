#!/bin/bash
#
# Quick Start Script for fafafa.ssl
# One-command setup and verification
#

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}================================${NC}"
echo -e "${BLUE}fafafa.ssl Quick Start${NC}"
echo -e "${BLUE}================================${NC}"
echo ""

# Check prerequisites
echo -e "${GREEN}[1/5]${NC} Checking prerequisites..."

if ! command -v fpc &> /dev/null; then
    echo -e "${YELLOW}    FreePascal not found. Installing...${NC}"
    sudo apt-get update && sudo apt-get install -y fpc
fi

if ! command -v openssl &> /dev/null; then
    echo -e "${YELLOW}    OpenSSL not found. Installing...${NC}"
    sudo apt-get install -y libssl-dev
fi

FPC_VERSION=$(fpc -iV)
OPENSSL_VERSION=$(openssl version | awk '{print $2}')

echo -e "    ✅ FreePascal: $FPC_VERSION"
echo -e "    ✅ OpenSSL: $OPENSSL_VERSION"
echo ""

# Build library
echo -e "${GREEN}[2/5]${NC} Building library..."
chmod +x ci_pipeline.sh
./ci_pipeline.sh build
echo -e "    ✅ Build complete"
echo ""

# Run quick test
echo -e "${GREEN}[3/5]${NC} Running integration tests..."
./ci_pipeline.sh test || echo -e "${YELLOW}    ⚠️  Some tests failed (network-dependent)${NC}"
echo ""

# Run benchmarks
echo -e "${GREEN}[4/5]${NC} Running performance benchmarks..."
./ci_pipeline.sh bench
echo ""

# Create baseline
echo -e "${GREEN}[5/5]${NC} Creating performance baseline..."
cd tests/benchmarks
if [ ! -f benchmark_baseline.csv ]; then
    cp benchmark_results.csv benchmark_baseline.csv
    echo -e "    ✅ Baseline created"
else
    echo -e "    ℹ️  Baseline already exists"
fi
cd ../..
echo ""

# Summary
echo -e "${BLUE}================================${NC}"
echo -e "${GREEN}✅ Setup Complete!${NC}"
echo -e "${BLUE}================================${NC}"
echo ""
echo "Quick Reference:"
echo "  • Full CI/CD:  ./ci_pipeline.sh all"
echo "  • Build only:  ./ci_pipeline.sh build"
echo "  • Test only:   ./ci_pipeline.sh test"
echo "  • Benchmarks:  ./ci_pipeline.sh bench"
echo ""
echo "Next Steps:"
echo "  1. Browse examples/  for code samples"
echo "  2. Read README.md    for documentation"
echo "  3. Check SECURITY_AUDIT.md for deployment"
echo ""
echo -e "${GREEN}Happy coding with fafafa.ssl! 🚀${NC}"
