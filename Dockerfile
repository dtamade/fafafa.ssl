# fafafa.ssl Docker Support

FROM ubuntu:22.04

LABEL maintainer="fafafa.ssl team"
LABEL description="Production-ready SSL/TLS library for FreePascal"
LABEL version="2.0.0"

# Install dependencies
RUN apt-get update && apt-get install -y \
    fpc \
    libssl-dev \
    ca-certificates \
    git \
    && rm -rf /var/lib/apt/lists/*

# Create workspace
WORKDIR /opt/fafafa.ssl

# Copy source code
COPY src/ ./src/
COPY examples/ ./examples/
COPY tests/ ./tests/
COPY ci_pipeline.sh ./
COPY README.md ./

# Make scripts executable
RUN chmod +x ci_pipeline.sh

# Set environment
ENV FPC=/usr/bin/fpc
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# Run tests by default
CMD ["./ci_pipeline.sh", "all"]

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD fpc -i || exit 1

# Expose nothing (library only)
# EXPOSE none

# Volume for benchmark results
VOLUME ["/opt/fafafa.ssl/tests/benchmarks"]

# Example usage:
# docker build -t fafafa-ssl .
# docker run --rm fafafa-ssl
# docker run --rm -v $(pwd)/results:/opt/fafafa.ssl/tests/benchmarks fafafa-ssl
