#!/bin/bash
# Remove set -e to allow all tests to run even if some fail
# set -e

echo "Running all tests..."
mkdir -p bin

failed_tests=0

for testfile in tests/*.pas; do
    # Skip WinSSL tests on Linux
    if [[ "$testfile" == *"winssl"* ]]; then
        echo "Skipping $testfile (WinSSL test)..."
        continue
    fi

    echo "----------------------------------------"
    echo "Compiling $testfile..."
    filename=$(basename "$testfile" .pas)
    if ! fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib "$testfile" -o"./bin/$filename" > /dev/null; then
        echo "Compilation failed for $testfile"
        failed_tests=$((failed_tests + 1))
        continue
    fi
    
    echo "Running $filename..."
    # Send Enter key to skip "Press Enter to exit"
    if ! echo "" | "./bin/$filename"; then
        echo "Test $filename FAILED"
        failed_tests=$((failed_tests + 1))
    else
        echo "Test $filename PASSED"
    fi
    echo "----------------------------------------"
done

if [ $failed_tests -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$failed_tests tests failed."
    exit 1
fi
