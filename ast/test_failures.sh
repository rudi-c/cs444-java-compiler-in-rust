#!/bin/bash

cargo build || exit $?

PROGRAM="./target/ast"

echo -n > log
for test in ../assignment_testcases/a1/Je*
do
    "$PROGRAM" "$test"
    cargo run $test
    code=$?
    if [ $code -eq 0 ]; then
        echo "Marmoset test case $test did not fail when it should have." >> log
    elif [ $code -ne 42 ]; then
        echo "Test $test failed without returning exit code 42." >> log
    fi
done

for test in ../custom_testcases/a1/Je*
do
    "$PROGRAM" "$test"
    cargo run $test
    code=$?
    if [ $code -eq 0 ]; then
        echo "Custom test case $test did not fail when it should have." >> log
    elif [ $code -ne 42 ]; then
        echo "Test $test failed without returning exit code 42." >> log
    fi
done

cat log
