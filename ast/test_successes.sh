#!/bin/bash

cargo build || exit $?

PROGRAM="./target/ast"

echo -n > log
for test in ../assignment_testcases/a1/J1*
do
    echo "Running test $test."
    "$PROGRAM" "$test"
    if [ $? -ne 0 ]; then
        echo "Marmoset test case $test did not succeed when it should have." >> log
    fi
done

for test in ../custom_testcases/a1/J1*
do
    echo "Running test $test."
    "$PROGRAM" "$test"
    if [ $? -ne 0 ]; then
        echo "Custom test case $test did not succeed when it should have." >> log
    fi
done

cat log
