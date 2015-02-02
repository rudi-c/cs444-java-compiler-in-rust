#!/bin/bash

cat /dev/null > log
for test in ../assignment_testcases/a1/Je*
do
    cargo run $test
    if [ $? -eq 0 ]; then
        echo "Marmoset test case $test did not fail when it should have." >> log
    fi
done

for test in ../custom_testcases/a1/Je*
do
    cargo run $test
    if [ $? -eq 0 ]; then
        echo "Custom test case $test did not fail when it should have." >> log
    fi
done

cat log
