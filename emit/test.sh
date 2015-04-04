#!/bin/bash

ASSIGN=$1
shift

shopt -s globstar

if [[ "$1" == "--release" ]]; then
    RELEASE="--release"
    PROGRAM="./target/release/emit"
    shift
else
    RELEASE=""
    PROGRAM="./target/emit"
fi

cargo build $RELEASE || exit $?

STDLIB=("../stdlib/${ASSIGN}.0/"**/*.java)

if [[ $# > 0 ]]; then
    TESTCASES=("$@")
else
    TESTCASES=(../{assignment,custom}_testcases/a${ASSIGN}/J*.java)
fi

LOG=""
TEST_ASM_FILE="test.s"
FAILURES=0
for test in "${TESTCASES[@]}"; do
    echo "running on $test"
    if [[ "$(basename "$test")" =~ ^Je ]]; then
        PASS_CODE=42
    else
        PASS_CODE=0
    fi
    if [[ "$test" =~ custom ]]; then
        MULTI="--multi"
    else
        MULTI=""
    fi
    if [[ -f "$test" ]]; then
        "$PROGRAM" $MULTI "$test" "${STDLIB[@]}" > $TEST_ASM_FILE
    else
        "$PROGRAM" $MULTI "$test"/**/*.java "${STDLIB[@]}" > $TEST_ASM_FILE
    fi
    CODE="$?"
    if [[ $CODE != $PASS_CODE ]]; then
        LOG="${LOG}$test expected exit code $PASS_CODE, got $CODE"$'\n'
        FAILURES=$(($FAILURES+1))
        continue
    fi

    if [[ $CODE == 0 ]]; then
        if ! nasm -O1 -f elf -g -F dwarf $TEST_ASM_FILE; then
            LOG="${LOG}$test failed at assembly"$'\n'
            FAILURES=$(($FAILURES+1))
        else
            ld test.o runtime.o -melf_i386
            ./a.out > actual_output.txt
            ACTUAL_EXIT_CODE="$?"

            CLASS_NAME=`basename ${test[0]} .java`
            sed s/CLASSNAME/"$CLASS_NAME"/g TesterTemplate.java > Tester.java
            javac "${test[0]}" Tester.java
            java -classpath `dirname "${test[0]}"`:. Tester > expected_output.txt
            EXPECTED_EXIT_CODE="$?"

            if [[ $ACTUAL_EXIT_CODE != $EXPECTED_EXIT_CODE ]]; then
                LOG="${LOG}$test failed: expected exit code $EXPECTED_EXIT_CODE, got exit code $ACTUAL_EXIT_CODE"$'\n'
                FAILURES=$(($FAILURES+1))
            elif ! cmp --silent actual_output.txt expected_output.txt; then
                LOG="${LOG}$test failed: expected output:"$'\n'`cat expected_output.txt`$'\n'"got output"$'\n'`cat actual_output.txt`$'\n'
                FAILURES=$(($FAILURES+1))
            fi
        fi
    fi
done

echo -n "${LOG}"
echo "${FAILURES} failures"
