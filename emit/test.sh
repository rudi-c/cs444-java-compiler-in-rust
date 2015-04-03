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
    TESTCASES=(../{assignment,custom}_testcases/a${ASSIGN}/J*)
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
        "$PROGRAM" $MULTI "${STDLIB[@]}" "$test" > $TEST_ASM_FILE
    else
        "$PROGRAM" $MULTI "${STDLIB[@]}" "$test"/**/*.java > $TEST_ASM_FILE
    fi
    CODE="$?"
    if [[ $CODE != $PASS_CODE ]]; then
        LOG="${LOG}$test expected exit code $PASS_CODE, got $CODE"$'\n'
        FAILURES=$(($FAILURES+1))
        continue
    fi

    nasm -O1 -f elf -g -F dwarf $TEST_ASM_FILE
    CODE="$?"
    if [[ $CODE != $PASS_CODE ]]; then
        LOG="${LOG}$test failed at assembly"$'\n'
        FAILURES=$(($FAILURES+1))
    fi
done

echo -n "${LOG}"
echo "${FAILURES} failures"
