#!/bin/bash

ASSIGN=$1
shift

set +o pipefail

if [[ "$1" == "--release" ]]; then
    RELEASE="--release"
    PROGRAM="./target/release/ast"
    shift
else
    RELEASE=""
    PROGRAM="./target/ast"
fi

cargo build $RELEASE || exit $?

STDLIB="../stdlib/${ASSIGN}.0/"

if [[ $# > 0 ]]; then
    TESTCASES=("$@")
else
    TESTCASES=(../{assignment,custom}_testcases/a${ASSIGN}/J*)
fi

LOG=""
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
    "$PROGRAM" $MULTI $(find "$STDLIB" "$test" -name '*.java')
    CODE="$?"
    if [[ $CODE != $PASS_CODE ]]; then
        LOG="${LOG}$test expected exit code $PASS_CODE, got $CODE"$'\n'
        FAILURES=$(($FAILURES+1))
    fi
done

echo -n "${LOG}"
echo "${FAILURES} failures"
