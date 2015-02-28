#!/bin/bash

set +o pipefail

cargo build || exit $?

PROGRAM="./target/ast"

STDLIB="../stdlib/2.0/"

if [[ $# > 0 ]]; then
    TESTCASES=("$@")
else
    TESTCASES=(../{assignment,custom}_testcases/a2/J*)
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
    "$PROGRAM" $(find "$STDLIB" "$test" -name '*.java')
    CODE="$?"
    if [[ $CODE != $PASS_CODE ]]; then
        LOG="${LOG}$test: expected exit code $PASS_CODE, got $CODE"$'\n'
        FAILURES=$(($FAILURES+1))
    fi
done

echo -n "${LOG}"
echo "${FAILURES} failures"
