#!/bin/bash

ASSIGN=$1
shift

shopt -s globstar
trap 'wait; echo "interrupted"; exit 1' SIGINT

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

LOG=()
TEST_ASM_FILE="output/test.s"
for test in "${TESTCASES[@]}"; do
    echo "running on $test"
    git clean -Xdqf output
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
        TESTS=("$test")
    else
        TESTS=("$test"/**/*.java)
    fi
    "$PROGRAM" $MULTI "${TESTS[@]}" "${STDLIB[@]}" > $TEST_ASM_FILE
    CODE="$?"
    if [[ $CODE != $PASS_CODE ]]; then
        LOG+=("$test (compile) expected exit code $PASS_CODE, got $CODE")
        continue
    fi

    if [[ $CODE == 0 ]]; then
        if ! nasm -O1 -f elf -g -F dwarf $TEST_ASM_FILE; then
            LOG+="$test (assemble) failed at assembly"
        else
            nasm -O1 -f elf -g -F dwarf -o output/runtime.o ../stdlib/5.0/runtime.s || exit $?
            ld output/test.o output/runtime.o -melf_i386 -o output/a.out || exit $?
            ./output/a.out > output/actual_output.txt
            ACTUAL_EXIT_CODE="$?"

            CLASS_NAME=`basename ${TESTS[0]} .java`
            sed s/CLASSNAME/"$CLASS_NAME"/g TesterTemplate.java > output/Tester.java
            javac -d output "${TESTS[@]}" output/Tester.java
            java -classpath output Tester > output/expected_output.txt
            EXPECTED_EXIT_CODE="$?"

            if [[ $ACTUAL_EXIT_CODE != $EXPECTED_EXIT_CODE ]]; then
                LOG+=("$test (run) expected exit code $EXPECTED_EXIT_CODE, got exit code $ACTUAL_EXIT_CODE")
            elif ! cmp --silent output/actual_output.txt output/expected_output.txt; then
                LOG+=("$test (run) expected output:"$'\n'"$(<output/expected_output.txt)"$'\n'"got output"$'\n'"$(<output/actual_output.txt)")
            fi
        fi
    fi
done

printf '%s\n' "${LOG[@]}"
echo "${#LOG[@]} failures"
