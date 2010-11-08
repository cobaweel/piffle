#!/bin/bash

# This script test all the Piffle programs specified on the command
# line. It expects PIFFLEBOILER and PFC to be set. It is normally
# called from the Makefile.

cat <<EOF

----------------------------------------------------------------------
                        PIFFLE TEST SUITE
----------------------------------------------------------------------

EOF

scream () {
    cat <<EOF

**********************************************************************
**********************************************************************
             I FOUND A BUG! I FOUND A BUUUUUG!
                          $1
               This is not supposed to happen.
          Please report bugs to nearest exterminator.
**********************************************************************
**********************************************************************

EOF
}

# Run the C compiler

do_cc () {
    grep -q 'null_' <<< $1 || cc -g -Wall -o $2 $1 -lpcap
}

# Check whether a test is supposed to fail or succeed

is_supposed_to_be_bad () {
    grep -q 'bad' <<< $1
}

# Run a compiled test on some input

test_input () {
    test=$1
    s=${2%.in}

    grep -q 'null_' <<< $test ||\
    grep -q '\*' <<< $s ||\
        (
        echo "Testing $s.in ..."
        if is_supposed_to_be_bad $s; then
            (./$test -o - -f - < $s.in > $s.output 2>/dev/null) &&\
                diff $s.output $s.out >/dev/null &&\
                scream $s
        else
            ./$test -o - -f - < $s.in > $s.output &&\
                diff $s.output $s.out ||\
                scream $s
        fi        
    )
}

# Compile a test, and run it on input if available

test_pfl () {
    testpfl=$1
    test=${testpfl%.pfl}
    testc=${test}.c
    boiler=-B`cut -d_ -f1 <<< $test`.c
    if [ $boiler = "-Bnull.c" ]; then boiler=""; fi

    if is_supposed_to_be_bad $test; then 
        echo "Testing $test ..."
        $PFC $boiler $testpfl 2>/dev/null &&\
            do_cc $testc $test 2>>/dev/null &&\
            scream $test
    else
        CPU=`$PFC -C0 $testpfl 2>&1 | cut -d\  -f4`
        MEM=`$PFC -M0 $testpfl 2>&1 | cut -d\  -f4`
        echo "Testing $test [cpu=$CPU mem=$MEM]..."
        $PFC $boiler $testpfl &&\
            do_cc $testc $test ||\
            scream $test
        for input in ${test}_*.in ; do test_input $test $input; done

    fi
}

# Compile and run all available tests

for pfl in *.pfl; do test_pfl $pfl; done


