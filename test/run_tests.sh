#!/bin/sh

VLCC="../src/vlcc"
GCC="gcc"
PYTHON="python"

# Set time limit for all operations
ulimit -t 30

globallog="../testall.log"
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testall.sh [options] [.vl files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    echo "FAILED"
    error=1
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile. 
# Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
        SignalError "$1 differs"
        echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
        SignalError "failure: $*"
        return 1
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.vl//'`
    reffile=`echo $1 | sed 's/.vl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.c" &&
    Run "$VLCC" "-c" $1 &&
    referencedfiles="$(cat ${basename}.c | grep '#include \".*\.c\"' |
        sed 's/\#include *\"//' | sed 's/\.c\"/.c/')" &&
    generatedfiles="$generatedfiles $referencedfiles" &&
    for file in $referencedfiles; do 
        Run "$VLCC" "-c" "${file%.c}.vl";
    done &&
    generatedfiles="$generatedfiles ${basename}.o" &&
    Run "$GCC" "-c -fPIC" ${basename}.c &&
    generatedfiles="$generatedfiles ${basename}.so" &&
    Run "$GCC" "-shared -o" ${basename}.so ${basename}.o &&
    generatedfiles="$generatedfiles ${basename}.py" &&
    Run "$VLCC" "-d" $1 &&
    generatedfiles="$generatedfiles ${basename}.c.out" &&
    Run "$PYTHON" ${basename}.py ${basename}.in > ${basename}.c.out && 
    Compare ${basename}.c.out ${reffile}.out ${basename}.c.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
        if [ $keep -eq 0 ] ; then
            rm -f $generatedfiles
        fi
        echo "OK"
        echo "###### SUCCESS" 1>&2
    else
        echo "###### FAILED" 1>&2
        globalerror=$error
    fi
}

CheckPass() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.vl//'`
    reffile=`echo $1 | sed 's/.vl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2
    
    generatedfiles=""
    # Basically check if we can compile all of it, 
    # then stop short of any testing
    generatedfiles="$generatedfiles ${basename}.c" &&
    Run "$VLCC" "-c" $1 &&
    generatedfiles="$generatedfiles ${basename}.o" &&
    Run "$GCC" "-c -fPIC" ${basename}.c &&

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
        if [ $keep -eq 0 ] ; then
            rm -f $generatedfiles
        fi
        echo "OK"
        echo "###### SUCCESS" 1>&2
    else
        echo "###### FAILED" 1>&2
        globalerror=$error
    fi
}

SignalPass() {
    if [ $error -eq 1 ] ; then
        echo "OK"
        error=0
    fi
}

# RunFail <args>
# Report the command, run it, and report any errors
RunFail() {
    echo $* 1>&2
    eval $* && {
        SignalError "uncaught: $*" 
        return 1
    } || {
        SignalPass
        return 0
    }
}

CheckFail() {
    error=1
    basename=`echo $1 | sed 's/.*\\///
                             s/.vl//'`
    reffile=`echo $1 | sed 's/.vl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    RunFail "$VLCC" "-c" $1

    # Report the status and clean up the generated files
    if [ $error -eq 0 ] ; then
        if [ $keep -eq 0 ] ; then
            rm -f $generatedfiles
        fi
        echo "###### SUCCESS" 1>&2
    else
        echo "###### FAILED" 1>&2
        globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
        k) # Keep intermediate files
            keep=1
        ;;
        h) # Help
            Usage
        ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="./fail-*.vl ./pass-*.vl ./test-*.vl"
fi


for file in $files
do
    case $file in
        *test-*)
            Check $file 2>> $globallog
        ;;
        *fail-*)
            CheckFail $file 2>> $globallog
        ;;
        *pass-*)
            CheckPass $file 2>> $globallog
        ;;
        *)
            echo "unknown file type $file"
            globalerror=1
        ;;
    esac
    # Date and Time stamp for user log
    echo "Test completed at $(date '+%H:%M:%S on %m/%d/%y')" 1>> $globallog
done

exit $globalerror
