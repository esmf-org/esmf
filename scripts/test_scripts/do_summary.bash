#!/bin/bash
# $Id: do_summary.bash,v 1.2 2004/07/06 19:41:02 svasquez Exp $

# which summary to say what seems to have build and run in the examples,
# unit tests, and system tests

export platform=${ESMF_ARCH}.${ESMF_COMPILER}.${ESMF_PREC}.${ESMF_SITE}
export utdir=$ESMF_DIR/test/test${ESMF_BOPT}/$platform
export exdir=$ESMF_DIR/examples/examples${ESMF_BOPT}/$platform
export stdir=$ESMF_DIR/test/test${ESMF_BOPT}/$platform

# how many executables found
ut_exe_count=`ls $utdir/*UTest | wc -l`
ex_exe_count=`ls $exdir/*Ex    | wc -l`
st_exe_count=`ls $stdir/*STest | wc -l`

# how many output files found
ut_stdout_count=`ls $utdir/*UTest.stdout | wc -l`
ex_stdout_count=`ls $exdir/*Ex.stdout    | wc -l`
st_stdout_count=`ls $stdir/*STest.stdout | wc -l`

# how many output files with PASS found
ut_pass_count=`fgrep -l " PASS " $utdir/*UTest.stdout | wc -l`
ex_pass_count=`fgrep -l " PASS:" $exdir/*Ex.stdout | wc -l`
st_pass_count=`fgrep -l " PASS " $utdir/*STest.stdout | wc -l`

# how many output files with FAIL found
ut_fail_count=`fgrep -l " FAIL " $utdir/*UTest.stdout | wc -l`
ex_fail_count=`fgrep -l " FAIL:" $exdir/*Ex.stdout | wc -l`
st_fail_count=`fgrep -l " FAIL " $utdir/*STest.stdout | wc -l`

# and finally print it all out
echo "Unit Test, Example, and System Test summary"
echo ""
echo $ut_exe_count "unit test executables found"
echo $ut_stdout_count "unit tests ran"
echo $ut_pass_count "unit tests passed partially or fully"
echo $ut_fail_count "unit tests failed partially or fully"

echo ""
echo $ex_exe_count "example executables found"
echo $ex_stdout_count "examples ran"
echo $ex_pass_count "examples passed partially or fully"
echo $ex_fail_count "examples failed partially or fully"

echo ""
echo $st_exe_count "system test executables found"
echo $st_stdout_count "system tests ran"
echo $st_pass_count "system tests passed partially or fully"
echo $st_fail_count "system tests failed partially or fully"


