#!/bin/bash
# $Id: do_ut_results.bash,v 1.2 2004/07/06 19:39:03 svasquez Exp $
# This script runs at the end of the "run_tests" and "run_tests_uni" targets.
# The purpose is to give the user the results of running the unit tests.

echo ""
echo "The following is the analysis of the Unit Tests results:"
echo ""

export dir=$ESMF_DIR/test/test$ESMF_BOPT/$ESMF_ARCH.$ESMF_COMPILER.$ESMF_PREC.$ESMF_SITE

find $dir -name "*UTest" -print | wc -l > stdoutfiles
if grep " 0" stdoutfiles > NULL # check if there are any unit tests executable files 
then
	echo "There are no executable Unit test files, either the 'gmake build_tests' has "
	echo "not been run or the 'build_tests' did not build successfully."
	echo "" 
else
	echo "Executable unit tests found:"
	(cd $dir; ls -l *UTest)
	echo "" 
	echo "All of the executable unit tests should have a corresponding stdout file." 
	echo "If not, it's an indication that the unit test was not executed, or that it failed to execute." 
	echo "" 

fi # end of if grep " 0" stdoutfiles

find $dir -name "*UTest*.stdout" -print | wc -l > stdoutfiles
if grep  " 0" stdoutfiles > NULL
then
	echo "There are no Unit Test stdout files." 
	echo "" 
else
	echo "Unit Tests stdout files found: " 
	(cd $dir; ls -l *UTest*.stdout)
	echo "" 
	echo "Unit test stdout files of zero length indicate that the unit test" 
	echo "did not run because it failed to compile or it failed to execute. " 
	echo "" 
	tests=`grep  PASS $dir/*UTest*.stdout | wc -l`
	echo  $tests " Unit Tests passed " 
        echo ""
	if grep  FAIL $dir/*UTest*.stdout > NULL
	then
		echo "The following Unit Tests Failed: "  
		(cd $dir; grep FAIL *UTest*.stdout)
		echo "" 
	else
		find $dir -name "*UTest*.stdout"  -size 0 -print | wc -l > zerofiles
		if diff stdoutfiles zerofiles  > NULL #check if all the stdoutfiles are of zero length
		then
			echo ""
		else
			echo "No Unit Tests Failed." 
			echo "" 

		fi #end of if diff stdoutfiles zerofiles

	fi #end of if grep FAIL $dir/*UTest*.stdout

fi #end of if grep " 0" stdoutfiles
rm -f stdoutfiles
rm -f zerofiles
rm -f NULL

