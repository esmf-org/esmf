#!/bin/bash
# $Id: do_ex_results.bash,v 1.3 2004/07/30 22:56:07 svasquez Exp $
# This script runs at the end of the "run_example" target.
# The purpose is to give the user the results of running the examples.

echo ""
echo "The following is the analysis of the run examples results:"
echo ""

export dir=$ESMF_DIR/examples/examples$ESMF_BOPT/$ESMF_ARCH.$ESMF_COMPILER.$ESMF_PREC.$ESMF_SITE 

find $dir -name "*Ex" -print | wc -l > stdoutfiles
if grep " 0" stdoutfiles > NULL # check if there are any examples executable files 
then
	echo "There are no executable examples files, either the 'gmake build_examples' has "
	echo "not been run or the 'build_examples' did not build successfully."
	echo "" 
else
	echo "Executable examples found:"
	(cd $dir; ls -l *Ex)
	echo "" 
	echo "All of the executable examples should have a corresponding stdout file." 
	echo "If not, it's an indication that the examples ere not executed, or that it failed to execute." 
	echo "" 

fi # end of if grep " 0" stdoutfiles

find $dir -name "*Ex.stdout" -print | wc -l > stdoutfiles
if grep  " 0" stdoutfiles > NULL
then
	echo "There are no examples stdout files." 
	echo "" 
else
	echo "Examples stdout files found: " 
	(cd $dir; ls -l *Ex.stdout)
	echo "" 
	echo "Example stdout files of zero length indicate that the example" 
	echo "did not run because it failed to compile or it failed to execute. " 
	echo "" 
	if grep "PASS:" $dir/*Ex.stdout > NULL
	then
		echo "The following examples passed: "  
		(cd $dir; grep -l "PASS:" *Ex.stdout)
		echo "" 
 	else
		echo "No examples passed." 
		echo "" 
	fi #end of if grep PASS ...

        echo ""
	if grep  "FAIL:" $dir/*Ex.stdout > NULL
	then
		echo "The following examples failed: "  
		(cd $dir; grep -l "FAIL:" *Ex.stdout)
		echo "" 
	else
		find $dir -name "*Ex.stdout"  -size 0 -print | wc -l > zerofiles
		if diff stdoutfiles zerofiles  > NULL #check if all the stdoutfiles are of zero length
		then
			echo ""
		else
			echo "No examples Failed." 
			echo "" 

		fi #end of if diff stdoutfiles zerofiles

	fi #end of if grep FAIL $dir/*Ex.stdout

fi #end of if grep " 0" stdoutfiles
rm -f stdoutfiles
rm -f zerofiles
rm -f NULL

exit 0
