#!/usr/bin/env perl
# $Id$
# This prints a summary of system tests, and unit tests.
# The script calls sys_tests_results, and unit_tests_results 
# with the summary option turned on,
# therefore the summary results are printed to the screen.


# Options:
#
#  -h   ESMF_TESTSCRIPTS
#  -d	TEST_DIR
#  -e	EX_DIR
#  -b   ESMF_BOPT
#  -f   ESMF_COMM
#  

use Getopt::Std;
use Cwd;

getopts("h:d:e:b:m:f", \%options); 


	$ESMF_TESTSCRIPTS = "$options{h}"; 
	$TEST_DIR = "$options{d}"; 
	$EX_DIR = "$options{e}"; 
	$ESMF_BOPT = "$options{b}";
	$ESMF_COMM = "$options{f}";


#
# Define location of test scripts.
#
unshift (@INC, "$ESMF_TESTSCRIPTS");
#
# Declare subroutines
#
require "sys_tests_results.pl";
require "unit_tests_results.pl";

# get pwd
$dir = cwd();

# Call sys_tests_result with SUMMARY request turned on.
&sys_tests_results("$TEST_DIR","$ESMF_BOPT","$ESMF_COMM","1");

# Go back tp pwd
chdir $dir;

# Call unit_tests_results with SUMMARY request turned on.
&unit_tests_results("$TEST_DIR","$ESMF_BOPT","$ESMF_COMM","1");

# Go back tp pwd
chdir $dir;

exit;
