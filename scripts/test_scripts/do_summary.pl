#!/usr/bin/env perl
# $Id: do_summary.pl,v 1.6.4.2 2008/04/18 21:28:21 svasquez Exp $
# This prints a summary of system tests, unit tests and examples.
# The script calls sys_tests_results, examples_results and 
# unit_tests_results with the summary option turned on,
# therefore the summary results are printed to the screen.


# Options:
#
#  -h   ESMF_TESTSCRIPTS
#  -d	TEST_DIR
#  -e	EX_DIR
#  -b   ESMF_BOPT
#  

use Getopt::Std;
use Cwd;

getopts("h:d:e:b:m:", \%options); 


	$ESMF_TESTSCRIPTS = "$options{h}"; 
	$TEST_DIR = "$options{d}"; 
	$EX_DIR = "$options{e}"; 
	$ESMF_BOPT = "$options{b}";


#
# Define location of test scripts.
#
unshift (@INC, "$ESMF_TESTSCRIPTS");
#
# Declare subroutines
#
require "sys_tests_results.pl";
require "examples_results.pl";
require "unit_tests_results.pl";

# get pwd
$dir = cwd();

# Call sys_tests_result with SUMMARY request turned on.
&sys_tests_results("$TEST_DIR","$ESMF_BOPT","1");

# Go back tp pwd
chdir $dir;

# Call examples_results with SUMMARY request turned on.
&examples_results("$EX_DIR","$ESMF_BOPT","1");

# Go back tp pwd
chdir $dir;

# Call unit_tests_results with SUMMARY request turned on.
&unit_tests_results("$TEST_DIR","$ESMF_BOPT","1");

# Go back tp pwd
chdir $dir;

exit;
