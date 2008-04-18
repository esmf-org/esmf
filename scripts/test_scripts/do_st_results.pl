#!/usr/bin/env perl
# $Id: do_st_results.pl,v 1.10.4.2 2008/04/18 21:28:21 svasquez Exp $
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running the system tests.
# The script calls sys_tests_results with the summary option turned off,
# therefore the complete results are printed to the screen.

# Options:
#
#  -h   ESMF_TESTSCRIPTS
#  -d	TEST_DIR
#  -b   ESMF_BOPT 
#

use Getopt::Std;

getopts("h:d:b:m:", \%options); 


	$ESMF_TESTSCRIPTS = "$options{h}"; 
	$TEST_DIR = "$options{d}"; 
	$ESMF_BOPT = "$options{b}";

#
# Define location of test scripts.
#
unshift (@INC, "$ESMF_TESTSCRIPTS");
#
# Declare subroutine
#
require "sys_tests_results.pl";

# Call sys_tests_result with SUMMARY request turned off.
&sys_tests_results("$TEST_DIR","$ESMF_BOPT","0");

exit;

