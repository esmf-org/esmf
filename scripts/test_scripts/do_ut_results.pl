#!/usr/bin/env perl
# $Id: do_ut_results.pl,v 1.17 2006/03/07 20:48:00 svasquez Exp $
# This script runs at the end of the "run_tests" and "run_tests_uni" targets.
# The purpose is to give the user the results of running the unit tests.
# The script calls unit_tests_results with the summary option turned off,
# therefore the complete results are printed to the screen.

# Options:
#  -h   ESMF_TESTSCRIPTS
#  -d   TEST_DIR
#  -b   ESMF_BOPT

use Getopt::Std;

getopts("h:d:b:", \%options);

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
require "unit_tests_results.pl";

# Call unit_tests_result with SUMMARY request turned off.
&unit_tests_results("$TEST_DIR","$ESMF_BOPT","0");

exit;
