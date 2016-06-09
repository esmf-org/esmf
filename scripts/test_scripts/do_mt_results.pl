#!/usr/bin/env perl
# $Id$
# This script runs at the end of the "run_mapl_tests" and "run_mapl_tests_uni" targets.
# The purpose is to give the user the results of running the mapl tests.
# The script calls mapl_tests_results with the summary option turned off,
# therefore the complete results are printed to the screen.

# Options:
#  -h   ESMF_TESTSCRIPTS
#  -e   ESMF_DIR
#  -d   TEST_DIR
#  -b   ESMF_BOPT

use Getopt::Std;

getopts("h:e:d:b:", \%options);

	$ESMF_TESTSCRIPTS = "$options{h}";
	$ESMF_DIR = "$options{e}";
	$TEST_DIR = "$options{d}"; 
	$ESMF_BOPT = "$options{b}";

#
# Define location of test scripts.
#
unshift (@INC, "$ESMF_TESTSCRIPTS");
#
# Declare subroutine
#
require "mapl_tests_results.pl";

# Call mapl_tests_result with SUMMARY request turned off.
&mapl_tests_results("$ESMF_DIR","$TEST_DIR","$ESMF_BOPT","0");

exit;
