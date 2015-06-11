#!/usr/bin/env perl
# $Id$
# This script runs when the user wants to compare the elapsed time of the current
# elapsed time of the current uit tests to a previously benchmarked value.
# The variance is provided by the user and is used to determine acceptability.
# The script calls unit_tests_results with the summary option turned off,
# therefore the complete results are printed to the screen.

# Options:
#  -h   ESMF_TESTSCRIPTS
#  -d   TEST_DIR
#  -e   BM_DIR
#  -f   VARIANCE
#  -g   THRESHOLD
#  -i   ESMF_BOPT

use Getopt::Std;

getopts("h:d:e:f:g:i:", \%options);

	$ESMF_TESTSCRIPTS = "$options{h}";
	$TEST_DIR = "$options{d}"; 
	$BM_DIR = "$options{e}";
	$TOLERANCE = "$options{f}";
	$THRESHOLD = "$options{g}";
	$ESMF_BOPT = "$options{i}";

#
# Define location of test scripts.
#
unshift (@INC, "$ESMF_TESTSCRIPTS");
#
# Declare subroutine
#
require "unit_tests_bm_results.pl";

# Call unit_tests_bm_result with SUMMARY request turned off.
&unit_tests_bm_results("$TEST_DIR","$BM_DIR","$TOLERANCE","$THRESHOLD","$ESMF_BOPT","0");

exit;
