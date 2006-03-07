#!/usr/bin/env perl
# $Id: do_ex_results.pl,v 1.19 2006/03/07 20:47:59 svasquez Exp $
# This script runs at the end of the examples and "check_results" targets.
# The purpose is to give the user the results of running the examples.
# The script calls examples_results with the summary option turned off,
# therefore the complete results are printed to the screen.

# Options:
#
#  -h	ESMF_TESTSCRIPTS
#  -d	EX_DIR
#  -b	ESMF_BOPT


use Getopt::Std;

getopts("h:d:b:", \%options);


	$ESMF_TESTSCRIPTS = "$options{h}"; 
	$EX_DIR = "$options{d}"; 
	$ESMF_BOPT = "$options{b}";

#
# Define location of test scripts.
#
unshift (@INC, "$ESMF_TESTSCRIPTS");
#
# Declare subroutine
#
require "examples_results.pl";

# Call examples_result with SUMMARY request turned off.
&examples_results("$EX_DIR","$ESMF_BOPT","0");

exit;
