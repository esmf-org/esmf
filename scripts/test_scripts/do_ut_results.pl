#!/usr/bin/perl
# $Id: do_ut_results.pl,v 1.4 2004/08/02 21:04:49 svasquez Exp $
# This script runs at the end of the "run_tests" and "run_tests_uni" targets.
# The purpose is to give the user the results of running the unit tests.

# Options:
#
#  -d	TEST_DIR
#  

use Getopt::Std;
use File::Find

# Arrays of unit tests files
@u_t_ex = ();		# Unit Test executable files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@stdout_files = (); 	# Unit Test stdout files 
@file_lines = ();	# stdout file lines
@fail_lines = ();	# stdout file fail lines
%options = ();		#arguments

getopts("d:", \%options); 


	$TEST_DIR = "$options{d}"; 


	print "\n";
	print "The following is the analysis of the unit tests results:\n";
	print "\n";

	#go to the test directory
	chdir "$TEST_DIR/";

	find(\&wanted, '.'); 
	sub wanted {
			# Put all executable files in a list
			 push ex_files, "$File::Find::name\n" if -x ;
	}
	# Get count of executable Unit Tests found
	$u_t_count=grep (/\UTest/i, @ex_files);
	# Get executable Unit tests files
	@u_t_ex=grep (/\UTest/i, @ex_files);

	if ($u_t_count eq 0) {
		print "There are no executable unit test files, either the 'gmake build_tests' has \n";
		print "not been run or the 'build_tests' did not build successfully. \n\n";
	}
	else {
		#Sort the list of executable unit tests files.
		@u_t_ex = sort (@u_t_ex);
		print "Executable unit tests found:\n";
		# Delete "./" from file name
		foreach ( @u_t_ex) {
			s/\.\///;
		}
		# do an "ls -l" on each file
		foreach $file ( @u_t_ex) {
			open($file, "| ls  -l $file");
			close($file);
		}
		print "\n\n";
		
	}

	print "All of the executable unit tests should have a corresponding stdout file.\n";
	print "If not, it's an indication that the unit test was not executed, or that it failed to execute.\n\n";

	find(\&wanted2, '.'); 
	sub wanted2 {
			# Put all files in a list
			 push all_files, "$File::Find::name\n"  if -e;
	}
	# Get count of *UTest.stdout files found
	$u_t_so_count=grep (/\UTest.stdout/i, @all_files);
	# Get *UTest.stdout files
	@stdout_files=grep (/\UTest.stdout/i, @all_files);

        if ($u_t_so_count eq 0) {
                print "There are no unit test stdout files. \n\n";
        }
        else {
                #Sort the list of stdout files.
                @stdout_files = sort (@stdout_files);
                print "Unit tests stdout files found:\n";
                # Delete "./" from file name
                foreach ( @stdout_files) {
                        s/\.\///;
                }
                # do an "ls -l" on each file
		# and count the number of PASS and FAIL
		$count=0;
		$pass_count=0;
                foreach $file ( @stdout_files) {
                        open($file, "| ls  -l $file");
			open(F,$file);
			foreach $line (<F>){
				push(file_lines, $line);
			}
			close ($file);
			$count=grep ( /PASS/i, @file_lines);
			$pass_count=$pass_count + $count;
			$count=grep ( /FAIL/i, @file_lines);
			$fail_count=$fail_count + $count;
			push (fail_lines, grep( /FAIL/i, @file_lines));
			@file_lines=();
                }
                print "\n\n";

		print "Unit test stdout files of zero length indicate that the unit test\n";
		print "did not run because it failed to compile or it failed to execute. \n\n";

		print " $pass_count  Unit Tests passed \n\n"; 

		if ($fail_count != 0) {
			print "The following unit tests fail:\n";
			print @fail_lines;
		}
		else {
			print "No unit tests failed\n";
		}
        }

exit 0;

