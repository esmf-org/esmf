#!/usr/bin/perl
# $Id: do_st_results.pl,v 1.3 2004/08/05 17:14:32 svasquez Exp $
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running the system tests.

# Options:
#
#  -d	TEST_DIR
#  

use Getopt::Std;
use File::Find

# Arrays of system tests files
@s_t_ex = ();		# System Test executable files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@stdout_files = (); 	# System Test stdout files 
@file_lines = ();	# stdout file lines
@fail_tests = ();	# system tests that failed
@pass_tests = ();	# system tests that passed
%options = ();		#arguments

getopts("d:", \%options); 


	$TEST_DIR = "$options{d}"; 


	print "\n";
	print "The following is the analysis of the run system tests results:\n";
	print "\n";

	#go to the test directory
	$ok = chdir "$TEST_DIR/";

	if ( not $ok) {
		# The tests directory does not exist
                print "There are no executable or stdout system test files, either the 'gmake build_system_tests' has \n";
                print "not been run or the 'build_system_tests' did not build successfully. \n\n";
	}
	else {
		# The test directory exists
		find(\&wanted, '.'); 
		sub wanted {
			# Put all executable files in a list
		 	push ex_files, "$File::Find::name\n" if -x ;
		}
		# Get count of executable System Tests found
		$s_t_count=grep (/STest/, @ex_files);
		# Get executable system tests files
		@s_t_ex=grep (/STest/, @ex_files);


		if ($s_t_count eq 0) {
			print "There are no executable system test files, either the 'gmake build_system_tests' has \n";
			print "not been run or the 'build_system_tests' did not build successfully. \n\n";
		}
		else {
			#Sort the list of executable system tests files.
			@s_t_ex = sort (@s_t_ex);
			print "Executable system tests found:\n";
			# Delete "./" from file name
			foreach ( @s_t_ex) {
				s/\.\///;
			}
			# do an "ls -l" on each file
			foreach $file ( @s_t_ex) {
				open($file, "| ls  -l $file");
				close($file);
			}
			print "\n\n";
			
		}

		print "All of the executable system tests should have a corresponding stdout file.\n";
		print "If not, it's an indication that the system test was not executed, or that it failed to execute.\n\n";
	
		find(\&wanted2, '.'); 
		sub wanted2 {
			# Put all files in a list
		 	push all_files, "$File::Find::name\n"  if -e;
		}
		# Get count of *STest.stdout files found
		$s_t_so_count=grep (/STest.stdout/, @all_files);
		# Get *STest.stdout files
		@stdout_files=grep (/STest.stdout/, @all_files);
	
        	if ($s_t_so_count eq 0) {
                	print "There are no system test stdout files. \n\n";
        	}
        	else {
                	#Sort the list of stdout files.
                	@stdout_files = sort (@stdout_files);
                	print "System tests stdout files found:\n";
                	# Delete "./" from file name
                	foreach ( @stdout_files) {
                        	s/\.\///;
                	}
                	# do an "ls -l" on each file
			# and count the number of PASS and FAIL
			# push pass and fail tests to a list.
			$count=0;
			$pass_count=0;
                	foreach $file ( @stdout_files) {
                        	open($file, "| ls  -l $file");
				open(F,$file);
				foreach $line (<F>){
					push(file_lines, $line);
				}
				close ($file);
				$count=grep ( /PASS/, @file_lines);
				if ($count != 0) {
					push (pass_tests, $file);
				}
				else {
					push (fail_tests, $file);
					$fail_count=$fail_count + 1;
				}
				$pass_count=$pass_count + $count;
				@file_lines=();
                	}
                	print "\n\n";

			print "System test stdout files of zero length indicate that the system test\n";
			print "did not run because it failed to compile or it failed to execute. \n\n";
	
			if ($pass_count != 0) {
				print "The following system tests pass:\n";
				print @pass_tests;
			}
			else {
				print "No system tests passed.\n";
			}
	
                	print "\n\n";

			if ($fail_count != 0) {
				print "The following system tests fail:\n";
				print @fail_tests;
			}
			else {
				print "No system tests failed.\n";
			}
        	}
                print "\n\n";
	}
exit 0;

