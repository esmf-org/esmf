#!/usr/bin/perl
# $Id: do_ex_results.pl,v 1.4 2004/10/27 17:25:00 svasquez Exp $
# This script runs at the end of the examples and "check_results" targets.
# The purpose is to give the user the results of running the examples.

# Options:
#
#  -d	EX_DIR
#  

use Getopt::Std;
use File::Find

# Arrays of examples files
@ex_ex = ();		# Examples executable files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@stdout_files = (); 	# Examples stdout files 
@file_lines = ();	# stdout file lines
@fail_examples = ();	# Examples that failed
@pass_examples = ();	# Examples that passed
%options = ();		#arguments

getopts("d:", \%options); 


	$EX_DIR = "$options{d}"; 


	print "\n";
	print "The following is the analysis of the run examples results:\n";
	print "\n";

	#go to the examples directory
	$ok = chdir "$EX_DIR/";

	if (not $ok) {
		#The examples directory does not exist.
		print "There are no executable or stdout examples files, either the 'gmake build_examples' has \n";
		print "not been run or the 'build_examples' did not build successfully. \n\n";
	}
	else {
		# The examples directory exists.
		find(\&wanted, '.'); 
		sub wanted {
				# Put all executable files in a list
			 	push ex_files, "$File::Find::name\n" if -x ;
		}
		# Get count of executable Examples found
		$ex_count=grep (/Ex$/, @ex_files);
		# Get executable system tests files
		@ex_ex=grep (/Ex$/, @ex_files);


		if ($ex_count eq 0) {
			print "There are no executable examples files, either the 'gmake build_examples' has \n";
			print "not been run or the 'build_examples' did not build successfully. \n\n";
		}
		else {
			#Sort the list of executable examples files.
			@ex_ex = sort (@ex_ex);
			print "Executable examples found:\n";
			# Delete "./" from file name
			foreach ( @ex_ex) {
				s/\.\///;
			}
			# do an "ls -l" on each file
			foreach $file ( @ex_ex) {
				open($file, "| ls  -l $file");
				close($file);
			}
			print "\n\n";
			
		}
	
		print "All of the executable examples should have a corresponding stdout file.\n";
		print "If not, it's an indication that the example was not executed, or that it failed to execute.\n\n";
	
		find(\&wanted2, '.'); 
		sub wanted2 {
				# Put all files in a list
			 	push all_files, "$File::Find::name\n"  if -e;
		}
		# Get count of *Ex.stdout files found
		$ex_so_count=grep (/Ex.stdout/, @all_files);
		# Get *Ex.stdout files
		@stdout_files=grep (/Ex.stdout/, @all_files);
	
        	if ($ex_so_count eq 0) {
                	print "There are no examples stdout files. \n\n";
        	}
        	else {
                	#Sort the list of stdout files.
                	@stdout_files = sort (@stdout_files);
                	print "Example stdout files found:\n";
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
					$pass_count=$pass_count + 1;
				}
				else {
					push (fail_tests, $file);
					$fail_count=$fail_count + 1;
				}
				@file_lines=();
                	}
                	print "\n\n";
	
			print "Example stdout files of zero length indicate that the system test\n";
			print "did not run because it failed to compile or it failed to execute. \n\n";
	
			if ($pass_count != 0) {
				print "The following examples pass:\n";
				print @pass_tests;
			}
			else {
				print "No examples passed.\n";
			}
	
                	print "\n\n";

			if ($fail_count != 0) {
				print "The following examples fail:\n";
				print @fail_tests;
			}
			else {
				print "No examples tests failed.\n";
			}
        	}
               	print "\n\n";
	}
			
exit 0;

