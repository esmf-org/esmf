#!/usr/bin/perl
# $Id: do_ex_results.pl,v 1.6 2005/01/31 23:54:23 svasquez Exp $
# This script runs at the end of the examples and "check_results" targets.
# The purpose is to give the user the results of running the examples.

# Options:
#
#  -d	EX_DIR
#  -s   source directory
#  

use Getopt::Std;
use File::Find

# Arrays of examples files
@act_ex_files = (); 	# Actual example files
@st_ex_files = ();	# Stripped example file names
@all_files = (); 	# All files
@stdout_files = (); 	# Examples stdout files 
@stdout_ex_files = ();	# stdout that are real examples
@fail_ex_files = ();	# failed examples files
@pass_ex_files = ();	# passed examples files
@file_lines = ();	# stdout file lines
%options = ();		#arguments


getopts("d:", \%options);


	$EX_DIR = "$options{d}"; 

	# Find all example files
	#Find all files
	find(\&allFiles, '.'); 
	sub allFiles {
			# Put all files in a list
	 		push all_files, "$File::Find::name\n" if -e ; 
	}	
	@ex_files=grep (/Ex/, @all_files);
	# Find the example files with the "!EXAMPLE" string
	# grep for "!EXAMPLE"
	# push pass and fail tests to a list.
	$count=0;
	$ex_count=0;
	foreach $file ( @ex_files) {
		open(F,$file);
		foreach $line (<F>){
			push(file_lines, $line);
			}
			close ($file);
			$count=grep ( /!EXAMPLE/, @file_lines);
			if ($count != 0) {
				push (act_ex_files, $file);
                                        $ex_count=$ex_count + 1;
				}
                        @file_lines=();
	}
	if ( $ex_count == 0 ) {
		print "Found no examples.\n";
		exit 0;
	}
	# Delete "./" from file name
	foreach (@act_ex_files) {
		s/\.\///;
	}
	# Sort the act_ex_files
	@act_ex_files = sort (@act_ex_files);

        # Get stripped example file names
	@st_ex_files = @act_ex_files;
	foreach ( @st_ex_files) {
		s/\///g; # Delete all the "/"
		s/ESMF/ ESMF/;# Break it into 2 fields
		s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
		s/\./ /; # Break it into 2 fields
		s/([^ ]*) ([^ ]*)/$1.stdout\n/; # Get rid of the 2nd field
	}
	#Sort the list of st_ex_files 
	@st_ex_files = sort (@st_ex_files);
	#go to the examples directory
	$ok = chdir "$EX_DIR/";

	if (not $ok) {
		#The examples directory does not exist.
		print "There are no executable or stdout examples files, either the 'gmake build_examples' has \n";
		print "not been run or the 'build_examples' did not build successfully. \n\n";
	}
	else {
		@all_files = (); 	# Clear all_files array
		# The examples output directory exists.
		find(\&wanted2, '.'); 
		sub wanted2 {
				# Put all files in a list
			 	push all_files, "$File::Find::name\n"  if -e;
		}
		# Get *Ex.stdout files
		@stdout_files=grep (/Ex.stdout/, @all_files);
		#Sort the list of stdout files.
		@stdout_files = sort (@stdout_files);
		# Find the stdout fles that are in the st_ex_files
               	foreach $file ( @st_ex_files) {
				push @stdout_ex_files, grep (/$file/, @stdout_files);
		}
		@stdout_ex_files = sort (@stdout_ex_files);

		# Count the number of PASS and FAIL
		# push pass and fail tests to a list.
		$count=0;
		$pass_count=0;
		$fail_count=0;
		foreach $file ( @stdout_ex_files) {
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
			@file_lines=();
		}
		$fail_count = $ex_count - $pass_count;
		print "\n\n";
		print "There are $ex_count examples, $pass_count pass and $fail_count fail.\n";
		print "\n\n";
                if ($pass_count != 0) {
                        #Strip the names of failed examples
                        foreach (@pass_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_ex_files fles that are in the fail_tests
                        foreach $file ( @pass_tests) {
                                push @pass_ex_files, grep (/$file/, @act_ex_files);
                        }
                	if ($pass_count == 1) {
                        	print "The following example passes:\n";
			}
			else {
                        	print "The following examples pass:\n";
			}
                        print "\n\n";
                        # Sort the pass_ex_files
                        @pass_ex_files = sort (@pass_ex_files);
                        foreach $file (@pass_ex_files) {
                        print "         $file";
                        }
                        print "\n\n";
                }

		if ($fail_count != 0) {
                	# Find the act_ex_files fles that are in the pass_tests
                	foreach $file ( @pass_tests) {
                               	pop @act_ex_files, grep (/$file/, @act_ex_files);
                	}
			if ($fail_count == 1) {
				print "The following example failed, did not build, or did not execute:\n";
			}
			else {
				print "The following examples failed, did not build, or did not execute:\n";
			}
               		print "\n\n";
			# Sort the act_ex_files
			@act_ex_files = sort (@act_ex_files);
			foreach $file (@act_ex_files) {
			print "         $file";
			}
               		print "\n\n";
		}
			
	}
exit 0;

