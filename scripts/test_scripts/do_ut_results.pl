#!/usr/bin/perl
# $Id: do_ut_results.pl,v 1.9 2005/02/09 16:54:10 svasquez Exp $
# This script runs at the end of the "run_tests" and "run_tests_uni" targets.
# The purpose is to give the user the results of running the unit tests.

# Options:
#
#  -d	TEST_DIR
#  

use Getopt::Std;
use File::Find

# Arrays of unit tests files
@ut_files = ();		# Unit Test files
@ut_x_files = ();	# Unit test executable files
@all_files = (); 	# All files
@stdout_files = (); 	# Unit Test stdout files 
@file_lines = ();	# stdout file lines
@fail_lines = ();	# any file fail lines
@fail_test_list = ();	# the list of unit tests failures
@fail_list = ();	# the list of unit tests that fail
@pass_list = ();	# the list of unit tests that pass
@crashed_list = ();	# the list of unit tests that crashed
@log_files = ();	# log files that should be removed
@st_ut_files = ();	# Stripped unit tests file names
@sorted_fail_testtest__list = ();	# fail_list without duplicated lines.
%options = ();		#arguments

getopts("d:b:", \%options);


	$TEST_DIR = "$options{d}"; 
	$ESMF_BOPT = "$options{b}";

	# Open the test config file 
	$ok=open(F,"$TEST_DIR/tests.config");
	if (!(defined $ok)) {
		print "\n\nERROR:Unable to open $TEST_DIR/tests.config file.\n\n";
		exit 0;
	}
	# Get flags from tests_config file.
	# exhaustive = 0 for ESMF_EXHAUSTIVE=OFF
	# exhaustive = 1 for ESMF_EXHAUSTIVE=ON
	# processor = 0 for uni_processor
	# processor = 1 for multi_processor
	foreach $line (<F>){
                        push(file_lines, $line);
			$count=grep(/Non-exhaustive/, @file_lines);
			if ($count == 1) {
				$exhaustive=0;
			}
			$count=grep(/Exhaustive/, @file_lines);
			if ($count == 1) {
				$exhaustive=1;
			}
			$count=grep(/Uniprocessor/, @file_lines);
			if ($count == 1) {
				$processor=0;
			}
			$count=grep(/Multiprocessor/, @file_lines);
			if ($count == 1) {
				$processor=1;
			}
	}
		
        #Find all files
        find(\&allFiles, '.');
        sub allFiles {
                        # Put all files in a list
                        push all_files, "$File::Find::name\n" if -e ;
        }
        # Get all unit tests files
        @ut_files=grep (/UTest/, @all_files);
	# Delete all testg or testO from list
	@stdout_files=grep (/test$ESMF_BOPT/, @ut_files);
        # Delete stdout files from list
        foreach $file ( @stdout_files) {
                foreach (@ut_files){
                        s/$file//s;     
                }               
        }
	# Delete all UTestLog from list
	@log_files=grep (/UTestLog/, @ut_files);
        # Delete logfile files from list
        foreach $file ( @log_files) {
                foreach (@ut_files){
                        s/$file//s;     
                }               
        }
	# Clear all_files list
	@all_files = ();
	# Get the list of Unit tests stdout files
        find(\&wanted_stdoutfiles, $TEST_DIR);
        sub wanted_stdoutfiles {
                        # Put all files in a list
                        push all_files, "$File::Find::name\n" if -e ;
        }
        @stdout_files=grep (/UTest.stdout/, @all_files);
	# Sort the stdout files list
	@stdout_files=sort(@stdout_files);

        # Get stripped unit tests names
        @st_ut_files = @ut_files;
        foreach ( @st_ut_files) {
                s/\.\///; # Delete all the "./"
                s/\///g; # Delete all the "/"
                s/ESM/ ESM/;# Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                s/\./ /; # Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$1\n/; # Get rid of the 2nd field
        }
        #Sort the list of st_ut_files
        @st_ut_files = sort (@st_ut_files);
	
	# Sort the unit tests file list
	@ut_files = sort(@ut_files);
	# For each unit test file, we need to count the number of tests,
	# Look for the corresponding stdout file.
	# If it does not exist, add the unit test file in the crashed list
	# If the stdout file exists, read the number of processors.
	# Count the PASS/FAILS divide by the number of processors
	# The results of the division must be egual to the number of tests
	# If it doesn't put the unit test in the crashed list.
	# Keep track of pass count and failed tests list.
	
			$total_pass_count = 0;
			$total_fail_count = 0;

	$total_test_count = 0;
        foreach $file ( @ut_files) {
                	open(F,$file);
                	foreach $line (<F>){
                                push(file_lines, $line);
        		}       
          		close ($file);
                
                	if ($exhaustive == 0) {
                        	# Non_exhaustive tests
                        	$count=grep ( /NEX_UTest/, @file_lines);
                        	$test_count = $test_count + $count;
                        	if ( $processor == 0) {
                                	# Uniprocessor subtract multi processor non-exhaustive unit tests
                                	$count=grep ( /NEX_UTest_Multi/, @file_lines);
                                	$test_count = $test_count - $count;
                        	}       
                	}       
                	else {
                        	# Exhaustive tests
                        	$count=grep ( /EX_UTest/, @file_lines);
                        	$test_count = $test_count + $count;
                        	if ( $processor == 0) {
                                	# Uniprocessor subtract multi processor exhaustive unit tests
                                	$count=grep ( /EX_UTest_Multi/, @file_lines);
                                	$test_count = $test_count - $count;
                        	}       
                	}
                	@file_lines = (); # Clear file lines
			$total_test_count = $total_test_count + $test_count;

			# Find the corresponding stdout file if the test count is not zero
			if ($test_count != 0) {
				$test_file = $file;
				foreach ($test_file) {
                			s/\///g; # Delete all the "/"
                			s/ESM/ ESM/;# Break it into 2 fields
                			s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                			s/\./ /; # Break it into 2 fields
                			s/([^ ]*) ([^ ]*)/$1.stdout\n/; # Get rid of the 2nd field
        			}
				# Open the stdout file for this test and read how many processors it used
				@file_lines = ();
        			$ok=open(F,"$TEST_DIR/$test_file");
        			if (!(defined $ok)) {
                			push(crashed_list, $file);
        			}               
				else {
					foreach $line (<F>){
						push(file_lines, $line);
					}
					close ("$TEST_DIR/$file");
					$pet_count=grep ( /NUMBER_OF_PROCESSORS/, @file_lines);
					if ($pet_count == 0) {
						$pet_count = 1;
					}
                        		$pass_count=grep( /PASS/, @file_lines);
					$pass_count = $pass_count/$pet_count;
					if ($pass_count == $test_count){
						push(pass_list, $file);
					}
					else {
                        			$fail_count=grep( /FAIL/, @file_lines);
						$fail_count = $fail_count/$pet_count;
						if ($fail_count !=0 ) {
							push @fail_test_list, grep (/FAIL/, @file_lines);
						}
						if ($test_count != $pass_count + $fail_count) {
					       		push(crashed_list, $file);
                                		}
						else {
							push(fail_list, $file);
						}
					}
					$total_pass_count = $total_pass_count + $pass_count;
					$total_fail_count = $total_fail_count + $fail_count;
					$fail_count =0;
					$pass_count =0;
	
					@file_lines = ();
				
				}
				$test_count =0;
			}

			
        }
	$total_fail_count = $total_test_count - $total_pass_count;
        # Print to the screen
        print "\n\nThere are a total of $total_test_count ";
        if ($exhaustive == 0) {
                print "non-exhaustive ";
        }       
        else {
                print "exhaustive ";
        }
        if ($processor == 0) {
                print "single processor unit tests, ";
        }
        else {
                print "multi processor unit tests, ";
        }
	if ( $total_pass_count == 1) {
		print "$total_pass_count passes and ";
	}
	else {
		print "$total_pass_count pass and ";
	}
	if ( $total_fail_count == 1) {
		print "$total_fail_count fails.\n\n";
	}
	else {
		print "$total_fail_count fail.\n\n";
	}
	# Delete ./ from all lists
        foreach ( @pass_list) {
                s/\.\///; # Delete all the "./"
	}
        foreach ( @crashed_list) {
                s/\.\///; # Delete all the "./"
	}
        foreach ( @fail_list) {
                s/\.\///; # Delete all the "./"
	}
	if (@pass_list != ()){
		print "The unit tests in the following files all pass:\n\n";
		print @pass_list;
	}
	if (@crashed_list != ()){
		print "\n\nThe following unit tests files failed to build, failed to execute or crashed during execution:\n\n";
		print @crashed_list;
	}
	if (@fail_list != ()){
		print "\n\nThe following unit tests files had failed unit tests:\n\n";
		print @fail_list;
	}


	if (@fail_test_list != ()){
		# Delete repeated lines in fail_list
		$sorted_fail_test_list = ();
		foreach $file (@fail_test_list){
			if (grep (/$file/, @sorted_fail_test_list) == 0) {
				push (sorted_fail_test_list, $file);
			}
		}	
		print "\n\nThe following unit tests fail:\n\n";
		print @sorted_fail_test_list;
	}
        if ($total_fail_count == $total_test_count) {
		# Check if there are any unit tests executable files
		# The test  output directory exists.
		@all_files = ();        # Clear all_files array
		find(\&wanted3, $TEST_DIR);
		sub wanted3 {
			# Put all executable files in a list
			push all_files, "$File::Find::name\n"  if -x;
		}
		# Get *UTest files
		@ut_x_files=grep (/UTest/, @all_files);
		# Count the number unit tests in stdout_ex_files
      		$ut_count = 0;
		foreach $file ( @ut_x_files) {
			$ut_count = $ut_count + 1;
		}
		if ($ut_count == 0) {
			print "\n\nNOTE: There are no executable unit tests files, either the 'gmake ESMF_BOPT=$ESMF_BOPT build_tests' has \n";
			print "not been run or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
		}
	}
	print "\n\nThe stdout files for the unit tests can be found at:\n";
	print "$TEST_DIR\n\n";


exit 0;



