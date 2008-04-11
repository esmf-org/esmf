#!/usr/bin/env perl
# $Id: unit_tests_results.pl,v 1.9.2.6 2008/04/11 02:02:58 svasquez Exp $
# This script runs at the end of the "run_unit_tests", "run_unit_tests_uni" and "check_results" targets.
# The purpose is to give the user the results of running the unit tests.
# The results are either complete results or a summary.

sub unit_tests_results($$$) {

        my $TEST_DIR    = $_[0];
        my $ESMF_BOPT   = $_[1];
        my $SUMMARY     = $_[2];


use File::Find

# Arrays of unit tests files
@ut_files = ();		# Unit Test files
@ut_x_files = ();	# Unit test executable files
@all_files = (); 	# All files
@Log_files = (); 	# Unit Test Log files 
@file_lines = ();	# Log file lines
@fail_lines = ();	# any file fail lines
@fail_test_list = ();	# the list of unit tests failures
@fail_list = ();	# the list of unit tests that fail
@pass_list = ();	# the list of unit tests that pass
@crashed_list = ();	# the list of unit tests that crashed
@log_files = ();	# log files that should be removed
@st_ut_files = ();	# Stripped unit tests file names
@sorted_fail_testtest__list = ();	# fail_list without duplicated lines.


	# Open the unit test config file 
	$ok=open(F,"$TEST_DIR/unit_tests.config");
	if (!(defined $ok)) {
		print "\n\n";
        	if ($SUMMARY) { # Print only if full output requested
                	print "UNIT TESTS SUMMARY\n";
        	}
		print "NOTE: Unable to open $TEST_DIR/unit_tests.config file.\n";
		print "Either the 'gmake ESMF_BOPT=$ESMF_BOPT build_unit_tests' has not been run ";
		print "or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";

		return 0;
	}
	# Get flags from unit_tests_config file.
	# exhaustive = 0 for ESMF_TESTEXHAUSTIVE=OFF
	# exhaustive = 1 for ESMF_TESTEXHAUSTIVE=ON
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
                        push all_files, "$File::Find::name\n" if -T ;
        }
        # Get all unit tests files
        @ut_files=grep (/UTest/, @all_files);
	# Delete all testg or testO from list
	@Log_files=grep (/test$ESMF_BOPT/, @ut_files);
        # Delete Log files from list
        foreach $file ( @Log_files) {
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
	# Get the list of Unit tests Log files
        find(\&wanted_Logfiles, $TEST_DIR);
        sub wanted_Logfiles {
                        # Put all files in a list
                        push all_files, "$File::Find::name\n" if -e ;
        }
        @Log_files=grep (/UTest.Log/, @all_files);
	# Sort the Log files list
	@Log_files=sort(@Log_files);

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
	# Look for the corresponding Log file.
	# If it does not exist, add the unit test file in the crashed list
	# If the Log file exists, read the number of processors.
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

			# Find the corresponding Log file if the test count is not zero
			if ($test_count != 0) {
				$test_file = $file;
				foreach ($test_file) {
                			s/\///g; # Delete all the "/"
                			s/ESM/ ESM/;# Break it into 2 fields
                			s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                			s/\./ /; # Break it into 2 fields
                			s/([^ ]*) ([^ ]*)/$1.Log\n/; # Get rid of the 2nd field
        			}
				# Open the Log file for this test and read how many processors it used
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
					$pass_count = int $pass_count/$pet_count;
					# HALT_FAILED must be handled differently
					# if it occurs we must subtract the pass count by one.
                        		$fail_count=grep( /HALT_FAILED/, @file_lines);
					if ($fail_count !=0 ) {
						$pass_count = $pass_count - 1;
					}
					if ($pass_count == $test_count){
						push(pass_list, $file);
					}
					else {
                        			$fail_count=grep( /FAIL/, @file_lines);
						$fail_count = int $fail_count/$pet_count;
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
	# Special code for handling the new Regrid test scheme.
	# If running Exhaustive unit tests
	# Set test count to zero
	$regrid_test_count = 0;
	if ($exhaustive == 1) {
		# Determine if ESMF_RegridToolUTest is in the unit test list
		$regrid_test_found=grep ( /ESMF_RegridToolUTest/, @ut_files);
		if ($regrid_test_found != 0) {
			@regrid_test=grep (/ESMF_RegridToolUTest/, @ut_files);
			# open the ESMF_RegridToolUTest.stdout files to read number of tests
			$ok=open(F,"$TEST_DIR/ESMF_RegridToolUTest.stdout");
                        if (!(defined $ok)) {
				# if the stdout file is not present put it in the crashed list
				push(crashed_list, @regrid_test);
			}      
			else { # Read the number of tests from ESMF_RegridToolUTest.stdout
				$test_string_found = -1;
				while (<F>) { #read the file unitil "TEST_COUNT" is found
					($test_string,$regrid_test_count) = split(/:/, $_);
					if ($test_string eq " TEST_COUNT") {
						$test_string_found = 1;
						goto CONTINUE; 
					}
					
				}
			}
		}
		else {
			# ESMF_RegridToolUTest file not found, therefore ignore
			goto DONE;
		}
		CONTINUE: if ($test_string_found == -1) {
			# The "TEST_COUNT" string was not found inESMF_RegridToolUTest.stdout 
			push(crashed_list, @regrid_test);
		}
		else {	# Found the "TEST_count" count pass/fails
			#open the ESMF_RegridToolUTest.Log file
			$ok=open(F,"$TEST_DIR/ESMF_RegridToolUTest.Log");
                        if (!(defined $ok)) {
				# no Log file was found
				push(crashed_list, @regrid_test);
                        }
			else {
				foreach $line (<F>){
					push(file_lines, $line);
                               	}
				close ("$TEST_DIR/$ESMF_RegridToolUTest.Log");
                                $pet_count=grep ( /NUMBER_OF_PROCESSORS/, @file_lines);
                                if ($pet_count == 0) {
                                	$pet_count = 1;
                               	}
                               	$pass_count=grep( /PASS/, @file_lines);
                               	$pass_count = int $pass_count/$pet_count;
				if ($regrid_test_count == 0) {
					
					goto DONE;
				}
				if ($pass_count == $regrid_test_count){
					push(pass_list, @regrid_test);
				}
				else {
					$fail_count=grep( /FAIL/, @file_lines);
					$fail_count = int $fail_count/$pet_count;
					if ($fail_count !=0 ) {
                       			push @fail_test_list, grep (/FAIL/, @file_lines);
					}
					if ($regrid_test_count!= $pass_count + $fail_count) {
						push(crashed_list, @regrid_test);
					}
					else {
						push(fail_list, @regrid_test);
					}
				}
				$total_pass_count = $total_pass_count + $pass_count;
				$total_fail_count = $total_fail_count + $fail_count;
				$total_test_count = $total_test_count + $regrid_test_count;
			}

		}
		
	}
	DONE: $total_fail_count = $total_test_count - $total_pass_count;
       	if ( $regrid_test_count == 0) {
       		foreach $file (@crashed_list){
                        # if in crashed list delete it
                        if (grep (/ESMF_RegridToolUTest/, $file) == 0) {
                                push (new_crashed_list, $file);
                        }
		}
         }


	# sort all lists
	@pass_list=sort(@pass_list);
	@crashed_list=sort(@new_crashed_list);
	@fail_list=sort(@fail_list);

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
	if (!$SUMMARY) { # Print only if full output requested
        	# Print to the screen
		if (@pass_list != ()){
			print "\n\nThe unit tests in the following files all pass:\n\n";
			print @pass_list;
		}
		if (@crashed_list != ()){
			print "\n\nThe following unit test files failed to build, failed to execute or crashed during execution:\n\n";
			print @crashed_list;
		}
		if (@fail_list != ()){
			print "\n\nThe following unit test files had failed unit tests:\n\n";
			print @fail_list;
		}
	}

	if (@fail_test_list != ()){
		# Delete date type and PET part of the fail message.
		foreach (@fail_test_list) {
			s/^.*?FAIL/   FAIL/;# Delete everything before FAIL
		}
		# Delete repeated lines in fail_test_list
		$sorted_fail_test_list = ();
		foreach $file (@fail_test_list){
			# if not in the list push it in
			if (grep (/$file/, @sorted_fail_test_list) == 0) {
				push (sorted_fail_test_list, $file);
			}
		}	
		if (!$SUMMARY) { # Print only if full output requested
			print "\n\nThe following individual unit tests fail:\n\n";
			print @sorted_fail_test_list;
		}
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
		# Count the number unit tests in Log_ex_files
      		$ut_count = 0;
		foreach $file ( @ut_x_files) {
			$ut_count = $ut_count + 1;
		}
		if ($ut_count == 0) {
			print "\n\n";
			if ($SUMMARY) { # Print only if full output requested
				print "UNIT TESTS SUMMARY\n";
			}
			print "NOTE: There are no executable unit tests files, either the 'gmake ESMF_BOPT=$ESMF_BOPT build_unit_tests' has \n";
			print "not been run or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
			return 0;
		}
	}
	if (!$SUMMARY) { # Print only if full output requested
		print "\n\nThe log and stdout files for the unit tests can be found at:\n";
		print "$TEST_DIR\n\n\n";
	}
	else { # Print only if full output requested
		print "\n\nUNIT TESTS SUMMARY\n";
	}
	if ($total_test_count == 0) {
		print "NOTE: Found no ";
	}
	else {
        	print "Found $total_test_count ";
	}
        if ($exhaustive == 0) {
                print "non-exhaustive ";
        }       
        else {
                print "exhaustive ";
        }
        if ($processor == 0) {
                print "single processor unit tests";
        }
        else {
                print "multi-processor unit tests";
        }
	if ($total_test_count == 0) {
		print ".\n\n";
	}
	else {
		print ", $total_pass_count passed and $total_fail_count failed.\n\n";
	}


	# Write test results to be read by regression tests scripts.
	$results_file="$TEST_DIR/unit_tests_results";
	open(MYHANDLE, ">$results_file");
	print MYHANDLE "PASS $total_pass_count FAIL $total_fail_count \n";



}
1; # This is for the "require" function to work properly.


