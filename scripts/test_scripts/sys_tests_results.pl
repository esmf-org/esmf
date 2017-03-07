#!/usr/bin/env perl
# $Id$
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running the system tests.
# The results are either complete results or a summary.

sub sys_tests_results($$$$) {

        my $TEST_DIR	= $_[0];
        my $ESMF_BOPT   = $_[1];
        my $ESMF_COMM   = $_[2];
        my $SUMMARY     = $_[3];


# This subroutine reads the number of pets from the *ST.Log files.
sub get_pet_count {

        my @logFile    = @_;

        # Find # of processors string
        $count=grep ( /NUMBER_OF_PROCESSORS/, @logFile);
        if (($count == "") || ($count == 0)){
                # Did not find the # of processors string
                return(0);
        }
         # Create list of processor count strings
        @num_procs = grep(/NUMBER_OF_PROCESSORS/, @file_lines);
        $pet_count_found = 0;
        foreach (@num_procs){
                # remove all white spaces
        	s/ //g;
        	$pet_count = 0;
        	($test_string,$pet_count) = split(/NUMBER_OF_PROCESSORS/, $_);
		if ($pet_count != 0) {
			# Read the number of pets from log file.
        		return($pet_count);
		}
        }
	#Could not read the number of pets from log file.
	return(0);
}


use File::Find

# Arrays of system tests files
@s_t_ex = ();		# System Test executable files
@st_files = ();		# System Test files
@act_st_files = ();	# Actual system Test files
@st_st_files = ();	# Stripped system Test files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@Log_files = (); 	# System Test Log files 
@tmp_Log_files = (); 	# tmp system Test Logt files 
@file_lines = ();	# stdout file lines
@fail_tests = ();	# system tests that failed
@pass_tests = ();	# system tests that passed


                        
        # Open the system test config file 
        $ok=open(F,"$TEST_DIR/sys_tests.config");
        if (!(defined $ok)) {
                print "\n\n";
                if ($SUMMARY) { # Print only if full output requested
                        print "SYSTEM TESTS SUMMARY\n";
                }
                print "NOTE: Unable to open $TEST_DIR/sys_tests.config file.\n";
                print "Either the 'gmake ESMF_BOPT=$ESMF_BOPT build_system_tests' has not been run ";
                print "or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
                
                return 0;
        } 
        # Get flags from sys_tests_config file.
        # testmpmd = 0 for ESMF_TESTMPMD=OFF
        # testmpmd = 1 for ESMF_TESTMPMD=ON
        # testsharedobj = 0 for ESMF_TESTSHAREDOBJ=OFF
        # testsharedobj = 1 for ESMF_TESTSHAREDOBJ=ON
        # processor = 0 for uni_processor
        # processor = 1 for multi_processor
        foreach $line (<F>){
                        push(@file_lines, $line);
                        $count=grep(/Nontestmpmd/, @file_lines);
                        if ($count == 1) {
                                $testmpmd=0;
                        }
                        $count=grep(/Testmpmd/, @file_lines);
                        if ($count == 1) {
                                $testmpmd=1;
                        }
                        $count=grep(/Nontestsharedobj/, @file_lines);
                        if ($count == 1) {
                                $testsharedobj=0;
                        }
                        $count=grep(/Testsharedobj/, @file_lines);
                        if ($count == 1) {
                                $testsharedobj=1;
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
        find(\&wanted, '.');
        sub wanted {
                        ## Put all files in a list
			push @all_files, "$File::Find::name\n" ;
        }
        # Get all system tests files
        @st_files=grep (/STest.F90/, @all_files);
	# Check for special case of MPMD test
	$count=grep (/STestA.F90/, @all_files);
	if ($count != 0) {
		@mpmd_file=grep (/STestA.F90/, @all_files);
        	push (@st_files, @mpmd_file);
	}
        # Find the system test files with the "ESMF_SYSTEM_TEST" string
        # grep for system tests to report on
        $count=0;
        $st_count=0;
        foreach $file ( @st_files) {
                open(F,$file);
                foreach $line (<F>){
                        push(@file_lines, $line);
                        }
                        close ($file);
			if ( $processor == 0) {
				# Get the uni-PET system tests
                        	$count=grep ( /ESMF_SYSTEM_TEST/, @file_lines);
                        	if ($count != 0) {
                                	push (@act_st_files, $file);
                                       	$st_count=$st_count + 1;
                                }
			}
			else {
				# Get the mult-PET only system_tests
                        	$count=grep ( /ESMF_MULTI_PROC_SYSTEM_TEST/, @file_lines);
                        	if ($count != 0) {
                                	push (@act_st_files, $file);
                                        $st_count=$st_count + 1;
                                }
				# Include the uni-PET system tests
                        	$count=grep ( /ESMF_SYSTEM_TEST/, @file_lines);
                        	if ($count != 0) {
                                	push (@act_st_files, $file);
                                       	$st_count=$st_count + 1;
                                }
			}
                        if ( ($testmpmd == 1) &  ( $processor == 1)) {
			  # Include MPMD system tests only if running multi processor
                          $count=grep ( /ESMF_MPMD_SYSTEM_TEST/, @file_lines);
                          if ($count != 0) {
                                push (@act_st_files, $file);
                                        $st_count=$st_count + 1;
                                }
                        }
                        if ( ($testsharedobj == 1) &  ( $processor == 1)) {
			  # Include Sharedobj system tests only if running multi processor
                          $count=grep ( /ESMF_SHAREDOBJ_SYSTEM_TEST/, @file_lines);
                          if ($count != 0) {
                                push (@act_st_files, $file);
                                        $st_count=$st_count + 1;
                                }
                        }
                        @file_lines=();
        }
        if ( $st_count == 0 ) {
        	if ($SUMMARY) { # Print only if full output requested
                	print "\n\nSYSTEM TESTS SUMMARY\n";
        	}
                print "\n\n";
                print "NOTE: Found no system test files.\n";
                return 0;
        }
        # Delete "./" from file name
        foreach (@act_st_files) {
                s/\.\///;
        }
        # Sort the act_st_files
        @act_st_files = sort (@act_st_files);

        # Get stripped system file names
        @st_st_files = @act_st_files;
        foreach ( @st_st_files) {
                s/\///g; # Delete all the "/"
                s/ESMF//;# Break first ESMF string
                s/ESMF/ ESMF/;# Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                s/\./ /; # Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$1.Log\n/; # Get rid of the 2nd field
        }
        #Sort the list of st_st_files
        @st_st_files = sort (@st_st_files);

        #go to the test directory
        $ok = chdir "$TEST_DIR/";
        if (not $ok) {
		print "\n\n";
        	if ($SUMMARY) { # Print only if full output requested
                	print "SYSTEM TESTS SUMMARY\n";
        	}
                #The test directory does not exist.
                print "\n\n";
                print "NOTE: There is no $TEST_DIR directory,\n";
                print "either the 'gmake ESMF_BOPT=$ESMF_BOPT build_system_tests' has not been run or ";
                print "the 'gmake  ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
                return 0;
        }
        else {
                # The tests output directory exists.
                @all_files = ();        # Clear all_files array
                find(\&wanted2, '.');
                sub wanted2 {
                                # Put all files in a list
                                push @all_files, "$File::Find::name\n"  if -e;
                }
                # Get *STest*.Log files in 2 steps
                @tmp_Log_files=grep (/Log/, @all_files);
                @Log_files=grep (/STest/, @tmp_Log_files);
                #Sort the list of Log files.
                @Log_files = sort (@Log_files);
                # Find the Log fles that are in the st_ex_files
                foreach $file ( @st_st_files) {
                                push @Log_st_files, grep (/$file/, @Log_files);
                }
                #Sort the list of Log files.
                @Log_st_files = sort (@Log_st_files);

		# For each system test we need to
		# find the corresponding Log file.
		# if it does not exist, add the system test to the crashed list.
		# If the Log file exists, read the number of processors.
		# Count the PASSes and compare to the number of processors
		# If they are not equal put the system test in the crashed list.
		# Keep track of pass count and failed tests list.
                $count=0;
                $pass_count=0;
                $fail_count=0;
                foreach $file ( @Log_st_files) {
                        open(F,$file);
                        foreach $line (<F>){
                                push(@file_lines, $line);
                        }
                        close ($file);
			#Read the pet count from Log file.
			$pet_count = &get_pet_count(@file_lines);
			if ($pet_count != 0) {
				$count=grep ( /PASS/, @file_lines);
				if ($count == $pet_count) {
                               		push (@pass_tests, $file);
                               		$pass_count=$pass_count + 1;
				}
				else {
					push (@fail_tests, $file);
				}			
			}
                        else {
				push (@fail_tests, $file);
			}
       		@file_lines=();
                }
                # Calculate fail_count
                $fail_count = $st_count - $pass_count;
		$system_test_count = $st_count;
                if ($pass_count != 0) {
                        #Strip the names of passed system_tests
                        foreach (@pass_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_st_files fles that are in the pass_tests
                        foreach $file ( @pass_tests) {
                                push @pass_st_files, grep (/$file.F90/, @act_st_files);
                        }
			if (!$SUMMARY) { # Print only if full output requested
                		print "\n\n";
                        	if ($pass_count == 1) {
                                	print "The following system test passed:\n";
                        	}
                        	else {
                                	print "The following system tests passed:\n";
                        	}
                        	print "\n\n";
                        	# Sort the pass_st_files
                        	@pass_st_files = sort (@pass_st_files);
                                foreach $file ( @pass_st_files ) {
					chomp($file);
					print ("PASS: $ESMF_COMM/$ESMF_BOPT: $file\n");
                                }
                        	print "\n\n";
                	}
		}
                if ($fail_count != 0) {
				# Find the act_st_files fles that are in the pass_tests
				# to create list of failed system tests.
                        	foreach $file ( @pass_st_files) {
                                foreach (@act_st_files){
                                        s/$file//s;
                                }

                        }


			if (!$SUMMARY) { # Print only if full output requested
                        	if ($fail_count == 1) {
                                	print "The following system test failed, did not build, or did not execute:\n";
                        	}
                        	else {
                                	print "The following system tests failed, did not build, or did not execute:\n";
                        	}
                        	print "\n\n";
                        	# Sort the fail_st_files
                        	@act_st_files = sort (@act_st_files);
                                foreach $file ( @act_st_files ) {
					#Do not print empty lines
					if (grep (/ESM/, $file)){
						chomp($file);
				  		print ("FAIL: $ESMF_COMM/$ESMF_BOPT: $file\n");
					}
                                }
                        	print "\n\n";
                	}
		}
                if ($fail_count == $st_count) {
                        # Check if there are any system test executable files
                        # The system test output directory exists.
                        @all_files = ();        # Clear all_files array
                        find(\&wanted3, '.');
                        sub wanted3 {
                                        # Put all executable files in a list
                                        push @all_files, "$File::Find::name\n"  if -x;
                        }
                        # Get *Ex files
                        @st_x_files=grep (/STest/, @all_files);
			@stdout_st_files = (); #Clear the file list.
                        # Delete the .stdout from each file
                        foreach ( @st_st_files) {
                                s/\.stdout//; # Delete stdout
                        }

                        # Find the system test executable files that are in the st_ex_files
                        foreach $file ( @st_st_files) {
                                        push @stdout_st_files, grep (/$file/, @st_x_files);
                        }
                        # Count the number system test in stdout_ex_files
                        $st_count = 0;
                        foreach $file ( @stdout_st_files) {
                                $st_count = $st_count + 1;
                        }
                        if ($st_count == 0) {
                                print "NOTE: There are no executable system test files, either the 'gmake ESMF_BOPT=$ESMF_BOPT build_system_tests' has \n";
                                print "not been run or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
                        }
                }
		 if (!$SUMMARY) { # Print only if full output requested
                	print "\n\nThe stdout files for the system_tests can be found at:\n";
                	print "$TEST_DIR\n\n\n";
		}
		else { # Print only if full output requested
			print "\n\nSYSTEM TESTS SUMMARY\n";
		}
		print "Found $system_test_count ";
        	if ($processor == 0) {
                	print "single processor system ";
		}
        	else {  
                	print "multi-processor system ";
		}
		if ($system_test_count == 1) { 
			print "test, ";
		}
		else {
			print "tests, ";
        	}   
		print "$pass_count passed and $fail_count failed.\n\n";

                # Write test results to be read by regression tests scripts.
                $results_file="$TEST_DIR/system_tests_results";
                open(MYHANDLE, ">$results_file");
                print MYHANDLE "PASS $pass_count FAIL $fail_count \n";

	}
}
1; # This is for the "require" function to work properly.
