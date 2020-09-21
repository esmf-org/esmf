#!/usr/bin/env perl
# $Id$
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running memory leak on the system tests.
# The results are either complete results or a summary.

sub sys_tests_ml_results($$$$) {

        my $TEST_DIR	= $_[0];
        my $ESMF_BOPT   = $_[1];
        my $SUMMARY     = $_[2];



use File::Find

# Arrays of system tests files
@s_t_ex = ();		# System Test executable files
@st_files = ();		# System Test files
@act_st_files = ();	# Actual system Test files
@st_st_files = ();	# Stripped system Test files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@stdout_files = (); 	# System Test stdout files 
@tmp_stdout_files = (); # tmp system Test stdout files 
@file_lines = ();	# stdout file lines
@no_leak_tests = ();	# system tests that habe no memory leaks
@leak_tests = ();	# system tests that have memory


                        
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
        @st_files=grep (/STest.F90$|STest.C$/, @all_files);
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
                s/([^ ]*) ([^ ]*)/$1.stdout\n/; # Get rid of the 2nd field
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
                # Get *STest*.stdout files in 2 steps
                @tmp_stdout_files=grep (/stdout/, @all_files);
                @stdout_files=grep (/STest/, @tmp_stdout_files);
                #Sort the list of stdout files.
                @sdout_files = sort (@sdout_files);
                # Find the sdout fles that are in the st_ex_files
                foreach $file ( @st_st_files) {
                                push @stdout_st_files, grep (/$file/, @stdout_files);
                }
                #Sort the list of stdout files.
                @stdout_st_files = sort (@stdout_st_files);

		# For each system test we need to
		# find the corresponding stdout file.
		# if it does not exist, add the system test to the crashed list.
		# If the sdout file exists, grep for "LEAK".
		# If "LEAK" i found then put the system test in the leak list.
		# Keep track of Leak count and no leak count tests list.
                $count=0;
                $leak_count=0;
                $no_leak_count=0;
                foreach $file ( @stdout_st_files) {
                        open(F,$file);
                        foreach $line (<F>){
                                push(@file_lines, $line);
                        }
                        close ($file);
			$count=grep ( /LEAK/, @file_lines);
			if ($count != 0) {
                       		push (@leak_tests, $file);
                        	$leak_count=$leak_count + 1;
			}
			else {
				push (@no_leak_tests, $file);
			}			
       			@file_lines=();
                }
                # Calculate no_leak_count
                $no_leak_count = $st_count - $leak_count;
		$system_test_count = $st_count;
                if ($leak_count != 0) {
                        #Strip the names of system_tests that have memory leaks.
                        foreach (@leak_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_st_files fles that are in the leak_tests
                        foreach $file ( @leak_tests) {
                                push @leak_st_files, grep (/$file.F90/, @act_st_files);
                        }
		}
                if ($no_leak_count != 0) {
				# Find the act_st_files fles that are in the leak_tests
				# to create list of system tests with no memory leaks..
                        	foreach $file ( @leak_st_files) {
                                foreach (@act_st_files){
                                        s/$file//s;
                                }

                        }


			if (!$SUMMARY) { # Print only if full output requested
                        	print "\n\n";
                        	if ($no_leak_count == 1) {
                                	print "The following system test have no memory leaks\n";
                        	}
                        	else {
                                	print "The following system tests have no memory leaks:\n";
                        	}
                        	print "\n\n";
                        	# Sort the no_leak_st_files
                        	@act_st_files = sort (@act_st_files);
                                foreach $file ( @act_st_files ) {
					#Do not print empty lines
					if (grep (/ESM/, $file)){
				  		print ("No memory leaks: $file");
					}
                                }
                        	print "\n\n";

                        if (!$SUMMARY) { # Print only if full output requested
                                print "\n\n";
                                if ($leak_count == 1) {
                                        print "The following system test have memory leaks:\n";
                                }
                                else {
                                        print "The following system tests have memory leaks:\n";
                                }
                                print "\n\n";
                                # Sort the leak_st_files
                                @leak_st_files = sort (@leak_st_files);
                                foreach $file ( @leak_st_files ) {
                                        print ("Memory Leaks: $file");
                                }
                                print "\n\n";
                        }

                	}
		}
                if ($no_leak_count == $st_count) {
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
		print "$no_leak_count have no memory leaks and $leak_count have memory leaks.\n\n";

                # Write test results to be read by regression tests scripts.
                $results_file="$TEST_DIR/system_tests_ml_results";
                open(MYHANDLE, ">$results_file");
                print MYHANDLE "NO_LEAK $no_leak_count LEAK $leak_count \n";

	}
}
1; # This is for the "require" function to work properly.
