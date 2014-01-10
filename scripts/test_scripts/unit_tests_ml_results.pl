#!/usr/bin/env perl
# $Id$
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running memory leak on the system tests.
# The results are either complete results or a summary.

sub unit_tests_ml_results($$$$) {

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
        $ok=open(F,"$TEST_DIR/unit_tests.config");
        if (!(defined $ok)) {
                print "\n\n";
                if ($SUMMARY) { # Print only if full output requested
                        print "UNIT TESTS SUMMARY\n";
                }
                print "NOTE: Unable to open $TEST_DIR/unit_tests.config file.\n";
                print "Either the 'gmake ESMF_BOPT=$ESMF_BOPT build_system_tests' has not been run ";
                print "or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
                
                return 0;
        } 
        # Get flags from unit_tests_config file.
        # exhaustive = 0 for ESMF_TESTEXHAUSTIVE=OFF
        # exhaustive = 1 for ESMF_TESTEXHAUSTIVE=ON
        # processor = 0 for uni_processor
        # processor = 1 for multi_processor
        foreach $line (<F>){
                        push(@file_lines, $line);
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
        find(\&wanted, '.');
        sub wanted {
                        ## Put all files in a list
			push @all_files, "$File::Find::name\n" ;
        }
        # Get all unit tests files
        @temp_files=grep (/UTest/, @all_files);
        @act_ut_files=grep(!/cppF90/, @temp_files);


        # Delete "./" from file name
        foreach (@act_ut_files) {
                s/\.\///;
        }
        # Sort the act_ut_files
        @act_ut_files = sort (@act_ut_files);

        # Get stripped unit test file names
        @st_ut_files = @act_ut_files;
                foreach ( @st_ut_files) {
                s/\.\///; # Delete all the "./"
                s/\///g; # Delete all the "/"
                s/ESMF_/ ESMF_/;# Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                s/ESMC_/ ESMC_/;# Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                s/ESMCI_/ ESMCI_/;# Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
                s/\./ /; # Break it into 2 fields
                s/([^ ]*) ([^ ]*)/$1.stdout\n/; # Get rid of the 2nd field
        }

        #Sort the list of st_ut_files
        @st_ut_files = sort (@st_ut_files);

        #go to the test directory
        $ok = chdir "$TEST_DIR/";
        if (not $ok) {
		print "\n\n";
        	if ($SUMMARY) { # Print only if full output requested
                	print "UNIT TESTS SUMMARY\n";
        	}
                #The test directory does not exist.
                print "\n\n";
                print "NOTE: There is no $TEST_DIR directory,\n";
                print "either the 'gmake ESMF_BOPT=$ESMF_BOPT build_unit_tests' has not been run or ";
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
                # Get *UTest*.stdout files in 2 steps
                @tmp_stdout_files=grep (/stdout/, @all_files);
                @stdout_files=grep (/UTest/, @tmp_stdout_files);
                #Sort the list of stdout files.
                @stdout_files = sort (@stdout_files);
                # Delete repeated lines in st_ut_files
                $sorted_st_ut_files = ();
                foreach $file (@st_ut_files){
                        # if not in the list push it in
                        if (grep (/$file/, @sorted_st_ut_files) == 0) {
                                push (@sorted_st_ut_files, $file);
                        }
                }
		@st_ut_files = sort (@sorted_st_ut_files);

                # Find the sdout fles that are in the st_ex_files
                foreach $file ( @st_ut_files) {
                                push @stdout_ut_files, grep (/$file/, @stdout_files);
                }
                #Sort the list of stdout files.
                @stdout_ut_files = sort (@stdout_ut_files);

		# For each unit test we need to
		# find the corresponding stdout file.
		# if it does not exist, add the unit test to the crashed list.
		# If the sdout file exists, grep for "LEAK".
		# If "LEAK" is found then put the unit test in the leak list.
		# Keep track of Leak count and no leak count tests list.
                $count=0;
                $leak_count=0;
                $no_leak_count=0;
                foreach $file ( @stdout_ut_files) {
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
				$no_leak_count=$no_leak_count + 1 ;
			}			
       			@file_lines=();
                }


                # Calculate the unit test count
		$unit_test_count = $no_leak_count + $leak_count;
                if ($leak_count != 0) {
                        #Strip the names of unit tests that have memory leaks.
                        foreach (@leak_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_ut_files fles that are in the leak_tests
                        foreach $file ( @leak_tests) {
                                push @leak_ut_files, grep (/$file.F90/, @act_ut_files);
                                push @leak_ut_files, grep (/$file.C/, @act_ut_files);
                        }
		}
                if ($no_leak_count != 0) {
                        #Strip the names of unit tests that have memory no leaks.
                        foreach (@no_leak_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_ut_files fles that are in the leak_tests
                        foreach $file ( @no_leak_tests) {
                                push @no_leak_ut_files, grep (/$file.F90/, @act_ut_files);
                                push @no_leak_ut_files, grep (/$file.C/, @act_ut_files);
                        }

			if (!$SUMMARY) { # Print only if full output requested
                        	print "\n\n";
                        	if ($no_leak_count == 1) {
                                	print "The following unit test have no memory leaks\n";
                        	}
                        	else {
                                	print "The following unit tests have no memory leaks:\n";
                        	}
                        	print "\n\n";
                        	# Sort the no_leak_ut_files
                        	@no_leak_ut_files = sort (@no_leak_ut_files);
                                foreach $file ( @no_leak_ut_files ) {
					#Do not print empty lines
					if (grep (/ESM/, $file)){
				  		print ("No memory leaks: $file");
					}
                                }
                        	print "\n\n";

                        if (!$SUMMARY) { # Print only if full output requested
                                print "\n\n";
                                if ($leak_count == 1) {
                                        print "The following unit test have memory leaks:\n";
                                }
                                else {
                                        print "The following unit tests have memory leaks:\n";
                                }
                                print "\n\n";
                                # Sort the leak_ut_files
                                @leak_ut_files = sort (@leak_ut_files);
                                foreach $file ( @leak_ut_files ) {
                                        print ("Memory Leaks: $file");
                                }
                                print "\n\n";
                        }

                	}
		}
                if ($no_leak_count == $ut_count) {
                        # Check if there are any unit test executable files
                        # The unit test output directory exists.
                        @all_files = ();        # Clear all_files array
                        find(\&wanted3, '.');
                        sub wanted3 {
                                        # Put all executable files in a list
                                        push @all_files, "$File::Find::name\n"  if -x;
                        }
                        # Get *Ex files
                        @ut_x_files=grep (/UTest/, @all_files);
			@stdout_ut_files = (); #Clear the file list.
                        # Delete the .stdout from each file
                        foreach ( @st_ut_files) {
                                s/\.stdout//; # Delete stdout
                        }

                        # Find the unit test executable files that are in the st_ex_files
                        foreach $file ( @st_ut_files) {
                                        push @stdout_ut_files, grep (/$file/, @ut_x_files);
                        }
                        # Count the number unit test in stdout_ex_files
                        $ut_count = 0;
                        foreach $file ( @stdout_ut_files) {
                                $ut_count = $ut_count + 1;
                        }
print $ut_count;
                        if ($ut_count == 0) {
                                print "NOTE: There are no executable unit test files, either the 'gmake ESMF_BOPT=$ESMF_BOPT build_unit_tests' has \n";
                                print "not been run or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
                        }
                }
		 if (!$SUMMARY) { # Print only if full output requested
                	print "\n\nThe stdout files for the unit_tests can be found at:\n";
                	print "$TEST_DIR\n\n\n";
		}
		else { # Print only if full output requested
			print "\n\nSYSTEM TESTS SUMMARY\n";
		}
		print "Found $unit_test_count ";
        	if ($processor == 0) {
                	print "single processor unit ";
		}
        	else {  
                	print "multi-processor unit ";
		}
		if ($unit_test_count == 1) { 
			print "test, ";
		}
		else {
			print "tests, ";
        	}   
		print "$no_leak_count have no memory leaks and $leak_count have memory leaks.\n\n";

                # Write test results to be read by regression tests scripts.
                $results_file="$TEST_DIR/unit_tests_ml_results";
                open(MYHANDLE, ">$results_file");
                print MYHANDLE "NO_LEAK $no_leak_count LEAK $leak_count \n";

	}
}
1; # This is for the "require" function to work properly.
