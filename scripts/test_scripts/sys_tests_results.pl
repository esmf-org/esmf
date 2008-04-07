#!/usr/bin/env perl
# $Id: sys_tests_results.pl,v 1.7 2008/04/07 06:45:50 theurich Exp $
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running the system tests.
# The results are either complete results or a summary.

sub sys_tests_results($$$$) {

        my $TEST_DIR	= $_[0];
        my $ESMF_BOPT   = $_[1];
        my $SUMMARY     = $_[2];
        my $MPMDFLAG    = $_[3];


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
@fail_tests = ();	# system tests that failed
@pass_tests = ();	# system tests that passed

        #Find all files
        find(\&allFiles, '.');
        sub allFiles {
                        # Put all files in a list
                        push all_files, "$File::Find::name\n" if -T ;
        }
        # Get all system tests files
        @st_files=grep (/STest/, @all_files);
        # Find the system test files with the "ESMF_SYSTEM_TEST" string
        # grep for "ESMF_SYSTEM_TEST"
        $count=0;
        $st_count=0;
        foreach $file ( @st_files) {
                open(F,$file);
                foreach $line (<F>){
                        push(file_lines, $line);
                        }
                        close ($file);
                        $count=grep ( /ESMF_SYSTEM_TEST/, @file_lines);
                        if ($count != 0) {
                                push (act_st_files, $file);
                                        $st_count=$st_count + 1;
                                }
                        if ( $MPMDFLAG =~ m/^ON$/ ) {
                          $count=grep ( /ESMF_MPMD_SYSTEM_TEST/, @file_lines);
                          if ($count != 0) {
                                push (act_st_files, $file);
                                        $st_count=$st_count + 1;
                                }
                        }
                        @file_lines=();
        }
        if ( $st_count == 0 ) {
        	if ($SUMMARY) { # Print only if full output requested
                	print "\n\nSYSTEM TESTS SUMMARY\n";
        	}
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
                                push all_files, "$File::Find::name\n"  if -e;
                }
                # Get *STest*.stdout files in 2 steps
                @tmp_stdout_files=grep (/stdout/, @all_files);
                @stdout_files=grep (/STest/, @tmp_stdout_files);
                #Sort the list of stdout files.
                @stdout_files = sort (@stdout_files);
                # Find the stdout fles that are in the st_ex_files
                foreach $file ( @st_st_files) {
                                push @stdout_st_files, grep (/$file/, @stdout_files);
                }
                #Sort the list of stdout files.
                @stdout_st_files = sort (@stdout_st_files);

                # Count the number of PASS and FAIL
                # push pass system test to a list.
                $count=0;
                $pass_count=0;
                $fail_count=0;
                foreach $file ( @stdout_st_files) {
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
                # Calculate fail_count
                $fail_count = $st_count - $pass_count;
		$system_test_count = $st_count;
                if ($pass_count != 0) {
                        #Strip the names of failed system_tests
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
                        	print @pass_st_files;
                        	print "\n\n";
                	}
		}
                if ($fail_count != 0) {
                        # Find the act_st_files fles that are in the pass_tests
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
                        	# Sort the act_st_files
                        	@act_st_files = sort (@act_st_files);
                        	print @act_st_files;
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
                                        push all_files, "$File::Find::name\n"  if -x;
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
		print "Found $system_test_count system tests, $pass_count passed and $fail_count failed.\n\n";

	}
}
1; # This is for the "require" function to work properly.
