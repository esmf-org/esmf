#!/usr/bin/perl
# $Id: do_st_results.pl,v 1.4 2005/02/03 22:56:03 svasquez Exp $
# This script runs at the end of the system tests and "check_results" targets.
# The purpose is to give the user the results of running the system tests.

# Options:
#
#  -d	TEST_DIR
#  -b   ESMF_BOPT 
#

use Getopt::Std;
use File::Find

# Arrays of system tests files
@s_t_ex = ();		# System Test executable files
@st_files = ();		# System Test files
@act_st_files = ();	# Actual system Test files
@st_st_files = ();	# Stripped system Test files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@stdout_files = (); 	# System Test stdout files 
@file_lines = ();	# stdout file lines
@fail_tests = ();	# system tests that failed
@pass_tests = ();	# system tests that passed
%options = ();		#arguments

getopts("d:b:", \%options); 


	$TEST_DIR = "$options{d}"; 
	$ESMF_BOPT = "$options{b}";

        #Find all files
        find(\&allFiles, '.');
        sub allFiles {
                        # Put all files in a list
                        push all_files, "$File::Find::name\n" if -e ;
        }
        # Get all system tests files
        @st_files=grep (/STest/, @all_files);
        # Find the system test files with the "!SYSTEM_TEST" string
        # grep for "!SYSTEM_TEST"
        $count=0;
        $st_count=0;
        foreach $file ( @st_files) {
                open(F,$file);
                foreach $line (<F>){
                        push(file_lines, $line);
                        }
                        close ($file);
                        $count=grep ( /!SYSTEM_TEST/, @file_lines);
                        if ($count != 0) {
                                push (act_st_files, $file);
                                        $st_count=$st_count + 1;
                                }
                        @file_lines=();
        }
        #if ( $st_count == 0 ) {
                #print "NOTE: Found no system test files.\n";
                #exit 0;
        #}
        # Delete "./" from file name
        foreach (@act_st_files) {
                s/\.\///;
        }
        # Sort the act_st_files
        @act_st_files = sort (@act_st_files);

	print "\n\n";
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
                #The test directory does not exist.
                print "NOTE: There is no $TEST_DIR directory,\n";
                print "either the 'gmake ESMF_BOPT=$ESMF_BOPT build_system_tests' has not been run or \n";
                print "the 'gmake  ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
                exit 0;
        }
        else {
                # The tests output directory exists.
                @all_files = ();        # Clear all_files array
                find(\&wanted2, '.');
                sub wanted2 {
                                # Put all files in a list
                                push all_files, "$File::Find::name\n"  if -e;
                }
                # Get *STest.stdout files
                @stdout_files=grep (/STest.stdout/, @all_files);
                #Sort the list of stdout files.
                @stdout_files = sort (@stdout_files);
                # Find the stdout fles that are in the st_ex_files
                foreach $file ( @st_st_files) {
                                push @stdout_st_files, grep (/$file/, @stdout_files);
                }
                #Sort the list of stdout files.
                @stdout_st_files = sort (@stdout_st_files);

                # Count the number of PASS and FAIL
                # push pass examples tests to a list.
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
                print "\n\n";
                if ($pass_count == $st_count) {
			if ($pass_count == 1) {
                        	print "There is $st_count system test, it passed.\n";
			}
			else {
                        	print "There are $st_count system tests, they all passed.\n";
			}
                }
                elsif ($fail_count == $st_count) {
			if ($fail_count == 1) {
                        	print "There is $st_count system test, it failed.\n";
			}
			else {
                        	print "There are $st_count system tests, they all failed.\n";
			}
                }
                else {
                        print "There are $st_count system tests, $pass_count passed and $fail_count failed.\n";
                }
                print "\n\n";
                if ($pass_count != 0) {
                        #Strip the names of failed system_tests
                        foreach (@pass_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_st_files fles that are in the pass_tests
                        foreach $file ( @pass_tests) {
                                push @pass_st_files, grep (/$file/, @act_st_files);
                        }
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
                if ($fail_count != 0) {
                        # Find the act_st_files fles that are in the pass_tests
                        foreach $file ( @pass_st_files) {
                                foreach (@act_st_files){
                                        s/$file//s;
                                }
                        }
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
                else{
                        print "The stdout files for the system_tests can be found at:\n";
                        print "$TEST_DIR\n\n";
                }







}
exit 0;





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

