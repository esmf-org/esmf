#!/usr/bin/perl
# $Id: do_summary.pl,v 1.2 2004/08/05 15:43:13 svasquez Exp $
# This prints a summary of system tests, unit tests ansd examples.

# Options:
#
#  -d	TEST_DIR
#
#  -e	EX_DIR
#  

use Getopt::Std;
use File::Find

# Arrays of system tests files
@ex_files = (); 	# All executable files
@all_files = (); 	# All files
@stdout_files = (); 	# stdout files 
@file_lines = ();	# stdout file lines
%options = ();		#arguments

getopts("d:e:", \%options); 


	$TEST_DIR = "$options{d}"; 

	$EX_DIR   = "$options{e}"; 

	#go to the test directory
	chdir "$TEST_DIR/";

	find(\&wanted, '.'); 
	sub wanted {
			# Put all executable files in a list
			 push ex_files, "$File::Find::name\n" if -x ;
	}
	# Get count of executable System Tests found
	$st_exe_count=grep (/STest$/, @ex_files);
	# Get count of executable unit Tests found
	$ut_exe_count=grep (/UTest$/, @ex_files);
	# Get executable unit tests files


	find(\&wanted2, '.'); 
	sub wanted2 {
			# Put all files in a list
			 push all_files, "$File::Find::name\n"  if -e;
	}
	# Get count of *STest.stdout files found
	$st_std_count=grep (/STest.stdout/, @all_files);
	# Get *STest.stdout files 
        @stdout_files=grep (/STest.stdout/, @all_files);

        if ($st_std_count eq 0) {
                $st_pass_count = 0;
		$st_fail_count = 0;
        }
	else {
		# Count the number of PASS and FAIL.
		$count=0;
		$st_pass_count=0;
		$st_fail_count=0;
        	foreach $file ( @stdout_files) {
			open(F,$file);
			foreach $line (<F>){
				push(file_lines, $line);
			}
			close ($file);
			$count=grep ( /PASS/, @file_lines);
			if ($count != 0) {
				$st_pass_count=$st_pass_count + 1;
			}
			else {
				$st_fail_count=$st_fail_count + 1;
			}
			@file_lines = ();
         	}
	}
	
	# Clear list of system tests stdout files
	@stdout_files = ();
	# Get count of *UTest.stdout files found
	$ut_std_count=grep (/UTest.stdout/, @all_files);
	# Get *UTest.stdout files 
        @stdout_files=grep (/UTest.stdout/, @all_files);

        if ($ut_std_count eq 0) {
                $ut_pass_count = 0;
		$ut_fail_count = 0;
        }
	else {
		# Count the number of PASS and FAIL.
		$count=0;
		$ut_pass_count=0;
		$ut_fail_count=0;
        	foreach $file ( @stdout_files) {
			open(F,$file);
			foreach $line (<F>){
				push(file_lines, $line);
			}
			close ($file);
			$count=grep ( /PASS/, @file_lines);
			$ut_pass_count=$ut_pass_count + $count;
			$count=grep ( /FAIL/, @file_lines);
                        $ut_fail_count=$ut_fail_count + $count;
			# Clear file lines for next loop so PASS/FAIL
			# are not recounted.
			@file_lines = ();
         	}
	}

	# start with cleared lists.
	@ex_files = ();
	@all_files = ();
        #go to the examples directory
        chdir "$EX_DIR/";

        find(\&wanted, '.'); 
        sub wanted {
                        # Put all executable files in a list
                         push ex_files, "$File::Find::name\n" if -x ;
        }
        # Get count of executable Examples found
        $ex_exe_count=grep (/Ex$/, @ex_files);

        find(\&wanted2, '.'); 
        sub wanted2 {
                        # Put all files in a list
                         push all_files, "$File::Find::name\n"  if -e;
        }
	@stdout_files = ();
        # Get count of *Ex.stdout files found
        $ex_std_count=grep (/Ex.stdout/, @all_files);
        # Get *Ex.stdout files
        @stdout_files=grep (/Ex.stdout/, @all_files);

        if ($ex_std_count eq 0) {
		$ex_pass_count = 0;
		$ex_fail_count = 0;
        }
        else {
                # count the number of PASS and FAIL
                $count=0;
                $pass_count=0;
                foreach $file ( @stdout_files) {
                        open(F,$file);
                        foreach $line (<F>){
                                push(file_lines, $line);
                        }
                        close ($file);
                        $count=grep ( /PASS/, @file_lines);
                        if ($count != 0) {
                                $ex_pass_count=$ex_pass_count + 1;
                        }
                        else {
                                $ex_fail_count=$ex_fail_count + 1;
                        }
			# Clear file lines for next loop so PASS/FAIL
			# are not recounted.
                        @file_lines=();
                }
	}
	print "\n\n";
	print "Unit Test, Example, and System Test summary.\n\n";
	print "$ut_exe_count unit test executables found.\n";
	print "$ut_std_count unit tests ran.\n";
	print "$ut_pass_count unit tests passed.\n";
	print "$ut_fail_count unit tests failed. \n\n";

	print "$ex_exe_count examples executables found.\n";
	print "$ex_std_count examples ran.\n";
	print "$ex_pass_count examples passed.\n";
	print "$ex_fail_count examples failed. \n\n";

	print "$st_exe_count system tests executables found.\n";
	print "$st_std_count system tests ran.\n";
	print "$st_pass_count system tests passed.\n";
	print "$st_fail_count system tests failed. \n\n";
exit 0;

