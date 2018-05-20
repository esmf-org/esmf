#!/usr/bin/env perl
# $Id$
# This script runs when the user wants to compare the elapsed time of the current
# elapsed time of the current uit tests to a previously benchmarked value.
# The tolerance is provided by the user and is used to determine acceptability.
# The results are either complete results or a summary.

sub unit_tests_bm_results($$$$$) {

        my $TEST_DIR    = $_[0];
	my $BM_DIR	= $_[1];
	my $TOLERANCE	= $_[2];
	my $THRESHOLD	= $_[3];
        my $ESMF_BOPT   = $_[4];
        my $SUMMARY     = $_[5];



# This subroutine compare the average test elapsed times of the
# unit test to the benchmark and returns PASS/FAIL
# and test_ET, BM_ET, percent.
sub run_benchmark {
	my @list = @_;
	
	
        my $testfile = $_[0];
        my $test_dir = $_[1];
        my $bm_dir = $_[2];
        my $tolerance = $_[3];
        my $threshold = $_[4];

	$ET_ave = 0;
	$BM_ave = 0;
	$ans = 0;
	$th = 1;
        # open the testfile and read the elapsed times.
	chomp($test_file);
        $ok=open(F,"$test_dir/$testfile");
        if (!(defined $ok)) {
        # if the stdout file is not present return FAIL
		return($ET_ave, $BM_ave, $ans, 2);
        }else { 
		@ET_list = ();
		@BM_list = ();
        	$test_string_found = 0;
        	while (<F>) { #read the file unitil "Test Elapsed Time" is found
			s/ msec.$//;
        		($test_string,$test_ET) = split(/Time/, $_);
			$count = grep ( /Test Elapsed/, $test_string);
			if ($count != 0 ){
				$test_string_found = 1;
				push(@ET_list,$test_ET);
			} 
		}
	}
	$sum=0;
	$count=0;
	foreach $test_ET (@ET_list) {
		$sum=$sum + $test_ET;
		$count=$count + 1;
	}
	if ($count != 0 ){
		$ET_ave = $sum/$count;
	} else {
		$ET_ave = 0;
	}
        # open the testfile and read the elapsed times.
        $ok=open(F,"$bm_dir/$testfile");
        if (!(defined $ok)) {
        	# if the stdout file is not present return FAIL
		 return($ET_ave, $BM_ave, $ans, ,$th, 3);
        }else {
        	$test_string_found = 0;
        	while (<F>) { #read the file unitil "Test Elapsed Time" is found
			s/ msec.$//;
        		($test_string,$bm_ET) = split(/Time/, $_);
        		$count = grep ( /Test Elapsed/, $test_string);
        		if ($count != 0 ){
        			$test_string_found = 1;
				push(@BM_list,$bm_ET);
        		}
        	}
        }
        $sum=0;
        $count=0;
        foreach $bm_ET (@BM_list) {
                $sum=$sum + $bm_ET;
                $count=$count + 1;
        }
        if ($count != 0 ){
        	$BM_ave = $sum/$count;
        } else {
                $BM_ave = 0;
        }
	# Compare the test elapsed time to threshold
	if ( $ET_ave >= $threshold ) {
		$th = 0;
	} else {
		$th = 1;  
	}
	if (( $ET_ave == 0) || ( $BM_ave == 0)){
		return($ET_ave, $BM_ave, $ans, $th, 0);
	}
	$ans=(($ET_ave - $BM_ave)/$BM_ave);
	$ans = sprintf("%0.4f", $ans);
	if ( $ET_ave <= $BM_ave ) {
		return($ET_ave, $BM_ave, $ans, $th, 0);
	} else {
		if ($ans <= $tolerance ) {
			return($ET_ave, $BM_ave, $ans, $th, 0);
		} else {
			return($ET_ave, $BM_ave, $ans, $th, 1);
		}
	}
		
        
}
use File::Find

# Arrays of unit tests files
@ut_files = ();		# Unit Test files
@temp_files = ();	# Unit Test files
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
	# Delete files from previous run.
	system("rm -fr $TEST_DIR/bm_fail_info"); 
	system("rm -fr $TEST_DIR/bm_pass_info"); 
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
        find(\&allFiles, '.');
        sub allFiles {
                        # Put all files in a list
                        push @all_files, "$File::Find::name\n" ;
        }
        # Get all source unit tests files
        @c_files=grep (/UTest.C/, @all_files);
        @F90_files=grep (/UTest.F90/, @all_files);
        foreach $file ( @c_files) {
                push (@F90_files, $file);               
        }
	@st_ut_files = @F90_files;
        @ut_files = @st_ut_files;
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
	

	$total_test_count = 0;
	$fail_count = 0;
	$pass_count = 0;
	$match_count = 0;
        @ut_files = sort(@ut_files);
        foreach $file ( @ut_files) {
                	open(F,$file);
                	foreach $line (<F>){
                                push(@file_lines, $line);
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

			#Convert % to decimal
			$d_tol = $TOLERANCE/100;
			# Find the corresponding stdout file if the test count is not zero
			if ($test_count != 0) {
				$test_file = $file;
				foreach ($test_file) {
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
				# Open the Log file for this test and read how many processors it used
				@file_lines = ();
				chomp($test_file);
				my ( $test_ET, $bm_ET, $ans, $th, $rc)  =  run_benchmark($test_file, $TEST_DIR, $BM_DIR, $d_tol, $THRESHOLD);
				
				$total_file_count = $total_file_count + 1;
				if ( $th == 0 ) {
					if ( $rc == 0 ) {
						# The BM test passed
						$pass_count = $pass_count + 1;
						push(@pass_list, $file);
						$file =~ s/\.\/// ;
                                        	$PC=$ans*100;
                                        	chomp($file);
                                        	system ("echo 'Benchmark PASS: $file' >> $TEST_DIR/bm_pass_info ");
                                        	system ("echo '      Test elapsed time: $test_ET msec.' >> $TEST_DIR/bm_pass_info ");
                                        	system ("echo '      Benchmark elapsed time: $bm_ET msec.' >> $TEST_DIR/bm_pass_info ");
                                        	system ("echo '      Difference: $PC%' >> $TEST_DIR/bm_pass_info ");
                                        	system ("echo '' >> $TEST_DIR/bm_pass_info ");
					} elsif ( $rc == 1 ) {
						# The BM test failed
						$fail_count = $fail_count + 1;
						push(@fail_list, $file);
						$file =~ s/\.\/// ;
						$PC=$ans*100;
						chomp($file);
						system ("echo 'Benchmark FAIL: $file' >> $TEST_DIR/bm_fail_info ");
						system ("echo '      Test elapsed time: $test_ET msec.' >> $TEST_DIR/bm_fail_info ");
						system ("echo '      Benchmark elapsed time: $bm_ET msec.' >> $TEST_DIR/bm_fail_info ");
						system ("echo '      Increase: $PC%' >> $TEST_DIR/bm_fail_info ");
						system ("echo '' >> $TEST_DIR/bm_fail_info ");
					} elsif ( $rc == 2 ) {
						# The stdout file in test_dir not found
						$match_count = $match_count + 1;
						push(@test_list, $file);
					} elsif ( $rc == 3 ) {
						# The stdout file in bm_dir not found
						$match_count = $match_count + 1;
						push(@bm_list, $file);
					}
				}
					
			}
			$test_count =0;		
				
        }

	# sort all lists
	@pass_list=sort(@pass_list);
	@fail_list=sort(@fail_list);
	@bm_list=sort(@bm_list);
	@test_list=sort(@test_list);

	# Delete ./ from all lists
        foreach ( @pass_list) {
                s/\.\//Benchmark PASS: /; # Delete all the "./"
	}
        foreach ( @test_list) {
                s/\.\//TEST FILE NOT FOUND: /; # Delete all the "./"
	}
        foreach ( @bm_list) {
                s/\.\//benchmark FILE NOT FOUND: /; # Delete all the "./"
	}
        foreach ( @fail_list) {
                s/\.\///; # Delete all the "./"
	}
	if (!$SUMMARY) { # Print only if full output requested
        	# Print to the screen
		if (@pass_list != ()){
			print "\n\nThe following unit tests with a threshold of $THRESHOLD msecs. passed the $TOLERANCE tolerance benchmark test:\n\n";
			print @pass_list;
		}
		if ($fail_count != 0){
			print "\n\nThe following unit tests with a threshold of $THRESHOLD msecs. failed the $TOLERANCE tolerance benchmark test:\n\n";
			system("cat $TEST_DIR/bm_fail_info");
		}
		if (@test_list != ()){
			print "\n\nThe following unit test stdout files were not found in ESMF TESTDIR:\n\n";
			print @test_list;
		}
		if (@bm_list != ()){
			print "\n\nThe following unit test stdout files were not found in Benchmark TESTDIR:\n\n";
			print @bm_list;
		}
	}

	if ($total_file_count == 0) {
		print "\n\nNOTE: Found no ";
	}
	else {
        	print "\n\nFound $total_file_count ";
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
	if ($total_file_count == 0) {
		print ".\n\n";
	}
	else {
		print " files, of those with a threshold of $THRESHOLD msecs. $pass_count passed the $TOLERANCE tolerance benchmark test, ";
	}
	if ($match_count == 0) {
                print "and $fail_count failed.\n\n";
        }
	else {
		print "$fail_count failed and $match_count could not be matched.\n\n";
	}
	print "Benchmark install date: ";
	system("cat $BM_DIR/bm_timestamp");
	print "\n\n";



        # Write test results to be read by regression tests scripts.
        $results_file="$TEST_DIR/unit_tests_bm_results";
        open(MYHANDLE, ">$results_file");
        print MYHANDLE "PASS $pass_count FAIL $fail_count  MATCH $match_count \n";

}
1; # This is for the "require" function to work properly.


