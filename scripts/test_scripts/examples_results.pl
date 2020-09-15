#!/usr/bin/env perl
# $Id$
# This subroutine is called at the end of the examples, "check_examples" and "check_results" targets.
# The purpose is to give the user the results of running the examples.
# The results are either complete results or a summary.

sub examples_results($$$) {

	my $EX_DIR	= $_[0];
    	my $ESMF_BOPT	= $_[1];
	my $ESMF_COMM	= $_[2];
    	my $SUMMARY	= $_[3];


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



use File::Find;

# Arrays of examples files
@act_ex_files = (); 	# Actual example files
@st_ex_files = ();	# Stripped example file names
@all_files = (); 	# All files
@log_files = (); 	# Examples log files 
@log_ex_files = ();	# log that are real examples
@pass_ex_files = ();	# passed examples files
@file_lines = ();	# file lines
@ex_x_files = ();	# examples executable files


        # Open the examples config file
        $ok=open(F,"$EX_DIR/examples.config");
        if (!(defined $ok)) {
                print "\n\n";
                if ($SUMMARY) { # Print only if full output requested
                        print "EXAMPLES SUMMARY\n";
                }
                print "NOTE: Unable to open $EX_DIR/examples.config file.\n";
                print "Either the 'gmake ESMF_BOPT=$ESMF_BOPT build_examples_tests' has not been run ";
                print "or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";

                return 0;
        }
        # Get flag from examples_config file.
        # processor = 0 for uni_processor
        # processor = 1 for multi_processor
        foreach $line (<F>){
			push(@file_lines, $line);
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
	# Get all example files
	@ex_files=grep (/Ex.F90$|Ex.C$/, @all_files);
	# Find the example files 
	# grep for "ESMF_EXAMPLE" and/or  "ESMF_MULTI_PROC_EXAMPLE" depending on "processor".
	$count=0;
	$ex_count=0;
	foreach $file ( @ex_files) {
		open(F,$file);
		foreach $line (<F>){
			push(@file_lines, $line);
			}
			close ($file);
			if ( $processor == 0) {
                                # Get the uni-PET examples
                                $count=grep ( /ESMF_EXAMPLE/, @file_lines);
                                if ($count != 0) {
                                        push (@act_ex_files, $file);
                                        $ex_count=$ex_count + 1;
                                }
                        }
                        else {
                                # Get the mult-PET only examples
                                $count=grep ( /ESMF_MULTI_PROC_EXAMPLE/, @file_lines);
                                if ($count != 0) {
                                        push (@act_ex_files, $file);
                                        $ex_count=$ex_count + 1;
                                }
                                # Include the uni-PET system tests
                                $count=grep ( /ESMF_EXAMPLE/, @file_lines);
                                if ($count != 0) {
                                        push (@act_ex_files, $file);
                                        $ex_count=$ex_count + 1;
                                }
                        }
                        @file_lines=();
	}
	if ( $ex_count == 0 ) {
	if ($SUMMARY) { # Print only if full output requested
		print "\n\nEXAMPLES SUMMARY\n";
	}
		print "NOTE: Found no example files.\n";
		return 0;
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
		s/ESM/ ESM/;# Break it into 2 fields
		s/([^ ]*) ([^ ]*)/$2/; # Get rid of the 1st field
		s/\./ /; # Break it into 2 fields
		s/([^ ]*) ([^ ]*)/$1.Log\n/; # Get rid of the 2nd field
	}
	#Sort the list of st_ex_files 
	@st_ex_files = sort (@st_ex_files);
	#go to the examples directory
	$ok = chdir "$EX_DIR/";

	if (not $ok) {
		print "\n\n";
		#The examples directory does not exist.
        	if ($SUMMARY) { # Print only if full output requested
                	print "EXAMPLES SUMMARY\n";
        	}
		print "NOTE: There is no $EX_DIR directory,\n";
		print "either the 'gmake ESMF_BOPT=$ESMF_BOPT build_examples' has not been run or ";
		print "the 'gmake  ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
		return 0;
	}
	else {
		# The examples output directory exists.
		@all_files = (); 	# Clear all_files array
		find(\&wanted2, '.'); 
		sub wanted2 {
				# Put all files in a list
			 	push @all_files, "$File::Find::name\n"  if -e;
		}
		# Get *Ex.Log files
		@log_files=grep (/Ex.Log/, @all_files);
		#Sort the list of Log files.
		@log_files = sort (@log_files);
		# Find the Log fles that are in the st_ex_files
               	foreach $file ( @st_ex_files) {
				push @log_ex_files, grep (/$file/, @log_files);
		}
		#Sort the list of log files.
		@log_ex_files = sort (@log_ex_files);


                # For each example we need to
                # find the corresponding Log file.
                # if it does not exist, add the system test to the crashed list.
                # If the Log file exists, read the number of processors.
                # Count the PASSes and compare to the number of processors
                # If they are not equal put the system test in the crashed list.
                # Keep track of pass count and failed tests list.

		$count=0;
		$pass_count=0;
		$fail_count=0;
		foreach $file ( @log_ex_files) {
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
		$fail_count = $ex_count - $pass_count;
		$example_count = $ex_count;
                if ($pass_count != 0) {
                        #Strip the names of passed examples
                        foreach (@pass_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_ex_files fles that are in the pass_tests
                        foreach $file ( @pass_tests) {
                                push @pass_ex_files, grep (/$file/, @act_ex_files);
                        }
			if (!$SUMMARY) { # Print only if full output requested
                        	print "\n\n";
                		if ($pass_count == 1) {
                        		print "The following example passed:\n";
				}
				else {
                        		print "The following examples passed:\n\n\n";
				}
                        	# Sort the pass_ex_files
                        	@pass_ex_files = sort (@pass_ex_files);
                                foreach $file ( @pass_ex_files ) {
                                	print ("PASS: $ESMF_COMM/$ESMF_BOPT: $file");
                                }
                        	print "\n\n";
			}
                }
		if ($fail_count != 0) {
                        # Find the act_ex_files fles that are in the pass_tests
			# to create list of failed examples.
                        foreach $file ( @pass_ex_files) {
                                foreach (@act_ex_files){
                                        s/$file//s;
                                }
			}

			if (!$SUMMARY) { # Print only if full output requested
				if ($fail_count == 1) {
					print "The following example failed, did not build, or did not execute:\n";
				}
				else {
					print "The following examples failed, did not build, or did not execute:\n";
				}
               			print "\n\n";
				# Sort the act_ex_files
				@act_ex_files = sort (@act_ex_files);
				# comment out until bug is fixed
                                foreach $file ( @act_ex_files ) {
                                	#Do not print empty lines
                                        if (grep (/ESM/, $file)){
                                                print ("FAIL: $ESMF_COMM/$ESMF_BOPT: $file");
                                        }
                                }
               			print "\n\n";
			}
		}
		
		if ($fail_count == $ex_count) {
			# Check if there are any example executable files
                	# The examples output directory exists.
                	@all_files = ();        # Clear all_files array
                	find(\&wanted3, '.');
                	sub wanted3 {
                                	# Put all executable files in a list
                                	push @all_files, "$File::Find::name\n"  if -x;
			}
			# Get *Ex files
			@ex_x_files=grep (/Ex/, @all_files);
			@log_ex_files = (); #Clear the file list.
                        # Delete the .Log from each file
                        foreach ( @st_ex_files) {
                                s/\.Log//; # Delete Log
                        }

			# Find the example executable fles that are in the st_ex_files
			foreach $file ( @st_ex_files) {
                                	push @log_ex_files, grep (/$file/, @ex_x_files);
                	}
			# Count the number examples in log_ex_files
			$ex_count = 0;
			foreach $file ( @log_ex_files) {
                             	$ex_count = $ex_count + 1;
                	}
			if ($ex_count == 0) {
				print "NOTE: There are no executable examples files, either the 'gmake ESMF_BOPT=$ESMF_BOPT build_examples' has \n";
				print "not been run or the 'gmake ESMF_BOPT=$ESMF_BOPT' did not build successfully. \n\n";
			}
		}

		if (!$SUMMARY) { # Print only if full output requested
			print "\n\nThe stdout files for the examples can be found at:\n";
			print "$EX_DIR\n\n\n";
		}
                else { # Print only if full output requested
                        print "\n\nEXAMPLES SUMMARY\n";
                }
                print "Found $example_count ";
                if ($processor == 0) {
                        print "single processor ";
                }
                else {
                        print "multi-processor ";
                }
                if ($examples_count == 1) {
                        print "example, ";
                }
                else {
                        print "examples, ";
                }
                print "$pass_count passed and $fail_count failed.\n\n";

                # Write test results to be read by regression tests scripts.
                $results_file="$EX_DIR/examples_results";
                open(MYHANDLE, ">$results_file");
                print MYHANDLE "PASS $pass_count FAIL $fail_count \n";
			
	}
}
1; # This is for the "require" function to work properly.

