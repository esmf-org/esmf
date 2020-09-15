#!/usr/bin/env perl
# $Id$
# This subroutine is called at the end of the examples, "check_examples" and "check_results" targets.
# The purpose is to give the user the results of running the examples.
# The results are either complete results or a summary.

sub examples_ml_results($$$) {

	my $EX_DIR	= $_[0];
    	my $ESMF_BOPT	= $_[1];
    	my $SUMMARY	= $_[2];




use File::Find;

# Arrays of examples files
@act_ex_files = (); 	# Actual example files
@st_ex_files = ();	# Stripped example file names
@all_files = (); 	# All files
@stdout_files = (); 	# Examples stdout files 
@stdout_ex_files = ();	# stdout that are real examples
@leak_ex_files = ();	# leaked examples files
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
		s/([^ ]*) ([^ ]*)/$1.stdout\n/; # Get rid of the 2nd field
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
		# Get *Ex.stdout files
		@stdout_files=grep (/Ex.stdout/, @all_files);
		#Sort the list of stdout files.
		@stdout_files = sort (@stdout_files);
		# Find the stdout fles that are in the st_ex_files
               	foreach $file ( @st_ex_files) {
				push @stdout_ex_files, grep (/$file/, @stdout_files);
		}
		#Sort the list of stdout files.
		@stdout_ex_files = sort (@stdout_ex_files);


                # For each example we need to
                # find the corresponding stdout file.
                # if it does not exist, add the system test to the crashed list.
                # If the stdout file exists, grep for "LEAK".
                # Keep track of leak count and no_leaked tests list.

		$count=0;
		$leak_count=0;
		$no_leak_count=0;
		foreach $file ( @stdout_ex_files) {
			open(F,$file);
			foreach $line (<F>){
				push(@file_lines, $line);
			}
			close ($file);
                        $count=grep ( /LEAK/, @file_lines);
                        if ($count != 0 ) {
                        	push (@leak_tests, $file);
                               	$leak_count=$leak_count + 1;
                        }
                        else {
                        	push (@no_leak_tests, $file);
                        }
                @file_lines=();
		}
		# Calculate no_leak_count
		$no_leak_count = $ex_count - $leak_count;
		$example_count = $ex_count;
                if ($leak_count != 0) {
                        #Strip the names of leaked examples
                        foreach (@leak_tests) {
                                s/\.\///; # Delete "./"
                                s/\./ /; # Break it into 2 fields
                                s/([^ ]*) ([^ ]*)/$1/; # Get rid of the 2nd field
                        }
                        # Find the act_ex_files fles that are in the leak_tests
                        foreach $file ( @leak_tests) {
                                push @leak_ex_files, grep (/$file/, @act_ex_files);
                        }
                }
		if ($no_leak_count != 0) {
                        # Find the act_ex_files fles that are in the leak_tests
			# to create list of no_leaked examples.
                        foreach $file ( @leak_ex_files) {
                                foreach (@act_ex_files){
                                        s/$file//s;
                                }
			}

			if (!$SUMMARY) { # Print only if full output requested
               			print "\n\n";
				if ($no_leak_count == 1) {
					print "The following example has no memory leaks:\n";
				}
				else {
					print "The following examples have no memory leaks:\n";
				}
               			print "\n\n";
				# Sort the act_ex_files
				@act_ex_files = sort (@act_ex_files);
				# comment out until bug is fixed
                                foreach $file ( @act_ex_files ) {
                                	#Do not print empty lines
                                        if (grep (/ESM/, $file)){
                                                print ("No memory leaks: $file");
                                        }
                                }
               			print "\n\n";
			}
                        if (!$SUMMARY) { # Print only if full output requested
                                print "\n\n";
                                if ($leak_count == 1) {
                                        print "The following example has memory leaks:\n";
                                }
                                else {
                                        print "The following examples have memory leaks:\n\n\n";
                                }
                                # Sort the leak_ex_files
                                @leak_ex_files = sort (@leak_ex_files);
                                foreach $file ( @leak_ex_files ) {
                                        print ("Memory Leaks: $file");
                                }
                                print "\n\n";
                        }
		}
		
		if ($no_leak_count == $ex_count) {
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
			@stdout_ex_files = (); #Clear the file list.
                        # Delete the .stdout from each file
                        foreach ( @st_ex_files) {
                                s/\.stdout//; # Delete stdout
                        }

			# Find the example executable fles that are in the st_ex_files
			foreach $file ( @st_ex_files) {
                                	push @stdout_ex_files, grep (/$file/, @ex_x_files);
                	}
			# Count the number examples in stdout_ex_files
			$ex_count = 0;
			foreach $file ( @stdout_ex_files) {
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
                print "$no_leak_count have no memory leaks and $leak_count have memory leaks.\n\n";

                # Write test results to be read by regression tests scripts.
                $results_file="$EX_DIR/examples_ml_results";
                open(MYHANDLE, ">$results_file");
                print MYHANDLE "PASS $no_leak_count FAIL $leak_count \n";
			
	}
}
1; # This is for the "require" function to work properly.

