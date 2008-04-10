#!/usr/bin/env perl
# $Id: examples_results.pl,v 1.3.2.2 2008/04/10 15:26:38 svasquez Exp $
# This subroutine is called at the end of the examples, "check_examples" and "check_results" targets.
# The purpose is to give the user the results of running the examples.
# The results are either complete results or a summary.

sub examples_results($$$) {

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
@pass_ex_files = ();	# passed examples files
@file_lines = ();	# file lines
@ex_x_files = ();	# examples executable files

	#Find all files
	find(\&allFiles, '.'); 
	sub allFiles {
			# Put all files in a list
	 		push all_files, "$File::Find::name\n" if -T ; 
	}	
	# Get all example files
	@ex_files=grep (/Ex/, @all_files);
	# Find the example files with the "ESMF_EXAMPLE" string
	# grep for "ESMF_EXAMPLE"
	$count=0;
	$ex_count=0;
	foreach $file ( @ex_files) {
		open(F,$file);
		foreach $line (<F>){
			push(file_lines, $line);
			}
			close ($file);
			$count=grep ( /ESMF_EXAMPLE/, @file_lines);
			if ($count != 0) {
				push (act_ex_files, $file);
                                        $ex_count=$ex_count + 1;
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
			 	push all_files, "$File::Find::name\n"  if -e;
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

		# Count the number of PASS and FAIL
		# push pass examples tests to a list.
		$count=0;
		$pass_count=0;
		$fail_count=0;
		foreach $file ( @stdout_ex_files) {
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
		$fail_count = $ex_count - $pass_count;
		$example_count = $ex_count;
                if ($pass_count != 0) {
                        #Strip the names of failed examples
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
                        	print @pass_ex_files;
                        	print "\n\n";
			}
                }
		if ($fail_count != 0) {
                	# Find the act_ex_files fles that are in the pass_tests
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
				print @act_ex_files;
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
                                	push all_files, "$File::Find::name\n"  if -x;
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
		if ($ex_count == 1) {
			print "Found $example_count example, $pass_count passed and $fail_count failed.\n\n";
		}
		else {
			print "Found $example_count examples, $pass_count passed and $fail_count failed.\n\n";
		}
		

		# Write test results to be read by regression tests scripts.
		$results_file="$EX_DIR/examples_results";
		open(MYHANDLE, ">$results_file");
		print MYHANDLE "PASS $pass_count FAIL $fail_count \n";
			
	}
}
1; # This is for the "require" function to work properly.

