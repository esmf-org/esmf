#! /usr/bin/env perl -w
#
# $Id: test_for_docs.pl,v 1.8 2006/11/17 16:59:13 cdeluca Exp $
#
# test_for_docs.pl
# 
# This script simple tests for the existence of
# of a list of ESMF docs.  Right now it will
# check for the docs create by the EMSF 
# and impl_rep releases.
#
#
# Options:  
#
# -h   Print out help message
#
# -p   Print out a list of the docs 
#      that will be searched for.
#
# -s Print out summary or the results.  Summary 
#    will be printed out before PASS/FAIL results.
#
# -e <ESMF_DIR>
#
# -i <ESMF_IMP_DIR>
#
#

############################################################
#
#  Main program.
#
############################################################

use Getopt::Std;

#
# Prototypes
#
sub GetDocStatus($$$);
sub GetNewDocs($);



#
#  List of ESMF docs to look for.
#  Specify just the root part of the
#  file name, not the extension.  
#  
@esmf_docs     = ("ESMF_refdoc",
		  "ESMF_usrdoc");

#
#  List of impl_rep docs to look for.
#  Specify just the root part of the
#  file name, not the extension.  
#
@impl_rep_docs = ("IMPL_repdoc",
		  "IMPL_usrdoc");


#
#  Read the command line arguments.
#
%options = ();
getopts("hpse:i:",\%options);

#
# Print out help message if -h argument is used.
# Also print out this message if no arguments
# are used.
#
$numberOfArgs = keys %options;

if ($options{h} || ($numberOfArgs == 0)) {
    # print out help message
    print "\n";
    print "test_for_docs.pl  \n\n";

    print "This script tests for the existence of \n";
    print "ESMF and impl_rep docs. \n\n";

    print "-h          This help message \n\n";

    print "-p          Print the list of docs that\n";
    print "            this script will look for.\n\n";

    print "-s          Print out a summary of the results.\n";
    print "            Summary will be printed out before PASS/FAIL\n";
    print "            results are printed.\n\n";

    print "-e <DIR>    Look for ESMF docs.  DIR should be \n";
    print "            the value of ESMF_DIR. \n\n";

    print "-i <DIR>    Look for impl_rep docs.  DIR should be\n";
    print "            the value of ESMF_IMPL_DIR. \n\n";

    exit 0;
}

#
# Print out a list of documents that will be
# searched for.
#
if ($options{p}) {

    print "This script will look for these docs.\n\n";
    
    print "In ESMF: \n";
    foreach $doc (@esmf_docs) {
	print "  $doc\n";
    }
    print "\n";
    print "In impl_rep: \n";
    foreach $doc (@impl_rep_docs) {
	print "  $doc\n";
    }
    print "\n";
    
}

#
#  Test for ESMF docs
#
if ($options{e}) {

    $esmf_docs_num  = 0;
    $esmf_docs_pass = 0;
    $esmf_docs_fail = 0;

    # Check for existence of impl_rep dir.
    if (! -d $options{e}){
	print "Error: ESMF dir does not exist: $options{i} \n";
    }else{
	# Check for existence of impl_rep/doc dir.
	$doc_dir = "$options{e}/doc";
	if (! -d $doc_dir) { 
	    print "Error: ESMF doc dir does not exist: $doc_dir \n";
	}else{
	    # Check for all docs listed in the impl_rep_docs array.
	    foreach $doc (@esmf_docs){
		@esmf_result = GetDocStatus("$doc_dir","ESMF",$doc);
		push @esmf_result_list, $esmf_result[0];
		$esmf_docs_num++;
		if($esmf_result[1]){
		    $esmf_docs_pass++ ;
		}else{
		    $esmf_docs_fail++ ;
		}
	    }
	}
    }
}

#
#  Test for impl_rep docs
#
if ($options{i}) {

    $impl_rep_docs_num  = 0;
    $impl_rep_docs_pass = 0;
    $impl_rep_docs_fail = 0;
    
    # Check for existence of impl_rep dir.
    if (! -d $options{i}) {
	print "Error: impl_rep dir does not exist: $options{i} \n";
    }else{
	# Check for existence of impl_rep/doc dir.
	$doc_dir = "$options{i}/doc";
	if(! -d $doc_dir ) {
	    print "Error: impl_rep doc dir does not exist: $doc_dir \n";
	}else{

	    # Check for all docs listed in the impl_rep_docs array.
	    foreach $doc (@impl_rep_docs){
		@impl_rep_result = GetDocStatus("$doc_dir","impl_rep",$doc);
		push @impl_rep_result_list, $impl_rep_result[0];
		$impl_rep_docs_num++;
		if($impl_rep_result[1]){
		    $impl_rep_docs_pass++ ;
		}else{
		    $impl_rep_docs_fail++ ;
		}
	    }
	}
    }
}

#
# print out summary
#
if($options{s}) {

    print "\n\n";

    print "test_for_docs runtime: ",scalar localtime, "\n\n";


    if ($options{e}) {
	print "ESMF doc build results: \n";
	print "   Number of docs  : $esmf_docs_num \n";
	print "   Number built    : $esmf_docs_pass \n";
	print "   Number failed   : $esmf_docs_fail \n";
    }

    if ($options{i}) {

	print "\n";
	print "impl_rep doc build results: \n";
	print "   Number of docs  : $impl_rep_docs_num \n";
	print "   Number built    : $impl_rep_docs_pass \n";
	print "   Number failed   : $impl_rep_docs_fail \n";
    }

    print "\n"
}

#
# print out results
# 
if ($options{e}) {
    foreach $result_line (@esmf_result_list){
	print $result_line;
    }
}

if ($options{i}) {
    foreach $result_line (@impl_rep_result_list){
	print $result_line;
    }
}



# Leave the main program.  Return code 0;
exit 0;


############################################################
#
#  Subroutine GetDocStatus
#
############################################################

# Subroutine to check for existence of .pdf  and html versions
# of a doc.  Input parameters are three strings:
#     
#    $_[0] = path to docs 
#    $_[1] = A name for the source code tree (ESMF, impl_rep)
#    $_[2] = The root name of the doc.  (ex: ESMF_usrdoc )
#
# Returns two strings in an array:
#
#    result_strings[0] = string containing PASS or FAIL and a 
#    message. 
#
#    result_strings[1] = PASS/FAIL value. FAIL = 0, PASS = 1;
#

sub GetDocStatus($$$) {
    my $dir      = $_[0];
    my $source   = $_[1];  
    my $root_doc = $_[2];
    
    my $pdf_exist        = 0;  # False for now
    my $pdf_zero_length  = 0;  # False for now
    my $html_exist       = 0;  # False for now
    my $html_zero_length = 0;  # False for now

    my $pdf_file  = "$dir/$root_doc\.pdf";
    my $html_file = "$dir/$root_doc/index.html";

    my @result_strings = ("Not set", 0);

    if( -e $pdf_file ) {   # pdf exists ?
	$pdf_exist = 1;
	if ( -z $pdf_file ) {   # pdf file zero length ?
	    $pdf_zero_length = 1;
	}
    }

    if( -e $html_file ) {   # html exists ?
	$html_exist = 1;
	if ( -z $html_file ) {   # html file zero length ?
	    $html_zero_length = 1;
	}
    }

    if(($pdf_exist) && (!$pdf_zero_length)){
	# PDF File valid

	if(($html_exist) && (!$html_zero_length)){  	 
            #HTML index file valid
	    $result_strings[0] = "PASS ($source) $root_doc\n";
	    $result_strings[1] = 1;       
	}elsif(($html_exist) && ($html_zero_length)) {   
	    #HTML index file has zero length
	    $result_strings[0] = "FAIL ($source) $root_doc has pdf file, but zero length html index file. \n";
	}else{
	    #HTML index file does not exist.
	    $result_strings[0] = "FAIL ($source) $root_doc has pdf file, but no html index file. \n";
	}

    }elsif(($pdf_exist) && ($pdf_zero_length)){
	# PDF File is zero length

	if(($html_exist) && (!$html_zero_length)){  	 
            #HTML index file valid
	    $result_strings[0] = "FAIL ($source) $root_doc has zero length pdf file, but has an html index file \n";
	}elsif(($html_exist) && ($html_zero_length)) {   
	    #HTML index file has zero length
	    $result_strings[0] = "FAIL ($source) $root_doc has zero length pdf file and zero length html index file.\n";
	}else{
	    #HTML index file does not exist.
	    $result_strings[0] = "FAIL ($source) $root_doc has zero length pdf file and no html index file.\n";
	}

    }else{
	# PDF file does not exist.

	if(($html_exist) && (!$html_zero_length)){  	 
            #HTML index file valid
	    $result_strings[0] = "FAIL ($source) $root_doc has no pdf file, but has an html index file \n";
	}elsif(($html_exist) && ($html_zero_length)) {   
	    #HTML index file has zero length
	    $result_strings[0] = "FAIL ($source) $root_doc has no pdf file and has a zero length html index file.\n";
	}else{
	    #HTML index file does not exist.
	    $result_strings[0] = "FAIL ($source) $root_doc has no pdf file and no html index file.\n";
	}
    }

    return @result_strings;
}


