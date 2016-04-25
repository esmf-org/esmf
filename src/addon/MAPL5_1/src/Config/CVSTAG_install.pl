#!/usr/bin/perl -w
########################################################################
#
# name - CVSTAG_install.pl
# purpose - This script takes the tag ID from the CVS/Tag file and
#     writes it to the CVSTAG file in the build's etc directory.
#
# input argument -
#  $1 : location of the build's etc directory
#
# REVISION HISTORY:
#
# 11Aug2006  Stassi  Initial version added to CVS repository
# 12Sep2006  Stassi  Do not overwrite pre-existing CVSTAG file.  This
#                    allows users to manually edit the file without fear
#                    of having their modification overwritten.
#########################################################################
use strict;
use File::Basename;

my $scriptname = basename($0);
my ($esmaetc,$CVSTAG);
my ($TagFile,$Ntag,$tag);

# get ESMA etc directory location
#--------------------------------
if (scalar(@ARGV) < 1) {
    print STDERR "usage: $scriptname \$ESMAETC\n";
    exit 2;
}
$esmaetc = shift @ARGV;

# verify that directory location exists
#--------------------------------------
if (! -d $esmaetc) {
    print STDERR "$scriptname: Error ... $esmaetc is not a directory.\n";
    exit 3;
}

# if output file already exists, then make note and exit gracefully
#------------------------------------------------------------------
$CVSTAG = "$esmaetc/CVSTAG";
if (-e $CVSTAG) { 
    warn "$CVSTAG already exists and will not be overwritten.\n";
    exit 0;
 }

# get tag info from Tag file in CVS directory, if it exists
#----------------------------------------------------------
$tag = "tag-unknown";
$TagFile = "./CVS/Tag";
if (-e $TagFile) {
    open TAG, $TagFile;
    $Ntag = <TAG>;
    if ($Ntag =~ /[NT](.*)/) {$tag = $1};
    close TAG;
}    

# write tag info to CVSTAG file in ESMA etc directory
#----------------------------------------------------
open CVSTAG, "> $CVSTAG";
print CVSTAG "$tag\n";
