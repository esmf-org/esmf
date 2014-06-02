#!/usr/bin/perl -w
# +-======-+ 
#  Copyright (c) 2003-2007 United States Government as represented by 
#  the Admistrator of the National Aeronautics and Space Administration.  
#  All Rights Reserved.
#  
#  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
#  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
#  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
#  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
#  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
#  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
#  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
#  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
#  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
#  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
#  
#  Government Agency: National Aeronautics and Space Administration
#  Government Agency Original Software Designation: GSC-15354-1
#  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
#  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
#  Government Agency Point of Contact for Original Software:  
#  			Dale Hithon, SRA Assistant, (301) 286-2691
#  
# +-======-+ 
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
