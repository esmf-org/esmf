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
#=======================================================================
# name - mklpath
# purpose - print value of MKLPATH to standard output
#
# Notes
# 1. Attempt to get value from MKLPATH environment variable
# 2. If MKLPATH does not exist, attempt to determine value from the
#    FPATH environment variable
# 3. Nothing is printed if MKLPATH cannot be determined
# 4. MKL = Math Kernal Library (Intel Fortran compiler)
#
# !Revision History
# ----------------
# 22Jun2009   Stassi    Initial version of code.
#=======================================================================
use strict;

# main program
#-------------
{
    my ($fpath, @lib, $mklpath);

    # first check to see if MKLPATH environment variable is defined
    #--------------------------------------------------------------
    $mklpath = $ENV{"MKLPATH"};
    if ($mklpath) {
        print "$mklpath";
        exit;
    }

    # next, try to extract MKLPATH from FPATH environment variable
    #-------------------------------------------------------------
    exit unless $ENV{"FPATH"};
    $fpath = $ENV{"FPATH"};
    @lib = split /:/, $fpath;
    foreach (@lib) {
        ($mklpath = $_) =~ s/include/lib\/em64t/;
        last if -d $mklpath;
        $mklpath = "";
    }
    print "$mklpath";
}
