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
# name - endiantest.pl
# purpose - This script checks the byte-order of a machine and prints
#   information ("BIG_ENDIAN" or "LITTLE_ENDIAN") to standard output.
#
# !Revision History
# 23Jan2009  Stassi     Initial version of code.
#=======================================================================
use strict;

# global variables
#-----------------
my ($is_big_endian, $is_little_endian);

# main program
#-------------
{
    determine_byte_order();
    print_results();
}

#=======================================================================
# name - determine_byte_order
#=======================================================================
sub determine_byte_order {
    $is_big_endian    = unpack("h*", pack("s", 1)) =~ /01/;
    $is_little_endian = unpack("h*", pack("s", 1)) =~ /^1/;
    return;
}

#=======================================================================
# name - print_results
#=======================================================================
sub print_results {

    # error conditions
    #-----------------
    die "ERROR: Unable to determine byte-order(0).\n"
        unless ($is_big_endian or $is_little_endian);
    die "ERROR: Unable to determine byte-order(1).\n"
        if ($is_big_endian and $is_little_endian);
    
    # print byte-order to stdout
    #---------------------------
    if ($is_big_endian) { print "BIG_ENDIAN\n";    }
    else                { print "LITTLE_ENDIAN\n"; }
    return;
}
