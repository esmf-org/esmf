#!/usr/bin/perl -w
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
