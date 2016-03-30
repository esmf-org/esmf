#!/usr/bin/env perl
#=======================================================================
# name - OScheck.pl
# purpose - Routines to operate on a system file which gives information
#           about the Operating System; See usage() for details.
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($cmpdir, $copydir, $display, $sysfname, $version, $quiet);
my ($fname, $sysfile);

# main program
#-------------
{
    init();

    # note: none of these subroutine calls return
    #--------------------------------------------
    if ($display)  { displayfile()   }
    if ($cmpdir)   { comparefiles()  }
    if ($copydir)  { copyfile()      }
    if ($sysfname) { printsysfname() }
    if ($version)  { printversion()  }
}

#=======================================================================
# name - init
# purpose - get runtime parameters and check for existence of system OS file
#=======================================================================
sub init {
    use Getopt::Long;
    my $help;

    # runtime parameters
    #-------------------
    GetOptions( "cmp=s"   => \$cmpdir,
                "cp=s"    => \$copydir,
                "display" => \$display,
                "sysfile" => \$sysfname,
                "v"       => \$version,
                "q"       => \$quiet, 
                "h|help"  => \$help );
    usage() if $help;
    $display = 1 unless $cmpdir or $copydir or $sysfname or $version;

    # global variables
    #-----------------
    $fname = "SuSE-release";
    $sysfile = "/etc/$fname";
}

#=======================================================================
# name - displayfile
# purpose - print file to STDOUT
#
# input arguments
# => $file: (optional) name of file to print; defaults to $sysfile
#=======================================================================
sub displayfile {
    my ($file, $noExit, $border);

    $file = shift @_;

    if ($file) { $noExit = 1      }
    else       { $file = $sysfile }

    checkx($file);

    $border = "-"x (length($file) + 6);
    print "\nFile: $file\n$border\n";
    open OSFILE, "< $file" or die "Error opening file: $file;";
    foreach (<OSFILE>) { print $_ }
    close OSFILE;
    exit unless $noExit;
}

#=======================================================================
# name - comparefiles
# purpose - compare system OS file to version in $cmpdir directory
#=======================================================================
sub comparefiles {
    my ($myfile, $waitstatus, $status);

    $myfile = "$cmpdir/$fname";

    checkx($sysfile);
    checkx($myfile);

    $waitstatus = system "diff -bwi $sysfile $myfile >& /dev/null";
    $status = $waitstatus >> 8;

    if ($status) {
        print "\nWarning: Files $sysfile and $myfile differ\n";
        displayfile($sysfile);
        displayfile($myfile);
    }
    else {
        print "Files $sysfile and $myfile are equivalent.\n";
    }
    exit $status;
}

#=======================================================================
# name - copyfile
# purpose - copy system OS file to $copydir
#=======================================================================
sub copyfile {
    use File::Copy ("copy");
    my $myfile;

    checkx($sysfile);
    checkx($copydir, "d");

    $myfile = "$copydir/$fname";
    if (-f $myfile) {
        warn "$myfile already exists and will not be overwritten.\n";
        exit;
    }
    copy $sysfile, $myfile or die "Error. cp $sysfile $myfile;";
    print "$sysfile copied to $myfile\n";
    exit;
}

#=======================================================================
# name - printsysfname
# purpose - print full pathname of system OS file
#=======================================================================
sub printsysfname {
    print "$sysfile\n";
    exit;
}

#=======================================================================
# name - printversion
# purpose - print version number from system OS file
#=======================================================================
sub printversion {
    my $ver;

    if (check($sysfile)) {
        open OSFILE, "< $sysfile" or die "Error opening file: $sysfile;";
        foreach (<OSFILE>) { $ver = $1 if /VERSION\s*=\s*(\S*)/i }
        close OSFILE;
        warn "VERSION not found in $sysfile;" unless $ver;
    }
    if ($ver) { print "$ver\n" }
    else      { print "0\n"    }
    exit;
}

#=======================================================================
# name - check
# purpose - check for existence of file or directory
#
# input arguments
# => $file: name of file or directory to check
# => $type: (optional) ="f" to check existence of files (default)
#                      ="d" to check existence of directory
# return value
# = 1 if found
# = 0 if not found
#=======================================================================
sub check {
    my ($name, $type, $tname);

    $name = shift @_;
    $type = shift @_;
    $type = "f" unless $type and $type eq "d";

    if ($type eq "f") {
        unless (-f $name) {
            warn "Warning. file not found: $name;" unless $quiet;
            return;
        }    
    }

    elsif ($type eq "d") {
        unless (-d $name) {
            warn "Warning. directory not found: $name;";
            return;
        }    
    }

    return 1;
}

#=======================================================================
# name - checkx
# purpose - wrapper for check subroutine; exit if file not found
#=======================================================================
sub checkx {
    my $status = check(@_);
    exit unless $status;
}

#=======================================================================
# name - usage
# purpose - print usage information to standard output
#=======================================================================
sub usage {
    use File::Basename;
    my $name = basename($0);

    print << "EOF";

usage: $name [option]

  options:
    -cmp dir   compare system OS file to one in the build\'s etc directory
    -cp dir    copy system OS file to dir
    -display   print full pathname and contents of system OS file to STDOUT
    -sysfile   print full pathname of system OS file
    -v         print VERSION info from system OS file to STDOUT
    -q         quiet mode; do not print warning if system OS file not found
    -h         print usage information

  Notes:
  1. The -display option is the default option if no other option is given.
  2. The -cmp option ignores differences in case and white space.

EOF
exit;
}
