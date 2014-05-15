#!/usr/bin/perl
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
#
# Creates MAPL no-op grid components
#
#----------------------------------------------------------------------------

use Env;                 # make env vars readily available
use Getopt::Std;         # command line options

# Command line options
# --------------------
  getopts('hp:s');
  usage() if ( $opt_h || $#ARGV < 0 );

  $NAME = $ARGV[0];
  $GCPROXY = "MAPL_Mod" unless $GCPROXY = "$opt_p";
  unless ( $SETSERVICES = $opt_s ) {
      if ( "$GCPROXY" eq "MAPL_Mod" ) {
           $SETSERVICES = "MAPL_GenericSetServices" ;
      } else {  
           $SETSERVICES = "SetServices" ; # work for most components
      }
  }

  print <<EOF;
!
! Stub code automatically generated on the fly by geos_stub.pl; 
! do not edit or check in, change GNUmakefile instead.
!
module $NAME
   use ESMF
   use $GCPROXY,  only:  ProxySetServices => $SETSERVICES
   private
   public SetServices
contains
   subroutine SetServices ( GC, RC )
      type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
      integer, optional                  :: RC  ! return code
      call ProxySetServices ( GC, RC=RC )
   end subroutine SetServices 
end module $NAME
EOF

exit 0;


#......................................................................

sub usage {

   print <<"EOF";

NAME
     stub.pl - Creates stub ESMF Grid Component
          
SYNOPSIS

     stub.pl [-hps] module_name
          
DESCRIPTION

     Creates a simple stub ESMF Grid Component which inherits the
     Initialize/Run/Finalize methods from another component, usually
     MAPL Generic. Resulting stub is written to stdout.

OPTIONS
     -p proxy_name  The proxy Grid Component name; default is 
                    "MAPL_Mod"
     -s             SetServices entry point in proxy; default depends on the
                    proxy name: if proxy name is "MAPL_GenericMod" it 
                    defaults to "MAPL_GenericSetServices", otherwise it 
                    defaults to simply "SetServices".

AUTHOR
     Arlindo.daSilva@nasa.gov 

EOF

  exit(1)

 }

