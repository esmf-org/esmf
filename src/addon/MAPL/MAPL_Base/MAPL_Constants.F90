! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
module MAPL_ConstantsMod

!  $Id: MAPL_Constants.F90,v 1.6 2014-01-29 20:51:36 ltakacs Exp $

implicit none
private


!=============================================================================
!BOP

! !MODULE: -- A container module for global constants

! !PUBLIC VARIABLES:

real(kind=8), parameter, public :: MAPL_PI_R8     = 3.14159265358979323846
real, parameter, public :: MAPL_PI     = MAPL_PI_R8
real, parameter, public :: MAPL_GRAV   = 9.80665                ! m^2/s
real, parameter, public :: MAPL_RADIUS = 6371.0E3               ! m
real, parameter, public :: MAPL_OMEGA  = 2.0*MAPL_PI/86164.0    ! 1/s
real, parameter, public :: MAPL_STFBOL = 5.6734E-8              ! W/(m^2 K^4)
real, parameter, public :: MAPL_AIRMW  = 28.965                 ! kg/Kmole
real, parameter, public :: MAPL_H2OMW  = 18.015                 ! kg/Kmole
real, parameter, public :: MAPL_O3MW   = 47.9982                ! kg/Kmole
real, parameter, public :: MAPL_RUNIV  = 8314.47                ! J/(Kmole K)
real, parameter, public :: MAPL_ALHL   = 2.4665E6               ! J/kg @15C
real, parameter, public :: MAPL_ALHF   = 3.3370E5               ! J/kg
real, parameter, public :: MAPL_ALHS   = MAPL_ALHL+MAPL_ALHF    ! J/kg

real, parameter, public :: MAPL_RDRY   = MAPL_RUNIV/MAPL_AIRMW  ! J/(kg K)
real, parameter, public :: MAPL_CPDRY  = 3.5*MAPL_RDRY          ! J/(kg K)
real, parameter, public :: MAPL_CVDRY  = MAPL_CPDRY-MAPL_RDRY   ! J/(kg K)

real, parameter, public :: MAPL_RVAP   = MAPL_RUNIV/MAPL_H2OMW  ! J/(kg K)
real, parameter, public :: MAPL_CPVAP  = 4.*MAPL_RVAP           ! J/(kg K)
real, parameter, public :: MAPL_CVVAP  = MAPL_CPVAP-MAPL_RVAP   ! J/(kg K)

real, parameter, public :: MAPL_KAPPA  = MAPL_RDRY/MAPL_CPDRY   ! (2.0/7.0)


real, parameter, public :: MAPL_EPSILON= MAPL_H2OMW/MAPL_AIRMW  ! --
real, parameter, public :: MAPL_DELTAP = MAPL_CPVAP/MAPL_CPDRY  ! --
real, parameter, public :: MAPL_DELTAV = MAPL_CVVAP/MAPL_CVDRY  ! --
real, parameter, public :: MAPL_GAMMAD = MAPL_CPDRY/MAPL_CVDRY  ! --

real, parameter, public :: MAPL_RGAS   = MAPL_RDRY              ! J/(kg K) (DEPRECATED)
real, parameter, public :: MAPL_CP     = MAPL_RGAS/MAPL_KAPPA   ! J/(kg K) (DEPRECATED)
real, parameter, public :: MAPL_VIREPS = 1.0/MAPL_EPSILON-1.0   !          (DEPRECATED)

real, parameter, public :: MAPL_P00    = 100000.0               ! Pa
real, parameter, public :: MAPL_CAPICE = 2000.                  ! J/(K kg)
real, parameter, public :: MAPL_CAPWTR = 4218.                  ! J/(K kg)
real, parameter, public :: MAPL_RHOWTR = 1000.                  ! kg/m^3
real, parameter, public :: MAPL_NUAIR  = 1.533E-5               ! m^2/S (@ 18C)
real, parameter, public :: MAPL_TICE   = 273.16                 ! K
real, parameter, public :: MAPL_SRFPRS = 98470                  ! Pa
real, parameter, public :: MAPL_KARMAN = 0.40                   ! --
real, parameter, public :: MAPL_USMIN  = 1.00                   ! m/s
real, parameter, public :: MAPL_AVOGAD = 6.023E26               ! 1/kmol

integer,parameter, public :: MAPL_R8 = selected_real_kind(12) ! 8 byte real
integer,parameter, public :: MAPL_R4 = selected_real_kind( 6) ! 4 byte real
integer,parameter, public :: MAPL_RN = kind(1.0)              ! native real
integer,parameter, public :: MAPL_I8 = selected_int_kind (13) ! 8 byte integer
integer,parameter, public :: MAPL_I4 = selected_int_kind ( 6) ! 4 byte integer
integer,parameter, public :: MAPL_IN = kind(1)                ! native integer


!EOP

end module MAPL_CONSTANTSMOD

