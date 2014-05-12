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

!  $Id: MAPL_SatVapor.F90,v 1.8 2009-04-23 13:34:00 f4mjs Exp $


module MAPL_SatVaporMod


#ifdef MAPL
  use MAPL_ConstantsMod
#endif

!BOP

! !MODULE: MAPL_SatVaporMod -- A module for saturation humidity calculations

! !DESCRIPTION:
!
!  This module provides a function that returns
!  the  saturation specific humidity $q_s$, mixing ratio, $r_s$,
!  or vapor pressure $e_s$, over either liquid water or ice.
!  The function can also return the derivatives $\frac{d q_s}{dT}$,
!  $\frac{d r_s}{dT}$, or $\frac{d e_s}{dT}$ through an optional argument.
!  The module does not depend on ESMF and its only dependence
!  on the rest of MAPL is for the definition
!  of gas constants. If the preprocessor macro MAPL is not defined,
!  these are assigned standard values and the build becomes
!  independent of the rest of MAPL.
!  \newline
!

! !USES:
!
!  use MAPL_ConstantsMod
!
  implicit none
  private
!
! !PUBLIC MEMBER FUNCTIONS:
!
! 
    public MAPL_EQsatSET ! A subroutine to set parameters that control 
                         ! the behavior of MAPL\_EQsat
!
    public MAPL_EQsat    ! The working function.
!
! !PUBLIC DATA MEMBERS:
!
! Enumeration values for the saturation vapor pressure formulation to be used.

    public MAPL_UseStarrQsat     
    public MAPL_UseGoffGratchQsat       
    public MAPL_UseMurphyKoopQsat

! !FILES USED:
!
!   The main computations are done in the following include files:
!
!      eqsat.H esatlqu.H esatice.H qsatlqu.H qsatice.H. 
!
! !BUGS:

!   The tables and some control parameters are globals.
!   This can result in unsafe race conditions when called from
!   multiple threads. Most of these, however, will be benign.
!
!EOP

  interface MAPL_EQsat
     module procedure QSAT0
     module procedure QSAT1
     module procedure QSAT2
     module procedure QSAT3
     module procedure QSATd0
     module procedure QSATd1
     module procedure QSATd2
     module procedure QSATd3
  end interface

#ifndef MAPL
  real*8,    parameter :: ESFAC      = 0.622
  real*8,    parameter :: ZEROC      = 273.16  ! K
#else
  real*8,    parameter :: ESFAC      = MAPL_H2OMW/MAPL_AIRMW
  real*8,    parameter :: ZEROC      = MAPL_TICE
#endif

! Physical parameters

  real*8,    parameter :: MINPFAC    = 2.0
  real*8,    parameter :: MAX_RS     = 1.0/(MINPFAC-1.0)  
  real*8,    parameter :: MAX_QS     = MAX_RS/(1.0+MAX_RS)

! Table parameters

  real*8,    parameter :: TMINTBL    =  150.0       ! lower T bound of tables
  real*8,    parameter :: TMAXTBL    =  333.0       ! upper T bound of tables

! Some limits

  real*8,    parameter :: TMINICE    =  ZEROC - 95.
  real*8,    parameter :: TMAXICE    =  ZEROC      
  real*8,    parameter :: TMINLQU    =  ZEROC - 40.
  real*8,    parameter :: TMAXLQU    =  TMAXTBL    

! Starr parameters

  real*8,    parameter :: TMINSTR = TMINICE - ZEROC
  real*8,    parameter :: TSTARR1 = -75.
  real*8,    parameter :: TSTARR2 = -65.
  real*8,    parameter :: TSTARR3 = -50.
  real*8,    parameter :: TSTARR4 = -40.
  real*8,    parameter :: TMAXSTR = +60.

  real*8,    parameter :: B6 = 6.136820929E-11*100.0
  real*8,    parameter :: B5 = 2.034080948E-8 *100.0
  real*8,    parameter :: B4 = 3.031240396E-6 *100.0
  real*8,    parameter :: B3 = 2.650648471E-4 *100.0
  real*8,    parameter :: B2 = 1.428945805E-2 *100.0
  real*8,    parameter :: B1 = 4.436518521E-1 *100.0
  real*8,    parameter :: B0 = 6.107799961E+0 *100.0
  real*8,    parameter :: BI6= 1.838826904E-10*100.0
  real*8,    parameter :: BI5= 4.838803174E-8 *100.0
  real*8,    parameter :: BI4= 5.824720280E-6 *100.0
  real*8,    parameter :: BI3= 4.176223716E-4 *100.0
  real*8,    parameter :: BI2= 1.886013408E-2 *100.0
  real*8,    parameter :: BI1= 5.034698970E-1 *100.0
  real*8,    parameter :: BI0= 6.109177956E+0 *100.0
  real*8,    parameter :: S16= 0.516000335E-11*100.0
  real*8,    parameter :: S15= 0.276961083E-8 *100.0
  real*8,    parameter :: S14= 0.623439266E-6 *100.0
  real*8,    parameter :: S13= 0.754129933E-4 *100.0
  real*8,    parameter :: S12= 0.517609116E-2 *100.0
  real*8,    parameter :: S11= 0.191372282E+0 *100.0
  real*8,    parameter :: S10= 0.298152339E+1 *100.0
  real*8,    parameter :: S26= 0.314296723E-10*100.0
  real*8,    parameter :: S25= 0.132243858E-7 *100.0
  real*8,    parameter :: S24= 0.236279781E-5 *100.0
  real*8,    parameter :: S23= 0.230325039E-3 *100.0
  real*8,    parameter :: S22= 0.129690326E-1 *100.0
  real*8,    parameter :: S21= 0.401390832E+0 *100.0
  real*8,    parameter :: S20= 0.535098336E+1 *100.0

! Goff-Gratch Parameters

  real*8,    parameter :: DL(1:6) = (/-7.902980, 5.02808, -1.3816E-7, 11.344, 8.1328E-3, -3.49149 /)
  real*8,    parameter :: DI(0:3) = (/ 57518.5606E08, 2.01889049, 3.56654, 20.947031        /)
  real*8,    parameter :: LOGPS   = 3.005714898  ! log10(1013.246)
  real*8,    parameter :: TS      = 373.16

! Murphy and Koop Parameters
 
  real*8,    parameter :: CL(0:9) = (/ 54.842763, -6763.22, -4.21000, .000367, &
                                       0.0415, 218.8,  53.878000, -1331.22,    &
                                      -9.44523, 0.014025                      /)
  real*8,    parameter :: CI(0:3) = (/ 9.550426, -5723.265, 3.53068, -.00728332             /)

! Enumeration for formulation type

  integer,   parameter :: Starr      = 1 
  integer,   parameter :: GoffGratch = 2 
  integer,   parameter :: MurphyKoop = 3 

  integer,   parameter :: MAPL_UseStarrQsat      = Starr     
  integer,   parameter :: MAPL_UseGoffGratchQsat = GoffGratch       
  integer,   parameter :: MAPL_UseMurphyKoopQsat = MurphyKoop


! Tables and other Global variables

  integer,   parameter :: DEFAULT_SUBS = 100

  logical,   save      :: UTBL       = .true.
  logical,   save      :: MXRT       = .false.
  integer,   save      :: TYPE       =  1
  logical,   save      :: TableReady = .false.

  integer,   save      :: DEGSUBS    =  DEFAULT_SUBS ! subdivisions per deg K
  integer,   save      :: TABLESIZE  =  nint(TMAXTBL-TMINTBL)*DEFAULT_SUBS + 1
  real*8,    save      :: DELTA_T    =  1.0 / DEFAULT_SUBS

  real*8,    save      :: ESTFRZ
  real*8,    save      :: ESTLQU

  real*8, allocatable, save :: ESTBLE(:)
  real*8, allocatable, save :: ESTBLW(:)

  integer,   parameter :: WATER   = 1
  integer,   parameter :: ICE     = 2

  real*8,    save      :: TMIN(2) = (/ TMINLQU, TMINICE /)
  real*8,    save      :: TMAX(2) = (/ TMAXLQU, TMAXICE /) 

contains

!==============================================

!BOPI

! !IROUTINE: MAPL_EQsatSET -- Sets behavior of MAPL_EQsat.

! !INTERFACE:

  subroutine MAPL_EQsatSET(UseTable,Formulation,Subdivisions,MixingRatio)

! !ARGUMENTS:

    logical, optional, intent(IN) :: UseTable
    integer, optional, intent(IN) :: Formulation
    integer, optional, intent(IN) :: Subdivisions
    logical, optional, intent(IN) :: MixingRatio

! !DESCRIPTION: 
!  MAPL\_EQsatSet can be used to set three parameters that control
!  the behavior of the working routine, MAPL\_EQsat:\newline
!
!  If {\tt \bf UseTable} is true, tabled values of the saturation vapor pressures are used,
!  instead of the ``exact'' calculations.
!  These tables are automatically generated at a 0.01K resolution for whatever
!  vapor pressure formulation is being used. If never set, MAPL\_EQsat will use the tables.
!  If not specified, the table behavior is left unmodified.\newline
!
!  {\tt \bf Formulation} sets the saturation vapor pressure function to use
!  to use in generating tables or for ``exact'' calculations.
!  Three formulations of saturation vapor pressure are supported:
! \begin{itemize} 
!  \item the Starr code, as was used in NSIPP-1 (MAPL\_UseStarrQsat), 
!  \item the Goff-Gratch formulation, as used in CAM (MAPL\_UseGoffGratchQsat), and 
!  \item Murphy and Koop (2005, QJRMS) (MAPL\_UseMurphyKoopQsat).
! \end{itemize}
!  If it has not been set, the formulation is Starr (MAPL\_UseStarrQsat).
!  If not specified, the formulation is left unmodified.\newline
!
! {\tt \bf Subdivisions} sets the number of subdivisions per degree in the saturation
!  vapor pressure tables. If never set, it is 100; if not specified, it is left unmodified.
!  \newline
!
!  {\tt \bf MixingRatio} sets whether MAPL\_EQsat will return saturation mixing ratio (true)
!  or saturation specific humidity (false) when the pressure is present
!  (see MAPL\_EQsat documentation). If never set, it is false; 
!  if not specified, it is left unmodified.\newline
!
!  MAPL\_EQsatSET also initializes the tables. If MAPL\_EQsatSET is
!  not called and tables are required, they will be initialized the first time
!  one of the  MAPL\_EQsat functions is called. The tables are reset with every call
!  to MAPL\_EQsatSET.\newline

!EOPI

! Set the global flags

    if(present(UseTable   )) UTBL = UseTable
    if(present(Formulation)) TYPE = Formulation
    if(present(MixingRatio)) MXRT = MixingRatio

    if(present(SubDivisions)) then
       DEGSUBS    =  SubDivisions
       TABLESIZE  =  nint(TMAXTBL-TMINTBL)*DEGSUBS + 1
       DELTA_T    =  1.0 / DEGSUBS
    endif

    if(TYPE/=Starr .and. TYPE/=GOFFGRATCH .and. TYPE/=MurphyKoop) then
       print *, 'Bad argument to MAPL_EQsatSET: FORMULATION=',TYPE
       print *, 'Must be one of: ', Starr, GOFFGRATCH, MurphyKoop
       stop 999
    end if

! Set the formulation dependent limits
    
    if(TYPE==MurphyKoop)  then
       TMIN(ICE  ) =  max(TMINTBL,110._8)
       TMIN(WATER) =  max(TMINTBL,123._8)
    else
       TMIN(WATER) =  TMINLQU
       TMIN(ICE  ) =  TMINICE
    endif

! Initialize or reset the tables, even if not needed.

    if(allocated(ESTBLE)) deallocate(ESTBLE)
    if(allocated(ESTBLW)) deallocate(ESTBLW)

    allocate(ESTBLE(TABLESIZE))
    allocate(ESTBLW(TABLESIZE))

    call ESINIT

    return

  contains
!=======================================================================================

    subroutine ESINIT

! Saturation vapor pressure table initialization. This is invoked if UTBL is true 
! on the first call to any qsat routine or whenever MAPL_QsatSet is called 
! N.B.--Tables are in Pa
 
      integer :: I
      real*8  :: T
      logical :: UT

! Save the value of UTBL and temporarily set it to false to get the exact
! formulation.

      UT   = UTBL
      UTBL =.false.

! The size of the table and its resolution are currently hardwired.

      do I=1,TABLESIZE
       
         T = (I-1)*DELTA_T + TMINTBL

! The two standard tables. ESTBLW contains saturation vapor pressures over liquid
! for all temperatures. ESTBLE contains saturation vapor pressures over ice for 
! temperatures below 0C and over water above 0C.
       
         ESTBLW(I) = QSATD0(T,OverIce=.false.)
         ESTBLE(I) = QSATD0(T,OverIce=.true. )

      end do

! Reset UTBL to what it was on entry.

      UTBL = UT

! Mark table as initialized

      TableReady = .true.
    
    end subroutine ESINIT

  end subroutine MAPL_EQsatSET

!=========================================================================

!BOPI

! !IROUTINE: MAPL_EQsat - Computes saturation vapor pressure or specific humidity

! !INTERFACE:

!    function MAPL_EQsat(TL,PL,DQ,OverIce) result(QS)
!
! !ARGUMENTS:
!
!      real,               intent(IN)  :: TL      ! Temperature in Kelvins.
!      real,     optional, intent(IN)  :: PL      ! Air pressure in Pascals.
!      real,     optional, intent(OUT) :: DQ      ! Derivative of result wrt TL.
!      logical,  optional, intent(IN)  :: OverIce ! If set, result is over ice;
                                                  ! otherwise it is over liquid
!      real                            :: QS      ! Result is in Pascals for pressure
                                                  ! otherwise, nondimensional.
!
!    Overloads:
!
!      TL, PL, QL, and QS must all be of the same kind and shape.
!      Overloads exist for kinds 4 and 8 and for ranks 0, 1, 2, and 3.
!
!            
! !DESCRIPTION:  MAPL\_EQsat uses various formulations of the saturation
!                vapor pressure to compute the saturated specific 
!    humidity and, optionally, its derivative with respect to temperature
!    for temperature TL and pressure PL. If PL is not present
!    it returns the saturation vapor pressure and, optionally, 
!    its derivative with respect to temperature.
!    If MixingRation has been set using MAPL\_EQsatSet and PL is present, it returns saturated
!    mixing ratio instead of saturated specific humidity.
! \newline

!    All pressures are in Pascals and all temperatures in Kelvins.
! \newline

!    The choice of saturation vapor pressure formulation is set with MAPL\_EQsatSET.
!    See MAPL\_EQsatSET for a full explanation.
! \newline

!    Another choice is whether to use the exact formulation
!    or a table look-up. This can also be controlled with MAPL\_EQsatSet.
!    The default is to do a table look-up.
! \newline

!    The logical argument OverIce determines whether the saturation vapor pressure
!    is computed over liquid or frozen water. If T is above 273.16K, OverIce is ignored
!    the value returned is always over liquid.
!    All three formulations are valid up to 333K. 
!    Murphy and Koop is valid down to 150K, for both liquid and ice.
!    The other two are valid down to 178K for ice and 233K for super-cooled liquid. 
!    Outside these ranges, the nearest valid value is used for vapor pressure, 
!    and the derivatives with respect to temperature are set to zero.
! \newline

!    Once the saturation vapor pressure is obtained, the saturation specific humidity
!    is computed from:
!$$
!   q_s(T,p) = \frac{M_v}{M_d}  \frac{e_s(T)}{p + (\frac{M_v}{M_d}-1) e_s(T)}
!$$
!    and its derivative from:
!$$
!   \frac{d q_s(T,p)}{dT} = \frac{M_v}{M_d}  \frac{d e_s(T)}{dT}
!                           \frac{p}{(p + (\frac{M_v}{M_d}-1) e_s(T))^2}
!$$
!   If saturation mixing ratios are called for, they are computed from:
!$$
!   r_s(T,p) = \frac{M_v}{M_d}  \frac{e_s(T)}{p-e_s(T)} = \frac{q_s}{1-q_s}
!$$
!    and its derivative from:
!$$
!   \frac{d r_s(T,p)}{dT} = \frac{M_v}{M_d}  \frac{d e_s(T)}{dT}
!                           \frac{p}{(p -  e_s(T))^2}
!$$
!   The ratio of the molecular weights of vapor and dry air is
!   $\frac{M_v}{M_d} = \frac{18.01}{28.97} \approx 0.622$.
! \newline

!   At low pressures ($p < 2 e_s(T)$), the saturation mixing ratio is capped at 1 and the
!   saturation specific humidity at $\frac{1}{2}$. In either case the derivative is set to zero.
! \newline
!
!EOPI
  
#define TX TL
#define PX PL
#define EX QS
#define DX DQ
#define KIND_ 4
  recursive function QSAT0(TL,PL,DQ,OverIce) result(QS)
    real*4,              intent(IN) :: TL
    real*4, optional,    intent(IN) :: PL
    real*4, optional,    intent(OUT):: DQ
    logical,optional,    intent(IN) :: OverIce
    real*4                          :: QS


    real*4    :: TI,W
    real*4    :: DD, TT, EF
    real*4    :: DDQ
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    if(OverLqu) then
#include "qsatlqu.H"
    else
#include "qsatice.H"
    end if

    return
  end function QSAT0

#undef  KIND_
#define KIND_ 8

  recursive function QSATD0(TL,PL,DQ,OverIce) result(QS)
    real*8,              intent(IN) :: TL
    real*8, optional,    intent(IN) :: PL
    real*8, optional,    intent(OUT):: DQ
    logical,optional,    intent(IN) :: OverIce
    real*8                          :: QS


    real*8    :: TI,W
    real*8    :: DD, TT, EF
    real*8    :: DDQ
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    if(OverLqu) then
#include "qsatlqu.H"
    else
#include "qsatice.H"
    end if

    return
  end function QSATD0

#undef  DX
#undef  TX
#undef  EX
#undef  PX

#define TX TL(I)
#define PX PL(I)
#define EX QS(I)
#define DX DQ(I)

#undef  KIND_
#define KIND_ 4

   function QSAT1(TL,PL,DQ,OverIce) result(QS)
    real*4,              intent(IN) :: TL(:)
    real*4, optional,    intent(IN) :: PL(:)
    real*4, optional,    intent(OUT):: DQ(:)
    logical,optional,    intent(IN) :: OverIce
    real*4                          :: QS(SIZE(TL,1))

    integer   :: I
    real*4    :: TI,W  
    real*4    :: DD, TT, EF
    real*4    :: DDQ
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do I=1,size(TL,1)
       if(OverLqu) then
#include "qsatlqu.H"
       else
#include "qsatice.H"
       end if
    end do

    return
  end function QSAT1

#undef  KIND_
#define KIND_ 8

   function QSATD1(TL,PL,DQ, OverIce) result(QS)
    real*8,              intent(IN) :: TL(:)
    real*8, optional,    intent(IN) :: PL(:)
    real*8, optional,    intent(OUT):: DQ(:)
    logical,optional,    intent(IN) :: OverIce
    real*8                          :: QS(SIZE(TL,1))

    integer   :: I
    real*8    :: TI,W  
    real*8    :: DDQ
    real*8    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do I=1,size(TL,1)
       if(OverLqu) then
#include "qsatlqu.H"
       else
#include "qsatice.H"
       end if
    end do

    return
  end function QSATD1

#undef  DX
#undef  TX
#undef  PX
#undef  EX

#define TX TL(I,J)
#define PX PL(I,J)
#define EX QS(I,J)
#define DX DQ(I,J)

#undef  KIND_
#define KIND_ 4

   function QSAT2(TL,PL,DQ,OverIce) result(QS)
    real*4,              intent(IN) :: TL(:,:)
    real*4, optional,    intent(IN) :: PL(:,:)
    real*4, optional,    intent(OUT):: DQ(:,:)
    logical,optional,    intent(IN) :: OverIce
    real*4    :: QS(SIZE(TL,1),SIZE(TL,2))

    integer   :: I, J
    real*4    :: TI,W  
    real*4    :: DDQ
    real*4    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do J=1,size(TL,2)
       do I=1,size(TL,1)
          if(OverLqu) then
#include "qsatlqu.H"
          else
#include "qsatice.H"
          end if
       end do
    end do

    return
  end function QSAT2

#undef  KIND_
#define KIND_ 8

   function QSATD2(TL,PL,DQ,OverIce) result(QS)
    real*8,              intent(IN) :: TL(:,:)
    real*8, optional,    intent(IN) :: PL(:,:)
    real*8, optional,    intent(OUT):: DQ(:,:)
    logical,optional,    intent(IN) :: OverIce
    real*8    :: QS(SIZE(TL,1),SIZE(TL,2))

    integer   :: I, J
    real*8    :: TI,W  
    real*8    :: DDQ
    real*8    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do J=1,size(TL,2)
       do I=1,size(TL,1)
          if(OverLqu) then
#include "qsatlqu.H"
          else
#include "qsatice.H"
          end if
       end do
    end do

    return
  end function QSATD2

#undef  DX
#undef  TX
#undef  PX
#undef  EX

#define TX TL(I,J,K)
#define PX PL(I,J,K)
#define EX QS(I,J,K)
#define DX DQ(I,J,K)

#undef  KIND_
#define KIND_ 4

   function QSAT3(TL,PL,DQ, OverIce) result(QS)
    real*4,              intent(IN) :: TL(:,:,:)
    real*4, optional,    intent(IN) :: PL(:,:,:)
    real*4, optional,    intent(OUT):: DQ(:,:,:)
    logical,optional,    intent(IN) :: OverIce
    real*4    :: QS(SIZE(TL,1),SIZE(TL,2),SIZE(TL,3))

    integer   :: I, J, K
    real*4    :: TI,W  
    real*4    :: DDQ
    real*4    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do K=1,size(TL,3)
       do J=1,size(TL,2)
          do I=1,size(TL,1)
             if(OverLqu) then
#include "qsatlqu.H"
             else
#include "qsatice.H"
             end if
          end do
       end do
    end do

    return
  end function QSAT3

#undef  KIND_
#define KIND_ 8

   function QSATD3(TL,PL,DQ, OverIce) result(QS)
    real*8,              intent(IN) :: TL(:,:,:)
    real*8, optional,    intent(IN) :: PL(:,:,:)
    real*8, optional,    intent(OUT):: DQ(:,:,:)
    logical,optional,    intent(IN) :: OverIce
    real*8    :: QS(SIZE(TL,1),SIZE(TL,2),SIZE(TL,3))

    integer   :: I, J, K
    real*8    :: TI,W  
    real*8    :: DDQ
    real*8    :: DD, TT, EF
    integer   :: IT
    logical   :: OverLqu

    OverLqu = .true.
    if(present(OverIce)) OverLqu=.not.OverIce

    do K=1,size(TL,3)
       do J=1,size(TL,2)
          do I=1,size(TL,1)
             if(OverLqu) then
#include "qsatlqu.H"
             else
#include "qsatice.H"
             end if
          end do
       end do
    end do

    return
  end function QSATD3

#undef  DX
#undef  TX
#undef  PX
#undef  EX

#undef  KIND_


end module MAPL_SatVaporMod

