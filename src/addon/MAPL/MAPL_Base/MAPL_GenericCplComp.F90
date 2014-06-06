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
!  $Id: MAPL_GenericCplComp.F90,v 1.11.72.2.2.1 2013-07-08 21:10:19 amolod Exp $

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: MAPL_GenericCplCompMod

! !DESCRIPTION:
!
!  This is a generic coupler component used by \ggn\ to instantiate 
!  the automatic couplers it needs.
!  \newline

! !INTERFACE:

module MAPL_GenericCplCompMod

! !USES:

  use ESMF
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_ConstantsMod
  use MAPL_IOMod
  use MAPL_ProfMod
  use MAPL_SunMod
  use MAPL_VarSpecMod

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public GenericCplSetServices
  public MAPL_CplCompSetVarSpecs

!EOP

  type MAPL_CplCnt
     integer, pointer  :: PTR1C(:)     => null()
     integer, pointer  :: PTR2C(:,:)   => null()
     integer, pointer  :: PTR3C(:,:,:) => null()
  end type MAPL_CplCnt

  type MAPL_GenericCplState
     private
! These are done in set services
     type (ESMF_Config)           :: CF
     logical                      :: ACTIVE
     character(LEN=ESMF_MAXSTR)   :: NAME
     type (MAPL_VarSpec), pointer :: SRC_SPEC(:) => null()
     type (MAPL_VarSpec), pointer :: DST_SPEC(:) => null()
! These are done in init
     integer          , pointer   :: CLEAR_INTERVAL(:)
     integer          , pointer   :: COUPLE_INTERVAL(:)
     type (ESMF_Alarm), pointer   :: TIME_TO_CLEAR(:)
     type (ESMF_Alarm), pointer   :: TIME_TO_COUPLE(:)
     type (ESMF_LocalArray), pointer :: ACCUMULATORS(:)
     type(MAPL_CplCnt), pointer   :: ARRAY_COUNT(:) => null()
     integer          , pointer   :: ACCUM_COUNT(:)
     integer          , pointer   :: ACCUM_RANK(:)
     integer          , pointer   :: couplerType(:) => null()
  end type MAPL_GenericCplState

  type MAPL_GenericCplWrap
     type (MAPL_GenericCplState), pointer :: INTERNAL_STATE
  end type MAPL_GenericCplWrap

contains

!=============================================================================
!=============================================================================
!=============================================================================

!BOPI

! !IROUTINE: GenericCplSetServices

! !DESCRIPTION: \ssv\  for generic couplers.

! !INTERFACE:

  subroutine GenericCplSetServices ( CC, RC )

! !ARGUMENTS:

    type (ESMF_CplComp  )                          :: CC  
    integer,                         intent(  OUT) :: RC
    
!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer :: STATE
    type (MAPL_GenericCplWrap )          :: CCWRAP

! Begin...

! Get this instance's name and set-up traceback handle.
! -----------------------------------------------------

    call ESMF_CplCompGet( CC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "GenericCplSetServices"

! Allocate this instance of the internal state and put it in wrapper.
! -------------------------------------------------------------------

    allocate(STATE, STAT=STATUS)
    VERIFY_(STATUS)

    CCWRAP%INTERNAL_STATE => STATE

! Have ESMF save pointer to the wrapped internal state in the C.C.
! ----------------------------------------------------------------

    call ESMF_CplCompSetInternalState(CC, CCWRAP, STATUS)
    VERIFY_(STATUS)

! Register services for this component
! ------------------------------------

    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_INITIALIZE,  Initialize, &
                                     rc=STATUS )
    VERIFY_(STATUS)

    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_RUN,   Run,        &
                                     rc=STATUS )
    VERIFY_(STATUS)

    call ESMF_CplCompSetEntryPoint ( CC, ESMF_METHOD_FINALIZE, Finalize,   &
                                     rc=STATUS )
    VERIFY_(STATUS)

! Put the inherited configuration in the internal state
! -----------------------------------------------------

!ALT not_used    call ESMF_CplCompGet( CC, CONFIG=STATE%CF, RC=STATUS )
!ALT not_used    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine GenericCplSetServices

  subroutine MAPL_CplCompSetVarSpecs ( CC, SRC_SPEC, DST_SPEC, RC )
    type (ESMF_CplComp  ),           intent(INOUT) :: CC  
    type (MAPL_VarSpec  ), target,   intent(IN   ) :: SRC_SPEC(:)
    type (MAPL_VarSpec  ), target,   intent(IN   ) :: DST_SPEC(:)
    integer, optional,               intent(  OUT) :: RC
    
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer :: STATE
    type (MAPL_GenericCplWrap )          :: WRAP
    character(len=ESMF_MAXSTR)           :: SRC_NAME
    character(len=ESMF_MAXSTR)           :: DST_NAME

    integer                              :: I, NCPL

! Begin...

! Get this instance's name and set-up traceback handle.
! -----------------------------------------------------

    call ESMF_CplCompGet( CC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "MAPL_CplCompSetVarSpecs"

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    VERIFY_(STATUS)

    STATE  =>  WRAP%INTERNAL_STATE

! Make sure the specs match
!--------------------------

    ASSERT_(size(SRC_SPEC)==size(DST_SPEC))

    do I=1,size(SRC_SPEC)
       call MAPL_VarSpecGet(SRC_SPEC(I),SHORT_NAME=SRC_NAME,RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_VarSpecGet(DST_SPEC(I),SHORT_NAME=DST_NAME,RC=STATUS)
       VERIFY_(STATUS)

!ALT       ASSERT_(SRC_NAME==DST_NAME)
    end do

! Put miscellaneous info in the internal state
!---------------------------------------------

    STATE%SRC_SPEC => SRC_SPEC
    STATE%DST_SPEC => DST_SPEC

    STATE%ACTIVE = .true.
    STATE%NAME   = COMP_NAME

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CplCompSetVarSpecs

!=============================================================================

!BOPI

! !IROUTINE: INITIALIZE

! !DESCRIPTION: Initialize method for generic couplers.

! !INTERFACE:

  subroutine Initialize(CC, SRC, DST, CLOCK, RC)

! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    character (len=ESMF_MAXSTR)           :: NAME
    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP
    type (ESMF_TimeInterval   )           :: TCPL
    type (ESMF_TimeInterval   )           :: TCLR
    type (ESMF_TimeInterval   )           :: TS
    type (ESMF_Time           )           :: TM0 
    type (ESMF_Time           )           :: currTime ! current time of the clock
    type (ESMF_Time           )           :: rTime 
    type (ESMF_Calendar       )           :: cal
    integer                               :: J, L1, LN
    integer                               :: NCPLS
    integer                               :: DIMS
    real, pointer                         :: PTR1 (:    )
    real, pointer                         :: PTR2 (:,:  )
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:    )
    real, pointer                         :: PTR20(:,:  )
    real, pointer                         :: PTR30(:,:,:)
    integer                               :: CLEAR_INTERVAL
    integer                               :: COUPLE_INTERVAL
    integer                               :: CLEAR
    integer                               :: COUPLE
    integer, pointer                      :: ungrd(:)
    logical                               :: has_ungrd
    type(ESMF_Field)                      :: field
    integer                               :: minmax

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "MAPL_GenericCplCompInitialize"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    VERIFY_(STATUS)

    STATE  =>  WRAP%INTERNAL_STATE

! The number of couplings for this pair
!--------------------------------------

    NCPLS = size(STATE%DST_SPEC)

    ASSERT_(NCPLS == size(STATE%SRC_SPEC))

! Allocate arrays of ESMF arrays for accumulators
!-------------------------------------------------

    allocate(STATE%ACCUMULATORS(NCPLS), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STATE%ARRAY_COUNT (NCPLS), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STATE%ACCUM_RANK (NCPLS), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STATE%couplerType(NCPLS), STAT=STATUS)
    VERIFY_(STATUS)

! Allocate internal state objects
! -------------------------------

    allocate (STATE%CLEAR_INTERVAL (NCPLS), stat=status)
    VERIFY_(STATUS)
    allocate (STATE%COUPLE_INTERVAL(NCPLS), stat=status)
    VERIFY_(STATUS)
    allocate (STATE%TIME_TO_CLEAR  (NCPLS), stat=status)
    VERIFY_(STATUS)
    allocate (STATE%TIME_TO_COUPLE (NCPLS), stat=status)
    VERIFY_(STATUS)
    allocate(STATE%ACCUM_COUNT     (NCPLS), stat=status)
    VERIFY_(STATUS)

! Get clock info
!---------------

    call ESMF_ClockGet(CLOCK, calendar=cal, currTime=currTime, timeStep=TS, rc=STATUS)
    VERIFY_(STATUS)

    TM0 = currTime

! Initialize the counters to 0. This may do some unnecessary
!   accumulations immediately after initialize
!-----------------------------------------------------------

    STATE%ACCUM_COUNT = 0

    DO J = 1, NCPLS

! Get info from the DST spec
!---------------------------

       call MAPL_VarSpecGet(STATE%DST_SPEC(J),        &
            ACCMLT_INTERVAL = STATE%CLEAR_INTERVAL(J), &
            COUPLE_INTERVAL = STATE%COUPLE_INTERVAL(J), &
            SHORT_NAME      = NAME, &
                                            RC = STATUS )
       VERIFY_(STATUS)

! Initalize COUPLE ALARM from destination properties
!---------------------------------------------------

       call ESMF_TimeIntervalSet(TCPL, S=STATE%COUPLE_INTERVAL(J), &
            calendar=cal, RC=STATUS)
       VERIFY_(STATUS)

       rTime = TM0

       STATE%TIME_TO_COUPLE(J) = ESMF_AlarmCreate(NAME='TIME2COUPLE_' // trim(COMP_NAME) &
            // '_' // trim(NAME),   &
            clock        = CLOCK,   &
            ringInterval = TCPL,    &
            ringTime     = rTime,   &
            sticky       = .false., &
            rc=STATUS   )
       VERIFY_(STATUS)

       if(rTime == currTime) then
          call ESMF_AlarmRingerOn(STATE%TIME_TO_COUPLE(J), rc=status); VERIFY_(STATUS)
       end if


! initalize CLEAR ALARM from destination properties
!--------------------------------------------------

       call ESMF_TimeIntervalSet(TCLR, S=STATE%CLEAR_INTERVAL(J), &
            calendar=cal, RC=STATUS)
       VERIFY_(STATUS)

       if (TCLR < TS) TCLR = TS

       rTime = TM0 + TCPL - TCLR

       do while (rTime < currTime) 
          rTime = rTime + TCPL
       end do

       STATE%TIME_TO_CLEAR(J) = ESMF_AlarmCreate(NAME='TIME2CLEAR_' // trim(COMP_NAME) &
            // '_' // trim(NAME),   &
            clock        = CLOCK,   &
            ringInterval = TCPL,    & 
            ringTime     = rTime,   &
            sticky       = .false., &
            rc=STATUS   )
       VERIFY_(STATUS)

       if(rTime == currTime) then
          call ESMF_AlarmRingerOn(STATE%TIME_TO_CLEAR(J), rc=status); VERIFY_(STATUS)
       end if

! Get info from the SRC spec
!---------------------------

       call MAPL_VarSpecGet(STATE%SRC_SPEC(J),      &
                            DIMS       = DIMS,      &
                            SHORT_NAME = NAME,      &
                            UNGRIDDED_DIMS=UNGRD,   &
                                           RC=STATUS)
       VERIFY_(STATUS)

! We currently make these d1mension assumptions
!----------------------------------------------

       if(DIMS==MAPL_DIMSHORZVERT) then
          DIMS=3
       elseif(DIMS==MAPL_DIMSHORZONLY .or. DIMS==MAPL_DIMSTILETILE) then
          DIMS=2
       elseif(DIMS==MAPL_DIMSVERTONLY .or. DIMS==MAPL_DIMSTILEONLY) then
          DIMS=1
       else
          RETURN_(ESMF_FAILURE)
       end if


       has_ungrd = associated(UNGRD)
       if (has_ungrd) then
          DIMS = DIMS + size(UNGRD)
       end if
       ASSERT_(DIMS < 4) ! ALT: due to laziness we are supporting only 3 dims

       STATE%ACCUM_RANK(J) = DIMS

       call ESMF_StateGet(src, NAME, field, rc=status)
       VERIFY_(STATUS)
       call ESMF_AttributeGet(field, NAME="MINMAX", VALUE=minmax, RC=STATUS)
       if (status /= ESMF_SUCCESS) then
          minmax = MAPL_CplAverage
       end if
       state%couplerType(J) = MinMax

! Create Accumulators for 3 dimensions
!-------------------------------------

       select case(DIMS)

       case(3)
! Get SRC pointer, making sure it is allocated.
          call MAPL_GetPointer(SRC, PTR3, NAME, ALLOC=.TRUE., RC=STATUS)
          VERIFY_(STATUS)
! Allocate space for accumulator
          L1 = LBOUND(PTR3,3)
          LN = UBOUND(PTR3,3)
          allocate(PTR30(size(PTR3,1),size(PTR3,2),L1:LN), STAT=STATUS)
          VERIFY_(STATUS)
          if (STATE%couplerType(J) /= MAPL_CplAverage) then
             PTR30 = MAPL_UNDEF
          else
! Set accumulator values to zero
             PTR30 = 0.0
          endif
! Put pointer in accumulator
          STATE%ACCUMULATORS(J)=ESMF_LocalArrayCreate( PTR30, RC=STATUS)
          VERIFY_(STATUS)
          
       case(2)
          call MAPL_GetPointer(SRC, PTR2, NAME, ALLOC=.TRUE., RC=STATUS)
          VERIFY_(STATUS)
          allocate(PTR20(size(PTR2,1),size(PTR2,2)), STAT=STATUS)
          VERIFY_(STATUS)
          if (STATE%couplerType(J) /= MAPL_CplAverage) then
             PTR20 = MAPL_UNDEF
          else
             PTR20 = 0.0
          end if
          STATE%ACCUMULATORS(J)=ESMF_LocalArrayCreate( PTR20, RC=STATUS)
          VERIFY_(STATUS)

       case(1)
          call MAPL_GetPointer(SRC, PTR1, NAME, ALLOC=.TRUE., RC=STATUS)
          VERIFY_(STATUS)
          allocate(PTR10(size(PTR1)), STAT=STATUS)
          VERIFY_(STATUS)
          if (STATE%couplerType(J) /= MAPL_CplAverage) then
             PTR10 = MAPL_UNDEF
          else
             PTR10 = 0.0
          end if
          STATE%ACCUMULATORS(J)=ESMF_LocalArrayCreate( PTR10, RC=STATUS)
          VERIFY_(STATUS)

       case default
          RETURN_(ESMF_FAILURE)

       end select
    end do

    RETURN_(ESMF_SUCCESS)

  end subroutine Initialize


!BOPI

! !IROUTINE: RUN

! !DESCRIPTION: {Run method for the generic coupler.}

! !INTERFACE:

  subroutine Run(CC, SRC, DST, CLOCK, RC)
    
! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "MAPL_GenericCplCompRun"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    VERIFY_(STATUS)

    STATE     =>  WRAP%INTERNAL_STATE

! If the state is inactive, src and dst are the same
! --------------------------------------------------
    
    if(STATE%ACTIVE) then

! Make sure SRC and DST descriptors exist 
!----------------------------------------

       ASSERT_(associated(STATE%SRC_SPEC))
       ASSERT_(associated(STATE%DST_SPEC))

! Update accumulators on SRC grid.
!---------------------------------

       call  ACCUMULATE(SRC, STATE, RC=STATUS)
       VERIFY_(STATUS)

! Periodically transfer accumulators to DST arrays
!-------------------------------------------------

       call  COUPLE    (DST, STATE, RC=STATUS)
       VERIFY_(STATUS)

! Zero accumulators when next averaging interval starts
!------------------------------------------------------

       call ZERO_CLEAR_COUNT(STATE, RC=STATUS)
       VERIFY_(STATUS)
    end if

    RETURN_(ESMF_SUCCESS)

  contains

  subroutine ACCUMULATE(SRC, STATE, RC)
    type (ESMF_State)           :: SRC
    type (MAPL_GenericCplState) :: STATE
    integer, optional           :: RC 

! local vars

    integer                               :: J
    integer                               :: I1, I2, I3
    integer                               :: couplerType
    character (len=ESMF_MAXSTR)           :: NAME
    integer                               :: DIMS
    real, pointer                         :: PTR1 (:)
    real, pointer                         :: PTR2 (:,:)
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:)
    real, pointer                         :: PTR20(:,:)
    real, pointer                         :: PTR30(:,:,:)
    integer, pointer                      :: PTR1c(:)     => NULL()
    integer, pointer                      :: PTR2c(:,:)   => NULL()
    integer, pointer                      :: PTR3c(:,:,:) => NULL()

    character(*), parameter       :: IAm="ACCUMULATE"
    integer                       :: STATUS

    do J = 1, size(STATE%SRC_SPEC)
       couplerType = state%couplerType(J)

! Accumulate only if we are in the couplings averaging interval
!--------------------------------------------------------------

       if(STATE%ACCUM_COUNT(J) < 0) cycle

! Get info from the SRC spec
!---------------------------

       call MAPL_VarSpecGet(STATE%SRC_SPEC(J), SHORT_NAME=NAME, RC=STATUS)
       VERIFY_(STATUS)

       DIMS = STATE%ACCUM_RANK(J)

! Process the 3 dimensions
!------------------------- 

       select case(DIMS)

       case(3)
          call MAPL_GetPointer  (SRC, PTR3, NAME,            RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR30,RC=STATUS)
          VERIFY_(STATUS)
          PTR3c => STATE%ARRAY_COUNT(J)%PTR3C

          if(.not.associated(PTR3C)) then
             if(  any( PTR3==MAPL_UNDEF ) ) then
                allocate(PTR3C(size(PTR3,1), size(PTR3,2), size(PTR3,3)),STAT=STATUS)
                VERIFY_(STATUS)
                PTR3C = STATE%ACCUM_COUNT(J)
!               put it back into array
                STATE%ARRAY_COUNT(J)%PTR3C => PTR3c
                VERIFY_(STATUS)
             end if
          end if

          if (couplerType == MAPL_CplAverage) then
             if(associated(PTR3C)) then
                where (PTR3 /= MAPL_Undef)
                   PTR30 = PTR30 + PTR3
                   PTR3c = PTR3c + 1
                end where
             else
                PTR30 = PTR30 + PTR3
             end if
          else
             DO I1=1,size(PTR3,1)
                DO I2=1,size(PTR3,2)
                   DO I3=1,size(PTR3,3)
                      if (PTR30(I1,I2,I3)== MAPL_Undef) then
                         PTR30(I1,I2,I3) = PTR3(I1,I2,I3)
                      else 
                         if (couplerType == MAPL_CplMax) then
                            PTR30(I1,I2,I3) = max(PTR30(I1,I2,I3),PTR3(I1,I2,I3))
                         else if (couplerType == MAPL_CplMin) then
                            PTR30(I1,I2,I3) = min(PTR30(I1,I2,I3),PTR3(I1,I2,I3))
                         end if
                      end if
                   end DO
                end DO
             end DO
          end if

       case(2)
          call MAPL_GetPointer  (SRC, PTR2, NAME,            RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR20,RC=STATUS)
          VERIFY_(STATUS)
          PTR2c => STATE%ARRAY_COUNT(J)%PTR2C

          if(.not.associated(PTR2C)) then
             if(  any( PTR2==MAPL_UNDEF ) ) then
                allocate(PTR2C(size(PTR2,1), size(PTR2,2)), STAT=STATUS)
                VERIFY_(STATUS)
                PTR2C = STATE%ACCUM_COUNT(J)
!               put it back into array
                STATE%ARRAY_COUNT(J)%PTR2C => PTR2c
                VERIFY_(STATUS)
             end if
          end if

          if (couplerType == MAPL_CplAverage) then
             if(associated(PTR2C)) then
                where (PTR2 /= MAPL_Undef)
                   PTR20 = PTR20 + PTR2
                   PTR2c = PTR2c + 1
                end where
             else
                PTR20 = PTR20 + PTR2
             end if
          else
             DO I1=1,size(PTR2,1)
                DO I2=1,size(PTR2,2)
                   if (PTR20(I1,I2)== MAPL_Undef) then
                      PTR20(I1,I2) = PTR2(I1,I2)
                   else 
                      if (couplerType == MAPL_CplMax) then
                         PTR20(I1,I2) = max(PTR20(I1,I2),PTR2(I1,I2))
                      else if (couplerType == MAPL_CplMin) then
                         PTR20(I1,I2) = min(PTR20(I1,I2),PTR2(I1,I2))
                      end if
                   end if
                end DO
             end DO
          endif

       case(1)
          call MAPL_GetPointer  (SRC, PTR1, NAME,            RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR10,RC=STATUS)
          VERIFY_(STATUS)
          PTR1c => STATE%ARRAY_COUNT(J)%PTR1C

          if(.not.associated(PTR1C)) then
             if(  any( PTR1==MAPL_UNDEF ) ) then
                allocate(PTR1C(size(PTR1,1)), STAT=STATUS)
                VERIFY_(STATUS)
                PTR1C = STATE%ACCUM_COUNT(J)
!               put it back into array
                STATE%ARRAY_COUNT(J)%PTR1C => PTR1c
                VERIFY_(STATUS)
             end if
          end if

          if (couplerType == MAPL_CplAverage) then
             if(associated(PTR1C)) then
                where (PTR1 /= MAPL_Undef)
                   PTR10 = PTR10 + PTR1
                   PTR1c = PTR1c + 1
                end where
             else
                PTR10 = PTR10 + PTR1
             end if
          else
             DO I1=1,size(PTR1,1)
                if (PTR10(I1)== MAPL_Undef) then
                   PTR10(I1) = PTR1(I1)
                else 
                   if (couplerType == MAPL_CplMax) then
                      PTR10(I1) = max(PTR10(I1),PTR1(I1))
                   else if (couplerType == MAPL_CplMin) then
                      PTR10(I1) = min(PTR10(I1),PTR1(I1))
                   end if
                end if
             end DO
          endif

       case default
          RETURN_(ESMF_FAILURE)

       end select

       if(couplerType == MAPL_CplMax .or. couplerType == MAPL_CplMin) then
        STATE%ACCUM_COUNT(J) = 1
       else
        STATE%ACCUM_COUNT(J) = STATE%ACCUM_COUNT(J) + 1
       endif

    end do

    RETURN_(ESMF_SUCCESS)
  end subroutine ACCUMULATE


  subroutine ZERO_CLEAR_COUNT(STATE, RC)
    type (MAPL_GenericCplState) :: STATE
    integer, optional           :: RC 

! local vars

    integer                               :: J
    integer                               :: DIMS
    logical                               :: RINGING
    real, pointer                         :: PTR1 (:)
    real, pointer                         :: PTR2 (:,:)
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:)
    real, pointer                         :: PTR20(:,:)
    real, pointer                         :: PTR30(:,:,:)

    character(*), parameter       :: IAm="ZERO_CLEAR_COUNT"
    integer                       :: STATUS

    do J = 1, size(STATE%SRC_SPEC)

       RINGING = ESMF_AlarmIsRinging(STATE%TIME_TO_CLEAR(J), RC=STATUS)
       VERIFY_(STATUS)
       
       if (RINGING) then
          call ESMF_AlarmRingerOff(STATE%TIME_TO_CLEAR(J), RC=STATUS)
          VERIFY_(STATUS)

          DIMS = STATE%ACCUM_RANK(J)

! Process the 3 dimension possibilities
!--------------------------------------

          select case(DIMS)

          case(3)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR30,RC=STATUS)
             VERIFY_(STATUS)
             if (STATE%couplerType(J) /= MAPL_CplAverage) then
                PTR30 = MAPL_UNDEF
             else
                PTR30 = 0.0
             endif
             if (associated(STATE%ARRAY_COUNT(J)%PTR3C)) STATE%ARRAY_COUNT(J)%PTR3C = 0

          case(2)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR20,RC=STATUS)
             VERIFY_(STATUS)
             if (STATE%couplerType(J) /= MAPL_CplAverage) then
                PTR20 = MAPL_UNDEF
             else
                PTR20 = 0.0
             endif
             if (associated(STATE%ARRAY_COUNT(J)%PTR2C)) STATE%ARRAY_COUNT(J)%PTR2C = 0

          case(1)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR10,RC=STATUS)
             VERIFY_(STATUS)
             if (STATE%couplerType(J) /= MAPL_CplAverage) then
                PTR10 = MAPL_UNDEF
             else
                PTR10 = 0.0
             endif
             if (associated(STATE%ARRAY_COUNT(J)%PTR1C)) STATE%ARRAY_COUNT(J)%PTR1C = 0

          case default
             RETURN_(ESMF_FAILURE)

          end select

          STATE%ACCUM_COUNT(J) = 0.0
          
       end if
    end do

    RETURN_(ESMF_SUCCESS)
  end subroutine ZERO_CLEAR_COUNT


  subroutine COUPLE(SRC, STATE, RC)
    type (ESMF_State)           :: SRC
    type (MAPL_GenericCplState) :: STATE
    integer, optional           :: RC 

! local vars

    integer                               :: J
    character (len=ESMF_MAXSTR)           :: NAME
    integer                               :: DIMS
    real, pointer                         :: PTR1 (:)
    real, pointer                         :: PTR2 (:,:)
    real, pointer                         :: PTR3 (:,:,:)
    real, pointer                         :: PTR10(:)
    real, pointer                         :: PTR20(:,:)
    real, pointer                         :: PTR30(:,:,:)
    integer, pointer                      :: PTR1c(:)
    integer, pointer                      :: PTR2c(:,:)
    integer, pointer                      :: PTR3c(:,:,:)
    logical                               :: RINGING

    character(*), parameter       :: IAm="COUPLE"
    integer                       :: STATUS

    do J = 1, size(STATE%SRC_SPEC)

       RINGING = ESMF_AlarmIsRinging(STATE%TIME_TO_COUPLE(J), RC=STATUS)
       VERIFY_(STATUS)
       
       if (RINGING) then

          call ESMF_AlarmRingerOff(STATE%TIME_TO_COUPLE(J), RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_VarSpecGet(STATE%DST_SPEC(J), SHORT_NAME=NAME, RC=STATUS)
          VERIFY_(STATUS)

          DIMS = STATE%ACCUM_RANK(J)

! Process the three dimension possibilities
!------------------------------------------

          select case(DIMS)

          case(3)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR30,RC=STATUS)
             VERIFY_(STATUS)
             call MAPL_GetPointer  (DST, PTR3, NAME,            RC=STATUS)
             VERIFY_(STATUS)
             PTR3c => STATE%ARRAY_COUNT(J)%PTR3C
             if(associated(PTR3C)) then
                where (PTR3C /= 0) 
                   PTR30 = PTR30 / PTR3C
                elsewhere
                   PTR30 = MAPL_Undef
                end where
             elseif(STATE%ACCUM_COUNT(J)>0) then
                PTR30 = PTR30 / STATE%ACCUM_COUNT(J)
             else
                PTR30 = MAPL_Undef
             end if

! Regrid stubbed

             PTR3 = PTR30

          case(2)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR20,RC=STATUS)
             VERIFY_(STATUS)
             call MAPL_GetPointer  (DST, PTR2, NAME,            RC=STATUS)
             VERIFY_(STATUS)
             PTR2c => STATE%ARRAY_COUNT(J)%PTR2C
             if(associated(PTR2C)) then
                where (PTR2C /= 0) 
                   PTR20 = PTR20 / PTR2C
                elsewhere
                   PTR20 = MAPL_Undef
                end where
             elseif(STATE%ACCUM_COUNT(J)>0) then
                PTR20 = PTR20 / STATE%ACCUM_COUNT(J)
             else
                PTR20 = MAPL_Undef
             end if

! Regrid stubbed

             PTR2 = PTR20

          case(1)
             call ESMF_LocalArrayGet(STATE%ACCUMULATORS(J),farrayPtr=PTR10,RC=STATUS)
             VERIFY_(STATUS)
             call MAPL_GetPointer  (DST, PTR1, NAME,            RC=STATUS)
             VERIFY_(STATUS)
             PTR1c => STATE%ARRAY_COUNT(J)%PTR1C
             if(associated(PTR1C)) then
                where (PTR1C /= 0) 
                   PTR10 = PTR10 / PTR1C
                elsewhere
                   PTR10 = MAPL_Undef
                end where
             elseif(STATE%ACCUM_COUNT(J)>0) then
                PTR10 = PTR10 / STATE%ACCUM_COUNT(J)
             else
                PTR10 = MAPL_Undef
             end if

! Regrid stubbed

             PTR1 = PTR10

          case default
             RETURN_(ESMF_FAILURE)

          end select

          STATE%ACCUM_COUNT(J) = -1

       end if


    end do

    RETURN_(ESMF_SUCCESS)
  end subroutine COUPLE

 end subroutine Run

!---------------------------

!BOPI

! !IROUTINE: FINALIZE

! !DESCRIPTION: {Finalize method for the generic coupler.}

! !INTERFACE:

  subroutine Finalize(CC, SRC, DST, CLOCK, RC)

! !ARGUMENTS:

    type (ESMF_CplComp)      :: CC
    type (ESMF_State)        :: SRC
    type (ESMF_State)        :: DST
    type (ESMF_Clock)        :: CLOCK
    integer, intent(  OUT)   :: RC

!EOPI
! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Locals

    type (MAPL_GenericCplState), pointer  :: STATE
    type (MAPL_GenericCplWrap )           :: WRAP

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    IAm = "MAPL_GenericCplCompFinalize"
    call ESMF_CplCompGet( CC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

    call ESMF_CplCompGetInternalState ( CC, WRAP, STATUS )
    VERIFY_(STATUS)

    STATE     =>  WRAP%INTERNAL_STATE


    call write_parallel('STUBBED in CPL finalize')
    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize
end module MAPL_GenericCplCompMod
