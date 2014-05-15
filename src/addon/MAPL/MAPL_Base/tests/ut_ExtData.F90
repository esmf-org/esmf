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
!
! ut_ExtaData.x - Simple ESMF/MAPL example demonstrating 
!                 MAPL_ExtDataGridComp
!
!                       ---
!
! In addition to MAPL_ExtDataGridComp.rc also needs:
!    AGCM.rc 
!    CAP.rc
!    HISTORY.rc
!
! Sample files:
!    # ---- AGCM.rc ---------------
!    NX: 2
!    NY: 4
!    GRIDNAME: PC144x91-DC
!    LM: 72
!    # ----------------------------
!
!    # --- CAP.rc -----------------
!    MAPLROOT_COMPNAME: RUT
!            ROOT_NAME: RUT
!
!    ROOT_CF: AGCM.rc
!    HIST_CF: HISTORY.rc
!    
!    BEG_DATE:     18910301 000000
!    END_DATE:     20070630 210000
!    JOB_SGMT:     00000030 000000
!    NUM_SGMT:     1
!    HEARTBEAT_DT:       1800
!    # ----------------------------
!
!
!
! REVISION HISTORY:
!
! 29 Sep 2011    Anton Darmenov   First implementation
!


#include "MAPL_Generic.h"


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: RUTMod - Implements Interface to a minimalistic MAPL GC that
!                   serves as a parent of the MAPL_ExtData GC
!
! !INTERFACE:
!
   MODULE RUTMod
!
! !USES:
!
   USE ESMF_Mod
   USE MAPL_Mod

   use MAPL_ExtDataGridCompMod, only: ExtData_SetServices => SetServices


   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices

   integer :: ExtData



!
! !DESCRIPTION: 
!
!  {\tt RUT} Root Utility Test is a minimalistic parent grid component that creates 
!  ExtData grid component.
!
!
! !REVISION HISTORY:
!
!  29 Sep 2011  Anton Darmenov  Implemented as part of the ExtData 
!                               utility test.
!
!EOP
!-------------------------------------------------------------------------


CONTAINS


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices --- Sets IRF services for the RUT
!
! !INTERFACE:

   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: Sets Initialize, Run and Finalize services. 
!
! !REVISION HISTORY:
!
!  29 Sep 2011  Anton Darmenov   Cloned from the ExtData GC code
!
!EOP
!-------------------------------------------------------------------------

                            __Iam__('SetServices')

!   Local derived type aliases
    type(ESMF_Config)          :: CF
    character(len=ESMF_MAXSTR) :: comp_name

!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
    Iam = trim(comp_name) // '::' // trim(Iam)

!   Greetings
!   ---------
    if (MAPL_am_I_root()) then
         print *, trim(Iam)//': ACTIVE'
         print *
    end if

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETINIT,  Initialize_, __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETRUN,   Run_,        __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETFINAL, Finalize_,   __RC__ )

!   Add the ExtData as a child
!   --------------------------
    ExtData = MAPL_AddChild ( GC, NAME='ExtData', SS=ExtData_SetServices, __RC__ )

!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, __RC__ )

!   All done
!   --------

    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize RUT
!
! !INTERFACE:
!

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out)            :: rc         ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  29 Sep 2011  Anton Darmenov   Cloned from the ExtData GC code
!
!EOP
!-------------------------------------------------------------------------

                              __Iam__('Initialize_')

   type(ESMF_Grid)             :: GRID        ! Grid
   type(ESMF_Config)           :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)     :: comp_name

  
!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, config=CF, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Create grid for this GC
!  ------------------------
   call MAPL_GridCreate  (GC, __RC__ )
   call ESMF_GridCompGet (GC, grid=GRID, __RC__)

!  Initialize MAPL Generic
!  -----------------------
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  __RC__ )

!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs RUT
!
! !INTERFACE:
!

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  29 Sep 2011  Anton Darmenov   Cloned from the ExtData GC code
!
!EOP
!-------------------------------------------------------------------------

                              __Iam__('Run_')


   character(len=ESMF_MAXSTR)    :: comp_name


!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!   Call Run for every Child
!   -------------------------
    call MAPL_GenericRun ( GC, IMPORT, EXPORT, CLOCK,  __RC__)


!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize RUT
!
! !INTERFACE:
!

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  29 Sep 2011  Anton Darmenov   Cloned from the ExtData GC code
!
!EOP
!-------------------------------------------------------------------------

                              __Iam__('Finalize_')
   

   character(len=ESMF_MAXSTR)  :: comp_name

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // trim(Iam)

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  __RC__ )

!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

end module RUTMod


! ----------------------------------------------------------------------


Program ut_ExtData
   
   use MAPL_Mod

   use RUTMod,                  only: ROOT_SetServices    => SetServices
   use MAPL_ExtDataGridCompMod, only: ExtData_SetServices => SetServices

   implicit NONE

   integer :: STATUS
   logical :: am_I_root

   character(len=*), parameter :: Iam = 'ut_ExtData'

!                             -----
   
    call MAPL_CAP(ROOT_SetServices, AmIRoot = am_I_root, rc=STATUS)

    call exit(STATUS)

end program ut_ExtData
