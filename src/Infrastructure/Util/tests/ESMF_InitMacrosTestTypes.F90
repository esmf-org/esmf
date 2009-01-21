! $Id: ESMF_InitMacrosTestTypes.F90,v 1.2.2.2 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF InitMacro Module
      module ESMF_InitMacrosTestTypesMod


!
!==============================================================================
!
! This file contains funtions to support the Initialization Macros
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
! !USES:
  use ESMF_UtilTypesMod

  ! make contents of module private
  private

!!!!!!!!!!!!!  Test Types !!!!!!!!!!!!!!!!!!!!

!  Example Shallow Type
   type ESMF_Shallow

#ifdef ESMF_NO_INITIALIZERS    
    integer :: num
#else
    integer :: num=4
#endif

    ESMF_INIT_DECLARE
   end type ESMF_Shallow


!  Example Deep Type
   type ESMF_Deep
    private
    integer :: num

    ESMF_INIT_DECLARE
   end type ESMF_Deep

!--------------------------------------------------------------------------
!
! PUBLIC TYPES:
   public ESMF_Shallow
   public ESMF_Deep


!--------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

   ! Function for getting init value from shallow type
   public ESMF_ShallowGetInitVal, ESMF_ShallowInit
   public ESMF_DeepGetInitVal, ESMF_DeepCreate, ESMF_DeepDestroy

contains

   ! Function for getting init value from shallow type
   function ESMF_ShallowGetInitVal(s)
      type(ESMF_Shallow) :: s
      ESMF_INIT_TYPE :: ESMF_ShallowGetInitVal 
       
       ESMF_ShallowGetInitVal = ESMF_INIT_GET(s) 

   end function ESMF_ShallowGetInitVal

   ! Initialization routine for shallow type
   subroutine ESMF_ShallowInit(s)
      type(ESMF_Shallow), intent(inout) :: s
       
       s%num=4

       ESMF_INIT_SET_DEFINED(s)
   end subroutine ESMF_ShallowInit




   ! Function for getting init value from deep type
   function ESMF_DeepGetInitVal(s)
      type(ESMF_Deep) :: s
      ESMF_INIT_TYPE :: ESMF_DeepGetInitVal 
       
       ESMF_DeepGetInitVal = ESMF_INIT_GET(s) 

   end function ESMF_DeepGetInitVal


   ! Deep type constructor function 
   function ESMF_DeepCreate()
       type(ESMF_Deep) :: ESMF_DeepCreate

       ESMF_DeepCreate%num=7

       ESMF_INIT_SET_CREATED(ESMF_DeepCreate)
   end function ESMF_DeepCreate


   ! Deep type destructor subroutine
   subroutine ESMF_DeepDestroy(d)
      type(ESMF_Deep), intent(inout) :: d
       
       ESMF_INIT_SET_DELETED(d)
   end subroutine ESMF_DeepDestroy

end module ESMF_InitMacrosTestTypesMod


