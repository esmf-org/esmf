! $Id: ESMF_LocalArrayDataUTest.F90,v 1.1 2007/04/09 18:20:26 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_LocalArrayDataUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_LocalArrayDataTest - Check basic data methods in LocalArray
!
! !DESCRIPTION:
!
! The code in this file drives F90 LocalArrayData unit tests.
! The companion file ESMF\_LocalArray.F90 contains the definitions for the
! LocalArray methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_LocalArrayDataUTest.F90,v 1.1 2007/04/09 18:20:26 theurich Exp $'
!------------------------------------------------------------------------------
    
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    integer :: i
    type(ESMF_LocalArray) :: la


    ! Array of data type Integer * 4 
    type PtrIWrap1  
    sequence
      integer (ESMF_KIND_I4),dimension(:), pointer :: data
    end type 
    
#ifdef ESMF_EXHAUSTIVE
    ! Array of data type Real * 4 
    type PtrRWrap2  
    sequence
      real (ESMF_KIND_R4),dimension(:,:), pointer :: data
    end type 

    ! Array of data type Real * 8 
    type PtrR8Wrap3  
    sequence
      real (ESMF_KIND_R8),dimension(:,:,:), pointer :: data
    end type 
    
    ! used below
    type mytype
    sequence
       integer :: myint
       real :: myreal
    end type

    ! Arrays of a derived data type
    type PtrSWrap1  
    sequence
      type(mytype),dimension(:), pointer :: data
    end type 
#endif
    

    
    type(PtrIWrap1) :: sizetest1I
    
#if 0
#ifdef ESMF_EXHAUSTIVE
    type(PtrRWrap2) :: sizetest2R
    type(PtrR8Wrap3) :: sizetest3R8
    type(PtrSWrap1) :: sizetest1S
#endif
#endif
    

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    

!------------------------------------------------------------------------------
    !NEX_UTest
    allocate(sizetest1I%data(-12:-6), stat=rc)
    do i = -12, -6
      sizetest1I%data(i) = i*1000
    enddo

    la = ESMF_LocalArrayCreate(sizetest1I%data, ESMF_DATA_REF, rc=rc)
    write(failMsg, *) "cannot create LocalArray"
    write(name, *) "Creating a LocalArray from an allocated F90 Pointer"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------------

    call ESMF_LocalArrayPrint(la, rc=rc)

#ifdef ESMF_EXHAUSTIVE
    
#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_LocalArrayDataUTest
    
    
