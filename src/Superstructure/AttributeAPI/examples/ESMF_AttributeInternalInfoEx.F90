! $Id: ESMF_AttributeInternalInfoEx.F90,v 1.1 2012/04/25 23:04:30 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributeInternalInfoEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"

!BOE
! \subsubsection{Accessing object information through Attribute} 
! \label{ex:AttributeInternalInfoEx}
!
! This example demonstrates the ability to access object information through
! the Attribute class.  The
! functionality that is demonstrated includes setting and getting Attributes, 
! working with Attributes with different types and lists, removing Attributes,
! and getting default Attributes.  Various other uses of 
! {\tt ESMF\_AttributeGet()} is covered in detail in the last section.  The
! first thing we must do is declare variables and initialize ESMF.
!EOE


!  !PROGRAM: ESMF\_AttributeInternalInfo - Examples of Attribute usage.
!
!  !DESCRIPTION: 
!
! This program shows examples of Attribute usage


!BOC
      ! Use ESMF framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet, result
      type(ESMF_VM)           :: vm
      type(ESMF_Grid)         :: grid
      type(ESMF_DistGrid)     :: distgrid
      character(ESMF_MAXSTR)  :: name
      character(ESMF_MAXSTR),dimension(3) :: inputList 
      integer(ESMF_KIND_I4)  :: exclusiveLBound(2), exclusiveUBound(2), exclusiveCount(2)

      character(ESMF_MAXSTR)               :: testname
      character(ESMF_MAXSTR)               :: failMsg
!EOC

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_AttributeInternalInfoEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOC
      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="AttributeInternalInfoEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

      distgrid=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4, &
                       name="AttributeTestGrid", rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (localPet==0) then
        print *, "--------------------------------------------- "
        print *, "Start of ESMF_Attribute Internal Info Example"
        print *, "--------------------------------------------- "
      endif

!BOE
!     Simple example
!EOE

!BOC

  call ESMF_AttributeGet(grid, name="ESMF:name", value=name, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!     More complex example
!EOE

!BOC
  inputList(:) = ''
  inputList(1) = 'localDe:0'
  inputList(2) = 'itemflag:ESMF_GRIDITEM_MASK'
  inputList(3) = 'staggerloc:ESMF_STAGGERLOC_CENTER'
  call ESMF_AttributeGet(grid, name="ESMF:exclusiveCount", &
                         valueList=exclusiveCount, inputList=inputList, rc=rc)
!EOC

      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


    call ESMF_Finalize(rc=rc)

  if (localPet==0) then
      print *, "------------------------------------------- "
      print *, "End of ESMF_Attribute Internal Info Example"
      print *, "------------------------------------------- "
  endif

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributeInternalInfoEx.F90"
  else
    print *, "FAIL: ESMF_AttributeInternalInfoEx.F90"
  endif
  
end program
