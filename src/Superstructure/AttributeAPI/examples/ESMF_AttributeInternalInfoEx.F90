! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
! the Attribute class.  This capability is enabled only in the Grid class
! at this point.  Internal Grid information is retrieved through the 
! ESMF\_AttributeGet() interface by specifying the name as a character
! string holding the keyword of the desired piece of Grid information.
! Information that requires input arguments is retrieved by
! specifying the input argument in a character array.
!
! Some examples of this capability are given in this section.  The first
! shows how to get the name of a Grid, and the second shows how
! to get a more complex parameter which requires inputs.  First, we must
! initialize ESMF, declare some variables, and create a Grid:
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
      integer(ESMF_KIND_I4)  :: exclusiveLBound(2), exclusiveUBound(2)
      integer(ESMF_KIND_I4)  :: exclusiveCount(2)

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
      call ESMF_Initialize(vm=vm, &
                defaultlogfilename="AttributeInternalInfoEx.Log", &
                logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

      distgrid=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      grid=ESMF_GridCreate(distgrid=distgrid, &
                       coordTypeKind=ESMF_TYPEKIND_I4, &
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
!  This first call shows how to retrieve the name of a Grid.  The 
!  return value is a character string in this case, which must be
!  provided as the argument to 'value'.  The 'name' of the Attribute
!  is specified as a character string whose value is the keyword of the piece
!  of Grid information to retrieve preceded by a special tag.
!  This tag, 'ESMF:', tells the ESMF\_AttributeGet() routine that it
!  should be looking for class information, rather than an Attribute
!  that was previously created with the ESMF\_AttributeSet() call.
!EOE

!BOC

  call ESMF_AttributeGet(grid, name="ESMF:name", value=name, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!  This second call demonstrates how to retrieve the exclusiveCount from
!  a Grid.  As before, the 'name' of the Attribute is specified as the
!  keyword of the information to retrieve, preceded by the 'ESMF:' tag.
!  The value is an integer array, which must be allocated to a sufficient
!  size to hold all of the requested information.  The exclusiveCount of
!  a Grid requires three pieces of input information: localDe, itemflag, and
!  staggerloc.  These are specified in an array of character strings.  The
!  name of the input parameter is separated from the value by a ':'.
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

!BOE
!  That all there is to it!  Now we just have to Finalize ESMF:
!EOE

!BOC
    call ESMF_Finalize(rc=rc)
!EOC

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
