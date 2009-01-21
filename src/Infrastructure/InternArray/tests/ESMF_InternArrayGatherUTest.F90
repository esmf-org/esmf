! $Id: ESMF_InternArrayGatherUTest.F90,v 1.10.2.3 2009/01/21 21:25:22 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_ArrayGatherUTest

!------------------------------------------------------------------------------
!

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayGatherTest - Test ArrayGather functionalities
!
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayGather unit tests.
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternArrayGatherUTest.F90,v 1.10.2.3 2009/01/21 21:25:22 cdeluca Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    type(ESMF_VM):: vm
    integer:: localPet, petCount
    integer:: nlen, nsize, i, rootDE
    type(ESMF_InternArray) :: array1
    ! type(ESMF_Array) ::  array2
    integer(ESMF_KIND_I4), dimension(:), pointer :: f90ptr1
    ! integer(ESMF_KIND_I4), dimension(:), pointer :: f90ptr2
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_DELayout) :: layout
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords
    type(ESMF_Field) :: field
    type(ESMF_IGrid) :: igrid
    type(ESMF_FieldDataMap) :: datamap


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: rc, result = 0

    
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

    ! get global vm information
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)
    nsize =2
    nlen = nsize * petCount

    !Create the DELayout
   !===========================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) " DELayout Create Test"
    layout = ESMF_DELayoutCreate(vm, (/petCount,1/), rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    mincoords = (/0.0,0.0/)
    maxcoords = (/20.0,20.0/)

    !Create the igrid
   !===========================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) " IGrid Create Test"
    igrid = ESMF_IGridCreateHorzXYUni((/nlen,1/), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="igrid", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !Distribute the igrid
   !===========================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) " IGrid Distribute Test"
    call ESMF_IGridDistribute(igrid, delayout=layout, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !Datamap Set
   !===========================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) " Field Datamap set Test"
    call ESMF_FieldDataMapSetDefault(datamap, 1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !Specify settings for the field array
   !====================================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Array Spec Set Field Test"
    call ESMF_ArraySpecSet(arrayspec, rank=1, &
                           typekind=ESMF_TYPEKIND_I4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


   !Create the field 
   !=======================================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Create Field Test"
    field = ESMF_FieldCreate(igrid, arrayspec, &
                              horzRelloc=ESMF_CELL_CENTER, &
                              datamap=datamap, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !Validate
   !=======================================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Validate Field Test"
    call ESMF_FieldValidate(field, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !Get the Field array
   !=======================================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get the Field Array Test"
    call ESMF_FieldGet(field=field, array=array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
   !Get the Data Pointer
   !=======================================
   !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get the Data Pointer Test"
    call ESMF_InternArrayGetData(array1, f90ptr1, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
   !-------------------------------------------------------------------------------
    rootDE = petCount - 1
    nlen = nsize * petCount

   ! prepare array data
   do i=1, nsize
	f90ptr1(i) = ((2*localPet) + i)
	print *, "value = ", f90ptr1(i)
   enddo

   !-------------------------------------------------------------------------------
  !  Print an Array

    !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Print an Array Test"
    call ESMF_InternArrayPrint(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !-------------------------------------------------------------------------------
  !  Gather the arrays

    !NEX_removeUTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Test ArrayGather"
    ! Commented out because it hangs on some platforms
    ! Remove comment when bug 1324255 is fixed.
    !call ESMF_ArrayGather(array1, igrid=igrid, datamap=datamap, &
    !                      rootDE=rootDE, gatheredArray=array2, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
      !NEX____UTest Comment out because there is array2
      ! Verify array2 data after gather
      !write(failMsg, *) "Wrong data."
      !write(name, *) "Verifying array2 data after ArrayGather Test"
      !rc = ESMF_SUCCESS
      !if (localPet==rootDE) then
	!call ESMF_FieldGet(field=field, array=array2, rc=rc)
        !call ESMF_ArrayGetData(array2, f90ptr2, ESMF_DATA_REF, rc=rc)
        !do i=1, nlen
                !if (f90ptr2(i)/=i) rc = ESMF_FAILURE
		!print *, "f90ptr2 = ", f90ptr2(i)
        !enddo
        !call ESMF_ArrayPrint(array2)
      !else
        !rc = ESMF_SUCCESS
      !endif
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


     !-------------------------------------------------------------------------------
     ! Destroy all
       call ESMF_InternArrayDestroy(array1, rc)
      ! if (localPet==rootDE) then
       	!call ESMF_InternArrayDestroy(array2, rc)
       !endif
       call ESMF_FieldDestroy(field, rc)
       call ESMF_IGridDestroy(igrid, rc)
       call ESMF_DELayoutDestroy(layout, rc)

		
     

10    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_ArrayGatherUTest
    
!\end{verbatim}
  
