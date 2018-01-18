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
!
    program ESMF_FieldArgGridEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_FieldArgGridEx - Field with replicated dimension
!
! !DESCRIPTION:
!
! This program shows examples of Field with replicated dimension
!-----------------------------------------------------------------------------
#include "ESMF.h"
    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF
    implicit none
    
    type(ESMF_Grid) :: grid3d
    type(ESMF_VM) :: vm
    type(ESMF_ArraySpec) :: arrayspec2D
    integer :: ind1d, xdim, ydim, zdim, total, x, y
    integer :: i, remain
    integer :: myPet, petCount, halfPets
    integer :: localArbIndexCount
    integer, allocatable :: localArbIndex(:,:)
    integer                 :: finalrc, rc
    type(ESMF_Field)        :: field
    logical :: correct
    integer :: rank, dimCount, result
    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_FieldArbGridEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------



!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(vm=vm, defaultlogfilename="FieldArbGridEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Calculate localArbIndex and localArbIndexCount for a 100x200 2D arbitrary grid with 
    ! an optional undistributed 3rd dimenison of size 4
    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    ! grid dimension: xdim and ydim are arbitrarily distributed
    xdim = 100
    ydim = 200
    zdim = 4
  
    ! calculate the localcount and the local indices based on the total number of PETS
    total = xdim*ydim
    halfPets = petCount/2
    ! let's make the first half pet twice of the cells of the second half
    localArbIndexCount = total/(petCount+halfPets)
    remain = total-localArbIndexCount*(petCount+halfPets)
    if (myPet < halfPets) localArbIndexCount = localArbIndexCount*2
    if (myPet == petCount-1) localArbIndexCount = localArbIndexCount+remain
    ! car deal the cells with the first half of the Pets gets two each time
    ! the remaining cells are given to the last Pet
    allocate(localArbIndex(localArbIndexCount,2))
  
    if (myPet < halfPets) then
       ind1d = myPet*2
       do i=1,localArbIndexCount,2
         y = mod(ind1d,ydim)+1
         x = ind1d/ydim+1
         localArbIndex(i,1)=y
         localArbIndex(i,2)=x
         if (y<ydim) then
           localArbIndex(i+1,1)=y+1
           localArbIndex(i+1,2)=x
         else
           localArbIndex(i+1,1)=1
           localArbIndex(i+1,2)=x+1
         endif
         ind1d = ind1d+petCount+halfPets
       enddo 
    else
       ind1d=myPet+halfPets
       do i=1,localArbIndexCount
         y = mod(ind1d,ydim)+1
         x = ind1d/ydim+1
         localArbIndex(i,1)=y
         localArbIndex(i,2)=x
         ind1d = ind1d+petCount+halfPets
       enddo
    endif
    if (myPet == petCount-1) then
      ind1d = total-remain+1
      do i=localArbIndexCount-remain+1,localArbIndexCount
         y = mod(ind1d,ydim)+1
         x = ind1d/ydim+1
         localArbIndex(i,1)=y
         localArbIndex(i,2)=x
         ind1d = ind1d+1
      enddo
    endif
   
    correct=.true.
    rc=ESMF_SUCCESS

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create a Field on an arbitrarily distributed Grid}
!\label{sec:field:usage:createArbGrid}
!
!  With the introduction of Field on arbitrarily distributed Grid, Field has two kinds of dimension
!  count: one associated geometrical (or physical) dimensionality, the other one associated with its
!  memory index space representation. Field and Grid dimCount reflect the physical index 
!  space of the objects. A new type of dimCount rank should be added to both of these entities.
!  The rank gives the number of dimensions of the memory index space of the objects.
!  This would be the dimension of the pointer pulled out of Field and the
!  size of the bounds vector, for example. 
!
!  For non-arbitrary Grids rank=dimCount, but for grids and fields with
!  arbitrary dimensions rank = dimCount - (number of Arb dims) + 1
!  (Internally Field can use the Arb info from the grid to create the mapping
!  from the Field Array to the DistGrid)
!
!  When creating a Field size(GridToFieldMap)=dimCount for both Arb and Non-arb grids
!  This array specifies the mapping of Field to Grid identically for both Arb and Nonarb grids 
!  If a zero occurs in an entry corresponding to any arbitrary dimension, then
!  a zero must occur in every entry corresponding to an arbitrary dimension (i.e.
!  all arbitrary dimensions must either be all replicated or all not replicated,
!  they can't be broken apart).
!
!  In this example an {\tt ESMF\_Field} is created from an arbitrarily distributed {\tt ESMF\_Grid} and 
!  an {\tt ESMF\_Arrayspec}. A user can also use other {\tt ESMF\_FieldCreate()} methods to create 
!  such a Field, this example illustrates the key concepts and use of Field on arbitrary distributed Grid.
!  
!  The Grid is 3 dimensional in physics index space but the first two dimension are collapsed into
!  a single memory index space. Thus the resulting Field is 3D in physics index space and 2D in memory index
!  space. This is made obvious with the 2D arrayspec used to create this Field.
!
!EOE

!BOC
    ! create a 3D grid with the first 2 dimensions collapsed 
    ! and arbitrarily distributed
    grid3d = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
      minIndex=(/1,1,1/), maxIndex=(/xdim, ydim,zdim/), &
      arbIndexList=localArbIndex,arbIndexCount=localArbIndexCount, &
      name="arb3dgrid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create a 2D arrayspec
    call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R4, &
         rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create a 2D Field using the Grid and the arrayspec
    field = ESMF_FieldCreate(grid3d, arrayspec2D, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, &
                       rc=rc)
    if (myPet .eq. 0) print *, 'Field rank, dimCount', &
                                rank, dimCount
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    ! verify that the dimension counts are correct
    if (rank .ne. 2) correct = .false.
    if (dimCount .ne. 3) correct = .false.  
!EOC
    call ESMF_FieldDestroy(field, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{Create a Field on an arbitrarily distributed Grid with replicated dimensions \& ungridded bounds}
!\label{sec:field:usage:createArbGridRep}
!
!  The next example is slightly more complicated in
!  that the Field also contains one ungridded dimension and its gridded dimension
!  is replicated on the arbitrarily distributed dimension of the Grid.
! 
!  The same 3D Grid and 2D arrayspec in the previous example
!  are used but a gridToFieldMap argument
!  is supplied to the {\tt ESMF\_FieldCreate()} call. The first 2 entries of
!  the map are 0, the last (3rd) entry is 1. The 3rd dimension of the Grid is
!  mapped to the first dimension of the Field, this dimension is then replicated
!  on the arbitrarily distributed dimensions of the Grid. In addition, the
!  Field also has one ungridded dimension. Thus the final dimension count of the
!  Field is 2 in both physics and memory index space.
!
!EOE
!BOC
    field = ESMF_FieldCreate(grid3d, arrayspec2D,gridToFieldMap=(/0,0,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/),rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, &
                       rc=rc)
    if (myPet .eq. 0) print *, 'Field rank, dimCount', &
                                rank, dimCount
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    if (rank .ne. 2) correct = .false.
    if (dimCount .ne. 2) correct = .false.  
!EOC
    print *, "Field with replicated dimension returned"
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! release resources
    call ESMF_FieldDestroy(field, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid3d, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(localArbIndex)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
     call ESMF_Finalize(rc=rc)
!-------------------------------------------------------------------------

    if (rc.NE.ESMF_SUCCESS .or. (.not. correct)) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldArgGridEx.F90"
    else
        print *, "FAIL: ESMF_FieldArgGridEx.F90"
    end if
end program ESMF_FieldArgGridEx
