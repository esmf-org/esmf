! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_PointListUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_PointListUTest - This unit test file tests PointList methods.
! !DESCRIPTION:
!
! The code in this file drives F90 PointListCreate() unit tests.
! The companion file ESMF\_PointList.F90 contains the definitions for the
! PointList methods.

!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_PointListMod
  use ESMF_MeshMod
  use ESMF_VMMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc=1

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name



!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from user inputs"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_inputs(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing grid without masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_grid_nomask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)



  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing grid with masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_grid_wmask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)



  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing mesh via nodes without masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_mesh_nodes_nomask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing mesh via nodes with masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_mesh_nodes_wmask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing mesh via elements without masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_mesh_elems_nomask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)



  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing mesh via elements with masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_mesh_elems_wmask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing LocStream without masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_locStream_nomask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing LocStream with masking"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_locStream_wmask(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a PointList from existing mesh via nodes without masking, empty proc"
  write(failMsg, *) "Test unsuccessful"

  ! init success flag
  rc=ESMF_SUCCESS

  ! do test
  call test_pointlist_from_mesh_nodes_nomask_empty_proc(rc)

  ! return result
  call ESMF_TEST((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  call ESMF_UtilIOUnitFlush (6)

  call ESMF_TestEnd(ESMF_SRCLINE)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
#define ESMF_METHOD "ESMF_TESTS"

  subroutine test_pointlist_from_inputs(rc)
    integer, intent(out) :: rc

    integer :: localrc
    integer :: petCount,localPet

    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    real(ESMF_KIND_R8), dimension(3) :: mycoords1=(/11.0,13.0,17.0/)
    real(ESMF_KIND_R8), dimension(3) :: mycoords2=(/19.0,23.0,29.0/)
    real(ESMF_KIND_R8), dimension(3) :: mycoords3=(/31.0,37.0,41.0/)
    real(ESMF_KIND_R8), dimension(3) :: mycoords4=(/43.0,47.0,53.0/)

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !------------------------------------------------------------------------

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist = ESMF_PointListCreate(maxpts=7,numdims=3, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble creating pointlist'
       rc=ESMF_FAILURE
       return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. 7 .or. mypts .ne. 0 .or. mydims .ne. 3) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: 7  got: ',maxpts
       print*,'numpts should be: 0  got: ',mypts
       print*,'dims should be: 3  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       

    call ESMF_PointListAdd(pointlist=pointlist,id=123,loc_coords=mycoords1,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble adding point to pointlist'
       rc=ESMF_FAILURE
       return
    endif       
    call ESMF_PointListAdd(pointlist=pointlist,id=234,loc_coords=mycoords2,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble adding point to pointlist'
       rc=ESMF_FAILURE
       return
    endif       
    call ESMF_PointListAdd(pointlist=pointlist,id=345,loc_coords=mycoords3,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble adding point to pointlist'
       rc=ESMF_FAILURE
       return
    endif       
    call ESMF_PointListAdd(pointlist=pointlist,id=456,loc_coords=mycoords4,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble adding point to pointlist'
       rc=ESMF_FAILURE
       return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. 7 .or. mypts .ne. 4 .or. mydims .ne. 3) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: 7  got: ',maxpts
       print*,'numpts should be: 4  got: ',mypts
       print*,'dims should be: 3  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      

!    call ESMF_PointListWriteVTK(pointlist,"TestPointList")
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      


    !locations values are zero based
    call ESMF_PointListGetForLoc(pointlist,2,id=myid,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get for location routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (myid .ne. 345) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'id should be: 345  got: ',myid
       rc=ESMF_FAILURE
       return
    endif       

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble destroying pointlist'
       rc=ESMF_FAILURE
       return
    endif       

    rc=ESMF_SUCCESS

  end subroutine test_pointlist_from_inputs


  subroutine test_pointlist_from_grid_nomask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Grid) :: myGrid
    integer :: lDE, localDECount
    integer :: clbnd(3),cubnd(3)
    integer :: i1,i2
    real(ESMF_KIND_R8), pointer :: coordX(:),coordY(:)
    integer :: petCount,localPet
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


    ! Create Grid with globalXCountxglobalYCount cells
    myGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,20/), &
                                    coordSys=ESMF_COORDSYS_CART, &
                                    coordDep1 = (/1/), &
                                    coordDep2 = (/2/), &
                                    indexflag=ESMF_INDEX_GLOBAL,         &
                                    rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble creating grid'
       rc=ESMF_FAILURE
       return
    endif       

    ! Get number of local DEs
    call ESMF_GridGet(myGrid, localDECount=localDECount, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing localDECount from grid'
       rc=ESMF_FAILURE
       return
    endif       

    ! Allocate Center (e.g. Center) stagger
    call ESMF_GridAddCoord(myGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble adding coordinates to grid'
       rc=ESMF_FAILURE
       return
    endif       

    ! Loop through DEs and set Centers as the average of the corners
    do lDE=0,localDECount-1

      ! get and fill first coord array
      call ESMF_GridGetCoord(myGrid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, &
                             coordDim=1, &
                             computationalLBound=clbnd, computationalUBound=cubnd, &
                             farrayPtr=coordX, &
                             rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         print*,'ERROR:  trouble accessing coordinates from grid'
         rc=ESMF_FAILURE
                 return
      endif     

      do i1=clbnd(1),cubnd(1)
        coordX(i1) = i1*10.0
      enddo     
      local_pts=(cubnd(1)-clbnd(1)+1)
      test_coordx=coordX(clbnd(1))


      ! get and fill second coord array
      call ESMF_GridGetCoord(myGrid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             coordDim=2, &
                             computationalLBound=clbnd, computationalUBound=cubnd, &
                             farrayPtr=coordY, &
                             rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         print*,'ERROR:  trouble accessing coordinates from grid'
         rc=ESMF_FAILURE
                 return
      endif     

      do i2=clbnd(1),cubnd(1)
        coordY(i2) = i2*10.0
      enddo     
      local_pts=local_pts*(cubnd(1)-clbnd(1)+1)
      test_coordy=coordY(clbnd(1))

    enddo

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist=ESMF_PointListCreate(myGrid,ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      

    !locations values are zero based
    call ESMF_PointListGetForLoc(pointlist,0,loc_coords=test_coords,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get for location routine'
       rc=ESMF_FAILURE
       return
    endif       

    my_err1 = abs(test_coordx - test_coords(1))
    my_err2 = abs(test_coordy - test_coords(2))
    if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001) then
      print*,'ERROR:  unexpected coordinates for queried pointlist location:'
      print*,'expected ( ',test_coordx,' , ',test_coordy,' )  got  (',test_coords(1),',',test_coords(2),')'
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_GridDestroy(myGrid, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying grid'
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble destroying pointlist'
       rc=ESMF_FAILURE
       return
    endif       

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_grid_nomask


  subroutine test_pointlist_from_grid_wmask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Grid) :: myGrid
    integer :: lDE, localDECount
    integer :: clbndx(2),cubndx(2)
    integer :: clbndy(2),cubndy(2)
    integer :: i1,i2
    real(ESMF_KIND_R8), pointer :: coordX(:),coordY(:)
    integer :: petCount,localPet
    integer(ESMF_KIND_I4), pointer :: gMask(:,:)
    integer(ESMF_KIND_I4) :: maskValues(2)
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3


    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return



    ! Create Grid with globalXCountxglobalYCount cells
    myGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/10,20/), &
                                    coordSys=ESMF_COORDSYS_CART, &
                                    coordDep1 = (/1/), &
                                    coordDep2 = (/2/), &
                                    indexflag=ESMF_INDEX_GLOBAL,         &
                                    rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble creating grid'
       rc=ESMF_FAILURE
       return
    endif       

    ! Get number of local DEs
    call ESMF_GridGet(myGrid, localDECount=localDECount, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing localDECount from grid'
       rc=ESMF_FAILURE
       return
    endif       


    ! Allocate Center (e.g. Center) stagger
    call ESMF_GridAddCoord(myGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble adding coordinates to grid'
       rc=ESMF_FAILURE
       return
    endif       

    ! Allocate Masks
    call ESMF_GridAddItem(myGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
              itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
       print*,'ERROR:  trouble allocating mask info in grid'
       rc=ESMF_FAILURE
       return
    endif


    ! Loop through DEs and set Centers as the average of the corners
    do lDE=0,localDECount-1

      ! get and fill first coord array
      call ESMF_GridGetCoord(myGrid, localDE=lDE,  staggerloc=ESMF_STAGGERLOC_CENTER, &
                             coordDim=1, &
                             computationalLBound=clbndx, computationalUBound=cubndx, &
                             farrayPtr=coordX, &
                             rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         print*,'ERROR:  trouble accessing coordinates from grid'
         rc=ESMF_FAILURE
                 return
      endif     

      do i1=clbndx(1),cubndx(1)
        coordX(i1) = i1*10.0
      enddo     

      ! get and fill second coord array
      call ESMF_GridGetCoord(myGrid, localDE=lDE, staggerloc=ESMF_STAGGERLOC_CENTER, &
                             coordDim=2, &
                             computationalLBound=clbndy, computationalUBound=cubndy, &
                             farrayPtr=coordY, &
                             rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         print*,'ERROR:  trouble accessing coordinates from grid'
         rc=ESMF_FAILURE
                 return
      endif     

      do i2=clbndy(1),cubndy(1)
        coordY(i2) = i2*10.0
      enddo     

      call ESMF_GridGetItem(myGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, farrayPtr=gMask, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
        print*,'ERROR:  trouble accessing mask info from grid'
        rc=ESMF_FAILURE
        return
      endif

      local_pts=0
      do i1=clbndx(1),cubndx(1)
      do i2=clbndy(1),cubndy(1)
         if (i1 == i2) then
            gMask(i1,i2) = 2
         else   
            gMask(i1,i2) = 0
            local_pts=local_pts+1
            test_coordx=coordX(i1)
            test_coordy=coordY(i2)
         endif
      enddo
      enddo


    enddo

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    ! convert mask values
    maskValues=(/1,2/)

    pointlist=ESMF_PointListCreate(myGrid,ESMF_STAGGERLOC_CENTER, &
                                   maskValues=maskValues, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR: trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       

    !locations values are zero based
    call ESMF_PointListGetForLoc(pointlist,mypts-1,loc_coords=test_coords,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get for location routine'
       rc=ESMF_FAILURE
       return
    endif       

    my_err1 = abs(test_coordx - test_coords(1))
    my_err2 = abs(test_coordy - test_coords(2))
    if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001) then
      print*,'ERROR:  unexpected coordinates for queried pointlist location:'
      print*,'expected ( ',test_coordx,' , ',test_coordy,' )  got  (',test_coords(1),',',test_coords(2),')'
      rc=ESMF_FAILURE
      return
    endif


!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      

    call ESMF_GridDestroy(myGrid, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
       print*,'ERROR:  trouble destroying grid'
       rc=ESMF_FAILURE
       return
    endif

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble destroying pointlist'
       rc=ESMF_FAILURE
       return
    endif       


    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_grid_wmask




  subroutine test_pointlist_from_mesh_nodes_nomask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
    integer, allocatable :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    integer :: petCount,localPet
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source Mesh
    if (petCount .eq. 1) then
      ! Set number of nodes
      numNodes=9

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))

      nodeIds=(/1,2,3,4,5,6,7,8,9/)

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/0.0,0.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0,1.0, & ! node id 4
                   1.0,1.0, & ! node id 5
                   2.0,1.0, & ! node id 6
                   0.0,2.0, & ! node id 7
                   1.0,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on PET 0
      local_pts=9
      test_coordx=0.0
      test_coordy=0.0

      ! Set the number of each type of element, plus the total number.
      numQuadElems=3
      numTriElems=2
      numTotElems=numQuadElems+numTriElems


      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4,5/)

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


      ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the
      ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
      ! reflects that given in the Mesh options
      ! section for the corresponding entry
      ! in the elemTypes array. The number of
      ! entries in this elemConn array is the
      ! number of nodes in a quad. (4) times the
      ! number of quad. elements plus the number
      ! of nodes in a triangle (3) times the number
      ! of triangle elements.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &  ! elem id 1
                 2,3,5,   &  ! elem id 2
                 3,6,5,   &  ! elem id 3
                 4,5,8,7, &  ! elem id 4
                 5,6,9,8/)   ! elem id 5

    else if (petCount .eq. 4) then
      ! Setup mesh data depending on PET
      if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/1,2,4,5/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,0.0, & ! node id 1
                     1.0,0.0, & ! node id 2
                     0.0,1.0, & ! node id 4
                     1.0,1.0 /) ! node id 5

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 1
                     0, & ! node id 2
                     0, & ! node id 4
                     0/)  ! node id 5

        local_pts=4
        test_coordx=0.0
        test_coordy=0.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/1/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

        ! Allocate and fill the element connection type array.
        ! Note that entry are local indices
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 1

      else if (localPET .eq. 1) then !!! This part only for PET 1
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,3,5,6/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,0.0, & ! node id 2
                     2.0,0.0, & ! node id 3
                     1.0,1.0, & ! node id 5
                     2.0,1.0 /) ! node id 6

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     1, & ! node id 3
                     0, & ! node id 5
                     1/)  ! node id 6

        local_pts=2
        test_coordx=2.0
        test_coordy=0.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=0
        numTriElems=2
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/2,3/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                    ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,3, & ! elem id 2
                   2,4,3/)  ! elem id 3

      else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        local_pts=2
        test_coordx=0.0
        test_coordy=2.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

      else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9

        local_pts=1
        test_coordx=2.0
        test_coordy=2.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
      endif
    endif


    ! Create Mesh structure in 1 step
    myMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, &
         elementIds=elemIds, elementTypes=elemTypes, &
         elementConn=elemConn, rc=localrc)

    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR: trouble creating mesh'
      rc=ESMF_FAILURE
      return
    endif


    ! After the creation we are through with the arrays, so they may be
    ! deallocated.
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist=ESMF_PointListCreate(myMesh, ESMF_MESHLOC_NODE, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR: trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      

    !locations values are zero based
    call ESMF_PointListGetForLoc(pointlist,0,loc_coords=test_coords,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get for location routine'
       rc=ESMF_FAILURE
       return
    endif

    my_err1 = abs(test_coordx - test_coords(1))
    my_err2 = abs(test_coordy - test_coords(2))
    if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001) then
      print*,'ERROR:  unexpected coordinates for queried pointlist location:'
      print*,'expected ( ',test_coordx,' , ',test_coordy,' )  got  (',test_coords(1),',',test_coords(2),')'
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble destroying pointlist'
       rc=ESMF_FAILURE
       return
    endif       

    call ESMF_MeshDestroy(myMesh, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR: trouble destorying mesh'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_mesh_nodes_nomask



  subroutine test_pointlist_from_mesh_nodes_wmask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
    integer, allocatable :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    integer :: petCount,localPet
    integer, allocatable :: nodeMask(:)
    integer(ESMF_KIND_I4) :: maskValues(2)
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source Mesh
    if (petCount .eq. 1) then
      ! Set number of nodes
      numNodes=9

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))

      nodeIds=(/1,2,3,4,5,6,7,8,9/)

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/0.0,0.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0,1.0, & ! node id 4
                   1.0,1.0, & ! node id 5
                   2.0,1.0, & ! node id 6
                   0.0,2.0, & ! node id 7
                   1.0,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on PET 0

      ! Allocate and fill the node mask array.
      ! set masks on nodes 2,4,6,8
      allocate(nodeMask(numNodes))
      nodeMask=(/0,1,0,2,0,3,0,1,0/)

      local_pts=7
      test_coordx=0.0
      test_coordy=0.0

      ! Set the number of each type of element, plus the total number.
      numQuadElems=3
      numTriElems=2
      numTotElems=numQuadElems+numTriElems


      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4,5/)

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


      ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the
      ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
      ! reflects that given in the Mesh options
      ! section for the corresponding entry
      ! in the elemTypes array. The number of
      ! entries in this elemConn array is the
      ! number of nodes in a quad. (4) times the
      ! number of quad. elements plus the number
      ! of nodes in a triangle (3) times the number
      ! of triangle elements.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &  ! elem id 1
                 2,3,5,   &  ! elem id 2
                 3,6,5,   &  ! elem id 3
                 4,5,8,7, &  ! elem id 4
                 5,6,9,8/)   ! elem id 5


    else if (petCount .eq. 4) then
      ! Setup mesh data depending on PET
      if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/1,2,4,5/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,0.0, & ! node id 1
                     1.0,0.0, & ! node id 2
                     0.0,1.0, & ! node id 4
                     1.0,1.0 /) ! node id 5

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 1
                     0, & ! node id 2
                     0, & ! node id 4
                     0/)  ! node id 5

       ! Allocate and fill the node mask array.
       allocate(nodeMask(numNodes))
       nodeMask=(/0, & ! node id 1
                  1, & ! node id 2
                  2, & ! node id 4
                  0/)  ! node id 5

        local_pts=3
        test_coordx=0.0
        test_coordy=0.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/1/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

        ! Allocate and fill the element connection type array.
        ! Note that entry are local indices
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 1

      else if (localPET .eq. 1) then !!! This part only for PET 1
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,3,5,6/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,0.0, & ! node id 2
                     2.0,0.0, & ! node id 3
                     1.0,1.0, & ! node id 5
                     2.0,1.0 /) ! node id 6

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     1, & ! node id 3
                     0, & ! node id 5
                     1/)  ! node id 6

        ! Allocate and fill the node mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/1, & ! node id 2
                   0, & ! node id 3
                   0, & ! node id 5
                   3/)  ! node id 6

        local_pts=1
        test_coordx=2.0
        test_coordy=0.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=0
        numTriElems=2
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/2,3/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                    ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,3, & ! elem id 2
                   2,4,3/)  ! elem id 3

      else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Allocate and fill the node mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/2, & ! node id 4
                   0, & ! node id 5
                   0, & ! node id 7
                   1/)  ! node id 8

        local_pts=2
        test_coordx=0.0
        test_coordy=2.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9

        ! Allocate and fill the node mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/0, & ! node id 5
                   3, & ! node id 6
                   1, & ! node id 8
                   0/)  ! node id 9

        local_pts=1
        test_coordx=2.0
        test_coordy=2.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
      endif
    endif


    ! Create Mesh structure in 1 step
    myMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                           coordSys=ESMF_COORDSYS_CART, &
                           nodeIds=nodeIds, nodeCoords=nodeCoords, &
                           nodeOwners=nodeOwners, nodeMask=nodeMask, &
                           elementIds=elemIds, elementTypes=elemTypes, &
                           elementConn=elemConn, rc=localrc)

    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating mesh'
      rc=ESMF_FAILURE
      return
    endif


    ! After the creation we are through with the arrays, so they may be
    ! deallocated.
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(nodeMask)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

    ! convert mask values
    maskValues=(/2,3/)

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist=ESMF_PointListCreate(myMesh, ESMF_MESHLOC_NODE, maskValues=maskValues, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       



!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      


    !locations values are zero based
    call ESMF_PointListGetForLoc(pointlist,0,loc_coords=test_coords,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get for location routine'
       rc=ESMF_FAILURE
       return
    endif

    my_err1 = abs(test_coordx - test_coords(1))
    my_err2 = abs(test_coordy - test_coords(2))
    if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001) then
      print*,'ERROR:  unexpected coordinates for queried pointlist location:'
      print*,'expected ( ',test_coordx,' , ',test_coordy,' )  got  (',test_coords(1),',',test_coords(2),')'
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble destorying pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_MeshDestroy(myMesh, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destorying mesh'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_mesh_nodes_wmask



  subroutine test_pointlist_from_mesh_elems_nomask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
    integer, allocatable :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    real(ESMF_KIND_R8), allocatable :: elemCoords(:)
    integer :: petCount,localPet
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3


    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source Mesh
    if (petCount .eq. 1) then
      ! Set number of nodes
      numNodes=9

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))

      nodeIds=(/1,2,3,4,5,6,7,8,9/)

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/0.0,0.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0,1.0, & ! node id 4
                   1.0,1.0, & ! node id 5
                   2.0,1.0, & ! node id 6
                   0.0,2.0, & ! node id 7
                   1.0,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on PET 0

      ! Set the number of each type of element, plus the total number.
      numQuadElems=3
      numTriElems=2
      numTotElems=numQuadElems+numTriElems


      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4,5/)


      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


      ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the
      ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
      ! reflects that given in the Mesh options
      ! section for the corresponding entry
      ! in the elemTypes array. The number of
      ! entries in this elemConn array is the
      ! number of nodes in a quad. (4) times the
      ! number of quad. elements plus the number
      ! of nodes in a triangle (3) times the number
      ! of triangle elements.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &  ! elem id 1
                 2,3,5,   &  ! elem id 2
                 3,6,5,   &  ! elem id 3
                 4,5,8,7, &  ! elem id 4
                 5,6,9,8/)   ! elem id 5

      !! elem coords
      allocate(elemCoords(2*numTotElems))
      elemCoords=(/0.5,0.5, & ! 1
                   1.1,0.1, & ! 2
                   1.9,0.9, & ! 3
                   0.5,1.5, & ! 4
                   1.5,1.5/)  ! 5

      local_pts=5
      test_coordx=0.5
      test_coordy=0.5

    else if (petCount .eq. 4) then
      ! Setup mesh data depending on PET
      if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/1,2,4,5/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,0.0, & ! node id 1
                     1.0,0.0, & ! node id 2
                     0.0,1.0, & ! node id 4
                     1.0,1.0 /) ! node id 5

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 1
                     0, & ! node id 2
                     0, & ! node id 4
                     0/)  ! node id 5

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/1/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

        ! Allocate and fill the element connection type array.
        ! Note that entry are local indices
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 1

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/0.5,0.5/)  ! 1

        local_pts=1
        test_coordx=0.5
        test_coordy=0.5

      else if (localPET .eq. 1) then !!! This part only for PET 1
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,3,5,6/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,0.0, & ! node id 2
                     2.0,0.0, & ! node id 3
                     1.0,1.0, & ! node id 5
                     2.0,1.0 /) ! node id 6

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     1, & ! node id 3
                     0, & ! node id 5
                     1/)  ! node id 6

        ! Set the number of each type of element, plus the total number.
        numQuadElems=0
        numTriElems=2
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/2,3/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                    ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,3, & ! elem id 2
                   2,4,3/)  ! elem id 3

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/1.1,0.1, & ! 2
                     1.9,0.9/)  ! 3

        local_pts=2
        test_coordx=1.1
        test_coordy=0.1

     else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/0.5,1.5/)  ! 4

        local_pts=1
        test_coordx=0.5
        test_coordy=1.5

      else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/1.5,1.5/)  ! 5

        local_pts=1
        test_coordx=1.5
        test_coordy=1.5

      endif
    endif

    ! Create Mesh structure in 1 step
    myMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords, &
         rc=localrc)

    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating mesh'
      rc=ESMF_FAILURE
      return
    endif

    ! After the creation we are through with the arrays, so they may be
    ! deallocated.
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elemCoords)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist=ESMF_PointListCreate(myMesh, ESMF_MESHLOC_ELEMENT, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif

    !locations values are zero based
    call ESMF_PointListGetForLoc(pointlist,0,loc_coords=test_coords,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR:  trouble accessing pointlist data with get for location routine'
       rc=ESMF_FAILURE
       return
    endif

    my_err1 = abs(test_coordx - test_coords(1))
    my_err2 = abs(test_coordy - test_coords(2))
    if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001) then
      print*,'ERROR:  unexpected coordinates for queried pointlist location:'
      print*,'expected ( ',test_coordx,' , ',test_coordy,' )  got  (',test_coords(1),',',test_coords(2),')'
      rc=ESMF_FAILURE
      return
    endif

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_MeshDestroy(myMesh, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying mesh'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_mesh_elems_nomask


  subroutine test_pointlist_from_mesh_elems_wmask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
    integer, allocatable :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    real(ESMF_KIND_R8), allocatable :: elemCoords(:)
    integer :: petCount,localPet
    integer, allocatable :: elemMask(:)
    integer(ESMF_KIND_I4) :: maskValues(2)
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source Mesh
    if (petCount .eq. 1) then
      ! Set number of nodes
      numNodes=9

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))

      nodeIds=(/1,2,3,4,5,6,7,8,9/)

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/0.0,0.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0,1.0, & ! node id 4
                   1.0,1.0, & ! node id 5
                   2.0,1.0, & ! node id 6
                   0.0,2.0, & ! node id 7
                   1.0,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on PET 0

      ! Set the number of each type of element, plus the total number.
      numQuadElems=3
      numTriElems=2
      numTotElems=numQuadElems+numTriElems


      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4,5/)

      ! Allocate and fill the element mask array.
      ! set masks on elements 1,3,5
      allocate(elemMask(numTotElems))
      elemMask=(/1,0,2,0,3/)

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


      ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the
      ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
      ! reflects that given in the Mesh options
      ! section for the corresponding entry
      ! in the elemTypes array. The number of
      ! entries in this elemConn array is the
      ! number of nodes in a quad. (4) times the
      ! number of quad. elements plus the number
      ! of nodes in a triangle (3) times the number
      ! of triangle elements.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &  ! elem id 1
                 2,3,5,   &  ! elem id 2
                 3,6,5,   &  ! elem id 3
                 4,5,8,7, &  ! elem id 4
                 5,6,9,8/)   ! elem id 5

      !! elem coords
      allocate(elemCoords(2*numTotElems))
      elemCoords=(/0.5,0.5, & ! 1
                   1.1,0.1, & ! 2
                   1.9,0.9, & ! 3
                   0.5,1.5, & ! 4
                   1.5,1.5/)  ! 5

      local_pts=3
      test_coordx=1.1
      test_coordy=0.1

    else if (petCount .eq. 4) then
      ! Setup mesh data depending on PET
      if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/1,2,4,5/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,0.0, & ! node id 1
                     1.0,0.0, & ! node id 2
                     0.0,1.0, & ! node id 4
                     1.0,1.0 /) ! node id 5

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 1
                     0, & ! node id 2
                     0, & ! node id 4
                     0/)  ! node id 5

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/1/)

        ! Allocate and fill the element mask array.
        ! set masks on elements 1,3,5
        allocate(elemMask(numTotElems))
        elemMask=(/1/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

        ! Allocate and fill the element connection type array.
        ! Note that entry are local indices
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 1

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/0.5,0.5/)  ! 1

        local_pts=0
        test_coordx=0.5
        test_coordy=0.5

      else if (localPET .eq. 1) then !!! This part only for PET 1
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,3,5,6/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,0.0, & ! node id 2
                     2.0,0.0, & ! node id 3
                     1.0,1.0, & ! node id 5
                     2.0,1.0 /) ! node id 6

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     1, & ! node id 3
                     0, & ! node id 5
                     1/)  ! node id 6

        ! Set the number of each type of element, plus the total number.
        numQuadElems=0
        numTriElems=2
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/2,3/)

        ! Allocate and fill the element mask array.
        ! set masks on elements 1,3,5
        allocate(elemMask(numTotElems))
        elemMask=(/0,2/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                    ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,3, & ! elem id 2
                   2,4,3/)  ! elem id 3

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/1.1,0.1, & ! 2
                     1.9,0.9/)  ! 3

        local_pts=1
        test_coordx=1.1
        test_coordy=0.1

     else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

                     ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/)

        ! Allocate and fill the element mask array.
        ! set masks on elements 1,3,5
        allocate(elemMask(numTotElems))
        elemMask=(/0/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/0.5,1.5/)  ! 4

        local_pts=1
        test_coordx=0.5
        test_coordy=1.5

      else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9

                     ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)

        ! Allocate and fill the element mask array.
        ! set masks on elements 1,3,5
        allocate(elemMask(numTotElems))
        elemMask=(/3/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5

        !! elem coords
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/1.5,1.5/)  ! 5

        local_pts=1
        test_coordx=1.5
        test_coordy=1.5

      endif
    endif



    ! Create Mesh structure in 1 step
    myMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementMask=elemMask, elementCoords=elemCoords, &
         rc=localrc)

    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating mesh'
      rc=ESMF_FAILURE
      return
    endif

    ! After the creation we are through with the arrays, so they may be
    ! deallocated.
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elemMask)
    deallocate(elemCoords)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

    ! convert mask values
    maskValues=(/1,2/)

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist=ESMF_PointListCreate(myMesh, ESMF_MESHLOC_ELEMENT, &
                                   maskValues=maskValues, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif


!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif

    if (local_pts .gt. 0) then

      !locations values are zero based
      call ESMF_PointListGetForLoc(pointlist,0,loc_coords=test_coords,rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        print*,'ERROR:  trouble accessing pointlist data with get for location routine'
        rc=ESMF_FAILURE
        return
      endif

      my_err1 = abs(test_coordx - test_coords(1))
      my_err2 = abs(test_coordy - test_coords(2))
      if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001) then
        print*,'ERROR:  unexpected coordinates for queried pointlist location:'
        print*,'expected ( ',test_coordx,' , ',test_coordy,' )  got  (',test_coords(1),',',test_coords(2),')'
        rc=ESMF_FAILURE
        return
      endif


    endif

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_MeshDestroy(myMesh, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying mesh'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_mesh_elems_wmask


  subroutine test_pointlist_from_locStream_nomask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
    integer, pointer :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), pointer :: nodeCoords(:)
    real(ESMF_KIND_R8), pointer :: elemCoords(:)
    integer :: petCount,localPet
    integer, pointer :: elemMask(:)
    integer(ESMF_KIND_I4) :: maskValues(2)
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(3) :: test_coords
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3

    real(ESMF_KIND_R8), pointer :: temperature(:)
    real(ESMF_KIND_R8), allocatable :: lat(:),lon(:)
    integer :: i
    type(ESMF_LocStream) :: myLocStream
    type(ESMF_Field) :: field_temperature

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source locStream
    local_pts=6

    !-------------------------------------------------------------------
    ! Allocate and set example Field data
    !-------------------------------------------------------------------
    allocate(temperature(local_pts))
    allocate(lat(local_pts))
    allocate(lon(local_pts))

    do i=1,local_pts
      temperature(i)=80.0+i
      lon(i)=(i-1)*360.0/local_pts
      lat(i)=0.0
    enddo

    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    myLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=local_pts, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating locStream'
      rc=ESMF_FAILURE
      return
    endif


    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(myLocStream,                  &
                              keyName="ESMF:Lat",           &
                              farray=lat,                   &
                              datacopyflag=ESMF_DATACOPY_REFERENCE, &
                              keyUnits="Degrees",           &
                              keyLongName="Latitude", rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble adding LocStream key for Lat'
      rc=ESMF_FAILURE
      return
    endif
    call ESMF_LocStreamAddKey(myLocStream,                  &
                              keyName="ESMF:Lon",           &
                              farray=lon,                   &
                              datacopyflag=ESMF_DATACOPY_REFERENCE, &
                              keyUnits="Degrees",           &
                              keyLongName="Longitude", rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble adding LocStream key for Lon'
      rc=ESMF_FAILURE
      return
    endif


    !-------------------------------------------------------------------
    ! Create a Field on the Location Stream. In this case the
    ! Field is created from a user array, but any of the other
    ! Field create methods (e.g. from ArraySpec) would also apply.
    !-------------------------------------------------------------------
    field_temperature=ESMF_FieldCreate(myLocStream,   &
                                       temperature, &
                                       name="temperature", &
                                       rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating field on locStream'
      rc=ESMF_FAILURE
      return
    endif


    pointlist=ESMF_PointListCreate(myLocStream, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      


    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 3) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif


    if (local_pts .gt. 0) then

      !locations values are zero based
      call ESMF_PointListGetForLoc(pointlist,1,loc_coords=test_coords,rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        print*,'ERROR:  trouble accessing pointlist data with get for location routine'
        rc=ESMF_FAILURE
        return
      endif

      my_err1 = abs(test_coords(1) - 0.5)
      my_err2 = abs(test_coords(2) - 0.866)
      my_err3 = abs(test_coords(3) - 0.0)
      if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001 .or. my_err3 .gt. .0001) then
        print*,'ERROR:  unexpected coordinates for queried pointlist location:'
        print*,'expected ( 0.5 , 0.866 , 0 )  got  (',test_coords(1),',',test_coords(2),',',test_coords(3),')'
        rc=ESMF_FAILURE
        return
      endif
    endif

    deallocate(temperature)
    deallocate(lon)
    deallocate(lat)

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_LocStreamDestroy(myLocStream, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying LocStream'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_locStream_nomask


  subroutine test_pointlist_from_locStream_wmask(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
    integer, pointer :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), pointer :: nodeCoords(:)
    real(ESMF_KIND_R8), pointer :: elemCoords(:)
    integer :: petCount,localPet
    real(ESMF_KIND_R8), dimension(3) :: test_coords
    real(ESMF_KIND_R8) my_err1,my_err2,my_err3

    real(ESMF_KIND_R8), pointer :: lon(:),lat(:)
    real(ESMF_KIND_R8), pointer :: temperature(:)
    integer(ESMF_KIND_I4) :: maskValues(2)
    integer(ESMF_KIND_I4), pointer :: maskArray(:)
    integer :: local_pts,i
    type(ESMF_LocStream) :: myLocStream
    type(ESMF_Field) :: field_temperature

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source locStream
    local_pts=6

    !-------------------------------------------------------------------
    ! Allocate and set example Field data
    !-------------------------------------------------------------------
    allocate(temperature(local_pts))

    do i=1,local_pts
      temperature(i)=80.0+i
    enddo


    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    myLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=local_pts, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating locStream'
      rc=ESMF_FAILURE
      return
    endif


    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(myLocStream,                  &
                              keyName="ESMF:Lat",           &
                              KeyTypeKind=ESMF_TYPEKIND_R8, &
                              keyUnits="Degrees",           &
                              keyLongName="Latitude", rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble adding LocStream key for Lat'
      rc=ESMF_FAILURE
      return
    endif
    call ESMF_LocStreamAddKey(myLocStream,                  &
                              keyName="ESMF:Lon",           &
                              KeyTypeKind=ESMF_TYPEKIND_R8, &
                              keyUnits="Degrees",           &
                              keyLongName="Longitude", rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble adding LocStream key for Lon'
      rc=ESMF_FAILURE
      return
    endif
    call ESMF_LocStreamAddKey(myLocStream,                    &
                              keyName="ESMF:Mask",                &
                              KeyTypeKind=ESMF_TYPEKIND_I4, &
                              keyUnits="none",           &
                              keyLongName="mask values", rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble adding LocStream key for Mask'
      rc=ESMF_FAILURE
      return
    endif
    !-------------------------------------------------------------------
    ! Get key data.
    !-------------------------------------------------------------------
    call ESMF_LocStreamGetKey(myLocStream,                  &
                              localDE=0,                    &
                              keyName="ESMF:Lat",           &
                              farray=lat,                   &
                              rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble getting LocStream key for Lat'
      rc=ESMF_FAILURE
      return
    endif
    call ESMF_LocStreamGetKey(myLocStream,                  &
                              localDE=0,                    &
                              keyName="ESMF:Lon",           &
                              farray=lon,                   &
                              rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble getting LocStream key for Lon'
      rc=ESMF_FAILURE
      return
    endif
    call ESMF_LocStreamGetKey(myLocStream,                    &
                              localDE=0,                    &
                              keyName="ESMF:Mask",                &
                              farray=maskArray,                   &
                              rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble getting LocStream key for Lon'
      rc=ESMF_FAILURE
      return
    endif
    !-------------------------------------------------------------------
    ! Set key data.
    !-------------------------------------------------------------------
    do i=1,local_pts
       lon(i)=(i-1)*360.0/local_pts
       lat(i)=0.0
    enddo

    maskArray(1)=0
    maskArray(2)=1
    maskArray(3)=2
    maskArray(4)=2
    maskArray(5)=1
    maskArray(6)=0

    !-------------------------------------------------------------------
    ! Create a Field on the Location Stream. In this case the
    ! Field is created from a user array, but any of the other
    ! Field create methods (e.g. from ArraySpec) would also apply.
    !-------------------------------------------------------------------
    field_temperature=ESMF_FieldCreate(myLocStream,   &
                                       temperature, &
                                       name="temperature", &
                                       rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating field on locStream'
      rc=ESMF_FAILURE
      return
    endif

    maskValues=(/0,2/)
    pointlist=ESMF_PointListCreate(myLocStream, maskValues=maskValues, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif

    if (maxpts .ne. 2 .or. mypts .ne. 2 .or. mydims .ne. 3) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      


    if (local_pts .gt. 0) then

      !locations values are zero based
      call ESMF_PointListGetForLoc(pointlist,1,loc_coords=test_coords,rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        print*,'ERROR:  trouble accessing pointlist data with get for location routine'
        rc=ESMF_FAILURE
        return
      endif

      my_err1 = abs(test_coords(1) - (-0.5))
      my_err2 = abs(test_coords(2) - (-0.866))
      my_err3 = abs(test_coords(3) - (-0.0))
      if (my_err1 .gt. .0001 .or. my_err2 .gt. .0001 .or. my_err3 .gt. .0001) then
        print*,'ERROR:  unexpected coordinates for queried pointlist location:'
        print*,'expected ( 0.5 , 0.866 , 0 )  got  (',test_coords(1),',',test_coords(2),',',test_coords(3),')'
        rc=ESMF_FAILURE
        return
      endif

    endif

    deallocate(temperature)

    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying pointlist'
      rc=ESMF_FAILURE
      return
    endif       


    call ESMF_LocStreamDestroy(myLocStream, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying LocStream'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_locStream_wmask

  subroutine test_pointlist_from_mesh_nodes_nomask_empty_proc(rc)
    integer, intent(out) :: rc

    integer :: localrc

    !LOCAL VARIABLES:
    type(ESMF_PointList) :: pointlist
    type(ESMF_VM) :: vm
    integer :: maxpts, mydims, mypts, myid
    type(ESMF_Mesh) :: myMesh
    integer :: numNodes
    integer :: numTriElems, numQuadElems, numTotElems
    integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
    integer, allocatable :: nodeIds(:), nodeOwners(:)
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
    integer :: petCount,localPet
    integer :: local_pts
    real(ESMF_KIND_R8), dimension(2) :: test_coords
    real(ESMF_KIND_R8) test_coordx,test_coordy

    ! get global VM
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! If we don't have 1 or 4 PETS then exit successfully
    if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
      print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
      rc=ESMF_FAILURE
      return
    endif

    ! setup source Mesh
    if (petCount .eq. 1) then
      ! Set number of nodes
      numNodes=9

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))

      nodeIds=(/1,2,3,4,5,6,7,8,9/)

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/0.0,0.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0,1.0, & ! node id 4
                   1.0,1.0, & ! node id 5
                   2.0,1.0, & ! node id 6
                   0.0,2.0, & ! node id 7
                   1.0,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on PET 0
      local_pts=9
      test_coordx=0.0
      test_coordy=0.0

      ! Set the number of each type of element, plus the total number.
      numQuadElems=3
      numTriElems=2
      numTotElems=numQuadElems+numTriElems


      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4,5/)

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


      ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the
      ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
      ! reflects that given in the Mesh options
      ! section for the corresponding entry
      ! in the elemTypes array. The number of
      ! entries in this elemConn array is the
      ! number of nodes in a quad. (4) times the
      ! number of quad. elements plus the number
      ! of nodes in a triangle (3) times the number
      ! of triangle elements.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &  ! elem id 1
                 2,3,5,   &  ! elem id 2
                 3,6,5,   &  ! elem id 3
                 4,5,8,7, &  ! elem id 4
                 5,6,9,8/)   ! elem id 5

    else if (petCount .eq. 4) then
      ! Setup mesh data depending on PET
      if (localPET .eq. 0) then !!! This part only for PET 0

        ! Set number of nodes
        numNodes=9

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))

        nodeIds=(/1,2,3,4,5,6,7,8,9/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,0.0, & ! node id 1
                     1.0,0.0, & ! node id 2
                     2.0,0.0, & ! node id 3
                     0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     2.0,1.0, & ! node id 6
                     0.0,2.0, & ! node id 7
                     1.0,2.0, & ! node id 8
                     2.0,2.0 /) ! node id 9

        allocate(nodeOwners(numNodes))
        nodeOwners=0 ! everything on PET 0
        local_pts=9
        test_coordx=0.0
        test_coordy=0.0

        ! Set the number of each type of element, plus the total number.
        numQuadElems=3
        numTriElems=2
        numTotElems=numQuadElems+numTriElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/1,2,3,4,5/)

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                    ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                    ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                    ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                    ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


        ! Allocate and fill the element connection type array.
        ! Note that entries in this array refer to the
        ! positions in the nodeIds, etc. arrays and that
        ! the order and number of entries for each element
        ! reflects that given in the Mesh options
        ! section for the corresponding entry
        ! in the elemTypes array. The number of
        ! entries in this elemConn array is the
        ! number of nodes in a quad. (4) times the
        ! number of quad. elements plus the number
        ! of nodes in a triangle (3) times the number
        ! of triangle elements.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,5,4, &  ! elem id 1
                   2,3,5,   &  ! elem id 2
                   3,6,5,   &  ! elem id 3
                   4,5,8,7, &  ! elem id 4
                   5,6,9,8/)   ! elem id 5


      else if (localPET .gt. 0) then


        ! Set number of nodes
        numNodes=0

        local_pts=0

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))

        ! Set the number of each type of element, plus the total number.
         numQuadElems=0
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
      endif
    endif


    ! Create Mesh structure in 1 step
    myMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, &
         elementIds=elemIds, elementTypes=elemTypes, &
         elementConn=elemConn, rc=localrc)


    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR: trouble creating mesh'
      rc=ESMF_FAILURE
      return
    endif

    ! After the creation we are through with the arrays, so they may be
    ! deallocated.
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

    maxpts=-99
    mypts=-99
    mydims=-99
    myid=-99

    pointlist=ESMF_PointListCreate(myMesh, ESMF_MESHLOC_NODE, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      print*,'ERROR: trouble creating pointlist'
      rc=ESMF_FAILURE
      return
    endif       

    call ESMF_PointListGet(pointlist, dims=mydims, numpts=mypts, maxpts=maxpts, rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble accessing pointlist data with get routine'
       rc=ESMF_FAILURE
       return
    endif       

    if (maxpts .ne. local_pts .or. mypts .ne. local_pts .or. mydims .ne. 2) then
       print*,'ERROR:  unexpected values for newly created pointlist:'
       print*,'maxpts should be: ',local_pts,' got: ',maxpts
       print*,'numpts should be: ',local_pts,' got: ',mypts
       print*,'dims should be: 2  got: ',mydims
       rc=ESMF_FAILURE
       return
    endif       

!    call ESMF_PointListPrint(pointlist)
!    if (localrc /= ESMF_SUCCESS) then
!       rc=ESMF_FAILURE
!       return
!    endif      


    call ESMF_PointListDestroy(pointlist,rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
       print*,'ERROR: trouble destroying pointlist'
       rc=ESMF_FAILURE
       return
    endif       

    call ESMF_MeshDestroy(myMesh, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR: trouble destorying mesh'
      rc=ESMF_FAILURE
      return
    endif

    rc=ESMF_SUCCESS
  end subroutine test_pointlist_from_mesh_nodes_nomask_empty_proc


end program ESMF_PointListUTest
