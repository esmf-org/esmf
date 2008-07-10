!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessMod"
!
!  ESMF Test Harness Module
   module ESMF_TestHarnessMod
!
!===============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessMod
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the
! {\tt ESMF\_TestHarness}.  
!
!-------------------------------------------------------------------------------
! !USES:

  use ESMF_Mod
  use ESMF_TestHarnessTypesMod

  implicit none

!===============================================================================

  contains 

!===============================================================================


 !------------------------------------------------------------------------------
 ! Routines to parse input files for descriptor string, and specifier files.
 !------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine array_redist_test(PDS, VM, rc)
  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(in   ) :: PDS
  type(ESMF_VM), intent(in   ) :: VM
  integer, intent(  out) :: rc

! local parameters
  integer :: localrc ! local error status

  ! local
  type(ESMF_DistGrid) :: src_distgrid, dst_distgrid
  type(ESMF_Array) :: src_array, return_array, dst_array
  integer :: iDfile, iGfile, iD, iG
  character(ESMF_MAXSTR) :: liG, liD


  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! assign needed local variables
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! for a single problem descriptor string, loop through each specifier file
  ! combination
  !-----------------------------------------------------------------------------
  print*,'                                 '
  print*,'-----------------======array redist test==========-----------------------'
  print*,'PDS%nDfiles  PDS%nGfiles  PDS%Dfiles%nDspecs  PDS%Gfiles%nGspecs ', &
     PDS%nDfiles, PDS%nGfiles, PDS%Dfiles%nDspecs, PDS%Gfiles%nGspecs


  do iDfile=1,PDS%nDfiles         ! distribution specifier files
    do iGfile=1,PDS%nGfiles       ! grid specifier files
      do iD=1, PDS%Dfiles(iDfile)%nDspecs   ! entries in distribution specifier
        do iG=1, PDS%Gfiles(iGfile)%nGspecs ! entries in grid specifier file

     print*,'                                 '
     print*,'-----------------=====create objects===========-----------------------'
     print*,' iG, iD, iGfile, iDfile ',iG,iD,iGfile,iDfile
     print*,'                                 '
          !---------------------------------------------------------------------
          ! Create Source objects
          !---------------------------------------------------------------------
        print*, &
         ' Source grid/dist/memory rank ', &
          pds%SrcMem%GridRank,pds%SrcMem%DistRank,pds%SrcMem%memRank
        print*, &
         ' Destination grid/dist/memory rank ', &
          pds%DstMem%GridRank,pds%DstMem%DistRank,pds%DstMem%memRank

          ! create source and destination distributions
          print*,'> create source distribution '
          call create_distribution(PDS%SrcMem, PDS%Dfiles(iDfile)%src_dist(iD),&
                    PDS%Gfiles(iGfile)%src_grid(iG), src_distgrid, VM, localrc)

          print*,'               '

          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 
          if (ESMF_LogMsgFoundError(localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          print*,'> create src_array '
          call create_array(PDS%SrcMem, src_distgrid,                          &
                      PDS%Gfiles(iGfile)%src_grid(iG), src_array, localrc)
          if (ESMF_LogMsgFoundError(localrc,"error creating source array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          print*,'enter create return_array '
          call create_array(PDS%SrcMem, src_distgrid,                          &
                      PDS%Gfiles(iGfile)%src_grid(iG), return_array, localrc)
          if (ESMF_LogMsgFoundError(localrc,"error creating return array "     &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------
          ! Create Destination objects
          !---------------------------------------------------------------------
          print*,'> create destination distribution'
          call create_distribution(PDS%DstMem, PDS%Dfiles(iDfile)%dst_dist(iD),&
                    PDS%Gfiles(iGfile)%dst_grid(iG), dst_distgrid, VM, localrc)
          print*,'               '

          write(liG,"(i5)") iG 
          write(liD,"(i5)") iD 
          if (ESMF_LogMsgFoundError(localrc,"error creating source distgrid "  &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

          print*,'enter create dst_array'
          call create_array(PDS%DstMem, Dst_distgrid,                          &
                      PDS%Gfiles(iGfile)%dst_grid(iG), dst_array, localrc)
          if (ESMF_LogMsgFoundError(localrc,"error creating destinationarray " &
             // " with string "  // trim(adjustL(PDS%pds)) //                  &
             " with entry "  // trim(adjustL(liD)) // " of file " //           &
             trim(adjustL(PDS%Dfiles(iDfile)%filename))                        &
             // " and entry " // trim(adjustL(liG)) // " of file " //          &
             trim(adjustL(PDS%Gfiles(iGfile)%filename)),                       &
             rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! create source and destination distributions (in DistMod)

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! create source array, destination array, and second source array

  ! populate source array  (in grid)

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! run redistribution
  ! redistribution store
  ! redistribution  run

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! drs!!!! need iD and iG argument for test_status
  ! test agreement between source and returned source array contents
  ! PDS%test_status(iDfile,iGfile) = HarnessTest_SUCCESS _FAILURE


          !---------------------------------------------------------------------
          ! Clean up!!!!!!
          !---------------------------------------------------------------------
          !---------------------------------------------------------------------
          ! Destroy Array objects before moving to next test
          !---------------------------------------------------------------------
          print*,' destroy arrays '
          call ESMF_ArrayDestroy(src_array, rc=localrc) ! original source
          if (ESMF_LogMsgFoundError(localrc,"unable to destroy src_array",     &
             rcToReturn=rc)) return

          call ESMF_ArrayDestroy(dst_array, rc=localrc) ! redistribution 
          if (ESMF_LogMsgFoundError(localrc,"unable to destroy dst_array   ",  &
             rcToReturn=rc)) return

          call ESMF_ArrayDestroy(return_array, rc=localrc) ! return to source
          if (ESMF_LogMsgFoundError(localrc,"unable to destroy return_array",  &
             rcToReturn=rc)) return
          !---------------------------------------------------------------------
          ! Destroy DistGrid objects before running next test
          !---------------------------------------------------------------------
          print*,' destroy distgrids '
          call ESMF_DistGridDestroy(src_distgrid, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return

          call ESMF_DistGridDestroy(dst_distgrid, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc,"unable to destroy src_distgrid",  &
             rcToReturn=rc)) return

          !---------------------------------------------------------------------

        enddo  ! iG
      enddo  ! iD
    enddo  ! iGfile
  enddo   ! iDfile
  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine array_redist_test
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine create_distribution(Memory, DistRecord, GridRecord, DistGrid,VM,rc)
  !-----------------------------------------------------------------------------
  ! routine creates a single distribution from specifier files
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(memory_config), intent(in   ) :: Memory
  type(dist_specification_record), intent(in   ) :: DistRecord
  type(grid_specification_record), intent(in   ) :: GridRecord
  type(ESMF_DistGrid), intent(  out) :: DistGrid
  type(ESMF_VM), intent(in   ) :: VM
  integer, intent(inout) :: rc

  ! local parameters
  integer :: localrc ! local error status

  ! local integer variables
  integer :: k, nconnect
  integer, allocatable :: BIndx(:), EIndx(:)
  integer, allocatable :: decompOrder(:)
  type(ESMF_DecompFlag), allocatable :: decompType(:)
  integer, allocatable :: connectionList(:,:), repetitionVector(:) 
  integer, allocatable :: positionVector(:),orientationVector(:)

  ! local logicals
  logical :: noconnections

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! allocate input arrays with size of the grid rank - since dist rank is now
  ! equal to the size of Grid Rank, with any dist missing dimensions set to one.
  !-----------------------------------------------------------------------------
  allocate( BIndx(Memory%GridRank), EIndx(Memory%GridRank) )
  allocate( decompOrder(Memory%GridRank), decompType(Memory%GridRank) )
  allocate( connectionList(3*Memory%GridRank+2,1) )

  !-----------------------------------------------------------------------------
  ! fill input arrays:
  ! EIndx - filled with the grid sizes as specified by the grid specifier
  !         files, but in the order indicated by the problem descriptor strings
  ! decompOrder - filled with distribution sizes as specified by the dist
  !           specifier files, but in the order indicated by the PDStrings
  ! decompType - set to either ESMF_DECOMP_DEFAULT or ESMF_DECOMP_CYCLIC
  !           depending on how its indicated in the problem descriptor string
  !-----------------------------------------------------------------------------
  nconnect = 0           ! assume number of connections is zero

! print*,' memory rank ',Memory%memRank
! print*,' grid rank',Memory%GridRank
  ! fill the array with gridRank number of elements 
  do k=1,Memory%GridRank
    BIndx(k)   = 1
    EIndx(k) = GridRecord%gsize( Memory%GridOrder(k) )
  enddo  ! k

  ! if there are additional memory elements fill them with what is left over
! if( Memory%memRank > Memory%GridRank ) then
!   do k=Memory%GridRank+1,Memory%memRank
!     EIndx(k) = GridRecord%gsize( Memory%GridOrder(k) )
!   enddo  ! k
! endif

  ! pad the distribution with ones until its the same rank as the grid
  do k=1,Memory%DistRank
    decompOrder(k) = DistRecord%dsize( Memory%DistOrder(k) )
    decompType(k)  = ESMF_DECOMP_DEFAULT
  enddo   ! k

  do k=Memory%DistRank+1, Memory%GridRank
    decompOrder(k) = 1
    decompType(k)  = ESMF_DECOMP_DEFAULT
  enddo   ! k

  do k=1, Memory%DistRank
    !  assume the decomposition type is block unless block-cyclic is specified
    if( trim(adjustL(Memory%DistType(k)%string)) == "C" )  then
      decompType(k) = ESMF_DECOMP_CYCLIC
    endif

    ! look for periodic boundary conditions specified in the grid specifier file
    if( pattern_query(GridRecord%gtype(Memory%GridOrder(k))%string,            &
      "_periodic") /= 0 .or. pattern_query(                                    &
      GridRecord%gtype(Memory%GridOrder(k))%string,"_PERIODIC") /= 0)  then
      nconnect = nconnect + 1
    endif
  enddo  ! k

! print*,' mem rank ',Memory%memRank
! do k=1,Memory%GridRank
!   print*,k,' order/size ',Memory%GridOrder(k),decompOrder(k),EIndx(k)
! enddo
! print*,'        '
! print*,'record size dist ',DistRecord%dsize(1),DistRecord%dsize(2)
  !-----------------------------------------------------------------------------
  ! check for a connected domain - set connection call arguments
  !-----------------------------------------------------------------------------
  if( nconnect == 1 ) then
    ! singlely periodic domain
    noconnections = .FALSE. 
    ! workspace
    allocate( repetitionVector(Memory%memRank) )
    allocate( positionVector(Memory%memRank),orientationVector(Memory%memRank) )

    do k=1, Memory%GridRank
      positionVector(k) = 0
      orientationVector(k) = k
      repetitionVector(k) = 0
      if( pattern_query(GridRecord%gtype(Memory%GridOrder(k))%string,          &
        "_periodic") /= 0 .or. pattern_query(                                  &
        GridRecord%gtype(Memory%GridOrder(k))%string,"_PERIODIC") /= 0)  then
        positionVector(k) = EIndx(k)
        repetitionVector(k) = k 
      endif
    enddo
  elseif( nconnect > 1 ) then
    ! multiply periodic domain
    noconnections = .FALSE. 
      ! multiply connected domains are not currently supported
  else
    ! no patch connections specified
    noconnections = .TRUE.  

  endif

  !-----------------------------------------------------------------------------
  ! create the distgrid
  !-----------------------------------------------------------------------------
  if( noconnections ) then
    ! no connection
    distgrid = ESMF_DistGridCreate(minIndex=BIndx, maxIndex=EIndx,             &
                   regDecomp=decompOrder, decompflag=decompType,               &
                   vm=VM, rc=localrc)

    if (ESMF_LogMsgFoundError(localrc,"error creating distgrid",               &
             rcToReturn=rc)) return

  else
    ! singlely periodic connection

    call ESMF_DistGridConnection(connection=connectionList(:,1),               &
                                 patchIndexA=1, patchIndexB=1,                 &
                                 positionVector=positionVector,                &
                                 orientationVector=orientationVector,          &
                                 repetitionVector=repetitionVector, rc=localrc)

    distgrid = ESMF_DistGridCreate(minIndex=BIndx, maxIndex=EIndx,             &
                   regDecomp=decompOrder, decompflag=decompType,               &
                   connectionList=connectionList,rc=localrc)

  if (ESMF_LogMsgFoundError(localrc,"error creating distgrid",                 &
             rcToReturn=rc)) return
    deallocate( repetitionVector )
    deallocate( positionVector,orientationVector )
  endif


  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------
  deallocate( BIndx, EIndx )
  deallocate( decompOrder, decompType )
  deallocate( connectionList )

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine create_distribution
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine create_array(Memory, DistGrid, Grid, Array, rc)
  !-----------------------------------------------------------------------------
  ! routine creates a single distribution from specifier files
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(memory_config), intent(in   ) :: Memory
  type(ESMF_DistGrid), intent(in   ) :: DistGrid
  type(grid_specification_record), intent(in   ) :: Grid
  type(ESMF_Array), intent(  out) :: Array
  integer, intent(inout) :: rc
 
  ! local ESMF types
  type(ESMF_ArraySpec) :: ArraySpec

  ! local parameters
  integer :: localrc ! local error status

  ! local integer variables
  integer :: irank, k, tensorsize
  integer, allocatable :: haloL(:), haloR(:)
  integer, allocatable :: top(:), bottom(:)
! integer, allocatable :: map(:)

  ! local logicals
  logical :: nohaloflag


  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! set the dimensionality of actual data storage to the memory size specified
  ! by the problem descriptor string
  !-----------------------------------------------------------------------------
  print*,' array create - rank ', Memory%memRank
  call ESMF_ArraySpecSet(ArraySpec, typekind=ESMF_TYPEKIND_R8,                 &
                         rank=Memory%memRank, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc,"error creating ArraySpecSet",             &
                            rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! the distGridtoArrayMap requires the array dimension as a function of the
  ! distGrid dimension (e.g. the 1st distgrid dimension maps to the 3rd array
  ! dimension ), but the Memory%DistOrder contains the distgrid dimension as a
  ! function of the array dimension. Thus an inversion needs to take place.
  !-----------------------------------------------------------------------------
! allocate( map(Memory%DistRank) )
! map = 0
! do k=1, Memory%memRank
!    if( Memory%DistOrder(k) /= 0 ) then
!       map( Memory%DistOrder(k) ) = k
!    endif
! enddo

  !-----------------------------------------------------------------------------
  ! sanity check, make certain there are nonzero values for all distgrid dimensions
  !-----------------------------------------------------------------------------
! do k=1, Memory%DistRank
!    if( map(k) == 0 .or. map(k) > Memory%MemRank ) then
!       print*,'error - the inversion of DistOrder has failed',map(k)
!    endif
! enddo
! ! drs debug
! print*,' distgridto arraymap ',map
  ! drs debug

  !-----------------------------------------------------------------------------
  ! determine if halo is present
  !-----------------------------------------------------------------------------
  nohaloflag = .true.  
  do irank=1, Memory%GridRank
     if( Memory%HaloL(irank) /= 0 ) nohaloflag = .false.
     if( Memory%HaloR(irank) /= 0 ) nohaloflag = .false.
  enddo   ! irank

  !-----------------------------------------------------------------------------
  ! if no halo specified, create array from ArraySpec object
  !-----------------------------------------------------------------------------
  if( nohaloflag ) then
     print*,' no halos specified '
     !--------------------------------------------------------------------------
     ! assume that the GridRank=DistGridRank, by construction if the
     ! distGridRank < GridRank, we pad the distGrid with ones so that they are
     ! the same rank.
     !
     ! next if the MemoryRank = GridRank, there are no tensor dimensions (i.e.
     ! dimensions both not distributed nor associated with a grid), thus the 
     ! undistLBound and undistUBound arguments need not be specified.
     !--------------------------------------------------------------------------
     if( Memory%memRank ==  Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank = Grid Rank
        !-----------------------------------------------------------------------
        print*,'Memory Rank = Grid Rank ',Memory%memRank, ' = ', Memory%GridRank
        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)

        if (ESMF_LogMsgFoundError(localrc,"error creating non-haloed ESMF " // &
                 "Array with no tensor dimensions", rcToReturn=rc)) return

     elseif( Memory%memRank > Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank > Grid Rank, so there are tensor dimensions
        !-----------------------------------------------------------------------
        tensorsize = Memory%memRank-Memory%GridRank
        allocate( top(tensorsize), bottom(tensorsize) )

        print*,'Tensor dims ',tensorsize,' - Memory Rank > Grid Rank ',        &
               Memory%memRank, Memory%GridRank

        !-----------------------------------------------------------------------
        ! specify the bounds of the undistributed dimension(s).
        !-----------------------------------------------------------------------
        do k=Grid%grank,Grid%grank-tensorsize+1,-1
           bottom(k) = 1
           top(k) = Grid%gsize( Memory%GridOrder(k) )
        enddo  ! k

        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                  indexflag=ESMF_INDEX_GLOBAL,                                 &
                  undistLBound=bottom, undistUBound=top, rc=localrc)

        if (ESMF_LogMsgFoundError(localrc,"error creating non-haloed ESMF " // &
                 "Array with tensor dimensions", rcToReturn=rc)) return

        deallocate( top, bottom )
     else
        print*,'error - Memory Rank < Grid Rank'
        call ESMF_LogMsgSetError( ESMF_FAILURE,"memory rank < Grid rank not"// &
               "supported ",rcToReturn=localrc)
     endif

  else
  !-----------------------------------------------------------------------------
  ! else if halo is specified, create an array with halo padding by setting
  ! totalLWith and totalRwidth
  !-----------------------------------------------------------------------------
     allocate( haloL(Memory%memRank), haloR(Memory%memRank) )
     do k=1,Memory%GridRank
        haloL(k) = Memory%HaloL(k)
        haloR(k) = Memory%HaloR(k)
     enddo
     ! padd additional values so that array sizes matches memory rank
     do k=Memory%GridRank+1,Memory%memRank
        haloL(k) = 0
        haloR(k) = 0
     enddo

     !--------------------------------------------------------------------------
     ! assume that the GridRank=DistGridRank, by construction if the
     ! distGridRank < GridRank, we pad the distGrid with ones so that they are
     ! the same rank.
     !
     ! next if the MemoryRank = GridRank, there are no tensor dimensions (i.e.
     ! dimensions both not distributed nor associated with a grid), thus the 
     ! undistLBound and undistUBound arguments need not be specified.
     !--------------------------------------------------------------------------
     if( Memory%memRank ==  Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank = Grid Rank
        !-----------------------------------------------------------------------
        print*,'Memory Rank = Grid Rank ',Memory%memRank, Memory%GridRank
        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                     totalLWidth=HaloL, totalUWidth=HaloR,                     &
                     indexflag=ESMF_INDEX_GLOBAL, rc=localrc)

        if (ESMF_LogMsgFoundError(localrc,"error creating non-haloed ESMF " // &
                 "Array with no tensor dimensions", rcToReturn=rc)) return

     elseif( Memory%memRank > Memory%GridRank ) then
        !-----------------------------------------------------------------------
        ! Memory Rank > Grid Rank, so there are tensor dimensions
        !-----------------------------------------------------------------------
        tensorsize = Memory%memRank-Memory%GridRank
        allocate( top(tensorsize), bottom(tensorsize) )

        print*,'Tensor dims ',tensorsize,' - Memory Rank > Grid Rank ',        &
               Memory%memRank, Memory%GridRank

        !-----------------------------------------------------------------------
        ! specify the bounds of the undistributed dimension(s).
        !-----------------------------------------------------------------------
        do k=Grid%grank,Grid%grank-tensorsize+1,-1
           bottom(k) = 1
           top(k) = Grid%gsize( Memory%GridOrder(k) )
        enddo  ! k

        Array = ESMF_ArrayCreate(arrayspec=ArraySpec, distgrid=DistGrid,       &
                     totalLWidth=HaloL, totalUWidth=HaloR,                     &
                     indexflag=ESMF_INDEX_GLOBAL,                              &
                     undistLBound=bottom, undistUBound=top, rc=localrc)

        if (ESMF_LogMsgFoundError(localrc,"error creating haloed ESMF " //     &
                 "Array with tensor dimensions", rcToReturn=rc)) return

        deallocate( top, bottom )
     else
        print*,'error - Memory Rank < Grid Rank'
        call ESMF_LogMsgSetError( ESMF_FAILURE,"memory rank < Grid rank not"// &
               "supported ",rcToReturn=localrc)
     endif

     !--------------------------------------------------------------------------
     ! clean up
     !--------------------------------------------------------------------------
     deallocate( haloL, haloR )

  endif

  !-----------------------------------------------------------------------------
  ! clean up
  !-----------------------------------------------------------------------------
! deallocate( map )

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine create_array
  !-----------------------------------------------------------------------------



!-----------------------------------------------------------------------------
  subroutine populate_redist_array(Memory, DistGrid, Grid, Array, rc)
  !-----------------------------------------------------------------------------
  ! routine creates a single distribution from specifier files
  !
  !-----------------------------------------------------------------------------
  ! arguments
  type(memory_config), intent(in   ) :: Memory
  type(ESMF_DistGrid), intent(in   ) :: DistGrid
  type(grid_specification_record), intent(in   ) :: Grid
  type(ESMF_Array), intent(  out) :: Array
  integer, intent(inout) :: rc
 
  ! local ESMF types
  type(ESMF_ArraySpec) :: ArraySpec

  ! local parameters
  integer :: localrc ! local error status

  ! local integer variables
  integer :: irank, k, tensorsize
  integer, allocatable :: haloL(:), haloR(:)
  integer, allocatable :: top(:), bottom(:)

  ! local logicals
  logical :: nohaloflag

  ! local real variables
  real(ESMF_KIND_R8), pointer :: fptr1(:), fptr2(:,:)
  real(ESMF_KIND_R8), pointer :: fptr3(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr4(:,:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr5(:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr6(:,:,:,:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr7(:,:,:,:,:,:,:)


  ! debug
  integer :: i, j, de, localDeCount, dimCount 
  integer, allocatable ::  localDeList(:)
  type(ESMF_LocalArray), allocatable :: larrayList(:)
  integer, allocatable :: LBnd(:,:), UBnd(:,:) 
  real(ESMF_KIND_R8), pointer :: localFptr(:,:)
  type(ESMF_IndexFlag) :: indexflag

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! get local array DE list
  !-----------------------------------------------------------------------------
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=localrc)
  print*,' localdecount ',localDeCount
  if (ESMF_LogMsgFoundError(localrc,"error getting local DE count from array", &
          rcToReturn=rc)) return

  allocate(localDeList(localDeCount))
  call ESMF_ArrayGet(array, localDeList=localDeList, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc,"error getting local DE list from array",  &
          rcToReturn=rc)) return

  allocate(larrayList(localDeCount))
  call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc,"error getting local array list",          &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  ! get dimcount to allocate bound arrays
  !-----------------------------------------------------------------------------
  call ESMF_DistGridGet(DistGrid, dimCount=dimCount, rc=localrc)
  print*,' dimcount ',dimCount
  if (ESMF_LogMsgFoundError(localrc,"error getting dimCount from distGrid",    &
          rcToReturn=rc)) return
  
  allocate(UBnd(dimCount, localDeCount))
  allocate(LBnd(dimCount, localDeCount))  

  call ESMF_ArrayGet(array, indexflag=indexflag,                               &
           exclusiveLBound=LBnd, exclusiveUBound=UBnd, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc,"error getting exclusive bound range",     &
          rcToReturn=rc)) return

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

  deallocate(localDeList)
  deallocate(UBnd)
  deallocate(LBnd)
  deallocate(larrayList)

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine populate_redist_array 
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine report_descriptor_string(PDS, nstatus, localrc)
  !-----------------------------------------------------------------------------
  ! routine reports the configuration 
  ! report string takes the form:
  ! {status}: {source string} {action} {destination string}
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(in   ) :: PDS
  integer, intent(in   ) :: nstatus
  integer, intent(  out) :: localrc

  ! local character strings
  character(ESMF_MAXSTR) :: src_string, dst_string
  character(ESMF_MAXSTR) :: lstatus, laction, lgrid, ldist, lsuffix, ltmp
  character(7) :: lsize, lorder, ltmpL, ltmpR  

  ! local integer variables
  integer :: iDfile, iGfile, irank, iirank, igrid, idist, istatus


  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL


  !-----------------------------------------------------------------------------
  ! set action string
  !-----------------------------------------------------------------------------
  select case( PDS%process%tag )

    case( Harness_Redist )
      laction = "-->"
    case( Harness_BilinearRemap )
      laction = "=B=>"
    case( Harness_ConservRemap )
      laction = "=C=>"
    case( Harness_2ndConservRemap )
      laction = "=S=>"
    case( Harness_NearNeighRemap )
      laction = "=N=>"
    case( Harness_Error )
      ! error
    case default
      ! error
  end select


  do iDfile=1, PDS%nDfiles    ! loop through each of the dist specifier files
  do iGfile=1, PDS%nGfiles    ! loop through each of the grid specifier files
    !---------------------------------------------------------------------------
    ! set STATUS string
    !---------------------------------------------------------------------------
    if( PDS%test_status(iDfile,iGfile) == HarnessTest_SUCCESS ) then
      lstatus = "SUCCESS:"
      istatus = 1
    elseif( PDS%test_status(iDfile,iGfile) == HarnessTest_FAILURE ) then
      lstatus = "FAILURE:"
      istatus = 0
    elseif( PDS%test_status(iDfile,iGfile) == HarnessTest_UNDEFINED ) then
      lstatus = ""
      istatus =-1 
    else
      ! error
      call ESMF_LogMsgSetError( ESMF_FAILURE,"invalid test status value ",     &
               rcToReturn=localrc)

    endif
    ! print specifier files
    print*,iDfile,iGfile,'   > Specifier files:',trim(adjustL(                 &
           PDS%Dfiles(iDfile)%filename)),                                      &
           ', ',trim(adjustL(PDS%Gfiles(iGfile)%filename))

    do idist=1, PDS%Dfiles(iDfile)%nDspecs ! loop thru all of file's dist specifiers
    do igrid=1, PDS%Gfiles(iGfile)%nGspecs ! loop thru all of file's grid specifiers
      !-------------------------------------------------------------------------
      ! set source string
      !-------------------------------------------------------------------------
      src_string = '['
      do irank=1,PDS%SrcMem%memRank 
        ! after the first dimension add separaters to the string
        if( irank > 1 ) src_string = trim(adjustL(src_string)) // '; '

        ! for each dimension of the rank check if there is an associated
        ! distribution and/or grid dimension. If not, insert place holder
        if( PDS%SrcMem%DistOrder(irank) /= 0 ) then
          iirank = PDS%SrcMem%DistOrder(irank)
          write(lsize,"(i6)" ) PDS%Dfiles(iDfile)%src_dist(idist)%dsize(iirank)
          write(lorder,"(i1)") PDS%SrcMem%DistOrder(irank)
          ldist = trim(adjustL(PDS%SrcMem%DistType(irank)%string)) //          &
                  trim(adjustL(lorder))// '{' // trim(adjustL(lsize)) // '}'
        else
          ldist = '*{}'
        endif
        ! now do the grid part
        if( PDS%SrcMem%GridOrder(irank) /= 0 ) then
          iirank = PDS%SrcMem%GridOrder(irank)
          write(lsize,"(i6)" ) PDS%Gfiles(iGfile)%src_grid(igrid)%gsize(iirank)
          write(lorder,"(i1)") PDS%SrcMem%GridOrder(irank)
          lgrid = trim(adjustL(PDS%SrcMem%GridType(irank)%string)) //          &
                  trim(adjustL(lorder)) // '{' // trim(adjustL(lsize)) // '}'
        else
          lgrid = '*{}'
        endif

        ! now add the suffix indicating periodicity and/or a halo
        lsuffix = ''
        ! check if the grid type is periodic
        if( pattern_query(                                                     &
                    PDS%Gfiles(iGfile)%src_grid(igrid)%gtype(irank)%string,    &
                    "_periodic") /= 0 .or. pattern_query(                      &
                    PDS%Gfiles(iGfile)%src_grid(igrid)%gtype(irank)%string,    &
                           "_PERIODIC") /= 0 ) lsuffix = '+P'
        ! check for nonzero halos
        if( PDS%SrcMem%HaloL(irank) /= 0 .or. PDS%SrcMem%HaloR(irank) /= 0 ) then
          write(ltmpL,"(i6)") PDS%SrcMem%HaloL(irank)
          write(ltmpR,"(i6)") PDS%SrcMem%HaloR(irank)
          ltmp = '+H{'//trim(adjustL(ltmpL))// ':' //trim(adjustL(ltmpR))// '}'
          lsuffix = trim(adjustL(lsuffix)) // trim(adjustL(ltmp))
        endif
        ! concatenate the distribution and grid strings to the previous part of
        ! the source string
        src_string = trim(adjustL(src_string)) // trim(adjustL(ldist)) //      &
                     trim(adjustL(lgrid)) // trim(adjustL(lsuffix))

      enddo   ! irank
      ! terminate the string with a close bracket and a stagger specification 
      src_string = trim(adjustL(src_string)) // ']'
      !-------------------------------------------------------------------------
      ! set destination string
      !-------------------------------------------------------------------------
      dst_string = '['
      do irank=1,PDS%DstMem%memRank 
        ! after the first dimension add separaters to the string
        if( irank > 1 ) dst_string = trim(adjustL(dst_string)) // ';'

        ! for each dimension of the rank check if there is an associated
        ! distribution and/or grid dimension. If not, insert place holder
        if( PDS%DstMem%DistOrder(irank) /= 0 ) then
          iirank = PDS%DstMem%DistOrder(irank)
          write(lsize,"(i6)" ) PDS%Dfiles(iDfile)%dst_dist(idist)%dsize(iirank)
          write(lorder,"(i1)") PDS%DstMem%DistOrder(irank)
          ldist = trim(adjustL(PDS%DstMem%DistType(irank)%string)) //          &
                  trim(adjustL(lorder)) // '{' // trim(adjustL(lsize)) // '}'
        else
          ldist = '*{}'
        endif
        ! now do the grid part
        if( PDS%DstMem%GridOrder(irank) /= 0 ) then
          iirank = PDS%DstMem%GridOrder(irank)
          write(lsize,"(i6)" ) PDS%Gfiles(iGfile)%dst_grid(igrid)%gsize(iirank)
          write(lorder,"(i1)") PDS%DstMem%GridOrder(irank)
          lgrid = trim(adjustL(PDS%DstMem%GridType(irank)%string)) //          &
                  trim(adjustL(lorder)) // '{' // trim(adjustL(lsize)) // '}'
        else
          lgrid = '*{}'
        endif

        ! now add the suffix indicating periodicity and/or a halo
        lsuffix = '             '
        ! check if the grid type is periodic
        if( pattern_query(                                                     &
                    PDS%Gfiles(iGfile)%dst_grid(igrid)%gtype(irank)%string,    &
                    "_periodic") /= 0 .or. pattern_query(                      &
                    PDS%Gfiles(iGfile)%dst_grid(igrid)%gtype(irank)%string,    &
                           "_PERIODIC") /= 0 ) lsuffix = '+P'
        ! check for nonzero halos
        if( PDS%DstMem%HaloL(irank) /= 0 .or. PDS%DstMem%HaloR(irank) /= 0 ) then
          write(ltmpL,"(i6)") PDS%DstMem%HaloL(irank)
          write(ltmpR,"(i6)") PDS%DstMem%HaloR(irank)
          ltmp = '+H{'//trim(adjustL(ltmpL))// ':' //trim(adjustL(ltmpR))// '}'
          lsuffix = trim(adjustL(lsuffix)) // trim(adjustL(ltmp))
        endif
        ! concatenate the distribution and grid strings to the previous part of
        ! the source string
        dst_string = trim(adjustL(dst_string)) // trim(adjustL(ldist)) //      &
                     trim(adjustL(lgrid)) // trim(adjustL(lsuffix))

      enddo   ! irank
      ! terminate the string with a close bracket and a stagger specification 
      dst_string = trim(adjustL(dst_string)) // ']'


      ! print out result string if codes match
      if( istatus == nstatus .or. nstatus < 0 ) then
         print*,trim(adjustL(lstatus)), trim(adjustL(src_string)),             &
             trim(adjustL(laction)),trim(adjustL(dst_string))
      endif
    enddo    ! idist
    enddo    ! igrid

  enddo    ! iGfile
  enddo    ! iDfile
  !-----------------------------------------------------------------------------
  ! set destination string
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine report_descriptor_string
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine parse_descriptor_string(nstrings, pds, localrc)
  !-----------------------------------------------------------------------------
  ! Routine parses a problem descriptor string and extracts:
  !	operation - redistribution or regrid plus method
  !	rank of memory
  !	rank of distribution
  !	rank of grid association
  !     order of dist and grid
  !     type of dist or grid
  !
  ! Upon completion, the routine returns the values to the harness record
  ! Harness%Record(*)%string(*)%   for 
  !
  !     process%string          character string of process
  !     process%tag             numerical tag for process
  !
  !     SrcMem%memRank          rank of source memory block
  !     SrcMem%GridRank         rank of source grid 
  !     SrcMem%DistRank         rank of source distribution
  !     SrcMem%GridType         type of grid
  !     SrcMem%DistType         type of distribution
  !     SrcMem%GridOrder        order of grid elements with dimensions
  !     SrcMem%DistOrder        order of distribution elements with dimensions
  !     SrcMem%HaloL            Left halo size for each dimension
  !     SrcMem%HaloR            Right halo size for each dimension
  !     SrcMem%StagLoc          Stagger location for each dimension
  ! and the equivalent DstMem records.
  !
  !-----------------------------------------------------------------------------
  ! arguments
  integer, intent(in   ) :: nstrings
  type(problem_descriptor_strings), intent(inout) :: pds
  integer, intent(  out) :: localrc

  ! local character strings
  character(ESMF_MAXSTR) :: lstring, lname
  character(ESMF_MAXSTR) :: src_string, dst_string
  type(character_array), allocatable :: lsrc(:), ldst(:)
  type(character_array), allocatable :: grid_type(:), dist_type(:)

  logical :: flag = .true.

  ! local integer variables
  integer :: k
  integer :: tag, location(2)
  integer :: dst_beg, dst_end, src_beg, src_end
  integer :: srcMulti, dstMulti
  integer :: srcBlock,dstBlock
  integer :: src_mem_rank, dst_mem_rank
  integer :: dist_rank, grid_rank

  integer, allocatable :: grid_order(:), dist_order(:)
  integer, allocatable :: grid_HaloL(:), grid_HaloR(:)
  integer, allocatable :: grid_StagLoc(:)

  ! local logical variable
  logical :: endflag = .false.

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! parse the problem descriptor string
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! search for the test operation
  ! 1. search for "->" or "=>" - tip of symbol provides ending address
  ! 2. back up until reach white space - provides begining address
  ! 3. src_beg = 1, src_end = operation_beg-1
  !    dst_beg = operation_end+1, dst_end = length(trim(string))
  ! 4. LOCATION provides a reference for later splitting the string into
  !    source and destination parts
  !-----------------------------------------------------------------------------
  lstring = trim(adjustL( pds%pds ) )
  call process_query(lstring, lname, tag, location, localrc)  
  if( ESMF_LogMsgFoundError(localrc,"syntax error in problem descriptor" //    &
          " string " // trim(adjustL(lstring)),                                &
          rcToReturn=localrc) ) return


  ! store name (string) of operation and numerical code
  pds%process%string = lname
  pds%process%tag = tag

  !-----------------------------------------------------------------------------
  ! separate the problem descriptor string into source and destination
  ! chunks to ease with parsing, but first determine if the problem
  ! descriptor string describes a multiple block memory structure
  !-----------------------------------------------------------------------------
  call memory_topology(lstring, location, srcMulti, srcBlock,                  &
                       dstMulti, dstBlock, localrc)
  if( ESMF_LogMsgFoundError(localrc,"syntax error in problem descriptor" //    &
          " string " // trim(adjustL(lstring)),                                &
          rcToReturn=localrc) ) return

  ! a multiple block memory structure contains (), these are counted in
  ! srcMulti and dstMulti 
  if( (srcMulti >= 1).or.(dstMulti >= 1) ) then
     ! TODO break into multiple single block strings
     ! and parse each string separately

  elseif( (srcMulti == 0).and.(dstMulti == 0) ) then
     ! single block memory structure
     if( (srcBlock==1).and.(dstBlock==1) ) then
        src_beg = 1
        src_end = location(1)
        dst_beg = location(2)
        dst_end = len( trim(lstring) )
        !-----------------------------------------------------------------------
        ! separate string into source and destination strings
        !-----------------------------------------------------------------------
        src_string = adjustL( lstring(src_beg:src_end) )
        dst_string = adjustL( lstring(dst_beg:dst_end) )

        !-----------------------------------------------------------------------
        ! check that the source and destination memory ranks are not empty
        ! and that the sizes agree
        !-----------------------------------------------------------------------
        src_mem_rank = pattern_query(src_string,';') + 1
        pds%SrcMem%memRank = src_mem_rank
        dst_mem_rank = pattern_query(dst_string,';') + 1
        pds%DstMem%memRank = dst_mem_rank

        if( (src_mem_rank == 0).or.(dst_mem_rank == 0).or.                     &
            (src_mem_rank /= dst_mem_rank) )  then
           localrc = ESMF_FAILURE
           call ESMF_LogMsgSetError(ESMF_FAILURE,"rank of memory block "       &
                    // "symbols not properly paired", rcToReturn=localrc)
        endif

        ! test for common mistake of using commas instead of semicolons
        if( pattern_query(dst_string,',') > 0 ) then
           localrc = ESMF_FAILURE
           call ESMF_LogMsgSetError(ESMF_FAILURE,"syntax error - commas"       &
                    // " are not valid deliminators", rcToReturn=localrc)
        endif

        !-----------------------------------------------------------------------
        ! create work space for parsing the source descriptor string
        !-----------------------------------------------------------------------
        allocate( lsrc(src_mem_rank+1) )
        allocate( grid_order(src_mem_rank) )
        allocate( grid_type(src_mem_rank) )      
        allocate( grid_HaloL(src_mem_rank) )
        allocate( grid_HaloR(src_mem_rank) )
        allocate( grid_StagLoc(src_mem_rank) )    
        allocate( dist_order(src_mem_rank) )      
        allocate( dist_type(src_mem_rank) )      

        allocate( pds%SrcMem%GridType(src_mem_rank) )
        allocate( pds%SrcMem%DistType(src_mem_rank) )
        allocate( pds%SrcMem%GridOrder(src_mem_rank) )
        allocate( pds%SrcMem%DistOrder(src_mem_rank) )
        allocate( pds%SrcMem%HaloL(src_mem_rank) )
        allocate( pds%SrcMem%HaloR(src_mem_rank) )
        allocate( pds%SrcMem%StagLoc(src_mem_rank) )

        !-----------------------------------------------------------------------
        ! partition the source descriptor string into separate parts which
        ! correspond to memory locations and parse the substrings for 
        ! grid and distribution descriptions.
        !-----------------------------------------------------------------------
        call memory_separate( src_string, src_mem_rank, lsrc, localrc) 
        if( ESMF_LogMsgFoundError(localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - memory separate " //           &
                 trim(adjustL(lstring)), rcToReturn=localrc) ) return
        call interpret_descriptor_string( lsrc, src_mem_rank,                  & 
                 grid_rank, grid_order, grid_type, grid_HaloL, grid_HaloR,     &
                 grid_StagLoc, dist_rank, dist_order, dist_type, localrc)

        if( ESMF_LogMsgFoundError(localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - interpret string " //          &
                trim(adjustL(lstring)), rcToReturn=localrc) ) return

        pds%SrcMem%GridRank = grid_rank
        pds%SrcMem%DistRank = dist_rank

        do k=1,pds%SrcMem%memRank 
           pds%SrcMem%GridType(k)%string = grid_type(k)%string
           pds%SrcMem%DistType(k)%string = dist_type(k)%string

           pds%SrcMem%GridOrder(k) = grid_order(k)
       ! These are getting corrupted when tensor dimensions are specified 
       !   print*,' grid order outside ', grid_order(k)
       !   print*,' halo ', grid_HaloL(k), grid_HaloR(k)
           pds%SrcMem%DistOrder(k) = dist_order(k)
           pds%SrcMem%HaloL(k)     = grid_HaloL(k)
           pds%SrcMem%HaloR(k)     = grid_HaloR(k)
           pds%SrcMem%StagLoc(k)   = grid_StagLoc(k)
        enddo

        deallocate( lsrc )
        deallocate( grid_order, grid_type, grid_HaloL, grid_HaloR )
        deallocate( grid_StagLoc )    
        deallocate( dist_order, dist_type )      

        !-----------------------------------------------------------------------
        ! create work space for parsing the destination descriptor string
        !-----------------------------------------------------------------------
        allocate( ldst(dst_mem_rank+1) )
        allocate( grid_order(dst_mem_rank) )
        allocate( grid_type(dst_mem_rank) )
        allocate( grid_HaloL(dst_mem_rank) )
        allocate( grid_HaloR(dst_mem_rank) )
        allocate( grid_StagLoc(dst_mem_rank) )    
        allocate( dist_order(dst_mem_rank) )
        allocate( dist_type(dst_mem_rank) )      

        allocate( pds%DstMem%GridOrder(dst_mem_rank) )
        allocate( pds%DstMem%DistOrder(dst_mem_rank) )
        allocate( pds%DstMem%GridType(dst_mem_rank) )
        allocate( pds%DstMem%DistType(dst_mem_rank) )
        allocate( pds%DstMem%HaloL(dst_mem_rank) )
        allocate( pds%DstMem%HaloR(dst_mem_rank) )
        allocate( pds%DstMem%StagLoc(dst_mem_rank) )

        !-----------------------------------------------------------------------
        ! partition the destination descriptor string into separate parts
        ! which correspond to memory locations and parse the substrings 
        ! for grid and distribution descriptions.
        !-----------------------------------------------------------------------
        call memory_separate( dst_string, dst_mem_rank, ldst, localrc) 
        if( ESMF_LogMsgFoundError(localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - memory separate " //           &
                trim(adjustL(lstring)), rcToReturn=localrc) ) return
        call interpret_descriptor_string( ldst, dst_mem_rank,                  & 
                grid_rank, grid_order, grid_type, grid_HaloL, grid_HaloR,      &
                grid_StagLoc, dist_rank, dist_order, dist_type, localrc)
        if( ESMF_LogMsgFoundError(localrc,"syntax error in SRC portion " //    &
                "of problem descriptor string - interpret string " //          &
                trim(adjustL(lstring)), rcToReturn=localrc) ) return

        pds%DstMem%GridRank = grid_rank
        pds%DstMem%DistRank = dist_rank

        do k=1,pds%DstMem%memRank 
           pds%DstMem%GridType(k)%string  = grid_type(k)%string
           pds%DstMem%DistType(k)%string  = dist_type(k)%string

           pds%DstMem%GridOrder(k) = grid_order(k)
           pds%DstMem%DistOrder(k) = dist_order(k)

           pds%DstMem%HaloL(k)     = grid_HaloL(k)
           pds%DstMem%HaloR(k)     = grid_HaloR(k)
           pds%DstMem%StagLoc(k)   = grid_StagLoc(k)
        enddo

        deallocate( ldst )
        deallocate( grid_order, grid_type, grid_HaloL, grid_HaloR )
        deallocate( grid_StagLoc )    
        deallocate( dist_order, dist_type )      
     else  ! error does not conform to either single block or multiblock
        localrc = ESMF_FAILURE
        call ESMF_LogMsgSetError(ESMF_FAILURE,"syntax error - problem "        &
                 // " descriptor string does not conform to either " //        &
                 "single block syntax or to multiblock syntax",                &
                 rcToReturn=localrc)
     endif
  else   ! error does not conform to either single block or multiblock
     localrc = ESMF_FAILURE
     call ESMF_LogMsgSetError(ESMF_FAILURE,"syntax error - problem "           &
              // " descriptor string does not conform to either " //           &
              "single block syntax or to multiblock syntax",                   &
              rcToReturn=localrc)
  endif

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine parse_descriptor_string
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine interpret_descriptor_string(lstring, nstring,                     &
                                         grid_rank, grid_order, grid_type,     &
                                         HaloL, HaloR, StaggerLoc,             &
                                         dist_rank, dist_order, dist_type,     &
                                         localrc)
  !-----------------------------------------------------------------------------
  ! This routine parses a part ( either source or destination) of a problem
  ! descriptor string for for descriptions of the memory topology and rank,
  ! the grid rank, order, halo and stagger, and the distribution type, rank,
  ! and order. The routine assumes the string has beeen partitioned so that
  ! each memory slot is stored in an element of a character array.
  !-----------------------------------------------------------------------------

  ! arguments
  type(character_array), intent(in   ) :: lstring(:)
  integer,               intent(in   ) :: nstring 
  integer,               intent(  out) :: grid_rank, grid_order(:)
  integer,               intent(  out) :: dist_rank, dist_order(:)
  type(character_array), intent(  out) :: grid_type(:), dist_type(:)
  integer,               intent(  out) :: HaloL(:), HaloR(:)
  integer,               intent(  out) :: StaggerLoc(:)
  integer,               intent(  out) :: localrc


  ! local variables
  character(ESMF_MAXSTR) :: ltmp, lstagger, intstr
  integer :: k, n, kstring, rank, halo, ndelim
  integer :: iloc(1), mloc(1)
  integer :: hbeg, hmid, hend, sbeg, send, slen
  integer :: itmp, itmp_beg, itmp_end
  integer, allocatable ::  sdelim(:)
  integer, allocatable ::  assoc_grid(:)


  !-----------------------------------------------------------------------------
  ! initialize return variable
  !-----------------------------------------------------------------------------
  localrc = ESMF_RC_NOT_IMPL 

  !-----------------------------------------------------------------------------
  ! work array
  !-----------------------------------------------------------------------------
  allocate( assoc_grid(nstring) )
  assoc_grid = 0

  !-----------------------------------------------------------------------------
  ! Determine grid layout (rank and order)
  !-----------------------------------------------------------------------------
  grid_rank = 0 
  do kstring=1, nstring
     rank = set_query(lstring(kstring)%string, 'GU') 
     if( rank == 0 ) then
     ! no associated grid
        grid_type(kstring)%string = ' '
        grid_order(kstring) = 0
     elseif( rank == 1 ) then
     ! associated grid
        grid_rank = grid_rank +  rank
        call set_locate(lstring(kstring)%string, 'GU', rank , mloc)
        grid_type(kstring)%string = lstring(kstring)%string(mloc(1):mloc(1))
        read( lstring(kstring)%string(mloc(1)+1:mloc(1)+1), *) grid_order(kstring)
        !  keep track of associated dimensions
        assoc_grid( grid_order(kstring) ) =  -1

        halo = set_query(lstring(kstring)%string, 'H') 
        if( halo == 1 ) then
        !-----------------------------------------------------------------------
        ! halo is specified, now check that the syntax is correct
        !-----------------------------------------------------------------------
           halo = set_query(lstring(kstring)%string, '{:}') 
           if( halo == 3 ) then
              itmp = 1
              call pattern_locate(lstring(kstring)%string, 'H{', itmp, iloc)    
              hbeg = iloc(1)
              if( itmp /= 1) then
              !syntax error in halo specification
              call ESMF_LogMsgSetError( ESMF_FAILURE,                          &
                      "halo specification missing prefix",                     &
                      rcToReturn=localrc)
              endif

              call set_locate(lstring(kstring)%string, ':', itmp, iloc)    
              hmid = iloc(1)
              if( itmp /= 1) then
              !syntax error in halo specification
              call ESMF_LogMsgSetError( ESMF_FAILURE,                          &
                      "halo specification missing separator",                  &
                      rcToReturn=localrc)
              endif

              call set_locate(lstring(kstring)%string, '}', itmp, iloc)    
              hend = iloc(1)
              if( itmp /= 1) then
              !syntax error in halo specification
              call ESMF_LogMsgSetError( ESMF_FAILURE,                          &
                      "halo specification missing suffix",                     &
                      rcToReturn=localrc)
              endif
              !-----------------------------------------------------------------
              ! halo syntax is correct, now read in the halo values as characters 
              ! and convert then to integer values.
              !-----------------------------------------------------------------
              intstr = adjustL( lstring(kstring)%string(hmid-1:hbeg+2) )
              read (intstr, *) HaloL(kstring)
              intstr = adjustL( lstring(kstring)%string(hend-1:hmid+1) )
              read (intstr, *) HaloR(kstring)
  ! drs begug
              ! currently halo must be symmetric and non-negative
              if( HaloR(kstring) < 0 .or. HaloL(kstring) < 0 .or.              &
                  HaloL(kstring) /= HaloR(kstring)  )                          &
                  call ESMF_LogMsgSetError( ESMF_FAILURE,                      &
                      "halo specification "//trim(lstring(kstring)%string) //  &
                      " is not symmetric and/or is negative ",                 &
                      rcToReturn=localrc)
           else
           ! syntax error for halo specification
              call ESMF_LogMsgSetError( ESMF_FAILURE,                          &
                      "halo specification "//trim(lstring(kstring)%string),    &
                      rcToReturn=localrc)
           endif   ! halo

        elseif( halo == 0 ) then 
        ! no halo 
           HaloL(kstring) = 0
           HaloR(kstring) = 0
        else
        ! syntax error for halo specification
           HaloL(kstring) = 0
           HaloR(kstring) = 0
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                   "halo specification wrong "//trim(lstring(kstring)%string), &
                   rcToReturn=localrc)
        endif    ! halo

     else 
     ! error multiple grid specifications for single memory location
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                "multiple grid specifications for single memory location " //  &
                trim(lstring(kstring)%string), rcToReturn=localrc)
     endif

  enddo    !  kstring

  !-----------------------------------------------------------------------------
  ! fill in the sizes for the tensor dimensions
  !-----------------------------------------------------------------------------
  if( nstring > grid_rank ) then
     n = nstring
     do k=grid_rank+1, nstring
        do while( assoc_grid(n) == -1 )
           n = n-1
        enddo   ! while
        grid_order(k) = n 
     enddo
  endif

  !-----------------------------------------------------------------------------
  ! initialize stagger location
  !-----------------------------------------------------------------------------
  do k=1,grid_rank
     staggerloc(k) = 0
  enddo

  !-----------------------------------------------------------------------------
  ! determine if the stagger location is specified by the problem descriptor
  ! string. Look for trailing tag of the form "@{#,#,#}" following the 
  ! closing "]". If it exists it will be placed in the memory_rank+1 element 
  ! of the character array.
  !-----------------------------------------------------------------------------

  itmp_beg = pattern_query(lstring(nstring+1)%string, ']@{')
  if( itmp_beg == 1 ) then
     call pattern_locate( lstring(nstring+1)%string, ']@{', itmp_beg, iloc)
     sbeg = iloc(1)
     slen = len( trim( lstring(nstring+1)%string ) )

     ! extract the stagger substring for further parsing
     ltmp = trim( adjustL( lstring(nstring+1)%string(sbeg+2:slen) ))
     itmp_end = pattern_query(ltmp, '}')

     if( itmp_end == 1 ) then
        call pattern_locate( ltmp, '}', itmp_end, iloc )
        send = iloc(1)
        lstagger = trim( adjustL( ltmp(2:send) ))

        ! determine the number of entries
        ndelim = pattern_query( lstagger, ',')

        if( ndelim >= 1 .and. ndelim <= grid_rank-1 ) then   
           ! identify the separate entries, check that they are not empty,
           ! and read the values
           allocate( sdelim(ndelim) )
           call pattern_locate( lstagger, ',', ndelim, sdelim)

           if(  sdelim(1)-1 >= 1) then
              intstr = adjustL( lstagger( 1:sdelim(1)-1 ) ) 
              read(intstr, *) staggerloc(1)
           else
           ! specification empty
              call ESMF_LogMsgSetError( ESMF_FAILURE,                          &
                      "stagger location specification empty ",                 &
                       rcToReturn=localrc)
           endif
        
           do k=2,ndelim
              if(  sdelim(k)-1 > sdelim(k-1) ) then
                 intstr = adjustL( lstagger( sdelim(k-1)+1:sdelim(k)-1) ) 
                 read(intstr, *) staggerloc(k)
              else
              ! specification empty
                 call ESMF_LogMsgSetError( ESMF_FAILURE,                       &
                         "stagger location specification empty ",              &
                         rcToReturn=localrc)
              endif
           enddo     ! ndelim

           send = len( trim( adjustL( lstagger ) ) )
           if(  send-1 >= sdelim(ndelim) ) then
              intstr = adjustL( lstagger( send-1:sdelim(ndelim)+1 ) ) 
              read(intstr, *) staggerloc(ndelim+1)
           else
           ! specification empty
              call ESMF_LogMsgSetError( ESMF_FAILURE,                          &
                      "stagger location specification empty ",                 &
                      rcToReturn=localrc)
           endif
           ! clean up workspace
           deallocate( sdelim )

        else    
        ! wrong number of delimiters for grid rank
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                   "wrong number of delimiters for grid rank",                 &
                   rcToReturn=localrc)
        endif    ! number of delimiters
     else
     ! error missing ending delimiter
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                "missing stagger location ending delimitor from string",       &
                   rcToReturn=localrc)
     endif     ! proper ending syntax
  elseif( itmp_beg == 0 ) then
  ! no stagger assumption, assume cell center location
     do k=1,grid_rank
        staggerloc(k) = 0
     enddo
  else
  ! syntax error
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
             "problem descriptor string syntax error, strings ends with " //   &
             trim(lstring(nstring+1)%string), rcToReturn=localrc)
  endif     ! proper starting syntax

  !-----------------------------------------------------------------------------
  ! check stagger location for acceptable values
  !-----------------------------------------------------------------------------
  do k=1,grid_rank
     if( staggerloc(k) /= 0 .and. staggerloc(k) /= 1 ) then
        ! error invalid staggerlocs
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                "invalid stagger locations from problem descriptor string",    &
                rcToReturn=localrc)
     endif
  enddo

  !-----------------------------------------------------------------------------
  ! Determine Distribution layout (rank and order)
  !-----------------------------------------------------------------------------
  dist_rank = 0
  do kstring=1, nstring
     rank = set_query(lstring(kstring)%string, 'BCA') 
     if( rank == 0 ) then
     ! no associated distribution
        dist_type(kstring)%string = ' '
        dist_order(kstring) = 0
     elseif( rank == 1 ) then
     ! associated grid
        dist_rank = dist_rank +  rank
        call set_locate(lstring(kstring)%string, 'BCA', rank , mloc)
        dist_type(kstring)%string = lstring(kstring)%string(mloc(1):mloc(1))
        read( lstring(kstring)%string(mloc(1)+1:mloc(1)+1), *) dist_order(kstring)
     else 
        ! error multiple distribution specifications for single memory location
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                "multiple distribution specifications in single memory" //     &
                "location", rcToReturn=localrc)
     endif

  enddo    !  kstring

  !-----------------------------------------------------------------------------
  ! clean up 
  !-----------------------------------------------------------------------------
  deallocate( assoc_grid )

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine interpret_descriptor_string
  !-----------------------------------------------------------------------------

 !------------------------------------------------------------------------------

!===============================================================================
  end module ESMF_TestHarnessMod
!===============================================================================
