! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessGridMod"
!
!  ESMF Test Harness Grid Utility Module
   module ESMF_TestHarnessGridMod
!
!===============================================================================
!
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessGridMod
!
! !DESCRIPTION:
!
! The code in this module contains data types and basic functions for accessing
! grid specifications needed for the {\tt ESMF\_TestHarness}.
!
!-------------------------------------------------------------------------------
! !USES:

! use ESMF
! use ESMF_TestHarnessTypesMod
  use ESMF_TestHarnessUtilMod

  implicit none

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
  public read_grid_specification

!
!===============================================================================

  contains 

!===============================================================================


  !-----------------------------------------------------------------------------
  subroutine read_grid_specification(Gfile, rc)
  !-----------------------------------------------------------------------------
  ! driver for reading the grid specifier files. Inputs a single grid record
  ! corresponding to a single specifier file of a single problem descriptor string. 
  !
  !-----------------------------------------------------------------------------

  ! arguments
  type(grid_record), intent(inout) :: Gfile 
  integer, intent(  out) :: rc

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local character strings
  character(THARN_MAXSTR) :: ltmp, lfilename

  ! local integers variables
  integer :: localrc ! local error status

  integer :: i, igrid, irank

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot create config object",            &
                            rcToReturn=rc) ) return

  lfilename = Gfile%filename
  print*,'Opening Grid specifier file  ',trim( lfilename )
  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot load config file " // trim( lfilename ),                      &
         rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! Search and extract the grid type specifier
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'map_type:', rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label map_type:", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot read config label map_type:" , rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! Read the grid specifier file. The 'map_type' argument specifies the type 
  ! of grid specification to be read ( redistrbution or regridding ).
  !-----------------------------------------------------------------------------
  select case(trim(adjustL(ltmp)) )

     case('REDISTRIBUTION')
       print*,' read grid specification for redistribution test'
       call read_redistribution_grid(lfilename, Gfile%nGspecs, Gfile%src_grid, &
                Gfile%dst_grid,localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return

     case('REGRID')
       print*,' read grid specification for regridding test'
       call read_regridding_grid(lfilename, Gfile%nGspecs, Gfile%src_grid,      &
                Gfile%dst_grid, Gfile%testfunction, localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return
       
       if( debug_flag ) then
       !------------------------------------------------------------------------
       ! print out diagnostics
       !------------------------------------------------------------------------
       print*,'Number of grid specs', Gfile%nGspecs
       do igrid=1, Gfile%nGspecs
           print*,'====================================================='
           print*,igrid,'SRC rank: ', Gfile%src_grid(igrid)%grank
           do irank=1, Gfile%src_grid(igrid)%grank
             print*,'SRC grid type: ', Gfile%src_grid(igrid)%gtype(irank)%string
             print*,'SRC size/range:', Gfile%src_grid(igrid)%gsize(irank),'/',  &
                    Gfile%src_grid(igrid)%grange(irank,1),Gfile%src_grid(igrid)%grange(irank,2) 
             print*,'SRC grid units: ', Gfile%src_grid(igrid)%gunits(irank)%string
           enddo
           print*,igrid,'DST rank: ', Gfile%dst_grid(igrid)%grank
           do irank=1, Gfile%dst_grid(igrid)%grank
             print*,'DST grid type: ', Gfile%dst_grid(igrid)%gtype(irank)%string
             print*,'DST size/range: ', Gfile%dst_grid(igrid)%gsize(irank),'/',  &
                    Gfile%dst_grid(igrid)%grange(irank,1),Gfile%dst_grid(igrid)%grange(irank,2) 
             print*,'DST grid units: ', Gfile%dst_grid(igrid)%gunits(irank)%string
           enddo
           print*,'-----------------------------------------------------'
           print*,igrid,' testfunction: ', trim(Gfile%testfunction(igrid)%string)
           do i=1,Gfile%testfunction(igrid)%prank
             print*,' testfunction parameters: ',Gfile%testfunction(igrid)%param(i)
           enddo
           print*,'====================================================='
       enddo
       !------------------------------------------------------------------------
       ! print out diagnostics
       !------------------------------------------------------------------------
       endif

     case default
       ! error
       call ESMF_LogSetError( ESMF_FAILURE,                                 &
              msg="unknown grid type in " // trim(lfilename),                      &
              rcToReturn=rc)
       return
     end select
  !-----------------------------------------------------------------------------
  ! clean up CF     
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, rc=localrc) 
  if( ESMF_LogFoundError(localrc, msg="cannot destroy config object",            &
                            rcToReturn=rc) ) return

              
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------
  end subroutine read_grid_specification
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine read_redistribution_grid(lfilename, ngrids, grid, tmp_grid, rc)
  !-----------------------------------------------------------------------------
  ! routine to read the grid specifier file for a redistribution test. The
  ! routine reads the single grid specification needed by the redistribution
  ! test.
  !
  ! the grid specification takes the form of a table with row entries
  ! (0) grid rank
  ! and (5 * grid rank) remaining entries consisting of cycles of
  ! (1) grid type
  ! (2) grid size
  ! (3) grid minimum range
  ! (4) grid maximum range
  ! (5) grid units
  ! Acceptable values for the units are: DEGREE(S), DEGREE(S)_EAST,
  ! DEGREE(S)_NORTH, DEG, DEG_E, DEG_N, RADIANS, RAD, METERS, M, KILOMETERS, KM.
  ! 
  ! NOTES:
  ! Both the grid type and units should be enclosed by either single or double 
  ! quotes to insure proper parsing by the ESMF_Config calls.
  !
  !-----------------------------------------------------------------------------

  ! arguments
  character(THARN_MAXSTR), intent(in   ) :: lfilename
  type(grid_specification_record), pointer :: grid(:)      ! source grid spec
  type(grid_specification_record), pointer :: tmp_grid(:)  ! duplicate copy of
                                                           ! the grid specifier for 
                                                           ! the destination grid
  integer, intent(  out) :: ngrids
  integer, intent(inout) :: rc

  ! local parameters
  character(THARN_MAXSTR), parameter :: descriptor_label = "map_redist::"

  ! local esmf types
  type(ESMF_Config) :: localcf

  ! local character strings
  character(THARN_MAXSTR) :: ltmp, lchar
  character(THARN_MAXSTR) :: gtype, gunits

  logical :: flag

  ! local integer variables
  integer :: ntmp, grank, gsize
  integer :: irow, krow, nrows, igrid, ngrid, irank, kelements
  integer, allocatable :: ncolumns(:), new_row(:)
  integer :: localrc ! local error status
  integer :: allocRcToTest

  ! local real variables
  real(ESMF_KIND_R8) :: gmin, gmax

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot create config object",            &
                            rcToReturn=rc) ) return

  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot load config file " // trim( lfilename ),                      &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------- 
  ! extract the grid type specifier as sanity check
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'map_type:', rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label map_type", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label map_type:",                                 &
         rcToReturn=rc) ) return

  if( trim(adjustL( ltmp )) /= 'REDISTRIBUTION' ) then
     call ESMF_LogSetError(    &
            ESMF_FAILURE, msg="Wrong grid type for redist test in file " //        &
            trim( lfilename ), rcToReturn=rc) 
     return
  endif
  
  !-----------------------------------------------------------------------------
  ! search for the grid specifier table
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) print*,' find descriptor label failed'
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! determine the total number of table rows, continue only if not empty
  ! NOTE: the number of table rows >= number of grid entries due to the
  ! possibility of continued lines.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigGetDim(localcf, nrows, ntmp, label=trim(descriptor_label),         &
                         rc=localrc)
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot get descriptor table size in file " // trim(lfilename),       &
         rcToReturn=rc) ) return

  if( nrows .le. 0 ) then
     call ESMF_LogSetError( ESMF_FAILURE,                                   &
             msg="grid specifier table is empty in file " //trim(lfilename),       &
             rcToReturn=rc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! extract the table column lengths of this file
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return

  allocate( ncolumns(nrows), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array ncolumns in "// &
     " read_redistribution_grid", rcToReturn=rc)) then
  endif

  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     if( ESMF_LogFoundError(localrc,                                        &
             msg="cannot advance to next line of table " //                        &
              trim(descriptor_label) // " in file " // trim(lfilename),        &
              rcToReturn=rc) ) return


      ncolumns(krow) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(krow) .lt. 6 ) then
        write(lchar,"(i5)") krow
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                 msg="problem reading line " // trim(adjustl(lchar)) //            &
                 " of table in file " // trim(lfilename), rcToReturn=rc)
        return
      endif
  enddo    ! end  krow

  !-----------------------------------------------------------------------------
  ! determine the actual number of grids specified in the file by counting 
  ! lines not starting with the continuation symbol '&'. The number of actual
  ! grid specifications in the table is less than or equal to 'nrows' the number 
  ! of rows in the table. A new grid entry in a particular row is indicated by
  ! a non-zero value in 'new_row'. A value of zero in the array indicates that
  ! that that row starts with a continiued line. The non-zero value indicates 
  ! the number of the current grid being read.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return
  allocate( new_row(nrows), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array new_row "//     &
     " in read_redistribution_grid", rcToReturn=rc)) then
  endif


  !-----------------------------------------------------------------------------
  ! count the number of actual grids (less than or equal to number of table rows)
  !-----------------------------------------------------------------------------
  ngrid = 0
  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
     if( trim(adjustL(ltmp)) == "&" ) then
     ! continuation line
        new_row(krow) = 0
     else
        ngrid = ngrid + 1
        new_row(krow) =  ngrid
     endif
  enddo    ! end  krow
  ngrids = ngrid

  !-----------------------------------------------------------------------------
  ! allocate storage for the grid information, based on the calculated number of
  ! separate grid entries
  !-----------------------------------------------------------------------------
  allocate( grid(ngrid), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array ncolumns in "// &
     " read_redistribution_grid", rcToReturn=rc)) then
  endif
  allocate( tmp_grid(ngrid), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array ncolumns in "// &
     " read_redistribution_grid", rcToReturn=rc)) then
  endif

  !-----------------------------------------------------------------------------
  ! Read the grid specifications from the table:
  ! (1) start at the top of the table.
  ! (2) read the row elements until the end of the row is reached.
  ! (3) determine if all the elements are read; 
  !     (a) if not advance to the next line and continue to read elements until
  !         the end of the line is reached - repeat (3)
  !     (b) if all the elements read, skip to next row and repeat (2)
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! move to the next line in the table and confirm that (1) the line doesn't
  ! start with a continuation symbol, and (2) that the line isn't empty.
  !-----------------------------------------------------------------------------
  igrid= 0
  do krow=1,nrows

! drs debug print*,'krow',krow,' new row columns',new_row(krow),ncolumns(krow) 

     ! new grid specification - not continuation symbol and not end of row
     if( new_row(krow) /= 0 .and. ncolumns(krow) > 0 ) then
        call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
        if (ESMF_LogFoundError(localrc,                                     &
          msg="unable to go to next line in table " // trim(descriptor_label),     &
          rcToReturn=rc)) return

        ! extract rank of current grid
        call ESMF_ConfigGetAttribute(localcf, grank, rc=localrc)
        if (ESMF_LogFoundError(localrc, msg="unable to get grank",              &
           rcToReturn=rc)) return

        irank = 1        ! grid element being read (<= rank)
        kelements = 1    ! row element just read
        irow = krow   
        igrid = igrid +1 ! new grid description
        if( igrid > ngrid ) then
           call ESMF_LogSetError( ESMF_FAILURE,                             &
                 msg="attempting to access a higher index grid than exists.",      &
                 rcToReturn=rc)
           return
        endif
        ! allocate workspace
        grid(igrid)%grank = grank
        allocate( grid(igrid)%gtype(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gtype in  read_redistribution_grid", rcToReturn=rc)) then
        endif

        allocate( grid(igrid)%gunits(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gunits in  read_redistribution_grid", rcToReturn=rc)) then
        endif
        allocate( grid(igrid)%gsize(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//       &
           " gsize in  read_redistribution_grid", rcToReturn=rc)) then
        endif
        allocate( grid(igrid)%grange(grank,2), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="real array "//          &
           " grange in  read_redistribution_grid", rcToReturn=rc)) then
        endif

        tmp_grid(igrid)%grank = grank
        allocate( tmp_grid(igrid)%gtype(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gtype in  read_redistribution_grid", rcToReturn=rc)) then
        endif
        allocate( tmp_grid(igrid)%gunits(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gunits in  read_redistribution_grid", rcToReturn=rc)) then
        endif
        allocate( tmp_grid(igrid)%gsize(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//       &
           " gsize in  read_redistribution_grid", rcToReturn=rc)) then
        endif
        allocate( tmp_grid(igrid)%grange(grank,2), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="real array "//          &
           " grange in  read_redistribution_grid", rcToReturn=rc)) then
        endif

!       print*,'irow',irow
        ! read row elements until grid rank is reached
        do while ( irank <= grank )
!       print*,'irank/grank',irank,grank
           ! if haven't reached end of line, read grid type
           call read_table_string(gtype,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%gtype(irank)%string = gtype 
           tmp_grid(igrid)%gtype(irank)%string = gtype 

           !--------------------------------------------------------------------
           ! read grid size
           !--------------------------------------------------------------------
           call read_table_integer(gsize,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           
           ! sanity check - specification size of grid dimensions must be
           ! positive, but not huge.
           if( (gsize <= 0).or.(gsize > Harness_Max_Size) ) then
              localrc = ESMF_FAILURE
              write(ltmp,"(i9)") gsize
              call ESMF_LogSetError(ESMF_FAILURE,msg="grid specifier size "//   &
                 "is not within acceptable range " // ltmp,                    &
                 rcToReturn=rc)
              return
           endif

           grid(igrid)%gsize(irank) = gsize 
           tmp_grid(igrid)%gsize(irank) = gsize 

           !--------------------------------------------------------------------
           ! read minimum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmin,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%grange(irank,1) = gmin 
           tmp_grid(igrid)%grange(irank,1) = gmin 

           !--------------------------------------------------------------------
           ! read maximum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmax,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%grange(irank,2) = gmax 
           tmp_grid(igrid)%grange(irank,2) = gmax 

           !--------------------------------------------------------------------
           ! read grid units
           !--------------------------------------------------------------------
           call read_table_string(gunits,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%gunits(irank)%string = gunits
           tmp_grid(igrid)%gunits(irank)%string = gunits

           !--------------------------------------------------------------------
           ! increment grid rank and loop
           !--------------------------------------------------------------------
           irank = irank + 1 

        end do ! while
     endif    !

     !--------------------------------------------------------------------------
     ! move to next row 
     !--------------------------------------------------------------------------
  enddo    ! end  krow

  !-----------------------------------------------------------------------------
  ! deallocate workspace
  !-----------------------------------------------------------------------------
  deallocate( ncolumns )

  !-----------------------------------------------------------------------------
  ! clean up CF     
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, rc=localrc) 
  if( ESMF_LogFoundError(localrc, msg="cannot destroy config object",            &
                            rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! set error code to SUCCESS
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_redistribution_grid
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine read_regridding_grid(lfilename, ngrids, src_grid, dst_grid,        &
                                 testfunction, rc)
  !-----------------------------------------------------------------------------
  ! routine to read the grid specifier file for a regridding test. The routine
  ! reads a pair (source and destination) of grid specification needed for the 
  ! regridding test.
  !
  ! The grid specification takes the form of a table with row entries
  ! (0) grid rank
  ! and (5 * grid rank) remaining entries consisting of cycles of
  ! (1) grid type
  ! (2) grid size
  ! (3) grid minimum range
  ! (4) grid maximum range
  ! (5) grid units
  ! Acceptable values for the units are: DEGREE(S), DEGREE(S)_EAST,
  ! DEGREE(S)_NORTH, DEG, DEG_E, DEG_N, RADIANS, RAD, METERS, M, KILOMETERS, KM.
  ! (6) test function
  ! 
  ! NOTES:
  ! Both the grid type and units should be enclosed by either single or double 
  ! quotes to insure proper parsing by the ESMF_Config calls.
  !
  !-----------------------------------------------------------------------------

  ! arguments
  character(THARN_MAXSTR), intent(in   ) :: lfilename
  type(grid_specification_record), pointer :: src_grid(:), dst_grid(:)
  type(test_function_record), pointer :: testfunction(:)
  integer, intent(  out) :: ngrids
  integer, intent(inout) :: rc

  ! local parameters
  character(THARN_MAXSTR), parameter :: descriptor_label = "map_regrid::"

  ! local esmf types
  type(ESMF_Config) :: localcf

  ! local character strings
  character(THARN_MAXSTR) :: ltmp, lchar, lnumb
  ! character(THARN_MAXSTR) :: lchartmp
  character(THARN_MAXSTR) :: gtype, gunits, gtag
  type(character_array) :: wchar(10)

  logical :: flag

  ! local integer variables
  integer :: ntmp, grank, gsize
  integer :: irow, krow, nrows, igrid, ngrid, irank, kelements
  integer :: iTFun, k, out_counter
  integer, allocatable :: ncolumns(:), new_row(:)
  integer :: localrc ! local error status
  integer :: allocRcToTest

  ! local real variables
  real(ESMF_KIND_R8) :: gmin, gmax, tmp
  ! real(ESMF_KIND_R8) :: tmpchar

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot create config object",            &
                            rcToReturn=rc) ) return

  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot load config file " // trim( lfilename ),                      &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------- 
  ! extract the grid type specifier as sanity check
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'map_type:', rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label map_type", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label map_type:",                                 &
         rcToReturn=rc) ) return

  if( trim(adjustL( ltmp )) /= 'REGRID' ) then 
     call ESMF_LogSetError(                                                 &
     ESMF_FAILURE, msg="Wrong grid type for regrid test in file " //               &
     trim( lfilename ),  rcToReturn=rc)
     return
  endif
  !-----------------------------------------------------------------------------
  ! search for the grid specifier table
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
     msg="cannot find config label " // trim(descriptor_label),                    &
      rcToReturn=rc) ) return
  !-----------------------------------------------------------------------------
  ! determine the total number of table rows, continue only if not empty
  ! NOTE: the number of table rows >= number of grid entries due to the
  ! possibility of continued lines.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigGetDim(localcf, nrows, ntmp, label=trim(descriptor_label),         &
           rc=localrc)
  if( ESMF_LogFoundError(localrc,                                           &
     msg="cannot get descriptor table size in file " // trim(lfilename),           &
     rcToReturn=rc) ) return

  if( nrows .le. 0 ) then
     call ESMF_LogSetError( ESMF_FAILURE,                                   &
             msg="grid specifier table is empty in file " //trim(lfilename),       &
             rcToReturn=rc)
     return
  endif
  !-----------------------------------------------------------------------------
  ! extract the table column lengths of this file
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
     msg="cannot find config label " // trim(descriptor_label),                    &
     rcToReturn=rc) ) return

  allocate( ncolumns(nrows), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array ncolumns"//     &
     " in read_regridding_grid", rcToReturn=rc)) then
  endif


  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     if( ESMF_LogFoundError(localrc,                                        &
        msg="cannot advance to next line of table " //                             &
        trim(descriptor_label) // " in file " // trim(lfilename),              &
        rcToReturn=rc) ) return


      ncolumns(krow) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(krow) .lt. 1 ) then
        write(lchar,"(i5)") krow
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                 msg="problem reading line " // trim(adjustl(lchar)) //            &
                 " of table in file " // trim(lfilename), rcToReturn=rc)
        return
      endif
  enddo    ! end  krow
  !-----------------------------------------------------------------------------
  ! determine the actual number of grids specified in the file by counting 
  ! lines not starting with the continuation symbol '&'. The number of actual
  ! grid specifications in the table is less than or equal to 'nrows' the number 
  ! of rows in the table. A new grid entry in a particular row is indicated by
  ! a non-zero value in 'new_row'. A value of zero in the array indicates that
  ! that that row starts with a continiued line. The non-zero value indicates 
  ! the number of the current grid being read.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
     msg="cannot find config label " // trim(descriptor_label),                    &
     rcToReturn=rc) ) return
  allocate( new_row(nrows), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array new_row"//      &
     " in read_regridding_grid", rcToReturn=rc)) then
  endif


  !-----------------------------------------------------------------------------
  ! count the number of actual grids (less than or equal to number of table rows)
  !-----------------------------------------------------------------------------
  ngrid = 0
  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
     if( trim(adjustL(ltmp)) == "&" ) then
     ! continuation line
        new_row(krow) = 0
     else
        ngrid = ngrid + 1
        new_row(krow) =  ngrid
     endif
  enddo    ! end  krow
  ngrids = ngrid
  !-----------------------------------------------------------------------------
  ! allocate storage for the grid information based on the calculated number of
  ! separate grid entries
  !-----------------------------------------------------------------------------
  allocate( src_grid(ngrid), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="grid type src_grid"//         &
     " in read_regridding_grid", rcToReturn=rc)) then
  endif
  allocate( dst_grid(ngrid), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="grid type dst_grid "//        &
     " in read_regridding_grid", rcToReturn=rc)) then
  endif
  allocate( testfunction(ngrid), stat=allocRcToTest  )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="test type"//                  &
     " in read_regridding_grid", rcToReturn=rc)) then
  endif
  !-----------------------------------------------------------------------------
  ! Read the grid specifications from the table:
  ! (1) start at the top of the table.
  ! (2) read the row elements until the end of the row is reached.
  ! (3) determine if all the elements are read; 
  !     (a) if not advance to the next line and continue to read elements until
  !         the end of the line is reached - repeat (3)
  !     (b) if all the elements read, skip to next row and repeat (2)
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
     msg="cannot find config label " // trim(descriptor_label),                    &
     rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! move to the next line in the table and confirm that (1) the line doesn't
  ! start with a continuation symbol, and (2) that the line isn't empty.
  !-----------------------------------------------------------------------------
  irow = 1      ! start with first row
  igrid = 0
  !-----------------------------------------------------------------------------
  ! as long as the current row is within the bounds of the table process entry
  !-----------------------------------------------------------------------------
  out_counter = 0
  do while(irow <= nrows)
     if (new_row(irow) == 0 ) exit
     !--------------------------------------------------------------------------
     ! new grid specification - not continuation symbol and not end of row
     !--------------------------------------------------------------------------
     if( ncolumns(irow) > 0 ) then
        call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
        kelements = 0    ! no elements of the row have been read
        igrid = igrid +1 ! new grid description

        !-----------------------------------------------------------------------
        ! extract rank of current grid
        !-----------------------------------------------------------------------
        call ESMF_ConfigGetAttribute(localcf, grank)
        !-----------------------------------------------------------------------
        ! check that grid rank should be between 1 and 7
        !-----------------------------------------------------------------------
        if( grank < 1 .or. grank >7 ) then 
           write(lchar,"(i5)") irow
           write(lnumb,"(i5)") grank
           call ESMF_LogSetError(ESMF_FAILURE,msg="unacceptable rank, should"   &
                // " be " // "> 1 and <= 7. Rank is " // trim(lnumb) //        &
                ". On line " //                                                &
                trim(lchar) // " of table " // trim(descriptor_label) //       &
                " in file " // trim(lfilename), rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        ! allocate workspace
        !-----------------------------------------------------------------------
        src_grid(igrid)%grank = grank
        allocate( src_grid(igrid)%gtype(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gtype in read_regridding_grid", rcToReturn=rc)) then
        endif

        allocate( src_grid(igrid)%gunits(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gunits in read_regridding_grid", rcToReturn=rc)) then
        endif

        allocate( src_grid(igrid)%gsize(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//       &
           " gsize in read_regridding_grid", rcToReturn=rc)) then
        endif

        allocate( src_grid(igrid)%grange(grank,2), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="real array "//          &
           " grange in read_regridding_grid", rcToReturn=rc)) then
        endif

        dst_grid(igrid)%grank = grank
        allocate( dst_grid(igrid)%gtype(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " gtype in read_regridding_grid", rcToReturn=rc)) then
        endif

        allocate( dst_grid(igrid)%gunits(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="char array "//          &
           " grank in read_regridding_grid", rcToReturn=rc)) then
        endif

        allocate( dst_grid(igrid)%gsize(grank), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//       &
           " gsize in read_regridding_grid", rcToReturn=rc)) then
        endif

        allocate( dst_grid(igrid)%grange(grank,2), stat=allocRcToTest )
        if (ESMF_LogFoundAllocError(allocRcToTest, msg="real array "//          &
           " grange in read_regridding_grid", rcToReturn=rc)) then
        endif
        !-----------------------------------------------------------------------
        ! Source Grid
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        ! read source grid tag
        !-----------------------------------------------------------------------
           kelements = 1
         ! print*,'before tag',kelements, irow, nrows, ncolumns(irow), new_row(irow)
        call read_table_string(gtag,                                           &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
         ! print*,'after tag',kelements, irow, nrows, ncolumns(irow), new_row(irow)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return
        ! if tag not equal SRC post error
        if( trim(adjustL(gtag)) /= 'SRC' ) then
           call ESMF_LogSetError( ESMF_FAILURE,                             &
                 msg="Source flag expected but not found in regridding" //         &
                 " grid specifier file" // trim(lfilename), rcToReturn=rc)
           return
        endif
        !-----------------------------------------------------------------------
        ! read row elements until grid rank is reached
        !-----------------------------------------------------------------------
        do irank=1, grank
           !--------------------------------------------------------------------
           ! if haven't reached end of line, read source grid type
           !--------------------------------------------------------------------
           call read_table_string(gtype,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           src_grid(igrid)%gtype(irank)%string = gtype 

           !--------------------------------------------------------------------
           ! read source grid size
           !--------------------------------------------------------------------
           call read_table_integer(gsize,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           src_grid(igrid)%gsize(irank) = gsize 
           !--------------------------------------------------------------------
           ! read source minimum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmin,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           src_grid(igrid)%grange(irank,1) = gmin 

           !--------------------------------------------------------------------
           ! read source maximum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmax,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           src_grid(igrid)%grange(irank,2) = gmax 

           !--------------------------------------------------------------------
           ! read source grid units
           !--------------------------------------------------------------------
           call read_table_string(gunits,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           src_grid(igrid)%gunits(irank)%string = gunits
        end do !

        !-----------------------------------------------------------------------
        ! Destination Grid
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        ! read destination grid tag
        !-----------------------------------------------------------------------
        call read_table_string(gtag,                                           &
                               kelements, irow, nrows, ncolumns, new_row,      &
                               lfilename, descriptor_label, localcf, localrc)  
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return
        ! if tag not equal DST post error
        if( trim(adjustL(gtag)) /= 'DST' ) then
           call ESMF_LogSetError( ESMF_FAILURE,                             &
                 msg="Destination flag expected but not found in regridding" //    &
                 " grid specifier file " // trim(lfilename), rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        ! read row elements until grid rank is reached
        !-----------------------------------------------------------------------
        do irank=1, grank
           !--------------------------------------------------------------------
           ! if haven't reached end of line, read destination grid type
           !--------------------------------------------------------------------
           call read_table_string(gtype,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           dst_grid(igrid)%gtype(irank)%string = gtype 

           !--------------------------------------------------------------------
           ! read destination grid size
           !--------------------------------------------------------------------
           call read_table_integer(gsize,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           dst_grid(igrid)%gsize(irank) = gsize 

           !--------------------------------------------------------------------
           ! read destination minimum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmin,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           dst_grid(igrid)%grange(irank,1) = gmin 

           !--------------------------------------------------------------------
           ! read destination maximum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmax,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           dst_grid(igrid)%grange(irank,2) = gmax 

           !--------------------------------------------------------------------
           ! read destination grid units
           !--------------------------------------------------------------------
           call read_table_string(gunits,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           dst_grid(igrid)%gunits(irank)%string = gunits

        end do !

        !-----------------------------------------------------------------------
        ! Test Function
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        ! read destination grid tag
        !-----------------------------------------------------------------------
        call read_table_string(gtag,                                           &
                               kelements, irow, nrows, ncolumns, new_row,      &
                               lfilename, descriptor_label, localcf, localrc)  
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return
        ! if tag not equal FUNCTION post error
        if( trim(adjustL(gtag)) /= 'FUNCTION' ) then
           call ESMF_LogSetError( ESMF_FAILURE,                             &
                 msg="Test Function flag expected but not found in regridding" //  &
                 " grid specifier file " // trim(lfilename), rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        ! extract test function and parameters
        !-----------------------------------------------------------------------
        iTFun = 0  ! initialize test function counter

        !-----------------------------------------------------------------------
        ! read first function types
        !-----------------------------------------------------------------------
        call read_table_string(gtag,                                           &
                               kelements, irow, nrows, ncolumns, new_row,      &
                               lfilename, descriptor_label, localcf, localrc)  
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return

        do while (  trim(adjustL(gtag)) /= 'END' )

           !--------------------------------------------------------------------
           ! read function types
           !--------------------------------------------------------------------
           iTFun = iTFun + 1  ! count number of specified test functions
           wchar(iTFun)%string = gtag

           ! read next paramter
           call read_table_string(gtag,                                        &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

        end do ! while

        !-----------------------------------------------------------------------
        ! allocate character array for test functions & copy values from work array
        !-----------------------------------------------------------------------
        if(iTFun > 0) then
           allocate( testfunction(igrid)%param(iTFun-1), stat=allocRcToTest )
           if (ESMF_LogFoundAllocError(allocRcToTest, msg="type in "//          &
               " read_regridding_grid", rcToReturn=rc)) then
           endif
        endif

        testfunction(igrid)%prank = iTFun-1

        testfunction(igrid)%string = wchar(1)%string

        do k=2,iTFun
            lchar = trim( wchar(k)%string )
            read(lchar,*) tmp
            testfunction(igrid)%param(k-1) = tmp
        enddo

     endif    !

     !---------------------------------------------------------------------------
     ! both source and destination specifications have been read, move to next
     ! entry - check new row to make certain it is a new entry and not a
     ! continuation.
     !---------------------------------------------------------------------------
     if( irow+1 <= nrows ) then
        if( new_row(irow+1) /= 0)  then
          ! if there is a next row and it doesn't have a continuation symbol
          irow = irow + 1
        else
          ! error next line should be a new entry but instead a continuation
          ! symbol was found
          write(lchar,"(i5)") irow+1
          write(lnumb,"(i5)") irank
          call ESMF_LogSetError(ESMF_FAILURE,msg="next line in table " //      &
                trim(descriptor_label) // " should be a new entry, but " //   &
                "instead a continuation symbol was found. Line " //           &
                trim(lchar) // " of table " // trim(descriptor_label) //      &
                " in file " //trim(lfilename) // " had a continuation"        &
                // " symbol." , rcToReturn=rc)
                return
        endif
     elseif(irow+1 > nrows .and. trim(gtag) /= "END") then
       ! we should be done and can drop out, but there is no end tag
       call ESMF_LogSetError(ESMF_FAILURE,msg="should be at end of " //       &
                "table " // trim(descriptor_label) // " but no end tag" // &
                " found. File " // trim(lfilename) , rcToReturn=rc)
                return
     else
       ! we are at the end of the table so finish up.
       irow = irow+1
     endif

  end do    ! while

  !-----------------------------------------------------------------------------
  ! deallocate workspace
  !-----------------------------------------------------------------------------
  deallocate( ncolumns )

  !-----------------------------------------------------------------------------
  ! clean up CF     
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, rc=localrc) 
  if( ESMF_LogFoundError(localrc, msg="cannot destroy config object",            &
                            rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! set error code to SUCCESS
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_regridding_grid
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine generate_rectilinear_coord(grid,localrc)
  !-----------------------------------------------------------------------------
  ! routine that generates the coordinates for a separated rectilinear grid 
  ! as specified by the specification files.
  !-----------------------------------------------------------------------------

  ! arguments
  integer, intent(  out) :: localrc
  type(grid_specification_record), pointer :: grid(:)

  ! local ESMF types

  ! local character strings
! character(THARN_MAXSTR) :: ltmp, lchar

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------
  end subroutine generate_rectilinear_coord 
  !-----------------------------------------------------------------------------


!===============================================================================
  end module ESMF_TestHarnessGridMod
!===============================================================================
