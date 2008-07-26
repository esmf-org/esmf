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
#include <ESMF.h>
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

  use ESMF_Mod
  use ESMF_TestHarnessTypesMod
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

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lfilename

  ! local grid records 
  type(sized_char_array), pointer :: testfunction(:)

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",            &
                            rcToReturn=rc) ) return

  lfilename = Gfile%filename
! print*,'Opening Grid specifier file  ',trim( lfilename )
  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot load config file " // trim( lfilename ),                      &
         rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! Search and extract the grid type specifier
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'grid_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label grid_type:", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot read config label grid_type:" , rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! Read the grid specifier file. The 'grid_type' argument specifies the type 
  ! of grid specification to be read ( redistrbution or remapping ).
  !-----------------------------------------------------------------------------
  select case(trim(adjustL(ltmp)) )

     case('REDISTRIBUTION')
       print*,' read grid specification for redistribution test'
       call read_redistribution_grid(lfilename, Gfile%nGspecs, Gfile%src_grid, &
                Gfile%dst_grid,localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return

     case('REMAP')
!      print*,' read grid specification for remapping test'
       call read_remapping_grid(lfilename, Gfile%nGspecs, Gfile%src_grid,      &
                Gfile%dst_grid,testfunction, localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return

     case default
       ! error
       call ESMF_LogMsgSetError( ESMF_FAILURE,                                 &
              "unknown grid type in " // trim(lfilename),                      &
              rcToReturn=rc)
       return
     end select

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
  character(ESMF_MAXSTR), intent(in   ) :: lfilename
  type(grid_specification_record), pointer :: grid(:)      ! source grid spec
  type(grid_specification_record), pointer :: tmp_grid(:)  ! duplicate copy of
                                                           ! the grid specifier for 
                                                           ! the destination grid
  integer, intent(  out) :: ngrids
  integer, intent(inout) :: rc

  ! local parameters
  character(ESMF_MAXSTR), parameter :: descriptor_label = "grid_redist::"

  ! local esmf types
  type(ESMF_Config) :: localcf

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar
  character(ESMF_MAXSTR) :: gtype, gunits

  logical :: flag = .true.

  ! local integer variables
  integer :: ntmp, grank, gsize
  integer :: irow, krow, nrows, igrid, ngrid, irank, kelements
  integer, allocatable :: ncolumns(:), new_row(:)

  ! local real variables
  real(ESMF_KIND_R8) :: gmin, gmax

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",            &
                            rcToReturn=rc) ) return

  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot load config file " // trim( lfilename ),                      &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------- 
  ! extract the grid type specifier as sanity check
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'grid_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label grid_type", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label grid_type:",                                &
         rcToReturn=rc) ) return

  if( trim(adjustL( ltmp )) /= 'REDISTRIBUTION' ) then
     call ESMF_LogMsgSetError(    &
            ESMF_FAILURE, "Wrong grid type for redist test in file " //        &
            trim( lfilename ), rcToReturn=rc) 
     return
  endif
  
  !-----------------------------------------------------------------------------
  ! search for the grid specifier table
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) print*,' find descriptor label failed'
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! determine the total number of table rows, continue only if not empty
  ! NOTE: the number of table rows >= number of grid entries due to the
  ! possibility of continued lines.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigGetDim(localcf, nrows, ntmp, trim(descriptor_label),         &
                         rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot get descriptor table size in file " // trim(lfilename),       &
         rcToReturn=rc) ) return

  if( nrows .le. 0 ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
             "grid specifier table is empty in file " //trim(lfilename),       &
             rcToReturn=rc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! extract the table column lengths of this file
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return

  allocate( ncolumns(nrows) )

  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                        &
             "cannot advance to next line of table " //                        &
              trim(descriptor_label) // " in file " // trim(lfilename),        &
              rcToReturn=rc) ) return


      ncolumns(krow) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(krow) .lt. 6 ) then
        write(lchar,"(i5)") krow
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                 "problem reading line " // trim(adjustl(lchar)) //            &
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
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label " // trim(descriptor_label),                &
         rcToReturn=rc) ) return
  allocate( new_row(nrows) )

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
  allocate( grid(ngrid) )
  allocate( tmp_grid(ngrid) )

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
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label " // trim(descriptor_label),                &
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
        if (ESMF_LogMsgFoundError(localrc,                                     &
          "unable to go to next line in table " // trim(descriptor_label),     &
          rcToReturn=rc)) return

        ! extract rank of current grid
        call ESMF_ConfigGetAttribute(localcf, grank, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, "unable to get grank",              &
           rcToReturn=rc)) return

        irank = 1        ! grid element being read (<= rank)
        kelements = 1    ! row element just read
        irow = krow   
        igrid = igrid +1 ! new grid description
        if( igrid > ngrid ) then
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "attempting to access a higher index grid than exists.",      &
                 rcToReturn=rc)
           return
        endif
        ! allocate workspace
        grid(igrid)%grank = grank
        allocate( grid(igrid)%gtype(grank) )
        allocate( grid(igrid)%gunits(grank) )
        allocate( grid(igrid)%gsize(grank) )
        allocate( grid(igrid)%grange(grank,2) )

        tmp_grid(igrid)%grank = grank
        allocate( tmp_grid(igrid)%gtype(grank) )
        allocate( tmp_grid(igrid)%gunits(grank) )
        allocate( tmp_grid(igrid)%gsize(grank) )
        allocate( tmp_grid(igrid)%grange(grank,2) )

!       print*,'irow',irow
        ! read row elements until grid rank is reached
        do while ( irank <= grank )
!       print*,'irank/grank',irank,grank
           ! if haven't reached end of line, read grid type
           call read_table_string(gtype,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%gtype(irank)%string = gtype 
           tmp_grid(igrid)%gtype(irank)%string = gtype 

           !--------------------------------------------------------------------
           ! read grid size
           !--------------------------------------------------------------------
           call read_table_integer(gsize,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           
           ! sanity check - specification size of grid dimensions must be
           ! positive, but not huge.
           if( (gsize <= 0).or.(gsize > Harness_Max_Size) ) then
              localrc = ESMF_FAILURE
              write(ltmp,"(i9)") gsize
              call ESMF_LogMsgSetError(ESMF_FAILURE,"grid specifier size "//   &
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
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%grange(irank,1) = gmin 
           tmp_grid(igrid)%grange(irank,1) = gmin 

           !--------------------------------------------------------------------
           ! read maximum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmax,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           grid(igrid)%grange(irank,2) = gmax 
           tmp_grid(igrid)%grange(irank,2) = gmax 

           !--------------------------------------------------------------------
           ! read grid units
           !--------------------------------------------------------------------
           call read_table_string(gunits,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
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
  ! set error code to SUCCESS
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_redistribution_grid
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine read_remapping_grid(lfilename, ngrids,                            &
                                 src_grid, dst_grid, testfunction, rc)
  !-----------------------------------------------------------------------------
  ! routine to read the grid specifier file for a remapping test. The routine
  ! reads a pair (source and destination) of grid specification needed for the 
  ! remapping test.
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
  ! 
  ! NOTES:
  ! Both the grid type and units should be enclosed by either single or double 
  ! quotes to insure proper parsing by the ESMF_Config calls.
  !
  !-----------------------------------------------------------------------------

  ! arguments
  character(ESMF_MAXSTR), intent(in   ) :: lfilename
  type(grid_specification_record), pointer :: src_grid(:), dst_grid(:)
  type(sized_char_array), pointer :: testfunction(:)
  integer, intent(  out) :: ngrids
  integer, intent(inout) :: rc

  ! local parameters
  character(ESMF_MAXSTR), parameter :: descriptor_label = "grid_remap::"

  ! local esmf types
  type(ESMF_Config) :: localcf

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar
  character(ESMF_MAXSTR) :: gtype, gunits, gtag
  type(character_array) :: wchar(10)

  logical :: flag = .true.

  ! local integer variables
  integer :: ntmp, grank, gsize
  integer :: irow, krow, nrows, igrid, ngrid, irank, kelements
  integer :: iTFun, k
  integer, allocatable :: ncolumns(:), new_row(:)

  ! local real variables
  real(ESMF_KIND_R8) :: gmin, gmax

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",            &
                            rcToReturn=rc) ) return

  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot load config file " // trim( lfilename ),                      &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------- 
  ! extract the grid type specifier as sanity check
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'grid_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label grid_type", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
         "cannot find config label grid_type:",                                &
         rcToReturn=rc) ) return

  if( trim(adjustL( ltmp )) /= 'REMAP' ) then 
     call ESMF_LogMsgSetError(                                                 &
     ESMF_FAILURE, "Wrong grid type for remap test in file " //                &
     trim( lfilename ),  rcToReturn=rc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! search for the grid specifier table
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
     "cannot find config label " // trim(descriptor_label),                    &
      rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! determine the total number of table rows, continue only if not empty
  ! NOTE: the number of table rows >= number of grid entries due to the
  ! possibility of continued lines.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigGetDim(localcf, nrows, ntmp, trim(descriptor_label),         &
           rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                           &
     "cannot get descriptor table size in file " // trim(lfilename),           &
     rcToReturn=rc) ) return

  if( nrows .le. 0 ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                   &
             "grid specifier table is empty in file " //trim(lfilename),       &
             rcToReturn=rc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! extract the table column lengths of this file
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                           &
     "cannot find config label " // trim(descriptor_label),                    &
     rcToReturn=rc) ) return

  allocate( ncolumns(nrows) )

  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                        &
        "cannot advance to next line of table " //                             &
        trim(descriptor_label) // " in file " // trim(lfilename),              &
        rcToReturn=rc) ) return


      ncolumns(krow) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(krow) .lt. 1 ) then
        write(lchar,"(i5)") krow
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
                 "problem reading line " // trim(adjustl(lchar)) //            &
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
  if( ESMF_LogMsgFoundError(localrc,                                           &
     "cannot find config label " // trim(descriptor_label),                    &
     rcToReturn=rc) ) return
  allocate( new_row(nrows) )

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
  allocate( src_grid(ngrid), dst_grid(ngrid), testfunction(ngrid) )

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
  if( ESMF_LogMsgFoundError(localrc,                                           &
     "cannot find config label " // trim(descriptor_label),                    &
     rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! move to the next line in the table and confirm that (1) the line doesn't
  ! start with a continuation symbol, and (2) that the line isn't empty.
  !-----------------------------------------------------------------------------
  igrid= 0
  do krow=1,nrows

     print*,'krow',krow,' new row columns',new_row(krow),ncolumns(krow) 

     ! new grid specification - not continuation symbol and not end of row
     if( new_row(krow) /= 0 .and. ncolumns(krow) > 0 ) then
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
        ! extract rank of current grid
        call ESMF_ConfigGetAttribute(localcf, grank)
        irank = 1        ! grid element being read (<= rank)
        kelements = 1    ! row element just read
        irow = krow   
        igrid = igrid +1 ! new grid description
        if( igrid > ngrid ) then
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "attempting to access a higher index grid than exists.",      &
                 rcToReturn=rc)
           return
        endif
        ! allocate workspace
        src_grid(igrid)%grank = grank
        allocate( src_grid(igrid)%gtype(grank) )
        allocate( src_grid(igrid)%gunits(grank) )
        allocate( src_grid(igrid)%gsize(grank) )
        allocate( src_grid(igrid)%grange(grank,2) )
        dst_grid(igrid)%grank = grank
        allocate( dst_grid(igrid)%gtype(grank) )
        allocate( dst_grid(igrid)%gunits(grank) )
        allocate( dst_grid(igrid)%gsize(grank) )
        allocate( dst_grid(igrid)%grange(grank,2) )

        !-----------------------------------------------------------------------
        ! Source Grid
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        ! read source grid tag
        !-----------------------------------------------------------------------
        call read_table_string(gtag,                                           &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return
        ! if tag not equal SRC post error
        if( trim(adjustL(gtag)) /= 'SRC' ) then
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "Source flag expected but not found in remapping" //          &
                 " grid specifier file" // trim(lfilename), rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        ! read row elements until grid rank is reached
        !-----------------------------------------------------------------------
        do while ( irank <= grank )
           !--------------------------------------------------------------------
           ! if haven't reached end of line, read source grid type
           !--------------------------------------------------------------------
           call read_table_string(gtype,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           src_grid(igrid)%gtype(irank)%string = gtype 

           !--------------------------------------------------------------------
           ! read source grid size
           !--------------------------------------------------------------------
           call read_table_integer(gsize,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           src_grid(igrid)%gsize(irank) = gsize 

           !--------------------------------------------------------------------
           ! read source minimum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmin,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           src_grid(igrid)%grange(irank,1) = gmin 

           !--------------------------------------------------------------------
           ! read source maximum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmax,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           src_grid(igrid)%grange(irank,2) = gmax 

           !--------------------------------------------------------------------
           ! read source grid units
           !--------------------------------------------------------------------
           call read_table_string(gunits,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           src_grid(igrid)%gunits(irank)%string = gunits

           !--------------------------------------------------------------------
           ! increment grid rank and loop
           !--------------------------------------------------------------------
           irank = irank + 1 

        end do ! while

        !-----------------------------------------------------------------------
        ! Destination Grid
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        ! read destination grid tag
        !-----------------------------------------------------------------------
        call read_table_string(gtag,                                           &
                               kelements, irow, nrows, ncolumns, new_row,      &
                               lfilename, descriptor_label, localcf, localrc)  
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return
        ! if tag not equal DST post error
        if( trim(adjustL(gtag)) /= 'DST' ) then
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "Destination flag expected but not found in remapping" //     &
                 " grid specifier file " // trim(lfilename), rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        ! reset irank for destination input
        !-----------------------------------------------------------------------
        irank = 1        ! grid element being read (<= rank)

        !-----------------------------------------------------------------------
        ! read row elements until grid rank is reached
        !-----------------------------------------------------------------------
        do while ( irank <= grank )
           !--------------------------------------------------------------------
           ! if haven't reached end of line, read destination grid type
           !--------------------------------------------------------------------
           call read_table_string(gtype,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           dst_grid(igrid)%gtype(irank)%string = gtype 

           !--------------------------------------------------------------------
           ! read destination grid size
           !--------------------------------------------------------------------
           call read_table_integer(gsize,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           dst_grid(igrid)%gsize(irank) = gsize 

           !--------------------------------------------------------------------
           ! read destination minimum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmin,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           dst_grid(igrid)%grange(irank,1) = gmin 

           !--------------------------------------------------------------------
           ! read destination maximum grid range
           !--------------------------------------------------------------------
           call read_table_real(gmax,                                          &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return
           dst_grid(igrid)%grange(irank,2) = gmax 

           !--------------------------------------------------------------------
           ! read destination grid units
           !--------------------------------------------------------------------
           call read_table_string(gunits,                                      &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           dst_grid(igrid)%gunits(irank)%string = gunits

           !--------------------------------------------------------------------
           ! increment grid rank and loop
           !--------------------------------------------------------------------
           irank = irank + 1 

        end do ! while

        !-----------------------------------------------------------------------
        ! Test Function
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        ! read destination grid tag
        !-----------------------------------------------------------------------
        call read_table_string(gtag,                                           &
                               kelements, irow, nrows, ncolumns, new_row,      &
                               lfilename, descriptor_label, localcf, localrc)  
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
               rcToReturn=rc)) return
        ! if tag not equal FUNCTION post error
        if( trim(adjustL(gtag)) /= 'FUNCTION' ) then
           call ESMF_LogMsgSetError( ESMF_FAILURE,                             &
                 "Test Function flag expected but not found in remapping" //   &
                 " grid specifier file " // trim(lfilename), rcToReturn=rc)
           return
        endif

        !-----------------------------------------------------------------------
        !
        !-----------------------------------------------------------------------
        iTFun = 0  ! initialize test function counter

        do while (  trim(adjustL(gtag)) /= 'END' )

           !--------------------------------------------------------------------
           ! read function types
           !--------------------------------------------------------------------
           call read_table_string(gtag,                                        &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, localrc)  
           if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,               &
               rcToReturn=rc)) return

           iTFun = iTFun + 1  ! count number of specified test functions
           wchar(iTFun)%string = gtag

        end do ! while

        !-----------------------------------------------------------------------
        ! allocate character array for test functions & copy values from work array
        !-----------------------------------------------------------------------
        allocate( testfunction(ngrid)%tag(iTFun-1) )

        testfunction(igrid)%tagsize = iTFun-1

        do k=1,iTFun-1
            testfunction(igrid)%tag(k)%string = wchar(k)%string
        enddo

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
  ! set error code to SUCCESS
  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_remapping_grid
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
! character(ESMF_MAXSTR) :: ltmp, lchar

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
