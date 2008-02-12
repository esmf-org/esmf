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
  use ESMF_TestHarnessMod

  implicit none

!-------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

  type rectilinear_grid_record
     integer :: rank                    ! rank of the grid
     type(character_array), pointer :: type(:)  ! type of grid spacing
     type(character_array), pointer :: units(:) ! physical grid units
     integer, pointer :: size(:)        ! number of grid elements along axis
     real(ESMF_KIND_R8), pointer :: range(:,:)     ! physical range of axes
  end type rectilinear_grid_record

  type curvilinear_grid_record
     integer :: rank                    ! rank of the grid
     type(character_array), pointer :: units(:) ! physical grid units
     integer, pointer :: size(:)        ! number of grid elements along axis
  end type curvilinear_grid_record

!-------------------------------------------------------------------------------
! !PUBLIC TYPES:

  type grid_record
     character(ESMF_MAXSTR) :: name
     integer :: topology                ! key representing the geometry of the grid
     integer :: rank                    ! rank of the grid
     integer, pointer :: order(:)       ! axis number, zero for free.
     integer, pointer :: size(:)        ! number of grid elements along axis
     integer, pointer :: stagger(:,:)   ! stagger location (axis,rank)
     logical, pointer :: periodicity(:) ! (rank) periodicity along axis
     logical, pointer :: halo(:)        ! (rank) periodicity along axis
     integer, pointer :: halo_size(:,:) ! (rank,2) halo size along axis
  end type grid_record

  public grid_record
!-------------------------------------------------------------------------------
! PUBLIC METHODS:
  public read_grid_specification

!
!===============================================================================

  contains 

!===============================================================================


  !-----------------------------------------------------------------------------
  subroutine read_grid_specification(lfilename, rc)
  !-----------------------------------------------------------------------------
  ! driver for reading the grid specifier files and constructing the esmf grid
  ! object from an esmf array.
  !
  !-----------------------------------------------------------------------------

  ! arguments
  character(ESMF_MAXSTR), intent(in   ) :: lfilename
  ! global grid object
  integer, intent(  out) :: rc

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar

  ! local integers
  integer:: igrid, ngrid, irank

  ! local grid records 
  type(rectilinear_grid_record), pointer :: rgrid(:)
  type(curvilinear_grid_record), pointer :: cgrid(:)

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",           &
                            rcToReturn=rc) ) return

  print*,'Opening Grid specifier file  ',trim( lfilename )
  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot load config file " // trim( lfilename ),                     &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------
  ! Search and extract the grid type specifier
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'grid_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label grid_type:", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot read config label grid_type:" , rcToReturn=rc) ) return
  print*,'reading grid type:',trim(adjustL(ltmp))
  !-----------------------------------------------------------------------------
  ! Read the grid specifier file. The 'grid_type' argument specifies the type 
  ! of grid to be read.
  !-----------------------------------------------------------------------------
  select case(trim(adjustL(ltmp)) )

     case('RECTILINEAR')
       print*,' read rectilinear grid specification'
       call read_rectilinear_grid(lfilename, ngrid, rgrid, localrc)
       if (localrc .ne. ESMF_SUCCESS) print*,' read rect grid failed'
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return

       ! drs debug
!      print*,' After call in read_grid_specification - ngrid:',ngrid
       ! drs debug
!      do igrid=1, ngrid
!         print*,'grid rank:',rgrid(igrid)%rank
!         do irank=1,rgrid(igrid)%rank
!            print*,'grid type:',rgrid(igrid)%type(irank)%name
!            print*,'size of axis ',irank,':',rgrid(igrid)%size(irank)
!            print*,'axis range:',rgrid(igrid)%range(irank,1),' to ',   &
!                    rgrid(igrid)%range(irank,2)
!            print*,'units:',rgrid(igrid)%units(irank)%name
!         enddo ! irank
!      enddo ! igrid
       ! drs debug
       call generate_rectilinear_coord(rgrid,localrc)
       deallocate( rgrid )
     case('CURVILINEAR')
       print*,' read curvilinear grid specification'
       call read_curvilinear_grid(lfilename, cgrid, localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return
       deallocate( cgrid )

     case default
       ! error
       call ESMF_LogMsgSetError( ESMF_FAILURE,                                 &
              "unknown grid type in " // trim(lfilename),                      &
              rcToReturn=rc)

     end select

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------
  end subroutine read_grid_specification
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine read_rectilinear_grid(lfilename, ngrids, grid, rc)
  !-----------------------------------------------------------------------------
  ! routine to read the grid specifier file for rectilinear grids. 
  !
  ! rectilinear grid specification takes the form of a table with row entries
  ! (0) grid rank
  ! and (5 * grid rank) remaining entries consisting of cycles of
  ! (1) grid type
  ! (2) grid size
  ! (3) grid minimum range
  ! (4) grid maximum range
  ! (5) grid units
  !-----------------------------------------------------------------------------

  ! arguments
  character(ESMF_MAXSTR), intent(in   ) :: lfilename
  type(rectilinear_grid_record), pointer :: grid(:)
  integer, intent(  out) :: ngrids
  integer, intent(inout) :: rc

  ! local parameters
  character(ESMF_MAXSTR), parameter :: descriptor_label = "grid::"

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
  integer :: irow, krow, nrows, igrid, kgrid, ngrid, irank, kelements
  integer :: cpos, dpos, gpos, csize, dsize
  integer, allocatable :: ncolumns(:), new_row(:)

  ! local real variables
  real(ESMF_KIND_R8) :: gmin, gmax

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !----------------------------------------------------------------------------
  ! open the grid file
  !----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",           &
                            rcToReturn=rc) ) return

  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot load config file " // trim( lfilename ),                     &
         rcToReturn=rc) ) return
  !----------------------------------------------------------------------------
  ! extract the grid type specifier as sanity check
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'grid_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label grid_type", rcToReturn=rc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label grid_type:",                               &
         rcToReturn=rc) ) return

  if( trim(adjustL( ltmp )) /= 'RECTILINEAR' ) call ESMF_LogMsgSetError(      &
            ESMF_FAILURE, "Wrong grid type in file "// trim( lfilename ),     &
            rcToReturn=rc)

  !----------------------------------------------------------------------------
  ! search for the grid specifier table
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if (localrc .ne. ESMF_SUCCESS) print*,' find descriptor label failed'
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label " // trim(descriptor_label),               &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------
  ! determine the total number of table rows, continue only if not empty
  ! NOTE: the number of table rows >= number of grid entries due to the
  ! possibility of continued lines.
  !----------------------------------------------------------------------------
  call ESMF_ConfigGetDim(localcf, nrows, ntmp, trim(descriptor_label),         &
                         rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot get descriptor table size in file " // trim(lfilename),      &
         rcToReturn=rc) ) return

  if( nrows .le. 0 ) then
     call ESMF_LogMsgSetError( ESMF_FAILURE,                                  &
             "grid specifier table is empty in file " //trim(lfilename),      &
             rcToReturn=rc)
     return
  endif

  !----------------------------------------------------------------------------
  ! extract the table column lengths of this file
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label " // trim(descriptor_label),               &
         rcToReturn=rc) ) return

  allocate( ncolumns(nrows) )

  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
     if( ESMF_LogMsgFoundError(localrc,                                       &
             "cannot advance to next line of table " //                       &
              trim(descriptor_label) // " in file " // trim(lfilename),       &
              rcToReturn=rc) ) return


      ncolumns(krow) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(krow) .lt. 6 ) then
        write(lchar,"(i5)") krow
        call ESMF_LogMsgSetError( ESMF_FAILURE,                               &
                 "problem reading line " // trim(adjustl(lchar)) //           &
                 " of table in file " // trim(lfilename), rcToReturn=rc)
        return
      endif
  enddo    ! end  krow

! drs debug
  print*,'nrows ',nrows
  do krow=1,nrows
    print*,'krow=',krow,'  ncolumns=',ncolumns(krow)
  enddo
! drs debug

  !----------------------------------------------------------------------------
  ! determine the actual number of grids specified in the file by counting 
  ! lines not starting with the continuation symbol '&'. The number of actual
  ! grids in the table is less than or equal to 'nrows' the number of rows in
  ! the table. A particular row has a new grid entry if the value of 'new_row'
  ! is nonzero. The value zero indicates that that row is a continiued line,
  ! otherwise a nonzero value indicates the number of the current grid being 
  ! read.
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label " // trim(descriptor_label),               &
         rcToReturn=rc) ) return
  allocate( new_row(nrows) )

  !----------------------------------------------------------------------------
  ! count the number of actual grids (less than or equal to number of table rows)
  !----------------------------------------------------------------------------
  ngrid = 0
  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
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

  allocate( grid(ngrid) )

! drs debug
  print*,' number of grids=',ngrid
  do krow=1,nrows
    print*,'krow=',krow,' new_row=',new_row(krow)
  enddo
! drs debug

  !----------------------------------------------------------------------------
  ! Read the grid specifications from the table:
  ! (1) start at the top of the table.
  ! (2) read the row elements until the end of the row is reached.
  ! (3) determine if all the elements are read; 
  !     (a) if not advance to the next line and continue to read elements until
  !         the end of the line is reached - repeat (3)
  !     (b) if all the elements read, skip to next row and repeat (2)
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(descriptor_label), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label " // trim(descriptor_label),               &
         rcToReturn=rc) ) return

  !----------------------------------------------------------------------------
  ! move to the next line in the table and confirm that (1) the line doesn't
  ! start with a continuation symbol, and (2) that the line isn't empty.
  !----------------------------------------------------------------------------
  igrid= 0
  do krow=1,nrows

     print*,'krow',krow,' new row columns',new_row(krow),ncolumns(krow) 

     ! new grid specification - not continuation symbol and not end of row
     if( new_row(krow) /= 0 .and. ncolumns(krow) > 0 ) then
     call ESMF_ConfigNextLine(localcf, flag , rc=localrc)
        ! extract rank of current grid
        call ESMF_ConfigGetAttribute(localcf, grank)
        irank = 1        ! grid element being read (<= rank)
        kelements = 1    ! row element just read
        irow = krow   
        igrid = igrid +1 ! new grid description
        if( igrid > ngrid ) then
           call ESMF_LogMsgSetError( ESMF_FAILURE,                            &
                 "attempting to access a higher index grid than exists.",     &
                 rcToReturn=rc)
        endif
        ! allocate workspace
        grid(igrid)%rank = grank
        allocate( grid(igrid)%type(grank) )
        allocate( grid(igrid)%units(grank) )
        allocate( grid(igrid)%size(grank) )
        allocate( grid(igrid)%range(grank,2) )

        print*,'irow',irow
        ! read row elements until grid rank is reached
        do while ( irank <= grank )
        print*,'irank/grank',irank,grank
           ! if haven't reached end of line, read grid type
           if( kelements < ncolumns(irow) ) then
              call ESMF_ConfigGetAttribute(localcf, gtype, rc=localrc)
              ! if error
              write(lchar,"(i5)") krow
              if( ESMF_LogMsgFoundError(localrc,                              &
                "cannot read row " // trim(adjustL(lchar)) // " of table " // &
                trim(descriptor_label) // "in file " // trim(lfilename),      &
                rcToReturn=rc) ) return

              kelements = kelements + 1
              grid(igrid)%type(irank)%name = gtype 
              !drs debug
          print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow), gtype
              !drs debug


           ! reached end of the row, check if another line exists
           elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
              ! if new line starts with a continuation and there are at least
              ! two columns, advance and read
              if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
                 irow = irow + 1
                 kelements = 1
                 call ESMF_ConfigNextLine(localcf, flag, rc=localrc )
                 ! if error
                 write(lchar,"(i5)") irow
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 ! read and discard continuation symbol
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

               ! kelements = kelements + 1

                 ! read grid type
                 call ESMF_ConfigGetAttribute(localcf, gtype, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 kelements = kelements + 1
                 grid(igrid)%type(irank)%name = gtype 
              !drs debug
          print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow), gtype
              !drs debug

              else
                 ! error continuation line missing, but grid not finished
                 write(lchar,"(i5)") irow
                 call ESMF_LogMsgSetError( ESMF_FAILURE,                      &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc)

              endif     ! new line
           endif     ! read grid type

           !-------------------------------------------------------------------
           ! read grid size
           !-------------------------------------------------------------------
           if( kelements < ncolumns(irow) ) then
           ! read grid type
              call ESMF_ConfigGetAttribute(localcf, gsize, rc=localrc)
              ! if error
              write(lchar,"(i5)") krow
              if( ESMF_LogMsgFoundError(localrc,                              &
                "cannot read row " // trim(adjustL(lchar)) // " of table " // &
                trim(descriptor_label) // "in file " // trim(lfilename),      &
                rcToReturn=localrc) ) return

              kelements = kelements + 1
              grid(igrid)%size(irank) = gsize 
              !drs debug
         print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gsize
              !drs debug

           elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
           ! reached end of the row, check if another line exists
              if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
              ! if new line starts with a continuation and there are at least two
              ! columns, advance and read
                 irow = irow + 1
                 kelements = 1
                 ! if error
                 write(lchar,"(i5)") irow
                 call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 ! read and discard continuation symbol
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return
               ! kelements = kelements + 1

                 ! read grid size
                 call ESMF_ConfigGetAttribute(localcf, gsize, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 kelements = kelements + 1
                 grid(igrid)%size(irank) = gsize 
              !drs debug
          print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gsize
              !drs debug

              else
                 ! error continuation line missing
                 write(lchar,"(i5)") irow
                 call ESMF_LogMsgSetError( ESMF_FAILURE,                      &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc)

              endif    ! new line
           endif    ! read grid size

           !-------------------------------------------------------------------
           ! read minimum grid range
           !-------------------------------------------------------------------
           if( kelements < ncolumns(irow) ) then
           ! read grid type
              call ESMF_ConfigGetAttribute(localcf, gmin, rc=localrc)
              ! if error
              write(lchar,"(i5)") krow
              if( ESMF_LogMsgFoundError(localrc,                              &
                "cannot read row " // trim(adjustL(lchar)) // " of table " // &
                trim(descriptor_label) // "in file " // trim(lfilename),      &
                rcToReturn=localrc) ) return

              kelements = kelements + 1
              grid(igrid)%range(irank,1) = gmin 
              !drs debug
           print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gmin
              !drs debug

           ! reached end of the row, check if another line exists
           elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
              ! if new line starts with a continuation and there are at least
              ! two columns, advance and read
              if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
                 irow = irow + 1
                 kelements = 1
                 ! if error
                 call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
                 write(lchar,"(i5)") irow
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 ! read and discard continuation symbol
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

            !    kelements = kelements + 1

                 ! read minimum grid range
                 call ESMF_ConfigGetAttribute(localcf, gmin, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 kelements = kelements + 1
                 grid(igrid)%range(irank,1) = gmin 
              !drs debug
          print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gmin
              !drs debug

              else
                 ! error continuation line missing, but grid not finished
                 write(lchar,"(i5)") irow
                 call ESMF_LogMsgSetError( ESMF_FAILURE,                      &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc)
              endif     ! new line
           endif    ! read grid range

           !-------------------------------------------------------------------
           ! read maximum grid range
           !-------------------------------------------------------------------
           if( kelements < ncolumns(irow) ) then
           ! read grid type
              call ESMF_ConfigGetAttribute(localcf, gmax, rc=localrc)
              ! if error
              write(lchar,"(i5)") irow
              if( ESMF_LogMsgFoundError(localrc,                              &
                "cannot read row " // trim(adjustL(lchar)) //                 &
                " of table " //trim(descriptor_label) // "in file " //        &
                trim(lfilename), rcToReturn=rc) ) return

              kelements = kelements + 1
              grid(igrid)%range(irank,2) = gmax 
              !drs debug
          print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gmax
              !drs debug

           elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
           ! reached end of the row, check if another line exists
              if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
              ! if new line starts with a continuation and there are at least two
              ! columns, advance and read
                 irow = irow + 1
                 kelements = 1
                 ! if error
                 call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
                 write(lchar,"(i5)") irow
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 ! read and discard continuation symbol
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

           !     kelements = kelements + 1

                 ! read maximum grid range
                 call ESMF_ConfigGetAttribute(localcf, gmax, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 kelements = kelements + 1
                 grid(igrid)%range(irank,2) = gmax 
              !drs debug
          print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gmax
              !drs debug

              else
                 ! error continuation line missing, but grid not finished
                 write(lchar,"(i5)") irow
                 call ESMF_LogMsgSetError( ESMF_FAILURE,                      &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc)

              endif    ! new line
           endif    ! read grid range

           !-------------------------------------------------------------------
           ! read grid units
           !-------------------------------------------------------------------
           if( kelements < ncolumns(irow) ) then
              call ESMF_ConfigGetAttribute(localcf, gunits, rc=localrc)
              ! if error
              write(lchar,"(i5)") irow
              if( ESMF_LogMsgFoundError(localrc,                              &
                "cannot read row " // trim(adjustL(lchar)) //                 &
                " of table " //trim(descriptor_label) // "in file " //        &
                trim(lfilename), rcToReturn=rc) ) return

              kelements = kelements + 1
              grid(igrid)%units(irank)%name = gunits
              !drs debug
         print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gunits
              !drs debug

           elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
           ! reached end of the row, check if another line exists
              if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
              ! if new line starts with a continuation and there are at least two
              ! columns, advance and read
                 irow = irow + 1
                 kelements = 1
                 ! if error
                 call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
                 write(lchar,"(i5)") irow
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 ! read and discard continuation symbol
                 call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

           !     kelements = kelements + 1

                 ! read grid units
                 call ESMF_ConfigGetAttribute(localcf, gunits, rc=localrc)
                 if( ESMF_LogMsgFoundError(localrc,                           &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc) ) return

                 kelements = kelements + 1
                 grid(igrid)%units(irank)%name = gunits
              !drs debug
         print*,igrid,' kelements/ncolumns:',kelements-1,'/', ncolumns(irow),gunits
              !drs debug

              else
                 ! error continuation line missing, but grid not finished
                 write(lchar,"(i5)") irow
                 call ESMF_LogMsgSetError( ESMF_FAILURE,                      &
                   "cannot read row " // trim(adjustL(lchar)) //              &
                   " of table " //trim(descriptor_label) // "in file " //     &
                   trim(lfilename), rcToReturn=rc)


              endif    ! new line
           endif     ! read grid units

           irank = irank + 1

        end do ! while
     endif    !

  enddo    ! end  krow

  deallocate( ncolumns )

  !----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  end subroutine read_rectilinear_grid
  !----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine read_curvilinear_grid(lfilename, grid, localrc)
  !-----------------------------------------------------------------------------
  ! driver for reading the grid specifier files and constructing the esmf grid
  ! object from an esmf array.
  !
  !-----------------------------------------------------------------------------

  ! arguments
  character(ESMF_MAXSTR), intent(in   ) :: lfilename
! type(curvilinear_grid_record), pointer, intent(inout) :: grid(:)
  type(curvilinear_grid_record) :: grid(:)
  integer, intent(  out) :: localrc

  ! local ESMF types
  type(ESMF_Config)      :: localcf

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! open the grid file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(localrc)
  if( ESMF_LogMsgFoundError(localrc, "cannot create config object",           &
                            rcToReturn=localrc) ) return

  print*,'Opening Grid specifier file  ',trim( lfilename )
  call ESMF_ConfigLoadFile(localcf, trim( lfilename ), rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot load config file " // trim( lfilename ),                     &
         rcToReturn=localrc) ) return

  !----------------------------------------------------------------------------
  ! Search and extract the grid type specifier
  !----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, 'grid_type:', rc=localrc )
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label grid_type", rcToReturn=localrc) ) return  

  call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
  if( ESMF_LogMsgFoundError(localrc,                                          &
         "cannot find config label grid_type:",                               &
         rcToReturn=localrc) ) return

  if( trim(adjustL( ltmp )) /= 'CURVILINEAR' ) call ESMF_LogMsgSetError(      &
            ESMF_FAILURE, "Wrong grid typew in file "// trim( lfilename ),    &
            rcToReturn=localrc)
  !----------------------------------------------------------------------------
  ! CURRENTLY NOT SUPPORTED
  !----------------------------------------------------------------------------
  call ESMF_LogMsgSetError( ESMF_FAILURE,                                     &
           "Curvilinear grids not currently supported "// trim( lfilename ),  &
            rcToReturn=localrc)
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------
  end subroutine read_curvilinear_grid
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine generate_rectilinear_coord(grid,localrc)
  !-----------------------------------------------------------------------------
  ! routine that generates the coordinates for a separated rectilinear grid 
  ! as specified by the specification files.
  !-----------------------------------------------------------------------------

  ! arguments
  integer, intent(  out) :: localrc
! type(rectilinear_grid_record), pointer, intent(inout) :: grid(:)
  type(rectilinear_grid_record) :: grid(:)

  ! local ESMF types

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar

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

