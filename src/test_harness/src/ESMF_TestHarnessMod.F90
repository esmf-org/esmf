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

  implicit none

  ! Debug flag
  logical, parameter, private :: debug_flag = .false.

  ! Process Parameters
  integer, parameter :: Harness_Error            = 0
  integer, parameter :: Harness_Redist           = 1
  integer, parameter :: Harness_BilinearRemap    = 2
  integer, parameter :: Harness_ConservRemap     = 3
  integer, parameter :: Harness_2ndConservRemap  = 4
  integer, parameter :: Harness_ExchangeRemap    = 5
  integer, parameter :: Harness_NearNeighRemap   = 6
  integer, parameter :: Harness_UserProvRemap    = 7

  ! Distribution Parameters
  integer, parameter :: Harness_DistError        = 100
  integer, parameter :: Harness_BlockDist        = 101
  integer, parameter :: Harness_CyclicDist       = 102
  integer, parameter :: Harness_ArbitraryDist    = 103

  ! Grid Parameters
  integer, parameter :: Harness_GridError        = 200
  integer, parameter :: Harness_TensorGrid       = 201
  integer, parameter :: Harness_SphericalGrid    = 202
  integer, parameter :: Harness_UnstructuredGrid = 203


  ! Test Result Parameters
  integer, parameter :: HarnessTest_SUCCESS      = 1000
  integer, parameter :: HarnessTest_FAILURE      = 1001
  integer, parameter :: HarnessTest_UNDEFINED    = 1002


!-------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  private
  type name_record
     integer :: value
     character(ESMF_MAXSTR) :: descriptor
     character(ESMF_MAXSTR) :: flags
  end type name_record

  type test_report   
     integer :: status                ! status of test
     integer :: dist_config           ! index of distribution specification 
     integer :: grid_config           ! index of grid specification
     integer :: func_config           ! index of test function specification
  end type test_report

  type process_record
     character(ESMF_MAXSTR) ::string 
     integer :: tag                   ! process tag
     integer :: location              ! string location of method
  end type process_record


!-------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public

  ! character types
  type character_array
     character(ESMF_MAXSTR) :: string
  end type character_array

  ! sized char type
  type sized_char_array
     integer :: tagsize
     type(character_array), pointer :: tag(:)
  end type sized_char_array

!-------------------------------------------------------------------------------
!  Distribution Types

  type dist_specification_record
     integer :: drank                            ! rank of the distribution
     integer, pointer :: dsize(:)                ! num of dist elements along axis
  end type dist_specification_record

  type dist_record
     character(ESMF_MAXSTR) :: filename   ! grid specifier filename
     integer :: nDspecs                   ! number of grid spec records
     type(dist_specification_record), pointer :: src_dist(:)
     type(dist_specification_record), pointer :: dst_dist(:)
  end type dist_record


!-------------------------------------------------------------------------------
!  Grid Types

  type grid_specification_record
     integer :: grank                            ! rank of the grid
     type(character_array), pointer :: gtype(:)  ! type of grid spacing
     type(character_array), pointer :: gunits(:) ! physical grid units
     integer, pointer :: gsize(:)                ! num of grid elements along axis
     real(ESMF_KIND_R8), pointer :: grange(:,:)  ! physical range of axes
  end type grid_specification_record

  type grid_record
     character(ESMF_MAXSTR) :: filename   ! grid specifier filename
     integer :: nGspecs                   ! number of grid spec records
     type(grid_specification_record), pointer :: src_grid(:)
     type(grid_specification_record), pointer :: dst_grid(:)
  end type grid_record

!-------------------------------------------------------------------------------
!  Memory Types

  type memory_config
     character(ESMF_MAXSTR) :: string    ! memory string 
     integer :: memRank                  ! rank of memory chunk
     integer :: DistRank                 ! rank of distribution
     integer :: GridRank                 ! rank of grid
     integer, pointer :: DistOrder(:)
     integer, pointer :: GridOrder(:)
     type(character_array), pointer :: DistType(:)
     type(character_array), pointer :: GridType(:)
     integer, pointer :: HaloL(:)
     integer, pointer :: HaloR(:)
     integer, pointer :: StagLoc(:)
  end type memory_config

  type problem_descriptor_strings
     character(ESMF_MAXSTR) :: pds         ! problem descriptor string
     integer, pointer :: test_status(:,:)  ! test status of the (dist,grid) config
     type(process_record) :: process       ! method process
     type(memory_config) :: DstMem         ! destination memory configuration
     type(memory_config) :: SrcMem         ! source memory configuration
     !
     type(sized_char_array) :: classfile  ! class specification files
     !
     integer :: nDfiles                       ! number of distribution  spec files
     type(dist_record), pointer :: Dfiles(:)  ! distribution specification files
     !
     integer :: nGfiles                       ! number of grid spec files
     type(grid_record), pointer :: Gfiles(:)  ! grid specification files
  end type problem_descriptor_strings

  type problem_descriptor_records
     character(ESMF_MAXSTR) :: filename   ! filename of problem descriptor record
     integer :: numStrings                ! # of problem descriptor strings in record
     type(problem_descriptor_strings), pointer :: str(:)  ! problem descriptor strings
  end type problem_descriptor_records

  type harness_descriptor
     character(ESMF_MAXSTR) :: testClass       ! test class
     character(ESMF_MAXSTR) :: reportType      ! test result report type 
     character(ESMF_MAXSTR) :: setupReportType ! setup report type 
     integer :: numRecords                     ! number of problem descriptor records
     type(problem_descriptor_records), pointer :: rcrd(:)  ! problem descriptor record  
  end type harness_descriptor

!
!===============================================================================

  contains 

!===============================================================================


 !------------------------------------------------------------------------------
 ! Routines to parse input files for descriptor string, and specifier files.
 !------------------------------------------------------------------------------


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
  integer :: iDfile, iGfile, irank, igrid, idist, istatus


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
          write(lsize,"(i6)" ) PDS%Dfiles(iDfile)%src_dist(idist)%dsize(irank)
          write(lorder,"(i1)") PDS%SrcMem%DistOrder(irank)
          ldist = trim(adjustL(PDS%SrcMem%DistType(irank)%string)) //          &
                  trim(adjustL(lorder))// '{' // trim(adjustL(lsize)) // '}'
        else
          ldist = '*'
        endif
        ! now do the grid part
        if( PDS%SrcMem%GridOrder(irank) /= 0 ) then
          write(lsize,"(i6)" ) PDS%Gfiles(iGfile)%src_grid(igrid)%gsize(irank)
          write(lorder,"(i1)") PDS%SrcMem%GridOrder(irank)
          lgrid = trim(adjustL(PDS%SrcMem%GridType(irank)%string)) //          &
                  trim(adjustL(lorder)) // '{' // trim(adjustL(lsize)) // '}'
        else
          lgrid = '*'
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
          write(lsize,"(i6)" ) PDS%Dfiles(iDfile)%dst_dist(idist)%dsize(irank)
          write(lorder,"(i1)") PDS%DstMem%DistOrder(irank)
          ldist = trim(adjustL(PDS%DstMem%DistType(irank)%string)) //          &
                  trim(adjustL(lorder)) // '{' // trim(adjustL(lsize)) // '}'
        else
          ldist = '*'
        endif
        ! now do the grid part
        if( PDS%DstMem%GridOrder(irank) /= 0 ) then
          write(lsize,"(i6)" ) PDS%Gfiles(iGfile)%src_grid(igrid)%gsize(irank)
          write(lorder,"(i1)") PDS%DstMem%GridOrder(irank)
          lgrid = trim(adjustL(PDS%DstMem%GridType(irank)%string)) //          &
                  trim(adjustL(lorder)) // '{' // trim(adjustL(lsize)) // '}'
        else
          lgrid = '*'
        endif

        ! now add the suffix indicating periodicity and/or a halo
        lsuffix = ''
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
  integer :: k, kstring
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

! do k=1,pds%SrcMem%memRank 
!    print*,k,' in parse descriptor src gtype/dtype ', &
!    trim(pds%SrcMem%GridType(k)%string), & 
!    trim(pds%SrcMem%DistType(k)%string)
! enddo
! do k=1,pds%DstMem%memRank 
!    print*,k,' in parse descriptor dst gtype/dtype ', &
!    trim(pds%DstMem%GridType(k)%string), & 
!    trim(pds%DstMem%DistType(k)%string)
! enddo

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
  integer :: k, kstring, rank, halo, ndelim
  integer :: iloc(1), mloc(1)
  integer :: hbeg, hmid, hend, sbeg, send, slen
  integer :: itmp, itmp_beg, itmp_end
  integer, allocatable ::  sdelim(:)


  !-----------------------------------------------------------------------------
  ! initialize return variable
  !-----------------------------------------------------------------------------
  localrc = ESMF_RC_NOT_IMPL 

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
           HaloL(kstring) = -1
           HaloR(kstring) = -1
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
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine interpret_descriptor_string
  !-----------------------------------------------------------------------------

 !------------------------------------------------------------------------------
 ! Routines to search strings
 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function char2int( lstring, strloc, localrc )
    !---------------------------------------------------------------------------
    ! This function converts a character representation of an integer digit
    ! between 0-9 into its integer equivalent. Optional argument of character
    ! address allows selection of individual characters in a string. Default
    ! assumes conversion of the first character of the string.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   )           :: lstring
    integer,          intent(in   ), optional :: strloc
    integer,          intent(inout)           :: localrc

    ! local variables
    integer :: ntemp, sloc

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    if(present(strloc) ) then
       sloc = strloc
    else
       sloc = 1
    endif   
    
 !------------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    ! Convert string to integer
    !---------------------------------------------------------------------------
    ntemp = iachar( lstring(sloc:sloc) )-iachar('0')
    
    ! check to see that values is within the acceptable range
    if ( ntemp < 0 .and. ntemp > 9 ) then
       call ESMF_LogMsgSetError( ESMF_FAILURE,                                 &
                 "character is not a digit between 0 and 9",                   &
                 rcToReturn=localrc)
       char2int = 0
    else
       char2int = ntemp
    endif
    
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    return
    
    !---------------------------------------------------------------------------
    end function char2int
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function findblank( lstring, strloc, increment )
    !---------------------------------------------------------------------------
    ! This function searches for the address of the closest blank space to a
    ! specified address in the string. The parameter increment determines if
    ! the search proceedes foward (+1) or backward (-1).
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: lstring
    integer,          intent(in) :: strloc
    integer,          intent(in) :: increment

    ! local variables
    integer :: k, len_string

    ! initialize variables
    findblank = 0

    len_string = len( trim(adjustL(lstring)) )
    !---------------------------------------------------------------------------
    k = strloc
    do while( (  k + increment >= 1 ).and.(  k + increment <= len_string) )
       k = k + increment 
       if(lstring(k:k) == ' ') then
           findblank = k
           return
       endif
    end do    ! while 
    
    return
    
    !---------------------------------------------------------------------------
    end function findblank
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine dist_query(lstring, MemBeg, MemEnd, MemRank, &
                          MemTopology, MemOrder, localrc)
    !---------------------------------------------------------------------------
    ! This subroutine returns distribution topology (B - block, C - block 
    ! cyclic, A - arbitrary) and order as specified by the descriptor string. 
    !---------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(inout) :: MemBeg, MemEnd
    integer,          intent(in   ) :: MemRank
    integer,          intent(  out) :: MemTopology
    integer,          intent(  out) :: MemOrder(:)
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: i, tmpMemRank
    integer, allocatable :: MemLoc(:)

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL
    
    !---------------------------------------------------------------------------
    ! Check each memory chunk and extract distribution information. 
    !---------------------------------------------------------------------------
    pattern3 = 'BCA'
    allocate( MemLoc(MemRank) )
    tmpMemRank = MemRank
    call set_locate(lstring(MemBeg:MemEnd), pattern3, tmpMemRank, MemLoc)
    MemLoc = MemLoc + MemBeg - 1     ! shift to global string position
    
    !---------------------------------------------------------------------------
    ! check that distribution tags are consistent
    !---------------------------------------------------------------------------
    do i=1,MemRank
       if ( lstring(MemLoc(1):MemLoc(1)) /= lstring(MemLoc(i):MemLoc(i)) ) then
          localrc = ESMF_FAILURE
          ! syntax error, more than one type of distribution specified
          print*,'Syntax error, more than one type of distribution specified'
          call ESMF_LogMsgSetError( ESMF_FAILURE, "syntax error, more than" // &
                   " one type of distribution specified",                      &
                   rcToReturn=localrc)
       endif
    enddo
    !
    do i=1,MemRank
       if ( lstring( MemLoc(i):MemLoc(i) ) == 'B' ) then
       
          MemTopology = Harness_BlockDist
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
                    
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'C' ) then
       
          MemTopology = Harness_CyclicDist
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
          
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'A' ) then
       
          MemTopology = Harness_ArbitraryDist
          
       else  ! Error
       
          MemTopology = Harness_DistError
          
       endif
    enddo
  
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine dist_query
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function dist_rank(lstring, MemBeg, MemEnd, localrc)
    !---------------------------------------------------------------------------
    ! This function returns the distribution rank as specified by the 
    ! descriptor string. 
    !---------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: nDist

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    !---------------------------------------------------------------------------
    ! Check each memory chunk to see if any of the dimensions are distributed. 
    !---------------------------------------------------------------------------
    pattern3 = 'BCA'
    nDist = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nDist == 0 ) then
       call ESMF_LogMsgSetError( ESMF_FAILURE,                              &
                "Syntax Error - no distribution indicated",                 &
                rcToReturn=localrc)
       return
    endif
    dist_rank = nDist

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end function dist_rank
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine grid_query(lstring, MemBeg, MemEnd, MemRank, &
                          MemTopology, MemOrder, localrc)
    !---------------------------------------------------------------------------
    ! This subroutine returns grid topology (G - tensor, U - unstructured)
    ! and order as specified by the descriptor string. 
    !---------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(in   ) :: MemRank
    integer,          intent(  out) :: MemTopology
    integer,          intent(  out) :: MemOrder(:)
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: i, tmpMemRank
    integer, allocatable :: MemLoc(:)

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL
    
    !---------------------------------------------------------------------------
    ! Check each memory chunk and extract distribution information. 
    !---------------------------------------------------------------------------
    pattern3 = 'GU'
    allocate( MemLoc(MemRank) )
    tmpMemRank = MemRank
    call set_locate(lstring(MemBeg:MemEnd), pattern3, tmpMemRank, MemLoc)
    MemLoc = MemLoc + MemBeg - 1     ! shift to global string position
    
    !---------------------------------------------------------------------------
    ! check that distribution tags are consistent
    !---------------------------------------------------------------------------
    do i=1,MemRank
       if ( lstring(MemLoc(1):MemLoc(1)) /= lstring(MemLoc(i):MemLoc(i)) ) then
          localrc = ESMF_FAILURE
          ! syntax error, more than one type of grid specified
          print*,'Syntax error, more than one type of grid specified'
          call ESMF_LogMsgSetError( ESMF_FAILURE, "syntax error, more than" // &
                   " one type of grid specified", rcToReturn=localrc)
       endif
    enddo
    !
    do i=1,MemRank
       if ( lstring( MemLoc(i):MemLoc(i) ) == 'G' ) then
       
          MemTopology = Harness_TensorGrid
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
                    
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'U' ) then
       
          MemTopology = Harness_UnstructuredGrid
          
       else  ! Error
       
          MemTopology = Harness_DistError
          
       endif
    enddo
  
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine grid_query
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function grid_rank(lstring, MemBeg, MemEnd, localrc)
    !---------------------------------------------------------------------------
    ! This function returns the grid rank as specified by the descriptor
    ! string.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: nGrid

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    !---------------------------------------------------------------------------
    ! Check each memory chunk to see if any of the dimensions are associated
    ! with a grid.
    !---------------------------------------------------------------------------
    pattern3 = 'GSU'
    nGrid = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nGrid == 0 ) then
       ! syntax error, no grid layout specified
       print*,'Syntax error, no grid layout'
       call ESMF_LogMsgSetError( ESMF_FAILURE, "syntax error, no grid " //     &
                "layout specified", rcToReturn=localrc)
    endif
    grid_rank = nGrid

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end function grid_rank
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine memory_separate(lstring, iRank, lmem,  localrc)
    !---------------------------------------------------------------------------
    ! For a structured block of memory, return the memory rank (iRank) as 
    ! specified by the descriptor string, and separate each the string
    ! corresponding to each memory dimension into a separate element of a
    ! character array ( lmem(1:iRank) ). Finely separate any specification of
    ! stagger into the last (iRank+1) element of the character array lmem.
    !---------------------------------------------------------------------------

    ! arguments
    character(*), intent(in   ) :: lstring
    integer,                intent(in   ) :: iRank
    type(character_array),  intent(  out) :: lmem(:)
    integer,                intent(  out) :: localrc

    ! local variables
    character(len=1) :: pattern
    integer :: k, nMem, nEnd, iBeg, iEnd
    integer :: EndPos(1)
    integer, allocatable :: MemPos(:)

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    ! initialize variables

    !---------------------------------------------------------------------------
    ! The structured memory is delineated by square brackets. To determine
    ! how this memory is specified, it is first necessary to determne 
    ! the existence of two matching pairs of square brackets.
    !---------------------------------------------------------------------------
    pattern = ';'
    nMem = pattern_query(lstring, pattern)
    nEnd = pattern_query(lstring, ']')

    if( nMem+1 /=  iRank .and. nEnd /= 1 ) then
       call ESMF_LogMsgSetError(ESMF_FAILURE, "asserted memory rank does not"  &
                // " agree with actual memory rank", rcToReturn=localrc)
    else
       !------------------------------------------------------------------------
       !------------------------------------------------------------------------
       allocate( MemPos(nMem) )
       call pattern_locate(lstring, pattern, nMem, MemPos)
       call pattern_locate(lstring, ']', nEnd, EndPos)

       !------------------------------------------------------------------------
       ! extract first memory location - not enclosed by ";" on front side
       !------------------------------------------------------------------------
       iBeg = 2
       iEnd = MemPos(1)-1
       lmem(1)%string = lstring(iBeg:iEnd)

       !------------------------------------------------------------------------
       ! extract remaining memory locations
       !------------------------------------------------------------------------
       do k=2,nMem
          iBeg = MemPos(k-1) +1
          iEnd = MemPos(k) -1
          lmem(k)%string = lstring(iBeg:iEnd)
       enddo

       !------------------------------------------------------------------------
       ! extract last memory location - not enclosed by ";" on back side
       !------------------------------------------------------------------------
       iBeg = MemPos(nMeM) +1
       iEnd = EndPos(1) -1
       lmem(nMem+1)%string = lstring(iBeg:iEnd)

       !------------------------------------------------------------------------
       ! extract stagger location if included
       !------------------------------------------------------------------------
       iBeg = EndPos(1)
       iEnd = len(trim(lstring)) 
       if( iEnd > iBeg ) then
          lmem(nMem+2)%string = lstring(iBeg:iEnd)
       else
          lmem(nMem+2)%string = ' '
       endif

       deallocate( MemPos )
    endif
     
    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine memory_separate
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine memory_topology(lstring, location, srcMulti, srcBlock,          &
                               dstMulti,dstBlock, localrc)
    !---------------------------------------------------------------------------
    ! routine checks the whole input string to determine if the memory topology
    ! consists of a single structured Logically Rectangular block of memory, or 
    ! multiple structured blocks. It also conducts syntax checking on the
    ! string. Output is through two sets (source and destination) of two 
    ! variables; *Block the number of single contigous blocks of memory, and 
    ! *Multi the number of multiple memory blocks.
    !---------------------------------------------------------------------------

    ! arguments
    character(ESMF_MAXSTR), intent(in   ) :: lstring
    integer,                intent(in   ) :: location(2)
    integer,                intent(  out) :: srcMulti, dstMulti
    integer,                intent(  out) :: srcBlock,dstBlock
    integer,                intent(  out) :: localrc

    ! local variables
    integer :: nsingle, nmult, strlen

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL
    strlen = len(lstring)
    srcBlock = 0
    srcMulti = 0
    dstBlock = 0
    dstMulti = 0

    !---------------------------------------------------------------------------
    ! Memory Layout
    !---------------------------------------------------------------------------
    nmult = set_query(lstring,'()' )
    nsingle = set_query(lstring,'[]' )

    !---------------------------------------------------------------------------
    ! check if memory is structured with multiple blocks
    !---------------------------------------------------------------------------
    if( (nmult >= 4).and.(nsingle >= 4) ) then
       !------------------------------------------------------------------------
       ! check to see if symbols are properly paired on each half of the string
       !------------------------------------------------------------------------
       if( pattern_match(lstring(1:location(1)), '(', ')') .and.               &
           pattern_match(lstring(location(2):strlen), '(', ')') .and.          &
           pattern_match(lstring(1:location(1)), '[', ']') .and.               &
           pattern_match(lstring(location(2):strlen), '[', ']')  ) then

           srcBlock = set_query(lstring(1:location(1)),'[' )
           srcMulti = set_query(lstring(1:location(1)),'(' )
           dstBlock = set_query(lstring(location(2):strlen),'[' )
           dstMulti = set_query(lstring(location(2):strlen),'(' )
       else
          call ESMF_LogMsgSetError(ESMF_FAILURE,"symbols not properly paired", &
                   rcToReturn=localrc)
       endif

    !---------------------------------------------------------------------------
    ! if memory is single block
    !---------------------------------------------------------------------------
    elseif( (nmult == 0).and.(nsingle == 4) ) then
       !------------------------------------------------------------------------
       ! check to see if symbols are properly paired on each half of the string
       !------------------------------------------------------------------------
       if( pattern_match(lstring(1:location(1)), '[', ']') .and.               &
           pattern_match(lstring(location(1):strlen), '[', ']')  ) then            

           srcBlock = set_query(lstring(1:location(1)),'[' )
           dstBlock = set_query(lstring(location(2):strlen),'[' )

       else
          call ESMF_LogMsgSetError(ESMF_FAILURE,"symbols not properly paired", &
                   rcToReturn=localrc)
       endif

    else   ! syntax error
       call ESMF_LogMsgSetError( ESMF_FAILURE, "symbols not paired properly",  &
                rcToReturn=localrc)
    endif

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine memory_topology
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine  process_query(lstring, lname, tag, symbol_loc, localrc)
    !---------------------------------------------------------------------------
    ! routine checks the input test string for a method specifier. Acceptable
    ! methods include:
    ! REDIST (-->), BILINEAR REMAP (=B=>), CONSERVATIVE REMAP (=C=>), 
    ! SECOND ORDER CONSERVATIVE REMAP (=S=>), NEAREST NEIGHBOR REMAP (=N=>),
    ! EXCHANGE GRID CONSERVATIVE REMAPPING (=E=>), and a USER SPECIFIED REMAP 
    ! METHOD (=U=>).
    ! 
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   ) :: lstring
    character(len=*), intent(inout) :: lname
    integer,          intent(  out) :: tag
    integer,          intent(  out) :: symbol_loc(2)
    integer,          intent(  out) :: localrc

    ! local variables
    character(ESMF_MAXSTR) :: pattern
    integer :: iredist, iregrid, direction, ib

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    !---------------------------------------------------------------------------
    ! Determine the Process type (REDISTRIBUTION or REMAPPING)
    ! Search for the two signifiers and determine which, if any applies.
    !---------------------------------------------------------------------------
    iredist = index(lstring,'-->')
    iregrid = index(lstring,'=>')

    if( (iredist /= 0).and.(iregrid == 0) ) then
       ! redistribution
       lname = 'REDISTRIBUTION'
       tag = Harness_Redist
       symbol_loc(1) = iredist -1
       symbol_loc(2) = iredist +3
    elseif( (iredist == 0).and.(iregrid /= 0) ) then
       ! regrid, now determine what type
       direction = -1
       ! find beginning of process symbol
       ib = findblank( lstring, iregrid, direction )
       print*,'ib ',ib
       pattern = lstring(ib:iregrid+1)
       select case ( trim(adjustL(pattern)) )

          case('=B=>')
             ! remap method Bilinear
             lname = 'Bilinear REMAP'
             tag  = Harness_BilinearRemap
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=C=>')
             ! remap method first order conservative
             lname = 'Conservative REMAP'
             tag  = Harness_ConservRemap
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=S=>')
             ! remap method second order conservative
             lname = '2nd Order Conservative REMAP'
             tag  = Harness_2ndConservRemap
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=E=>')
             ! remap method exchange grid
             lname = 'Exchange Grid REMAP'
             tag  = Harness_ExchangeRemap
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=N=>')
             ! remap method nearest neighbor
             lname = 'Nearest Neighbor REMAP'
             tag  = Harness_NearNeighRemap
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=U=>')
             ! remap method undefined/user provided
             lname = 'User Provided REMAP'
             tag  = Harness_UserProvRemap
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case default
             ! syntax error - no recognized method specified
             call ESMF_LogMsgSetError( ESMF_FAILURE,                           &
                      "process symbol not recognized",                         &
                      rcToReturn=localrc)
             lname = 'ERROR'
             tag  = Harness_Error
             symbol_loc(1) = 0
             symbol_loc(2) = 0

          end select  ! remap type
      elseif( (iredist == 0).and.(iregrid == 0) ) then
         ! syntax error - no action
         call ESMF_LogMsgSetError( ESMF_FAILURE,                               &
               "no process symbol found",                                      &
               rcToReturn=localrc)
         lname = 'ERROR'
         tag = Harness_Error
      elseif( (iredist /= 0).and.(iregrid /= 0) ) then
         ! syntax error - multiple actions
         call ESMF_LogMsgSetError( ESMF_FAILURE,                               &
                  "more than one process symbol found",                        &
                  rcToReturn=localrc)
         lname = 'ERROR'
         tag = Harness_Error
      endif    ! process test

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end subroutine process_query
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine set_locate(lstring, lset, number_hits, hit_loc)
    
    !---------------------------------------------------------------------------
    ! Locates in STRING, any elements of SET and places their location 
    ! in hit_loc. 
    !---------------------------------------------------------------------------
 
    ! arguments
    character(len=*),     intent(in   ) :: lstring
    character(len=*),     intent(in   ) :: lset
    integer,              intent(inout) :: number_hits
    integer, dimension(:),intent(inout) :: hit_loc       ! of size number_hits
    
    ! local variables
    integer :: k, klast, ncount
    integer :: len_string, len_set

    ! initialize variables
    ncount = 0
    klast = 0
    len_string = len(lstring)
    len_set = len( trim(adjustL(lset) ))

    !---------------------------------------------------------------------------
    ! error check - conduct the scan only if the set and string are not empty.
    !---------------------------------------------------------------------------
    if( (len_set > 0) .and. (len_string > 0) ) then
       !------------------------------------------------------------------------
       ! examine the string to find the find the FIRST instance of an 
       ! element in the set.
       !------------------------------------------------------------------------
       k = scan(lstring(klast+1:len_string), trim(adjustL(lset) ))
       do while( k > 0 )
          ncount = ncount + 1 ! count match.
          hit_loc(ncount) = k+klast  ! save location of set match.
          klast = k + klast ! slide forward in string looking for next match.
          k = scan(lstring(klast+1:len_string), trim(adjustL(lset) ))
       enddo     ! while
    endif
    ! sanity check - if these do not agree something has failed.
    if (number_hits /= ncount) ncount = 0
    ! return value
    number_hits = ncount

    !---------------------------------------------------------------------------
    end subroutine set_locate
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function set_query(lstring,lset)
    !---------------------------------------------------------------------------
    ! Scans a string for any character from a SET of characters. Returns zero
    ! if none of the elements of the set occur in the string or if the set or
    ! string is empty.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: lstring
    character(len=*), intent(in) :: lset

    ! local variables
    integer :: k, klast, ncount
    integer :: len_string, len_set

    ! initialize variables
    ncount = 0
    klast = 0
    len_string = len(lstring)
    len_set = len( trim(adjustL(lset) ))
    
    !---------------------------------------------------------------------------
    ! error check - conduct the scan only if the set and string are not empty
    !---------------------------------------------------------------------------
    if( (len_set > 0) .and. (len_string > 0) ) then
       !------------------------------------------------------------------------
       ! examine the string to find the find the FIRST instance of an 
       ! element in the set.
       !------------------------------------------------------------------------
       k = scan(lstring(klast+1:len_string), trim(adjustL(lset) ))
       do while( k > 0 )
          !---------------------------------------------------------------------
          ! if scan is nonzero, there is at least one match, increment
          ! through the string checking for additional matches. 
          !---------------------------------------------------------------------
          ncount = ncount + 1    ! count match
          klast = k + klast      ! slide forward in string looking for next match
          k = scan(lstring(klast+1:len_string), trim(adjustL(lset) ))
       enddo     ! while
    endif 
    
    set_query = ncount
    
    !---------------------------------------------------------------------------
    end function set_query
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    logical function pattern_match(lstring, lcharL, lcharR)
    !---------------------------------------------------------------------------
    ! function checks the input string and determines if there are matching 
    ! pairs of the enclosing symbols ( lcharL, lcharR) and that they are in the 
    ! proper order.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   ) :: lstring
    character(len=*), intent(in   ) :: lcharL, lcharR

    ! local variables
    integer, allocatable :: locL(:), locR(:)
    integer ::  k, ntestL, ntestR
    logical :: flag

    ! initialize variables
    flag = .true.

    !---------------------------------------------------------------------------
    ! check that the patterns mach up
    !---------------------------------------------------------------------------
    ntestL = pattern_query(lstring,trim(adjustL(lcharL)) )
    ntestR = pattern_query(lstring,trim(adjustL(lcharR)) )
    if( ntestL == ntestR ) then
       !------------------------------------------------------------------------
       ! the numbers match, so now check that the order is left to right
       !------------------------------------------------------------------------
       allocate( locL(ntestL), locR(ntestR) )
       call pattern_locate(lstring,trim(adjustL(lcharL)), ntestL, locL )
       call pattern_locate(lstring,trim(adjustL(lcharR)), ntestR, locR )

       !------------------------------------------------------------------------
       ! are any of the symbols out of order
       !------------------------------------------------------------------------
       do k=1, ntestL
          if( locL(k) > locR(k) ) flag = .false.
       enddo
       if( flag ) then
          ! order correct
          pattern_match = .true.
       else
          ! order wrong
          pattern_match = .false.
       endif     ! flag 
       deallocate( locL, locR )

    else
       ! numbers of symbols don't pair
       pattern_match = .false.
    endif

    !---------------------------------------------------------------------------
    end function pattern_match  
    !---------------------------------------------------------------------------

!-------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine pattern_locate(lstring, lpattern, number_hits, hit_loc)
    !---------------------------------------------------------------------------
    ! Locates all instances of a PATTERN in STRING, placing the location of
    ! the beginning of the pattern in hit_loc.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   ) :: lstring
    character(len=*), intent(in   ) :: lpattern
    integer,          intent(inout) :: number_hits
    integer,          intent(inout) :: hit_loc(:)
    
    ! local variables
    integer :: k, klast, ncount
    integer :: len_string, len_pattern

    ! initialize variables
    ncount = 0
    len_string = len(lstring)
    len_pattern = len(  trim(adjustL(lpattern) ))

    !---------------------------------------------------------------------------
    ! error check - conduct the search only if the pattern & string are not empty
    !---------------------------------------------------------------------------
    if( (len_pattern > 0) .and. (len_string > 0) ) then
       klast = 0
       !------------------------------------------------------------------------
       ! examine the string to find the find the FIRST instance of the pattern
       !------------------------------------------------------------------------
       k = index(lstring(klast+1:len_string), trim(adjustL(lpattern) ))

       do while( k > 0 )
          !---------------------------------------------------------------------
          ! if index is nonzero, there is at least one match, increment
          ! through the string checking for additional matches.
          !---------------------------------------------------------------------
          ncount = ncount + 1      ! count match
          klast = k + klast        ! slide forward in string looking for next match
          hit_loc(ncount) = klast  ! save location of the pattern match
          k = index(lstring(klast+1:len_string),  trim(adjustL(lpattern) ))
       enddo     ! while
    endif
    ! if they do not agree something has failed.
    if (number_hits /= ncount) ncount = 0
    ! return value
    number_hits = ncount

    !---------------------------------------------------------------------------
    end subroutine pattern_locate
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function pattern_query(lstring,lpattern)
    !---------------------------------------------------------------------------
    ! Searches a string for a PATTERN of characters. Returns zero if the pattern 
    ! is not found, or if the pattern or string is empty.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: lstring
    character(len=*), intent(in) :: lpattern

    ! local variables
    integer :: k, klast, ncount
    integer :: len_string, len_pattern

    ! initialize variables
    ncount = 0
    klast = 0
    len_string = len(lstring)
    len_pattern = len( trim(adjustL(lpattern)) )
    
    !---------------------------------------------------------------------------
    ! error check - conduct the search only if the pattern & string are not empty
    !---------------------------------------------------------------------------
    if( (len_pattern > 0) .and. (len_string > 0) ) then
       !------------------------------------------------------------------------
       ! examine the string to find the find the FIRST instance of the pattern 
       !------------------------------------------------------------------------
       k = index(lstring(klast+1:len_string), trim(adjustL(lpattern) ))
       do while( k > 0 )
          !---------------------------------------------------------------------
          ! if index is nonzero, there is at least one match, increment
          ! through the string checking for additional matches. 
          !---------------------------------------------------------------------
          ncount = ncount + 1    ! count match
          klast = k + klast      ! slide forward in string looking for next match
          k = index(lstring(klast+1:len_string), trim(adjustL(lpattern) ))
       enddo     ! while
    endif 
    
    pattern_query = ncount
    !---------------------------------------------------------------------------
    end function pattern_query
    !---------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! methods to query config tables
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine  read_table_integer(int_value,                                    &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, rc)  

  !-----------------------------------------------------------------------------
  ! helper routine to read a single integer entry of a config file table. The
  ! tables are designed to have multiple lines for a single entry by using "&"
  ! continuation symbols to indicate a line continuation. It internally tests 
  ! for the end of the table line and looks for a continuation symbol on the
  ! next line. If found it advances to the next entry.
  !
  ! This separate routine was created to avoid significant duplication of code
  ! necessary for parsing the tables.
  !-----------------------------------------------------------------------------
  character(ESMF_MAXSTR), intent(in   ) :: lfilename, descriptor_label
  integer, intent(  out) :: int_value
  integer, intent(in   ) :: ncolumns(:), new_row(:)
  integer, intent(inout) :: irow       ! current row of table
  integer, intent(in   ) :: nrows      ! total rows in table
  integer, intent(inout) :: kelements  ! current element of row irow of table
  type(ESMF_Config), intent(inout) :: localcf
  integer, intent(inout) :: rc

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: lchar, ltmp
  integer :: int_tmp

  logical :: flag = .true.

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! if kelements < ncolumns then not at end of the row and can safely read
  ! another element.
  !-----------------------------------------------------------------------------
  if( kelements < ncolumns(irow) ) then
     if( debug_flag) print*,' get attribute integer '
     call ESMF_ConfigGetAttribute(localcf, int_tmp, rc=localrc)
     !--------------------------------------------------------------------------
     ! if error
     !--------------------------------------------------------------------------
     write(lchar,"(i5)") irow
     if( ESMF_LogMsgFoundError(localrc,                                        &
         "cannot read row " // trim(adjustL(lchar)) //                         &
         " of table " //trim(descriptor_label) // "in file " //                &
         trim(lfilename), rcToReturn=rc) ) return
         kelements = kelements + 1
         int_value = int_tmp

  elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
     !--------------------------------------------------------------------------
     ! reached end of the row, check if another line exists
     !--------------------------------------------------------------------------
     if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
        !-----------------------------------------------------------------------
        ! if new line starts with a continuation and there are at least two
        ! columns, advance and read
        !-----------------------------------------------------------------------
        irow = irow + 1
        kelements = 1
        ! if error
        if( debug_flag) print*,' get next line '
        call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
        write(lchar,"(i5)") irow
        if( ESMF_LogMsgFoundError(localrc,                                     &
           "cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return
        !-----------------------------------------------------------------------
        ! read and discard continuation symbol
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute integer - contin symbol '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogMsgFoundError(localrc,                                     &
           "cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read string from table
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute integer - after contin symbol '
        call ESMF_ConfigGetAttribute(localcf, int_tmp, rc=localrc)
        if( ESMF_LogMsgFoundError(localrc,                                     &
           "cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        kelements = kelements + 1
        int_value = int_tmp

     else
        !-----------------------------------------------------------------------
        ! error continuation line missing, but grid not finished
        !-----------------------------------------------------------------------
        write(lchar,"(i5)") irow
        call ESMF_LogMsgSetError( ESMF_FAILURE,                                &
              "cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc)
     endif    ! new line
  endif    ! 

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_table_integer
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine  read_table_real(flt_value,                                       &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, rc)  

  !-----------------------------------------------------------------------------
  ! helper routine to read a single real entry of a config file table. The
  ! tables are designed to have multiple lines for a single entry by using "&"
  ! continuation symbols to indicate a line continuation. It internally tests 
  ! for the end of the table line and looks for a continuation symbol on the
  ! next line. If found it advances to the next entry.
  !
  ! This separate routine was created to avoid significant duplication of code
  ! necessary for parsing the tables.
  !-----------------------------------------------------------------------------
  character(ESMF_MAXSTR), intent(in   ) :: lfilename, descriptor_label
  real(ESMF_KIND_R8), intent(  out) :: flt_value
  integer, intent(in   ) :: ncolumns(:), new_row(:)
  integer, intent(inout) :: irow       ! current row of table
  integer, intent(in   ) :: nrows      ! total rows in table
  integer, intent(inout) :: kelements  ! current element of row irow of table
  type(ESMF_Config), intent(inout) :: localcf
  integer, intent(inout) :: rc

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: lchar, ltmp
  real(ESMF_KIND_R8) :: flt_tmp

  logical :: flag = .true.

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! if kelements < ncolumns then not at end of the row and can safely read
  ! another element.
  !-----------------------------------------------------------------------------
  if( kelements < ncolumns(irow) ) then
     if( debug_flag) print*,' get attribute - real '
     call ESMF_ConfigGetAttribute(localcf, flt_tmp, rc=localrc)
     !--------------------------------------------------------------------------
     ! if error
     !--------------------------------------------------------------------------
     write(lchar,"(i5)") irow
     if( ESMF_LogMsgFoundError(localrc,                                        &
         "cannot read row " // trim(adjustL(lchar)) //                         &
         " of table " //trim(descriptor_label) // "in file " //                &
         trim(lfilename), rcToReturn=rc) ) return
         kelements = kelements + 1
         flt_value = flt_tmp

  elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
     !--------------------------------------------------------------------------
     ! reached end of the row, check if another line exists
     !--------------------------------------------------------------------------
     if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
        !-----------------------------------------------------------------------
        ! if new line starts with a continuation and there are at least two
        ! columns, advance and read
        !-----------------------------------------------------------------------
        irow = irow + 1
        kelements = 1
        ! if error
        if( debug_flag) print*,' get next line  in real'
        call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
        write(lchar,"(i5)") irow
        if( ESMF_LogMsgFoundError(localrc,                                     &
           "cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return
        !-----------------------------------------------------------------------
        ! read and discard continuation symbol
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute - contin symbol '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogMsgFoundError(localrc,                                     &
           "cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read string from table
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute - real after continuation '
        call ESMF_ConfigGetAttribute(localcf, flt_tmp, rc=localrc)
        if( ESMF_LogMsgFoundError(localrc,                                     &
           "cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        kelements = kelements + 1
        flt_value = flt_tmp

     else
        !-----------------------------------------------------------------------
        ! error continuation line missing, but grid not finished
        !-----------------------------------------------------------------------
        write(lchar,"(i5)") irow
        call ESMF_LogMsgSetError( ESMF_FAILURE," continuation missing " //     &
              "cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc)
     endif    ! new line
  endif    ! 

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_table_real
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  subroutine  read_table_string(string,                                        &
                                kelements, irow, nrows, ncolumns, new_row,     &
                                lfilename, descriptor_label, localcf, rc)  

  !-----------------------------------------------------------------------------
  ! helper routine to read character string entry of a config file table. The
  ! tables are designed to have multiple lines for a single entry by using "&"
  ! continuation symbols to indicate a line continuation. It internally tests 
  ! for the end of the table line and looks for a continuation symbol on the
  ! next line. If found it advances to the next entry.
  !
  ! This separate routine was created to avoid significant duplication of code
  ! necessary for parsing the tables.
  !-----------------------------------------------------------------------------
  ! arguments
  character(ESMF_MAXSTR), intent(in   ) :: lfilename, descriptor_label
  character(ESMF_MAXSTR), intent(  out) :: string
  integer, intent(in   ) :: ncolumns(:), new_row(:)
  integer, intent(inout) :: irow       ! current row of table
  integer, intent(in   ) :: nrows      ! total rows in table
  integer, intent(inout) :: kelements  ! current element of row irow of table
  type(ESMF_Config), intent(inout) :: localcf
  integer, intent(inout) :: rc

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(ESMF_MAXSTR) :: ltmp, lchar

  logical :: flag = .true.

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! if kelements < ncolumns then not at end of the row and can safely read
  ! another element.
  !-----------------------------------------------------------------------------
  if( kelements < ncolumns(irow) ) then
     if( debug_flag) print*,' get attribute string '
     call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
     !--------------------------------------------------------------------------
     ! if error
     !--------------------------------------------------------------------------
     write(lchar,"(i5)") irow
     if( ESMF_LogMsgFoundError(localrc,                                        &
         "cannot read row " // trim(adjustL(lchar)) //                         &
         " of table " //trim(descriptor_label) // "in file " //                &
         trim(lfilename), rcToReturn=rc) ) return
         kelements = kelements + 1
         string = ltmp

  elseif( kelements >= ncolumns(irow) .and. irow+1 <= nrows ) then
     !--------------------------------------------------------------------------
     ! reached end of the row, check if another line exists
     !--------------------------------------------------------------------------
     if( new_row(irow+1) == 0 .and. ncolumns(irow+1) >= 2 ) then
        !-----------------------------------------------------------------------
        ! if new line starts with a continuation and there are at least two
        ! columns, advance and read
        !-----------------------------------------------------------------------
        irow = irow + 1
        kelements = 1
        ! if error
        if( debug_flag) print*,' next line string '
        call ESMF_ConfigNextLine(localcf, flag, rc=localrc)
        write(lchar,"(i5)") irow
        if( ESMF_LogMsgFoundError(localrc,                                     &
              "cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc) ) return
        !-----------------------------------------------------------------------
        ! read and discard continuation symbol
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute - read continuation symbol '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogMsgFoundError(localrc,                                     &
              "cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read string from table
        !-----------------------------------------------------------------------
        if( debug_flag)  print*,' get attribute string after continuation '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogMsgFoundError(localrc,                                     &
              "cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc) ) return

        kelements = kelements + 1
        string = ltmp

     else
        !-----------------------------------------------------------------------
        ! error continuation line missing, but grid not finished
        !-----------------------------------------------------------------------
        write(lchar,"(i5)") irow
        call ESMF_LogMsgSetError( ESMF_FAILURE," continuation missing " //     &
               "cannot read row " // trim(adjustL(lchar)) //                   &
               " of table " //trim(descriptor_label) // "in file " //          &
               trim(lfilename), rcToReturn=rc)
     endif    ! new line
  endif    ! 

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_table_string
  !-----------------------------------------------------------------------------

!===============================================================================
  end module ESMF_TestHarnessMod
!===============================================================================
