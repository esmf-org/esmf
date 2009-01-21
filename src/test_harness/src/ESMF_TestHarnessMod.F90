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
#define ESMF_FILENAME "ESMF_TestHarnessMod"
!
!  ESMF Test Harness Module
   module ESMF_TestHarnessMod
!
!==============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessMod
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the
! {\tt ESMF\_TestHarness}.  
!
!------------------------------------------------------------------------------
! !USES:

  use ESMF_Mod

  implicit none

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


!------------------------------------------------------------------------------
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
     character(ESMF_MAXSTR) :: name
     integer :: tag                   ! process tag
     integer :: location              ! string location of method
  end type process_record

  type memory_record
     character(ESMF_MAXSTR) :: name
     integer :: chunks               ! number of logically rectangular chunks
     integer, pointer :: rank(:)
  end type memory_record

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

  type dist_record
     character(ESMF_MAXSTR) :: name
     integer :: topology           ! key (simple block, block-cyclic, arbitrary)
     integer :: rank               ! rank of distribution
     integer, pointer :: order(:)  ! axis number, zero for free.
     integer, pointer :: size(:)   ! number of DE for axis number, zero for free.
     integer, pointer :: period(:) ! period for block-cyclic, zero for simple block
     integer, pointer :: specifier(:,:)  ! 
  end type dist_record



!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public

  ! character types
  type character_array
     character(ESMF_MAXSTR) :: name
  end type character_array

  ! sized char type
  type sized_char_array
     integer :: size
     type(character_array), pointer :: string(:)
  end type sized_char_array

  type problem_descriptor_strings
     character(ESMF_MAXSTR) :: pds        ! problem descriptor string
     type(process_record) :: process      ! method process
     type(sized_char_array) :: classfile  ! distribution specification files
     type(sized_char_array) :: distfiles  ! distribution specification files
     type(sized_char_array) :: gridfiles  ! grid specification files
  end type problem_descriptor_strings

  type problem_descriptor_records
     character(ESMF_MAXSTR) :: filename   ! filename of problem descriptor record
     integer :: numStrings                ! # of problem descriptor strings in recd
     type(problem_descriptor_strings), pointer :: string(:)  ! problem descriptor  strings
  end type problem_descriptor_records

  type harness_descriptor
     character(ESMF_MAXSTR) :: testClass   ! test class
     integer :: reportType                 ! report type 
     integer :: numRecords                 ! number of problem descriptor records
     type(problem_descriptor_records), pointer :: record(:)  ! problem descriptor record  
  end type harness_descriptor

!------------superceeded: to be removed
  type problem_descriptor_record
     character(ESMF_MAXSTR) :: string           ! problem descriptor string
     type(test_report), pointer :: report(:)    ! record of test result
     type(process_record) :: process            ! method process
     type(sized_char_array) :: distfiles        ! distribution specification files
     type(sized_char_array) :: gridfiles        ! grid specification files
     type(memory_record) :: src_memory          ! memory topology
     type(memory_record) :: dst_memory          ! memory topology
     type(dist_record) :: src_dist  ! src distribution specification
     type(dist_record) :: dst_dist  ! dst distribution specification
     type(grid_record) :: src_grid  !  src grid specification
     type(grid_record) :: dst_grid  !  dst grid specification
  end type problem_descriptor_record

!
!==============================================================================

  contains 

!==============================================================================


 !-----------------------------------------------------------------------------
 ! Routines to parse input files for descriptor string, and specifier files.
 !-----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  subroutine interpret_descriptor_string(problem_descriptor, returnrc)
  !----------------------------------------------------------------------------
  ! Parse a single  problem descriptor string for problem description items
  ! such as process (REDISTRIBUTION or REMAPPING), memory topology and rank,
  ! distribution and grid rank.
  !----------------------------------------------------------------------------

  ! arguments
  type(problem_descriptor_record), intent(inout) :: problem_descriptor
  integer, intent(out) :: returnrc

  ! local variables
  integer :: localrc = ESMF_SUCCESS ! assume success

  ! test variables
! integer :: SrcMemRank, DstMemRank
! integer, allocatable :: SrcMemLoc(:), DstMemLoc(:)
! integer :: SrcMemBeg, SrcMemEnd, DstMemBeg, DstMemEnd
! integer :: SrcDistRank, DstDistRank, SrcGridRank, DstGridRank


  !----------------------------------------------------------------------------
  ! initialize variables
  !----------------------------------------------------------------------------
  returnrc = ESMF_SUCCESS

  ! Initialize to failure.
  problem_descriptor%process%name = 'NONE'
  problem_descriptor%process%tag = Harness_Error

  !----------------------------------------------------------------------------
  ! Determine testing process (REDISTRIBUTION or REMAPPING)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  ! Determine memory layout (Logically Rectangular or General)
  !----------------------------------------------------------------------------
     !-----------------------------------------------------------------------
     ! Determine the problem's destination distribution rank, order, topology 
     !------------------------------------------------------------------------
             
     !-------------------------------------------------------------------------
     ! Determine source grid rank and order
     !-------------------------------------------------------------------------
             
     !-------------------------------------------------------------------------
     ! Determine destination grid rank and order
     !-------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  end subroutine interpret_descriptor_string
  !----------------------------------------------------------------------------


  !----------------------------------------------------------------------------
  subroutine read_distgrid(descriptor, returnrc)
  !----------------------------------------------------------------------------
  ! Routine extracts from the distgrid specifier files the DistGrid information
  !----------------------------------------------------------------------------

  ! arguments
  type(problem_descriptor_record), intent(inout) :: descriptor
  integer, intent(out) :: returnrc

  ! local variables
  integer :: localrc = ESMF_SUCCESS ! assume success

  type(ESMF_Config)   :: localcf

  integer :: ncount
  integer :: src_dist_rank,dst_dist_rank
  integer, allocatable :: partialcount(:)
  character(ESMF_MAXSTR) :: lfile, ltag
  integer :: ncol

  ! initialize variables
  returnrc = ESMF_SUCCESS

  ! create a config handle and load the config file
  localcf = ESMF_ConfigCreate(localrc)

  ! allocate workspace to hold the count of the entries for each file.
  print*,'number of descriptor files is:',descriptor%distfiles%size
  allocate( partialcount(descriptor%distfiles%size) )

  src_dist_rank = descriptor%src_dist%rank
  dst_dist_rank = descriptor%dst_dist%rank

  print*,'src/dst rank of dist is:',src_dist_rank,'/',dst_dist_rank

  !----------------------------------------------------------------------------
  ! loop over the number of DistGrid descriptor files, and count the total entries.
  !----------------------------------------------------------------------------
  do ncount=1, descriptor%distfiles%size
     lfile = descriptor%distfiles%string(ncount)%name
     call ESMF_ConfigLoadFile( localcf, trim( lfile ), rc=localrc )
     if (localrc /= ESMF_SUCCESS) then
        print*,'error, could not open DistGrid specifier file:',trim( lfile )
        returnrc = ESMF_FAILURE
        return
     endif

     !-------------------------------------------------------------------------
     ! Search for the tag distgrid_block_#D#D to extract the source and
     ! destination ranks sizes.
     !-------------------------------------------------------------------------
 10  format('distgrid_block_',i1.0,'D',i1.0,'D')
     write(ltag,10) src_dist_rank,dst_dist_rank
     print*,'looking for tag:', trim( ltag )
     if( src_dist_rank /= dst_dist_rank) then
        print*,'error,src dist rank ',src_dist_rank,' /= dst dist rank ',  &
               dst_dist_rank
        returnrc = ESMF_FAILURE
        return
     endif
     ! obtain the number of config entries
     call ESMF_ConfigGetDim(localcf,partialcount(ncount),ncol,trim(ltag),   &
                            rc=localrc)

     ! close config file before opening another
     call ESMF_ConfigDestroy(localcf, localrc)
  enddo    ! ncount

! bunch of stuff here deleted 

  ! clean up
  deallocate(partialcount)


  !----------------------------------------------------------------------------
  end subroutine read_distgrid
  !----------------------------------------------------------------------------


 !-----------------------------------------------------------------------------
 ! Routines to search strings
 !-----------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    integer function char2int( lstring, strloc, localrc )
    !--------------------------------------------------------------------------
    ! This function converts a character representation of an integer digit
    ! between 0-9 into its integer equivalent. Optional argument of character
    ! address allows selection of individual characters in a string. Default
    ! assumes conversion of the first character of the string.
    !--------------------------------------------------------------------------

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
    
 !-----------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    ! Convert string to integer
    !--------------------------------------------------------------------------
    ntemp = iachar( lstring(sloc:sloc) )-iachar('0')
    
    ! check to see that values is within the acceptable range
    if ( ntemp < 0 .and. ntemp > 9 ) then
       call ESMF_LogMsgSetError( ESMF_FAILURE,                              &
                 "character is not a digit between 0 and 9",                &
                 rcToReturn=localrc)
       char2int = 0
    else
       char2int = ntemp
    endif
    
    return
    
    !--------------------------------------------------------------------------
    end function char2int
    !--------------------------------------------------------------------------
 !-----------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    integer function findblank( lstring, strloc, increment )
    !--------------------------------------------------------------------------
    ! This function searches for the address of the closest blank space to a
    ! specified address in the string. The parameter increment determines if
    ! the search proceedes foward (+1) or backward (-1).
    !--------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: lstring
    integer,          intent(in) :: strloc
    integer,          intent(in) :: increment

    ! local variables
    integer :: k, len_string

    ! initialize variables
    findblank = 0

    len_string = len( trim(adjustL(lstring)) )
    !--------------------------------------------------------------------------
    k = strloc
    do while( (  k + increment >= 1 ).and.(  k + increment <= len_string) )
       k = k + increment 
       if(lstring(k:k) == ' ') then
           findblank = k
           return
       endif
    end do    ! while 
    
    return
    
    !--------------------------------------------------------------------------
    end function findblank
    !--------------------------------------------------------------------------

 !-----------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    subroutine dist_query(lstring, MemBeg, MemEnd, MemRank, &
                          MemTopology, MemOrder, localrc)
    !--------------------------------------------------------------------------
    ! This subroutine returns distribution topology (B - block, C - block 
    ! cyclic, A - arbitrary) and order as specified by the descriptor string. 
    !--------------------------------------------------------------------------

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
    
    !--------------------------------------------------------------------------
    ! Check each memory chunk and extract distribution information. 
    !--------------------------------------------------------------------------
    pattern3 = 'BCA'
    allocate( MemLoc(MemRank) )
    tmpMemRank = MemRank
    call set_locate(lstring(MemBeg:MemEnd), pattern3, tmpMemRank, MemLoc)
    MemLoc = MemLoc + MemBeg - 1     ! shift to global string position
    
    !--------------------------------------------------------------------------
    ! check that distribution tags are consistent
    !--------------------------------------------------------------------------
    do i=1,MemRank
       if ( lstring(MemLoc(1):MemLoc(1)) /= lstring(MemLoc(i):MemLoc(i)) ) then
          localrc = ESMF_FAILURE
          ! syntax error, more than one type of distribution specified
          print*,'Syntax error, more than one type of distribution specified'
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
  
    !------------------------------------------------------------------------
    end subroutine dist_query
    !------------------------------------------------------------------------

 !---------------------------------------------------------------------------

    !------------------------------------------------------------------------
    integer function dist_rank(lstring, MemBeg, MemEnd, localrc)
    !------------------------------------------------------------------------
    ! This function returns the distribution rank as specified by the 
    ! descriptor string. 
    !------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: nDist

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    !------------------------------------------------------------------------
    ! Check each memory chunk to see if any of the dimensions are distributed. 
    !------------------------------------------------------------------------
    pattern3 = 'BCA'
    nDist = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nDist == 0 ) then
       call ESMF_LogMsgSetError( ESMF_FAILURE,                              &
                "Syntax Error - no distribution indicated",                 &
                rcToReturn=localrc)
       return
    endif
    dist_rank = nDist

    !------------------------------------------------------------------------
    end function dist_rank
    !------------------------------------------------------------------------

 !-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine grid_query(lstring, MemBeg, MemEnd, MemRank, &
                          MemTopology, MemOrder, localrc)
    !------------------------------------------------------------------------
    ! This subroutine returns grid topology (G - tensor, S - spherical, 
    ! U - unstructured) and order as specified by the descriptor string. 
    !------------------------------------------------------------------------

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
    
    !------------------------------------------------------------------------
    ! Check each memory chunk and extract distribution information. 
    !------------------------------------------------------------------------
    pattern3 = 'GSU'
    allocate( MemLoc(MemRank) )
    tmpMemRank = MemRank
    call set_locate(lstring(MemBeg:MemEnd), pattern3, tmpMemRank, MemLoc)
    MemLoc = MemLoc + MemBeg - 1     ! shift to global string position
    
    !--------------------------------------------------------------------------
    ! check that distribution tags are consistent
    !--------------------------------------------------------------------------
    do i=1,MemRank
       if ( lstring(MemLoc(1):MemLoc(1)) /= lstring(MemLoc(i):MemLoc(i)) ) then
          localrc = ESMF_FAILURE
          ! syntax error, more than one type of distribution specified
          print*,'Syntax error, more than one type of distribution specified'
       endif
    enddo
    !
    do i=1,MemRank
       if ( lstring( MemLoc(i):MemLoc(i) ) == 'G' ) then
       
          MemTopology = Harness_TensorGrid
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
                    
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'S' ) then
       
          MemTopology = Harness_SphericalGrid
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
          
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'U' ) then
       
          MemTopology = Harness_UnstructuredGrid
          
       else  ! Error
       
          MemTopology = Harness_DistError
          
       endif
    enddo
  
    !--------------------------------------------------------------------------
    end subroutine grid_query
    !--------------------------------------------------------------------------

 !-----------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    integer function grid_rank(lstring, MemBeg, MemEnd, localrc)
    !--------------------------------------------------------------------------
    ! This function returns the grid rank as specified by the descriptor
    ! string.
    !--------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: nGrid

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    !------------------------------------------------------------------------
    ! Check each memory chunk to see if any of the dimensions are associated
    ! with a grid.
    !------------------------------------------------------------------------
    pattern3 = 'GSU'
    nGrid = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nGrid == 0 ) then
       ! syntax error, no grid layout specified
       print*,'Syntax error, no grid layout'
       localrc = ESMF_FAILURE
    endif
    grid_rank = nGrid

    !------------------------------------------------------------------------
    end function grid_rank
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine memory_locate(lstring, MemCount, MemBeg, MemEnd, MemLoc, &
                             localrc)      
    !------------------------------------------------------------------------
    ! This subroutine returns the location of the memory delimiters as
    ! specified by the descriptor string.
    !------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemCount
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(inout) :: MemLoc(:)
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=1) :: pattern
    integer, allocatable :: mloc(:)
    integer :: tmpmem

    ! initialize variables
    localrc = ESMF_SUCCESS

    ! initialize variables
    pattern = ';'
    MemLoc(1) = MemBeg

    allocate( mloc(MemCount-1) )
    tmpmem = MemCount-1
    call pattern_locate(lstring(MemBeg:MemEnd),pattern,tmpmem,mloc)
    MemLoc(2:MemCount) = mloc(1:MemCount-1) + MemBeg -1
    MemLoc(MemCount+1) = MemEnd
 
    deallocate( mloc )
    
    !--------------------------------------------------------------------------
    end subroutine memory_locate
    !--------------------------------------------------------------------------

 !-----------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    subroutine memory_rank(lstring, iRank, iBeg, iEnd, localrc)
    !--------------------------------------------------------------------------
    ! For a structured block of memory, return the memory rank as specified
    ! by the descriptor string. 
    ! 
    !------------------------------------------------------------------------

    ! arguments
    character(ESMF_MAXSTR), intent(in   ) :: lstring
    integer,                intent(  out) :: iRank
    integer,                intent(  out) :: iBeg, iEnd
    integer,                intent(  out) :: localrc

    ! local variables
    character(len=1) :: pattern
    character(len=2) :: pattern2
    integer :: nMemory
    integer :: MemPos(2)

    ! initialize variables
    localrc = ESMF_RC_NOT_IMPL

    ! initialize variables
    iRank = 0 

    !------------------------------------------------------------------------
    ! The structured memory is delineated by square brackets. To determine
    ! how this memory is specified, it is first necessary to determne 
    ! the existence of two matching pairs of square brackets.
    !------------------------------------------------------------------------
    pattern2 = '[]'
    nMemory = set_query(lstring, pattern2)

    if ( (nMemory == 0).and.(nMemory >= 4) ) then
       ! sanity check - syntax error, no brackets or more than one set
       call ESMF_LogMsgSetError(ESMF_FAILURE,"symbols not properly paired",    &
                rcToReturn=localrc)
    elseif (nMemory == 2) then
       !-----------------------------------------------------------------------
       ! there must be two pairs of matching brackets
       !-----------------------------------------------------------------------
       call set_locate(lstring,pattern2, nMemory, MemPos)
       iBeg = MemPos(1)
       iEnd = MemPos(2)
       pattern = ';'
       iRank = pattern_query(lstring(iBeg:iEnd),pattern) + 1
    else 
       ! brackets are mismatched
        call ESMF_LogMsgSetError(ESMF_FAILURE,"symbols not properly paired",   &
                 rcToReturn=localrc)
    endif
     
    !---------------------------------------------------------------------------
    end subroutine memory_rank
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine memory_topology(lstring, location, srcMulti, srcBlock,          &
                               dstMulti,dstBlock, localrc)
    !---------------------------------------------------------------------------
    ! routine checks the whole input string to determine if the memory topology
    ! consists of a single structured Logically Rectangular block of memory, or 
    ! multiple structured blocks. It also conducts syntax checking on the
    ! string. Output is through two variables; nBlock the number of single
    ! contigous blocks of memory, and nMult the number of multiple memory
    ! blocks.
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
    ! check if memory is multiple block
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
       call ESMF_LogMsgSetError( ESMF_FAILURE,                                 &
               "symbols not paired properly",                                  &
               rcToReturn=localrc)
    endif

    !---------------------------------------------------------------------------
    end subroutine memory_topology
    !---------------------------------------------------------------------------

!2345678901234567890123456789012345678901234567890123456789012345678901234567890
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
    character(len=*),     intent(in   ) :: lstring
    character(len=*),     intent(in   ) :: lpattern
    integer,              intent(inout) :: number_hits
    integer, dimension(:),intent(inout) :: hit_loc
    
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

!2345678901234567890123456789012345678901234567890123456789012345678901234567890
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


!===============================================================================
  end module ESMF_TestHarnessMod
!===============================================================================

