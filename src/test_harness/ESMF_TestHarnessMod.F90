!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
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
#include <ESMF.h>
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

  ! IGrid Parameters
  integer, parameter :: Harness_IGridError        = 200
  integer, parameter :: Harness_TensorIGrid       = 201
  integer, parameter :: Harness_SphericalIGrid    = 202
  integer, parameter :: Harness_UnstructuredIGrid = 203


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
     integer :: igrid_config           ! index of igrid specification
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

  type igrid_record
     character(ESMF_MAXSTR) :: name
     integer :: topology                     ! key representing the geometry of the igrid
     integer :: rank                         ! rank of the igrid
     integer, pointer :: order(:)        ! axis number, zero for free.
     integer, pointer :: size(:)         ! number of igrid elements along axis
     integer, pointer :: stagger(:,:)    ! stagger location (axis,rank)
     logical, pointer :: periodicity(:)  ! (rank) periodicity along axis
     logical, pointer :: halo(:)         ! (rank) periodicity along axis
     integer, pointer :: halo_size(:,:)  ! (rank,2) halo size along axis
  end type igrid_record

  type dist_record
     character(ESMF_MAXSTR) :: name
     integer :: topology                ! key (simple block, block-cyclic, arbitrary)
     integer :: rank                    ! rank of distribution
     integer, pointer :: order(:)   ! axis number, zero for free.
     integer, pointer :: size(:)    ! number of DE for axis number, zero for free.
     integer, pointer :: period(:)  ! period for block-cyclic, zero for simple block
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

  type problem_descriptor_record
     character(ESMF_MAXSTR) :: string           ! problem descriptor string
     type(test_report), pointer :: report(:)    ! record of test result
     type(process_record) :: process            ! method process
     type(sized_char_array) :: distfiles        ! distribution specification files
     type(sized_char_array) :: igridfiles        ! igrid specification files
     type(memory_record) :: src_memory          ! memory topology
     type(memory_record) :: dst_memory          ! memory topology
     type(dist_record) :: src_dist  ! src distribution specification
     type(dist_record) :: dst_dist  ! dst distribution specification
     type(igrid_record) :: src_igrid  !  src igrid specification
     type(igrid_record) :: dst_igrid  !  dst igrid specification
  end type problem_descriptor_record

!
!==============================================================================

  contains 

!==============================================================================


 !----------------------------------------------------------------------------
 ! Routines to parse input files for descriptor string, and specifier files.
 !----------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  subroutine interpret_descriptor_string(problem_descriptor, returnrc)
  !-------------------------------------------------------------------------
  ! Parse a single  problem descriptor string for problem description items
  ! such as process (REDISTRIBUTION or REMAPPING), memory topology and rank,
  ! distribution and igrid rank.
  !-------------------------------------------------------------------------

  ! arguments
  type(problem_descriptor_record), intent(inout) :: problem_descriptor
  integer, intent(out) :: returnrc

  ! local variables
  integer :: localrc = ESMF_SUCCESS ! assume success

  ! test variables
  integer :: SrcMemRank, DstMemRank
  integer, allocatable :: SrcMemLoc(:), DstMemLoc(:)
  integer :: SrcMemBeg, SrcMemEnd, DstMemBeg, DstMemEnd
  integer :: SrcDistRank, DstDistRank, SrcIGridRank, DstIGridRank


  !-------------------------------------------------------------------------
  ! initialize variables
  !-------------------------------------------------------------------------
  returnrc = ESMF_SUCCESS

  ! Initialize to failure.
  problem_descriptor%process%name = 'NONE'
  problem_descriptor%process%tag = Harness_Error

  !-------------------------------------------------------------------------
  ! Determine testing process (REDISTRIBUTION or REMAPPING)
  !-------------------------------------------------------------------------
  print*,'pds is:',problem_descriptor%string
  call process_query(problem_descriptor%string,                        &
                     problem_descriptor%process%name,                  &
                     problem_descriptor%process%tag,                   &
                     problem_descriptor%process%location,              &
                     localrc)

  !-------------------------------------------------------------------------
  ! Determine memory layout (Logically Rectangular or General)
  !-------------------------------------------------------------------------
  if ( .NOT. memory_topology(problem_descriptor%string, localrc) ) then
     ! if memory structure is General, i.e. multiple logically rectangular
     ! chunks of memory.

  else  ! set memory structure to LOGICALLY_RECTANGULAR
     print*,'set memory structure to LOGICALLY_RECTANGULAR'
     call memory_rank(problem_descriptor%string,                       &
                      SrcMemRank, DstMemRank,                          &
                      SrcMemBeg, SrcMemEnd,                            &
                      DstMemBeg, DstMemEnd, localrc)

     allocate( SrcMemLoc(SrcMemRank+1), DstMemLoc(DstMemRank+1) )

     call memory_locate(problem_descriptor%string,                     &
                        SrcMemRank,                                    &
                        SrcMemBeg, SrcMemEnd,                          &
                        SrcMemLoc, localrc)

     call memory_locate(problem_descriptor%string,                     &
                        DstMemRank,                                    &
                        DstMemBeg, DstMemEnd,                          &
                        DstMemLoc, localrc)

     if (localrc /= ESMF_SUCCESS) then
        print*,'Syntax error in problem descriptor string:',    &
                                                  problem_descriptor%string
        returnrc = ESMF_FAILURE
        return
     endif

     !-------------------------------------------------------------------------
     ! Determine the problem's source distribution rank, order, and topology 
     !-------------------------------------------------------------------------
     SrcDistRank = dist_rank(problem_descriptor%string,                &
                             SrcMemBeg, SrcMemEnd, localrc)

     allocate( problem_descriptor%src_dist%order(SrcDistRank) )
     problem_descriptor%src_dist%rank = SrcDistRank
     call dist_query(problem_descriptor%string,                        &
                     SrcMemBeg, SrcMemEnd, SrcMemRank,                 &
                     problem_descriptor%src_dist%topology,             &
                     problem_descriptor%src_dist%order, localrc)
      
     !-------------------------------------------------------------------------
     ! Determine the problem's destination distribution rank, order, and topology 
     !-------------------------------------------------------------------------
     DstDistRank = dist_rank(problem_descriptor%string,                &
                             DstMemBeg, DstMemEnd, localrc)
     allocate( problem_descriptor%dst_dist%order(DstDistRank) )
     problem_descriptor%dst_dist%rank = DstDistRank
     call dist_query(problem_descriptor%string,                        &
                     DstMemBeg, DstMemEnd, DstMemRank,                 &
                     problem_descriptor%dst_dist%topology,             &
                     problem_descriptor%dst_dist%order, localrc)
             
     !-------------------------------------------------------------------------
     ! Determine source igrid rank and order
     !-------------------------------------------------------------------------
     SrcIGridRank = igrid_rank(problem_descriptor%string, SrcMemBeg, SrcMemEnd, localrc)
     allocate( problem_descriptor%src_igrid%order(SrcIGridRank) )
     problem_descriptor%src_igrid%rank = SrcIGridRank
     call igrid_query(problem_descriptor%string,                        &
                     SrcMemBeg, SrcMemEnd, SrcMemRank,                 &
                     problem_descriptor%src_igrid%topology,             &
                     problem_descriptor%src_igrid%order, localrc)
             
     !-------------------------------------------------------------------------
     ! Determine destination igrid rank and order
     !-------------------------------------------------------------------------
     DstIGridRank = igrid_rank(problem_descriptor%string, DstMemBeg, DstMemEnd, localrc)
     allocate( problem_descriptor%dst_igrid%order(DstIGridRank) )
     problem_descriptor%dst_IGrid%rank = DstIGridRank
     call igrid_query(problem_descriptor%string,                        &
                     DstMemBeg, DstMemEnd, DstMemRank,                 &
                     problem_descriptor%dst_igrid%topology,             &
                     problem_descriptor%dst_igrid%order, localrc)
            !
     print*,'SrcDistRank ', SrcDistRank,' DstDistRank ', DstDistRank
     print*,'SrcIGridRank ', SrcIGridRank,' DstIGridRank ', DstIGridRank

     deallocate( SrcMemLoc, DstMemLoc )
  endif       ! memory topology

  !-------------------------------------------------------------------------
  end subroutine interpret_descriptor_string
  !-------------------------------------------------------------------------


  !-------------------------------------------------------------------------
  subroutine read_distgrid(descriptor, returnrc)
  !-------------------------------------------------------------------------
  ! Routine extracts from the distgrid specifier files the DistGrid information
  !-------------------------------------------------------------------------

  ! arguments
  type(problem_descriptor_record), intent(inout) :: descriptor
  integer, intent(out) :: returnrc

  ! local variables
  integer :: localrc = ESMF_SUCCESS ! assume success

  type(ESMF_Config)   :: localcf

  integer :: count,totalcount
  integer :: src_dist_rank,dst_dist_rank
  integer, allocatable :: partialcount(:)
  character(ESMF_MAXSTR) :: lfile, ltag
  integer :: ncol
! logical :: flag

  ! initialize variables
  returnrc = ESMF_SUCCESS

  ! create a config handle and load the config file
  localcf = ESMF_ConfigCreate(localrc)

  ! loop over the number of DistGrid descriptor files, and count the total entries.
  print*,'number of descriptor files is:',descriptor%distfiles%size

  allocate( partialcount(descriptor%distfiles%size) )

  do count=1, descriptor%distfiles%size
     lfile = descriptor%distfiles%string(count)%name
     call ESMF_ConfigLoadFile( localcf, trim( lfile ), rc=localrc )
     if (localrc /= ESMF_SUCCESS) then
        print*,'error, could not open DistGrid specifier file:',trim( lfile )
        returnrc = ESMF_FAILURE
        return
     endif

     src_dist_rank = descriptor%src_dist%rank
     dst_dist_rank = descriptor%dst_dist%rank

     print*,'src/dst rank of dist is:',src_dist_rank,'/',dst_dist_rank

     ! so what tag to look for? if src_dist_rank = dst_dist_rank, then
     ! look for symmetric tag of the form distgrid_block_#D#D
 10  format('distgrid_block_',i1.0,'D',i1.0,'D')
     write(ltag,10) src_dist_rank,dst_dist_rank
     print*,'looking for tag:', trim( ltag )
     if( src_dist_rank /= dst_dist_rank) then
        print*,'error,src dist rank ',src_dist_rank,' /= dst dist rank ',dst_dist_rank
        returnrc = ESMF_FAILURE
        return
     endif
     ! obtain the number of config entries
     call ESMF_ConfigGetDim(localcf,partialcount(count),ncol,trim(ltag),rc=localrc)
  enddo
  totalcount = sum(partialcount)
  print*,'total dist specifiers:',totalcount


  !

  deallocate(partialcount)

  !-------------------------------------------------------------------------
  end subroutine read_distgrid
  !-------------------------------------------------------------------------


 !----------------------------------------------------------------------------
 ! Routines to search strings
 !----------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    integer function char2int( lstring, sloc, localrc )
    !------------------------------------------------------------------------
    ! This function converts a character representation of an integer digit
    ! between 0-9 into its integer equivalent.
    !------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   ) :: lstring
    integer,          intent(in   ) :: sloc
    integer,          intent(inout) :: localrc

    ! local variables
    integer :: ntemp

    ! initialize variables
    localrc = ESMF_SUCCESS

    !------------------------------------------------------------------------
    ! Convert string to integer
    !------------------------------------------------------------------------
    ntemp = iachar( lstring(sloc:sloc) )-iachar('0')
    
    ! check to see that values is within the acceptable range
    if ( ntemp < 0 .and. ntemp > 9 ) then
       localrc = ESMF_FAILURE
       ! syntax error, no igrid layout specified
       print*,'Syntax error, no igrid layout'
       char2int = 0
    else
       char2int = ntemp
    endif
    
    !------------------------------------------------------------------------
    end function char2int
    !------------------------------------------------------------------------

 !-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine dist_query(lstring, MemBeg, MemEnd, MemRank, MemTopology, MemOrder, localrc)
    !------------------------------------------------------------------------
    ! This subroutine returns distribution topology (B - block, C - block 
    ! cyclic, A - arbitrary) and order as specified by the descriptor string. 
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
    integer :: i
    integer, allocatable :: MemLoc(:)

    ! initialize variables
    localrc = ESMF_SUCCESS
    
    !------------------------------------------------------------------------
    ! Check each memory chunk and extract distribution information. 
    !------------------------------------------------------------------------
    pattern3 = 'BCA'
    allocate( MemLoc(MemRank) )
    call set_locate(lstring(MemBeg:MemEnd), pattern3, MemRank, MemLoc)
    MemLoc = MemLoc + MemBeg - 1     ! shift to global string position
    
    !------------------------------------------------------------------------
    ! check that distribution tags are consistent
    !------------------------------------------------------------------------
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

 !-------------------------------------------------------------------------

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
    localrc = ESMF_SUCCESS

    !------------------------------------------------------------------------
    ! Check each memory chunk to see if any of the dimensions are distributed. 
    !------------------------------------------------------------------------
    pattern3 = 'BCA'
    nDist = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nDist == 0 ) then
       localrc = ESMF_FAILURE
       ! syntax error, no distribution specified
       print*,'Syntax error, no distribution'
    endif
    dist_rank = nDist

    !------------------------------------------------------------------------
    end function dist_rank
    !------------------------------------------------------------------------

 !-------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine igrid_query(lstring, MemBeg, MemEnd, MemRank, MemTopology, MemOrder, localrc)
    !------------------------------------------------------------------------
    ! This subroutine returns igrid topology (G - tensor, S - spherical, 
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
    integer :: i
    integer, allocatable :: MemLoc(:)

    ! initialize variables
    localrc = ESMF_SUCCESS
    
    !------------------------------------------------------------------------
    ! Check each memory chunk and extract distribution information. 
    !------------------------------------------------------------------------
    pattern3 = 'GSU'
    allocate( MemLoc(MemRank) )
    call set_locate(lstring(MemBeg:MemEnd), pattern3, MemRank, MemLoc)
    MemLoc = MemLoc + MemBeg - 1     ! shift to global string position
    
    !------------------------------------------------------------------------
    ! check that distribution tags are consistent
    !------------------------------------------------------------------------
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
       
          MemTopology = Harness_TensorIGrid
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
                    
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'S' ) then
       
          MemTopology = Harness_SphericalIGrid
          MemOrder(i) = char2int( lstring, MemLoc(i)+1, localrc )
          
       elseif ( lstring( MemLoc(i):MemLoc(i) ) == 'U' ) then
       
          MemTopology = Harness_UnstructuredIGrid
          
       else  ! Error
       
          MemTopology = Harness_DistError
          
       endif
    enddo
  
    !------------------------------------------------------------------------
    end subroutine igrid_query
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !------------------------------------------------------------------------
    integer function igrid_rank(lstring, MemBeg, MemEnd, localrc)
    !------------------------------------------------------------------------
    ! This function returns the igrid rank as specified by the descriptor
    ! string.
    !------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(in   ) :: MemBeg, MemEnd
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=3) :: pattern3
    integer :: nIGrid

    ! initialize variables
    localrc = ESMF_SUCCESS

    !------------------------------------------------------------------------
    ! Check each memory chunk to see if any of the dimensions are associated
    ! with a igrid.
    !------------------------------------------------------------------------
    pattern3 = 'GSU'
    nIGrid = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nIGrid == 0 ) then
       ! syntax error, no igrid layout specified
       print*,'Syntax error, no igrid layout'
       localrc = ESMF_FAILURE
    endif
    igrid_rank = nIGrid

    !------------------------------------------------------------------------
    end function igrid_rank
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine memory_locate(lstring, MemCount, MemBeg, MemEnd, MemLoc, localrc)      
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

    ! initialize variables
    localrc = ESMF_SUCCESS

    ! initialize variables
    pattern = ';'
    MemLoc(1) = MemBeg

    allocate( mloc(MemCount-1) )

    call pattern_locate(lstring(MemBeg:MemEnd),pattern,MemCount-1,mloc)
    MemLoc(2:MemCount) = mloc(1:MemCount-1) + MemBeg -1
    MemLoc(MemCount+1) = MemEnd
 
    deallocate( mloc )
    
    !------------------------------------------------------------------------
    end subroutine memory_locate
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine memory_rank(lstring, SrcRank, DstRank, SrcBeg, SrcEnd,           &
                                                      DstBeg, DstEnd, returnrc)
    !------------------------------------------------------------------------
    ! If the memory topology is structured, this routine returns the memory
    ! rank as specified by the descriptor string. 
    ! 
    !------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(  out) :: SrcRank,DstRank
    integer,          intent(  out) :: SrcBeg, SrcEnd, DstBeg, DstEnd
    integer,          intent(  out) :: returnrc

    ! local variables
    character(len=1) :: pattern
    character(len=2) :: pattern2
    integer :: nMemory
    integer :: MemPos(4)

    logical :: flag

    ! initialize variables
    returnrc = ESMF_SUCCESS

    ! initialize variables
    flag = .false.

    !------------------------------------------------------------------------
    ! The structured memory is delineated by square brackets. To determine
    ! how this memory is specified, it is first necessary to determne 
    ! the existence of two matching pairs of square brackets.
    !------------------------------------------------------------------------
    pattern2 = '[]'
    nMemory = set_query(lstring, pattern2)

    if (nMemory ==0) then
       ! syntax error, no brackets
       print*,'Syntax error, no brackets'
       returnrc = ESMF_FAILURE
       return
    elseif (nMemory == 4) then
       !---------------------------------------------------------------------
       ! there must be two pairs of matching brackets
       !---------------------------------------------------------------------
       call set_locate(lstring,pattern2, nMemory, MemPos)
       SrcBeg = MemPos(1)
       SrcEnd = MemPos(2)
       DstBeg = MemPos(3)
       DstEnd = MemPos(4)
       if( lstring(SrcBeg:SrcBeg)=='[' .and. lstring(SrcEnd:SrcEnd)==']'  &
           .and. lstring(DstBeg:DstBeg)=='[' .and. lstring(SrcEnd:SrcEnd)==']' ) then
           print*,'two pairs of brackets found'
           flag = .true.
       endif
    else 
       ! brackets are mismatched
       print*,'Syntax error, missing or extra brackets'
       returnrc = ESMF_FAILURE
       return
    endif
    !------------------------------------------------------------------------
    ! if the brackets exist and match, continue to extract memory information
    !------------------------------------------------------------------------
    if (flag) then
       !---------------------------------------------------------------------
       ! Extract Memory Rank
       !---------------------------------------------------------------------
       pattern = ';'
       SrcRank = pattern_query(lstring(SrcBeg:SrcEnd),pattern) + 1
       DstRank = pattern_query(lstring(DstBeg:DstEnd),pattern) + 1
       
       !---------------------------------------------------------------------
       ! Extract Memory Rank
       !---------------------------------------------------------------------
    else ! not flag
       ! Syntax error, brackets not consistent
       print*,'Syntax error, brackets not consistent'
       returnrc = ESMF_FAILURE
       return
    endif       ! if memory chunks
     
    !------------------------------------------------------------------------
    end subroutine memory_rank
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !------------------------------------------------------------------------
    logical function memory_topology(lstring, localrc)
    !------------------------------------------------------------------------
    ! function checks the input test string to determine if the memory topology
    ! is structured (true) or unstructured (false). 
    !------------------------------------------------------------------------

    ! arguments
    character(len=ESMF_MAXSTR), intent(in   ) :: lstring
    integer,          intent(  out) :: localrc

    ! local variables
    character(len=2) :: pattern
    integer :: nMemory

    ! initialize variables
    localrc = ESMF_SUCCESS

    !--------------------------------------------------------------------
    ! Memory Layout (Single Chunk Logically Rectangular or General)
    !--------------------------------------------------------------------
    pattern = '()'
    nMemory = set_query(lstring, pattern)

    if ( nMemory == 0 ) then
       memory_topology = .true.
    else 
       memory_topology = .false.
    endif

    !------------------------------------------------------------------------
    end function memory_topology
    !----------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !----------------------------------------------------------------------
    subroutine pattern_locate(lstring, pattern, number_hits, hits)
    !----------------------------------------------------------------------
    ! Locates all instances of PATTERN in STRING, placing the positions in 
    ! hits.
    !----------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: lstring
    character(len=*), intent(in) :: pattern
    integer, intent(in)  :: number_hits
    integer, dimension(:), intent(out) :: hits

    ! local variables
    integer :: n, k, klast, count
    integer :: len_string, len_pattern

    ! initialize variables
    count = 0
    len_string = len(lstring)
    len_pattern = len(pattern)

    !----------------------------------------------------------------------
    ! error check
    !----------------------------------------------------------------------
    if (len_string <= len_pattern) print*,    &
                            'ERROR string_query: string shorter than pattern'
    if (len_pattern <= 0) print*, 'ERROR string_query: no pattern'
    !----------------------------------------------------------------------
    ! Search string for a match with pattern.
    !----------------------------------------------------------------------
    klast = 0
    do n=1,number_hits
       k = index(lstring(klast+1:len_string),pattern)
       if ( k > 0 ) then
          hits(n) = k+klast
          count = count + 1
          klast = k+klast
          k = 0
       else
          exit
       endif
    enddo
    !
    if (number_hits /= count) print*,'ERROR string_locate', number_hits, count
    
    !------------------------------------------------------------------------
    end subroutine pattern_locate
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !----------------------------------------------------------------------
    integer function pattern_query(string,pattern)
    !----------------------------------------------------------------------
    ! Counts the number of instances PATTERN is found in STRING,
    ! and returns zero if the pattern does not occur as a substring.
    !----------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: pattern

    ! local variables
    integer :: k, klast, count
    integer :: len_string, len_pattern

    ! initialize variables
    count = 0
    len_string = len(string)
    len_pattern = len(pattern)

    !----------------------------------------------------------------------
    ! error check
    !----------------------------------------------------------------------
    if (len_string <= len_pattern) print*,    &
                            'ERROR pattern_query: string shorter than pattern'
    if (len_pattern <= 0) print*, 'ERROR pattern_query: no pattern'
    !----------------------------------------------------------------------
    ! Search string for a match with pattern. 
    !----------------------------------------------------------------------
    klast = 0
    do 
       ! Examine a sub-string and find the fisrt instance of the pattern.
       k = index(string(klast+1:len_string),pattern) 
       ! if index is nonzero, there is a match, increment count and slide 
       ! sub-string window forward.
       if ( k > 0 ) then 
          count = count + 1
          klast = k + klast
          k = 0
       else
          exit
       endif  
    enddo

    pattern_query = count
    
    !------------------------------------------------------------------------
    end function pattern_query
    !------------------------------------------------------------------------

 !----------------------------------------------------------------------------

    !------------------------------------------------------------------------
    subroutine  process_query(lstring, lname, tag, location, returnrc)
    !------------------------------------------------------------------------
    ! routine checks the input test string for a method specifier. Acceptable
    ! methods include:
    ! REDIST (-->), BILINEAR REMAP (=B=>), CONSERVATIVE REMAP (=C=>), 
    ! SECOND ORDER CONSERVATIVE REMAP (=S=>), NEAREST NEIGHBOR REMAP (=N=>),
    ! EXCHANGE IGRID CONSERVATIVE REMAPPING (=E=>), and a USER SPECIFIED REMAP 
    ! METHOD (=X=>).
    ! 
    !------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   ) :: lstring
    character(len=ESMF_MAXSTR), intent(inout) :: lname
    integer,          intent(inout) :: tag
    integer,          intent(inout) :: location
    integer,          intent(  out) :: returnrc

    ! local variables
    character(len=2) :: pattern_remap
    character(len=3) :: pattern_redist
    character(len=4) :: pattern4
    integer :: nRedist, RedistPos(1), nRemap, RemapPos(1)

    ! initialize variables
    returnrc = ESMF_SUCCESS

    !--------------------------------------------------------------------
    ! Determine the Process (REDISTRIBUTION or REMAPPING)
    !--------------------------------------------------------------------
    pattern_redist = '-->'
    nRedist = pattern_query(lstring, pattern_redist)

    !--------------------------------------------------------------------
    ! Check if Process is  REDISTRIBUTION
    !--------------------------------------------------------------------
    if (nRedist == 1) then
       ! if redist keep position
       call pattern_locate(lstring, pattern_redist, nRedist, RedistPos)
       print*,'process is Redist'
       lname = 'REDIST'
       tag  = Harness_Redist
       location = RedistPos(1)
    elseif (nRedist > 1) then
       ! ERROR syntax error in process declaration
       print*,'ERROR syntax error in process declaration'
       returnrc = ESMF_FAILURE
       return

    elseif (nRedist == 0) then
       print*,'process is not redistribution, check to see if it is a remap'

       pattern_remap = '=>'
       nRemap = pattern_query(lstring, pattern_remap)
       !-----------------------------------------------------------------
       ! Check if Process is  REMAP
       !-----------------------------------------------------------------
       if (nRemap == 1) then
          ! if remap, keep position
          call pattern_locate(lstring, pattern_remap, nRemap, RemapPos)
          location = RemapPos(1)-2
          ! If Remapping, what type
          pattern4 = lstring(RemapPos(1)-2:RemapPos(1)+1)
          ! looks like remap, so check what method type.
          select case (pattern4)

              case('=B=>')
                 ! remap method Bilinear
                 print*,'remap method Bilinear'
                 lname = 'BilinearRemap'
                 tag  = Harness_BilinearRemap

              case('=C=>')
                 ! remap method first order conservative
                 print*,'remap method first order conservative'
                 lname = 'ConservRemap'
                 tag  = Harness_ConservRemap

              case('=S=>')
                 ! remap method second order conservative
                 print*,'remap method second order conservative'
                 lname = '2ndOrderConservRemap'
                 tag  = Harness_2ndConservRemap

               case('=E=>')
                  ! remap method exchange igrid
                  print*,'remap method exchange igrid'
                  lname = 'ExchangeIGridRemap'
                  tag  = Harness_ExchangeRemap

               case('=N=>')
                  ! remap method nearest neighbor
                  print*,'remap method nearest neighbor'
                  lname = 'NearestNeighborRemap'
                  tag  = Harness_NearNeighRemap

               case('=X=>')
                  ! remap method undefined/user provided
                  print*,'remap method undefined/user provided'
                  lname = 'UserProvidedRemap'
                  tag  = Harness_UserProvRemap

               case default
                  ! syntax error no recognized method specified
                  location = 0
                  print*,'syntax error no recognized method specified'
                  returnrc = ESMF_FAILURE
                  return

          end select  ! remap type

          elseif (nRemap > 1) then

             ! ERROR syntax error in process declaration
             print*,'ERROR syntax error in process declaration'
             returnrc = ESMF_FAILURE
             return

          elseif (nRemap == 0) then

             ! ERROR no process is specified
             print*,'ERROR no process is specified'
             returnrc = ESMF_FAILURE
             return

          endif         ! if remap
       endif            ! if redist

    !------------------------------------------------------------------------
    end subroutine process_query
    !------------------------------------------------------------------------

!--------------------------------------------------------------------------

    !----------------------------------------------------------------------
    subroutine set_locate(string, set, number_hits, hits)
    !----------------------------------------------------------------------
    ! Locates all instances of SET in STRING, placing the positions in
    ! hits.
    !----------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: set
    integer, intent(in)  :: number_hits
    integer, dimension(:), intent(out) :: hits

    ! local variables
    integer :: n, k, klast, count
    integer :: len_string, len_set

    ! initialize variables
    count = 0
    len_string = len(string)
    len_set = len(set)

    !----------------------------------------------------------------------
    ! error check
    !----------------------------------------------------------------------
    if (len_string <= len_set) print*,    &
                            'ERROR set_query: string shorter than set'
    if (len_set <= 0) print*, 'ERROR set_query: no set'
    !----------------------------------------------------------------------
    ! Search string for a match with set.
    !----------------------------------------------------------------------
    klast = 0
    do n=1,number_hits
       k = scan(string(klast+1:len_string),set)
       if ( k > 0 ) then
          hits(n) = k+klast
          count = count + 1
          klast = k+klast
          k = 0
       else
          exit
       endif
    enddo
    !
    if (number_hits /= count) print*,'ERROR set_locate'

    !------------------------------------------------------------------------
    end subroutine set_locate
    !------------------------------------------------------------------------

 !-------------------------------------------------------------------------

    !----------------------------------------------------------------------
    integer function set_query(lstring,set)
    !----------------------------------------------------------------------
    ! Counts the number of instances SET is found in STRING,
    ! and returns zero if the SET does not occur as a substring.
    !----------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in) :: lstring
    character(len=*), intent(in) :: set

    ! local variables
    integer :: k, klast, count
    integer :: len_string, len_set

    ! initialize variables
    count = 0
    len_string = len(lstring)
    len_set = len(set)
    !----------------------------------------------------------------------
    ! error check
    !----------------------------------------------------------------------
    if (len_string <= len_set) print*,    &
                            'ERROR set_query: string shorter than set'
    if (len_set <= 0) print*, 'ERROR set_query: no pattern'
    !----------------------------------------------------------------------
    ! Search string for a match with pattern.
    !----------------------------------------------------------------------
    klast = 0

    do
       ! Examine a sub-string and find the first instance of the SET.
       k = scan(lstring(klast+1:len_string),set)
       ! if index is nonzero, there is a match, increment count and slide
       ! sub-string window forward.
       if ( k > 0 ) then
          count = count + 1
          klast = k + klast
          k = 0
       else
          exit
       endif 
    enddo

    set_query = count
    
    !------------------------------------------------------------------------
    end function set_query
    !------------------------------------------------------------------------


!============================================================================
  end module ESMF_TestHarnessMod
!============================================================================

