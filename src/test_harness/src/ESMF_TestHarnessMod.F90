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

  type memory_record
     character(ESMF_MAXSTR) :: string
     integer :: chunks               ! number of logically rectangular chunks
     integer, pointer :: rank(:)
  end type memory_record

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
     integer :: charsize
     type(character_array), pointer :: tag(:)
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
     integer :: numStrings                ! # of problem descriptor strings in record
     type(problem_descriptor_strings), pointer :: string(:)  ! problem descriptor strings
  end type problem_descriptor_records

  type harness_descriptor
     character(ESMF_MAXSTR) :: testClass   ! test class
     integer :: reportType                 ! report type 
     integer :: numRecords                 ! number of problem descriptor records
     type(problem_descriptor_records), pointer :: record(:)  ! problem descriptor record  
  end type harness_descriptor

!
!===============================================================================

  contains 

!===============================================================================


 !------------------------------------------------------------------------------
 ! Routines to parse input files for descriptor string, and specifier files.
 !------------------------------------------------------------------------------

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

  print*,' Determine grid layout'
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
  ! 
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
          ! syntax error, more than one type of distribution specified
          print*,'Syntax error, more than one type of distribution specified'
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
       localrc = ESMF_FAILURE
    endif
    grid_rank = nGrid

    !---------------------------------------------------------------------------
    end function grid_rank
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine memory_locate(lstring, MemCount, MemBeg, MemEnd, MemLoc,        &
                             localrc)      
    !---------------------------------------------------------------------------
    ! This subroutine returns the location of the memory delimiters as
    ! specified by the descriptor string.
    !---------------------------------------------------------------------------

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
    
    !---------------------------------------------------------------------------
    end subroutine memory_locate
    !---------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    subroutine memory_separate(lstring, iRank, lmem,  localrc)
    !---------------------------------------------------------------------------
    ! For a structured block of memory, return the memory rank as specified
    ! by the descriptor string. 
    ! 
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
       call ESMF_LogMsgSetError(ESMF_FAILURE,                                  &
                "asserted memory rank not agree with actual memory rank",      &
                rcToReturn=localrc)
    else
       !------------------------------------------------------------------------
       !------------------------------------------------------------------------
       allocate( MemPos(nMem) )
       call pattern_locate(lstring, pattern, nMem, MemPos)
       call pattern_locate(lstring, ']', nEnd, EndPos)

       !------------------------------------------------------------------------
       ! extract first memory location - not enclosed by "," on front side
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
       ! extract last memory location - not enclosed by "," on back side
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


!===============================================================================
  end module ESMF_TestHarnessMod
!===============================================================================

