! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!===============================================================================
#define ESMF_FILENAME "ESMF_TestHarnessUtilMod"
!
!  ESMF Test Harness Types Module
   module ESMF_TestHarnessUtilMod
!
!===============================================================================
!
! This file contains parameters, global data types, and parser 
! functions/subroutines for the Testing Harness.
! These methods are used by the test harness driver ESMF_TestHarnessUTest.F90
! and all the other test harness modules.
!
!-------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessUtilMod
!
! !DESCRIPTION:
!
! The code in this file contains basic utilities for string manipulation and 
! inputting configuration table entries and misc utility functions
!
!-------------------------------------------------------------------------------
! !USES:

  use ESMF_TestHarnessTypesMod


  implicit none

!===============================================================================
! minimum error neighborhood for regrid interpolation
real(ESMF_KIND_R8), parameter :: RegridMinNeighborhood = 1.0D-14


! global storage of test specification
type (harness_descriptor), save :: har

integer :: localPet
integer :: petCount
integer :: rootPet = Harness_rootPet


  contains 

!===============================================================================

 !------------------------------------------------------------------------------
 ! Routines to search strings
 !------------------------------------------------------------------------------

 !------------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    integer function calc_grid_rank(lstring, MemBeg, MemEnd, localrc)
    !---------------------------------------------------------------------------
    ! This function returns the grid rank as specified by the descriptor
    ! string.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=THARN_MAXSTR), intent(in   ) :: lstring
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
    pattern3 = 'GU'
    nGrid = set_query(lstring(MemBeg:MemEnd), pattern3)

    if ( nGrid == 0 ) then
       ! syntax error, no grid layout specified
       print*,'Syntax error, no grid layout'
       call ESMF_LogSetError( ESMF_FAILURE, msg="syntax error, no grid " //     &
                "layout specified", rcToReturn=localrc)
       return
    endif
    calc_grid_rank = nGrid

    !---------------------------------------------------------------------------
    ! if I've gotten this far without an error, then the routine has succeeded.
    !---------------------------------------------------------------------------
    localrc = ESMF_SUCCESS

    !---------------------------------------------------------------------------
    end function calc_grid_rank
    !---------------------------------------------------------------------------

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
       call ESMF_LogSetError( ESMF_FAILURE,                                 &
                 msg="character is not a digit between 0 and 9",                   &
                 rcToReturn=localrc)
        return
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
    integer function dist_rank(lstring, MemBeg, MemEnd, localrc)
    !---------------------------------------------------------------------------
    ! This function returns the distribution rank as specified by the 
    ! descriptor string. 
    !---------------------------------------------------------------------------

    ! arguments
    character(len=THARN_MAXSTR), intent(in   ) :: lstring
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
       call ESMF_LogSetError( ESMF_FAILURE,                              &
                msg="Syntax Error - no distribution indicated",                 &
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
    integer :: allocRcToTest

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
       call ESMF_LogSetError(ESMF_FAILURE, msg="asserted memory rank does not"  &
                // " agree with actual memory rank", rcToReturn=localrc)
       return
    else
       !------------------------------------------------------------------------
       !------------------------------------------------------------------------
       allocate( MemPos(nMem), stat=allocRcToTest  )
       if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//        &
          "MemPos in memory_separate", rcToReturn=localrc)) then
       endif

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
    character(THARN_MAXSTR), intent(in   ) :: lstring
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
          call ESMF_LogSetError(ESMF_FAILURE,msg="symbols not properly paired", &
                   rcToReturn=localrc)
          return
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
          call ESMF_LogSetError(ESMF_FAILURE,msg="symbols not properly paired", &
                   rcToReturn=localrc)
          return
       endif

    else   ! syntax error
       call ESMF_LogSetError( ESMF_FAILURE, msg="symbols not paired properly",  &
                rcToReturn=localrc)
       return
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
    character(THARN_MAXSTR) :: pattern
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
       pattern = lstring(ib:iregrid+1)
       select case ( trim(adjustL(pattern)) )

          case('=B=>')
             ! remap method Bilinear
             lname = 'Bilinear REGRID'
             tag  = Harness_BilinearRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=P=>')
             ! remap method Bilinear
             lname = 'patch REGRID'
             tag  = Harness_PatchRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=C=>')
             ! remap method first order conservative
             lname = 'Conservative REGRID'
             tag  = Harness_ConservRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=S=>')
             ! remap method second order conservative
             lname = '2nd Order Conservative REGRID'
             tag  = Harness_2ndConservRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=E=>')
             ! remap method exchange grid
             lname = 'Exchange Grid REGRID'
             tag  = Harness_ExchangeRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=N=>')
             ! remap method nearest neighbor
             lname = 'Nearest Neighbor REGRID'
             tag  = Harness_NearNeighRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case('=U=>')
             ! remap method undefined/user provided
             lname = 'User Provided REGRID'
             tag  = Harness_UserProvRegrid
             symbol_loc(1) = ib
             symbol_loc(2) = iregrid +2

          case default
             ! syntax error - no recognized method specified
             call ESMF_LogSetError( ESMF_FAILURE,                           &
                      msg="process symbol not recognized",                         &
                      rcToReturn=localrc)
             lname = 'ERROR'
             tag  = Harness_Error
             symbol_loc(1) = 0
             symbol_loc(2) = 0
             return

          end select  ! remap type
      elseif( (iredist == 0).and.(iregrid == 0) ) then
         ! syntax error - no action
         call ESMF_LogSetError( ESMF_FAILURE,                               &
               msg="no process symbol found",                                      &
               rcToReturn=localrc)
         lname = 'ERROR'
         tag = Harness_Error
         return

      elseif( (iredist /= 0).and.(iregrid /= 0) ) then
         ! syntax error - multiple actions
         call ESMF_LogSetError( ESMF_FAILURE,                               &
                  msg="more than one process symbol found",                        &
                  rcToReturn=localrc)
         lname = 'ERROR'
         tag = Harness_Error
         return

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
    logical function pattern_match(lstring, lcharL, lcharR, rc)
    !---------------------------------------------------------------------------
    ! function checks the input string and determines if there are matching 
    ! pairs of the enclosing symbols ( lcharL, lcharR) and that they are in the 
    ! proper order.
    !---------------------------------------------------------------------------

    ! arguments
    character(len=*), intent(in   ) :: lstring
    character(len=*), intent(in   ) :: lcharL, lcharR
    integer, optional :: rc

    ! local variables
    integer, allocatable :: locL(:), locR(:)
    integer ::  k, ntestL, ntestR
    integer :: allocRcToTest
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
       allocate( locL(ntestL), stat=allocRcToTest )
       if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//        &
          "locL in pattern_match", rcToReturn=rc)) then
       endif
       allocate( locR(ntestR), stat=allocRcToTest )
       if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array "//        &
          "locR in pattern_match", rcToReturn=rc)) then
       endif
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
  character(THARN_MAXSTR), intent(in   ) :: lfilename, descriptor_label
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
  character(THARN_MAXSTR) :: lchar, ltmp
  integer :: int_tmp

  logical :: flag

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
     if( ESMF_LogFoundError(localrc,                                        &
         msg="cannot read row " // trim(adjustL(lchar)) //                         &
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
        call ESMF_ConfigNextLine(localcf, tableEnd=flag, rc=localrc)
        write(lchar,"(i5)") irow
        if( ESMF_LogFoundError(localrc,                                     &
           msg="cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read and discard continuation symbol
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute integer - contin symbol '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogFoundError(localrc,                                     &
           msg="cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read string from table
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute integer - after contin symbol '
        call ESMF_ConfigGetAttribute(localcf, int_tmp, rc=localrc)
        if( ESMF_LogFoundError(localrc,                                     &
           msg="cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        kelements = kelements + 1
        int_value = int_tmp

     else
        !-----------------------------------------------------------------------
        ! error continuation line missing, but grid not finished
        !-----------------------------------------------------------------------
        write(lchar,"(i5)") irow
        call ESMF_LogSetError( ESMF_FAILURE,                                &
              msg="cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc)
        return
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
  character(THARN_MAXSTR), intent(in   ) :: lfilename, descriptor_label
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
  character(THARN_MAXSTR) :: lchar, ltmp
  real(ESMF_KIND_R8) :: flt_tmp

  logical :: flag

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
     if( ESMF_LogFoundError(localrc,                                        &
         msg="cannot read row " // trim(adjustL(lchar)) //                         &
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
        call ESMF_ConfigNextLine(localcf, tableEnd=flag, rc=localrc)
        write(lchar,"(i5)") irow
        if( ESMF_LogFoundError(localrc,                                     &
           msg="cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return
        !-----------------------------------------------------------------------
        ! read and discard continuation symbol
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute - contin symbol '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogFoundError(localrc,                                     &
           msg="cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read string from table
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute - real after continuation '
        call ESMF_ConfigGetAttribute(localcf, flt_tmp, rc=localrc)
        if( ESMF_LogFoundError(localrc,                                     &
           msg="cannot read row " // trim(adjustL(lchar)) //                       &
           " of table " //trim(descriptor_label) // "in file " //              &
           trim(lfilename), rcToReturn=rc) ) return

        kelements = kelements + 1
        flt_value = flt_tmp

     else
        !-----------------------------------------------------------------------
        ! error continuation line missing, but grid not finished
        !-----------------------------------------------------------------------
        write(lchar,"(i5)") irow
        call ESMF_LogSetError( ESMF_FAILURE,msg=" continuation missing " //     &
              "cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc)
        return
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
  character(THARN_MAXSTR), intent(in   ) :: lfilename, descriptor_label
  character(THARN_MAXSTR), intent(  out) :: string
  integer, intent(in   ) :: ncolumns(:), new_row(:)
  integer, intent(inout) :: irow       ! current row of table
  integer, intent(in   ) :: nrows      ! total rows in table
  integer, intent(inout) :: kelements  ! current element of row irow of table
  type(ESMF_Config), intent(inout) :: localcf
  integer, intent(inout) :: rc

  ! local parameters
  integer :: localrc ! local error status

  ! local character strings
  character(THARN_MAXSTR) :: ltmp, lchar

  logical :: flag

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
     if( ESMF_LogFoundError(localrc,                                        &
         msg="cannot read row " // trim(adjustL(lchar)) //                         &
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
        call ESMF_ConfigNextLine(localcf, tableEnd=flag, rc=localrc)
        write(lchar,"(i5)") irow
        if( ESMF_LogFoundError(localrc,                                     &
              msg="cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc) ) return
        !-----------------------------------------------------------------------
        ! read and discard continuation symbol
        !-----------------------------------------------------------------------
        if( debug_flag) print*,' get attribute - read continuation symbol '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogFoundError(localrc,                                     &
              msg="cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc) ) return

        !-----------------------------------------------------------------------
        ! read string from table
        !-----------------------------------------------------------------------
        if( debug_flag)  print*,' get attribute string after continuation '
        call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
        if( ESMF_LogFoundError(localrc,                                     &
              msg="cannot read row " // trim(adjustL(lchar)) //                    &
              " of table " //trim(descriptor_label) // "in file " //           &
              trim(lfilename), rcToReturn=rc) ) return

        kelements = kelements + 1
        string = ltmp

     else
        !-----------------------------------------------------------------------
        ! error continuation line missing, but grid not finished
        !-----------------------------------------------------------------------
        write(lchar,"(i5)") irow
        call ESMF_LogSetError( ESMF_FAILURE,msg=" continuation missing " //     &
               "cannot read row " // trim(adjustL(lchar)) //                   &
               " of table " //trim(descriptor_label) // "in file " //          &
               trim(lfilename), rcToReturn=rc)
        return
     endif    ! new line
  endif    ! 

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_table_string
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !
  ! check an actual value against and expected value within a (relative) tolerance
  !
  logical function check_value (exp_val, act_val, tol)
    real(ESMF_KIND_R8), intent(in) :: exp_val
    real(ESMF_KIND_R8), intent(in) :: act_val
    real(ESMF_KIND_R8), intent(in) :: tol

    real(ESMF_KIND_R8) :: abs_err
    real(ESMF_KIND_R8) :: tol_band

    abs_err  = abs(act_val - exp_val)
    tol_band = abs(tol * exp_val) + RegridMinNeighborhood

    check_value = abs_err .LT. tol_band

#if 1
    ! print error message if value rejected because its outside of the minimum tolerance band
    if (.NOT. check_value) then
      if (abs_err .GE. RegridMinNeighborhood) then
        call ESMF_LogSetError (ESMF_FAILURE, msg="regrid error - value outside of minimum tolerance band", &
          line=__LINE__, file=__FILE__)
      end if
    end if
#endif

! debug
#if 0
    if (.NOT. check_value) then
      print *, "check_value - value out of tolerance(debug)", exp_val, act_val, abs_err, tol_band
    endif
#endif

    return
  !-----------------------------------------------------------------------------
  end function check_value
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !
  ! check an ESMF return code, display file/line no of error location
  !
  logical function CheckError (checkpoint, line, file, rcValue, msg, rcToReturn)
    logical,          intent(in)  :: checkpoint
    integer,          intent(in)  :: line
    character(len=*), intent(in)  :: file
    character(len=*), intent(in)  :: msg
    integer,          intent(in)  :: rcValue
    integer,          intent(out) :: rcToReturn

    if (checkpoint) then
      print '("checkpoint at line ", I5, " in file ", A)', line, file
    end if

    rcToReturn = ESMF_SUCCESS

    CheckError = ESMF_LogFoundError (rcValue, msg=msg, rcToReturn=rcToReturn)

    if (CheckError) then
      print '("error detected at line ", I5, " in file ", A, " - return code = ", I8)', &
        line, file, rcToReturn
      print '("     ", A)', msg
    end if

    return
  !-----------------------------------------------------------------------------
  end function CheckError
  !-----------------------------------------------------------------------------

!-----------------------------------------------------------------------


!===============================================================================
  end module ESMF_TestHarnessUtilMod
!===============================================================================
