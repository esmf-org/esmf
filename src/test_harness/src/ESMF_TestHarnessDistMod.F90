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
#define ESMF_FILENAME "ESMF_TestHarnessDistMod"
!
!  ESMF Test Harness DistGrid Utility Module
   module ESMF_TestHarnessDistMod
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
! !MODULE: ESMF_TestHarnessDistMod
!
! !DESCRIPTION:
!
! The code in this module contains data types and basic functions for accessing
! distribution specifications needed for the {\tt ESMF\_TestHarness}.
!
!-------------------------------------------------------------------------------
! !USES:

! use ESMF
! use ESMF_TestHarnessTypesMod
  use ESMF_TestHarnessUtilMod

  implicit none

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
  public read_dist_specification

!
!===============================================================================

  contains 

!===============================================================================

  !-----------------------------------------------------------------------------
  subroutine read_dist_specification( nPEs, Dfile, DstMem, SrcMem, rc)
  !-----------------------------------------------------------------------------
  ! routine to read the dist specifier files and populate the Dfile type
  !
  ! each dist specifier entry contains:
  ! (0) discriptive string
  ! then for each dist rank
  ! (1) a divider tag (SRC, DST, END)                      
  ! (2) a series of size specifiers for each dimension (==, =+, =*)
  ! (3) a termination tag (END)                      
  !-----------------------------------------------------------------------------
  ! arguments
  integer, intent(in   ) :: nPEs
  type(dist_record),   intent(inout) :: Dfile 
  type(memory_config), intent(in   ) :: DstMem   ! destination memory configuration
  type(memory_config), intent(in   ) :: SrcMem   ! source memory configuration
  integer, intent(inout) :: rc

  ! local esmf types
  type(ESMF_Config) :: localcf

  ! local parameters
  integer :: localrc ! local error status
  type(character_array) ::  pattern(7)

  ! local character strings
  character(THARN_MAXSTR) :: ltmp, lchar, lnumb, ltag
  character(THARN_MAXSTR) :: dtag
  character(THARN_MAXSTR) :: distribution_label

  type(character_array) :: lop(56)  ! work space dimensioned 8*7 = max number
                                    ! of operators * maximum rank of distribution
  type(character_array) :: loper(7,8)  ! dimensioned 7x8 = rank of dist by # oper


  ! local reals
  real(ESMF_KIND_R8) :: opv(56)  ! work space dimensioned 8*7 = maximum number
                                 ! of operators * maximum rank of distribution
  real(ESMF_KIND_R8) :: op_val(7,8)  ! dimensioned 7x8 = rank of dist by # oper
  real(ESMF_KIND_R8) :: tvalue

  ! local logical
  logical :: flag

  ! local integer variables
  integer :: ntmp, src_rank, dst_rank
  integer :: irow, krow, nrows, idist, ndist, irank, erank
  integer :: n, k, kelements
  integer :: counter, sanity_counter, out_counter
  integer, allocatable :: ncolumns(:), new_row(:)
  integer :: numOp(8)  ! dimensioned 8 = allowable number of operators
  integer :: allocRcToTest

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  ! matching patterns
  pattern(1)%string = "D1"
  pattern(2)%string = "D2"
  pattern(3)%string = "D3"
  pattern(4)%string = "D4"
  pattern(5)%string = "D5"
  pattern(6)%string = "D6"
  pattern(7)%string = "D7"

  !-----------------------------------------------------------------------------
  ! open the distribution file
  !-----------------------------------------------------------------------------
  localcf = ESMF_ConfigCreate(rc=localrc)
  if( ESMF_LogFoundError(localrc, msg="cannot create config object",           &
                            rcToReturn=localrc) ) return

  print*,'Opening Dist specifier file  ',trim( Dfile%filename )

  call ESMF_ConfigLoadFile(localcf, trim( Dfile%filename ), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot load config file " // trim( Dfile%filename ),                 &
         rcToReturn=localrc) ) return

  !----------------------------------------------------------------------------
  ! find the appropriate DistGrid specifier table for the SRC and DST DistRanks
  !----------------------------------------------------------------------------
  !  create label "distgrid_block_ndmd" using DstMem%DistRank SrcMem%DistRank
10 format(i1,'d',i1,'d::')
  write(ltmp,10)  SrcMem%DistRank,DstMem%DistRank
  distribution_label = "distgrid_block_" // trim(adjustL(ltmp))

  if( debug_flag ) print*,' Dist Ranks ',DstMem%DistRank,SrcMem%DistRank,      &
                          trim(distribution_label)
  call ESMF_ConfigFindLabel(localcf, trim(distribution_label), rc=localrc )

  if (localrc .ne. ESMF_SUCCESS) print*,' could not find distribution label'
  if( ESMF_LogFoundError(localrc,msg="could not find distribution label " //    &
          trim(distribution_label), rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! determine the total number of table rows, continue only if not empty
  ! NOTE: the number of table rows >= number of dist entries due to the
  ! possibility of continued lines.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigGetDim(localcf, nrows, ntmp, label=trim(distribution_label),       &
                         rc=localrc)
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot get descriptor table size in file " // trim(Dfile%filename),  &
         rcToReturn=rc) ) return

  if( nrows .le. 0 ) then
     call ESMF_LogSetError(ESMF_FAILURE,msg="table "//trim(distribution_label)  &
             // " empty in file " //trim(Dfile%filename), rcToReturn=rc)
     return
  endif

  !-----------------------------------------------------------------------------
  ! extract the table column lengths of this file
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(distribution_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,                                           &
         msg="cannot find config label " // trim(distribution_label),              &
         rcToReturn=rc) ) return

  allocate( ncolumns(nrows), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array ncolumns in "// &
     " read_dist_specification", rcToReturn=rc)) then
  endif

  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     if( ESMF_LogFoundError(localrc,                                        &
             msg="cannot advance to next line of table " //                        &
              trim(distribution_label) // " in file " // trim(Dfile%filename), &
              rcToReturn=rc) ) return


      ncolumns(krow) = ESMF_ConfigGetLen(localcf, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS .or. ncolumns(krow) .lt. 1 ) then
        write(lchar,"(i5)") krow
        call ESMF_LogSetError( ESMF_FAILURE,                                &
                 msg="problem reading line " // trim(adjustl(lchar)) //            &
                 " of table in file " // trim(Dfile%filename), rcToReturn=rc)
        return
      endif
  enddo    ! end  krow

  !-----------------------------------------------------------------------------
  ! determine the actual number of dist specifications in the file by counting 
  ! lines not starting with the continuation symbol '&'. The number of actual
  ! specifications in the table is less than or equal to 'nrows', the number 
  ! of rows in the table. A new grid entry in a particular row is indicated by
  ! a non-zero value in 'new_row'. A value of zero in the array indicates that
  ! that that row starts with a continiued line. The non-zero value indicates 
  ! the number of the current grid being read.
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(distribution_label), rc=localrc )
  if( ESMF_LogFoundError(localrc,msg="cannot find config label " //             &
          trim(distribution_label),rcToReturn=rc) ) return

  allocate( new_row(nrows), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array new_row "//     &
     " in read_dist_specification", rcToReturn=rc)) then
  endif


  !-----------------------------------------------------------------------------
  ! count the number of actual grids (less than or equal to number of table rows)
  !-----------------------------------------------------------------------------
  ndist = 0
  do krow=1,nrows
     call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
     call ESMF_ConfigGetAttribute(localcf, ltmp, rc=localrc)
     if( trim(adjustL(ltmp)) == "&" ) then
     ! continuation line
        new_row(krow) = 0
     else
        ndist = ndist + 1
        new_row(krow) =  ndist
     endif
  enddo    ! end  krow
  Dfile%nDspecs = ndist

  !-----------------------------------------------------------------------------
  ! allocate storage for the dist information based on the calculated number of
  ! separate grid entries
  !-----------------------------------------------------------------------------
  allocate( Dfile%src_dist(Dfile%nDspecs), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array src_dist "//    &
     " in read_dist_specification", rcToReturn=rc)) then
  endif
  allocate( Dfile%dst_dist(Dfile%nDspecs), stat=allocRcToTest )
  if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array dst_dist "//    &
     " in read_dist_specification", rcToReturn=rc)) then
  endif

  !-----------------------------------------------------------------------------
  ! Read the specifications from the table:
  ! (1) start at the top of the table.
  ! (2) read the row elements until the end of the row is reached.
  ! (3) determine if all the elements are read; 
  !     (a) if not advance to the next line and continue to read elements until
  !         the end of the line is reached - repeat (3)
  !     (b) if all the elements read, skip to next row and repeat (2)
  !-----------------------------------------------------------------------------
  call ESMF_ConfigFindLabel(localcf, trim(distribution_label), rc=localrc )

  if( ESMF_LogFoundError(localrc,msg="cannot find config label " //             &
          trim(distribution_label),rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  ! move to the next line in the table and confirm that (1) the line doesn't
  ! start with a continuation symbol, and (2) that the line isn't empty.
  !-----------------------------------------------------------------------------

  irow = 1      ! start with first row
  idist = 0
  !-----------------------------------------------------------------------------
  ! as long as the current row is within the bounds of the table process entry
  !-----------------------------------------------------------------------------
  out_counter = 0
  do while(irow <= nrows)
    if (new_row(irow) == 0 ) exit
    !---------------------------------------------------------------------------
    ! start at the top of the table and read new specification. check that it is
    ! not a continuation symbol and not end of row
    !---------------------------------------------------------------------------
    if( ncolumns(irow) > 0 ) then
       call ESMF_ConfigNextLine(localcf, tableEnd=flag , rc=localrc)
       kelements = 0 ! first element on the row
       idist = idist + 1 ! index to count distributions in the table

       call read_table_string(dtag, kelements, irow, nrows, ncolumns, new_row, &
                Dfile%filename, distribution_label, localcf, localrc)  

       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return
    endif
    if( debug_flag ) print*,' distribution  ',dtag

    !---------------------------------------------------------------------------
    ! read the SRC distribution tag, first time through it should be SRC 
    !---------------------------------------------------------------------------
    call read_table_string(ltag, kelements, irow, nrows, ncolumns, new_row,    &
             Dfile%filename, distribution_label, localcf, localrc)  

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,rcToReturn=rc)) return

    if( trim(adjustL(ltag)) /= "SRC") then
       ! wrong tag, SRC expected - return error 
       write(lchar,"(i5)") irow
       call ESMF_LogSetError(ESMF_FAILURE,msg="wrong tag, SRC expected on line "&
                // trim(lchar) // " of table "// trim(distribution_label) //   &
                " in file " // trim(Dfile%filename), rcToReturn=rc)
       return
    endif

    !---------------------------------------------------------------------------
    ! read the SRC rank, should be between 1 and 7
    !---------------------------------------------------------------------------
    call read_table_integer(src_rank, kelements, irow, nrows, ncolumns,        &
             new_row, Dfile%filename, distribution_label, localcf, localrc)  
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                      &
             rcToReturn=rc)) return
    if( src_rank < 1 .or. src_rank > 7 ) then
       ! error - unacceptable rank
       write(lchar,"(i5)") irow
       write(lnumb,"(i5)") src_rank
       call ESMF_LogSetError(ESMF_FAILURE,msg="unacceptable rank, should be "   &
                // "> 1 and <= 7. Rank is " // trim(lnumb) // ". On line " //  &
                trim(lchar) // " of table " // trim(distribution_label) //     &
                " in file " // trim(Dfile%filename), rcToReturn=rc)
       return
    endif

    allocate( Dfile%src_dist(idist)%dsize(SrcMem%memRank), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array dsize "//    &
       " in read_dist_specification", rcToReturn=rc)) then
    endif
    Dfile%src_dist(idist)%drank = src_rank


    !---------------------------------------------------------------------------
    ! initialize counters
    !---------------------------------------------------------------------------
    sanity_counter = 0
    counter = 0

    do while( trim(adjustL(ltag)) /= "DST" .and. trim(adjustL(ltag)) /= "END"  )
       ! read distribution syntax operator 
       call read_table_string(ltag, kelements, irow, nrows, ncolumns,          &
                new_row, Dfile%filename, distribution_label, localcf, localrc)  

       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                   &
               rcToReturn=rc)) return

       if( pattern_query(trim(ltag), "==") /= 0 .or.                           &
           pattern_query(trim(ltag), "=+") /= 0 .or.                           &
           pattern_query(trim(ltag), "=*") /= 0 ) then
          ! valid operator so now read value
          call read_table_real(tvalue, kelements, irow, nrows, ncolumns,       &
                   new_row, Dfile%filename, distribution_label, localcf, localrc)  

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
                  rcToReturn=rc)) return

          if( debug_flag ) print*,' operator/value ',trim(ltag),tvalue
          ! save pair of values
          counter = counter + 1
          lop(counter)%string = ltag
          opv(counter) = tvalue
       endif

       ! test to avoid an infinite loop - drop out if too many iterations
       sanity_counter = sanity_counter + 1
       if( sanity_counter > 8*src_rank ) then
          ! infinite loop - post error and return
          write(lchar,"(i5)") irow
          call ESMF_LogSetError(ESMF_FAILURE,msg="SRC while loop not " //       &
                   "completing. Line " // trim(lchar) // " of table " //       &
                   trim(distribution_label) // " in file " //                  &
                   trim(Dfile%filename), rcToReturn=rc)
          return
       endif
    enddo   ! while

    !-----------------------------------------------------------------------------
    ! process operator and values
    !     - sort operators by rank dimension
    !     - search for case of equivalence operator "=="
    !-----------------------------------------------------------------------------
    do irank=1, src_rank
       n = 0
       numOp(irank) = 0
       do k=1, counter
          if( pattern_query(lop(k)%string,pattern(irank)%string) /= 0 ) then
             n = n + 1
             loper(irank,n)%string = lop(k)%string
             op_val(irank,n) = opv(k)
             numOp(irank) = n
             if( debug_flag ) print*,' operator ',trim(loper(irank,n)%string), &
                                     op_val(irank,n)
          endif 
       enddo   ! k
    enddo   ! irank

    !-----------------------------------------------------------------------------
    ! search for equivalence operators to know how to partition the total PE
    ! amoung the distribution axes. The base value = (nPE)**(1/erank), where
    ! erank is the effective rank = source rank - number of equivalences
    !-----------------------------------------------------------------------------
    erank = src_rank
    do irank=1, src_rank
       if( numOp(irank) > 0 .and. numOp(irank) < 9 ) then
          do n=1, numOp(irank)
             if( pattern_query(loper(irank,n)%string, "==") /= 0 ) then
                if( n > 1 ) then
                   ! error can't have both equivalence and multiple operators
                   write(lchar,"(i5)") irow
                   call ESMF_LogSetError(ESMF_FAILURE,msg="only single operator"&
                         // " allowed when equivalence operator used. Line "// &
                         trim(lchar) // " of table "//trim(distribution_label) &
                         // " in file " //trim(Dfile%filename), rcToReturn=rc)
                   return
                endif
                erank = erank - 1
             endif
          enddo   ! k
       else
          ! error operator missing for rank
          write(lchar,"(i5)") irow
          write(lnumb,"(i5)") irank
          call ESMF_LogSetError(ESMF_FAILURE,msg="operator missing from SRC" // &
                   " specification, rank " // trim(lnumb) // " Line " //       & 
                   trim(lchar) // " of table " // trim(distribution_label) //  &
                   " in file " //trim(Dfile%filename), rcToReturn=rc)
                   return
       endif
    enddo   ! irank

    !---------------------------------------------------------------------------
    ! compute the distribution dimension size.
    !---------------------------------------------------------------------------
    call dist_size(nPEs, erank, numOp, loper, op_val, Dfile%src_dist(idist),   &
                   SrcMem%memRank, rc)

  !-----------------------------------------------------------------------------
  ! DST Phase
  !-----------------------------------------------------------------------------
    if( debug_flag ) print*,' entering DST ', irow

    !---------------------------------------------------------------------------
    ! read the DST distribution tag, first time through it should be DST 
    !---------------------------------------------------------------------------
    if( debug_flag ) print*,'ltag ',ltag

    if( trim(adjustL(ltag)) /= "DST") then
       ! wrong tag, DST expected - return error 
       write(lchar,"(i5)") irow
       call ESMF_LogSetError(ESMF_FAILURE,msg="wrong tag, DST expected on line" &
                // trim(lchar) // " of table "// trim(distribution_label) //   &
                " in file " // trim(Dfile%filename), rcToReturn=rc)
       return
    endif

    !---------------------------------------------------------------------------
    ! read the DST rank, should be between 1 and 7
    !---------------------------------------------------------------------------
    call read_table_integer(dst_rank, kelements, irow, nrows, ncolumns,        &
             new_row, Dfile%filename, distribution_label, localcf, localrc)  
    if( debug_flag ) print*,' dst_rank', dst_rank, kelements
    if (ESMF_LogFoundError(localrc,ESMF_ERR_PASSTHRU,rcToReturn=rc)) return

    if( dst_rank < 1 .or. dst_rank > 7 ) then
       ! error - unacceptable rank
       write(lchar,"(i5)") irow
       write(lnumb,"(i5)") dst_rank
       call ESMF_LogSetError(ESMF_FAILURE,msg="unacceptable rank, should be > " &
                // "1 and <= 7. Rank is " // trim(lnumb) // ". On line " //    &
                trim(lchar) // " of table " // trim(distribution_label) //     &
                " in file " // trim(Dfile%filename), rcToReturn=rc)
       return
    endif

    allocate( Dfile%dst_dist(idist)%dsize(DstMem%memRank), stat=allocRcToTest )
    if (ESMF_LogFoundAllocError(allocRcToTest, msg="integer array dsize "//    &
       " in read_dist_specification", rcToReturn=rc)) then
    endif
    Dfile%dst_dist(idist)%drank = dst_rank

    !---------------------------------------------------------------------------
    ! initialize counters
    !---------------------------------------------------------------------------
    sanity_counter = 0
    counter = 0

    do while( trim(adjustL(ltag)) /= "END" .and. irow <= nrows )
       ! read distribution syntax operator 
       call read_table_string(ltag, kelements, irow, nrows, ncolumns,          &
                new_row, Dfile%filename, distribution_label, localcf, localrc)  

       if (ESMF_LogFoundError(localrc,ESMF_ERR_PASSTHRU,rcToReturn=rc)) return

       if( pattern_query(trim(ltag), "==") /= 0 .or.                           &
           pattern_query(trim(ltag), "=+") /= 0 .or.                           &
           pattern_query(trim(ltag), "=*") /= 0 ) then
          ! valid operator so now read value
          call read_table_real(tvalue, kelements, irow, nrows, ncolumns,       &
                   new_row, Dfile%filename, distribution_label, localcf, localrc)  

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
                  rcToReturn=rc)) return

          if( debug_flag ) print*,' operator/value ',trim(ltag),tvalue
          ! save pair of values
          counter = counter + 1
          lop(counter)%string = ltag
          opv(counter) = tvalue
       endif

       ! test to avoid an infinite loop - drop out if too many iterations
       sanity_counter = sanity_counter + 1
       if( sanity_counter > 8*dst_rank ) then
          ! infinite loop - post error and return
          write(lchar,"(i5)") irow
          call ESMF_LogSetError(ESMF_FAILURE,msg="DST while loop not completing"&
                   // " Line " // trim(lchar) // " of table " //               &
                   trim(distribution_label) // " in file " //                  &
                   trim(Dfile%filename), rcToReturn=rc)
          return
       endif
    enddo   ! while

    !---------------------------------------------------------------------------
    ! process operator and values
    !     - sort operators by rank dimension
    !     - search for case of equivalence operator "=="
    !---------------------------------------------------------------------------
    do irank=1, dst_rank
       n = 0
       numOp(irank) = 0
       do k=1, counter
          if( pattern_query(lop(k)%string,pattern(irank)%string) /= 0 ) then
             n = n + 1
             loper(irank,n)%string = lop(k)%string
             op_val(irank,n) = opv(k)
             numOp(irank) = n
             if( debug_flag ) print*,' operator ',trim(loper(irank,n)%string), &
                                     op_val(irank,n)
          endif 
       enddo   ! k
    enddo   ! irank

    !---------------------------------------------------------------------------
    ! search for equivalence operators to know how to partition the total PE
    ! amoung the distribution axes. The base value = (nPE)**(1/erank), where
    ! erank is the effective rank = source rank - number of equivalences
    !-----------------------------------------------------------------------------
    erank = dst_rank
    do irank=1, dst_rank
       if( numOp(irank) > 0 .and. numOp(irank) < 9 ) then
          do n=1, numOp(irank)
             if( pattern_query(loper(irank,n)%string, "==") /= 0 ) then
                if( n > 1 ) then
                   ! error can't have both equivalence and multiple operators
                   write(lchar,"(i5)") irow
                   call ESMF_LogSetError(ESMF_FAILURE,msg="only single operator"&
                   // " allowed when equivalence operator used. Line " //      &
                   trim(lchar) // " of table " // trim(distribution_label) //  &
                   " in file " //trim(Dfile%filename), rcToReturn=rc)
                   return
                endif
                erank = erank - 1
             endif
          enddo   ! k
       else
          ! error no operator for rank
          write(lchar,"(i5)") irow
          write(lnumb,"(i5)") irank
          call ESMF_LogSetError(ESMF_FAILURE,msg="operator missing from DST" // &
                   " specification, rank " // trim(lnumb) // " Line " //       & 
                   trim(lchar) // " of table " // trim(distribution_label) //  &
                   " in file " //trim(Dfile%filename), rcToReturn=rc)
                   return
       endif
    enddo   ! irank

    !---------------------------------------------------------------------------
    ! compute the distribution dimension size.
    !---------------------------------------------------------------------------
    call dist_size(nPEs, erank, numOp, loper, op_val, Dfile%dst_dist(idist),   &
                   DstMem%memRank, rc)

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
                trim(distribution_label) // " should be a new entry, but " // &
                "instead a continuation symbol was found. Line " //           &
                trim(lchar) // " of table " // trim(distribution_label) //    &
                " in file " //trim(Dfile%filename) // " had a continuation"   &
                // " symbol." , rcToReturn=rc)
                return
       endif
    elseif(irow+1 > nrows .and. trim(ltag) /= "END") then
       ! we should be done and can drop out, but there is no end tag
       call ESMF_LogSetError(ESMF_FAILURE,msg="should be at end of " //       &
                "table " // trim(distribution_label) // " but no end tag" // &
                " found. File " // trim(Dfile%filename) , rcToReturn=rc)
                return
    else
       ! we are at the end of the table so finish up.
       irow = irow+1
    endif

    ! sanity check to catch infinite loops
    out_counter = out_counter + 1
    if(out_counter > 1000 ) then
       call ESMF_LogSetError(ESMF_FAILURE,msg="we seem to have gotten into"    &
                // " an infinite loop reading table " //                      &
                trim(distribution_label) // " in file " //                    &
                trim(Dfile%filename), rcToReturn=rc)
                return
    endif
  enddo  ! while

  !-----------------------------------------------------------------------------
  ! clean up CF     
  !-----------------------------------------------------------------------------
  call ESMF_ConfigDestroy(localcf, rc=localrc) 
  if( ESMF_LogFoundError(localrc, msg="cannot destroy config object",            &
                            rcToReturn=rc) ) return

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine read_dist_specification
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine dist_size(nPEs, erank, noper, oper, value, dist, memRank, rc)
  !-----------------------------------------------------------------------------
  ! routine to populate the distribution sizes according to the specification
  ! file.
  !-----------------------------------------------------------------------------
  integer, intent(in   ) :: nPEs   ! number of PEs  
  integer, intent(in   ) :: erank  ! rank of dim seeded with base dist size
  integer, intent(in   ) :: noper(:)  ! number of operators 
  type(character_array), intent(in   ) :: oper(:,:)  ! operator
  real(ESMF_KIND_R8) :: value(:,:)  ! value for operator
  type(dist_specification_record), intent(inout) :: dist  ! dist spec record
  integer, intent(in   ) :: memRank  ! rank of memory
  integer, intent(inout) :: rc

  ! local integers
  integer :: ibase, irank, n

  ! local reals
  real(ESMF_KIND_R8) :: arg, base

  ! initialize return flag
  rc = ESMF_RC_NOT_IMPL

  !-----------------------------------------------------------------------------
  ! compute the base distribution dimension size. Base is always at least one.
  !-----------------------------------------------------------------------------
  ibase = 1
  if( erank > 0 ) then
     arg = 1.0*erank
     base = (nPEs)**(1.0/arg)
     ibase = int(base)
  endif

  !-----------------------------------------------------------------------------
  ! initialize distribution size to base value
  !-----------------------------------------------------------------------------
  do irank=1, dist%drank
     dist%dsize(irank) = ibase
  enddo

  !-----------------------------------------------------------------------------
  ! modify distribution size based on operators
  !-----------------------------------------------------------------------------
  do irank=1, dist%drank
    do n=1,noper(irank)
      if( pattern_query(trim(adjustL(oper(irank,n)%string)), "==") /= 0 ) then
        ! if equivalence operator, overwrite base value, and set to value
        dist%dsize(irank) = value(irank,n)
        if( noper(irank) > 1 ) then
          ! error - should only be one operator when equivalence is found
          call ESMF_LogSetError(ESMF_FAILURE,msg="only one operator " //        &
                "should be found when equivalence operator == is used. " //    &
                " More than one was found. ", rcToReturn=rc)
          return
        endif

      elseif( pattern_query(trim(adjustL(oper(irank,n)%string)),"=+") /= 0) then
        ! if addition operator, add new value to base value.
        dist%dsize(irank) =  dist%dsize(irank) + value(irank,n)

      elseif( pattern_query(trim(adjustL(oper(irank,n)%string)),"=*") /= 0) then
        ! if multiplication operator, multiply new value with base value.
        dist%dsize(irank) =  dist%dsize(irank) * value(irank,n)

      else
        ! error - unrecognized operator
        call ESMF_LogSetError(ESMF_FAILURE,msg="unrecognized operator " //      &
               trim(adjustL(oper(irank,n)%string)) // " found. ", rcToReturn=rc)
        return
      endif
    enddo   ! n
  enddo    !irank

  !-----------------------------------------------------------------------------
  ! check that each dimension has a minimum value of 1, specifically the memory
  ! locations beyond the dist rank. This means that tensor dimensions are
  ! distributed by one processor.
  !-----------------------------------------------------------------------------
  do irank=1, memRank
     if( dist%dsize(irank) < 1 ) dist%dsize(irank) = 1
  enddo    !irank

  !-----------------------------------------------------------------------------
  rc = ESMF_SUCCESS     
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  end subroutine dist_size
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

!===============================================================================
  end module ESMF_TestHarnessDistMod
!===============================================================================

