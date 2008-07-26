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
#define ESMF_FILENAME "ESMF_TestHarnessReportMod"
!
!  ESMF Test Harness Report Module
   module ESMF_TestHarnessReportMod
!
!===============================================================================
!
! This file contains functions/subroutines for the Testing Harness.
! These methods are used only by the test harness driver ESMF_TestHarnessUTest.F90
!
!-------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!===============================================================================
!BOPI
! !MODULE: ESMF_TestHarnessReportMod
!
! !DESCRIPTION:
!
! The code in this file contains  Modules for reporting the test harness
! results.
!
!-------------------------------------------------------------------------------
! !USES:

  use ESMF_Mod
  use ESMF_TestHarnessTypesMod
  use ESMF_TestHarnessUtilMod

  implicit none

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------
  public report_descriptor_string



!===============================================================================

  contains 

!===============================================================================

 !------------------------------------------------------------------------------
 ! Routines to Report Test Results
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
    ! print specifier filenames
    !---------------------------------------------------------------------------
    print*,iDfile,iGfile,'   > Specifier files:',trim(adjustL(                 &
           PDS%Dfiles(iDfile)%filename)),                                      &
           ', ',trim(adjustL(PDS%Gfiles(iGfile)%filename))

    do idist=1, PDS%Dfiles(iDfile)%nDspecs ! loop thru all dist and grid
    do igrid=1, PDS%Gfiles(iGfile)%nGspecs ! specifiers in a file
       !------------------------------------------------------------------------
       ! set STATUS string
       !------------------------------------------------------------------------
       if( PDS%test_record(iDfile,iGfile)%test_status(idist,igrid) ==          &
               HarnessTest_SUCCESS ) then
          lstatus = "SUCCESS:"
          istatus = 1
       elseif( PDS%test_record(iDfile,iGfile)%test_status(idist,igrid) ==      &
               HarnessTest_FAILURE ) then
          lstatus = "FAILURE:"
          istatus = 0
       elseif( PDS%test_record(iDfile,iGfile)%test_status(idist,igrid) ==      &
               HarnessTest_UNDEFINED ) then
          lstatus = ""
          istatus =-1 
       else
       ! error
          call ESMF_LogMsgSetError( ESMF_FAILURE,"invalid test status value ", &
               rcToReturn=localrc)
          return
       endif

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

 !------------------------------------------------------------------------------

!===============================================================================
  end module ESMF_TestHarnessReportMod
!===============================================================================
