! $Id: ESMF_TestHarnessReportMod.F90,v 1.9.2.1 2010/02/05 22:35:15 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
#include "ESMF.h"
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

  use ESMF_TestHarnessUtilMod

  implicit none

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------
  public report_descriptor_string, construct_descriptor_string



!===============================================================================

  contains 

!===============================================================================

 !------------------------------------------------------------------------------
 ! Routines to Report Test Results
 !------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine construct_descriptor_string(PDS, nstatus, localPet, localrc)
  !-----------------------------------------------------------------------------
  ! routine constructs the test repost string and prints the test configurations
  ! before the test commences.
  ! report string takes the form:
  ! {status}: {source string} {action} {destination string}
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(inout) :: PDS
  integer, intent(in   ) :: nstatus, localPet
  integer, intent(  out) :: localrc

  ! local character strings
  character(ESMF_MAXSTR) :: src_string, dst_string
  character(ESMF_MAXSTR) :: lstatus, laction, lgrid, ldist, lsuffix, ltmp
  character(7) :: lsize, lorder, ltmpL, ltmpR, l1, l2, l3, l4  

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
    case( Harness_BilinearRegrid )
      laction = "=B=>"
    case( Harness_PatchRegrid )
      laction = "=P=>"
    case( Harness_ConservRegrid )
      laction = "=C=>"
    case( Harness_2ndConservRegrid )
      laction = "=S=>"
    case( Harness_NearNeighRegrid )
      laction = "=N=>"
    case( Harness_Error )
      ! error
    case default
      ! error
  end select

  ! print out result string if codes match
  if( localPet == Harness_rootPet .and. nstatus > 0 ) then
     print*,'Problem Descriptor String ',trim(adjustL(PDS%pds))
     print*,'( grid config, distribution config, Grid file, Distribution file)'
  endif

  do iDfile=1, PDS%nDfiles    ! loop through each of the dist specifier files
  do iGfile=1, PDS%nGfiles    ! loop through each of the grid specifier files
    !---------------------------------------------------------------------------
    ! print specifier filenames
    !---------------------------------------------------------------------------
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


      ! save result string to character array for later use
      PDS%test_record(iDfile,iGfile)%test_string(idist,igrid)%string =         &
          trim(adjustL(src_string)) // trim(adjustL(laction)) //               &
          trim(adjustL(dst_string))

      !-------------------------------------------------------------------------
      ! print out result string if codes match
      !-------------------------------------------------------------------------
      if( localPet == Harness_rootPet .and. nstatus > 0 ) then
         write(l1,"(i6)") igrid
         write(l2,"(i6)") idist
         write(l3,"(i6)") iDfile
         write(l4,"(i6)") iGfile
         ltmp = '(' // trim(adjustL(l1)) // ',' // trim(adjustL(l2)) // ','    &
            // trim(adjustL(l3)) // ',' //  trim(adjustL(l4)) //               &
            ') string=' //                                                     &
            trim(PDS%test_record(iDfile,iGfile)%test_string(idist,igrid)%string)
         write(*,*) trim( ltmp )
      endif

    enddo    ! idist
    enddo    ! igrid

  enddo    ! iGfile
  enddo    ! iDfile

  print*,'               '

  !-----------------------------------------------------------------------------
  ! if I've gotten this far without an error, then the routine has succeeded.
  !-----------------------------------------------------------------------------
  localrc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  end subroutine construct_descriptor_string
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  subroutine report_descriptor_string(PDS, iG, iD, iGfile, iDfile,             &
                reportType, localPET, localrc)
  !-----------------------------------------------------------------------------
  ! routine displays the test result, followed by the problem descriptor string,
  ! followed by the entry number and file names for the grid and distribution 
  ! specifiers.
  ! 
  ! report string takes the form:
  ! {status}: {source string} {action} {destination string}
  !-----------------------------------------------------------------------------
  ! arguments
  type(problem_descriptor_strings), intent(inout) :: PDS
  integer, intent(in   ) :: iG, iD, iDfile, iGfile
  character(ESMF_MAXSTR), intent(in   ) :: reportType
  integer, intent(in   ) :: localPET
  integer, intent(  out) :: localrc

  ! local character strings
  character(ESMF_MAXSTR) :: lstatus, lout, test_string
  character(7) :: l1, l2, lpet

  ! local integer variables
  integer :: test_status

  ! initialize return flag
  localrc = ESMF_RC_NOT_IMPL


  !-----------------------------------------------------------------------------
  ! extract variables
  !-----------------------------------------------------------------------------
  test_string = PDS%test_record(iDfile,iGfile)%test_string(iD,iG)%string
  test_status = PDS%test_record(iDfile,iGfile)%test_status(iD,iG)

  !-----------------------------------------------------------------------------
  ! output format 
  !-----------------------------------------------------------------------------
  write(l1,"(i6)") iG
  write(l2,"(i6)") iD
  write(lpet,"(i6)") localPET

 10  format('PET(',a,') ', a, /,'   > element=', a,' of grid file= ', a,       &
            /,'   > element=', a,' of dist file= ', a)
 
  !-----------------------------------------------------------------------------
  ! set STATUS string
  !-----------------------------------------------------------------------------
  select case (trim(adjustL(reportType)))
     case("FULL")
     !--------------------------------------------------------------------------
     ! report both successes and failures
     !--------------------------------------------------------------------------
     if( test_status == HarnessTest_SUCCESS ) then
        lstatus = "SUCCESS:"
        lout = trim(adjustL(lstatus)) // trim(adjustL(test_string))
        write(*,10) trim(adjustL(lpet)), trim(adjustL(lout)),                  &
           trim(adjustL(l1)), trim(adjustL(PDS%Gfiles(iGfile)%filename)),      &
           trim(adjustL(l2)), trim(adjustL(PDS%Dfiles(iDfile)%filename)) 
     elseif( test_status == HarnessTest_FAILURE ) then
        lstatus = "FAILURE:"
        lout = trim(adjustL(lstatus)) // trim(adjustL(test_string))
        write(*,10) trim(adjustL(lpet)), trim(adjustL(lout)),                  &
           trim(adjustL(l1)), trim(adjustL(PDS%Gfiles(iGfile)%filename)),      &
           trim(adjustL(l2)), trim(adjustL(PDS%Dfiles(iDfile)%filename)) 
     else
        ! error
        call ESMF_LogMsgSetError( ESMF_FAILURE,"invalid test status value ",   &
             rcToReturn=localrc)
        return
     endif

     case("FAILURE")
     !--------------------------------------------------------------------------
     ! only report failures
     !--------------------------------------------------------------------------
     if( test_status == HarnessTest_FAILURE ) then
        lstatus = "FAILURE:"
        lout = trim(adjustL(lstatus)) // trim(adjustL(test_string))
        write(*,10) trim(adjustL(lpet)), trim(adjustL(lout)),                  &
           trim(adjustL(l1)), trim(adjustL(PDS%Gfiles(iGfile)%filename)),      &
           trim(adjustL(l2)), trim(adjustL(PDS%Dfiles(iDfile)%filename)) 
     endif
     
     case("SUCCESS")
     !--------------------------------------------------------------------------
     ! only report success
     !--------------------------------------------------------------------------
     if( test_status == HarnessTest_SUCCESS ) then
        lstatus = "SUCCESS:"
        lout = trim(adjustL(lstatus)) // trim(adjustL(test_string))
        write(*,10) trim(adjustL(lpet)), trim(adjustL(lout)),                  &
           trim(adjustL(l1)), trim(adjustL(PDS%Gfiles(iGfile)%filename)),      &
           trim(adjustL(l2)), trim(adjustL(PDS%Dfiles(iDfile)%filename)) 
     endif

     case("NONE")
     !--------------------------------------------------------------------------
     ! no report
     !--------------------------------------------------------------------------
  
     case default
     ! error
     print*,"error, report flag improperly set"
     call ESMF_LogMsgSetError( ESMF_FAILURE," report flag improperly set",     &
               rcToReturn=localrc)
     return

  end select  ! case of report flag

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
