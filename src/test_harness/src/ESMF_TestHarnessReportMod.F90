! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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
! In addition, This module will print a summary report describing the the
! tests that will be run for a harness suite.
!
!-------------------------------------------------------------------------------
! !USES:

  use ESMF_TestHarnessTypesMod
  use ESMF_TestHarnessUtilMod

  implicit none

!-------------------------------------------------------------------------------
! PUBLIC METHODS:
!-------------------------------------------------------------------------------
  public report_descriptor_string, construct_descriptor_string
  public summary_report_generate



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
  character(THARN_MAXSTR) :: src_string, dst_string
  character(THARN_MAXSTR) :: lstatus, laction, lgrid, ldist, lsuffix, ltmp
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
          call ESMF_LogSetError( ESMF_FAILURE, msg="invalid test status value ", &
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
  character(THARN_MAXSTR), intent(in   ) :: reportType
  integer, intent(in   ) :: localPET
  integer, intent(  out) :: localrc

  ! local character strings
  character(THARN_MAXSTR) :: lstatus, lout, test_string
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
        call ESMF_LogSetError( ESMF_FAILURE,msg="invalid test status value ",   &
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
     call ESMF_LogSetError( ESMF_FAILURE, msg=" report flag improperly set",     &
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
!------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  subroutine summ_rpt_write_detail_line (DSpecNo, GSpecNo, &
      srcDist, dstDist, srcGrid, dstGrid, rptLun, localrc)
    integer,                         intent(in)  :: DSpecNo
    integer,                         intent(in)  :: GSpecNo
    type(dist_specification_record), intent(in)  :: srcDist
    type(dist_specification_record), intent(in)  :: dstDist
    type(grid_specification_record), intent(in)  :: srcGrid
    type(grid_specification_record), intent(in)  :: dstGrid
    integer,                         intent(in)  :: rptLun
    integer,                         intent(out) :: localrc

    integer :: RankNo

    localrc = ESMF_SUCCESS

    write (rptLun, 9001)

    write (rptLun, 9005) DSpecNo
    write (rptLun, 9006) GSpecNo

    ! make sure distribution rank is consistent
    if (srcDist%drank .NE. dstDist%drank) then
      write (rptLun, '("source distribution rank does not match destination rank", I4, 2x, I4)') &
        srcDist%drank, dstDist%drank
    end if

    ! make sure grid rank is consistent
    if (srcGrid%grank .NE. dstGrid%grank) then
      write (rptLun, '("source grid rank does not match destination rank", I4, 2x, I4)') &
        srcGrid%grank, dstGrid%grank
    end if

    ! make sure distribution and grid are consistent
    if (srcDist%drank .NE. srcGrid%grank) then
      write (rptLun, '("distribution rank does not match grid rank", I4, 2x, I4)') &
        srcDist%drank, srcGrid%grank
    end if

    write (rptLun, 9007) srcDist%drank
    write (rptLun, 9008) srcGrid%grank

    ! write source distribution
    do RankNo = 1, srcDist%drank
      write (rptLun, 9012) RankNo, srcDist%dsize(RankNo), RankNo
    end do

    ! write destination distribution
    do RankNo = 1, dstDist%drank
      write (rptLun, 9013) RankNo, dstDist%dsize(RankNo), RankNo
    end do

    ! write source grid
    do RankNo = 1, srcGrid%grank
      write (rptLun, 9020) RankNo, srcGrid%gsize(RankNo), RankNo
      write (rptLun, 9021) RankNo, srcGrid%grange(RankNo, 1), RankNo
      write (rptLun, 9022) RankNo, srcGrid%grange(RankNo, 2), RankNo
      write (rptLun, 9023) RankNo, trim(srcGrid%gtype(RankNo)%string), RankNo
      write (rptLun, 9024) RankNo, trim(srcGrid%gunits(RankNo)%string), RankNo
    end do

    ! write destination grid
    do RankNo = 1, dstGrid%grank
      write (rptLun, 9030) RankNo, dstGrid%gsize(RankNo), RankNo
      write (rptLun, 9031) RankNo, dstGrid%grange(RankNo, 1), RankNo
      write (rptLun, 9032) RankNo, dstGrid%grange(RankNo, 2), RankNo
      write (rptLun, 9033) RankNo, trim(dstGrid%gtype(RankNo)%string), RankNo
      write (rptLun, 9034) RankNo, trim(dstGrid%gunits(RankNo)%string), RankNo
    end do

    write (rptLun, 9002)

    9001 FORMAT ('        <Row>')
    9002 FORMAT ('        </Row>')

    9005 FORMAT ('          <DSpecNo>', I3, '</DSpecNo>')
    9006 FORMAT ('          <GSpecNo>', I3, '</GSpecNo>')
    9007 FORMAT ('          <DistRank>', I1, '</DistRank>')
    9008 FORMAT ('          <GridRank>', I1, '</GridRank>')

    9012 FORMAT ('          <SourceDistSize', I1, '>', I5, '</SourceDistSize', I1, '>')
    9013 FORMAT ('          <DestDistSize', I1, '>', I5, '</DestDistSize', I1, '>')

    ! EN format specifier displays real in engineering format (ISO 1539-1:2004 sect. 10.6.1.2.3)
    9020 FORMAT ('          <SourceGridSize', I1, '>', I5, '</SourceGridSize', I1, '>')
    9021 FORMAT ('          <SourceGridCoordLow', I1, '>', EN17.6, '</SourceGridCoordLow', I1, '>')
    9022 FORMAT ('          <SourceGridCoordHigh', I1, '>', EN17.6, '</SourceGridCoordHigh', I1, '>')
    9023 FORMAT ('          <SourceGridType', I1, '>', A16, '</SourceGridType', I1, '>')
    9024 FORMAT ('          <SourceGridUnits', I1, '>', A16, '</SourceGridUnits', I1, '>')

    9030 FORMAT ('          <DestGridSize', I1, '>', I5, '</DestGridSize', I1, '>')
    9031 FORMAT ('          <DestGridCoordLow', I1, '>', EN17.6, '</DestGridCoordLow', I1, '>')
    9032 FORMAT ('          <DestGridCoordHigh', I1, '>', EN17.6, '</DestGridCoordHigh', I1, '>')
    9033 FORMAT ('          <DestGridType', I1, '>', A16, '</DestGridType', I1, '>')
    9034 FORMAT ('          <DestGridUnits', I1, '>', A16, '</DestGridUnits', I1, '>')

  end subroutine summ_rpt_write_detail_line

  !-----------------------------------------------------------------------------
  subroutine summ_rpt_proc_dist_grid_files (DfileNo, GfileNo, DRec, GRec, rptLun, localrc)
    integer,            intent(in)  :: DFileNo
    integer,            intent(in)  :: GfileNo
    type(dist_record),  intent(in)  :: DRec
    type(grid_record),  intent(in)  :: GRec
    integer,            intent(in)  :: rptLun
    integer,            intent(out) :: localrc

    integer :: DSpecCnt
    integer :: DSpecNo

    integer :: GSpecCnt
    integer :: GSpecNo

    localrc = ESMF_SUCCESS

    write (rptLun, 9001)

    write (rptLun, 9012) DfileNo
    write (rptLun, 9013) GfileNo

    DSpecCnt = DRec%nDspecs
    write (rptLun, 9010) DSpecCnt

    GSpecCnt = GRec%nGspecs
    write (rptLun, 9011) GSpecCnt

    ! process all dist records
    do DSpecNo = 1,DSpecCnt
      ! process all grid records
      do GSpecNo = 1, GSpecCnt
        ! display distribution/grid
        call summ_rpt_write_detail_line (DSpecNo, GSpecNo, &
          DRec%src_dist(DSpecNo), DRec%dst_dist(DSpecNo), &
          GRec%src_grid(GSpecNo), GRec%dst_grid(GSpecNo), rptLun, localrc)
      end do
    end do

    write (rptLun, 9002)

    9001 FORMAT ('      <DistGridFile>')
    9002 FORMAT ('      </DistGridFile>')
    9010 FORMAT ('        <DSpecCnt>', I3, '</DSpecCnt>')
    9011 FORMAT ('        <GSpecCnt>', I3, '</GSpecCnt>')
    9012 FORMAT ('        <DFileNo>', I3, '</DFileNo>')
    9013 FORMAT ('        <GFileNo>', I3, '</GFileNo>')
  end subroutine summ_rpt_proc_dist_grid_files

  !-----------------------------------------------------------------------------
  subroutine summ_rpt_proc_prob_descr_string (pdsNo, probDescrStr, rptLun, localrc)
    integer,                           intent(in)  :: pdsNo
    type(problem_descriptor_strings),  intent(in)  :: probDescrStr
    integer,                           intent(in)  :: rptLun
    integer,                           intent(out) :: localrc

    !integer :: PdsCnt
    !integer :: PdsNo

    integer :: DfileCnt
    integer :: DfileNo

    integer :: GfileCnt
    integer :: GfileNo

    write (rptLun, 9001)
    write (rptLun, 9015) pdsNo
    write (rptLun, 9010) trim(probDescrStr%pds)

    write (rptLun, 9011)  trim(probDescrStr%process%string)
    write (rptLun, 9012) probDescrStr%process%tag

    DfileCnt = probDescrStr%nDFiles
    write (rptLun, 9013) DfileCnt

    GfileCnt = probDescrStr%nGFiles
    write (rptLun, 9014) GfileCnt

    ! for each distribution file
    do DfileNo = 1, DfileCnt
      ! for each grid file
      do GfileNo = 1, GfileCnt
        ! display dist file and grid file entry
        call summ_rpt_proc_dist_grid_files (DfileNo, GfileNo, &
          probDescrStr%Dfiles(DfileNo), probDescrStr%Gfiles(GfileNo), rptLun, localrc)
      end do
    end do

    write (rptLun, 9002)

    localrc = ESMF_SUCCESS

    90 continue

    9001 FORMAT ('    <ProbDescString>')
    9002 FORMAT ('    </ProbDescString>')
    9010 FORMAT ('      <PDS>', A, '</PDS>')
    9011 FORMAT ('      <Process>', A, '</Process>')
    9012 FORMAT ('      <ProcessCode>', I3, '</ProcessCode>')
    9013 FORMAT ('      <DistFileCnt>', I3, '</DistFileCnt>')
    9014 FORMAT ('      <GridFileCnt>', I3, '</GridFileCnt>')
    9015 FORMAT ('      <PdsNo>', I3, '</PdsNo>')

  end subroutine summ_rpt_proc_prob_descr_string

  !-----------------------------------------------------------------------------
  subroutine summ_rpt_proc_prob_descr_rec (pdrNo, probDescrRec, rptLun, localrc)
    integer,                           intent(in)  :: pdrNo
    type(problem_descriptor_records),  intent(in)  :: probDescrRec
    integer,                           intent(in)  :: rptLun
    integer,                           intent(out) :: localrc

    integer :: PdsCnt
    integer :: PdsNo

    write (rptLun, 9001)
    write (rptLun, 9012) pdrNo
    write (rptLun, 9010) trim(probDescrRec%filename)

    PdsCnt = probDescrRec%numStrings
    write (rptLun, 9011) PdsCnt

    do PdsNo = 1, PdsCnt
      call summ_rpt_proc_prob_descr_string (PdsNo, probDescrRec%str(PdsNo), rptLun, localrc)
    end do

    write (rptLun, 9002)

    localrc = ESMF_SUCCESS

    90 continue

    9001 FORMAT ('  <ProbDescRec>')
    9002 FORMAT ('  </ProbDescRec>')
    9010 FORMAT ('    <Filename>', A, '</Filename>')
    9011 FORMAT ('    <PdsCnt>', I3, '</PdsCnt>')
    9012 FORMAT ('    <PdrNo>', I4, '</PdrNo>')
  end subroutine summ_rpt_proc_prob_descr_rec

  !-----------------------------------------------------------------------------
  subroutine summary_report_generate (harnDesc, reportFname, localrc)
    type (harness_descriptor), intent(in)  :: harnDesc
    character(len=*),          intent(in)  :: reportFname
    integer,                   intent(out) :: localrc

    integer :: rptLun
    integer :: iostat
    logical :: openstat

    integer :: PdrNo
    integer :: PdrCnt

    integer :: timeStamp(8)

    ! initialize return status
    localrc = ESMF_FAILURE
    
    ! find an open unit number
    call ESMF_UtilIOUnitGet (rptLun, rc=localrc)
    if (ESMF_LogFoundError(localrc, msg="Unable to find available unit.")) &
      return ! bail out

    ! open report file
    open (unit=rptLun, file=reportFname, status='REPLACE', iostat=iostat, action='WRITE')
    if (iostat .NE. 0) then
      print '("error in summ_rpt_generate, unable to open report file - filename = ", A)', &
        reportFname
      go to 90
    end if

    ! get timestamp
    call date_and_time (values=timeStamp)

    ! write top level structure
    write (rptLun, 9000)
    !write (rptLun, 9001)
    write (rptLun, 9005)
    write (rptLun, 9010) timeStamp(1:3), timeStamp(5:7)
    write (rptLun, 9011) adjustL(trim(harnDesc%configPath))
    write (rptLun, 9012) adjustL(trim(harnDesc%topFname))
    write (rptLun, 9013) adjustL(trim(harnDesc%testClass))
    write (rptLun, 9014) trim(harnDesc%reportType)
    write (rptLun, 9015) trim(harnDesc%setupReportType)
    write (rptLun, 9016) harnDesc%numRecords

    ! problem_descriptor_records
    PdrCnt = harnDesc%numRecords
    do PdrNo = 1, PdrCnt
      call summ_rpt_proc_prob_descr_rec (PdrNo, harnDesc%rcrd(PdrNo), rptLun, localrc)
    end do

    ! write trailer
    write (rptLun, 9006)

    localrc = ESMF_SUCCESS

    ! return status & exit
    90 continue

    ! close report file
    inquire (rptLun, opened=openstat)
    if (openstat) then
      close (rptLun)
    end if

    9000 FORMAT ('<?xml version="1.0"?>')
    !9001 FORMAT ('<?xml-stylesheet type="application/xml" href="Harness.xslt"?>')
    9005 FORMAT ('<TopConfig>')
    9006 FORMAT ('</TopConfig>')
    9010 FORMAT ('  <Timestamp>', I4, '/', I2, '/', I2, 2x, I2, ':', I2, ':', I2, '</Timestamp>')
    9011 FORMAT ('  <ConfigPath>', A, '</ConfigPath>')
    9012 FORMAT ('  <TopFname>', A, '</TopFname>')
    9013 FORMAT ('  <TestClass>', A, '</TestClass>')
    9014 FORMAT ('  <ReportType>', A, '</ReportType>')
    9015 FORMAT ('  <SetUpReportType>', A, '</SetUpReportType>')
    9016 FORMAT ('  <PDRCnt>', I3, '</PDRCnt>')

  end subroutine summary_report_generate
  !-----------------------------------------------------------------------------
!===============================================================================
  end module ESMF_TestHarnessReportMod
!===============================================================================
