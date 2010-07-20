! $Id: ESMF_XGridEx.F90,v 1.3 2010/07/20 21:10:20 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_XGridEx

!------------------------------------------------------------------------------
!ESMF_Disable_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
! !PROGRAM: ESMF_XGridEx - XGrid Examples
!
! !DESCRIPTION:
!
! This program shows examples of XGrid get data pointer methods
!-----------------------------------------------------------------------------
#include "ESMF.h"
    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF_Mod
    use ESMF_XGridMod
    use ESMF_XGridCreateMod
    implicit none
    
    ! Local variables
    integer                             :: localrc, i
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)
    type(ESMF_DistGrid)                 :: sideAdg(2), sideBdg(1)
    real*8                              :: centroid(12,2), area(12)
    type(ESMF_XGridSpec)                :: sparseMatA2X(2)

    integer :: finalrc
!   !Set finalrc to success
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(defaultlogfilename="XGridEx.Log", &
                    defaultlogtype=ESMF_LOG_MULTI, rc=localrc)
    if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!-------------------------------- Example -----------------------------
!>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>%
!BOE
!\subsubsection{XGrid from user input data}
!\label{sec:xgrid:usage:xgrid_createraw}
!
! XGrid can be created from user input data, such as Grids on either side,
! area and centroid information of XGrid cells, sparse matrix matmul information
! such as factorList and factorIndexList. 
! 
! In this example, we will set up a simple XGrid from overlapping Grids on
! either side of the XGrid. The Grids are laid out in the following figure.
!\begin{center}
!\begin{figure}
!\scalebox{0.75}{\includegraphics{XGridEx1}}
!\caption{Grid layout for simple XGrid creation example.}
!\label{fig:xgridsimple}
!\end{figure}
!\end{center}
!
!EOE
!BOC

    ! set up Grids on either side of XGrid
    ! - create side A distgrids
    sideAdg(1) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    sideAdg(2) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/1,2/), rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! - create side B distgrids
    sideBdg = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! - create side A Grids
    do i = 1, 2
        sideA(i) = ESMF_GridCreate(distgrid=sideAdg(i), rc=localrc)
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    enddo

    ! - create side B Grids
    do i = 1, 1
        sideB(i) = ESMF_GridCreate(distgrid=sideBdg(i), rc=localrc)
        if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
    enddo

    ! Set up the sparsematrix matmul information
    allocate(sparseMatA2X(1)%factorIndexList(2,9), sparseMatA2X(1)%factorList(9))
    allocate(sparseMatA2X(2)%factorIndexList(2,3), sparseMatA2X(2)%factorList(3))

    ! Refer to the figure above to check the numbering
    ! - set up mapping between A1 -> X
    sparseMatA2X(1)%factorIndexList(1,1)=1
    sparseMatA2X(1)%factorIndexList(1,2)=2
    sparseMatA2X(1)%factorIndexList(1,3)=2
    sparseMatA2X(1)%factorIndexList(1,4)=3
    sparseMatA2X(1)%factorIndexList(1,5)=4
    sparseMatA2X(1)%factorIndexList(1,6)=4
    sparseMatA2X(1)%factorIndexList(1,7)=3
    sparseMatA2X(1)%factorIndexList(1,8)=4
    sparseMatA2X(1)%factorIndexList(1,9)=4
    sparseMatA2X(1)%factorIndexList(2,1)=1
    sparseMatA2X(1)%factorIndexList(2,2)=2
    sparseMatA2X(1)%factorIndexList(2,3)=3
    sparseMatA2X(1)%factorIndexList(2,4)=4
    sparseMatA2X(1)%factorIndexList(2,5)=5
    sparseMatA2X(1)%factorIndexList(2,6)=6
    sparseMatA2X(1)%factorIndexList(2,7)=7
    sparseMatA2X(1)%factorIndexList(2,8)=8
    sparseMatA2X(1)%factorIndexList(2,9)=9
    ! - set up mapping between A2 -> X
    sparseMatA2X(2)%factorIndexList(1,1)=1
    sparseMatA2X(2)%factorIndexList(1,2)=2
    sparseMatA2X(2)%factorIndexList(1,3)=2
    sparseMatA2X(2)%factorIndexList(2,1)=10
    sparseMatA2X(2)%factorIndexList(2,2)=11
    sparseMatA2X(2)%factorIndexList(2,3)=12
!EOC
!BOE
! Now we are ready to create the XGrid from user supplied input.
!EOE
!BOC
    xgrid = ESMF_XGridCreate(sideA, sideB, area=area, centroid=centroid, &
        sparseMatA2X=sparseMatA2X, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

    ! Destroy the XGrid
    call ESMF_XGridDestroy(xgrid, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)

!EOC
    deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
    deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
    if(localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    print *, "XGrid create from user input example returned"

!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=localrc)
    if (localrc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!-------------------------------------------------------------------------

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_XGridEx.F90"
    else
        print *, "FAIL: ESMF_XGridEx.F90"
    end if
end program ESMF_XGridEx
