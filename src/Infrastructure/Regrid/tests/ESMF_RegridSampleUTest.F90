! $Id: ESMF_RegridSampleUTest.F90,v 1.10 2005/10/20 22:09:55 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
    program ESMF_RegridSampleUTest
    !program ESMF_FieldRegridUTest

!------------------------------------------------------------------------------

!================================================================================


#include <ESMF_Macros.inc>

    ! USES:Framework module
    use ESMF_TestMod  ! test methods
    use ESMF_Mod      ! Framework module

    use ESMF_RegridArgs
    use ESMF_RegridSubroutines



    character(len=100           ) :: longString    ! [128]
   !character(len=ESMF_MAXSTR-13) :: longString    ! [128]

    call ESMF_TestStart(ESMF_SRCLINE, rc=lrc)
    call ESMF_VMGetGlobal(vm, rc=lrc)
    !--- Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=localPet,  rc=lrc)

  !--LongString specifies all the adjustable parameters of the regrid test.
  !  Possible options are as follows (Ref: Options_Table subroutine):
  !   FUNCTION: C, D    (Note, A and B are not appropriate for a sphere)
  !   REGSCHEME: BILINEAR, 1CONSERV
  !   SRCGRID  : A, D_NE, C_NE
  !   DSTGRID  : A, D_NE, C_NE
  !   SRCDELAYOUT: 1DX, 1DY, 2D
  !   DSTDELAYOUT: 1DX, 1DY, 2D

    longString='FUNCTION:C:REGSCHEME:1CONSERV' &
               //':SRCGRID:A:DSTGRID:A:SRCDELAYOUT:1DY:DSTDELAYOUT:1DY' &
               //':DOMAIN:WHOLEGLOBE:SRCHALO:3:DSTHALO:0'

    call setupRegridUTest(longString,ier)

#ifdef ESMF_EXHAUSTIVE
   !Test for "success" of regridding
   !--------------------------------
   !EX_UTest
    write(failMsg, *) "Error in regrid"
    write(name, *) "Regrid test: "//longString
    call ESMF_Test((regrid_rc.eq.ESMF_SUCCESS),name, failMsg, result, &
                    ESMF_SRCLINE)
#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

!    end program ESMF_FieldRegridUTest
    end program ESMF_RegridSampleUTest

!===============================================================================
