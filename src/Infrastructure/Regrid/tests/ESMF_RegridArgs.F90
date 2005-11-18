
! $Id: ESMF_RegridArgs.F90,v 1.3 2005/11/18 22:49:44 svasquez Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    module ESMF_RegridArgs
!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

    !--- USES:Framework module
    use ESMF_TestMod  ! test methods
    use ESMF_Mod      ! Framework module


    integer :: lrc,iFunction,iRegrid,ig
    integer ::  npets, localPet
    type(ESMF_VM) :: vm
    type(ESMF_RegridMethod) :: RegridChoice(2)

    integer, parameter :: n_grid_pairs=4
    type(ESMF_GridHorzStagger) :: SrcGridHorzChoice(n_grid_pairs), &
                                  DstGridHorzChoice(n_grid_pairs)
    type(ESMF_RelLoc) :: SrcRelLocChoice(n_grid_pairs), &
                         DstRelLocChoice(n_grid_pairs)

    !--- cumulative result: count failures; no failures equals "all pass"
    integer :: result=0
    integer :: regrid_rc    !single test error indicator

    integer :: iSrcDistr, iDstDistr, nXY(3,2)
    integer :: TwoOrOne

    integer, parameter :: nHalo=4
    integer, dimension(nHalo) :: SrcHaloChoice, DstHaloChoice

    !--- individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    contains

#undef  ESMF_METHOD
#define ESMF_METHOD "initTables"


    subroutine initTables

    integer :: i
   !--- Domain Decompositions used
    nXY(1,:)=(/ npets, 1 /)                       !1DX
    nXY(2,:)=(/ 1, npets /)                       !1DY
    nXY(3,:)=(/ npets/(1 + mod(npets+1,2)), (1 + mod(npets+1,2)) /)       !2D
    !nXY(3,:)=(/ npets/2,2 /)       !2D
    !TwoOrOne= 1 + mod(npets+1,2)
    !nXY(3,:)=(/ npets/(1 + mod(npets+1,2)), (1 + mod(npets+1,2)) /)       !2D

    regrid_rc=ESMF_SUCCESS

   !--- Regrid methods being tested
    RegridChoice(1)=ESMF_REGRID_METHOD_BILINEAR
    RegridChoice(2)=ESMF_REGRID_METHOD_CONSERV1

   !--- SOURCE Grid types and corresponding field relative_cell_location
    SrcGridHorzChoice(1) = ESMF_GRID_HORZ_STAGGER_A
    SrcRelLocChoice(1) = ESMF_CELL_CENTER

    SrcGridHorzChoice(2) = ESMF_GRID_HORZ_STAGGER_D_NE
    SrcRelLocChoice(2) = ESMF_CELL_EFACE

    SrcGridHorzChoice(3) = ESMF_GRID_HORZ_STAGGER_B_NE
    SrcRelLocChoice(3) = ESMF_CELL_NECORNER

    SrcGridHorzChoice(4) = ESMF_GRID_HORZ_STAGGER_C_NE
    SrcRelLocChoice(4) = ESMF_CELL_NFACE

   !--- DESTINATION Grid types and corresponding field relative_cell_location
    DstGridHorzChoice(1) = ESMF_GRID_HORZ_STAGGER_A
    DstRelLocChoice(1) = ESMF_CELL_CENTER

    DstGridHorzChoice(2) = ESMF_GRID_HORZ_STAGGER_D_NE
    DstRelLocChoice(2) = ESMF_CELL_EFACE

    DstGridHorzChoice(3) = ESMF_GRID_HORZ_STAGGER_B_NE
    DstRelLocChoice(3) = ESMF_CELL_NECORNER

    DstGridHorzChoice(4) = ESMF_GRID_HORZ_STAGGER_C_NE
    DstRelLocChoice(4) = ESMF_CELL_NFACE

   !---SOURCE  and DESTINATION  halo size 
    do i=1,4
      SrcHaloChoice(i) = i-1
      DstHaloChoice(i) = i-1
    end do

  !TODO: Print documentation for what 1)the 1-4 field test functions are
  !TODO:                              2)source and dest. grid choices are
  !TODO:                              3)formula is used for relative error calc

    end subroutine initTables
#undef  ESMF_METHOD
    end module 

