!  $Id: ESMF_Attribute_C.F90,v 1.1 2012/08/24 00:45:27 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable
!   arrays, or ...
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Attribute_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_Attribute_C.F90,v 1.1 2012/08/24 00:45:27 rokuingh Exp $'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridattgetinfoint"
  subroutine f_esmf_gridattgetinfoint(gridp, name, value, &
                                      rc)
                                      !il_present, inputList, len1, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_AttributeInternalsMod

  implicit none


  !arguments
    type(ESMF_Pointer) :: gridp
    character(len=*)   :: name
    integer            :: value
    !integer            :: il_present
    !integer            :: len1
    !character(len=*)   :: inputList(1:len1)
    integer            :: rc

    ! Local vars
    integer :: localrc                   ! local return code
    type(ESMF_Grid) :: grid
    type(ESMF_GridStatus_Flag) :: gridstatus

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    print *, "f_esmf_gridattgetinfoint"

    ! initialize the grid
    grid%this = gridp
    ESMF_INIT_SET_CREATED(grid)

    call ESMF_GridGet(grid, status=gridstatus)
    print *, "Attribute_C.F90: GRIDSTATUS = ", gridstatus
    
    ! Call into the Attribute public Fortran layer
#if 0
    if (il_present == 1) then
      call ESMF_AttributeGetInfo(grid, name, value, inputList=inputList, rc=localrc)
    else if (il_present == 0) then
      call ESMF_AttributeGetInfo(grid, name, value, rc=localrc)
    endif
#endif
    call ESMF_AttributeGetInfo(grid, name, value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine f_esmf_gridattgetinfoint
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridattgetinfochar"
  subroutine f_esmf_gridattgetinfochar(gridp, name, value, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_AttributeInternalsMod

  implicit none


  !arguments
    type(ESMF_Pointer) :: gridp
    character(len=*)   :: name
    character(len=*)   :: value
    integer            :: rc

    ! Local vars
    integer :: localrc                   ! local return code
    type(ESMF_Grid) :: grid
    type(ESMF_GridStatus_Flag) :: gridstatus

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    ! initialize the grid
    grid%this = gridp
    ESMF_INIT_SET_CREATED(grid)

    call ESMF_GridGet(grid, status=gridstatus)
    print *, "Attribute_C.F90: GRIDSTATUS = ", gridstatus

    ! Call into the Attribute public Fortran layer
    call ESMF_AttributeGetInfo(grid, name, value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine f_esmf_gridattgetinfochar
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridattgetinfointlist"
  subroutine f_esmf_gridattgetinfointlist(gridp, name, valueList, len1, &
                                      il_present, inputList, len2, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_AttributeInternalsMod

  implicit none


  !arguments
    type(ESMF_Pointer) :: gridp
    character(len=*)   :: name
    integer            :: len1, len2
    integer            :: valueList(1:len1)
    integer            :: il_present
    character(len=*)   :: inputList(1:len2)
    integer            :: rc

    ! Local vars
    integer :: localrc                   ! local return code
    type(ESMF_Grid) :: grid

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    ! initialize the grid
    grid%this = gridp
    ESMF_INIT_SET_CREATED(grid)

    ! Call into the Attribute public Fortran layer
    if (il_present == 1) then
      call ESMF_AttributeGetInfo(grid, name, valueList, inputList=inputList, rc=localrc)
    else if (il_present == 0) then
      call ESMF_AttributeGetInfo(grid, name, valueList, rc=localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine f_esmf_gridattgetinfointlist
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridattgetinfor8list"
  subroutine f_esmf_gridattgetinfor8list(gridp, name, valueList, len1, &
                                         il_present, inputList, len2, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_AttributeInternalsMod

  implicit none


  !arguments
    type(ESMF_Pointer)     :: gridp
    character(len=*)       :: name
    integer                :: len1, len2
    real(ESMF_KIND_R8)     :: valueList(1:len1)
    integer                :: il_present
    character(len=*)       :: inputList(1:len2)
    integer                :: rc

    ! Local vars
    integer :: localrc                   ! local return code
    type(ESMF_Grid) :: grid

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    ! initialize the grid
    grid%this = gridp
    ESMF_INIT_SET_CREATED(grid)
print *, "f_esmf_gridattgetinfor8list - inputList = ", inputList
    ! Call into the Attribute public Fortran layer
    if (il_present == 1) then
      call ESMF_AttributeGetInfo(grid, name, valueList, inputList=inputList, rc=localrc)
    else if (il_present == 0) then
      call ESMF_AttributeGetInfo(grid, name, valueList, rc=localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine f_esmf_gridattgetinfor8list
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridattgetinfologicallist"
  subroutine f_esmf_gridattgetinfologicallist(gridp, name, valueList, len1, &
                                              il_present, inputList, len2, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_AttributeInternalsMod

  implicit none


  !arguments
    type(ESMF_Pointer) :: gridp
    character(len=*)   :: name
    integer            :: len1, len2
    logical            :: valueList(1:len1)
    integer            :: il_present
    character(len=*)   :: inputList(1:len2)
    integer            :: rc

    ! Local vars
    integer :: localrc                   ! local return code
    type(ESMF_Grid) :: grid

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
    ESMF_INIT_SET_CREATED(grid)

    ! Call into the Attribute public Fortran layer
    if (il_present == 1) then
      call ESMF_AttributeGetInfo(grid, name, valueList, inputList=inputList, rc=localrc)
    else if (il_present == 0) then
      call ESMF_AttributeGetInfo(grid, name, valueList, rc=localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine f_esmf_gridattgetinfologicallist
!------------------------------------------------------------------------------

