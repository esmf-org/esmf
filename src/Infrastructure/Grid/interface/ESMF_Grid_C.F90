!  $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2013, University Corporation for Atmospheric Research, 
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
#define ESMF_FILENAME "ESMF_Grid_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcreatenoperidim"
  subroutine f_esmf_gridcreatenoperidim(gridp, maxIndex, len1, &
                                        coordSys, cspresent, &
                                        coordTypeKind, ctkpresent, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Pointer)             :: gridp
    integer, intent(in)            :: len1
    integer                        :: maxIndex(1:len1)
    integer                        :: cspresent, ctkpresent
    type(ESMF_CoordSys_Flag)       :: coordSys
    type(ESMF_TypeKind_Flag)       :: coordTypeKind
    integer, intent(out)           :: rc              

    type(ESMF_Grid) :: grid

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! handle the optional arguments
    if (cspresent == 0 .and. ctkpresent == 0) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
    elseif (cspresent == 0 .and. ctkpresent == 1) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      coordTypeKind=coordTypeKind, &
                                      indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
    elseif (cspresent == 1 .and. ctkpresent == 0) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      coordSys=coordSys, &
                                      indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
    elseif (cspresent == 1 .and. ctkpresent == 1) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      coordSys=coordSys, &
                                      coordTypeKind=coordTypeKind, &
                                      indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    gridp=grid%this

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_gridcreatenoperidim

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcreate1peridim"
  subroutine f_esmf_gridcreate1peridim(gridp, maxIndex, len1, &
                                        coordSys, cspresent, &
                                        coordTypeKind, ctkpresent, &
                                        poleKind, pkpresent, len2, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Pointer)             :: gridp
    integer, intent(in)            :: len1, len2
    integer                        :: maxIndex(1:len1)
    integer                        :: cspresent, ctkpresent, pkpresent
    type(ESMF_CoordSys_Flag)       :: coordSys
    type(ESMF_TypeKind_Flag)       :: coordTypeKind
    type(ESMF_PoleKind_Flag)       :: poleKind(1:len2)
    integer, intent(out)           :: rc              
  
    type(ESMF_Grid) :: grid
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! handle the optional arguments
    if (pkpresent == 0) then
      if (cspresent == 0 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      elseif (cspresent == 0 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        coordSys=coordSys, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        coordSys=coordSys, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      endif
    elseif (pkpresent == 1) then
      if (cspresent == 0 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        polekindflag=poleKind, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      elseif (cspresent == 0 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        polekindflag=poleKind, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        polekindflag=poleKind, &
                                        coordSys=coordSys, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        polekindflag=poleKind, &
                                        coordSys=coordSys, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=ESMF_INDEX_GLOBAL, rc=rc)    
      endif
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    gridp = grid%this
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_gridcreate1peridim

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcreatefromfile"
  subroutine f_esmf_gridcreatefromfile(gridp, filename, fileTypeFlag, &
  	     			       regDecomp, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Pointer)                     :: gridp
    character(len=*), intent(in)           :: filename
    type(ESMF_FileFormat_Flag), intent(in) :: fileTypeFlag
    integer, dimension(2), intent(in)      :: regDecomp
    integer, intent(out)                   :: rc              
  
    type(ESMF_Grid) :: grid

    ! print '("Start ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile(",A,", ",I1,", [",I1,",",I1,"])")', filename, fileTypeFlag, regDecomp(1), regDecomp(2)

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    !grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, addCornerStagger=.true., rc=rc)

    !grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, isSphere=.false., coordNames=(/ 'lon','lat' /),rc=rc)

    !grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, isSphere=.false.,rc=rc)

    grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, rc=rc)

    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    gridp = grid%this
  
    rc = ESMF_SUCCESS

    ! print '("End ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile()")'
  
  end subroutine f_esmf_gridcreatefromfile
