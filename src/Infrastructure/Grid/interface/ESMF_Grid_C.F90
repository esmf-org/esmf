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
  subroutine f_esmf_gridcreatefromfile(gridp, filename, fileTypeFlag, rank, regDecomp, &
      decompflag, dfpresent, isSphere, ispresent, addCornerStagger, acspresent, &
      addUserArea, auapresent, addMask, ampresent, varname, vnpresent, &
      coordNames, cnpresent, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_DistGridMod

    implicit none

    ! arguments
    type(ESMF_Pointer)                     :: gridp
    character(len=*), intent(in)           :: filename
    type(ESMF_FileFormat_Flag), intent(in) :: fileTypeFlag
    integer, intent(in)                    :: rank
    integer, intent(in),dimension(rank)    :: regDecomp
    type(ESMF_Decomp_Flag),dimension(rank) :: decompflag
    integer                                :: dfpresent
    logical                                :: isSphere
    integer                                :: ispresent
    logical                                :: addCornerStagger
    integer                                :: acspresent
    logical                                :: addUserArea
    integer                                :: auapresent
    logical                                :: addMask
    integer                                :: ampresent
    character(len=*)                       :: varname
    integer                                :: vnpresent
    character(len=*)                       :: coordNames(:)
    integer                                :: cnpresent
    integer, intent(out)                   :: rc
  
    type(ESMF_Grid)                     :: grid
    type(ESMF_Decomp_Flag),dimension(2) :: decompflag_loc
    logical                             :: isSphere_loc
    logical                             :: addCornerStagger_loc
    logical                             :: addUserArea_loc
    logical                             :: addMask_loc

    integer :: sz
    
    !print '("Start ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile(",A,", ",I1,", [",I1,",",I1,"])")', filename, fileTypeFlag, regDecomp(1), regDecomp(2)
    !call flush()

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! Set reasonable defaults for optional arguments
    if (dfpresent == 0) then
      decompflag_loc = ESMF_DECOMP_BALANCED
    else
      decompflag_loc = decompflag
    endif
    if (ispresent == 0) then
       isSphere_loc = .true.
    else
       isSphere_loc = isSphere
    endif
    if (acspresent == 0) then
       addCornerStagger_loc = .false.
    else
       addCornerStagger_loc = addCornerStagger
    endif
    if (auapresent == 0) then 
       addUserArea_loc = .false.
    else
       addUserArea_loc = addUserArea
    endif
    if (ampresent == 0) then
       addMask_loc = .false.
    else
       addMask_loc = addMask
    endif
    
    if (vnpresent == 0 .and. cnpresent == 0) then
       grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, &
                              decompflag=decompflag_loc, &
                              isSphere=isSphere_loc, &
                              addCornerStagger=addCornerStagger_loc, &
                              addUserArea=addUserArea_loc, rc=rc)
       if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (vnpresent == 0 .and. cnpresent == 1) then
       grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, &
                              decompflag=decompflag_loc, &
                              isSphere=isSphere_loc, &
                              addCornerStagger=addCornerStagger_loc, &
                              addUserArea=addUserArea_loc, &
                              coordNames=coordNames, rc=rc)
       if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (vnpresent == 1 .and. cnpresent == 0) then
       grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, &
                              decompflag=decompflag_loc, &
                              isSphere=isSphere_loc, &
                              addCornerStagger=addCornerStagger_loc, &
                              addUserArea=addUserArea_loc, &
                              addMask=addMask_loc, varname=varname,rc=rc)
       if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (vnpresent == 1 .and. cnpresent == 1) then
       grid = ESMF_GridCreate(filename, fileTypeFlag, regDecomp, &
                              decompflag=decompflag_loc, &
                              isSphere=isSphere_loc, &
                              addCornerStagger=addCornerStagger_loc, &
                              addUserArea=addUserArea_loc, &
                              addMask=addMask_loc, varname=varname, &
                              coordNames=coordNames, rc=rc)
       if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    endif
 
    gridp = grid%this
  
    rc = ESMF_SUCCESS

    !print '("End ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile()")'
  
  end subroutine f_esmf_gridcreatefromfile
