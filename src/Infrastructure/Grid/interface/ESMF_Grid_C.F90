!  $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
                                        coordTypeKind, ctkpresent, &
                                        indexflag, rc)

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
    type(ESMF_Index_Flag)          :: indexflag
    integer, intent(out)           :: rc              

    type(ESMF_Grid) :: grid

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! handle the optional arguments
    if (cspresent == 0 .and. ctkpresent == 0) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      indexflag=indexflag, rc=rc)    
    elseif (cspresent == 0 .and. ctkpresent == 1) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      coordTypeKind=coordTypeKind, &
                                      indexflag=indexflag, rc=rc)    
    elseif (cspresent == 1 .and. ctkpresent == 0) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      coordSys=coordSys, &
                                      indexflag=indexflag, rc=rc)    
    elseif (cspresent == 1 .and. ctkpresent == 1) then
      grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, &
                                      coordSys=coordSys, &
                                      coordTypeKind=coordTypeKind, &
                                      indexflag=indexflag, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    gridp=grid%this

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_gridcreatenoperidim

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcreate1peridim"
  subroutine f_esmf_gridcreate1peridim(gridp, maxIndex, len1, &
                                       polekindflag, len2, &
                                       periodicDim, &
                                       poleDim, &
                                       coordSys, &
                                       coordTypeKind, &
                                       indexflag, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Pointer)                  :: gridp
    integer, intent(in)                 :: len1, len2
    integer                             :: maxIndex(1:len1)
    type(ESMF_PoleKind_Flag), optional  :: polekindflag(1:len2)
    integer, optional                   :: periodicDim
    integer, optional                   :: poleDim
    type(ESMF_CoordSys_Flag), optional  :: coordSys
    type(ESMF_TypeKind_Flag), optional  :: coordTypeKind
    type(ESMF_Index_Flag), optional     :: indexflag
    integer, intent(out), optional      :: rc              
  
    type(ESMF_Grid) :: grid
    integer         :: poleDim_Loc

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! set default for poleDim
    if (present(poleDim)) then
      poleDim_Loc = poleDim
    else
      poleDim_Loc = 2
    endif

    grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                   polekindflag=polekindflag, &
                                   periodicDim=periodicDim, &
                                   poleDim=poleDim_Loc, &
                                   coordSys=coordSys, &
                                   coordTypeKind=coordTypeKind, &
                                   indexflag=indexflag, &
                                   rc=rc)    

    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    gridp = grid%this
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_gridcreate1peridim
    
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcreatecubedsphere"
  subroutine f_esmf_gridcreatecubedsphere(gridp, tilesize, &
                                          regDecompPTile, len11, len12, rdpresent, &
                                          decompFlagPTile, len21, len22, dfpresent, &
                                          deLabelList, len3, llpresent, &
                                          !delayout, &
                                          staggerLocList, len4, &
                                          name, &
                                          rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_DistGridMod
    use ESMF_StaggerLocMod

    implicit none

    ! arguments
    type(ESMF_Pointer)             :: gridp
    integer, intent(in)            :: len11, len12, len21, len22, len3, len4
    integer                        :: tilesize
    integer                        :: regDecompPTile(len11, len12)
    integer                        :: rdpresent, dfpresent, llpresent
    integer                        :: decompFlagPTile(len21, len22)
    integer                        :: deLabelList(len3)
    !type(ESMF_DELayout)            :: delayout
    integer                        :: staggerLocList(len4)
    character(len=*)               :: name
    integer, intent(out)           :: rc              
  
    type(ESMF_Grid) :: grid
    type(ESMF_Decomp_Flag) :: decompFlagPTile_local(len21, len22)
    type(ESMF_StaggerLoc) :: staggerLocList_local(len4)
    integer :: i, j

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! glue together decompFlagPTile
    if (dfpresent == 1) then
      do i = 1, len21
        do j = 1, len22
          if (decompFlagPTile(i, j) == 1) then
            decompFlagPTile_local(i, j) = ESMF_DECOMP_BALANCED
          else if (decompFlagPTile(i, j) == 2) then
            decompFlagPTile_local(i, j) = ESMF_DECOMP_RESTFIRST
          else if (decompFlagPTile(i, j) == 3) then
            decompFlagPTile_local(i, j) = ESMF_DECOMP_RESTLAST
          else if (decompFlagPTile(i, j) == 4) then
            decompFlagPTile_local(i, j) = ESMF_DECOMP_CYCLIC
          endif
        enddo
      enddo
    endif

    do i = 1, len4
        if (staggerLocList(i) == 0) then
            staggerLocList_local(i) = ESMF_STAGGERLOC_CENTER
        else if (staggerLocList(i) == 3) then
            staggerLocList_local(i) = ESMF_STAGGERLOC_CORNER
        endif
    enddo


    grid = ESMF_GridCreateCubedSphere(tilesize, &
                                      regDecompPTile=regDecompPTile, &
                                      !decompFlagPTile=decompFlagPTile_local, &
                                      !deLabelList=deLabelList, &
                                      !delayout=delayout, &
                                      staggerLocList=staggerLocList_local, &
                                      name=name, &
                                      rc=rc)

    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    gridp = grid%this

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_gridcreatecubedsphere

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcreatefromfile"
  subroutine f_esmf_gridcreatefromfile(gridp, filename, fileTypeFlag, &
      regDecomp, decompflag, isSphere, polekindflag, len1, addCornerStagger, &
      addUserArea, indexflag, addMask, varname, coordNames, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_DistGridMod
    use ESMF_VMMod

    implicit none

    ! arguments
    type(ESMF_Pointer)                     :: gridp
    character(len=*), intent(in)           :: filename
    type(ESMF_FileFormat_Flag), optional   :: fileTypeFlag
    integer, optional                      :: regDecomp(2)
    type(ESMF_Decomp_Flag), optional       :: decompflag(2)
    logical, optional                      :: isSphere
    integer                                :: len1
    type(ESMF_PoleKind_Flag), optional     :: polekindflag(1:len1)
    logical, optional                      :: addCornerStagger
    logical, optional                      :: addUserArea
    type(ESMF_Index_Flag), optional        :: indexflag
    logical, optional                      :: addMask
    character(len=*), optional             :: varname
    character(len=*), optional             :: coordNames(2)
    integer, intent(out), optional         :: rc
  
    type(ESMF_Grid)                     :: grid
    logical                             :: isSphere_loc
    logical                             :: addCornerStagger_loc
    logical                             :: addUserArea_loc
    logical                             :: addMask_loc

    !print '("Start ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile(",A,", ",I1,"])")', filename, fileTypeFlag

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! Set reasonable defaults for optional arguments
    isSphere_loc = .true.
    if(present(isSphere)) then
       isSphere_loc = isSphere
    endif
    
    addCornerStagger_loc = .false.
    if(present(addCornerStagger)) then
       addCornerStagger_loc = addCornerStagger
    endif
    
    addUserArea_loc = .false.
    if(present(addUserArea)) then
       addUserArea_loc = addUserArea
    endif
    
    addMask_loc = .false.
    if(present(addMask)) then
       addMask_loc = addMask
    endif
    
    grid = ESMF_GridCreate(filename, fileTypeFlag, &
                           regDecomp=regDecomp, &
                           decompflag=decompflag, &
                           isSphere=isSphere_loc, &
                           polekindflag=polekindflag, &
                           addCornerStagger=addCornerStagger_loc, &
                           addUserArea=addUserArea_loc, &
                           indexflag=indexflag, &
                           addMask=addMask_loc, &
                           varname=varname, &
                           coordNames=coordNames, &
                           rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return

    gridp = grid%this
  
    rc = ESMF_SUCCESS

    !print '("End ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile()")'
  
  end subroutine f_esmf_gridcreatefromfile
