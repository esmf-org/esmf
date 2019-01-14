!  $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
                                        periodicDim, pdpresent, &
                                        poleDim, pldpresent, &
                                        coordSys, cspresent, &
                                        coordTypeKind, ctkpresent, &
                                        poleKind, pkpresent, len2, &
                                        indexflag, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Pointer)             :: gridp
    integer, intent(in)            :: len1, len2
    integer                        :: maxIndex(1:len1)
    integer                        :: periodicDim
    integer                        :: poleDim
    integer                        :: pdpresent, pldpresent, cspresent
    integer                        :: ctkpresent, pkpresent
    type(ESMF_CoordSys_Flag)       :: coordSys
    type(ESMF_TypeKind_Flag)       :: coordTypeKind
    type(ESMF_PoleKind_Flag)       :: poleKind(1:len2)
    type(ESMF_Index_Flag)          :: indexflag
    integer, intent(out)           :: rc              
  
    type(ESMF_Grid) :: grid
    integer         :: poleDim_Loc

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! set default for poleDim
    if (pldpresent == 0) then
      poleDim_Loc = 2
    else
      poleDim_Loc = poleDim
    endif

    ! handle the optional arguments
    if ((pkpresent == 0) .and. (pdpresent == 0)) then
      if (cspresent == 0 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 0 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        coordSys=coordSys, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        coordSys=coordSys, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      endif
    elseif ((pkpresent == 1) .and. (pdpresent == 0)) then
      if (cspresent == 0 .and. ctkpresent == 0) then 
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 0 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        coordSys=coordSys, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        coordSys=coordSys, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      endif
    elseif ((pkpresent == 0) .and. (pdpresent == 1)) then
      if (cspresent == 0 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 0 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        coordSys=coordSys, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        coordSys=coordSys, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      endif
    elseif ((pkpresent == 1) .and. (pdpresent == 1)) then
      if (cspresent == 0 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 0 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 0) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        coordSys=coordSys, &
                                        indexflag=indexflag, rc=rc)    
      elseif (cspresent == 1 .and. ctkpresent == 1) then
        grid = ESMF_GridCreate1PeriDim(maxIndex=maxIndex, &
                                        periodicDim=periodicDim, &
                                        poleDim=poleDim_Loc, &
                                        polekindflag=poleKind, &
                                        coordSys=coordSys, &
                                        coordTypeKind=coordTypeKind, &
                                        indexflag=indexflag, rc=rc)    
      endif
    endif
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
      regDecomp, rdpresent, decompflag, dfpresent, &
      isSphere, ispresent, addCornerStagger, acspresent, &
      addUserArea, auapresent, indexflag, addMask, ampresent, varname, vnpresent, &
      coordNames, cnpresent, rc)

    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_DistGridMod
    use ESMF_VMMod

    implicit none

    ! arguments
    type(ESMF_Pointer)                     :: gridp
    character(len=*), intent(in)           :: filename
    type(ESMF_FileFormat_Flag), intent(in) :: fileTypeFlag
    integer                                :: regDecomp(2)
    integer                                :: rdpresent
    type(ESMF_Decomp_Flag)                 :: decompflag(2)
    integer                                :: dfpresent
    logical                                :: isSphere
    integer                                :: ispresent
    logical                                :: addCornerStagger
    integer                                :: acspresent
    logical                                :: addUserArea
    integer                                :: auapresent
    type(ESMF_Index_Flag)                  :: indexflag
    logical                                :: addMask
    integer                                :: ampresent
    character(len=*)                       :: varname
    integer                                :: vnpresent
    character(len=*)                       :: coordNames(2)
    integer                                :: cnpresent
    integer, intent(out)                   :: rc
  
    type(ESMF_Grid)                     :: grid
    logical                             :: isSphere_loc
    logical                             :: addCornerStagger_loc
    logical                             :: addUserArea_loc
    logical                             :: addMask_loc

    !print '("Start ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile(",A,", ",I1,"])")', filename, fileTypeFlag

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! Set reasonable defaults for optional arguments
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
    
    if (rdpresent == 0) then
        ! regDecomp not present
        if (dfpresent == 0) then
            ! decompflag not present
            if (vnpresent == 0 .and. cnpresent == 0) then
               !print *,'v=0,c=0'
           grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      isSphere=isSphere_loc, &
                                  addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 0 .and. cnpresent == 1) then
               !print *,'v=0,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      coordNames=coordNames, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 0) then
               !print *,'v=1,c=0'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      addMask=addMask_loc, varname=varname, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 1) then
               !print *,'v=1,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      addMask=addMask_loc, varname=varname, &
                                      coordNames=coordNames, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            endif
        else
            ! decompflag present
            if (vnpresent == 0 .and. cnpresent == 0) then
               !print *,'v=0,c=0'
           grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                    decompflag=decompflag, &
                    isSphere=isSphere_loc, &
                              addCornerStagger=addCornerStagger_loc, &
                                  addUserArea=addUserArea_loc, &
                                  indexflag=indexflag, rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 0 .and. cnpresent == 1) then
               !print *,'v=0,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                        decompflag=decompflag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, &
                                      coordNames=coordNames, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 0) then
               !print *,'v=1,c=0'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                        decompflag=decompflag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, &
                                      addMask=addMask_loc, varname=varname, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 1) then
               !print *,'v=1,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      decompflag=decompflag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, &
                                      addMask=addMask_loc, varname=varname, &
                                      coordNames=coordNames, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            endif
        endif
    else
        ! regDecomp present
        if (dfpresent == 0) then
            ! decompflag not present
            if (vnpresent == 0 .and. cnpresent == 0) then
               !print *,'v=0,c=0'
           grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                      isSphere=isSphere_loc, &
                                  addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 0 .and. cnpresent == 1) then
               !print *,'v=0,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      coordNames=coordNames, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 0) then
               !print *,'v=1,c=0'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      addMask=addMask_loc, varname=varname, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 1) then
               !print *,'v=1,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      addMask=addMask_loc, varname=varname, &
                                      coordNames=coordNames, &
                                      indexflag=indexflag, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            endif
        else
            ! decompflag present
            if (vnpresent == 0 .and. cnpresent == 0) then
               !print *,'v=0,c=0'
           grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                  regDecomp=regDecomp, &
                                    decompflag=decompflag, &
                      isSphere=isSphere_loc, &
                              addCornerStagger=addCornerStagger_loc, &
                                  addUserArea=addUserArea_loc, &
                                  indexflag=indexflag, rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 0 .and. cnpresent == 1) then
               !print *,'v=0,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                        decompflag=decompflag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, &
                                      coordNames=coordNames, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 0) then
               !print *,'v=1,c=0'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                        decompflag=decompflag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, &
                                      addMask=addMask_loc, varname=varname, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            elseif (vnpresent == 1 .and. cnpresent == 1) then
               !print *,'v=1,c=1'
               grid = ESMF_GridCreate(filename, fileTypeFlag, &
                                      regDecomp=regDecomp, &
                                      decompflag=decompflag, &
                                      isSphere=isSphere_loc, &
                                      addCornerStagger=addCornerStagger_loc, &
                                      addUserArea=addUserArea_loc, &
                                      indexflag=indexflag, &
                                      addMask=addMask_loc, varname=varname, &
                                      coordNames=coordNames, rc=rc)
               if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            endif
        endif
    endif

    gridp = grid%this
  
    rc = ESMF_SUCCESS

    !print '("End ESMF_Grid_C.F90 : f_esmf_gridcreatefromfile()")'
  
  end subroutine f_esmf_gridcreatefromfile
