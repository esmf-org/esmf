!  $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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
!
!------------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_Mesh_C.F90"

! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_meshcreatefromfile"
   subroutine f_esmf_meshcreatefromfile(meshp, filename, fileTypeFlag, &
                                        convertToDual, &
                                        addUserArea, &
                                        meshname, mnpresent, &
                                        maskFlag, mfpresent, &
                                        varname, vnpresent, &
                                        parametricDim, &
                                        spatialDim, &
                                        coordSys, &
                                        rc)
   use ESMF_UtilTypesMod
   use ESMF_LogErrMod
   use ESMF_MeshMod

   implicit none

   ! arguments
   type(ESMF_Pointer)             :: meshp
   character(len=*), intent(in)   :: filename
   type(ESMF_FileFormat_Flag)     :: fileTypeFlag
   integer                        :: mnpresent
   integer                        :: mfpresent, vnpresent
   type(ESMF_Logical)             :: convertToDual
   type(ESMF_Logical)             :: addUserArea
   character(len=*)               :: meshname
   type(ESMF_MeshLoc)             :: maskFlag
   character(len=*)               :: varname
   integer, intent(out)           :: parametricDim
   integer, intent(out)           :: spatialDim
   type(ESMF_CoordSys_Flag), intent(out) :: coordSys
   integer, intent(out)           :: rc

   type(ESMF_Mesh) :: mesh
   logical :: convertToDual_loc
   logical :: addUserArea_loc

   ! initialize return code; assume routine not implemented
   rc = ESMF_RC_NOT_IMPL

   convertToDual_loc = convertToDual
   addUserArea_loc = addUserArea

   if (filetypeflag == ESMF_FILEFORMAT_SCRIP) then
      mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                             convertToDual=convertToDual_loc, &
                             addUserArea=addUserArea_loc, rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
   elseif (filetypeflag == ESMF_FILEFORMAT_ESMFMESH) then
      mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                             addUserArea=addUserArea_loc, rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
   elseif (filetypeflag == ESMF_FILEFORMAT_UGRID) then
       ! handle the optional arguments
       if (mnpresent == 1 .and. mfpresent == 1 .and. vnpresent == 1) then
                  mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                         maskFlag=maskFlag, &
                                         varname=varname, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       elseif (mnpresent == 1) then
                  mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                         rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       elseif (mnpresent == 1 .and. mfpresent == 1 .and. vnpresent == 1) then
                  mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                         maskFlag=maskFlag, &
                                 varname=varname, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       elseif (mnpresent == 1) then
                  mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                         rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       else
          PRINT*, "ESMF_Mesh_C.F90(f_esmf_meshcreatefromfile): incorrect args for UGRID"
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                                        msg="- incorrect args for UGRID", &
                                ESMF_CONTEXT, rcToReturn=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       endif
   else
   endif

    ! Get internal pointer (to native mesh ... )
   call ESMF_MeshGetIntPtr(mesh, meshp, rc=rc)
   if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! Get other information
   call ESMF_MeshGet(mesh, &
                     parametricDim=parametricDim, &
                     spatialDim=spatialDim, &
                     coordSys=coordSys, rc=rc)
   if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! Return success
   rc = ESMF_SUCCESS

   end subroutine f_esmf_meshcreatefromfile

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_getmeshdistgrid"
    subroutine f_esmf_getmeshdistgrid(distgridPtr, count, indices, rc)
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_DistGridMod

      implicit none

      type(ESMF_Pointer)     :: distgridPtr
      integer, intent(in)    :: count
      integer, intent(inout) :: indices(count)
      integer, intent(out)   :: rc

      integer :: localrc
      integer, allocatable :: indicesLocal(:)
      type(ESMF_DistGrid)    :: distgrid

      ! initialize return code; assume routine not implemented
      rc = ESMF_RC_NOT_IMPL

      allocate(indicesLocal(count))

      if (count > 0) then
        indicesLocal(1:count) = indices(1:count)
      endif

      ! Create the DistGrid
      distgrid = ESMF_DistGridCreate(indicesLocal, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then
        deallocate(indicesLocal)  ! prevent memory leak when bailing
        return
      endif

      ! Get the pointer to the actual internal DistGrid object (vs. the F90 wrapper object)
      call ESMF_DistGridGetThis(distgrid, distgridPtr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then
        deallocate(indicesLocal)  ! prevent memory leak when bailing
        return
      endif

      ! clean up
      deallocate(indicesLocal)

      ! Return success
      rc = ESMF_SUCCESS

    end subroutine f_esmf_getmeshdistgrid

