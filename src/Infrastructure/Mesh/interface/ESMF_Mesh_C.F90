!  $Id: ESMF_Mesh_C.F90,v 1.14 2012/10/10 16:53:46 jcjacob Exp $
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
!
!------------------------------------------------------------------------------
#define ESMF_FILENAME "ESMF_Mesh_C.F90"

! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_Mesh_C.F90,v 1.14 2012/10/10 16:53:46 jcjacob Exp $'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_meshcreatefromfile"
   subroutine f_esmf_meshcreatefromfile(meshp, filename, fileTypeFlag, &
                                        convert3D, c3dpresent, &
                                        convertToDual, ctodpresent, &
                                        addUserArea, auapresent, &
                                        meshname, mnpresent, &
                                        addMask, ampresent, &
                                        varname, vnpresent, &
                                        rc)
   use ESMF_UtilTypesMod
   use ESMF_LogErrMod
   use ESMF_MeshMod

   implicit none

   ! arguments
   type(ESMF_Pointer)             :: meshp
   character(len=*), intent(in)   :: filename
   type(ESMF_FileFormat_Flag)     :: fileTypeFlag
   logical                        :: convert3D
   integer                        :: c3dpresent, ctodpresent
   integer                        :: auapresent, mnpresent
   integer                        :: ampresent, vnpresent
   logical                        :: convertToDual
   logical                        :: addUserArea
   character(len=*)               :: meshname
   logical                        :: addMask
   character(len=*)               :: varname
   integer, intent(out)           :: rc

   type(ESMF_Mesh) :: mesh

   ! initialize return code; assume routine not implemented
   rc = ESMF_RC_NOT_IMPL

   ! handle the optional arguments
   if (filetypeflag == ESMF_FILEFORMAT_SCRIP) then
      if (c3dpresent == 0 .and. ctodpresent == 0 .and. &
          auapresent == 0) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, rc=rc)
      elseif (c3dpresent == 0 .and. ctodpresent == 0 .and. &
              auapresent == 1) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
  	                        addUserArea=addUserArea, rc=rc)      
      elseif (c3dpresent == 0 .and. ctodpresent == 1 .and. &
              auapresent == 0) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                convertToDual=convertToDual, rc=rc)      
      elseif (c3dpresent == 0 .and. ctodpresent == 1 .and. &
              auapresent == 1) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                convertToDual=convertToDual, &
                                addUserArea=addUserArea, rc=rc)      
      elseif (c3dpresent == 1 .and. ctodpresent == 0 .and. &
              auapresent == 0) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                convert3D=convert3D, rc=rc)      
      elseif (c3dpresent == 1 .and. ctodpresent == 0 .and. &
              auapresent == 1) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                convert3D=convert3D, &
                                addUserArea=addUserArea, rc=rc)      
      elseif (c3dpresent == 1 .and. ctodpresent == 1 .and. &
              auapresent == 0) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &      
                                convert3D=convert3D, &
				convertToDual=convertToDual, rc=rc)
      elseif (c3dpresent == 1 .and. ctodpresent == 1 .and. &
              auapresent == 1) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                convert3D=convert3D, &
				convertToDual=convertToDual, &
                                addUserArea=addUserArea, rc=rc)
      endif
   elseif (filetypeflag == ESMF_FILEFORMAT_ESMFMESH) then
      if (c3dpresent == 0 .and. auapresent == 0) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, rc=rc)
      elseif (c3dpresent == 0 .and. auapresent == 1) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
	                        addUserArea=addUserArea, rc=rc)
      elseif (c3dpresent == 1 .and. auapresent == 0) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                                convert3D=convert3D, rc=rc)
      elseif (c3dpresent == 1 .and. auapresent == 1) then
      	 mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
	                        addUserArea=addUserArea, convert3D=convert3D, &
                                rc=rc)
      endif
   elseif (filetypeflag == ESMF_FILEFORMAT_UGRID) then
       if (mnpresent == 1 .and. ampresent == 1 .and. vnpresent == 1) then
       	  mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
	       	                 meshname=meshname, addMask=addMask, &
				 varname=varname, rc=rc)
       elseif (mnpresent == 1) then
       	  mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
	       	                 meshname=meshname, rc=rc)
       else
          PRINT*, "ESMF_Mesh_C.F90(f_esmf_meshcreatefromfile): incorrect args for UGRID"
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
       	                        msg="- incorrect args for UGRID", & 
                                ESMF_CONTEXT, rcToReturn=rc) 
          return
       endif
   else
   endif


   if (c3dpresent == 0 .and. ctodpresent == 0 .and. auapresent == 0 .and. &
       mnpresent == 0 .and. ampresent == 0 .and. vnpresent == 0) then
       mesh = ESMF_MeshCreate(filename, fileTypeFlag, rc=rc)
   else if (c3dpresent == 1 .and. ctodpresent == 1 .and. &
            auapresent == 1 .and. mnpresent == 1 .and. &
            ampresent == 1 .and. vnpresent == 1) then
       mesh = ESMF_MeshCreate(filename, fileTypeFlag, &
                              convert3D=convert3D, &
                              convertToDual=convertToDual, &
                              addUserArea=AddUserArea, &
                              meshname=meshname, &
                              addMask=addMask, &
                              varname=varname, &
                              rc=rc)
   endif
   if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
   
   meshp=mesh%this

   rc = ESMF_SUCCESS
  
   end subroutine f_esmf_meshcreatefromfile

   subroutine f_esmf_getmeshdistgrid(dgrid, count, indices, rc)
     use ESMF_UtilTypesMod    ! ESMF base class
     use ESMF_BaseMod    ! ESMF base class
     use ESMF_DistGridMod
     
     implicit none
     
     type(ESMF_DistGrid), intent(inout) :: dgrid
     integer, intent(in)               :: count
     integer, intent(inout)            :: indices(count)
     integer, intent(out)              :: rc              

     integer, allocatable :: indicesLocal(:)

   ! initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL

     allocate(indicesLocal(count))


     if (count > 0) then
       indicesLocal(1:count) = indices(1:count)
     endif

     dgrid = ESMF_DistGridCreate(indicesLocal, rc=rc)

     deallocate(indicesLocal)

   end subroutine f_esmf_getmeshdistgrid

