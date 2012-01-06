!  $Id: ESMF_Field_C.F90,v 1.32 2012/01/06 20:16:38 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_Field_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_Field_C.F90,v 1.32 2012/01/06 20:16:38 svasquez Exp $'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatemeshas"
  subroutine f_esmf_fieldcreatemeshas(field, mesh_pointer, arrayspec, &
    gridToFieldMap, len1, ungriddedLBound, len2, ungriddedUBound, len3, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: mesh_pointer
    type(ESMF_ArraySpec)           :: arrayspec
    integer, intent(in)            :: len1, len2, len3
    integer                        :: gridToFieldMap(1:len1), ungriddedLBound(1:len2), ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreate(mesh_pointer)
  
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        name=name, &
        rc=rc)    
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatemeshas

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatemeshas"
  subroutine f_esmf_fieldcreatemeshtk(field, mesh_pointer, typekind, &
    gridToFieldMap, len1, ungriddedLBound, len2, ungriddedUBound, len3, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: mesh_pointer
    type(ESMF_TypeKind_Flag)       :: typekind
    integer, intent(in)            :: len1, len2, len3
    integer                        :: gridToFieldMap(1:len1), ungriddedLBound(1:len2), ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreate(mesh_pointer)

    field = ESMF_FieldCreate(mesh, typekind=typekind, gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        name=name, &
        rc=rc)    
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatemeshtk

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldprint"
  subroutine f_esmf_fieldprint(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_FieldMod
    use ESMF_FieldPrMod

    implicit none

    type(ESMF_Field),intent(inout) :: field
    integer, intent(out)           :: rc              

    integer :: localrc

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_FieldPrint(field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldprint

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcast"
  subroutine f_esmf_fieldcast(fieldOut, fieldIn, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_FieldMod
    use ESMF_FieldPrMod

    implicit none

    type(ESMF_Field),intent(inout) :: fieldOut
    type(ESMF_Field),intent(inout) :: fieldIn
    integer, intent(out)           :: rc              

    integer :: localrc

    localrc = ESMF_RC_NOT_IMPL

    ! simple assignment
    fieldOut = fieldIn

    ! return successfully
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcast

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldgetmesh"
  subroutine f_esmf_fieldgetmesh(field, meshp, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod

    implicit none

    type(ESMF_Field),intent(inout) :: field
    type(ESMF_Pointer)             :: meshp
    integer, intent(out)           :: rc              

    type(ESMF_Mesh)             :: mesh

    rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    meshp = mesh%this;

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldgetmesh

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldgetarray"
  subroutine f_esmf_fieldgetarray(field, array, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_ArrayMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod

    implicit none

    ! arguments
    type(ESMF_Field),intent(inout) :: field
    type(ESMF_Array)               :: array
    integer, intent(out)           :: rc              

    ! local
    type(ESMF_Array)               :: l_array

    rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(field, array=l_array, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! because ESMF_Array.this is private, it cannot be accessed directly
    ! we use the public interface to do the ptr copy;
    ! the array object returned to the C interface must consist only of the
    ! this pointer. It must not contain the isInit member.
    call ESMF_ArrayCopyThis(l_array, array, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldgetarray

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fielddestroy"
  subroutine f_esmf_fielddestroy(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    type(ESMF_Field)               :: field
    integer, intent(out)           :: rc     
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
  
    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fielddestroy


  subroutine f_esmf_fieldcollectgarbage(field, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcollectgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    type(ESMF_Field)      :: field
    integer, intent(out)  :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting Field garbage"
    
    ! destruct internal data allocations
    call ESMF_FieldDestruct(field%ftypep, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! deallocate actual FieldType allocation      
    if (associated(field%ftypep)) then
      deallocate(field%ftypep, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Deallocating Field", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    nullify(field%ftypep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldcollectgarbage

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridstore"
  subroutine f_esmf_regridstore(srcField, dstField, routehandle, &
                                     regridmethod, unmappedaction, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

      type(ESMF_Field)  :: srcField
      type(ESMF_Field)  :: dstField
      type(ESMF_RouteHandle)  :: routehandle
      type(ESMF_RegridMethod_Flag)  :: regridmethod
      type(ESMF_UnmappedAction_Flag)  :: unmappedaction
      integer  :: rc 

    integer :: localrc
    type(ESMF_RouteHandle) :: l_routehandle
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

	! handle the regridmethod and unmappedaction flags

    call ESMF_FieldRegridStore(srcField, dstField, &
                               regridmethod=regridmethod, &
                               unmappedaction=unmappedaction, &
                               routehandle=l_routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! because ESMF_RouteHandle.this is private, it cannot be accessed directly
    ! we use the public interface to do the ptr copy;
    ! the array object returned to the C interface must consist only of the
    ! this pointer. It must not contain the isInit member.
    call ESMF_RoutehandleCopyThis(l_routehandle, routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_regridstore

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regrid"
  subroutine f_esmf_regrid(srcField, dstField, routehandle, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

    type(ESMF_Field)        :: srcField
    type(ESMF_Field)        :: dstField
    type(ESMF_RouteHandle)  :: routehandle
    integer                 :: rc 

    integer :: localrc
    type(ESMF_RouteHandle)  :: l_routehandle
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Must first create a proper ESMF_RouteHandle that contains the 
    ! required "isInit" class member.
    ! Copy the this pointer a new ESMF_RouteHandle object
    call ESMF_RouteHandleCopyThis(routehandle, l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! set the valid init code of the new object
    call ESMF_RouteHandleSetInitCreated(l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! handle the regridmethod and unmappedaction flags

    call ESMF_FieldRegrid(srcField, dstField, routehandle=l_routehandle, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_regrid

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridrelease"
  subroutine f_esmf_regridrelease(routehandle, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

    type(ESMF_RouteHandle)  :: routehandle
    integer                 :: rc 

    integer :: localrc
    type(ESMF_RouteHandle)  :: l_routehandle
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Must first create a proper ESMF_RouteHandle that contains the 
    ! required "isInit" class member.
    ! Copy the this pointer a new ESMF_RouteHandle object
    call ESMF_RouteHandleCopyThis(routehandle, l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! set the valid init code of the new object
    call ESMF_RouteHandleSetInitCreated(l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridRelease(routehandle=l_routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_regridrelease


