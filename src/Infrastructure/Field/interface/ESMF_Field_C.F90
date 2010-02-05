!  $Id: ESMF_Field_C.F90,v 1.18.2.1 2010/02/05 19:55:46 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Field_C.F90,v 1.18.2.1 2010/02/05 19:55:46 svasquez Exp $'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreate"
  subroutine f_esmf_fieldcreate(field, mesh_pointer, arrayspec, &
    gridToFieldMap, len1, ungriddedLBound, len2, ungriddedUBound, len3, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: mesh_pointer
    type(ESMF_ArraySpec)           :: arrayspec
    integer                        :: gridToFieldMap(1:len1), len1, ungriddedLBound(1:len2), len2, ungriddedUBound(1:len3), len3
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
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldprint"
  subroutine f_esmf_fieldprint(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_FieldMod
    use ESMF_FieldPrMod

    type(ESMF_Field),intent(inout) :: field
    integer, intent(out)           :: rc              

    integer :: localrc

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_FieldPrint(field, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldprint

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldget"
  subroutine f_esmf_fieldget(field, meshp, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod

    type(ESMF_Field),intent(inout) :: field
    type(ESMF_Pointer)             :: meshp
    integer, intent(out)           :: rc              

    type(ESMF_Mesh)             :: mesh

    rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    meshp = mesh%this;

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldget

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fielddestroy"
  subroutine f_esmf_fielddestroy(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    type(ESMF_Field)               :: field
    integer, intent(out)           :: rc     
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
  
    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &
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

    type(ESMF_Field)      :: field
    integer, intent(out)  :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting Field garbage"
    
    ! destruct internal data allocations
    call ESMF_FieldDestruct(field%ftypep, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! deallocate actual FieldType allocation      
    if (associated(field%ftypep)) then
      deallocate(field%ftypep, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating Field", &
        ESMF_CONTEXT, rc)) return
    endif
    nullify(field%ftypep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldcollectgarbage
