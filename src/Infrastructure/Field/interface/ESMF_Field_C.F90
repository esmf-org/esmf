!  $Id: ESMF_Field_C.F90,v 1.13 2009/09/24 18:48:37 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Field_C.F90,v 1.13 2009/09/24 18:48:37 feiliu Exp $'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreate"
  subroutine f_esmf_fieldcreate(field, mesh_pointer, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, rc)

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
    integer, dimension(:)          :: gridToFieldMap, ungriddedLBound, ungriddedUBound
    integer, intent(out), optional :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
    integer :: localrc              
  
  ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreate(mesh_pointer)
  
    !thefield = ESMF_FieldCreate(mesh, arrayspec=arrayspec, gridToFieldMap=gridToFieldMap, &
    !    ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
    !    rc=localrc)    
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        rc=localrc)    
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    if (present(rc)) rc = localrc
  end subroutine f_esmf_fieldcreate

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

    type(ESMF_Field)            :: field
    type(ESMF_Pointer)          :: meshp
    integer, intent(out), optional :: rc              

    integer     :: localrc
    type(ESMF_Mesh)             :: mesh

    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(field, mesh=mesh, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    meshp = mesh%this;

    if (present(rc)) rc = localrc
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
    integer, intent(out), optional :: rc     
  
    integer :: localrc              
  
  ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
    call ESMF_FieldDestroy(field, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    if (present(rc)) rc = localrc

  end subroutine f_esmf_fielddestroy


  subroutine f_esmf_fieldcollectgarbage(ftype, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcollectgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    type(ESMF_FieldType), pointer :: ftype
    integer, intent(out) :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting Field garbage"
    
    ! destruct internal data allocations
    call ESMF_FieldDestruct(ftype, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! deallocate actual FieldType allocation      
    if (associated(ftype)) then
      deallocate(ftype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating Field", &
        ESMF_CONTEXT, rc)) return
    endif
    nullify(ftype)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldcollectgarbage
