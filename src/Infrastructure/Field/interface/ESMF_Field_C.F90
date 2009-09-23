!  $Id: ESMF_Field_C.F90,v 1.11 2009/09/23 15:33:27 theurich Exp $
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
!      '$Id: ESMF_Field_C.F90,v 1.11 2009/09/23 15:33:27 theurich Exp $'
!==============================================================================

  subroutine f_esmf_fieldcreate(fieldp, mesh, arrayspec, &
    gridToFieldMap, ungriddedLBound, ungriddedUBound, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    ! arguments
    type(ESMF_Field), pointer      :: fieldp
    type(ESMF_Mesh), pointer       :: mesh
    type(ESMF_ArraySpec)           :: arrayspec
    integer, dimension(:), pointer :: gridToFieldMap, ungriddedLBound, ungriddedUBound
    integer, intent(out), optional :: rc              
  
    ! local variables  
    type(ESMF_Field), target :: thefield
  
    integer :: localrc              
  
  ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
    thefield = ESMF_FieldCreate(mesh, arrayspec=arrayspec, gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        rc=rc)    
   
    fieldp => thefield
    if (present(rc)) rc = localrc
  end subroutine f_esmf_fieldcreate

  subroutine f_esmf_fieldget(fieldp, meshp, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod

    type(ESMF_Field), pointer   :: fieldp
    type(ESMF_Mesh), pointer    :: meshp
    integer, intent(out), optional :: rc              

    integer     :: localrc

    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(fieldp, mesh=meshp, rc=localrc)

    if (present(rc)) rc = localrc
  end subroutine f_esmf_fieldget

  subroutine f_esmf_fielddestroy(fieldp, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    type(ESMF_Field), pointer :: fieldp      
    integer, intent(out), optional :: rc     
  
    integer :: localrc              
  
  ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
    call ESMF_FieldDestroy(fieldp, rc=localrc)
  
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
  
    ! destruct intenal data allocations
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
