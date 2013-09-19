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
#define ESMF_FILENAME "ESMF_Field_C.F90"
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
#define ESMF_METHOD "f_esmf_fieldcreategridas"
  subroutine f_esmf_fieldcreategridas(field, grid_pointer, arrayspec, &
    staggerloc, gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_StaggerLocMod
    use ESMF_GridMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: grid_pointer
    type(ESMF_ArraySpec)           :: arrayspec
    type(ESMF_StaggerLoc)          :: staggerloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Grid)          :: grid
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    grid%this = grid_pointer

    ESMF_INIT_SET_CREATED(grid)
    
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreategridas

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreategridtk"
  subroutine f_esmf_fieldcreategridtk(field, grid_pointer, typekind, &
    staggerloc, gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod
    use ESMF_StaggerLocMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: grid_pointer
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_StaggerLoc)          :: staggerloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Grid)          :: grid
 
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    grid%this = grid_pointer

    ESMF_INIT_SET_CREATED(grid)
 
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreategridtk

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatemeshas"
  subroutine f_esmf_fieldcreatemeshas(field, mesh_pointer, arrayspec, &
    gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

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
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreate(mesh_pointer)
  
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatemeshas

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatemeshtk"
  subroutine f_esmf_fieldcreatemeshtk(field, mesh_pointer, typekind, meshloc, &
    gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

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
    type(ESMF_MeshLoc)             :: meshloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreate(mesh_pointer)

    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    endif
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
      !print *, "deallocate(field%ftypep)"
      deallocate(field%ftypep, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Deallocating Field", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    nullify(field%ftypep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldcollectgarbage

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridgetarea"
  subroutine f_esmf_regridgetarea(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

    type(ESMF_Field)        :: field
    integer                 :: rc

    integer :: localrc

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    call ESMF_FieldRegridGetArea(field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_regridgetarea


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridstore"
  subroutine f_esmf_regridstore(srcField, dstField, &
                                srcMaskValues, len1, smvpresent, &
                                dstMaskValues, len2, dmvpresent, &
                                routehandle, &
				regridmethod, rmpresent, &
				polemethod, pmpresent, &
				regridPoleNPnts, rpnppresent, &
                                unmappedaction, uapresent, &
                                srcFracField, sffpresent, &
                                dstFracField, dffpresent, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

      type(ESMF_Field)                       :: srcField
      type(ESMF_Field)                       :: dstField
      integer, intent(in)                    :: len1, len2
      integer, intent(in)                    :: smvpresent, dmvpresent
      integer, intent(in)                    :: sffpresent, dffpresent
      integer                                :: srcMaskValues(1:len1), &
                                                dstMaskValues(1:len2)
      type(ESMF_RouteHandle)                 :: routehandle
      integer, intent(in)                    :: rmpresent, uapresent
      type(ESMF_RegridMethod_Flag)           :: regridmethod
      type(ESMF_PoleMethod_Flag), intent(in) :: polemethod
      integer, intent(in)                    :: pmpresent, rpnppresent
      integer, intent(in)                    :: regridPoleNPnts
      
      type(ESMF_UnmappedAction_Flag) :: unmappedaction
      type(ESMF_Field)               :: srcFracField
      type(ESMF_Field)               :: dstFracField
      integer                        :: rc 

    integer :: localrc
    type(ESMF_RouteHandle) :: l_routehandle
    type(ESMF_PoleMethod_Flag) :: polemethod_loc
    integer :: regridPoleNPnts_loc
  
    print *, "ESMF_Field_C.F90"
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Set reasonable defaults for some of the optional arguments
    if (pmpresent == 0) then
      if ((rmpresent == 1) .and. &
          ((regridmethod == ESMF_REGRIDMETHOD_CONSERVE) .or. &
	   (regridmethod == ESMF_REGRIDMETHOD_NEAREST_STOD) .or. &
	   (regridmethod == ESMF_REGRIDMETHOD_NEAREST_DTOS))) then
	polemethod_loc = ESMF_POLEMETHOD_NONE
      else
        polemethod_loc = ESMF_POLEMETHOD_ALLAVG
      endif
    else
      polemethod_loc = polemethod
    endif
    if (rpnppresent == 0) then
      regridPoleNPnts_loc = 0
    else
      regridPoleNPnts_loc = regridPoleNPnts
    endif

    ! handle the optional parameters
    if (rmpresent == 1 .and. uapresent == 1) then
      if (smvpresent == 0 .and. dmvpresent == 0 &
          .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      endif
    elseif (rmpresent == 0 .and. uapresent == 1) then
      if (smvpresent == 0 .and. dmvpresent == 0 &
          .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   unmappedaction=unmappedaction, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      endif
    elseif (rmpresent == 1 .and. uapresent == 0) then
      if (smvpresent == 0 .and. dmvpresent == 0 &
          .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   regridmethod=regridmethod, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      endif
    elseif (rmpresent == 0 .and. uapresent == 0) then
      if (smvpresent == 0 .and. dmvpresent == 0 &
          .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 0 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 0) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 0 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 0 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      else if (smvpresent == 1 .and. dmvpresent == 1 &
               .and. sffpresent == 1 .and. dffpresent == 1) then
        call ESMF_FieldRegridStore(srcField, dstField, &
                                   srcMaskValues=srcMaskValues, &
                                   dstMaskValues=dstMaskValues, &
                                   routehandle=l_routehandle, &
                                   srcFracField=srcFracField, &
                                   dstFracField=dstFracField, &
    				   polemethod=polemethod_loc, &
				   regridPoleNPnts=regridPoleNPnts_loc, &
                                   rc=localrc)
      endif
    endif

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
  subroutine f_esmf_regrid(srcField, dstField, routehandle, zeroregion, zrpresent, rc)

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
    type(ESMF_Region_Flag)  :: zeroregion
    integer, intent(in)     :: zrpresent
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

    ! handle the zeroregion flag
    if (zrpresent == 0) then
      call ESMF_FieldRegrid(srcField, dstField, routehandle=l_routehandle, &
        rc=localrc)
    elseif (zrpresent == 1) then
      call ESMF_FieldRegrid(srcField, dstField, routehandle=l_routehandle, &
        zeroregion=zeroregion, rc=localrc)
    endif
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


