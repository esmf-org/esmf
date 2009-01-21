! $Id: ESMF_StateHelpers.F90,v 1.12.2.5 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!

#define ESMF_FILENAME "ESMF_StateHelpers.F90"

#include "ESMF.h"

module ESMF_StateHelpers

   use ESMF_Mod

   public Create2DIGrids, Create3DIGrids
   public CreateFields, CreateFieldBundle, AddFieldBundle

   public FillConstantR8Field, FillConstantR4Field
   public FillConstantI8Field, FillConstantI4Field
   public ValidateConstantR8Field, ValidateConstantR4Field
   public ValidateConstantI8Field, ValidateConstantI4Field

   public FillConstantR8Halo, FillConstantR4Halo
   public ValidateConstantR8Halo, ValidateConstantR4Halo
   public FillIndexField
   public ValidateIndexField
   public FieldCleanup, FieldBundleCleanup

   public CreateIGrid, CreateLayout
   public CreateEmptyDataField, CreateDataField

contains

!------------------------------------------------------------------------------
!
! Create 2 igrids which have identical numbers of cells and coordinates,
! but are decomposed differently.
!
! TODO: pass in optional list of layouts, list of rellocs, and
!  make igrid an array of igrids?
!
#undef  ESMF_METHOD
#define ESMF_METHOD "Create2DIGrids"
subroutine Create2DIGrids(igrid1, igrid2, rc)
    type(ESMF_IGrid), intent(out) :: igrid1, igrid2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: npets
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

 
    rc = ESMF_FAILURE
        
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    layout1 = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    layout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    mincoords = (/  0.0,  0.0 /)
    maxcoords = (/ 20.0, 30.0 /)
    igrid1 = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(igrid1, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same igrid coordinates, but different layout
    igrid2 = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="dstigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(igrid2, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    rc = ESMF_SUCCESS
    return

end subroutine Create2DIGrids


!------------------------------------------------------------------------------
!
! Create 2 igrids which have identical numbers of cells and coordinates,
! but are decomposed differently.
!
! TODO: pass in optional list of layouts, list of rellocs, and
!  make igrid an array of igrids?
!
#undef  ESMF_METHOD
#define ESMF_METHOD "Create3DIGrids"
subroutine Create3DIGrids(igrid1, igrid2, rc)
    type(ESMF_IGrid), intent(out) :: igrid1, igrid2
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: npets
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

 
    rc = ESMF_FAILURE
        
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    layout1 = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    layout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    mincoords = (/  0.0,  0.0 /)
    maxcoords = (/ 20.0, 30.0 /)
    igrid1 = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(igrid1, delayout=layout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! same igrid coordinates, but different layout
    igrid2 = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="dstigrid", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_IGridDistribute(igrid2, delayout=layout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return


    rc = ESMF_SUCCESS
    return

end subroutine Create3DIGrids



!------------------------------------------------------------------------------
!
! Create 2 fields with the same igrid
!
! todo: make fields a list and create as many as are in the list?
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateFields"
subroutine CreateFields(igrid1, field1, field2, field3, field4, field5, &
                        dim1, dim2, dim3, dim4, dim5, &
                        dkind1, dkind2, dkind3, dkind4, dkind5, &
                        halo1, halo2, halo3, halo4, halo5, &
                        relloc1, relloc2, relloc3, relloc4, relloc5, &
                        vrelloc1, vrelloc2, vrelloc3, vrelloc4, vrelloc5, rc)
    type(ESMF_IGrid), intent(in) :: igrid1
    type(ESMF_Field), intent(out) :: field1
    type(ESMF_Field), intent(out), optional :: field2, field3, field4, field5
    integer, intent(in), optional :: dim1, dim2, dim3, dim4, dim5
    integer, intent(in), optional :: halo1, halo2, halo3, halo4, halo5
    type(ESMF_TypeKind), intent(in), optional :: dkind1, dkind2 
    type(ESMF_TypeKind), intent(in), optional :: dkind3, dkind4, dkind5
    type(ESMF_RelLoc), intent(in), optional :: relloc1, relloc2
    type(ESMF_RelLoc), intent(in), optional :: relloc3, relloc4, relloc5
    type(ESMF_RelLoc), intent(in), optional :: vrelloc1, vrelloc2
    type(ESMF_RelLoc), intent(in), optional :: vrelloc3, vrelloc4, vrelloc5
    integer, intent(out), optional :: rc
    
    ! Local variables
    integer :: halof1, halof2, halof3, halof4, halof5
    integer :: dimf1, dimf2, dimf3, dimf4, dimf5
    type(ESMF_TypeKind) :: dkindf1, dkindf2, dkindf3, dkindf4, dkindf5
    type(ESMF_RelLoc) :: rellocf1, rellocf2, rellocf3, rellocf4, rellocf5
    type(ESMF_RelLoc) :: vrellocf1, vrellocf2, vrellocf3, vrellocf4, vrellocf5
    type(ESMF_ArraySpec) :: arrayspecf1, arrayspecf2
    type(ESMF_ArraySpec) :: arrayspecf3, arrayspecf4, arrayspecf5

 
    rc = ESMF_FAILURE


    ! set default halowidths
    halof1 = 3
    halof2 = 3
    halof3 = 3
    halof4 = 3
    halof5 = 3
    if (present(halo1)) halof1 = halo1
    if (present(halo2)) halof2 = halo2
    if (present(halo3)) halof3 = halo3
    if (present(halo4)) halof4 = halo4
    if (present(halo5)) halof5 = halo5

    ! set default relative locations
    rellocf1 = ESMF_CELL_CENTER
    rellocf2 = ESMF_CELL_CENTER
    rellocf3 = ESMF_CELL_CENTER
    rellocf4 = ESMF_CELL_CENTER
    rellocf5 = ESMF_CELL_CENTER
    if (present(relloc1)) rellocf1 = relloc1
    if (present(relloc2)) rellocf2 = relloc2
    if (present(relloc3)) rellocf3 = relloc3
    if (present(relloc4)) rellocf4 = relloc4
    if (present(relloc5)) rellocf5 = relloc5

    ! set default vert relative locations
    vrellocf1 = ESMF_CELL_UNDEFINED
    vrellocf2 = ESMF_CELL_UNDEFINED
    vrellocf3 = ESMF_CELL_UNDEFINED
    vrellocf4 = ESMF_CELL_UNDEFINED
    vrellocf5 = ESMF_CELL_UNDEFINED
    if (present(vrelloc1)) vrellocf1 = vrelloc1
    if (present(vrelloc2)) vrellocf2 = vrelloc2
    if (present(vrelloc3)) vrellocf3 = vrelloc3
    if (present(vrelloc4)) vrellocf4 = vrelloc4
    if (present(vrelloc5)) vrellocf5 = vrelloc5

    ! set default data dimensionality
    dimf1 = 2
    dimf2 = 2
    dimf3 = 2
    dimf4 = 2
    dimf5 = 2
    if (present(dim1)) dimf1 = dim1
    if (present(dim2)) dimf2 = dim2
    if (present(dim3)) dimf3 = dim3
    if (present(dim4)) dimf4 = dim4
    if (present(dim5)) dimf5 = dim5

    ! set default data type/size
    dkindf1 = ESMF_TYPEKIND_R8
    dkindf2 = ESMF_TYPEKIND_R8
    dkindf3 = ESMF_TYPEKIND_R8
    dkindf4 = ESMF_TYPEKIND_R8
    dkindf5 = ESMF_TYPEKIND_R8

    if (present(dkind1)) dkindf1 = dkind1
    if (present(dkind2)) dkindf2 = dkind2
    if (present(dkind3)) dkindf3 = dkind3
    if (present(dkind4)) dkindf4 = dkind4
    if (present(dkind5)) dkindf5 = dkind5
   

    ! set the arrayspec as requested
    call ESMF_ArraySpecSet(arrayspecf1, dimf1, dkindf1, rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_ArraySpecSet(arrayspecf2, dimf2, dkindf2, rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_ArraySpecSet(arrayspecf3, dimf3, dkindf3, rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_ArraySpecSet(arrayspecf4, dimf4, dkindf4, rc)
    if (rc.NE.ESMF_SUCCESS) return
    call ESMF_ArraySpecSet(arrayspecf5, dimf5, dkindf5, rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    ! let field create call allocate the proper amount of space
    field1 = ESMF_FieldCreate(igrid1, arrayspecf1, &
                                horzRelloc=rellocf1, vertRelloc=vrellocf1, &
                                haloWidth=halof1, name="pressure 1", rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
                                
    if (present(field2)) then
      ! let field create call allocate the proper amount of space
      field2 = ESMF_FieldCreate(igrid1, arrayspecf2, &
                                horzRelloc=rellocf2, vertRelloc=vrellocf2, &
                                haloWidth=halof2, name="pressure 2", rc=rc)
      if (rc.NE.ESMF_SUCCESS) return
    endif
    if (present(field3)) then
      ! let field create call allocate the proper amount of space
      field3 = ESMF_FieldCreate(igrid1, arrayspecf3, &
                                horzRelloc=rellocf3, vertRelloc=vrellocf3, &
                                haloWidth=halof3, name="pressure 3", rc=rc)
      if (rc.NE.ESMF_SUCCESS) return
    endif
    if (present(field4)) then
      ! let field create call allocate the proper amount of space
      field4 = ESMF_FieldCreate(igrid1, arrayspecf4, &
                                horzRelloc=rellocf4, vertRelloc=vrellocf4, &
                                haloWidth=halof4, name="pressure 4", rc=rc)
      if (rc.NE.ESMF_SUCCESS) return
    endif
    if (present(field5)) then
      ! let field create call allocate the proper amount of space
      field5 = ESMF_FieldCreate(igrid1, arrayspecf5, &
                                horzRelloc=rellocf5, vertRelloc=vrellocf5, &
                                haloWidth=halof5, name="pressure 5", rc=rc)
      if (rc.NE.ESMF_SUCCESS) return
    endif
                                
    rc = ESMF_SUCCESS
    return

end subroutine CreateFields


!------------------------------------------------------------------------------
!
! Create bundle and add up to 5 fields to it
!
! TODO: make fields an array and add as many as are in the list
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateFieldBundle"
subroutine CreateFieldBundle(bundle, field1, field2, field3, field4, field5, rc)
    type(ESMF_FieldBundle), intent(out) :: bundle
    type(ESMF_Field), intent(inout), optional :: field1,field2,field3,field4,field5
    integer, intent(out), optional :: rc
    
    integer :: localrc

    if (present(rc)) rc = ESMF_FAILURE
        
    ! make a bundle and add fields to it
    bundle = ESMF_FieldBundleCreate(rc=localrc)
    if (localrc.NE.ESMF_SUCCESS) return

    if (present(field1)) then
      call ESMF_FieldBundleAdd(bundle, field1, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field2)) then
      call ESMF_FieldBundleAdd(bundle, field2, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field3)) then
      call ESMF_FieldBundleAdd(bundle, field3, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field4)) then
      call ESMF_FieldBundleAdd(bundle, field4, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field5)) then
      call ESMF_FieldBundleAdd(bundle, field5, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine CreateFieldBundle


!------------------------------------------------------------------------------
!
! Add up to 5 fields to an existing bundle.
!
! TODO: make fields an array and add as many as are in the list
!
#undef  ESMF_METHOD
#define ESMF_METHOD "AddFieldBundle"
subroutine AddFieldBundle(bundle, field1, field2, field3, field4, field5, rc)
    type(ESMF_FieldBundle), intent(inout) :: bundle
    type(ESMF_Field), intent(inout), optional :: field1,field2,field3,field4,field5
    integer, intent(out), optional :: rc
    
    integer :: localrc

    if (present(rc)) rc = ESMF_FAILURE
        
    if (present(field1)) then
      call ESMF_FieldBundleAdd(bundle, field1, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field2)) then
      call ESMF_FieldBundleAdd(bundle, field2, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field3)) then
      call ESMF_FieldBundleAdd(bundle, field3, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field4)) then
      call ESMF_FieldBundleAdd(bundle, field4, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(field5)) then
      call ESMF_FieldBundleAdd(bundle, field5, rc=localrc)
      if (localrc.NE.ESMF_SUCCESS) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine AddFieldBundle


!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
! This assumes the data is real*8, 2D to 5D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantR8Field"
subroutine FillConstantR8Field(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    call InternalFillConstantField(field, r8val=val, rc=rc)

end subroutine FillConstantR8Field

!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
! This assumes the data is real*4, 2D to 5D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantR4Field"
subroutine FillConstantR4Field(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    integer, intent(out) :: rc
    
    call InternalFillConstantField(field, r4val=val, rc=rc)

end subroutine FillConstantR4Field

!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
! This assumes the data is integer*8, 2D to 5D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantI8Field"
subroutine FillConstantI8Field(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_I8), intent(in) :: val
    integer, intent(out) :: rc
    
    call InternalFillConstantField(field, i8val=val, rc=rc)

end subroutine FillConstantI8Field

!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
! This assumes the data is integer*4, 2D to 5D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantI4Field"
subroutine FillConstantI4Field(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_I4), intent(in) :: val
    integer, intent(out) :: rc
    
    call InternalFillConstantField(field, i4val=val, rc=rc)

end subroutine FillConstantI4Field

!------------------------------------------------------------------------------
!
! Fill all the data associated with a field with a constant value.
!  Data must be R4, R8, I4, or I8, and 2D to 5D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "InternalFillConstantField"
subroutine InternalFillConstantField(field, r4val, r8val, i4val, i8val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in), optional :: r8val
    real (ESMF_KIND_R4), intent(in), optional :: r4val
    real (ESMF_KIND_I8), intent(in), optional :: i8val
    real (ESMF_KIND_I4), intent(in), optional :: i4val
    integer, intent(out), optional :: rc
    
    ! Local variables
    type(ESMF_TypeKind) :: dk
    integer :: rank, kind

    ! pointer zoo
    real (ESMF_KIND_R8), dimension(:,:),       pointer :: ptr2dr8
    real (ESMF_KIND_R8), dimension(:,:,:),     pointer :: ptr3dr8
    real (ESMF_KIND_R8), dimension(:,:,:,:),   pointer :: ptr4dr8
    real (ESMF_KIND_R4), dimension(:,:),       pointer :: ptr2dr4
    real (ESMF_KIND_R4), dimension(:,:,:),     pointer :: ptr3dr4
    real (ESMF_KIND_R4), dimension(:,:,:,:),   pointer :: ptr4dr4
    real (ESMF_KIND_I8), dimension(:,:),       pointer :: ptr2di8
    real (ESMF_KIND_I8), dimension(:,:,:),     pointer :: ptr3di8
    real (ESMF_KIND_I8), dimension(:,:,:,:),   pointer :: ptr4di8
    real (ESMF_KIND_I4), dimension(:,:),       pointer :: ptr2di4
    real (ESMF_KIND_I4), dimension(:,:,:),     pointer :: ptr3di4
    real (ESMF_KIND_I4), dimension(:,:,:,:),   pointer :: ptr4di4

#ifndef ESMF_NO_GREATER_THAN_4D
    real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: ptr5dr8
    real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: ptr5dr4
    real (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: ptr5di8
    real (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: ptr5di4
#endif

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.
    call ESMF_FieldGet(field, rank=rank, typekind=dk, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! force to integer so they can be used below in select cases.
    kind = dk

    ! TODO: if it is important to set the halo to something other than the
    ! constant value, then we will need the halo width and the igrid info.
    ! for now, simply set the entire data space to the constant value.
    !call ESMF_FieldGet(field, haloWidth=halo, igrid=igrid, rc=rc)

    select case (rank)
      case (2)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr2dr8(:,:) = r8val

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr2dr4(:,:) = r4val

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr2di8(:,:) = i8val

          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr2di4(:,:) = i4val

          case default
            print *, "unsupported data type in Field"
            return
         end select

      case (3)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr3dr8(:,:,:) = r8val

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr3dr4(:,:,:) = r4val

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr3di8(:,:,:) = i8val

          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr3di4(:,:,:) = i4val

          case default
            print *, "unsupported data type in Field"
            return
         end select

      case (4)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr4dr8(:,:,:,:) = r8val

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr4dr4(:,:,:,:) = r4val

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr4di8(:,:,:,:) = i8val

          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr4di4(:,:,:,:) = i4val

          case default
            print *, "unsupported data type in Field"
            return
         end select

#ifndef ESMF_NO_GREATER_THAN_4D
      case (5)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr5dr8(:,:,:,:,:) = r8val

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr5dr4(:,:,:,:,:) = r4val

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr5di8(:,:,:,:,:) = i8val

          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            ptr5di4(:,:,:,:,:) = i4val
          case default
            print *, "unsupported data type in Field"
            return
         end select
#endif
      case default
        print *, "unsupported rank"

    end select

    

    rc = ESMF_SUCCESS
    return

end subroutine InternalFillConstantField

!------------------------------------------------------------------------------
!
! Fill the halo region (only) associated with a field with a constant value.
! This assumes the data is real*8, 2D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantR8Halo"
subroutine FillConstantR8Halo(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R8), dimension(:,:), pointer :: ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    lb(:) = lbound(ptr)
    ub(:) = ubound(ptr)
    
    ! now set the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    ! bottom / south
    ptr(lb(1):ub(1), lb(2):lb(2)+halo-1) = val

    ! left / west edge
    ptr(lb(1):lb(1)+halo-1, lb(2):ub(2)) = val

    ! right / east edge
    ptr(ub(1)-halo+1:ub(1), lb(2):ub(2)) = val

    ! top / north
    ptr(lb(1):ub(1), ub(2)-halo+1:ub(2)) = val

    rc = ESMF_SUCCESS

    return

end subroutine FillConstantR8Halo


!------------------------------------------------------------------------------
!
! Fill the halo region (only) associated with a field with a constant value.
! This assumes the data is real*4, 2D.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillConstantR4Halo"
subroutine FillConstantR4Halo(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R4), dimension(:,:), pointer :: ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    lb(:) = lbound(ptr)
    ub(:) = ubound(ptr)
    
    ! now set the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    ! bottom / south
    ptr(lb(1):ub(1), lb(2):lb(2)+halo-1) = val

    ! left / west edge
    ptr(lb(1):lb(1)+halo-1, lb(2):ub(2)) = val

    ! right / east edge
    ptr(ub(1)-halo+1:ub(1), lb(2):ub(2)) = val

    ! top / north
    ptr(lb(1):ub(1), ub(2)-halo+1:ub(2)) = val

    rc = ESMF_SUCCESS

    return

end subroutine FillConstantR4Halo


!------------------------------------------------------------------------------
!
! Fill a field with real*8 values which are the global cell index number.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FillIndexField"
subroutine FillIndexField(field, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(7), ub(7), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(3), globalCellCounts(3), igridOffsets(3)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: ptr
    !integer :: i, j, k, l, m
    type(ESMF_IGrid) :: igrid
    !type(ESMF_Array) :: array
    !integer :: rank

    rc = ESMF_FAILURE
          
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, igrid=igrid, rc=rc)

    call ESMF_IGridGet(igrid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=igridOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(1:2) = lbound(ptr)
    ub(1:2) = ubound(ptr)

    ! Set the data values to the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      rownum = j - halowidth - 1
      cellNum = (igridOffsets(1) + 1) + &
                ((igridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        colnum = i - haloWidth - 1
        ptr(i, j) = cellNum + colnum
      enddo
    enddo

    rc = ESMF_SUCCESS
    return

end subroutine FillIndexField

!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is real*8.  Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantR8Field"
subroutine ValidateConstantR8Field(field, val, slop, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc

    call InternalValidateConstantField(field, r8val=val, slop=slop, rc=rc)

end subroutine ValidateConstantR8Field
    
!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is real*4.  Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantR4Field"
subroutine ValidateConstantR4Field(field, val, slop, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc
    
    call InternalValidateConstantField(field, r4val=val, slop=slop, rc=rc)

end subroutine ValidateConstantR4Field


!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is integer*8.  Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantI8Field"
subroutine ValidateConstantI8Field(field, val, slop, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_I8), intent(in) :: val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc

    call InternalValidateConstantField(field, i8val=val, slop=slop, rc=rc)

end subroutine ValidateConstantI8Field
    
!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is integer*4.  Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantI4Field"
subroutine ValidateConstantI4Field(field, val, slop, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_I4), intent(in) :: val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc
    
    call InternalValidateConstantField(field, i4val=val, slop=slop, rc=rc)

end subroutine ValidateConstantI4Field


!------------------------------------------------------------------------------
!
! Make sure the data in a field matches the constant value.
! Assumes data is R8, R4, I8, or I4, and 2D to 5D.  
! Do not check halo regions.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "InternalValidateConstantField"
subroutine InternalValidateConstantField(field, r8val, r4val, i8val, i4val, &
                                          slop, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in), optional :: r8val
    real (ESMF_KIND_R4), intent(in), optional :: r4val
    real (ESMF_KIND_I8), intent(in), optional :: i8val
    real (ESMF_KIND_I4), intent(in), optional :: i4val
    logical, intent(in), optional :: slop
    integer, intent(out), optional  :: rc
    
    ! Local variables
    integer :: i, j, k, l, m
    integer :: lb(7), ub(7), halo

    type(ESMF_TypeKind) :: dk
    integer :: rank, kind

    real (ESMF_KIND_R8), dimension(:,:),       pointer :: ptr2dr8
    real (ESMF_KIND_R8), dimension(:,:,:),     pointer :: ptr3dr8
    real (ESMF_KIND_R8), dimension(:,:,:,:),   pointer :: ptr4dr8
    real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: ptr5dr8
    real (ESMF_KIND_R4), dimension(:,:),       pointer :: ptr2dr4
    real (ESMF_KIND_R4), dimension(:,:,:),     pointer :: ptr3dr4
    real (ESMF_KIND_R4), dimension(:,:,:,:),   pointer :: ptr4dr4
    real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: ptr5dr4
    real (ESMF_KIND_I8), dimension(:,:),       pointer :: ptr2di8
    real (ESMF_KIND_I8), dimension(:,:,:),     pointer :: ptr3di8
    real (ESMF_KIND_I8), dimension(:,:,:,:),   pointer :: ptr4di8
    real (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: ptr5di8
    real (ESMF_KIND_I4), dimension(:,:),       pointer :: ptr2di4
    real (ESMF_KIND_I4), dimension(:,:,:),     pointer :: ptr3di4
    real (ESMF_KIND_I4), dimension(:,:,:,:),   pointer :: ptr4di4
    real (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: ptr5di4


    if (present(rc)) rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rank=rank, typekind=dk, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! force to integer so they can be used below in select cases.
    kind = dk

    ! if slop specified, and true, then do not check the outer row
    ! of cells in the computational area.  this is for regrid where
    ! the boundary conditions may affect the outer rows.
    if (present(slop)) then
        if (slop) halo = halo + 1
    endif

    ! get a Fortran 90 pointer back to the data
    select case (rank)
      case (2)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:2) = lbound(ptr2dr8)
            ub(1:2) = ubound(ptr2dr8)
  
            rc = ESMF_SUCCESS
            do j=lb(2)+halo, ub(2)-halo
              do i=lb(1)+halo, ub(1)-halo
                if (abs(ptr2dr8(i, j) - r8val) .gt. 10E-8) then
                    print *, "data mismatch at", i, j, ptr2dr8(i,j), &
                              " ne ", r8val
                    rc = ESMF_FAILURE
                    print *, "(bailing on first error - may be others)"
                    return 
                endif
              enddo
            enddo

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:2) = lbound(ptr2dr4)
            ub(1:2) = ubound(ptr2dr4)
  
            rc = ESMF_SUCCESS
            do j=lb(2)+halo, ub(2)-halo
              do i=lb(1)+halo, ub(1)-halo
                if (abs(ptr2dr4(i, j) - r4val) .gt. 10E-8) then
                    print *, "data mismatch at", i, j, ptr2dr4(i,j), &
                              " ne ", r4val
                    rc = ESMF_FAILURE
                    print *, "(bailing on first error - may be others)"
                    return 
                endif
              enddo
            enddo

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:2) = lbound(ptr2di8)
            ub(1:2) = ubound(ptr2di8)
  
            rc = ESMF_SUCCESS
            do j=lb(2)+halo, ub(2)-halo
              do i=lb(1)+halo, ub(1)-halo
                if (ptr2di8(i, j) .ne. i8val) then
                    print *, "data mismatch at", i, j, ptr2di8(i,j), &
                              " ne ", i8val
                    rc = ESMF_FAILURE
                    print *, "(bailing on first error - may be others)"
                    return 
                endif
              enddo
            enddo

          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr2di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:2) = lbound(ptr2di4)
            ub(1:2) = ubound(ptr2di4)
  
            rc = ESMF_SUCCESS
            do j=lb(2)+halo, ub(2)-halo
              do i=lb(1)+halo, ub(1)-halo
                if (ptr2di4(i, j) .ne. i4val) then
                    print *, "data mismatch at", i, j, ptr2di4(i,j), &
                              " ne ", i4val
                    rc = ESMF_FAILURE
                    print *, "(bailing on first error - may be others)"
                    return 
                endif
              enddo
            enddo
          
          case default
            print *, "unsupported data type in Field"
            return

        end select

      case (3)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:3) = lbound(ptr3dr8)
            ub(1:3) = ubound(ptr3dr8)
  
            rc = ESMF_SUCCESS
            do k=lb(3)+halo, ub(3)-halo
              do j=lb(2)+halo, ub(2)-halo
                do i=lb(1)+halo, ub(1)-halo
                  if (abs(ptr3dr8(i, j, k) - r8val) .gt. 10E-8) then
                      print *, "data mismatch at", i, j, k, &
                                ptr3dr8(i,j,k), " ne ", r8val
                      rc = ESMF_FAILURE
                      print *, "(bailing on first error - may be others)"
                      return 
                  endif
                enddo
              enddo
            enddo

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:3) = lbound(ptr3dr4)
            ub(1:3) = ubound(ptr3dr4)
  
            rc = ESMF_SUCCESS
            do k=lb(3)+halo, ub(3)-halo
              do j=lb(2)+halo, ub(2)-halo
                do i=lb(1)+halo, ub(1)-halo
                  if (abs(ptr3dr4(i, j, k) - r4val) .gt. 10E-8) then
                      print *, "data mismatch at", i, j, k, &
                                ptr3dr4(i,j,k), " ne ", r4val
                      rc = ESMF_FAILURE
                      print *, "(bailing on first error - may be others)"
                      return 
                  endif
                enddo
              enddo
            enddo

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:3) = lbound(ptr3di8)
            ub(1:3) = ubound(ptr3di8)
  
            rc = ESMF_SUCCESS
            do k=lb(3)+halo, ub(3)-halo
              do j=lb(2)+halo, ub(2)-halo
                do i=lb(1)+halo, ub(1)-halo
                  if (ptr3di8(i, j, k) .ne. i8val) then
                      print *, "data mismatch at", i, j, k, &
                                ptr3di8(i,j,k), " ne ", i8val
                      rc = ESMF_FAILURE
                      print *, "(bailing on first error - may be others)"
                      return 
                  endif
                enddo
              enddo
            enddo
  
          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr3di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:3) = lbound(ptr3di4)
            ub(1:3) = ubound(ptr3di4)
  
            rc = ESMF_SUCCESS
            do k=lb(3)+halo, ub(3)-halo
              do j=lb(2)+halo, ub(2)-halo
                do i=lb(1)+halo, ub(1)-halo
                  if (ptr3di4(i, j, k) .ne. i4val) then
                      print *, "data mismatch at", i, j, k, &
                                ptr3di4(i,j,k), " ne ", i4val
                      rc = ESMF_FAILURE
                      print *, "(bailing on first error - may be others)"
                      return 
                  endif
                enddo
              enddo
            enddo
          
          case default
            print *, "unsupported data type in Field"
            return

        end select

      case (4)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:4) = lbound(ptr4dr8)
            ub(1:4) = ubound(ptr4dr8)
  
            rc = ESMF_SUCCESS
            do l=lb(4)+halo, ub(4)-halo
              do k=lb(3)+halo, ub(3)-halo
                do j=lb(2)+halo, ub(2)-halo
                  do i=lb(1)+halo, ub(1)-halo
                    if (abs(ptr4dr8(i, j, k, l) - r8val) .gt. 10E-8) then
                        print *, "data mismatch at", i, j, k, l, &
                                  ptr4dr8(i,j,k,l), " ne ", r8val
                        rc = ESMF_FAILURE
                        print *, "(bailing on first error - may be others)"
                        return 
                    endif
                  enddo
                enddo
              enddo
            enddo

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:4) = lbound(ptr4dr4)
            ub(1:4) = ubound(ptr4dr4)
  
            rc = ESMF_SUCCESS
            do l=lb(4)+halo, ub(4)-halo
              do k=lb(3)+halo, ub(3)-halo
                do j=lb(2)+halo, ub(2)-halo
                  do i=lb(1)+halo, ub(1)-halo
                    if (abs(ptr4dr4(i, j, k, l) - r4val) .gt. 10E-8) then
                        print *, "data mismatch at", i, j, k, l, &
                                  ptr4dr4(i,j,k,l), " ne ", r4val
                        rc = ESMF_FAILURE
                        print *, "(bailing on first error - may be others)"
                        return 
                    endif
                  enddo
                enddo
              enddo
            enddo

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:4) = lbound(ptr4di8)
            ub(1:4) = ubound(ptr4di8)
  
            rc = ESMF_SUCCESS
            do l=lb(4)+halo, ub(4)-halo
              do k=lb(3)+halo, ub(3)-halo
                do j=lb(2)+halo, ub(2)-halo
                  do i=lb(1)+halo, ub(1)-halo
                    if (ptr4di8(i, j, k, l) .ne. i8val) then
                        print *, "data mismatch at", i, j, k, l, &
                                  ptr4di8(i,j,k,l), " ne ", i8val
                        rc = ESMF_FAILURE
                        print *, "(bailing on first error - may be others)"
                        return 
                    endif
                  enddo
                enddo
              enddo
            enddo
  
          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr4di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:4) = lbound(ptr4di4)
            ub(1:4) = ubound(ptr4di4)
  
            rc = ESMF_SUCCESS
            do l=lb(4)+halo, ub(4)-halo
              do k=lb(3)+halo, ub(3)-halo
                do j=lb(2)+halo, ub(2)-halo
                  do i=lb(1)+halo, ub(1)-halo
                    if (ptr4di4(i, j, k, l) .ne. i4val) then
                        print *, "data mismatch at", i, j, k, l, &
                                  ptr4di4(i,j,k,l), " ne ", i4val
                        rc = ESMF_FAILURE
                        print *, "(bailing on first error - may be others)"
                        return 
                    endif
                  enddo
                enddo
               enddo
             enddo
          
          case default
            print *, "unsupported data type in Field"
            return

        end select

#ifndef ESMF_NO_GREATER_THAN_4D
      case (5)
        select case (kind)
          case (ESMF_TYPEKIND_R8%dkind)
            if (.not.present(r8val)) then
                print *, "Error: data value does not match Field data type (R8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5dr8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:5) = lbound(ptr5dr8)
            ub(1:5) = ubound(ptr5dr8)
  
            rc = ESMF_SUCCESS
            do m=lb(5)+halo, ub(5)-halo
              do l=lb(4)+halo, ub(4)-halo
                do k=lb(3)+halo, ub(3)-halo
                  do j=lb(2)+halo, ub(2)-halo
                    do i=lb(1)+halo, ub(1)-halo
                      if (abs(ptr5dr8(i, j, k, l, m) - r8val) .gt. 10E-8) then
                          print *, "data mismatch at", i, j, k, l, m, &
                                    ptr5dr8(i,j,k,l,m), " ne ", r8val
                          rc = ESMF_FAILURE
                          print *, "(bailing on first error - may be others)"
                          return 
                      endif
                    enddo
                  enddo
                enddo
              enddo
            enddo

          case (ESMF_TYPEKIND_R4%dkind)
            if (.not.present(r4val)) then
                print *, "Error: data value does not match Field data type (R4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5dr4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:5) = lbound(ptr5dr4)
            ub(1:5) = ubound(ptr5dr4)
  
            rc = ESMF_SUCCESS
            do m=lb(5)+halo, ub(5)-halo
              do l=lb(4)+halo, ub(4)-halo
                do k=lb(3)+halo, ub(3)-halo
                  do j=lb(2)+halo, ub(2)-halo
                    do i=lb(1)+halo, ub(1)-halo
                      if (abs(ptr5dr4(i, j, k, l, m) - r4val) .gt. 10E-8) then
                          print *, "data mismatch at", i, j, k, l, m, &
                                    ptr5dr4(i,j,k,l,m), " ne ", r4val
                          rc = ESMF_FAILURE
                          print *, "(bailing on first error - may be others)"
                          return 
                      endif
                    enddo
                  enddo
                enddo
              enddo
            enddo

          case (ESMF_TYPEKIND_I8%dkind)
            if (.not.present(i8val)) then
                print *, "Error: data value does not match Field data type (I8)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5di8, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:5) = lbound(ptr5di8)
            ub(1:5) = ubound(ptr5di8)
  
            rc = ESMF_SUCCESS
            do m=lb(5)+halo, ub(5)-halo
              do l=lb(4)+halo, ub(4)-halo
                do k=lb(3)+halo, ub(3)-halo
                  do j=lb(2)+halo, ub(2)-halo
                    do i=lb(1)+halo, ub(1)-halo
                      if (ptr5di8(i, j, k, l, m) .ne. i8val) then
                          print *, "data mismatch at", i, j, k, l, m, &
                                    ptr5di8(i,j,k,l,m), " ne ", i8val
                          rc = ESMF_FAILURE
                          print *, "(bailing on first error - may be others)"
                          return 
                      endif
                    enddo
                  enddo
                enddo
              enddo
            enddo
  
          case (ESMF_TYPEKIND_I4%dkind)
            if (.not.present(i4val)) then
                print *, "Error: data value does not match Field data type (I4)"
                return
            endif

            call ESMF_FieldGetDataPointer(field, ptr5di4, ESMF_DATA_REF, rc=rc)
            if (rc.NE.ESMF_SUCCESS) return

            lb(1:5) = lbound(ptr5di4)
            ub(1:5) = ubound(ptr5di4)
  
            rc = ESMF_SUCCESS
            do m=lb(5)+halo, ub(5)-halo
              do l=lb(4)+halo, ub(4)-halo
                do k=lb(3)+halo, ub(3)-halo
                  do j=lb(2)+halo, ub(2)-halo
                    do i=lb(1)+halo, ub(1)-halo
                      if (ptr5di4(i, j, k, l, m) .ne. i4val) then
                          print *, "data mismatch at", i, j, k, l, m, &
                                    ptr5di4(i,j,k,l,m), " ne ", i4val
                          rc = ESMF_FAILURE
                          print *, "(bailing on first error - may be others)"
                          return 
                      endif
                    enddo
                  enddo
                enddo
              enddo
            enddo
          
          case default
            print *, "unsupported data type in Field"
            return

        end select
#endif

      case default
        print *, "no code to handle data of rank", rank

    end select

    

    ! return with whatever rc value it has

    return

end subroutine InternalValidateConstantField


!------------------------------------------------------------------------------
!
! Make sure the halo data in a field matches the constant value.
! Assumes data is real*8.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantHalo"
subroutine ValidateConstantHalo(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R8), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R8), dimension(:,:), pointer :: ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    lb(:) = lbound(ptr)
    ub(:) = ubound(ptr)
    
    ! now check the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    rc = ESMF_SUCCESS

    ! bottom / south
    do j=lb(2), lb(2)+halo-1
      do i=lb(1), ub(1)
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      do i=lb(1), lb(1)+halo-1
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! east edge
    do j=lb(2), ub(2)
      do i=ub(1)-halo+1, ub(1)
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! top / north
    do j=ub(2)-halo+1, ub(2)
      do i=lb(1), ub(1)
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantHalo


!------------------------------------------------------------------------------
!
! Make sure the halo data in a field matches the constant value.
! Assumes data is real*4.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateConstantR4Halo"
subroutine ValidateConstantR4Halo(field, val, rc)
    type(ESMF_Field), intent(inout) :: field
    real (ESMF_KIND_R4), intent(in) :: val
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), halo
    real (ESMF_KIND_R4), dimension(:,:), pointer :: ptr

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=halo, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    

    lb(:) = lbound(ptr)
    ub(:) = ubound(ptr)
    
    ! now check the chunks, one at a time.  this duplicates the overlaps
    ! at the corners, but it is the simplest to program.
 
    rc = ESMF_SUCCESS

    ! bottom / south
    do j=lb(2), lb(2)+halo-1
      do i=lb(1), ub(1)
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! west edge
    do j=lb(2), ub(2)
      do i=lb(1), lb(1)+halo-1
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! east edge
    do j=lb(2), ub(2)
      do i=ub(1)-halo+1, ub(1)
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! top / north
    do j=ub(2)-halo+1, ub(2)
      do i=lb(1), ub(1)
        if (abs(ptr(i, j) - val) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", val
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! return with whatever rc value it has

    return

end subroutine ValidateConstantR4Halo


!------------------------------------------------------------------------------
!
! Make sure the data in a field contains the correct global index numbers.
! Assumes data is real*8.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ValidateIndexField"
subroutine ValidateIndexField(field, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(out) :: rc
    
    ! Local variables
    integer :: i, j
    integer :: lb(2), ub(2), haloWidth, cellNum, rownum, colnum
    integer :: localCellCounts(2), globalCellCounts(2), igridOffsets(2)
    real (ESMF_KIND_R8), dimension(:,:), pointer :: ptr
    type(ESMF_IGrid) :: igrid

    rc = ESMF_FAILURE
        
    ! need a query here to be sure our data pointer is the same t/k/r
    ! as what is in the field.

    call ESMF_FieldGet(field, haloWidth=haloWidth, igrid=igrid, rc=rc)

    call ESMF_IGridGet(igrid, horzrelloc=ESMF_CELL_CENTER, &
                      globalCellCountPerDim=globalCellCounts, rc=rc)

    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=localCellCounts, &
                                 globalStartPerDim=igridOffsets, rc=rc)

    ! get a Fortran 90 pointer back to the data
    call ESMF_FieldGetDataPointer(field, ptr, ESMF_DATA_REF, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return
    
    lb(:) = lbound(ptr)
    ub(:) = ubound(ptr)

    ! start with success, and any mismatch sets error
    rc = ESMF_SUCCESS

    ! Check the data values against the global cell index number.
    do j=lb(2)+haloWidth, ub(2)-haloWidth
      rownum = j - halowidth - 1
      cellNum = (igridOffsets(1) + 1) + &
                ((igridOffsets(2)+rownum) * globalCellCounts(1)) 
      do i=lb(1)+haloWidth, ub(1)-haloWidth
        colnum = i - haloWidth - 1
        if (abs(ptr(i, j) - (cellNum+colnum)) .gt. 10E-8) then
            print *, "data mismatch at", i, j, ptr(i,j), " ne ", &
                        cellNum + colnum
            rc = ESMF_FAILURE
            print *, "(bailing on first error - may be others)"
            return 
        endif
      enddo
    enddo

    ! rc set above, leave it as is

    return

end subroutine ValidateIndexField


!------------------------------------------------------------------------------
!
! delete all fields; this code assumes that all share the identical igrid
!  and only delete it once.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FieldCleanup"
subroutine FieldCleanup(field1, field2, field3, field4, field5, doigrid, rc)
    type(ESMF_Field), intent(inout) :: field1
    type(ESMF_Field), intent(inout), optional :: field2, field3, field4, field5
    logical, optional :: doigrid
    integer, intent(out), optional :: rc
    
    ! Local variables
    type(ESMF_IGrid) :: igrid

    if (present(rc)) rc = ESMF_FAILURE

    ! query for igrid and data.  field1 is required; all other fields test
    ! first to be sure it is present
    call ESMF_FieldGet(field1, igrid=igrid, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    ! query for data only; igrid is shared and will be deleted last.
    if (present(field2)) then
        call ESMF_FieldDestroy(field2, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! query for data only; igrid is shared and will be deleted last.
    if (present(field3)) then
        call ESMF_FieldDestroy(field3, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! query for data only; igrid is shared and will be deleted last.
    if (present(field4)) then
        call ESMF_FieldDestroy(field4, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! query for data only; igrid is shared and will be deleted last.
    if (present(field5)) then
        call ESMF_FieldDestroy(field5, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return
    endif

    ! do this last, unless requested not to.
    if (.not. present(doigrid)) then
      call ESMF_IGridDestroy(igrid, rc=rc)
      if (rc.NE.ESMF_SUCCESS) return
    else if (doigrid) then
      call ESMF_IGridDestroy(igrid, rc=rc)
      if (rc.NE.ESMF_SUCCESS) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine FieldCleanup

!------------------------------------------------------------------------------
!
! delete all bundles; this code assumes that all share the identical igrid
!  and only delete it once.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "FieldBundleCleanup"
subroutine FieldBundleCleanup(bundle1, bundle2, bundle3, bundle4, bundle5, rc)
    type(ESMF_FieldBundle), intent(inout) :: bundle1
    type(ESMF_FieldBundle), intent(inout), optional :: bundle2, bundle3
    type(ESMF_FieldBundle), intent(inout), optional :: bundle4, bundle5
    integer, intent(out), optional :: rc
    
    ! Local variables
    !type(ESMF_IGrid) :: igrid
    !type(ESMF_Field) :: allfields(:)

    if (present(rc)) rc = ESMF_FAILURE

    ! query for igrid.  bundle1 is required; all other bundles optional.
    !call ESMF_FieldBundleGet(bundle1, field=allfields, igrid=igrid, rc=rc)
    !if (rc.NE.ESMF_SUCCESS) return

    call ESMF_FieldBundleDestroy(bundle1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) return

    !do i=1 to nfields

        !call ESMF_FieldGet(allfields(i), array=array, rc=rc)
        !if (rc.NE.ESMF_SUCCESS) return

        !call ESMF_FieldDestroy(allfields(i), rc=rc)
        !if (rc.NE.ESMF_SUCCESS) return
   
        !call ESMF_ArrayDestroy(array, rc=rc)
        !if (rc.NE.ESMF_SUCCESS) return

    !enddo

    !call ESMF_IGridDestroy(igrid, rc=rc)
    !if (rc.NE.ESMF_SUCCESS) return


    ! if present, delete
    if (present(bundle2)) then

        call ESMF_FieldBundleDestroy(bundle2, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

    endif

    ! if present, delete
    if (present(bundle3)) then

        call ESMF_FieldBundleDestroy(bundle3, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

    endif

    ! if present, delete
    if (present(bundle4)) then

        call ESMF_FieldBundleDestroy(bundle4, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

    endif

    ! if present, delete
    if (present(bundle5)) then

        call ESMF_FieldBundleDestroy(bundle5, rc=rc)
        if (rc.NE.ESMF_SUCCESS) return

    endif

    ! do this last.
    !call ESMF_IGridDestroy(igrid, rc=rc)
    !if (rc.NE.ESMF_SUCCESS) return

    if (present(rc)) rc = ESMF_SUCCESS
    return

end subroutine FieldBundleCleanup


!------------------------------------------------------------------------------
!
! Create a field.  All args but the name can be defaulted.
!  Only 1 of the two data values can be specified; it sets the data type
!  as well as the data value.  If neither specified, R8 is default.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateDataField"
function CreateDataField(name, igrid, layout, relloc, r4value, r8value, rc)
  type(ESMF_Field) :: CreateDataField

  character(len=*), intent(in) :: name                ! field name
  type(ESMF_IGrid), intent(in), optional :: igrid       ! if set, igrid to use
  type(ESMF_DELayout), intent(in), optional :: layout ! if set, layout to use
  type(ESMF_RelLoc), intent(in), optional :: relloc   ! if set, relloc to use
  real(ESMF_KIND_R4), intent(in), optional :: r4value ! if set, initial value
  real(ESMF_KIND_R8), intent(in), optional :: r8value ! if set, initial value
  integer, intent(out), optional :: rc                ! return code

  type(ESMF_IGrid) :: thisigrid
  type(ESMF_DELayout) :: thislayout
  type(ESMF_RelLoc) :: thisrelloc
  real(ESMF_KIND_R4) :: thisr4data
  real(ESMF_KIND_R8) :: thisr8data
  type(ESMF_ArraySpec) :: as
  type(ESMF_VM) :: vm
  integer :: status
  logical :: use_r8
  real(ESMF_KIND_R4), pointer :: r4data(:,:)
  real(ESMF_KIND_R8), pointer :: r8data(:,:)

  ! TODO: what about making sure the function return is null?
  if (present(rc)) rc = ESMF_FAILURE

  ! Set defaults here based on whether the arguments are present or not.

  ! default layout
  if (present(layout)) then
      thislayout = layout
  else
      call ESMF_VMGetGlobal(vm, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      thislayout = ESMF_DELayoutCreate(vm, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
  endif

  ! default igrid
  if (.not. present(igrid)) then
      thisigrid = ESMF_IGridCreateHorzXYUni(counts=(/100, 200/), &
                               minGlobalCoordPerDim=(/0.0_ESMF_KIND_R8, &
                                                      0.0_ESMF_KIND_R8/), &
                               maxGlobalCoordPerDim=(/180.0_ESMF_KIND_R8, &
                                                      360.0_ESMF_KIND_R8/), &
                               horzStagger=ESMF_IGRID_HORZ_STAGGER_B_NE, &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      call ESMF_IGridDistribute(thisigrid, delayout=thislayout, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
  else
      thisigrid = igrid
  endif

  ! default relloc
  if (present(relloc)) then
      thisrelloc = relloc
  else
      thisrelloc = ESMF_CELL_CENTER
  endif

  ! default data - both type and value.  only one of r4value or r8value
  ! can be specified.  if neither, default is r8.
  if (present(r4value) .and. present(r8value)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                               "Cannot specify both *4 and *8 values", &
                                ESMF_CONTEXT, rc)
      goto 10
  else if (present(r4value)) then
      use_r8 = .false.
      thisr4data = r4value
  else if (present(r8value)) then
      use_r8 = .true.
      thisr8data = r8value
  else ! neither specified, default to r8
      use_r8 = .true.
      thisr8data = 3.14159
  endif


  ! fixed items:  2d array, data type real, halo width of 2.
  if (use_r8) then
      call ESMF_ArraySpecSet(as, rank=2, &
                             typekind=ESMF_TYPEKIND_R8, rc=status)
  else
      call ESMF_ArraySpecSet(as, rank=2, &
                             typekind=ESMF_TYPEKIND_R4, rc=status)
  endif
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  ! make the new field
  CreateDataField = ESMF_FieldCreate(igrid=thisigrid, arrayspec=as, &
                                     horzRelloc=thisrelloc, haloWidth=2, &
                                     name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  ! initialize the data field
  if (use_r8) then
      call ESMF_FieldGetDataPointer(CreateDataField, r8data, ESMF_DATA_REF, rc=rc)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
 
      r8data(:,:) = thisr8data
  else 
      call ESMF_FieldGetDataPointer(CreateDataField, r4data, ESMF_DATA_REF, rc=rc)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
 
      r4data(:,:) = thisr4data
  endif
  

10 continue
  ! if coming here because of error, rc is already set to the error code.
  ! if falling thru the code, rc is was set to success by logerr.

end function CreateDataField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateLayout"
function CreateLayout(which, rc)
  type(ESMF_DELayout) :: CreateLayout

  integer, intent(in), optional :: which         ! optional layout type
  integer, intent(out), optional :: rc           ! return code

  type(ESMF_VM) :: vm
  integer :: npets, status, thiswhich


  if (present(rc)) rc = ESMF_FAILURE

  call ESMF_VMGetGlobal(vm, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  call ESMF_VMGet(vm, petCount=npets, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  ! if running with only 1 PET, this is your only option.
  if (npets .eq. 1) then
      CreateLayout = ESMF_DELayoutCreate(vm, (/ 1, 1 /), rc=status)

  else
      ! if not specified, use the default case: npets by 1
      if (.not.present(which)) then
          thiswhich = 0
      else
          thiswhich = which
      endif

      select case (thiswhich)
      case (1)
        ! npets must be even
        if (modulo(npets, 2) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=status)
        endif

      case (2)
        ! npets must be even
        if (modulo(npets, 2) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=status)
        endif

      case (3)
        ! npets must be evenly divisible by 4
        if (modulo(npets, 4) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets/4, 4 /), rc=status)
        endif
 
      case (4)
        ! npets must be evenly divisible by 4
        if (modulo(npets, 4) .ne. 0) then
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)
        else
          CreateLayout = ESMF_DELayoutCreate(vm, (/ 4, npets/4 /), rc=status)
        endif

      case (5)
          CreateLayout = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=status)

      case default
          CreateLayout = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=status)

      end select
  endif
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateLayout


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateIGrid"
function CreateIGrid(which, layout, rc)
  type(ESMF_IGrid) :: CreateIGrid

  integer, intent(in), optional :: which               ! optional igrid type
  type(ESMF_DELayout), intent(in), optional :: layout  ! layout to use
  integer, intent(out), optional :: rc                 ! return code

  type(ESMF_VM) :: vm
  type(ESMF_DELayout) :: thislayout
  real (ESMF_KIND_R8), dimension(2) :: mincoords1, maxcoords1
  real (ESMF_KIND_R8), dimension(2) :: mincoords2, maxcoords2
  integer :: npets, status, thiswhich


  if (present(rc)) rc = ESMF_FAILURE

  if (present(layout)) then
      thislayout = layout
  else
      call ESMF_VMGetGlobal(vm, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
    
      call ESMF_VMGet(vm, petCount=npets, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10
    
      thislayout = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=status)
   endif

   ! if not specified, use a default igrid spacing
   if (.not.present(which)) then
       thiswhich = 0
   else
       thiswhich = which
   endif

   ! used below in cases 1-4
   mincoords1 = (/  0.0,  0.0 /)
   maxcoords1 = (/ 180.0, 360.0 /)

   ! used below in cases 11-14
   mincoords2 = (/  0.0,  0.0 /)
   maxcoords2 = (/ 120.0, 300.0 /)

   select case (thiswhich)
   case (1)
      CreateIGrid = ESMF_IGridCreateHorzXYUni((/100, 200/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_IGRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (2)
      CreateIGrid = ESMF_IGridCreateHorzXYUni((/300, 500/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_IGRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (3)
      CreateIGrid = ESMF_IGridCreateHorzXYUni((/10, 20/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_IGRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (4)
      CreateIGrid = ESMF_IGridCreateHorzXYUni((/90, 180/), &
                                     mincoords1, maxcoords1, &
                                     horzStagger=ESMF_IGRID_HORZ_STAGGER_B_NE, &
                                     rc=status)

    case (11)
      CreateIGrid  = ESMF_IGridCreateHorzXYUni((/ 90, 180 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                                        rc=status)

    case (12)
      CreateIGrid  = ESMF_IGridCreateHorzXYUni((/ 400, 700 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                                        rc=status)

    case (13)
      CreateIGrid  = ESMF_IGridCreateHorzXYUni((/ 15, 80 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                                        rc=status)

    case (14)
      CreateIGrid  = ESMF_IGridCreateHorzXYUni((/ 900, 1800 /), &
                                        mincoords2, maxcoords2, &
                                        horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                                        rc=status)

    case default
      CreateIGrid = ESMF_IGridCreateHorzXYUni((/100, 200/), &
                                            mincoords1, maxcoords1, &
                                            rc=status)

  end select
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10


  ! distribute the igrid across the PETs
  call ESMF_IGridDistribute(CreateIGrid, delayout=thislayout, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10
   

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateIGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateLatLonIGrid"
function CreateLatLonIGrid(nx, ny, nz, xde, yde, name, data_xde, data_yde, rc)
  type(ESMF_IGrid) :: CreateLatLonIGrid

  integer, intent(in) :: nx, ny, nz               ! igrid size
  integer, intent(in) :: xde, yde                 ! layout counts
  character(len=*), intent(in), optional :: name  ! igrid name
  integer, intent(in), optional :: data_xde       ! if set, des with data
  integer, intent(in), optional :: data_yde       ! ditto
  integer, intent(out), optional :: rc            ! return code

  type(ESMF_IGrid) :: igrid
  type(ESMF_VM) :: vm
  type(ESMF_DELayout) :: thislayout
  real (ESMF_KIND_R8), dimension(2) :: mincoords1, maxcoords1
  real (ESMF_KIND_R8), dimension(:), allocatable :: deltas
  integer, dimension(2) :: counts
  integer :: npets, status, emptyx, emptyy, fullx, fully
  integer, allocatable :: xdecounts(:), ydecounts(:)


  if (present(rc)) rc = ESMF_FAILURE

  call ESMF_VMGetGlobal(vm, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10
    
  call ESMF_VMGet(vm, petCount=npets, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10
    
  if (npets .ge. (xde * yde)) then 
    thislayout = ESMF_DELayoutCreate(vm, (/ xde, yde /), rc=status)
  else if (npets .eq. 1) then
    thislayout = ESMF_DELayoutCreate(vm, (/ 1, 1 /), rc=status)
  else
    print *, "error - cannot support requested number of DEs"
    print *, "number of PETs = ", npets
    print *, "xde, yde, xde*yde = ", xde, yde, xde*yde
    !CreateLatLonIGrid = ESMF_IGridCreateEmpty()
    !call ESMF_IGridDestroy(CreateLatLonIGrid)
    rc = ESMF_FAILURE
    return
  endif

  ! fixed
  mincoords1 = (/   0.0, -90.0 /)
  maxcoords1 = (/ 360.0,  90.0 /)

  counts(1) = nx
  counts(2) = ny

  allocate(deltas(nz), stat=rc)
  if (ESMF_LogMsgFoundAllocError(status, "Allocating delta array", &
                                       ESMF_CONTEXT, rc)) return

  deltas(:) = 100.0

  igrid = ESMF_IGridCreateHorzLatLonUni(counts, &
                                      mincoords1, maxcoords1, &
                                      horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                                      dimNames=(/ "lon", "lat" /), &
                                      dimUnits=(/ "deg", "deg" /), &
                                      periodic=(/ ESMF_TRUE, ESMF_FALSE /), &
                                      name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10


  call ESMF_IGridAddVertHeight(igrid, deltas, &
                              vertstagger=ESMF_IGRID_VERT_STAGGER_CENTER, &
                              rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10


  ! distribute the igrid across the PETs.   allow the option of only
  ! having data on a (logically rectangular) subset of the DEs.
  if (present(data_xde) .or. present(data_yde)) then

    emptyx = 0
    emptyy = 0
    fullx = xde
    fully = yde

    ! do some basic error checks and then figure out how many x DEs are empty
    if (present(data_xde)) then
       if ((data_xde .le. 0) .or. (data_xde .gt. xde)) then

         call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                               "data_xde must be > 0 and <= xde", &
                                ESMF_CONTEXT, rc)
         goto 10
       else
        emptyx = xde - data_xde
        fullx = data_xde
       endif
    endif

    ! same thing for y
    if (present(data_yde)) then
       if ((data_yde .le. 0) .or. (data_yde .gt. yde)) then

         call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                               "data_yde must be > 0 and <= yde", &
                                ESMF_CONTEXT, rc)
          goto 10
       else
        emptyy = yde - data_yde
        fully = data_yde
       endif
    endif

    allocate(xdecounts(xde), ydecounts(yde), stat=status)
    if (ESMF_LogMsgFoundAllocError(status, "Allocating decount arrays", &
                                   ESMF_CONTEXT, rc)) return

    ! default to 0 data items per DE, then divide up the data counts 
    ! onto the requested subset.  always start at DE (1,1).
    xdecounts = 0
    ydecounts = 0
    
    xdecounts(1:fullx) = nx / fullx
    ydecounts(1:fully) = ny / fully

    ! and add remainders to the last de with data.
    xdecounts(fullx) = xdecounts(fullx) + mod(nx, fullx)
    ydecounts(fully) = ydecounts(fully) + mod(ny, fully)


    ! debug
    ! print *, xde, yde, nx, ny, xdecounts, ydecounts, emptyx, emptyy
    call ESMF_IGridDistribute(igrid, delayout=thislayout, &
                             countsPerDEDim1=xdecounts, &
                             countsPerDEDim2=ydecounts, &
                             rc=status)
  else
    call ESMF_IGridDistribute(igrid, delayout=thislayout, rc=status)
  endif 
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

  CreateLatLonIGrid = igrid

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point
  if (allocated(deltas))    deallocate(deltas,   stat=status)
  if (allocated(xdecounts)) deallocate(xdecounts, stat=status)
  if (allocated(ydecounts)) deallocate(ydecounts, stat=status)

end function CreateLatLonIGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateEmptyDataField"
function CreateEmptyDataField(name, rc)
  type(ESMF_Field) :: CreateEmptyDataField

  character(len=*), intent(in) :: name
  integer, intent(out) :: rc

  integer :: status

  rc = ESMF_FAILURE
  CreateEmptyDataField = ESMF_FieldCreateNoData(name=name, rc=status)
  if (ESMF_LogMsgFoundError(status, &
                            ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rc)) goto 10

10 continue
  ! rc will have been set by the call to logerr; 
  ! just return at this point

end function CreateEmptyDataField

!------------------------------------------------------------------------------

end module
