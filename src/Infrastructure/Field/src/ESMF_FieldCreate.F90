#pragma GCC set_debug_pwd "/Users/nancy/esmf/esmf_cvs/mod/modg/Darwin.absoft.32.default"
! $Id: ESMF_FieldCreate.F90,v 1.1 2004/04/19 22:01:50 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
! ESMF FieldCreate module
      module ESMF_FieldCreateMod
!
!==============================================================================
!
! This file contains the Field class methods which are automatically
! generated from macros to handle the type/kind/rank overloading.
! See ESMF_Field.F90 for non-macroized functions and subroutines.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_ArraySpecMod
      use ESMF_LocalArrayMod
      use ESMF_DataMapMod
      use ESMF_newDELayoutMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_ArrayCreateMod
      use ESMF_ArrayCommMod
      use ESMF_FieldMod
      use ESMF_FieldGetMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FieldCreate

!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldCreate.F90,v 1.1 2004/04/19 22:01:50 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldCreate - Create a new Field with data
!
! !INTERFACE:
      interface ESMF_FieldCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_FieldCreateNew
        module procedure ESMF_FieldCreateFromArray
        module procedure ESMF_FieldCreateRemap

      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_FieldCreateDPtr1DI1 
 module procedure ESMF_FieldCreateDPtr1DI2 
 module procedure ESMF_FieldCreateDPtr1DI4 
 module procedure ESMF_FieldCreateDPtr1DI8 
 module procedure ESMF_FieldCreateDPtr2DI1 
 module procedure ESMF_FieldCreateDPtr2DI2 
 module procedure ESMF_FieldCreateDPtr2DI4 
 module procedure ESMF_FieldCreateDPtr2DI8 
 module procedure ESMF_FieldCreateDPtr3DI1 
 module procedure ESMF_FieldCreateDPtr3DI2 
 module procedure ESMF_FieldCreateDPtr3DI4 
 module procedure ESMF_FieldCreateDPtr3DI8 
 module procedure ESMF_FieldCreateDPtr4DI1 
 module procedure ESMF_FieldCreateDPtr4DI2 
 module procedure ESMF_FieldCreateDPtr4DI4 
 module procedure ESMF_FieldCreateDPtr4DI8 
 module procedure ESMF_FieldCreateDPtr5DI1 
 module procedure ESMF_FieldCreateDPtr5DI2 
 module procedure ESMF_FieldCreateDPtr5DI4 
 module procedure ESMF_FieldCreateDPtr5DI8 
 module procedure ESMF_FieldCreateDPtr6DI1 
 module procedure ESMF_FieldCreateDPtr6DI2 
 module procedure ESMF_FieldCreateDPtr6DI4 
 module procedure ESMF_FieldCreateDPtr6DI8 
 module procedure ESMF_FieldCreateDPtr7DI1 
 module procedure ESMF_FieldCreateDPtr7DI2 
 module procedure ESMF_FieldCreateDPtr7DI4 
 module procedure ESMF_FieldCreateDPtr7DI8 
 module procedure ESMF_FieldCreateDPtr1DR4 
 module procedure ESMF_FieldCreateDPtr1DR8 
 module procedure ESMF_FieldCreateDPtr2DR4 
 module procedure ESMF_FieldCreateDPtr2DR8 
 module procedure ESMF_FieldCreateDPtr3DR4 
 module procedure ESMF_FieldCreateDPtr3DR8 
 module procedure ESMF_FieldCreateDPtr4DR4 
 module procedure ESMF_FieldCreateDPtr4DR8 
 module procedure ESMF_FieldCreateDPtr5DR4 
 module procedure ESMF_FieldCreateDPtr5DR8 
 module procedure ESMF_FieldCreateDPtr6DR4 
 module procedure ESMF_FieldCreateDPtr6DR8 
 module procedure ESMF_FieldCreateDPtr7DR4 
 module procedure ESMF_FieldCreateDPtr7DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_FieldCreateEPtr1DI1 
 module procedure ESMF_FieldCreateEPtr1DI2 
 module procedure ESMF_FieldCreateEPtr1DI4 
 module procedure ESMF_FieldCreateEPtr1DI8 
 module procedure ESMF_FieldCreateEPtr2DI1 
 module procedure ESMF_FieldCreateEPtr2DI2 
 module procedure ESMF_FieldCreateEPtr2DI4 
 module procedure ESMF_FieldCreateEPtr2DI8 
 module procedure ESMF_FieldCreateEPtr3DI1 
 module procedure ESMF_FieldCreateEPtr3DI2 
 module procedure ESMF_FieldCreateEPtr3DI4 
 module procedure ESMF_FieldCreateEPtr3DI8 
 module procedure ESMF_FieldCreateEPtr4DI1 
 module procedure ESMF_FieldCreateEPtr4DI2 
 module procedure ESMF_FieldCreateEPtr4DI4 
 module procedure ESMF_FieldCreateEPtr4DI8 
 module procedure ESMF_FieldCreateEPtr5DI1 
 module procedure ESMF_FieldCreateEPtr5DI2 
 module procedure ESMF_FieldCreateEPtr5DI4 
 module procedure ESMF_FieldCreateEPtr5DI8 
 module procedure ESMF_FieldCreateEPtr6DI1 
 module procedure ESMF_FieldCreateEPtr6DI2 
 module procedure ESMF_FieldCreateEPtr6DI4 
 module procedure ESMF_FieldCreateEPtr6DI8 
 module procedure ESMF_FieldCreateEPtr7DI1 
 module procedure ESMF_FieldCreateEPtr7DI2 
 module procedure ESMF_FieldCreateEPtr7DI4 
 module procedure ESMF_FieldCreateEPtr7DI8 
 module procedure ESMF_FieldCreateEPtr1DR4 
 module procedure ESMF_FieldCreateEPtr1DR8 
 module procedure ESMF_FieldCreateEPtr2DR4 
 module procedure ESMF_FieldCreateEPtr2DR8 
 module procedure ESMF_FieldCreateEPtr3DR4 
 module procedure ESMF_FieldCreateEPtr3DR8 
 module procedure ESMF_FieldCreateEPtr4DR4 
 module procedure ESMF_FieldCreateEPtr4DR8 
 module procedure ESMF_FieldCreateEPtr5DR4 
 module procedure ESMF_FieldCreateEPtr5DR8 
 module procedure ESMF_FieldCreateEPtr6DR4 
 module procedure ESMF_FieldCreateEPtr6DR8 
 module procedure ESMF_FieldCreateEPtr7DR4 
 module procedure ESMF_FieldCreateEPtr7DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides an entry point for methods that create a complete
! {\tt ESMF\_Field}. These method all contain an {\tt ESMF\_Grid} and
! {\tt ESMF\_Data}. The variations allow the user to specify the data
! using either a Fortran array or an {\tt ESMF\_Array}.
!
      end interface
!EOPI

!
!==============================================================================
!
      contains
!
!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a new Field

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreate()
      function ESMF_FieldCreateNew(grid, arrayspec, allocflag, horzRelloc, &
                                   vertRelloc, haloWidth, datamap, name, &
                                   iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNew
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_ArraySpec), intent(in) :: arrayspec
      type(ESMF_AllocFlag), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! An interface function to {\tt ESMF\_FieldCreate()}.
! Create an {\tt ESMF\_Field} and allocate space internally for a
! gridded {\tt ESMF\_Array}. Return a new {\tt ESMF\_Field}.
!
! The arguments are:
! \begin{description}
! \item [grid]
! Pointer to an {\tt ESMF\_Grid} object.
! \item [arrayspec]
! {\tt ESMF\_Data} specification.
! \item [{[allocflag]}]
! Whether to allocate space for the array. Default is
! {\tt ESMF\_DO\_ALLOCATE}. Other option is {\tt ESMF\_NO\_ALLOCATE}.
! \item [{[horzRelloc]}]
! Relative location of data per grid cell/vertex in the horizontal
! grid.
! \item [{[vertRelloc]}]
! Relative location of data per grid cell/vertex in the vertical grid.
! \item [{[haloWidth]}]
! Maximum halo depth along all edges. Default is 0.
! \item [{[datamap]}]
! Describes the mapping of data to the {\tt ESMF\_Grid}.
! \item [{[name]}]
! {\tt Field} name.
! \item [{[iospec]}]
! I/O specification.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.1, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype ! Pointer to new field
      integer :: status ! Error status
      logical :: rcpresent ! Return code present

      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNew%ftypep)

      ! Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_FieldCreateNew: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, &
                                  horzRelloc, vertRelloc, haloWidth, &
                                  datamap, name, iospec, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_FieldCreateNew: Field construct new asp"
        return
      endif

      ! Set return values.
      ESMF_FieldCreateNew%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field from an existing ESMF Array

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreate()
      function ESMF_FieldCreateFromArray(grid, array, copyflag, horzRelloc, &
                                         vertRelloc, haloWidth, datamap, name, &
                                         iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateFromArray
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(in) :: array
      type(ESMF_CopyFlag), intent(in), optional :: copyflag
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap
      character (len = *), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! An interface function to {\tt ESMF\_FieldCreate()}.
! This version of creation assumes the data exists already and is being
! passed in through an {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item [grid]
! Pointer to an {\tt ESMF\_Grid} object.
! \item [array]
! Includes data specification and allocated memory.
! \item [{[copyflag]}]
! Indicates whether to reference the array or make a
! copy of it. Valid values are {\tt ESMF\_DATA\_COPY} and
! {\tt ESMF\_DATA\_REF}, respectively.
! \item [{[horzRelloc]}]
! Relative location of data per grid cell/vertex in the horizontal
! grid.
! \item [{[vertRelloc]}]
! Relative location of data per grid cell/vertex in the vertical grid.
! \item [{[haloWidth]}]
! Maximum halo depth along all edges. Default is 0.
! \item [{[datamap]}]
! Describes the mapping of data to the {\tt ESMF\_Grid}.
! \item [{[name]}]
! {\tt Field} name.
! \item [{[iospec]}]
! I/O specification.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.2, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype ! Pointer to new field
      integer :: status ! Error status
      logical :: rcpresent ! Return code present

      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateFromArray%ftypep)

      ! Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_FieldCreateFromArray: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize field internals.
      call ESMF_FieldConstruct(ftype, grid, array, horzRelloc, &
                                       vertRelloc, haloWidth, datamap, name, &
                                       iospec, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_FieldCreateNew: Field construct NewArray"
        return
      endif


      ! Set return values.
      ESMF_FieldCreateFromArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldCreate - Create a Field by remapping another Field

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreate()
      function ESMF_FieldCreateRemap(srcfield, grid, horzRelloc, vertRelloc, &
                                     haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateRemap
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc
      integer, intent(in), optional :: haloWidth
      type(ESMF_DataMap), intent(in), optional :: datamap
      character (len = *), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
! An interface function to {\tt ESMF\_FieldCreate()}.
! Remaps data between an existing {\tt ESMF\_Grid} on a source {\tt ESMF\_Field}
! and a new {\tt ESMF\_Grid}. The {\tt ESMF\_Grid} is referenced by the
! new {\tt ESMF\_Field}. Data is copied.
!
!
! The arguments are:
! \begin{description}
! \item [srcfield]
! Source {\tt ESMF\_Field}.
! \item [grid]
! {\tt ESMF\_Grid} of source {\tt ESMF\_Field}.
! \item [horzRelLoc]
! Relative location of data per grid cell/vertex in the horizontal
! grid.
! \item [vertRelLoc]
! Relative location of data per grid cell/vertex in the vertical grid.
! \item [{[halowidth]}]
! Halo width.
! \item [{[datamap]}]
! {\tt ESMF\_DataMap}
! \item [{[name]}]
! {\tt ESMF\_Field} name.
! \item [{[iospec]}]
! {\tt ESMF\_Field} {\tt ESMF\_IOSpec}.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.1.5, FLD1.5.1, FLD1.6.1


      type(ESMF_FieldType), pointer :: ftype ! Pointer to new field
      integer :: status ! Error status
      logical :: rcpresent ! Return code present

      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateRemap%ftypep)

      ! Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_FieldCreateRemap: Allocate"
        return
      endif

      ! TODO: Insert field construction method

      ! Set return values.
      ESMF_FieldCreateRemap%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateRemap

!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field using an existing Fortran data pointer 
 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateDPtr<rank><type><kind>(grid, fptr, copyflag, & 
! horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateDPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_Grid), intent(in) :: grid 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! type(ESMF_CopyFlag), intent(in) :: copyflag 
! type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
! type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! integer, intent(in), optional :: haloWidth 
! type(ESMF_DataMap), intent(in), optional :: datamap 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field} and associate the data in the Fortran 
! array with the {\tt ESMF\_Field}. Return a new {\tt ESMF\_Field}. 
! 
! The arguments are: 
! \begin{description} 
! \item [grid] 
! Pointer to an {\tt ESMF\_Grid} object. 
! \item [fptr] 
! A Fortran array pointer which must be already allocated and the 
! proper size for this portion of the grid. 
! \item [copyflag] 
! Whether to copy the existing data space or reference directly. Default 
! is {\tt ESMF\_DATA\_COPY}. Other option is {\tt ESMF\_DATA\_REF}. 
! \item [{[horzRelloc]}] 
! Relative location of data per grid cell/vertex in the horizontal grid. 
! \item [{[vertRelloc]}] 
! Relative location of data per grid cell/vertex in the vertical grid. 
! \item [{[haloWidth]}] 
! Maximum halo depth along all edges. Default is 0. 
! \item [{[datamap]}] 
! Describes the mapping of data to the {\tt ESMF\_Grid}. 
! \item [{[name]}] 
! {\tt Field} name. 
! \item [{[iospec]}] 
! I/O specification. 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr1DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr1DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr1DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr1DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr1DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr1DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr1DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr1DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr1DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr1DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr1DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr1DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr2DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr2DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr2DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr2DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr2DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr2DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr2DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr2DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr2DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr2DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr2DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr2DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr3DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr3DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr3DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr3DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr3DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr3DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr3DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr3DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr3DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr3DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr3DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr3DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr4DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr4DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr4DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr4DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr4DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr4DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr4DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr4DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr4DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr4DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr4DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr4DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr5DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr5DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr5DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr5DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr5DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr5DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr5DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr5DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr5DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr5DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr5DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr5DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr6DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr6DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr6DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr6DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr6DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr6DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr6DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr6DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr6DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr6DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr6DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr6DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr7DI1(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr7DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr7DI1%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr7DI2(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr7DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr7DI2%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr7DI4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr7DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr7DI4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr7DI8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr7DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr7DI8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr1DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr1DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr1DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr1DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr1DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr1DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr2DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr2DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr2DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr2DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr2DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr2DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr3DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr3DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr3DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr3DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr3DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr3DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr4DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr4DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr4DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr4DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr4DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr4DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr5DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr5DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr5DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr5DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr5DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr5DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr6DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr6DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr6DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr6DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr6DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr6DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr7DR4(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr7DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr7DR4%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateDPtr7DR8(grid, fptr, copyflag, & 
 horzRelloc, vertRelloc, haloWidth, datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateDPtr7DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in) :: copyflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type(ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not associated, and fail if so. 
 if (.not.associated(fptr)) then 
 print *, "Error: Data Pointer must already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, array, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ESMF_FieldCreateDPtr7DR8%ftypep => ftype 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateDPtr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldCreate - Create a Field using an unallocated Fortran data pointer 
 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldCreate() 
! function ESMF_FieldCreateEPtr<rank><type><kind>(grid, fptr, allocflag, & 
! horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
! datamap, name, iospec, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_Field) :: ESMF_FieldCreateEPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! type(ESMF_Grid), intent(in) :: grid 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! type(ESMF_AllocFlag), intent(in), optional :: allocflag 
! integer, intent(in), optional :: haloWidth 
! integer, dimension(:), intent(in), optional :: lbounds 
! integer, dimension(:), intent(in), optional :: ubounds 
! type(ESMF_DataMap), intent(in), optional :: datamap 
! character (len=*), intent(in), optional :: name 
! type(ESMF_IOSpec), intent(in), optional :: iospec 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Field}, allocate necessary data space, and return 
! with the Fortran array pointer initialized to point to the data space. 
! Function return value is the new {\tt ESMF\_Field}. 
! 
! The arguments are: 
! \begin{description} 
! \item [grid] 
! Pointer to an {\tt ESMF\_Grid} object. 
! \item [fptr] 
! A Fortran array pointer which must be unallocated but of the 
! proper rank, type, and kind for the data to be associated with 
! this {\tt EWSF\_Field}. 
! \item [{[allocflag]}] 
! Whether to allocate space for the array. Default is 
! {\tt ESMF\_DO\_ALLOCATE}. Other option is {\tt ESMF\_NO\_ALLOCATE}. 
! \item [{[horzRelloc]}] 
! Relative location of data per grid cell/vertex in the horizontal grid. 
! \item [{[vertRelloc]}] 
! Relative location of data per grid cell/vertex in the vertical grid. 
! \item [{[haloWidth]}] 
! Maximum halo depth along all edges. Default is 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length 
! as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length 
! as the rank. 
! \item [{[datamap]}] 
! Describes the mapping of data to the {\tt ESMF\_Grid}. 
! \item [{[name]}] 
! {\tt Field} name. 
! \item [{[iospec]}] 
! I/O specification. 
! \item [{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr1DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr1DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 1, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr1DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr1DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr1DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr1DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 1, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr1DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr1DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr1DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr1DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 1, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr1DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr1DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr1DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr1DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 1, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr1DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr1DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr2DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr2DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 2, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr2DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr2DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr2DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr2DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 2, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr2DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr2DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr2DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr2DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 2, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr2DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr2DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr2DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr2DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 2, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr2DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr2DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr3DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr3DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 3, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr3DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr3DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr3DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr3DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 3, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr3DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr3DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr3DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr3DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 3, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr3DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr3DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr3DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr3DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 3, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr3DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr3DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr4DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr4DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 4, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr4DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr4DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr4DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr4DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 4, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr4DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr4DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr4DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr4DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 4, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr4DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr4DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr4DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr4DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 4, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr4DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr4DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr5DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr5DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 5, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr5DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr5DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr5DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr5DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 5, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr5DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr5DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr5DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr5DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 5, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr5DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr5DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr5DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr5DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 5, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr5DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr5DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr6DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr6DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 6, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr6DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr6DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr6DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr6DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 6, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr6DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr6DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr6DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr6DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 6, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr6DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr6DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr6DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr6DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 6, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr6DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr6DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr7DI1(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr7DI1 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 7, ESMF_I1, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr7DI1%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr7DI1, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr7DI2(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr7DI2 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 7, ESMF_I2, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr7DI2%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr7DI2, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr7DI4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr7DI4 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 7, ESMF_I4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr7DI4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr7DI4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr7DI8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr7DI8 
 
 type(ESMF_Grid), intent(in) :: grid 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 7, ESMF_I8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr7DI8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr7DI8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr1DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr1DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 1, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr1DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr1DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr1DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr1DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 1, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr1DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr1DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr2DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr2DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 2, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr2DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr2DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr2DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr2DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 2, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr2DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr2DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr3DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr3DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 3, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr3DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr3DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr3DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr3DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 3, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr3DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr3DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr4DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr4DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 4, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr4DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr4DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr4DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr4DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 4, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr4DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr4DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr5DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr5DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 5, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr5DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr5DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr5DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr5DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 5, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr5DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr5DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr6DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr6DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 6, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr6DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr6DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr6DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr6DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 6, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr6DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr6DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr7DR4(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr7DR4 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 7, ESMF_R4, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr7DR4%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr7DR4, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 function ESMF_FieldCreateEPtr7DR8(grid, fptr, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, lbounds, ubounds, & 
 datamap, name, iospec, rc) 
 
 type(ESMF_Field) :: ESMF_FieldCreateEPtr7DR8 
 
 type(ESMF_Grid), intent(in) :: grid 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_AllocFlag), intent(in), optional :: allocflag 
 type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
 type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
! TODO: this should not be lbounds, ubounds - but some global addr flag. 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 type(ESMF_DataMap), intent(in), optional :: datamap 
 character (len=*), intent(in), optional :: name 
 type(ESMF_IOSpec), intent(in), optional :: iospec 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_FieldType), pointer :: ftype ! Pointer to new field 
 type (ESMF_ArraySpec) :: arrayspec ! arrayspec object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer not already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer must not already be associated" 
 return 
 endif 
 
 ! Get rank, type, kind from ptr and initialize arrayspec 
 call ESMF_ArraySpecSet(arrayspec, 7, ESMF_R8, status) 
 
 allocate(ftype, stat=status) 
 ! If error write message and return. 
 if(status .NE. 0) then 
 print *, "ERROR in ESMF_FieldCreate: Allocate" 
 return 
 endif 
 
 ! Construction method allocates and initializes field internals. 
 call ESMF_FieldConstruct(ftype, grid, arrayspec, allocflag, & 
 horzRelloc, vertRelloc, haloWidth, & 
 datamap, name, iospec, status) 
 if(status .NE. ESMF_SUCCESS) then 
 print *, "ERROR in ESMF_FieldCreate: Field construct" 
 return 
 endif 
 
 ! Set return value, and then get pointer back. 
 ESMF_FieldCreateEPtr7DR8%ftypep => ftype 
 call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr7DR8, & 
 fptr, ESMF_DATA_REF, status) 
 
 if (rcpresent) rc = status 
 
 end function ESMF_FieldCreateEPtr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------


      end module ESMF_FieldCreateMod
