! $Id: ESMF_Field.F90,v 1.148 2004/05/26 18:29:35 nscollins Exp $
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
!     ESMF Field module
      module ESMF_FieldMod
!
!==============================================================================
!
! This file contains the Field class definition and all Field
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldMod - Combine physical field metadata, data and grid
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class, which 
! represents a
! single scalar or vector field.  {\tt ESMF\_Field}s associate a metadata 
! description 
! expressed as a set of {\tt ESMF\_Attributes} with a data {\tt ESMF\_Array}, 
! {\tt ESMF\_Grid}, and I/O specification, or {\tt ESMF\_IOSpec}.  
! An {\tt ESMF\_FieldDataMap} describes the 
! relationship of the {\tt ESMF\_Array} to the {\tt ESMF\_Grid}.  
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_ArraySpecMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayDataMapMod
      use ESMF_DELayoutMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_ArrayMod
      use ESMF_ArrayCreateMod
      use ESMF_ArrayCommMod
      use ESMF_FieldDataMapMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private
!------------------------------------------------------------------------------
!     ! ESMF_Access
!
!     ! Internal flag for tracking whether data is attached. 

      type ESMF_Access
      sequence
      private
        integer :: a_type
      end type

      type(ESMF_Access), parameter ::  &
                               ESMF_READWRITE = ESMF_Access(0), &
                               ESMF_READONLY = ESMF_Access(1)

!------------------------------------------------------------------------------
!     ! ESMF_AllocFlag
!
!     ! Interface flag for setting whether Field does the data allocation.

      type ESMF_AllocFlag
      sequence
      private
        integer :: a_type
      end type

      type(ESMF_AllocFlag), parameter ::  &
                               ESMF_DO_ALLOCATE = ESMF_AllocFlag(0), &
                               ESMF_NO_ALLOCATE = ESMF_AllocFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_IndexFlag
!
!     ! Interface flag for setting whether Field data has global index bounds

      type ESMF_IndexFlag
      sequence
      private
        integer :: i_type
      end type

      type(ESMF_IndexFlag), parameter ::  &
                               ESMF_LOCAL_INDEX  = ESMF_IndexFlag(0), &
                               ESMF_GLOBAL_INDEX = ESMF_IndexFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_LocalField
!      
!     ! The LocalField class contains information which is associated with the
!     ! local DE.

      type ESMF_LocalField
      sequence
      !private
   
        type (ESMF_Array) :: localdata           ! local data for this DE
        type (ESMF_ArraySpec) :: arrayspec       ! so field can allocate
        type (ESMF_Mask) :: mask                 ! may belong in Grid
        integer :: rwaccess                      ! reserved for future use
        integer :: accesscount                   ! reserved for future use

      end type

!------------------------------------------------------------------------------
!     ! ESMF_FieldType
      
!     ! Definition of the Field class.  A Field
!     ! is passed back to the user at Field creation.

      type ESMF_FieldType
      sequence
      !private
       
        type (ESMF_Base) :: base             ! base class object
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type (ESMF_Status) :: fieldstatus = ESMF_STATE_UNINIT
        type (ESMF_Status) :: gridstatus = ESMF_STATE_UNINIT
        type (ESMF_Status) :: datastatus = ESMF_STATE_UNINIT
        type (ESMF_GridClass), pointer :: gridp => NULL()  ! for faster access
#else
        type (ESMF_Status) :: fieldstatus
        type (ESMF_Status) :: gridstatus
        type (ESMF_Status) :: datastatus
        type (ESMF_GridClass), pointer :: gridp
#endif
        type (ESMF_Grid) :: grid             ! save to satisfy query routines
        type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_FieldDataMap) :: mapping  ! mapping of array indices to grid
        type (ESMF_IOSpec) :: iospec         ! iospec values
        type (ESMF_Status) :: iostatus       ! if unset, inherit from gcomp

      end type

!------------------------------------------------------------------------------
!     ! ESMF_Field
      
!     ! The Field data structure that is passed between implementation and
!     ! calling languages.

      type ESMF_Field
      sequence
      !private       
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type (ESMF_FieldType), pointer :: ftypep => NULL()
#else
        type (ESMF_FieldType), pointer :: ftypep
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Field, ESMF_Access
      public ESMF_FieldType, ESMF_LocalField  ! for internal lib use only
      public ESMF_AllocFlag, ESMF_NO_ALLOCATE, ESMF_DO_ALLOCATE
      public ESMF_IndexFlag, ESMF_LOCAL_INDEX, ESMF_GLOBAL_INDEX

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

   public ESMF_FieldCreateNoData       ! Create a new Field without data
   public ESMF_FieldDestroy            ! Destroy a Field

   public ESMF_FieldGet                ! Generic Get() routine, replaces others

   public ESMF_FieldGetGlobalGridInfo  ! Return global Grid info
   public ESMF_FieldGetLocalGridInfo   ! Return local Grid info

   public ESMF_FieldGetArray           ! Return the data Array
   public ESMF_FieldGetGlobalDataInfo  ! Return global data info
   public ESMF_FieldGetLocalDataInfo   ! Return local data info

   public ESMF_FieldSetGrid            ! Set a Grid (may regrid if different
                                       !   Grid is already present)
   public ESMF_FieldSetArray           ! Set a data Array in a Field
   public ESMF_FieldSetDataValues      ! Set Field data values 

   public ESMF_FieldSetDataMap         ! Set a DataMap (may reorder if different
                                       !   DataMap is already present)

   public ESMF_FieldAddAttribute       ! Set and Get Attributes
   public ESMF_FieldGetAttribute       !  

   public ESMF_FieldGetAttributeCount  ! number of attribs
   public ESMF_FieldGetAttributeInfo   ! get type, length by name or number

   public ESMF_FieldValidate           ! Check internal consistency
   public ESMF_FieldPrint              ! Print contents of a Field
   public ESMF_FieldBoxIntersect       ! Intersect bounding boxes

   public ESMF_FieldWrite              ! Write data and grid from a Field

   public ESMF_FieldConstruct          ! Only public for internal use

!  !subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!  !function ESMF_FieldReadRestart(name, iospec, rc)
!  !subroutine ESMF_FieldWrite(field, subset, iospec, rc)
!  !function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Field.F90,v 1.148 2004/05/26 18:29:35 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldCreateNoData - Create a new Field without data
!
! !INTERFACE:
      interface ESMF_FieldCreateNoData
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldCreateNoDataPtr
        module procedure ESMF_FieldCreateNoArray
        module procedure ESMF_FieldCreateNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for methods that create 
!     an {\tt ESMF\_Field} without allocating or referencing any associated data.
!     The variations allow an {\tt ESMF\_Grid} to be specified or not, and for
!     the data description to be specified or not.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstruct - Construct the internals of a new Field
!
! !INTERFACE:
      interface ESMF_FieldConstruct
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructNew
        module procedure ESMF_FieldConstructNewArray

! !DESCRIPTION:
!     This interface provides an entry point for methods that construct a 
!     complete {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructNoData - Construct the internals of a new empty Field
!
! !INTERFACE:
      interface ESMF_FieldConstructNoData
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructNoDataPtr
        module procedure ESMF_FieldConstructNoArray
        module procedure ESMF_FieldConstructNoGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for {\tt ESMF\_Field} construction 
!     methods that do not allocate or reference any associated data.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAddAttribute  - Set Field Attributes
!
! !INTERFACE:
      interface ESMF_FieldAddAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldAddIntAttr
        module procedure ESMF_FieldAddIntListAttr
        module procedure ESMF_FieldAddRealAttr
        module procedure ESMF_FieldAddRealListAttr
        module procedure ESMF_FieldAddLogicalAttr
        module procedure ESMF_FieldAddLogicalListAttr
        module procedure ESMF_FieldAddCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldGetAttribute  - Get Field Attributes
!
! !INTERFACE:
      interface ESMF_FieldGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldGetIntAttr
        module procedure ESMF_FieldGetIntListAttr
        module procedure ESMF_FieldGetRealAttr
        module procedure ESMF_FieldGetRealListAttr
        module procedure ESMF_FieldGetLogicalAttr
        module procedure ESMF_FieldGetLogicalListAttr
        module procedure ESMF_FieldGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Field}.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldGetAttributeInfo - Get type, count from a Field Attribute
!
! !INTERFACE:
      interface ESMF_FieldGetAttributeInfo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldGetAttrInfoByName
        module procedure ESMF_FieldGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_Field}.
 
!EOPI
      end interface

!
!
!
!==============================================================================
!
      contains
!
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddIntAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set an integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddIntAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an integer attribute to an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The integer value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, 1, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddIntAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddIntListAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set an integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddIntListAttr(field, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an integer list attribute to an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The integer values of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif
  
      limit = size(value)
      if (count > limit) then
          print *, "ESMF_FieldAddAttribute: count longer than value list"
          return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, count, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddIntListAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddRealAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set a real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddRealAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a real attribute to an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The real value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, 1, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddRealAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddRealListAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set a real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddRealListAttr(field, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a real list attribute to an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The real values of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          print *, "ESMF_FieldAddAttribute: count longer than value list"
          return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, count, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddRealListAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddLogicalAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddLogicalAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an logical attribute to an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The logical true/false value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, 1, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddLogicalAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddLogicalListAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddLogicalListAttr(field, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an logical list attribute to an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          print *, "ESMF_FieldAddAttribute: count longer than value list"
          return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, count, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddLogicalListAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAddCharAttr()"

!BOP
! !IROUTINE: ESMF_FieldAddAttribute - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAddAttribute()
      subroutine ESMF_FieldAddCharAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character attribute to an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The character value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetChar(field%ftypep%base, name, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldAddAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAddCharAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoDataPtr()"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated data buffer

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoDataPtr(grid, arrayspec, horzRelloc, vertRelloc, &
                                        haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoDataPtr   
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_ArraySpec), intent(in) :: arrayspec    
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap    
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreateNoData()}.
!     Creates an {\tt ESMF\_Field} in its entirety except for the assignment
!     or allocation of an associated raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoDataPtr%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in FieldCreateNoDataPtr: Allocate"
        return
      endif 

      ! Call construction method to build field internals.
      call ESMF_FieldConstructNoDataPtr(ftype, grid, arrayspec, horzRelloc, &
                                       vertRelloc, haloWidth, datamap, name, &
                                       iospec, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldCreateNoDataPtr: Field construct NoBuf"
        return
      endif 

      ! Set return values.
      ESMF_FieldCreateNoDataPtr%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoDataPtr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoArray()"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated Array object

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoArray(grid, horzRelloc, vertRelloc, &
                                       haloWidth, datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoArray 
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreateNoData()}.
!     This version of {\tt ESMF\_FieldCreate} builds an {\tt ESMF\_Field} 
!     and depends on a later call to add an {\tt ESMF\_Array} to it.  
!
!     The arguments are:
!     \begin{description}
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoArray%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in FieldCreateNoArray: Allocate"
        return
      endif 

      ! Call field construction method
      call ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, vertRelloc, &
                                      haloWidth, datamap, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNoArray: Construct"
        return
      endif 

      ! Set return values.
      ESMF_FieldCreateNoArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoGridArray()"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no Grid or Array

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoGridArray(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoGridArray 
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreateNoData()}.
!     This version of {\tt ESMF\_FieldCreate} builds an empty {\tt ESMF\_Field} 
!     and depends on later calls to add an {\tt ESMF\_Grid} and {\tt ESMF\_Array} to 
!     it.  
!
!     The arguments are:
!     \begin{description}
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      
      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      nullify(ftype)
      nullify(ESMF_FieldCreateNoGridArray%ftypep)

      ! Initialize return code   
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      allocate(ftype, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .NE. 0) then 
        print *, "ERROR in ESMF_FieldCreateNoGridArray: Allocate"
        return
      endif 

      ! Call field construction method
      call ESMF_FieldConstructNoGridArray(ftype, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldCreateNoGridArray: Construct"
        return
      endif 

      ! Set return values.
      ESMF_FieldCreateNoGridArray%ftypep => ftype
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoGridArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestroy()"

!BOP
! !IROUTINE: ESMF_FieldDestroy - Free all resources associated with a Field

! !INTERFACE:
      subroutine ESMF_FieldDestroy(field, rc)
!
! !ARGUMENTS:
      type(ESMF_Field) :: field       
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!     Releases all resources associated with the {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.4

      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! If already destroyed or never created, return ok
      if (.not. associated(field%ftypep)) then
        print *, "FieldDestroy called on uninitialized or destroyed Field"
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all field internals and then free field memory.
      call ESMF_FieldDestruct(field%ftypep, status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldDestroy from ESMF_FieldDestruct"
        return
      endif 
           
      if (associated(field%ftypep)) deallocate(field%ftypep, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if(status .ne. 0) then 
        print *, "ERROR in ESMF_FieldDestroy: Deallocate of Field class"
        return
      endif 
           
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestroy

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGet()"

!BOP
! !IROUTINE: ESMF_FieldGet - Return info associated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGet(field, grid, array, datamap, horzRelloc, &
                               vertRelloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field    
      type(ESMF_Grid), intent(out), optional :: grid     
      type(ESMF_Array), intent(out), optional :: array     
      type(ESMF_FieldDataMap), intent(out), optional :: datamap     
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      character(len=*), intent(out), optional :: name
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Query an {\tt ESMF\_Field} for various things.  All arguments after
!      the {\tt Field} are optional.  To select individual items use the
!      named\_argument=value syntax.
!
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[grid]}]
!           {\tt ESMF\_Grid}.
!     \item [{[array]}]
!           {\tt ESMF\_Array}.
!     \item [{[datamap]}]
!           {\tt ESMF\_FieldDataMap}.
!     \item [{[horzRelloc}]]
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}]
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[name]}]
!           Name of queried item.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.6.2

        type(ESMF_FieldType), pointer :: ftype
        integer :: status

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          print *, "ERROR: Invalid or Destroyed Field"
          return
        endif
 
        ftype => field%ftypep
        if (ftype%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: Field not ready"
          return
        endif

        if (present(grid)) then
            if (ftype%gridstatus .ne. ESMF_STATE_READY) then
              print *, "ERROR: No grid attached to Field"
              return
            endif
            grid = ftype%grid
        endif

        if (present(array)) then
            if (ftype%datastatus .ne. ESMF_STATE_READY) then
              print *, "ERROR: No data attached to Field"
              return
            endif
            array = ftype%localfield%localdata
        endif

        if (present(datamap)) then
            ! TODO: what's the proper test here?  you could have a map w/ no data yet
            !if (ftype%datastatus .ne. ESMF_STATE_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            datamap = ftype%mapping
        endif

        if (present(horzRelloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATE_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            call ESMF_FieldDataMapGet(ftype%mapping, horzRelloc=horzRelloc, rc=status)
            if (status .ne. ESMF_SUCCESS) then
                print *, "ERROR in getting Horizontal RelLoc in ESMF_FieldDataMapGet"
                rc = status
                return
            endif
        endif

        if (present(vertRelloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATE_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            call ESMF_FieldDataMapGet(ftype%mapping, vertRelloc=vertRelloc, rc=status)
            if (status .ne. ESMF_SUCCESS) then
                print *, "ERROR in getting Vertical RelLoc in ESMF_FieldDataMapGet"
                rc = status
                return
            endif
        endif

        if (present(name)) then
            call c_ESMC_GetName(ftype%base, name, status)
            if (status .ne. ESMF_SUCCESS) then
                print *, "ERROR in getting Field name in ESMF_FieldGet"
                rc = status
                return
            endif
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldGet

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetArray()"

!BOP
! !IROUTINE: ESMF_FieldGetArray - Get data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field      
      type(ESMF_Array), intent(out) :: array
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Get data in {\tt ESMF\_Array} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           Field {\tt ESMF\_Array}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Minimal error checking 
      if (.not.associated(field%ftypep)) then
        print *, "ESMF_FieldGetArray: Invalid or Destroyed Field"
        return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATE_READY) then
        print *, "ESMF_FieldGetArray: Field not ready"
        return
      endif

      if (ftypep%datastatus .ne. ESMF_STATE_READY) then
          print *, "ESMF_FieldGetArray: no data associated with field"
          return
      endif

      !call ESMF_StatusString(ftypep%datastatus, str, rc)
      !print *, "getting array data, status = ", trim(str)
      array = ftypep%localfield%localdata
   
      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetIntAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute  - Retrieve an integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetIntAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, 1, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetIntAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetIntListAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve an integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetIntListAttr(field, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer list attribute from an {\tt ESMF\_Field}.
!
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The integer values of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          print *, "ESMF_FieldGetAttribute: count longer than value list"
          return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_INTEGER, count, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetIntListAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetRealAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetRealAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a real attribute from an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, 1, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetRealAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetRealListAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetRealListAttr(field, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The real values of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          print *, "ESMF_FieldGetAttribute: count longer than value list"
          return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_REAL, count, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetRealListAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLogicalAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetLogicalAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an logical attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, 1, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLogicalAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLogicalListAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetLogicalListAttr(field, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an logical list attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The logical values of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          print *, "ESMF_FieldGetAttribute: count longer than value list"
          return
      endif

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
                                    ESMF_DATA_LOGICAL, count, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLogicalListAttr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetCharAttr()"

!BOP
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetCharAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from an {\tt ESMF\_Field}.
!
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetChar(field%ftypep%base, name, value, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttribute"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetCharAttr


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttributeCount()"

!BOP
! !IROUTINE: ESMF_FieldGetAttributeCount - Query the number of Attributes
!
! !INTERFACE:
      subroutine ESMF_FieldGetAttributeCount(field, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [count]
!           The number of attributes on this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetCount(field%ftypep%base, count, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttributeCount"
        return
      endif 

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttributeCount

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByName()"

!BOP
! !IROUTINE: ESMF_FieldGetAttributeInfo - Query Field Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttributeInfo()
      subroutine ESMF_FieldGetAttrInfoByName(field, name, type, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      character(len=*), intent(in) :: name
      type(ESMF_DataType), intent(out), optional :: type
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [type]
!           The type of the Attribute.
!     \item [count]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_DataType) :: localDt
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetAttrInfoName(field%ftypep%base, name, &
                                           localDt, localCount, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttributeInfo"
        return
      endif 

      if (present(type)) type = localDt
      if (present(count)) count = localCount

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByName

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByNum()"

!BOP
! !IROUTINE: ESMF_FieldGetAttributeInfo - Query Field Attributes by number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttributeInfo()
      subroutine ESMF_FieldGetAttrInfoByNum(field, num, name, type, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field  
      integer, intent(in) :: num
      character(len=*), intent(out), optional :: name
      type(ESMF_DataType), intent(out), optional :: type
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [num]
!           The number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [type]
!           Returns the type of the Attribute.
!     \item [count]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_DataType) :: localDt
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetAttrInfoNum(field%ftypep%base, num, &
                                         localName, localDt, localCount, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldGetAttributeInfo"
        return
      endif 

      if (present(name)) name = localName
      if (present(type)) type = localDt
      if (present(count)) count = localCount

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByNum

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGlobalGridInfo()"

!BOPI
! !IROUTINE: ESMF_FieldGetGlobalGridInfo - Get information about the Global Grid
!
! !INTERFACE:
      subroutine ESMF_FieldGetGlobalGridInfo(field, ndim, ncell, nvertex, &
        size, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field      
      integer, intent(out), optional :: ndim            
      integer, intent(out), optional :: ncell            
      integer, intent(out), optional :: nvertex           
      integer, intent(out), optional :: size(:)  
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!     Return global {\tt ESMF\_Grid} information. 
!
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[ndim]}]
!           Number of dimensions.
!     \item [{[ncell]}]
!           Number of cells.
!     \item [{[nvertex]}]
!           Number of vertex.
!     \item [{[size]}]
!           Size of grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS: FLD1.7.2


!
! TODO: code goes here.  This marked BOPI because it isn't implemented yet,
!   and it may be that a lot of the query routines are not actually
!   implemented at the field level - only the most commonly used ones.
!   the rest are to be implemented by GridGet() - the caller gets the grid
!   from the field and then queries the grid directly.
!
        end subroutine ESMF_FieldGetGlobalGridInfo

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalGridInfo()"

!BOPI
! !IROUTINE: ESMF_FieldGetLocalGridInfo - Get information about the Local Grid
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalGridInfo(field, ndim, ncell, nvertex, & 
        size, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field   
      integer, intent(out), optional :: ndim            
      integer, intent(out), optional :: ncell            
      integer, intent(out), optional :: nvertex           
      integer, intent(out), optional :: size(:)  
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Get {\tt ESMF\_Grid} information specific to the local {\tt ESMF\_DE}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[ndim]}]
!           Number of dimensions.
!     \item [{[ncell]}]
!           Number of cells.
!     \item [{[nvertex]}]
!           Number of vertex.
!     \item [{[size]}]
!           Size of grid.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS: FLD1.7.2


!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalGridInfo

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGlobalDataInfo()"

!BOPI
! !IROUTINE: ESMF_FieldGetGlobalDataInfo - Get information about Field Data
!
! !INTERFACE:
      subroutine ESMF_FieldGetGlobalDataInfo(field, size, &
        indexorder, datatype, interleave, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field            
      integer, intent(out), optional :: size(:)        
      integer, dimension(ESMF_MAXDIM), intent(out) :: indexorder 
      type(ESMF_DataType), intent(out) :: datatype
      type(ESMF_Interleave), intent(out) :: interleave   
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Retrieve global {\tt ESMF\_Field} data information.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[size]}]
!           Field size.
!     \item [indexorder]
!           Field index order.
!     \item [datatype]
!           Field data type.
!     \item [interleave]
!           Data may be ESMF\_IL\_BLOCK or ESMF\_IL\_ITEM.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


!
! TODO: code goes here.  BOPI because not implemented yet.
!
        end subroutine ESMF_FieldGetGlobalDataInfo

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalDataInfo()"

!BOPI
! !IROUTINE: ESMF_FieldGetLocalDataInfo - Get information about Field Data
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalDataInfo(field, size, &
        indexorder, datatype, interleave, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field            
      integer, intent(out), optional :: size(:)        
      integer, dimension(ESMF_MAXDIM), intent(out) :: indexorder 
      type(ESMF_DataType), intent(out) :: datatype
      type(ESMF_Interleave), intent(out) :: interleave   
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Retrieve {\tt ESMF\_Field} data information specific to the local
!     {\tt ESMF\_DE}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[size]}]
!           Field size.
!     \item [indexorder]
!           Data index order.
!     \item [datatype]
!           Field data type.
!     \item [interleave]
!           Data may be ESMF\_IL\_BLOCK or ESMF\_IL\_ITEM.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalDataInfo

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldPrint()"

!BOP
! !IROUTINE:  ESMF_FieldPrint - Print the contents of a Field

! !INTERFACE:
      subroutine ESMF_FieldPrint(field, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Routine to print information about a field.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!     \item [{[options]}]
!           Print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

        character(len=ESMF_MAXSTR) :: name, str
        type(ESMF_FieldType), pointer :: fp 
        integer :: status


        if (present(rc)) rc = ESMF_FAILURE

        print *, "Field Print:"
        if (.not. associated(field%ftypep)) then
          print *, "Empty or Uninitialized Field"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        fp => field%ftypep
        call ESMF_BasePrint(fp%base, str, status)
        if(status .NE. ESMF_SUCCESS) then 
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        call ESMF_StatusString(fp%fieldstatus, str, status)
        print *, "Field status = ", trim(str)

        if (fp%fieldstatus .ne. ESMF_STATE_READY) then
          if (present(rc)) rc = ESMF_FAILURE
          return
        endif

        call c_ESMC_GetName(fp%base, name, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldGetName"
          return
        endif 
        print *, "  Name = '",  trim(name), "'"

        call ESMF_StatusString(fp%gridstatus, str, status)
        print *, "Grid status = ", trim(str)
        if (fp%gridstatus .eq. ESMF_STATE_READY) then 
           call ESMF_GridPrint(fp%grid, "", status)
        endif

        call ESMF_StatusString(fp%datastatus, str, status)
        print *, "Data status = ", trim(str)
        if (fp%datastatus .eq. ESMF_STATE_READY) then 
           call ESMF_ArrayPrint(fp%localfield%localdata, "", status)
        endif

        call ESMF_FieldDataMapPrint(fp%mapping, "", status)

        !TODO: add code here to print more info

        ! global field contents
        !type (ESMF_IOSpec) :: iospec             ! iospec values
        !type (ESMF_Status) :: iostatus           ! if unset, inherit from gcomp

        ! local field contents
        !type (ESMF_Mask) :: mask                 ! may belong in Grid

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldPrint
!
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRead()"

!BOPI
! !IROUTINE: ESMF_FieldRead - Read in a Field from external storage
!
! !INTERFACE:
      function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: fname            ! field name to read
      character (len = *), intent(in), optional :: gname   ! grid name
      character (len = *), intent(in), optional :: dnames  ! data name
      type(ESMF_IOSpec), intent(in), optional :: iospec   ! file specs
      integer, intent(out), optional :: rc                ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!      This includes creating the {\tt ESMF\_Grid} associated with this {\tt ESMF\_Field}.
!      To share a single {\tt ESMF\_Grid} betwen multiple {\tt ESMF\_Field}s, see the {\tt ESMF\_FieldCreate} calls.
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[gname]}]
!            {\tt ESMF\_Grid} name.
!     \item [{[dnames]}]
!            Data name.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 


!
! TODO: code goes here.  this is filler to keep the compiler from complaining.
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldRead = a
!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.


        end function ESMF_FieldRead

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldReadRestart()"

!BOPI
! !IROUTINE: ESMF_FieldReadRestart - Read back in a saved Field
!
! !INTERFACE:
      function ESMF_FieldReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with an {\tt ESMF\_Field} from the 
!      last call to WriteRestart.
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.6.8

!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.

!
! TODO: code goes here; this is just filler to make the compiler not complain
!
        type (ESMF_Field) :: a
     
        nullify(a%ftypep)

        ESMF_FieldReadRestart = a

        end function ESMF_FieldReadRestart


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetArray()"

!BOP
! !IROUTINE: ESMF_FieldSetArray - Set data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field      
      type(ESMF_Array), intent(in) :: array
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Set data in {\tt ESMF\_Array} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           {\tt ESMF\_Array} containing data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.3, FLD1.6.4 (pri 2?), FLD1.7.2


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Minimal error checking 
      if (.not.associated(field%ftypep)) then
        print *, "ESMF_FieldSetData: Invalid or Destroyed Field"
        return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATE_READY) then
        print *, "ESMF_FieldSetData: Field not ready"
        return
      endif

      ! TODO: do we allow this?  if so, do we just destroy the old array?
      !if (ftypep%datastatus .eq. ESMF_STATE_READY) then
      !    print *, "ESMF_FieldSetData: data already associated with field"
      !    return
      !endif

      ftypep%localfield%localdata = array
      ftypep%datastatus = ESMF_STATE_READY
   
      ! TODO: add some validation here to be sure the array is the right
      ! size for the grid decomposition

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetGrid()"

!BOP
! !IROUTINE: ESMF_FieldSetGrid - Set Grid associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetGrid(field, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      type(ESMF_Grid), intent(in) :: grid      
      integer, intent(out), optional :: rc    
!
! !DESCRIPTION:
!  Used only with the version of {\tt ESMF\_FieldCreate} which creates an empty 
!  {\tt ESMF\_Field} and allows the {\tt ESMF\_Grid} to be specified later.  
!  Otherwise it is an error to try to change the {\tt ESMF\_Grid} 
!  associated with an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [grid]
!           {\tt ESMF\_Grid} to be added.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.1.3

        logical :: had_grid

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          print *, "ERROR: Invalid or Destroyed Field"
          return
        endif
 
        if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: Field not ready"
          return
        endif

        ! decide if we're regridding or just adding a grid to a partially
        ! created field.
        had_grid = .FALSE.
        if (field%ftypep%gridstatus .eq. ESMF_STATE_READY) had_grid = .TRUE.

        if (.not. had_grid) then
           ! if no grid, just add it
           field%ftypep%grid = grid
           field%ftypep%gridstatus = ESMF_STATE_READY
        else
           ! this could be considered a request to regrid the data
           print *, "not currently supported.  since a grid exists, this"
           print *, "will be a request to regrid the field"
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldSetGrid

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetDataMap()"

!BOP
! !IROUTINE: ESMF_FieldSetDataMap - Set DataMap assocated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataMap(field, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_FieldDataMap), intent(in) :: datamap
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Used to set the ordering of an {\tt ESMF\_Field}.  If an initialized 
!  {\tt ESMF\_FieldDataMap} and associated data are already in the 
!  {\tt ESMF\_Field}, the data will be reordered according to the new 
!  specification.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [datamap]
!           New memory order of data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.2

        logical :: had_data

        ! assume failure
        if (present(rc)) rc = ESMF_FAILURE

        ! Minimal error checking
        if (.not.associated(field%ftypep)) then
          print *, "ERROR: Invalid or Destroyed Field"
          return
        endif
 
        if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "ERROR: Field not ready"
          return
        endif

        ! decide if we're reordering data or just setting an initial map
        ! created field.
        had_data = .FALSE.
        if (field%ftypep%datastatus .eq. ESMF_STATE_READY) had_data = .TRUE.

        if (.not. had_data) then
           ! if no datamap, just add it
           field%ftypep%mapping = datamap
        else
           ! this could be considered a request to reorder the data
           print *, "not currently supported.  since data exists, this"
           print *, "will be a request to reorder the data in the field"
           return
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldSetDataMap

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetDataValues()"

!BOPI
! !IROUTINE: ESMF_FieldSetDataValues - Set contents of Data array
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataValues(field, index, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      integer, dimension (:), intent(in) :: index
      real, dimension (:), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Allows specified data values associated with an {\tt ESMF\_Field} to be set 
!      through the {\tt ESMF\_Field} interface instead of detaching data and setting 
!      it outside the framework.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [index]
!           Index or range to set.
!     \item [values]
!           Data values to set.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.6.7


!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.
!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetDataValues

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldValidate()"

!BOP
! !IROUTINE:  ESMF_FieldValidate - Check the internal consistency of a Field

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      character (len = *), intent(in) :: opt 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Routine to validate the internal state of an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [opt]
!           Validation option.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  FLD4.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      if (.not.associated(field%ftypep)) then 
          print *, "Uninitialized or Destroyed Field"
          return
      endif 

      if (field%ftypep%fieldstatus .ne. ESMF_STATE_READY) then
          print *, "Uninitialized or Destroyed Field"
          return
      endif 

      ! TODO: add more code here

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldValidate

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWrite()"

!BOP
! !IROUTINE: ESMF_FieldWrite - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWrite(field, & ! subset, 
                                 iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
!     type(ESMF_Subset), intent(in), optional :: subset
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[subset]}]
!            {\tt ESMF\_Subset}.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

        ! Local variables
        integer :: status, de_id
        logical :: rcpresent
        type(ESMF_Array) :: outarray
        type(ESMF_Grid) :: grid
        type(ESMF_DELayout) :: delayout
        character(len=ESMF_MAXSTR) :: filename

        ! TODO: revised code goes here


        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');

        ! Set initial values
        status = ESMF_FAILURE 
        rcpresent = .FALSE.

        ! Initialize return code
        if(present(rc)) then
            rcpresent=.TRUE.
            rc = ESMF_FAILURE      
        endif
           
        ! Get filename out of IOSpec, if specified.  Otherwise use the
        ! name of the Field.
        if (present(IOSpec)) then
            call ESMF_IOSpecGet(IOSpec, filename=filename, rc=status)
        else
            call ESMF_FieldGet(field, name=filename, rc=status)
        endif

        ! Collect results on DE 0 and output to a file
        call ESMF_FieldGet(field, grid=grid, rc=status)
        call ESMF_GridGet(grid, delayout=delayout, rc=status)
        call ESMF_DELayoutGet(delayout, localDE=de_id, rc=status)

        ! Output to file, from de_id 0 only
        call ESMF_ArrayGather(field%ftypep%localfield%localdata, &
                              field%ftypep%grid, field%ftypep%mapping, &
                              0, outarray, rc=status)
        !call ESMF_FieldAllGather(field, outarray, rc=status)
        if (de_id .eq. 0) then       
            call ESMF_ArrayWrite(outarray, filename=filename, rc=status)
            call ESMF_ArrayDestroy(outarray, status)
        endif

        end subroutine ESMF_FieldWrite


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWriteRestart()"

!BOPI
! !IROUTINE: ESMF_FieldWriteRestart - Save Field in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS: FLD1.6.8


!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.
!
! TODO: code goes here
!
        end subroutine ESMF_FieldWriteRestart


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the Field internal methods.
!
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNew()"

!BOPI
! !IROUTINE: ESMF_FieldConstructNew - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNew(ftype, grid, arrayspec, allocflag, &
                                        horzRelloc, vertRelloc, haloWidth, &
                                        datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_AllocFlag), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.  
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[allocflag]}]
!           Set to allocate space for data array.  Default is
!           {\tt ESMF\_DO\_ALLOCATE}.  Other option is {\tt ESMF\_NO\_ALLOCATE}.
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!           If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_Array) :: array                   ! New array
      type(ESMF_RelLoc) :: hRelLoc
      integer, dimension(ESMF_MAXDIM) :: gridcounts, arraycounts
      integer, dimension(ESMF_MAXDIM) :: dimorder, counts
      integer :: hwidth
      integer :: i, j, arrayRank, gridRank

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! make sure hwidth has a value here.
      if (present(haloWidth)) then
          hwidth = haloWidth
      else
          hwidth = 0
      endif

      call ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, vertRelloc, &
                                      hwidth, datamap, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Field construct NoA"
        return
      endif 

      call ESMF_ArraySpecGet(arrayspec, rank=arrayRank, rc=status)
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_ArraySpecGet"
        return
      endif 
      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGet"
        return
      endif 

      ! make sure hRelLoc has a value before GridGetDE call
      if (present(horzRelLoc)) then
          hRelLoc = horzRelloc
      else
          if (present(datamap)) then
              call ESMF_FieldDataMapGet(datamap, horzRelLoc=hRelLoc, rc=status)
          else
              print *, "ERROR in ESMF_FieldConstructNew: ", & 
                       "no valid RelLoc in either argument list or datamap."
              return
          endif
      endif
      call ESMF_GridGetDE(grid, horzRelLoc=hRelLoc, vertRelLoc=vertRelLoc, &
                          localCellCountPerDim=gridcounts(1:gridRank), rc=status)
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetDE"
        return
      endif 

      ! get information back from datamap
      call ESMF_FieldDataMapGet(ftype%mapping, dataIndices=dimorder, &
                                                    counts=counts, rc=status)

      arraycounts(:) = 1
      j = 1
      do i=1, arrayRank
         if (dimorder(i) .eq. 0) then
            arraycounts(i) = counts(j) 
            j = j + 1
         else
            arraycounts(i) = gridcounts(dimorder(i)) + (2 * hwidth)
         endif
      enddo

      array = ESMF_ArrayCreate(arrayspec, arraycounts, hwidth, rc=status) 
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Array create"
        return
      endif 

      ftype%localfield%localdata = array
      ftype%datastatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNew

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNewArray()"

!BOPI
! !IROUTINE: ESMF_FieldConstructNewArray - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructNewArray(ftype, grid, array, horzRelloc, &
                                             vertRelloc, haloWidth, datamap, &
                                             name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_Grid) :: grid               
      type(ESMF_Array), intent(in) :: array     
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.  
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [array]
!           Data. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      call ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, vertRelloc, &
                                      haloWidth, datamap, name, iospec, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNew: Field construct NoA 2"
        return
      endif 

      call ESMF_ArrayValidate(array, "", status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Error uninitialized or invalid array"
        return
      endif
      ftype%localfield%localdata = array
      !ftype%localfield%datastatus = ESMF_STATE_READY
      ftype%datastatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNewArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoDataPtr()"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoDataPtr - Construct a Field with no associated buffer

! !INTERFACE:
      subroutine ESMF_FieldConstructNoDataPtr(ftype, grid, arrayspec, &
                                           horzRelloc, vertRelloc, haloWidth, &
                                           datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype                
      type(ESMF_Grid) :: grid               
      type(ESMF_ArraySpec), intent(in) :: arrayspec     
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap 
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals except for the assignment of 
!     an associated data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: gridRank

      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     
 
      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftype%base, "Field", name, 0, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNoDataPtr: BaseCreate"
        return
      endif 

      ! TODO: Check to see grid is valid first.

      call ESMF_GridValidate(grid, "", status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Error uninitialized or invalid grid"
        return
      endif
      ftype%grid = grid
      ftype%gridstatus = ESMF_STATE_READY

      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      if (present(datamap)) then
        ftype%mapping = datamap   ! copy, datamap can be reused by user now
        ! if specified as explicit args to create, they override anything
        ! in the existing datamap
        call ESMF_FieldDataMapSet(ftype%mapping, horzRelloc=horzRelloc, &
                             vertRelloc=vertRelloc, rc=status)
      else
        if (gridRank .eq. 1) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_I, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 2) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJ, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 3) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJK, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        endif
      endif

!     call ESMF_ArrayConstructNoDataPtr(ftype%array)

      ! If I/O spec is present, copy it into the field object; otherwise just 
      ! initialize the I/O spec in the field object.
      if(present(iospec)) then
!       ESMF_IOSpecCopyInit(ftype%iospec, iospec, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldConstructNoDataPtr: IOSpec init"
          return
        endif 
      else 
!       ESMF_IOSpecInit(ftype%iospec, status)
        if(status .NE. ESMF_SUCCESS) then 
          print *, "ERROR in ESMF_FieldConstructNoDataPtr: IOSpec init"
          return
        endif 
      endif

      ftype%fieldstatus = ESMF_STATE_READY

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNoDataPtr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoArray()"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoArray - Construct a Field with no associated Array

! !INTERFACE:
      subroutine ESMF_FieldConstructNoArray(ftype, grid, horzRelloc, &
                                            vertRelloc, haloWidth, &
                                            datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype   
      type(ESMF_Grid) :: grid                 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(in), optional :: datamap              
      character (len=*), intent(in), optional :: name    
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
! Constructs an {\tt ESMF\_Field} except for its internal data {\tt ESMF\_Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [grid] 
!           Pointer to an {\tt ESMF\_Grid} object. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per grid cell/vertex in the horizontal
!           grid.
!     \item [{[vertRelloc]}] 
!           Relative location of data per grid cell/vertex in the vertical grid.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           Describes the mapping of data to the {\tt ESMF\_Grid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: gridRank

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftype%base, "Field", name, 0, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNoArray: BaseCreate"
        return
      endif 

      ! Attach grid
      call ESMF_GridValidate(grid, "", status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Error uninitialized or invalid grid"
        return
      endif
      ftype%grid = grid
      ftype%gridstatus = ESMF_STATE_READY

      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      if (present(datamap)) then
        ! this does a copy, datamap ok for user to delete now
        ftype%mapping = datamap   

        ! take care of override horz and vert rellocs.  if specified both in
        ! the datamap and as explicit args, the arguments take priority.
        call ESMF_FieldDataMapSet(ftype%mapping, horzRelloc=horzRelloc, &
                             vertRelloc=vertRelloc, rc=status)
      else
        ! create default datamap with 1-for-1 correspondence to grid
        if (gridRank .eq. 1) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_I, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 2) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJ, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        else if (gridRank .eq. 3) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJK, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=status)
        endif
      endif

!
! add more code here
!
     
      ftype%fieldstatus = ESMF_STATE_READY

      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoGridArray()"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoGridArray - Construct a Field with no Grid or Array
!
! !INTERFACE:
      subroutine ESMF_FieldConstructNoGridArray(ftypep, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftypep
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
!     Constructs {\tt ESMF\_Field} internals except those related to {\tt ESMF\_Grid} 
!     and {\tt ESMF\_Data}.
!
!
!     The arguments are:
!     \begin{description}
!     \item [ftypep]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[name]}]
!           {\tt ESMF\_Field} name.
!     \item [{[iospec]}]
!           {\tt ESMF\_Field} I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS: FLD1.1.3, FLD1.5.1


      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftypep%base, "Field", name, 0, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldConstructNoGridArray: BaseCreate"
        return
      endif 

      ! Initialize field contents
      !ftypep%localfield%gridstatus = ESMF_STATE_UNINIT
      !ftypep%localfield%datastatus = ESMF_STATE_UNINIT
      ftypep%gridstatus = ESMF_STATE_UNINIT
      ftypep%datastatus = ESMF_STATE_UNINIT

      ! Set the mapping as unknown/invalid
      call ESMF_FieldDataMapSetInvalid(ftypep%mapping, status)

      ftypep%fieldstatus = ESMF_STATE_READY

!
! add more code here
!
     
      if (rcpresent) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoGridArray

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestruct()"

!BOPI
! !IROUTINE:   ESMF_FieldDestruct - Free any Field memory allocated internally
!
! !INTERFACE:
      subroutine ESMF_FieldDestruct(ftype, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype        
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     Releases all resources except the {\tt ESMF\_Field} itself.
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status
      logical :: rcpresent                          ! Return code present

      ! Initialize return code; assume failure until success is certain
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif


      print *, "Field Destruct called"

      ! release the base class resources
      call ESMF_BaseDestroy(ftype%base, status)
      if(status .ne. ESMF_SUCCESS) then 
        print *, "ERROR in ESMF_FieldDestruct: BaseDestroy failed"
        return
      endif 

!
! TODO: more code goes here
!


      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestruct
!
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBoxIntersect()"

!BOPI
! !IROUTINE: ESMF_FieldBoxIntersect - Intersect bounding boxes
!
! !INTERFACE:
      subroutine ESMF_FieldBoxIntersect(srcField, dstField, recvDomainlist, &
                                        sendDomainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcField 
      type(ESMF_Field), intent(in) :: dstField
      type(ESMF_DomainList), intent(inout) :: recvDomainlist
      type(ESMF_DomainList), intent(inout) :: sendDomainlist
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Clips the src\_field physgrid box against the clip\_field, i.e. returns
!      a description of the area in clip\_field which is necessary to cover the
!      desired area in src\_field.  This procedure is mostly an entry point;
!      most of the work is done in the {\tt ESMF\_Grid} class.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source {\tt ESMF\_Field} object.
!     \item [dstField]
!           Destination {\tt ESMF\_Field} object.
!     \item [recvDomainlist]
!           Receive domain list.
!     \item [sendDomainlist]
!           Send domain list.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: my_DE, my_dst_DE, my_src_DE, gridrank
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: dst_min, dst_max
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: src_min, src_max
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: myAI
      type(ESMF_DELayout) :: parentDElayout, srcDElayout, dstDElayout
      type(ESMF_FieldType) :: stypep, dtypep      ! field type info
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc 

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      stypep = srcField%ftypep
      dtypep = dstField%ftypep

      ! Our DE number in the parent layout  TODO: for now, just use one of the
      !                                           grid layouts.  In the future,
      !                                           there will be proxy grids on all
      !                                           DEs of the coupler layout and a
      !                                           different method for determining
      !                                           if my_DE is part of a layout
      call ESMF_GridGet(stypep%grid, delayout=parentDElayout, rc=status)
      call ESMF_DELayoutGet(parentDElayout, localDE=my_DE, rc=status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_GridGet(stypep%grid, delayout=srcDElayout, rc=status)
  !jw    call ESMF_DELayoutGetDEExists(parentDElayout, my_DE, srcDElayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE)
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
        ! don't ask for our de number if this de isn't part of the layout
        call ESMF_DELayoutGet(srcDElayout, localDE=my_src_DE, rc=status)
        call ESMF_GridGet(stypep%grid, dimCount=gridrank, rc=status)
        call ESMF_FieldGet(srcField, horzRelloc=horzRelLoc, &
                           vertRelloc=vertRelLoc, rc=rc)
        call ESMF_GridGetDE(stypep%grid, horzRelLoc=horzRelLoc, &
                            vertRelLoc=vertRelLoc, &
                            globalAIPerDim=myAI(1:gridrank), rc=status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_GridGet(dtypep%grid, delayout=dstDElayout, rc=status)
   !jw   call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE)
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
        ! don't ask for our de number if this de isn't part of the layout
        call ESMF_DELayoutGet(dstDElayout, localDE=my_dst_DE, rc=status)
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
        if (rcpresent) rc = ESMF_SUCCESS
        return
      endif

      ! if src field exists on this DE, query it for information
      if (hassrcdata) then
        ! Query the datamap and set info for grid so it knows how to
        ! match up the array indices and the grid indices.
        call ESMF_FieldGet(srcField, grid=srcGrid, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in FieldBoxIntersect: FieldGet returned failure on Grid"
          return
        endif
        ! From the grid get the bounding box on this DE
        call ESMF_FieldGet(srcField, horzRelloc=horzRelLoc, &
                           vertRelloc=vertRelLoc, rc=rc)
        call ESMF_GridGetDE(srcGrid, &
                            horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                            minLocalCoordPerDim=src_min, &
                            maxLocalCoordPerDim=src_max, rc=status)
        call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, src_min, src_max, &
                                       myAI, sendDomainList, status)
      endif

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
        ! Query the datamap and set info for grid so it knows how to
        ! match up the array indices and the grid indices.
        call ESMF_FieldGet(dstField, grid=dstGrid, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in FieldBoxIntersect: FieldGet returned failure on Grid"
          return
        endif
        ! From the grid get the bounding box on this DE
        call ESMF_FieldGet(dstField, horzRelloc=horzRelLoc, &
                           vertRelloc=vertRelLoc, rc=rc)
        call ESMF_GridGetDE(dstGrid, &
                            horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                            minLocalCoordPerDim=dst_min, &
                            maxLocalCoordPerDim=dst_max, rc=status)
        call ESMF_GridBoxIntersectRecv(srcGrid, dst_min, dst_max, &
                                       recvDomainList, rc=status)
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBoxIntersect

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "No ESMF method defined"

      end module ESMF_FieldMod
