! $Id: ESMF_Field.F90,v 1.272.2.33 2009/01/21 21:25:20 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_Field.F90"
!==============================================================================
!
!     ESMF Field module
module ESMF_FieldMod
!
!==============================================================================
!
! This file contains the Field class definition and all Field
! class method.
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
! represents a single scalar or vector field.  {\tt ESMF\_Field}s associate
! a metadata description expressed as a set of {\tt ESMF\_Attributes} with
! a data {\tt ESMF\_Array}, {\tt ESMF\_Grid}, and I/O specification, or
! {\tt ESMF\_IOSpec} (NOT IMPLEMENTED). 
! 
! A gridToFieldMap describes the relationship of the {\tt ESMF\_Array} to
! the {\tt ESMF\_Grid}.  
!
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_VMMod
  use ESMF_LogErrMod
  use ESMF_IOSpecMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_StaggerLocMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_ArrayMod
  use ESMF_ArrayCreateMod
  use ESMF_ArrayGetMod
  use ESMF_TimeMod
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
! ! ESMF_Access
!
! ! Internal flag for tracking whether data is attached. 

  type ESMF_Access
    sequence
    private
    integer :: a_type
  end type

  type(ESMF_Access), parameter ::  &
    ESMF_READWRITE = ESMF_Access(0), &
    ESMF_READONLY = ESMF_Access(1)

!------------------------------------------------------------------------------
! ! ESMF_AllocFlag
!
! ! Interface flag for setting whether Field does the data allocation.

  type ESMF_AllocFlag
    sequence
    private
    integer :: a_type
  end type

  type(ESMF_AllocFlag), parameter ::  &
    ESMF_ALLOC = ESMF_AllocFlag(0), &
    ESMF_NO_ALLOC = ESMF_AllocFlag(1)

!------------------------------------------------------------------------------
! ! ESMF_FieldType
! ! Definition of the Field class.

  type ESMF_FieldType
    sequence
    !private
    type (ESMF_Base)              :: base             ! base class object
    type (ESMF_Array)             :: array
    type (ESMF_Grid)              :: grid
    type (ESMF_Status)            :: fieldstatus
    type (ESMF_Status)            :: gridstatus
    type (ESMF_Status)            :: datastatus
    type (ESMF_IOSpec)            :: iospec           ! iospec values
    type (ESMF_Status)            :: iostatus         ! if unset, inherit from gcomp
    type (ESMF_StaggerLoc)        :: staggerloc
    logical                       :: array_internal   ! .true. if field%array is
                                                      ! internally allocated
    logical                       :: is_proxy         ! .true. for a proxy field
    integer                       :: gridToFieldMap(ESMF_MAXDIM)
    integer                       :: ungriddedLBound(ESMF_MAXDIM)
    integer                       :: ungriddedUBound(ESMF_MAXDIM)
    integer                       :: maxHaloLWidth(ESMF_MAXDIM)
    integer                       :: maxHaloUWidth(ESMF_MAXDIM)
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_Field    
! ! The Field data structure that is passed between implementation and
! ! calling languages.

  type ESMF_Field
    sequence
    !private       
    type (ESMF_FieldType), pointer :: ftypep
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Field, ESMF_Access
  public ESMF_FieldType ! For internal use only
  public ESMF_AllocFlag, ESMF_NO_ALLOC, ESMF_ALLOC

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_FieldValidate           ! Check internal consistency

   public ESMF_FieldWrite              ! Write data and Grid from a Field

   public assignment(=)

! - ESMF-internal methods:
   public ESMF_FieldTypeGetInit        ! For Standardized Initialization
   public ESMF_FieldTypeInit           ! For Standardized Initialization
   public ESMF_FieldTypeValidate
   public ESMF_FieldGetInit            ! For Standardized Initialization
   public ESMF_FieldSerialize
   public ESMF_FieldDeserialize
   public ESMF_FieldInitialize         ! Default initiailze field member variables

   public ESMF_FieldGetInt4Attr
   public ESMF_FieldGetInt4ListAttr
   public ESMF_FieldGetInt8Attr
   public ESMF_FieldGetInt8ListAttr
   public ESMF_FieldGetReal4Attr
   public ESMF_FieldGetReal4ListAttr
   public ESMF_FieldGetReal8Attr
   public ESMF_FieldGetReal8ListAttr
   public ESMF_FieldGetLogicalAttr
   public ESMF_FieldGetLogicalListAttr
   public ESMF_FieldGetCharAttr

   public ESMF_FieldGetAttrInfoByName
   public ESMF_FieldGetAttrInfoByNum
   public ESMF_FieldGetAttributeCount

   public ESMF_FieldSetInt4Attr
   public ESMF_FieldSetInt4ListAttr
   public ESMF_FieldSetInt8Attr
   public ESMF_FieldSetInt8ListAttr
   public ESMF_FieldSetReal4Attr
   public ESMF_FieldSetReal4ListAttr
   public ESMF_FieldSetReal8Attr
   public ESMF_FieldSetReal8ListAttr
   public ESMF_FieldSetLogicalAttr
   public ESMF_FieldSetLogicalListAttr
   public ESMF_FieldSetCharAttr

!  !subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!  !function ESMF_FieldReadRestart(name, iospec, rc)
!  !subroutine ESMF_FieldWrite(field, subset, iospec, rc)
!  !function ESMF_FieldRead(fname, gname, dnames, iospec, rc)
!
!
!EOPI

! !PRIVATE MEMBER FUNCTIONS:

   private ESMF_FieldWriteFileASCII
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Field.F90,v 1.272.2.33 2009/01/21 21:25:20 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: assignment (=) - set one field equal to another
!
! !INTERFACE:
      interface assignment (=)
   
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_FieldAssign


! !DESCRIPTION:
!    Set one field equal to another note that since its 
!    a pointer copy the fields are actually the same
 
!EOPI
      end interface
!
!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAssign()"
!BOPI
! !IROUTINE:  ESMF_FieldAssign - set one field struct equal to another

! !INTERFACE:

   subroutine ESMF_FieldAssign(dval, sval)
!
! !ARGUMENTS:
 type(ESMF_Field), intent(out) :: dval
 type(ESMF_Field), intent(in) :: sval
!
! !DESCRIPTION:
!      Set one field structure equal to another
!
!     The arguments are:
!     \begin{description}
!     \item [dval]
!           destination structure
!     \item [dval]
!           source structure
!     \end{description}
!
!EOPI

 dval%ftypep => sval%ftypep

 ESMF_INIT_COPY(dval,sval)

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt4Attr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt4ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt8Attr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc       

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInt8ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetInt8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal4Attr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc           

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal4ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal8Attr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc            

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetReal8ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetReal8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLogicalAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetLogicalAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLogicalListAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetLogicalListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetCharAttr"

!BOPI
! !IROUTINE: ESMF_FieldGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_FieldGetCharAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetChar(field%ftypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttributeCount"

!BOPI
! !IROUTINE: ESMF_FieldGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_FieldGetAttributeCount(field, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt field} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetCount(field%ftypep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByName"

!BOPI
! !IROUTINE: ESMF_FieldGetAttributeInfo - Query Field attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttributeInfo()
      subroutine ESMF_FieldGetAttrInfoByName(field, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetAttrInfoName(field%ftypep%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByNum"

!BOPI
! !IROUTINE: ESMF_FieldGetAttributeInfo - Query Field attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttributeInfo()
      subroutine ESMF_FieldGetAttrInfoByNum(field, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttGetAttrInfoNum(field%ftypep%base, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldReadRestart"

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

!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.

!
! TODO: code goes here; this is just filler to make the compiler not complain
!
        type (ESMF_Field) :: a

        ! Initialize
        if (present(rc)) rc = ESMF_RC_NOT_IMPL     
        nullify(a%ftypep)

        ESMF_FieldReadRestart = a

        end function ESMF_FieldReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt4Attr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt4ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt8Attr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInt8ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetInt8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal4Attr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal4Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal4ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal4ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal8Attr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal8Attr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetReal8ListAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetReal8ListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetLogicalAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetLogicalAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetLogicalListAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetLogicalListAttr(field, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_BaseAttSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetCharAttr"

!BOPI
! !IROUTINE: ESMF_FieldSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldSetAttribute()
      subroutine ESMF_FieldSetCharAttr(field, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a character attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_BaseAttSetChar(field%ftypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldValidate"

!BOP
! !IROUTINE:  ESMF_FieldValidate - Check validity of a Field

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
      character (len = *), intent(in), optional :: options 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt field} is internally consistent.
!      Currently this method determines if the {\tt field} is uninitialized 
!      or already destroyed. It validates the contained array and grid objects.
!      The code also checks if the array and grid sizes agree.
!      This check compares the distgrid contained in array and grid; 
!      then it proceeds to compare the computational bounds contained 
!      in array and grid. 
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           {\tt ESMF\_Field} to validate.
!     \item [{[options]}]
!           Validation options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt field} 
!           is valid.
!     \end{description}
!
!EOP

      integer :: localrc

      type(ESMF_FieldType), pointer :: ftypep
      type(ESMF_StaggerLoc) :: staggerloc
      integer :: exclLBounds(ESMF_MAXDIM)  ! exclusive grid lower bounds
      integer :: exclUBounds(ESMF_MAXDIM)  ! exclusive grid upper bounds
      integer :: gridrank, arrayrank, gridrank_norep
      integer :: i, lDE                        ! helper variables to verify bounds
      integer :: localDECount, dimCount        ! and distgrid
      integer, allocatable :: distgridToGridMap(:)
      integer, allocatable :: distgridToPackedArrayMap(:)
      integer, allocatable :: arrayCompUBnd(:, :), arrayCompLBnd(:, :)
      integer, allocatable :: gridCompUBnd(:), gridCompLBnd(:)
      type(ESMF_DistGrid)  :: arrayDistGrid, gridDistGrid

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (.not.associated(field%ftypep)) then 
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed Field: ftypep unassociated", &
             ESMF_CONTEXT, rc)
         return
      endif 

      ftypep => field%ftypep


      ! make sure the field is ready before trying to look at contents
      if (ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed Field: fieldstatus not ready", &
             ESMF_CONTEXT, rc)
         return
      endif 

      staggerloc = ftypep%staggerloc

      ! make sure there is a grid before asking it questions.
      if (ftypep%gridstatus .eq. ESMF_STATUS_READY) then
          call ESMF_GridValidate(grid=ftypep%grid, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          ! get grid dim and extents for the local piece
          call ESMF_GridGet(ftypep%grid, dimCount=gridrank, &
                            distgrid=gridDistGrid, localDECount=localDECount, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                "Cannot retrieve distgrid, gridrank, localDECount from ftypep%grid", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          ! Bounds only valid if there are local DE's
          do lDE=0, localDECount-1
             call ESMF_GridGet(ftypep%grid, localDE=lDE, staggerloc=staggerloc, &
                               exclusiveLBound=exclLBounds, &
                               exclusiveUBound=exclUBounds, &
                               rc=localrc)
              if (localrc .ne. ESMF_SUCCESS) then
                 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                    "Cannot retrieve exclusive bounds from ftypep%grid", &
                     ESMF_CONTEXT, rc)
                 return
              endif 
          enddo
      endif

      ! make sure there is data before asking it questions.
      if (ftypep%datastatus .eq. ESMF_STATUS_READY) then
          call ESMF_ArrayValidate(array=ftypep%array, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                "Cannot validate ftypep%array", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          call ESMF_ArrayGet(ftypep%array, dimCount=dimCount, localDECount=localDECount, &
              distgrid=arrayDistGrid, rank=arrayrank, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                "Cannot retrieve dimCount, localDECount, arrayDistGrid, arrayrank from ftypep%array", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          
          ! Verify the distgrids in array and grid match.
          if(ESMF_DistGridMatch(gridDistGrid, arrayDistGrid, rc=localrc) .ne. ESMF_TRUE) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                 "grid DistGrid does not match array DistGrid", &
                  ESMF_CONTEXT, rc)
              return
          endif

          ! Verify that the computational bounds of array and grid contained
          ! in the field match.
          allocate(distgridToGridMap(dimCount))
          allocate(distgridToPackedArrayMap(dimCount))
          allocate(arrayCompLBnd(dimCount, 0:localDECount-1))
          allocate(arrayCompUBnd(dimCount, 0:localDECount-1))

          call ESMF_GridGet(ftypep%grid, distgridToGridMap=distgridToGridMap, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                "Cannot retrieve distgridToGridMap from ftypep%grid", &
                 ESMF_CONTEXT, rc)
             return
          endif 
          call ESMF_ArrayGet(ftypep%array, &
              distgridToPackedArrayMap=distgridToPackedArrayMap, &
              computationalLBound=arrayCompLBnd, &
              computationalUBound=arrayCompUBnd, rc=localrc)
          if (localrc .ne. ESMF_SUCCESS) then
             call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                "Cannot retrieve computational bounds from ftypep%array", &
                 ESMF_CONTEXT, rc)
             return
          endif 

          ! Verify that array rank is greater than or equal to grid rank + ungridded bound rank
          gridrank_norep = gridrank
          do i = 1, dimCount
            if(distgridToPackedArrayMap(i) == 0) gridrank_norep = gridrank_norep - 1
          enddo
          if ( arrayrank .lt. gridrank_norep) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                 "grid rank + ungridded Bound rank not equal to array rank", &
                  ESMF_CONTEXT, rc)
              return
          endif

          ! verify array computational bounds match grid computational bounds per localDE
          do lDE=0, localDECount-1
              allocate(gridCompUBnd(dimCount), gridCompLBnd(dimCount))
              call ESMF_GridGet(ftypep%grid, staggerloc=staggerloc, localDE=lDE, &
                  computationalUBound=gridCompUBnd, computationalLBound=gridCompLBnd, &
                  rc=localrc)
              if (localrc .ne. ESMF_SUCCESS) then
                 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                    "Cannot retrieve computational bounds from ftypep%grid on localDE", &
                     ESMF_CONTEXT, rc)
                 return
              endif 
              do i=1, dimCount
                  if(distgridToPackedArrayMap(i) .ne. 0) then
                      if(gridCompLBnd(distgridToGridMap(i)) .ne. &
                        arrayCompLBnd(distgridToPackedArrayMap(i), lDE)) then
                          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                             "grid computationalLBound does not match array computationalLBound", &
                              ESMF_CONTEXT, rc)
                          return
                      endif
                      if(gridCompUBnd(distgridToGridMap(i)) .ne. &
                        arrayCompUBnd(distgridToPackedArrayMap(i), lDE)) then
                          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                             "grid computationalUBound does not match array computationalUBound", &
                              ESMF_CONTEXT, rc)
                          return
                      endif
                  endif
              enddo
              deallocate(gridCompUBnd, gridCompLBnd)
          enddo
          deallocate(distgridToGridMap, arrayCompUBnd, arrayCompLBnd, distgridToPackedArrayMap)
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWrite"

!BOPI
! !IROUTINE: ESMF_FieldWrite - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWrite(field, iospec, timestamp, rc)
!
! !ARGUMENTS:
        type(ESMF_Field), intent(inout) :: field
        type(ESMF_IOSpec), intent(in), optional :: iospec
        type(ESMF_Time), intent(in), optional :: timestamp 
        integer, intent(out), optional :: rc               ! return code

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
!     \item [{[iospec]}]
!            I/O specification. ! NOT IMPLEMENTED
!     \item [{[timestamp]}]
!            A timestamp of type {\tt ESMF\_Time} for the data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Local variables
        integer :: localrc, de_id
        type(ESMF_Array) :: out_array
        type(ESMF_TypeKind) arr_kind
        integer out_rank
        integer out_kind
        integer, dimension(:), pointer :: out_counts
        integer, dimension(:), pointer :: out_lbounds
        integer, dimension(:), pointer :: out_ubounds
        integer, dimension(:), pointer :: out_strides
        type(ESMF_Grid) :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_DELayout) :: delayout
        type (ESMF_IOFileFormat) :: fileformat
        type(ESMF_Time) :: ts
        character (19) Date
      
        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

#if 0           
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

! TODO:FIELDINTEGRATION Restore the ESMF_FieldWrite() method.

        ! Get filename out of IOSpec, if specified.  Otherwise use the
        ! name of the Field.
        if (present(IOSpec)) then
           call ESMF_IOSpecGet(IOSpec, iofileformat=fileformat, rc=localrc)
           if (fileformat == ESMF_IO_FILEFORMAT_HDF) then
              print*, "HDF output is not currently supported."
              return
           else if (fileformat == ESMF_IO_FILEFORMAT_UNSPECIFIED) then
              call ESMF_FieldWriteFileASCII(field, iospec, rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
              if (present(rc)) rc = ESMF_SUCCESS
              return
           else if (fileformat == ESMF_IO_FILEFORMAT_NETCDF) then
              print*, "NetCDF output is not currently supported."
              return
           else
              print*, "Unrecognized IO Fileformat."
              return
           endif
        else ! No IOSpec passed in, so check in the Field
           call ESMF_IOSpecGet(field%ftypep%iospec, iofileformat=fileformat, rc=localrc)
           if (fileformat == ESMF_IO_FILEFORMAT_HDF) then
              print*, "HDF output is not currently supported."
              return
           else if (fileformat == ESMF_IO_FILEFORMAT_UNSPECIFIED) then
           call ESMF_FieldWriteFileASCII(field, iospec, rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
              if (present(rc)) rc = ESMF_SUCCESS

              return
           else if (fileformat == ESMF_IO_FILEFORMAT_NETCDF) then
              print*, "NetCDF output is not currently supported."
              return
           else
              print*, "Unrecognized IO Fileformat."
              return
           endif
        endif

        if ( present(timestamp) ) then
           ts = timestamp
        else
           ! as a default, set the date/time as the current real time.
           call ESMF_TimeSyncToRealTime(ts, localrc)
        endif
        ! get the date from the timestamp.
        call ESMF_TimeGet(ts, timeString=Date, rc=localrc)
        Date = Date(1:10)//'_'//Date(12:19)

        ! Collect results on DE 0 and output to a file
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
!!$        call ESMF_FieldGet( field, name=fieldname, rc=localrc)
! TODO:FIELDINTEGRATION Find another way to get the localDE.
        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        call ESMF_DistGridGet(distgrid, delayout=delayout, rc=localrc)
        call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=localrc)

        ! Output to file, from de_id 0 only
!!$        call ESMF_FieldAllGather(field, out_array, rc=localrc)
! TODO:FIELDINTEGRATION IArrayComms are being removed.
!        call ESMF_IArrayGather(field%ftypep%array, &
!                              field%ftypep%grid, field%ftypep%mapping, &
!                              0, out_array, rc=localrc)


        if (de_id .eq. 0) then       
        call ESMF_InternArrayGet(out_array, out_rank, arr_kind, rc=rc)
        allocate(out_counts (out_rank), &
                 out_lbounds(out_rank), &
                 out_ubounds(out_rank), &
                 out_strides(out_rank), stat=rc)
        call ESMF_InternArrayGet(out_array, counts=out_counts, lbounds=out_lbounds, &
                           ubounds=out_ubounds, strides=out_strides, rc=rc)

        out_kind = arr_kind%dkind

        endif ! (de_id .eq. 0) then  

        call ESMF_InternArrayDestroy(out_array, rc=localrc)
        if  (present(rc)) rc = ESMF_SUCCESS
#endif
        
        end subroutine ESMF_FieldWrite

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldWriteFileASCII - Write a Field to external storage
!
! !INTERFACE:
      subroutine ESMF_FieldWriteFileASCII(field, & ! subset, 
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
!            I/O specification. ! NOT IMPLEMENTED
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Local variables
        integer :: localrc, de_id
        type(ESMF_Array) :: outarray
        type(ESMF_Grid) :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_DELayout) :: delayout
        character(len=ESMF_MAXSTR) :: filename
        character(len=ESMF_MAXSTR) :: name

! TODO:FIELDINTEGRATION Restore the ESMF_FieldWriteFileASCII() method.

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL 
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

#if 0 
        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)
           
        ! Get filename out of IOSpec, if specified.  Otherwise use the
        ! name of the Field.
        if (present(IOSpec)) then
            call ESMF_IOSpecGet(IOSpec, filename=filename, rc=localrc)
        else
            call ESMF_FieldGet(field, name=filename, rc=localrc)
        endif
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Collect results on DE 0 and output to a file
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

! TODO: May want to get DELayout directly from Grid - however this 
!       may be a deprecated use of DELayout.
        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_DistGridGet(distgrid, delayout=delayout, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        write(name,'(i1)') de_id
        call ESMF_InternArrayWrite(field%ftypep%array,&
                             filename=trim(name), rc=localrc)

        ! Output to file, from de_id 0 only
        call ESMF_IArrayGather(field%ftypep%array, &
                              field%ftypep%grid, field%ftypep%mapping, &
                              0, outarray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        !call ESMF_FieldAllGather(field, outarray, rc=localrc)
        if (de_id .eq. 0) then       
            call ESMF_InternArrayWrite(outarray, filename=filename, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
            call ESMF_InternArrayDestroy(outarray, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

      if (present(rc)) rc = ESMF_SUCCESS
#endif

      end subroutine ESMF_FieldWriteFileASCII
        

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWriteRestart"

!BOPI
! !IROUTINE: ESMF_FieldWriteRestart - Save Field in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_FieldWriteRestart(field, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
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

        ! Initialize
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.
!
! TODO: code goes here
!
        end subroutine ESMF_FieldWriteRestart


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all Field internal methods.
!
!------------------------------------------------------------------------------

!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBoxIntersect"

!BOPI
! !IROUTINE: ESMF_FieldBoxIntersect - Intersect bounding boxes
!
! !INTERFACE:
      subroutine ESMF_FieldBoxIntersect(srcField, dstField, & 
!                                       recvDomainlist, &
!                                       sendDomainList, &
                                        rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: srcField 
      type(ESMF_Field), intent(inout) :: dstField
!      type(ESMF_DomainList), intent(inout) :: recvDomainlist
!      type(ESMF_DomainList), intent(inout) :: sendDomainlist
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

      integer :: localrc
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      type(ESMF_DELayout) :: gridDELayout
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      type(ESMF_StaggerLoc) :: dstStaggerloc, srcStaggerloc
      type(ESMF_VM) :: vm

      ! Initialize return code
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,srcField,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,dstField,rc)

#if 0

      ! TODO: replace this with a better way to get the current VM
! TODO:FIELDINTEGRATION Find a better way to get the VM.
      call ESMF_GridGet(srcField%ftypep%grid, delayout=gridDELayout, rc=localrc)
      call ESMF_DELayoutGet(gridDELayout, vm=vm, rc=localrc)

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      hasdata = ESMF_TRUE   ! temp for now to get rid of warning
      hassrcdata = (hasdata .eq. ESMF_TRUE)
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
        call ESMF_FieldGet(srcField, staggerloc=srcStaggerloc, &
                           grid=srcGrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      hasdstdata = (hasdata .eq. ESMF_TRUE)
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
        call ESMF_FieldGet(dstField, staggerloc=dstStaggerloc, &
                           grid=dstGrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
        if  (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! if src field exists on this DE, query it for information
      if (hassrcdata) then
        ! From the grid get the bounding box on this DE
        call ESMF_GridBoxIntersectSend(srcGrid, dstGrid, sendDomainList, &
                                       total=.false., layer=.false., rc=localrc)
      endif

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
        call ESMF_GridBoxIntersectRecv(srcGrid, dstGrid, vm, recvDomainList, &
                                       hasdstdata, hassrcdata, &
                                       total=.false., layer=.false., rc=localrc)
      endif

      if  (present(rc)) rc = ESMF_SUCCESS
#endif

      end subroutine ESMF_FieldBoxIntersect

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSerialize"

!BOPI
! !IROUTINE: ESMF_FieldSerialize - Serialize field info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_FieldSerialize(field, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Field} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldWrite()} and {\tt ESMF\_FieldRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           {\tt ESMF\_Field} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_FieldType), pointer :: fp    ! field type

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! shortcut to internals
      fp => field%ftypep

      call c_ESMC_BaseSerialize(fp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldSerialize(fp%fieldstatus, fp%gridstatus, &
                                 fp%datastatus, fp%iostatus, & 
                                 fp%staggerloc%staggerloc, fp%gridToFieldMap, &
                                 fp%ungriddedLBound, fp%ungriddedUBound, &
                                 fp%maxHaloLWidth, fp%maxHaloUWidth, &
                                 buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if (fp%gridstatus .eq. ESMF_STATUS_READY) then
         call ESMF_GridSerialize(fp%grid, buffer, length, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (fp%datastatus .eq. ESMF_STATUS_READY) then
          call c_ESMC_ArraySerialize(fp%array, buffer(1),&
                                    length, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDeserialize"

!BOPI
! !IROUTINE: ESMF_FieldDeserialize - Deserialize a byte stream into a Field
!
! !INTERFACE:
      function ESMF_FieldDeserialize(vm, buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldDeserialize   
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Field object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldWrite()} and {\tt ESMF\_FieldRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [vm]
!           Current VM in which this object should be created.
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_FieldType), pointer :: fp    ! field type

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! In case of error, make sure this is invalid.
      nullify(ESMF_FieldDeserialize%ftypep)

      ! Shortcut to internals
      allocate(fp, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                     "space for new Field object", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_BaseCreate(fp%base, "Field", "dummy", 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! This overwrites the name and adds attributes to the base obj.
      call c_ESMC_BaseDeserialize(fp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldDeserialize(fp%fieldstatus, fp%gridstatus, &
                                   fp%datastatus, fp%iostatus, &
                                   fp%staggerloc%staggerloc, fp%gridToFieldMap, &
                                   fp%ungriddedLBound, fp%ungriddedUBound, &
                                   fp%maxHaloLWidth, fp%maxHaloUWidth, &
                                   buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (fp%gridstatus .eq. ESMF_STATUS_READY) then
          fp%grid=ESMF_GridDeserialize(vm, buffer, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (fp%datastatus .eq. ESMF_STATUS_READY) then
          call c_ESMC_ArrayDeserialize(fp%array, buffer(1), offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

          call ESMF_ArraySetInitCreated(fp%array,rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif
    
      fp%is_proxy = .true.
      ESMF_FieldDeserialize%ftypep => fp
      ESMF_INIT_SET_CREATED(ESMF_FieldDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldDeserialize

!----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldTypeGetInit"
!BOPI
! !IROUTINE:  ESMF_FieldTypeGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FieldTypeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_FieldType), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_FieldTypeGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt fieldtype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldType} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_FieldTypeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_FieldTypeGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_FieldTypeGetInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldTypeInit"
!BOPI
! !IROUTINE:  ESMF_FieldTypeInit - Initialize FieldType

! !INTERFACE:
    subroutine ESMF_FieldTypeInit(s)
!
! !ARGUMENTS:
       type(ESMF_FieldType) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt fieldtype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldType} of which being initialized.
!     \end{description}
!
!EOPI

        s%fieldstatus   = ESMF_STATUS_UNINIT
        s%gridstatus    = ESMF_STATUS_UNINIT
        s%datastatus    = ESMF_STATUS_UNINIT
        s%array_internal = .false.
        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_FieldTypeInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldTypeValidate"
!BOPI
! !IROUTINE:  ESMF_FieldTypeValidate - Check validity of a FieldType

! !INTERFACE:
    subroutine ESMF_FieldTypeValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_FieldType), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt FieldType} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldType} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt s}
!           is valid.
!     \end{description}
!
!EOPI

     ESMF_INIT_CHECK_SHALLOW(ESMF_FieldTypeGetInit,ESMF_FieldTypeInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_FieldTypeValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInit"
!BOPI
! !IROUTINE:  ESMF_FieldGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FieldGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Field), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_FieldGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Field} from which to retrieve status.
!     \end{description}
!
!EOPI


       if (present(d)) then
         ESMF_FieldGetInit = ESMF_INIT_GET(d)
       else
         ESMF_FieldGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_FieldGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldInitialize"

!BOPI
! !IROUTINE: ESMF_FieldInitialize - Default initialize field member variables
!
! !INTERFACE:
      subroutine ESMF_FieldInitialize(ftypep, rc) 
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftypep 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Field} object and default initialize its
!      auxiliary data members. Only to be used by other Field Create methods.
!
!     The arguments are:
!     \begin{description}
!     \item [ftypep]
!           {\tt ESMF\_FieldType} object to be default initialized.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ftypep%fieldstatus = ESMF_STATUS_UNINIT
        ftypep%gridstatus  = ESMF_STATUS_UNINIT
        ftypep%datastatus  = ESMF_STATUS_UNINIT
        ftypep%iostatus    = ESMF_STATUS_UNINIT
       
        ftypep%staggerloc = ESMF_STAGGERLOC_CENTER

        ftypep%array_internal = .false. 
        ftypep%is_proxy       = .false. 
        ftypep%gridToFieldMap = -1
        ftypep%ungriddedLBound = -1
        ftypep%ungriddedUBound = -1
        ftypep%maxHaloLWidth   = -1
        ftypep%maxHaloUWidth   = -1

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldInitialize

!------------------------------------------------------------------------------

end module ESMF_FieldMod
