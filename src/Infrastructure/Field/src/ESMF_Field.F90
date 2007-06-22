! $Id: ESMF_Field.F90,v 1.248 2007/06/22 23:21:30 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Field.F90"
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
! !MODULE: ESMF_FieldMod - Combine physical field metadata, data and interngrid
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class, which 
! represents a
! single scalar or vector field.  {\tt ESMF\_Field}s associate a metadata 
! description 
! expressed as a set of {\tt ESMF\_Attributes} with a data {\tt ESMF\_Array}, 
! {\tt ESMF\_InternGrid}, and I/O specification, or {\tt ESMF\_IOSpec}.  
! An {\tt ESMF\_FieldDataMap} describes the 
! relationship of the {\tt ESMF\_Array} to the {\tt ESMF\_InternGrid}.  
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
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
      use ESMF_InternArrayDataMapMod
      use ESMF_DELayoutMod
      use ESMF_InternGridTypesMod
      use ESMF_InternGridMod
      use ESMF_InternArrayMod
      use ESMF_InternArrayCreateMod
      use ESMF_InternArrayGetMod
      use ESMF_InternArrayCommMod
      use ESMF_TimeMod
      use ESMF_FieldDataMapMod
      use ESMF_InitMacrosMod

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
                               ESMF_ALLOC = ESMF_AllocFlag(0), &
                               ESMF_NO_ALLOC = ESMF_AllocFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_LocalField
!      
!     ! The LocalField class contains information which is associated with the
!     ! local DE.

      type ESMF_LocalField
        sequence
        type(ESMF_InternArray) :: localdata   ! local data for this DE
        type(ESMF_Mask)        :: mask        ! may belong in InternGrid
        type(ESMF_ArraySpec)   :: arrayspec   ! so field can allocate

        integer :: rwaccess                      ! reserved for future use
        integer :: accesscount                   ! reserved for future use
        
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        logical :: localFlag = .true.            ! .true. if local data present
#else        
        logical :: localFlag                     ! .true. if local data present
#endif

        ESMF_INIT_DECLARE

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
        type (ESMF_Status) :: fieldstatus = ESMF_STATUS_UNINIT
        type (ESMF_Status) :: interngridstatus = ESMF_STATUS_UNINIT
        type (ESMF_Status) :: datastatus = ESMF_STATUS_UNINIT
        type (ESMF_Status) :: datamapstatus = ESMF_STATUS_UNINIT
#else
        type (ESMF_Status) :: fieldstatus
        type (ESMF_Status) :: interngridstatus
        type (ESMF_Status) :: datastatus
        type (ESMF_Status) :: datamapstatus
#endif
        type (ESMF_InternGrid) :: interngrid
        type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_FieldDataMap) :: mapping  ! mapping of array indices to interngrid
        type (ESMF_IOSpec) :: iospec         ! iospec values
        type (ESMF_Status) :: iostatus       ! if unset, inherit from gcomp
        ESMF_INIT_DECLARE

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
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Field, ESMF_Access
      public ESMF_FieldType, ESMF_LocalField  ! for internal lib use only
      public ESMF_AllocFlag, ESMF_NO_ALLOC, ESMF_ALLOC

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
   public ESMF_LocalFieldInit          ! For Standardized Initialization
   public ESMF_LocalFieldValidate      ! For Standardized Initialization
   public ESMF_LocalFieldGetInit       ! For Standardized Initialization

   public ESMF_FieldTypeInit           ! For Standardized Initialization
   public ESMF_FieldTypeValidate       ! For Standardized Initialization
   public ESMF_FieldTypeGetInit        ! For Standardized Initialization

   public ESMF_FieldGetInit            ! For Standardized Initialization


   public ESMF_FieldCreateNoData       ! Create a new Field without data
   public ESMF_FieldDestroy            ! Destroy a Field

   public ESMF_FieldGet                ! Generic Get() routine, replaces others

   public ESMF_FieldGetGlobalInternGridInfo  ! Return global InternGrid info
   public ESMF_FieldGetLocalInternGridInfo   ! Return local InternGrid info

   public ESMF_FieldGetInternArray     ! Return the data Array
   public ESMF_FieldGetLocalArray      ! Return the Local Array
   public ESMF_FieldGetGlobalDataInfo  ! Return global data info
   public ESMF_FieldGetLocalDataInfo   ! Return local data info

   public ESMF_FieldSetInternGrid            ! Set a InternGrid (may regrid if different
                                       !   InternGrid is already present)
   public ESMF_FieldSetInternArray     ! Set a data Array in a Field
   public ESMF_FieldSetLocalArray      ! Set a data Array in a Field
   public ESMF_FieldSetDataValues      ! Set Field data values 

   public ESMF_FieldSetDataMap         ! Set a DataMap (may reorder if different
                                       !   DataMap is already present)

   public ESMF_FieldSetAttribute       ! Set and Get attributes
   public ESMF_FieldGetAttribute       !  

   public ESMF_FieldGetAttributeCount  ! number of attribs
   public ESMF_FieldGetAttributeInfo   ! get type, length by name or number

   public ESMF_FieldValidate           ! Check internal consistency
   public ESMF_FieldPrint              ! Print contents of a Field
   public ESMF_FieldBoxIntersect       ! Intersect bounding boxes

   public ESMF_FieldWrite              ! Write data and interngrid from a Field

   public ESMF_FieldConstructIA        ! Only public for internal use
   public ESMF_FieldSerialize
   public ESMF_FieldDeserialize

   public assignment(=)

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
      '$Id: ESMF_Field.F90,v 1.248 2007/06/22 23:21:30 cdeluca Exp $'

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
        module procedure ESMF_FieldCreateNoInternGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for methods that create 
!     an {\tt ESMF\_Field} without allocating or referencing any associated data.
!     The variations allow an {\tt ESMF\_InternGrid} to be specified or not, and for
!     the data description to be specified or not.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldConstructIA - Construct the internals of a new Field
!
! !INTERFACE:
      interface ESMF_FieldConstructIA

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldConstructIANew
        module procedure ESMF_FieldConstructIANewArray

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
        module procedure ESMF_FieldConstructNoInternGridArray  

! !DESCRIPTION:
!     This interface provides an entry point for {\tt ESMF\_Field} construction 
!     methods that do not allocate or reference any associated data.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldSetAttribute  - Set Field attributes
!
! !INTERFACE:
      interface ESMF_FieldSetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldSetInt4Attr
        module procedure ESMF_FieldSetInt4ListAttr
        module procedure ESMF_FieldSetInt8Attr
        module procedure ESMF_FieldSetInt8ListAttr
        module procedure ESMF_FieldSetReal4Attr
        module procedure ESMF_FieldSetReal4ListAttr
        module procedure ESMF_FieldSetReal8Attr
        module procedure ESMF_FieldSetReal8ListAttr
        module procedure ESMF_FieldSetLogicalAttr
        module procedure ESMF_FieldSetLogicalListAttr
        module procedure ESMF_FieldSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldGetAttribute  - Get Field attributes
!
! !INTERFACE:
      interface ESMF_FieldGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldGetInt4Attr
        module procedure ESMF_FieldGetInt4ListAttr
        module procedure ESMF_FieldGetInt8Attr
        module procedure ESMF_FieldGetInt8ListAttr
        module procedure ESMF_FieldGetReal4Attr
        module procedure ESMF_FieldGetReal4ListAttr
        module procedure ESMF_FieldGetReal8Attr
        module procedure ESMF_FieldGetReal8ListAttr
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
! !IROUTINE: ESMF_FieldGetAttributeInfo - Get type, count from a Field attribute
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


!
!==============================================================================
!
      contains
!
!==============================================================================


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
#define ESMF_METHOD "ESMF_LocalFieldGetInit"
!BOPI
! !IROUTINE:  ESMF_LocalFieldGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LocalFieldGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocalField), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LocalFieldGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt localfield}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalField} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LocalFieldGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LocalFieldGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LocalFieldGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalFieldInit"
!BOPI
! !IROUTINE:  ESMF_LocalFieldInit - Initialize LocalField

! !INTERFACE:
    subroutine ESMF_LocalFieldInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocalField) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt localfield}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalField} of which being initialized.
!     \end{description}
!
!EOPI
       s%localFlag = .true.

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LocalFieldInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalFieldValidate"
!BOPI
! !IROUTINE:  ESMF_LocalFieldValidate - Check validity of a LocalField

! !INTERFACE:
    subroutine ESMF_LocalFieldValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_LocalField), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt LocalField} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalField} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
     ESMF_INIT_CHECK_SHALLOW(ESMF_LocalFieldGetInit,ESMF_LocalFieldInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LocalFieldValidate


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
        s%interngridstatus    = ESMF_STATUS_UNINIT
        s%datastatus    = ESMF_STATUS_UNINIT
        s%datamapstatus = ESMF_STATUS_UNINIT
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
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
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
#define ESMF_METHOD "ESMF_FieldCreateNoDataPtr"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated data buffer

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoDataPtr(interngrid, arrayspec, horzRelloc, &
                                         vertRelloc, haloWidth, &
                                         datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoDataPtr   
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid                 
      type(ESMF_ArraySpec), intent(inout) :: arrayspec    
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth    
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap    
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
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[haloWidth]}]
!           Halo region width when data is eventually created.  Defaults to 0.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_FieldType), pointer :: ftype      ! Pointer to new field
      integer :: localrc                         
   
      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      nullify(ftype)
      nullify(ESMF_FieldCreateNoDataPtr%ftypep)

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to build field internals.
      call ESMF_FieldConstructNoDataPtr(ftype, interngrid, arrayspec, horzRelloc, &
                                       vertRelloc, haloWidth, datamap, name, &
                                       iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_FieldCreateNoDataPtr%ftypep => ftype

      ESMF_INIT_SET_CREATED(ESMF_FieldCreateNoDataPtr)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoDataPtr

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoArray"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no associated Array object

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoArray(interngrid, horzRelloc, vertRelloc, &
                                       datamap, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoArray 
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid                 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap              
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
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
!     \item [{[name]}] 
!           {\tt Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
 
      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: localrc                    
      
      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      nullify(ftype)
      nullify(ESMF_FieldCreateNoArray%ftypep)

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      allocate(ftype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call field construction method
      call ESMF_FieldConstructNoArray(ftype, interngrid, horzRelloc, vertRelloc, &
                                      datamap, name, &
                                      iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_FieldCreateNoArray%ftypep => ftype

      ESMF_INIT_SET_CREATED(ESMF_FieldCreateNoArray)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldCreateNoInternGridArray"

!BOP
! !IROUTINE: ESMF_FieldCreateNoData - Create a Field with no InternGrid or Array

! !INTERFACE:
      ! Private name; call using ESMF_FieldCreateNoData()
      function ESMF_FieldCreateNoInternGridArray(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Field) :: ESMF_FieldCreateNoInternGridArray 
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     An interface function to {\tt ESMF\_FieldCreateNoData()}.
!     This version of {\tt ESMF\_FieldCreate} builds an empty {\tt ESMF\_Field} 
!     and depends on later calls to add an {\tt ESMF\_InternGrid} and {\tt ESMF\_Array} to 
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


      type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field
      integer :: localrc                     
      
      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      nullify(ftype)
      nullify(ESMF_FieldCreateNoInternGridArray%ftypep)

      allocate(ftype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating Field information", &
                                       ESMF_CONTEXT, rc)) return

      ! Call field construction method
      call ESMF_FieldConstructNoInternGridArray(ftype, name, iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_FieldCreateNoInternGridArray%ftypep => ftype

      ESMF_INIT_SET_CREATED(ESMF_FieldCreateNoInternGridArray)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldCreateNoInternGridArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestroy"

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

      ! Local variables
      integer :: localrc                         

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! TODO: If already destroyed or never created, return ok?
      ! (should it be ok to destroy the same object twice without complaint?)
      ! for now, no, you can't delete an object twice 
      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Destruct all field internals and then free field memory.
      call ESMF_FieldDestruct(field%ftypep, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
           
      if (associated(field%ftypep)) then
         deallocate(field%ftypep, stat=localrc)
         if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating Field", &
                                       ESMF_CONTEXT, rc)) return
      endif 
      ESMF_INIT_SET_DELETED(field)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGet"

!BOP
! !IROUTINE: ESMF_FieldGet - Return info associated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldGet(field, interngrid, array, datamap, horzRelloc, &
                               vertRelloc, haloWidth, iospec, typekind, &
                               rank, lbounds, ubounds, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field    
      type(ESMF_InternGrid), intent(out), optional :: interngrid     
      type(ESMF_InternArray), intent(out), optional :: array     
      type(ESMF_FieldDataMap), intent(out), optional :: datamap     
      type(ESMF_RelLoc), intent(out), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(out), optional :: vertRelloc 
      integer, intent(out), optional :: haloWidth
      type(ESMF_IOSpec), intent(out), optional :: iospec 
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: rank
      integer, dimension(:), intent(out), optional :: lbounds
      integer, dimension(:), intent(out), optional :: ubounds
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
!     \item [{[interngrid]}]
!           {\tt ESMF\_InternGrid}.
!     \item [{[array]}]
!           {\tt ESMF\_Array}.
!     \item [{[datamap]}]
!           {\tt ESMF\_FieldDataMap}.
!     \item [{[horzRelloc}]]
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.
!     \item [{[vertRelloc]}]
!           Relative location of data per interngrid cell/vertex in the vertical interngrid.
!     \item [{[haloWidth]}]
!           Integer value for the width of the halo (ghost zone) region in the
!           data array.  This can also be queried directly from the
!           {\tt ESMF\_Array} object.
!     \item [{[iospec]}]
!           {\tt ESMF\_IOSpec} object which contains settings for options
!     \item [{[typekind]}]
!           TypeKind specifier for Field.
!     \item [{[rank]}]
!           Rank of Field data.
!     \item [{[name]}]
!           Name of queried item.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        type(ESMF_FieldType), pointer :: ftype
        integer :: localrc

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

        ! Validate object first
        call ESMF_FieldValidate(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
 
        ftype => field%ftypep

        if (present(interngrid)) then
            if (ftype%interngridstatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "No InternGrid or invalid InternGrid attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            interngrid = ftype%interngrid
        endif

        if (present(array)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "No data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            array = ftype%localfield%localdata
        endif

        if (present(datamap)) then
            ! TODO: what's the proper test here?  you could have a map w/ no data yet
            !    if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
            !                    "No data attached to Field", &
            !                     ESMF_CONTEXT, rc)) return
            datamap = ftype%mapping
        endif

        if (present(horzRelloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATUS_READY) then
            !    if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
            !                    "No data attached to Field", &
            !                     ESMF_CONTEXT, rc)) return
            !endif
            call ESMF_FieldDataMapGet(ftype%mapping, horzRelloc=horzRelloc, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(vertRelloc)) then
            ! TODO: what's the proper test here?  ditto code above.
            !if (ftype%datastatus .ne. ESMF_STATUS_READY) then
            !  print *, "ERROR: No data attached to Field"
            !  return
            !endif
            call ESMF_FieldDataMapGet(ftype%mapping, vertRelloc=vertRelloc, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

        if (present(haloWidth)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                 "Cannot return haloWidth because no data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            call ESMF_InternArrayGet(ftype%localfield%localdata, &
                               haloWidth=haloWidth, rc=rc)
            if (ESMF_LogMsgFoundError(rc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

        if (present(typekind)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                 "Cannot return haloWidth because no data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            call ESMF_InternArrayGet(ftype%localfield%localdata, &
                               typekind=typekind, rc=rc)
            if (ESMF_LogMsgFoundError(rc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

        if (present(rank)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                 "Cannot return haloWidth because no data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            call ESMF_InternArrayGet(ftype%localfield%localdata, &
                               rank=rank, rc=rc)
            if (ESMF_LogMsgFoundError(rc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

        if (present(lbounds)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                 "Cannot return haloWidth because no data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            call ESMF_InternArrayGet(ftype%localfield%localdata, &
                               lbounds=lbounds, rc=rc)
            if (ESMF_LogMsgFoundError(rc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

        if (present(ubounds)) then
            if (ftype%datastatus .ne. ESMF_STATUS_READY) then
                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                 "Cannot return haloWidth because no data attached to Field", &
                                 ESMF_CONTEXT, rc)) return
            endif
            call ESMF_InternArrayGet(ftype%localfield%localdata, &
                               ubounds=ubounds, rc=rc)
            if (ESMF_LogMsgFoundError(rc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        endif

        if (present(iospec)) iospec = ftype%iospec

        if (present(name)) then
            call c_ESMC_GetName(ftype%base, name, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalArray"

!BOP
! !IROUTINE: ESMF_FieldGetLocalArray - Get Local Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_LocalArray), intent(out) :: array
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Get data in {\tt ESMF\_LocalArray} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           Field {\tt ESMF\_LocalArray}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc 
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_InternArray) :: iarray      

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! Validate first
      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_FieldGetInternArray(field, iarray, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      array%this = iarray%this


      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetLocalArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetInternArray"

!BOP
! !IROUTINE: ESMF_FieldGetInternArray - Get data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldGetInternArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field      
      type(ESMF_InternArray), intent(out) :: array
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Get data in {\tt ESMF\_InternArray} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           Field {\tt ESMF\_InternArray}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP


      integer :: localrc
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      !cheung turn off for now
      !! Validate first
      !call ESMF_FieldValidate(field, rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                          ESMF_ERR_PASSTHRU, &
      !                          ESMF_CONTEXT, rc)) return

      ftypep => field%ftypep

      if (ftypep%datastatus .ne. ESMF_STATUS_READY) then
           if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "No data associated with Field", &
                                 ESMF_CONTEXT, rc)) return
      endif

      !call ESMF_StatusString(ftypep%datastatus, str, rc)
      !print *, "getting array data, status = ", trim(str)
      array = ftypep%localfield%localdata
   
      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetInternArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGetAttribute  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_FieldGetAttribute(field, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Field), intent(inout) :: field  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt field}.
!     Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeGetChar(field%ftypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttributeCount"

!BOP
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
!EOP

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_AttributeGetCount(field%ftypep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetAttrInfoByName"

!BOP
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
!EOP

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_AttributeGetAttrInfoName(field%ftypep%base, name, &
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

!BOP
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
      character(len=*), intent(out), optional :: name
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
!EOP

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_AttributeGetAttrInfoNum(field%ftypep%base, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGlobalInternGridInfo"

!BOPI
! !IROUTINE: ESMF_FieldGetGlobalInternGridInfo - Get information about the Global InternGrid
!
! !INTERFACE:
      subroutine ESMF_FieldGetGlobalInternGridInfo(field, ndim, ncell, nvertex, &
        size, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field      
      integer, intent(out), optional :: ndim            
      integer, intent(out), optional :: ncell            
      integer, intent(out), optional :: nvertex           
      integer, intent(out), optional :: size(:)  
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!     Return global {\tt ESMF\_InternGrid} information. 
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
!           Size of interngrid.
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

!
! TODO: code goes here.  This marked BOPI because it isn't implemented yet,
!   and it may be that a lot of the query routines are not actually
!   implemented at the field level - only the most commonly used ones.
!   the rest are to be implemented by InternGridGet() - the caller gets the interngrid
!   from the field and then queries the interngrid directly.
!
        end subroutine ESMF_FieldGetGlobalInternGridInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalInternGridInfo"

!BOPI
! !IROUTINE: ESMF_FieldGetLocalInternGridInfo - Get information about the Local InternGrid
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalInternGridInfo(field, ndim, ncell, nvertex, & 
        size, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field   
      integer, intent(out), optional :: ndim            
      integer, intent(out), optional :: ncell            
      integer, intent(out), optional :: nvertex           
      integer, intent(out), optional :: size(:)  
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Get {\tt ESMF\_InternGrid} information specific to the local {\tt ESMF\_DE}.
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
!           Size of interngrid.
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

!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalInternGridInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetGlobalDataInfo"

!BOPI
! !IROUTINE: ESMF_FieldGetGlobalDataInfo - Get information about Field Data
!
! !INTERFACE:
      subroutine ESMF_FieldGetGlobalDataInfo(field, size, &
        indexorder, interleave, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field            
      integer, intent(out), optional :: size(:)        
      integer, dimension(ESMF_MAXDIM), intent(out) :: indexorder 
      type(ESMF_InterleaveFlag), intent(out) :: interleave   
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
!     \item [interleave]
!           Data may be ESMF\_IL\_BLOCK or ESMF\_IL\_ITEM.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      ! Initialize
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)
!
! TODO: code goes here.  BOPI because not implemented yet.
!
        end subroutine ESMF_FieldGetGlobalDataInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGetLocalDataInfo"

!BOPI
! !IROUTINE: ESMF_FieldGetLocalDataInfo - Get information about Field Data
!
! !INTERFACE:
      subroutine ESMF_FieldGetLocalDataInfo(field, size, &
        indexorder, interleave, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field            
      integer, intent(out), optional :: size(:)        
      integer, dimension(ESMF_MAXDIM), intent(out) :: indexorder 
      type(ESMF_InterleaveFlag), intent(out) :: interleave   
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
!     \item [interleave]
!           Data may be ESMF\_IL\_BLOCK or ESMF\_IL\_ITEM.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)
!
! TODO: code goes here
!
        end subroutine ESMF_FieldGetLocalDataInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldPrint"

!BOP
! !IROUTINE:  ESMF_FieldPrint - Print the contents of a Field

! !INTERFACE:
      subroutine ESMF_FieldPrint(field, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Prints information about the {\tt field} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        character(len=ESMF_MAXSTR) :: name, str
        type(ESMF_FieldType), pointer :: fp 
        integer :: localrc
        !character(len=ESMF_MAXSTR) :: msgbuf

!	Initialize
	localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

        !nsc call ESMF_LogWrite("Field Print:", ESMF_LOG_INFO)
        write(*,*) "Field Print:"
        if (.not. associated(field%ftypep)) then
        !jw  call ESMF_LogWrite("Empty or Uninitialized Field", ESMF_LOG_INFO)
          write(*,*) "Empty or Uninitialized Field"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        fp => field%ftypep
        call ESMF_BasePrint(fp%base, str, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_StatusString(fp%fieldstatus, str, localrc)
      !jw  write(msgbuf, *)  "Field status = ", trim(str)
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "Field status = ", trim(str)

        if (fp%fieldstatus .ne. ESMF_STATUS_READY) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        call c_ESMC_GetName(fp%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      !jw  write(msgbuf, *)  "  Name = '",  trim(name), "'"
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "  Name = '",  trim(name), "'"

        call ESMF_StatusString(fp%interngridstatus, str, localrc)
      !jw  write(msgbuf, *)  "InternGrid status = ", trim(str)
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "InternGrid status = ", trim(str)
        if (fp%interngridstatus .eq. ESMF_STATUS_READY) then 
           call ESMF_InternGridPrint(fp%interngrid, "", localrc)
        endif

        call ESMF_StatusString(fp%datastatus, str, localrc)
      !jw  write(msgbuf, *)  "Data status = ", trim(str)
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "Data status = ", trim(str)
        !TODO: add code here to print more info
        if (fp%datastatus .eq. ESMF_STATUS_READY) then 
           call ESMF_InternArrayPrint(fp%localfield%localdata, "", localrc)
        endif

        call ESMF_StatusString(fp%datamapstatus, str, localrc)
      !jw  write(msgbuf, *)  "FieldDataMap status = ", trim(str)
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "FieldDataMap status = ", trim(str)
        !TODO: add code here to print more info
        if (fp%datamapstatus .eq. ESMF_STATUS_READY) then 
           call ESMF_FieldDataMapPrint(fp%mapping, "", localrc)
        endif


        ! global field contents
        !type (ESMF_IOSpec) :: iospec             ! iospec values
        !type (ESMF_Status) :: iostatus           ! if unset, inherit from gcomp

        ! local field contents
        !type (ESMF_Mask) :: mask                 ! may belong in InternGrid

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldPrint
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRead"

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
      character (len = *), intent(in) :: fname             ! field name to read
      character (len = *), intent(in), optional :: gname   ! interngrid name
      character (len = *), intent(in), optional :: dnames  ! data name
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!      This includes creating the {\tt ESMF\_InternGrid} associated with this {\tt ESMF\_Field}.
!      To share a single {\tt ESMF\_InternGrid} betwen multiple {\tt ESMF\_Field}s, see the {\tt ESMF\_FieldCreate} calls.
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           An {\tt ESMF\_Field} name.
!     \item [{[gname]}]
!            {\tt ESMF\_InternGrid} name.
!     \item [{[dnames]}]
!            Data name.
!     \item [{[iospec]}]
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      type (ESMF_Field) :: a

      ! Initialize
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_FieldRead = a
!
!     TODO: code goes here
!
      end function ESMF_FieldRead

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
#define ESMF_METHOD "ESMF_FieldSetInternArray"

!BOP
! !IROUTINE: ESMF_FieldSetInternArray - Set data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetInternArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field      
      type(ESMF_InternArray), intent(in) :: array
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
!           {\tt ESMF\_InternArray} containing data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP


      integer :: localrc
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! validate before using
      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
 
      ftypep => field%ftypep

      ! TODO: do we allow this?  if so, do we just destroy the old array?
      !if (ftypep%datastatus .eq. ESMF_STATUS_READY) then
      !   if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
      !                          "Data already associated with Field", &
      !                           ESMF_CONTEXT, rc)) return
      !endif

      ftypep%localfield%localdata = array
      ftypep%datastatus = ESMF_STATUS_READY
   
      ! Now revalidate to be sure the interngrid and datamap, if they exist, are
      ! consistent with the new array.
      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
 
      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetInternArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetLocalArray"

!BOPI
! !IROUTINE: ESMF_FieldSetLocalArray - Set data Array associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetLocalArray(field, array, rc)

!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field      
      type(ESMF_LocalArray), intent(in) :: array
      integer, intent(out), optional :: rc           

!
! !DESCRIPTION:
!     Set data in {\tt ESMF\_LocalArray} form.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[array]}]
!           {\tt ESMF\_LocalArray} containing data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI


      integer :: localrc 
      !character(len=ESMF_MAXSTR) :: str
      type(ESMF_FieldType), pointer :: ftypep

      ! Initialize return code
      localrc = ESMF_RC_NOT_IMPL   
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! validate before using
      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
 
      ftypep => field%ftypep

      ! TODO: do we allow this?  if so, do we just destroy the old array?
      !if (ftypep%arraystatus .eq. ESMF_STATUS_READY) then
      !   if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
      !                          "Data already associated with Field", &
      !                           ESMF_CONTEXT, rc)) return
      !endif

!      ftypep%localfield%localarray = array
!      ftypep%arraystatus = ESMF_STATUS_READY
   
      ! Now revalidate to be sure the interngrid and datamap, if they exist, are
      ! consistent with the new array.
      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
 
      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetLocalArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldSetAttribute - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_FieldSetAttribute(field, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Field), intent(inout) :: field  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt field}.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}.
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
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
!      Attaches a character attribute to the {\tt field}.
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

      call c_ESMC_AttributeSetChar(field%ftypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetInternGrid"

!BOP
! !IROUTINE: ESMF_FieldSetInternGrid - Set InternGrid associated with the Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetInternGrid(field, interngrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      type(ESMF_InternGrid), intent(inout) :: interngrid      
      integer, intent(out), optional :: rc    
!
! !DESCRIPTION:
!  Used only with the version of {\tt ESMF\_FieldCreate} which creates an empty 
!  {\tt ESMF\_Field} and allows the {\tt ESMF\_InternGrid} to be specified later.  
!  Otherwise it is an error to try to change the {\tt ESMF\_InternGrid} 
!  associated with an {\tt ESMF\_Field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [interngrid]
!           {\tt ESMF\_InternGrid} to be added.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        type(ESMF_FieldType), pointer :: ftype
        logical :: had_interngrid
        integer :: localrc

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

        ! Validate first
        call ESMF_FieldValidate(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ftype => field%ftypep

        ! decide if we're regridding or just adding a interngrid to a partially
        ! created field.
        had_interngrid = .FALSE.
        if (ftype%interngridstatus .eq. ESMF_STATUS_READY) had_interngrid = .TRUE.

        if (.not. had_interngrid) then
           ! if no interngrid, just add it
           ftype%interngrid = interngrid
           ftype%interngridstatus = ESMF_STATUS_READY
        else
           ! this could be considered a request to regrid the data
           call ESMF_LogWrite("Replacing existing interngrid not yet supported", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)
           call ESMF_LogWrite("Will be considered a regrid request", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)
        endif

        ! now validate again to be sure that if the field had an existing
        ! array or datamap, that we haven't created an inconsistent object
        call ESMF_FieldValidate(field, "", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldSetInternGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetDataMap"

!BOP
! !IROUTINE: ESMF_FieldSetDataMap - Set DataMap assocated with a Field
!
! !INTERFACE:
      subroutine ESMF_FieldSetDataMap(field, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_FieldDataMap), intent(inout) :: datamap
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
        integer :: localrc
        logical :: had_data

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)
        ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit,datamap)

        ! Validate first
        call ESMF_FieldValidate(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
 
        ! decide if we're reordering data or just setting an initial map
        ! in an already created field without data.  (the latter is ok;
        ! the former is not implemented yet.)
        had_data = .FALSE.
        if (field%ftypep%datastatus .eq. ESMF_STATUS_READY) had_data = .TRUE.

        if (.not. had_data) then
           ! if no datamap, just add it
           field%ftypep%mapping = datamap
        else
           ! this could be considered a request to reorder the data
           call ESMF_LogWrite("Replacing existing datamap not yet supported", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)
           call ESMF_LogWrite("Will be considered a data reorder request", &
                               ESMF_LOG_WARNING, &
                               ESMF_CONTEXT)
           return
        endif

        ! and now revalidate to ensure a consistent datamap
        call ESMF_FieldValidate(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
 

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldSetDataMap

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetDataValues"

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
      real(ESMF_KIND_R4), dimension (:), intent(in) :: value
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

      ! Initialize
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

!       BOP/EOP have been changed to BOPI/EOPI until the subroutine is implemented.
!
! TODO: code goes here
!
        end subroutine ESMF_FieldSetDataValues

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldValidate"

!BOP
! !IROUTINE:  ESMF_FieldValidate - Check validity of a Field

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field 
      character (len = *), intent(in), optional :: options 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt field} is internally consistent.
!      Currently this method determines if the {\tt field} is uninitialized 
!      or already destroyed.  The code also checks if the data and interngrid sizes agree.
!      Currently we allow for 1 point mismatch to accommodate different staggerings.
!      The method returns an error code if problems 
!      are found.  
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
      type(ESMF_Relloc) :: horzRelloc, vertRelloc
      character(len=ESMF_MAXSTR) :: msgbuf
      integer :: interngridcounts(ESMF_MAXIGRIDDIM)   ! how big the local interngrid is
      integer :: arraycounts(ESMF_MAXDIM)      ! how big the local array is
      integer :: maplist(ESMF_MAXDIM)          ! mapping between them
      integer :: otheraxes(ESMF_MAXDIM)        ! counts for non-interngrid dims
      integer :: interngridrank, maprank, arrayrank, halo
      integer :: interngridcellcount, arraycellcount
      logical :: hasinterngrid, hasarray, hasmap     ! decide what we can validate
      integer :: i, j
    
      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (.not.associated(field%ftypep)) then 
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)
         return
      endif 

      ftypep => field%ftypep


      ! make sure the field is ready before trying to look at contents
      if (ftypep%fieldstatus .ne. ESMF_STATUS_READY) then
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Field", &
                                 ESMF_CONTEXT, rc)
         return
      endif 

      ! figure out whether there is a interngrid, datamap, and/or arrays first
      ! before doing tests to be sure they are consistent.
      hasinterngrid = .FALSE.
      hasarray = .FALSE.
      hasmap = .FALSE.

      ! make sure there is data before asking the datamap questions.
      if (ftypep%datamapstatus .eq. ESMF_STATUS_READY) then

          ! get needed info from datamap. 
          call ESMF_FieldDataMapGet(ftypep%mapping, horzRelloc=horzRelloc, &
                                    vertRelloc=vertRelloc, &
                                    dataRank=maprank, dataIndexList=maplist, &
                                    counts=otheraxes, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          hasmap = .TRUE.
      endif

      ! if there is no datamap yet, then default horzRelloc to cell center
      ! before asking theinterngrid for count information
      if (.not. hasmap) then
          horzRelloc = ESMF_CELL_CENTER
      endif

      ! make sure there is a interngrid before asking it questions.
      if (ftypep%interngridstatus .eq. ESMF_STATUS_READY) then

          ! get interngrid dim and extents for the local piece
          call ESMF_InternGridGet(ftypep%interngrid, distDimCount=interngridrank, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          call ESMF_InternGridGetDELocalInfo(ftypep%interngrid, horzRelloc, vertRelloc, &
                                    localCellCountPerDim=interngridcounts, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          hasinterngrid = .TRUE.

          ! compute total number of interngrid items for later
          interngridcellcount = 1
          do i = 1, interngridrank
              interngridcellcount = interngridcellcount * interngridcounts(i)
          enddo
      endif

      ! make sure there is data before asking it questions.
      if (ftypep%datastatus .eq. ESMF_STATUS_READY) then

        if (ftypep%localfield%localFlag) then
        
          ! get array counts and other info
          call ESMF_InternArrayGet(ftypep%localfield%localdata, counts=arraycounts, &
                             haloWidth=halo, rank=arrayrank, rc=localrc)

          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          hasarray = .TRUE.

          ! compute total number of array items for later
          arraycellcount = 1
          do i = 1, arrayrank
              arraycellcount = arraycellcount * arraycounts(i)
          enddo
        endif
      endif

      ! and now see if it is at all consistent.
      if (hasarray .and. hasmap) then
          if (arrayrank .ne. maprank) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                             "rank in fielddatamap must match rank in array", &
                                       ESMF_CONTEXT, rc)
              return
          endif
      endif

      ! if array and datamap and interngrid, check exact counts now
      if (hasarray .and. hasmap .and. hasinterngrid) then

          ! if the array and interngrid cell counts are 0, there is no local
          ! data on the PET and we should look no further.
          if ((arraycellcount .eq. 0) .and. (interngridcellcount .eq. 0)) then
             continue ! ok, no data on this local PET.  
          else
            j = 1
            do i = 1, arrayrank
              if (maplist(i) .eq. 0) then
                  ! maplist is 0 if the axes does not correspond to the interngrid.
                  ! in that case, it must correspond to the counts in the map.
                  ! TODO: for now the halo is added to all axes in the array,
                  ! not just those which correspond to the interngrid.  when we fix
                  ! this, then change this test to ignore halo widths.
                  if ((abs(arraycounts(i)) - (otheraxes(j) + (2*halo))) > 1 ) then
                      if (arraycounts(i) .eq. otheraxes(j)) then
                       write(msgbuf,*) "array index", i, "(", arraycounts(i), &
                                       ") != datamap index", j, &
                                       "(", otheraxes(j) + (2*halo), &
                                       ") including halo width"
                          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, msgbuf, &
                                                   ESMF_CONTEXT, rc)
                          return
                      else 
                       write(msgbuf,*) "array index", i, "(", arraycounts(i), &
                                       ") != datamap index", j, &
                                       "(", otheraxes(j) + (2*halo), ")"
                          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, msgbuf, &
                                                   ESMF_CONTEXT, rc)
                  
                          return
                      endif
                  endif
                  j = j + 1
              else
                  ! maplist is not 0, so this axes does correspond to the interngrid.
                  ! the sizes must match, taking into account the halo widths
                  ! and the index reordering.
                  if (abs(arraycounts(i) - (interngridcounts(maplist(i)) + (2*halo))) > 1 ) then
                      write(msgbuf,*) "array index", i, "(", arraycounts(i), &
                                      ") != interngrid index", maplist(i), "(", &
                                      interngridcounts(maplist(i)) + (2*halo), ")"
                      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, msgbuf, &
                                                ESMF_CONTEXT, rc)
                      return
                  endif
    
              endif
            enddo
          endif
      endif

      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWrite"

!BOP
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
!            I/O specification.
!     \item [{[timestamp]}]
!            A timestamp of type {\tt ESMF\_Time} for the data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

        ! Local variables
        integer :: localrc, de_id
        type(ESMF_InternArray) :: out_array
        type(ESMF_TypeKind) arr_kind
        integer out_rank
        integer out_kind
        integer, dimension(:), pointer :: out_counts
        integer, dimension(:), pointer :: out_lbounds
        integer, dimension(:), pointer :: out_ubounds
        integer, dimension(:), pointer :: out_strides
        type(ESMF_InternGrid) :: interngrid
        type(ESMF_DELayout) :: delayout
        type (ESMF_IOFileFormat) :: fileformat
        type(ESMF_Time) :: ts
        character (19) Date
      
        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)
           
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
        call ESMF_FieldGet(field, interngrid=interngrid, rc=localrc)
!!$        call ESMF_FieldGet( field, name=fieldname, rc=localrc)
        call ESMF_InternGridGet(interngrid, delayout=delayout, rc=localrc)
        call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=localrc)

        ! Output to file, from de_id 0 only
!!$        call ESMF_FieldAllGather(field, out_array, rc=localrc)
        call ESMF_IArrayGather(field%ftypep%localfield%localdata, &
                              field%ftypep%interngrid, field%ftypep%mapping, &
                              0, out_array, rc=localrc)


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
!            I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

        ! Local variables
        integer :: localrc, de_id
        type(ESMF_InternArray) :: outarray
        type(ESMF_InternGrid) :: interngrid
        type(ESMF_DELayout) :: delayout
        character(len=ESMF_MAXSTR) :: filename
        character(len=ESMF_MAXSTR) :: name

        ! TODO: revised code goes here


        ! call ESMF_Log(?, 'entry into ESMF_FieldWrite');

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL 
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

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
        call ESMF_FieldGet(field, interngrid=interngrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        call ESMF_InternGridGet(interngrid, delayout=delayout, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        write(name,'(i1)') de_id
        call ESMF_InternArrayWrite(field%ftypep%localfield%localdata,&
                             filename=trim(name), rc=localrc)

        ! Output to file, from de_id 0 only
        call ESMF_IArrayGather(field%ftypep%localfield%localdata, &
                              field%ftypep%interngrid, field%ftypep%mapping, &
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

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructIANew"

!BOPI
! !IROUTINE: ESMF_FieldConstructIANew - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructIANew(ftype, interngrid, arrayspec, allocflag, &
                                        horzRelloc, vertRelloc, haloWidth, &
                                        datamap, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_InternGrid) :: interngrid               
      type(ESMF_ArraySpec), intent(inout) :: arrayspec     
      type(ESMF_AllocFlag), intent(in), optional :: allocflag
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap           
      character (len=*), intent(in), optional :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
! 
!     Constructs all {\tt ESMF\_Field} internals, including the allocation
!     of a data {\tt ESMF\_Array}.   TODO: this is missing a counts argument,
!     which is required if the arrayspec rank is greater than the interngrid rank.
!     Either that, or we must enforce that a datamap comes in, and it
!     contains the counts for non-interngrid dims.
!
!     The arguments are:
!     \begin{description}
!     \item [ftype]
!           Pointer to an {\tt ESMF\_Field} object.
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[allocflag]}]
!           Allocate space for data array or not.  For possible values
!           see Section~\ref{opt:allocflag}.
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[haloWidth]}] 
!           Maximum halo depth along all edges.  Default is 0.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
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


      integer :: localrc
      type(ESMF_InternArray) :: array                   ! New array
      type(ESMF_RelLoc) :: hRelLoc, vRelLoc
      type(ESMF_FieldDataMap) :: dmap
      integer, dimension(ESMF_MAXDIM) :: interngridcounts, arraycounts
      integer, dimension(ESMF_MAXDIM) :: dimorder, counts
      integer :: hwidth, minRank
      integer :: i, j, arrayRank, interngridRank, baseInternGridRank

      ! Initialize return code   
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit,datamap)

      ! make sure hwidth has a value here.
      if (present(haloWidth)) then
          hwidth = haloWidth
      else
          hwidth = 0
      endif

      ! construct a reasonable datamap first before calling field construct
      call ESMF_ArraySpecGet(arrayspec, rank=arrayRank, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_InternGridGet(interngrid, distDimCount=interngridRank, dimCount=baseInternGridRank, &
                        rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      minRank = min(arrayRank, interngridRank)
      if (present(datamap)) then
          dmap = datamap
      else
          call ESMF_FieldDataMapSetDefault(dmap, minRank, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif

      ! this sets the interngrid status, and the datamap status
      call ESMF_FieldConstructNoArray(ftype, interngrid, horzRelloc, vertRelloc, &
                                      dmap, name, &
                                      iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! make sure hRelLoc has a value before InternGridGetDELocalInfo call
      if (present(horzRelLoc)) then
          hRelLoc = horzRelloc
      else
          if (present(datamap)) then
              call ESMF_FieldDataMapGet(datamap, horzRelLoc=hRelLoc, rc=localrc)
          else
               if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                       "no valid RelLoc in either argument list or datamap", &
                                 ESMF_CONTEXT, rc)) return
          endif
      endif
      if (present(vertRelLoc)) then
          vRelLoc = vertRelloc
      else
          if (present(datamap)) then
              call ESMF_FieldDataMapGet(datamap, vertRelLoc=vRelLoc, rc=localrc)
          else
              if (baseInternGridRank .le. 2) then
                  vRelLoc = ESMF_CELL_UNDEFINED
              else
                  if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
               "no valid vertical RelLoc in either argument list or datamap", &
                                            ESMF_CONTEXT, rc)) return
              endif
          endif
      endif
      call ESMF_InternGridGetDELocalInfo(ftype%interngrid, horzRelLoc=hRelLoc, &
                                   vertRelLoc=vRelLoc, &
                                   localCellCountPerDim=interngridcounts(1:interngridRank), &
                                   rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! get information back from datamap
      call ESMF_FieldDataMapGet(ftype%mapping, dataIndexList=dimorder, &
                                counts=counts, rc=localrc)

      arraycounts(:) = 1
      j = 1
      do i=1, arrayRank
         if (dimorder(i) .eq. 0) then
            arraycounts(i) = counts(j) 
            j = j + 1
         else
            !! TODO: decide if the counts going into ArrayCreate do or do not
            !! include either halo widths or allocation widths.  If so, this
            !! is the right count.  If not, then add hwidths plus awidths.
            !!arraycounts(i) = interngridcounts(dimorder(i))
            arraycounts(i) = interngridcounts(dimorder(i)) + (2 * hwidth)
         endif
      enddo

      array = ESMF_InternArrayCreate(arrayspec, arraycounts, hwidth, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ftype%localfield%localdata = array
      ftype%datastatus = ESMF_STATUS_READY

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructIANew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructIANewArray"

!BOPI
! !IROUTINE: ESMF_FieldConstructIANewArray - Construct the internals of a Field

! !INTERFACE:
      subroutine ESMF_FieldConstructIANewArray(ftype, interngrid, array, horzRelloc, &
                                             vertRelloc, datamap, &
                                             name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldType), pointer :: ftype 
      type(ESMF_InternGrid) :: interngrid               
      type(ESMF_InternArray), intent(in) :: array     
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap           
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
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [array]
!           Data. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      integer :: localrc 
      type(ESMF_Field) :: tfield                  ! temp field for error check

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      ! this validates the interngrid already, no need to validate it first.
      call ESMF_FieldConstructNoArray(ftype, interngrid, horzRelloc, vertRelloc, &
                                      datamap=datamap, name=name, &
                                      iospec=iospec, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! make sure the array is a valid object first.
      call ESMF_InternArrayValidate(array, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      ftype%localfield%localdata = array
      ftype%datastatus = ESMF_STATUS_READY

      ! instead of adding error checking all over the place, call the
      ! validate routine to check sizes of array vs interngrid to be sure
      ! they are consistent.  the tfield is a temp wrapper so we can
      ! call the user level validate
      tfield%ftypep => ftype
      ESMF_INIT_SET_CREATED(tfield)
      call ESMF_FieldValidate(tfield, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          ! rc is already set, we just need to mark that the array
          ! is not valid.
          ftype%datastatus = ESMF_STATUS_INVALID
          return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructIANewArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoDataPtr"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoDataPtr - Construct a Field with no associated buffer

! !INTERFACE:
      subroutine ESMF_FieldConstructNoDataPtr(ftype, interngrid, arrayspec, &
                                           horzRelloc, vertRelloc, haloWidth, &
                                           datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype                
      type(ESMF_InternGrid), intent(inout) :: interngrid               
      type(ESMF_ArraySpec), intent(inout) :: arrayspec     
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      integer, intent(in), optional :: haloWidth 
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap 
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
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [arrayspec]
!           Data specification. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical 
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[haloWidth]}]
!           Width of the halo region around the data.  Defaults to 0.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      integer :: localrc
      integer :: interngridRank, arrayRank

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)

      ! init local flag
      ftype%localfield%localFlag=.true.

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftype%base, "Field", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Check to see interngrid is valid first.
      call ESMF_InternGridValidate(interngrid, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      ftype%interngrid = interngrid
      ftype%interngridstatus = ESMF_STATUS_READY

      call ESMF_InternGridGet(interngrid, distDimCount=interngridRank, rc=localrc)
      if (present(datamap)) then
        ftype%mapping = datamap   ! copy, datamap can be reused by user now
        ! if specified as explicit args to create, they override anything
        ! in the existing datamap
        call ESMF_FieldDataMapSet(ftype%mapping, horzRelloc=horzRelloc, &
                             vertRelloc=vertRelloc, rc=localrc)
      else
          call ESMF_ArraySpecGet(arrayspec, rank=arrayRank, rc=localrc)
          call ESMF_FieldDataMapSetDefault(ftype%mapping, arrayRank, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=localrc)
      endif
      ftype%datamapstatus = ESMF_STATUS_READY

      ! construct the array here - but TODO: we are missing the counts
      ! in case there are non-interngrid axes.  there has to be an additional
      ! counts array which contains counts for any data axes which is
      ! not associated with the interngrid.  e.g. for a 3d data array on a 2d interngrid,
      ! there would be counts(1).  for 4d data, counts(2).
      

      ! If I/O spec is present, copy it into the field object; otherwise just 
      ! initialize the I/O spec in the field object.
      if(present(iospec)) then
        !ESMF_IOSpecCopyInit(ftype%iospec, iospec, localrc)
        !if (ESMF_LogMsgFoundError(localrc, &
        !                          ESMF_ERR_PASSTHRU, &
        !                          ESMF_CONTEXT, rc)) return
      else 
        !ESMF_IOSpecInit(ftype%iospec, localrc)
        !if (ESMF_LogMsgFoundError(localrc, &
        !                          ESMF_ERR_PASSTHRU, &
        !                          ESMF_CONTEXT, rc)) return
      endif

      ftype%fieldstatus = ESMF_STATUS_READY

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldConstructNoDataPtr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoArray"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoArray - Construct a Field with no associated Array

! !INTERFACE:
      subroutine ESMF_FieldConstructNoArray(ftype, interngrid, horzRelloc, &
                                            vertRelloc, &
                                            datamap, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftype   
      type(ESMF_InternGrid), intent(inout) :: interngrid                 
      type(ESMF_RelLoc), intent(in), optional :: horzRelloc 
      type(ESMF_RelLoc), intent(in), optional :: vertRelloc 
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap              
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
!     \item [interngrid] 
!           Pointer to an {\tt ESMF\_InternGrid} object. 
!     \item [{[horzRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the horizontal
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[vertRelloc]}] 
!           Relative location of data per interngrid cell/vertex in the vertical
!           interngrid.  If a relative location is specified both as an argument
!           here as well as set in the {\tt datamap}, this takes priority.
!     \item [{[datamap]}]
!           An {\tt ESMF\_FieldDataMap} which describes the mapping of 
!           data to the {\tt ESMF\_InternGrid}.
!     \item [{[name]}] 
!           {\tt ESMF\_Field} name. 
!     \item [{[iospec]}] 
!           I/O specification. 
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      integer :: localrc
      integer :: interngridRank

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)
     
      ! init local flag
      ftype%localfield%localFlag=.true.

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftype%base, "Field", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Attach interngrid
      call ESMF_InternGridValidate(interngrid, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      ftype%interngrid = interngrid
      ftype%interngridstatus = ESMF_STATUS_READY

      call ESMF_InternGridGet(ftype%interngrid, distDimCount=interngridRank, rc=localrc)
      if (present(datamap)) then
        ! this does a copy, datamap ok for user to delete now
        ftype%mapping = datamap   

        ! take care of override horz and vert rellocs.  if specified both in
        ! the datamap and as explicit args, the arguments take priority.
        call ESMF_FieldDataMapSet(ftype%mapping, horzRelloc=horzRelloc, &
                             vertRelloc=vertRelloc, rc=localrc)
      else
        ! create default datamap with 1-for-1 correspondence to interngrid
        if (interngridRank .eq. 1) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_I, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=localrc)
        else if (interngridRank .eq. 2) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJ, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=localrc)
        else if (interngridRank .eq. 3) then
          call ESMF_FieldDataMapSetDefault(ftype%mapping, ESMF_INDEX_IJK, &
                                horzRelloc=horzRelloc, &
                                vertRelloc=vertRelloc, rc=localrc)
        endif
      endif
      ftype%datamapstatus = ESMF_STATUS_READY

!
! add more code here
!
     
      ftype%fieldstatus = ESMF_STATUS_READY

      if  (present(rc)) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldConstructNoInternGridArray"

!BOPI
! !IROUTINE: ESMF_FieldConstructNoInternGridArray - Construct a Field with no InternGrid or Array
!
! !INTERFACE:
      subroutine ESMF_FieldConstructNoInternGridArray(ftypep, name, iospec, rc)
!
! !ARGUMENTS:     
      type(ESMF_FieldType), pointer :: ftypep
      character (len = *), intent(in), optional :: name  
      type(ESMF_IOSpec), intent(in), optional :: iospec  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
! 
!     Constructs {\tt ESMF\_Field} internals except those related to {\tt ESMF\_InternGrid} 
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


      ! Local variables
      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(ftypep%base, "Field", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Initialize field contents
      ftypep%interngridstatus = ESMF_STATUS_UNINIT
      ftypep%datastatus = ESMF_STATUS_UNINIT
      ftypep%datamapstatus = ESMF_STATUS_UNINIT

      ! Init Local flag 
      ftypep%localfield%localFlag=.true.

      ! Set the mapping as unknown/invalid
      call ESMF_FieldDataMapSetInvalid(ftypep%mapping, localrc)

      ftypep%fieldstatus = ESMF_STATUS_READY

!
! add more code here
!
     
      if (present(rc)) rc = ESMF_SUCCESS
      
      end subroutine ESMF_FieldConstructNoInternGridArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldDestruct"

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

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


      ! release the base class resources
      call ESMF_BaseDestroy(ftype%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

!
! TODO: more code goes here
!


      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldDestruct
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBoxIntersect"

!BOPI
! !IROUTINE: ESMF_FieldBoxIntersect - Intersect bounding boxes
!
! !INTERFACE:
      subroutine ESMF_FieldBoxIntersect(srcField, dstField, recvDomainlist, &
                                        sendDomainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: srcField 
      type(ESMF_Field), intent(inout) :: dstField
      type(ESMF_DomainList), intent(inout) :: recvDomainlist
      type(ESMF_DomainList), intent(inout) :: sendDomainlist
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Clips the src\_field physgrid box against the clip\_field, i.e. returns
!      a description of the area in clip\_field which is necessary to cover the
!      desired area in src\_field.  This procedure is mostly an entry point;
!      most of the work is done in the {\tt ESMF\_InternGrid} class.
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
      type(ESMF_DELayout) :: interngridDELayout
      type(ESMF_InternGrid) :: srcInternGrid, dstInternGrid
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      type(ESMF_RelLoc) :: dstHorzRelLoc, dstVertRelLoc, &
                           srcHorzRelLoc, srcVertRelLoc
      type(ESMF_VM) :: vm

      ! Initialize return code
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,srcField,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,dstField,rc)

      ! TODO: replace this with a better way to get the current VM
      call ESMF_InternGridGet(srcField%ftypep%interngrid, delayout=interngridDELayout, rc=localrc)
      call ESMF_DELayoutGet(interngridDELayout, vm=vm, rc=localrc)

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      hasdata = ESMF_TRUE   ! temp for now to get rid of warning
      hassrcdata = (hasdata .eq. ESMF_TRUE)
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
        call ESMF_FieldGet(srcField, horzRelloc=srcHorzRelLoc, &
                           vertRelloc=srcVertRelLoc, interngrid=srcInternGrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      hasdstdata = (hasdata .eq. ESMF_TRUE)
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
        call ESMF_FieldGet(dstField, horzRelloc=dstHorzRelLoc, &
                           vertRelloc=dstVertRelLoc, interngrid=dstInternGrid, rc=localrc)
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
        ! From the interngrid get the bounding box on this DE
        call ESMF_InternGridBoxIntersectSend(srcInternGrid, dstInternGrid, sendDomainList, &
                                       total=.false., layer=.false., rc=localrc)
      endif

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
        call ESMF_InternGridBoxIntersectRecv(srcInternGrid, dstInternGrid, vm, recvDomainList, &
                                       hasdstdata, hassrcdata, &
                                       total=.false., layer=.false., rc=localrc)
      endif

      if  (present(rc)) rc = ESMF_SUCCESS

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

#if 0
      ! TODO: these are currently unused and are not serialized or deserialized.
      type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_Mask) :: mask                 ! may belong in InternGrid
        integer :: rwaccess                      ! reserved for future use
        integer :: accesscount                   ! reserved for future use
#endif

      call c_ESMC_BaseSerialize(fp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldSerialize(fp%fieldstatus, fp%interngridstatus, fp%datastatus, &
                                 fp%datamapstatus, fp%iostatus, &
                                 buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if (fp%interngridstatus .eq. ESMF_STATUS_READY) then
          call ESMF_InternGridSerialize(fp%interngrid, buffer, length, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (fp%datamapstatus .eq. ESMF_STATUS_READY) then
          call ESMF_FieldDataMapSerialize(fp%mapping, buffer, length, &
                                          offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

    ! TODO: if shallow, call C directly?
      !call ESMF_IOSpecSerialize(fp%iospec, buffer, length, offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

      if (fp%datastatus .eq. ESMF_STATUS_READY) then
          call c_ESMC_IArraySerializeNoData(fp%localfield%localdata, buffer(1),&
                                           length, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      !call c_ESMC_IArraySpecSerialize(fp%localfield%arrayspec, buffer, length, &
      !                               offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

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

      ! in case of error, make sure this is invalid.
      nullify(ESMF_FieldDeserialize%ftypep)

      ! shortcut to internals
      allocate(fp, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                     "space for new Field object", &
                                     ESMF_CONTEXT, rc)) return


#if 0
      ! TODO: these are currently unused and not serialized.
      type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_Mask) :: mask                 ! may belong in InternGrid
        integer :: rwaccess                      ! reserved for future use
        integer :: accesscount                   ! reserved for future use
#endif
    
      
      ESMF_INIT_SET_DEFINED(fp%localfield)  ! indicate that this localfield
      fp%localfield%localFlag = .false.     ! is a proxy object

      call ESMF_BaseCreate(fp%base, "Field", "dummy", 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! this overwrites the name and adds attributes to the base obj.
      call c_ESMC_BaseDeserialize(fp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldDeserialize(fp%fieldstatus, fp%interngridstatus, &
                                   fp%datastatus, fp%datamapstatus, &
                                   fp%iostatus, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if (fp%interngridstatus .eq. ESMF_STATUS_READY) then
          fp%interngrid = ESMF_InternGridDeserialize(vm, buffer, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (fp%datamapstatus .eq. ESMF_STATUS_READY) then
          call ESMF_FieldDataMapDeserialize(fp%mapping, buffer, &
                                          offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

    ! TODO: if shallow, call C directly?
      !call ESMF_IOSpecDeserialize(fp%iospec, buffer, offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

      if (fp%datastatus .eq. ESMF_STATUS_READY) then
          call c_ESMC_IArrayDeserializeNoData(fp%localfield%localdata, &
                                       buffer(1), offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif
    
    ! TODO: if shallow, call C directly?
      !call ESMF_ArraySpecDeserialize(fp%arrayspec, buffer, offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

      ESMF_FieldDeserialize%ftypep => fp
      ESMF_INIT_SET_CREATED(ESMF_FieldDeserialize)
      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldDeserialize
!------------------------------------------------------------------------------

      end module ESMF_FieldMod

