! $Id: ESMF_State.F90,v 1.226 2010/12/15 00:57:38 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_State.F90"
!
!     ESMF State module
module ESMF_StateMod
!
!==============================================================================
!
! This file contains the State class definition and all State
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"

#define ESMF_ENABLESTATENEEDED
! #define ESMF_ENABLENAMEMAP
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateMod - Data exchange between components
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to the {\tt State} class and associated data structures.
!
!
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_ArrayBundleMod
      use ESMF_FieldMod
      use ESMF_FieldGetMod
      use ESMF_FieldCreateMod
      use ESMF_FieldBundleMod
      use ESMF_RHandleMod
      use ESMF_StateTypesMod
      use ESMF_StateVaMod
      use ESMF_InitMacrosMod
      use ESMF_IO_NetCDFMod
      use ESMF_IOUtilMod
      use ESMF_UtilMod
      
      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State               ! implemented in ESMF_StateTypesMod

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateCreate, ESMF_StateDestroy
      
      public ESMF_StateDestruct    ! for ESMF garbage collection

      public ESMF_StateAdd
      public ESMF_StateGet
!      public ESMF_StateIsReconcileNeeded
!      public ESMF_StateRemove
      public ESMF_StateReplace

#define ESMF_ENABLESTATENEEDED
#if defined (ESMF_ENABLESTATENEEDED)
      public ESMF_StateGetNeeded
      public ESMF_StateIsNeeded
#endif

      public ESMF_StateWriteRestart
      public ESMF_StateReadRestart

      public ESMF_StateRead
      public ESMF_StateWrite
      public ESMF_StatePrint

      public ESMF_StateSerialize, ESMF_StateDeserialize

      public ESMF_StateClassFindData
      
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_State.F90,v 1.226 2010/12/15 00:57:38 w6ws Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAdd -- Add items to a State

! !INTERFACE:
  interface ESMF_StateAdd

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_StateAddOneArray
    module procedure ESMF_StateAddOneArrayX
    module procedure ESMF_StateAddArrayList

    module procedure ESMF_StateAddOneArrayBundle
    module procedure ESMF_StateAddOneArrayBundleX
    module procedure ESMF_StateAddArrayBundleList

    module procedure ESMF_StateAddOneField
    module procedure ESMF_StateAddOneFieldX
    module procedure ESMF_StateAddFieldList

    module procedure ESMF_StateAddOneFieldBundle
    module procedure ESMF_StateAddOneFieldBundleX
    module procedure ESMF_StateAddFieldBundleList

    module procedure ESMF_StateAddOneName
    module procedure ESMF_StateAddNameList

    module procedure ESMF_StateAddOneRouteHandle
    module procedure ESMF_StateAddRouteHandleList

    module procedure ESMF_StateAddOneState
    module procedure ESMF_StateAddOneStateX
    module procedure ESMF_StateAddStateList


! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_StateAdd} functions.   
!  
!EOPI 
  end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateGet -- Get items from a State

! !INTERFACE:
  interface ESMF_StateGet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_StateGetArray
      module procedure ESMF_StateGetArrayBundle
      module procedure ESMF_StateGetField
      module procedure ESMF_StateGetFieldBundle
      module procedure ESMF_StateGetRouteHandle
      module procedure ESMF_StateGetState
      module procedure ESMF_StateGetInfo
      module procedure ESMF_StateGetItemInfo


! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_StateGet} functions.   
!  
!EOPI 
  end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateReplace -- Replace items in a State

! !INTERFACE:
  interface ESMF_StateReplace

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_StateRepOneArray
    module procedure ESMF_StateRepOneArrayBundle
    module procedure ESMF_StateRepOneField
    module procedure ESMF_StateRepOneFieldBundle
    module procedure ESMF_StateRepOneState


! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_StateReplace} functions.   
!  
!EOPI 
  end interface



!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAdd - Add a single item to a State
!
! !INTERFACE:
!  subroutine ESMF_StateAdd(state, <item>, rc)
!
! !ARGUMENTS:
!    type(ESMF_State), intent(inout)          :: state
!    <item>, see below for supported values
!    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Add a reference to a single <item> to an existing 
!      {\tt state}.  Any of the supported <item>s can
!      be marked needed for a particular run using the
!      {\tt ESMF\_StateSetNeeded()} call.  The name of the
!      <item> must be unique within the {\tt state}.  
!
!      One of the supported options below is to add only the name of the
!      item to the {\tt state} during a first pass.  The name can be
!      replaced with the actual <item> in a later call.
!      When doing this, the name of the <item> provided to the
!      {\tt state} during the first pass must match the name stored
!      in the <item> itself.
!
!      Supported values for <item> are:
!      \begin{description}
!      \item type(ESMF\_Array), intent(in)            :: array
!      \item type(ESMF\_ArrayBundle), intent(in)      :: arraybundle
!      \item type(ESMF\_Field), intent(in)            :: field
!      \item type(ESMF\_FieldBundle), intent(in)      :: fieldbundle
!      \item character (len=*), intent(in)            :: name
!      \item type(ESMF\_RouteHandle), intent(in)      :: routehandle
!      \item type(ESMF\_State), intent(in)            :: nestedState
!      \end{description}
!
! The arguments are:
! \begin{description}
! \item[state]
!      The {\tt ESMF\_State} to which <item>s will be added.
! \item[<item>]
!      The <item> to be added.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneArray"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add an Array to a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateAdd()   
  subroutine ESMF_StateAddOneArray(state, array, rc)
!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)          :: state
    type(ESMF_Array), intent(in)             :: array
    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!  Add a single {\tt array} reference to an existing 
!  {\tt state}.  The {\tt array} name must be unique 
!  within the {\tt state}.
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[array]
!     The {\tt ESMF\_Array} to be added.  This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the objects contained in it will
!     not be destroyed.   Also, the {\tt ESMF\_Array} cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, array, proxyflag=.false., rc=rc)

  end subroutine ESMF_StateAddOneArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneArrayX"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add an Array to a State with proxyflag
!
! !INTERFACE:
  ! Private name; call using ESMF_StateAdd()   
  subroutine ESMF_StateAddOneArrayX(state, array,  &
                                    proxyflag, replaceflag, rc)
!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)          :: state
    type(ESMF_Array), intent(in)             :: array
    logical,          intent(in)             :: proxyflag
    logical,          intent(in),   optional :: replaceflag
    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!  Add a single {\tt array} reference to an existing 
!  {\tt state}.  The {\tt array} name must be unique 
!  within the {\tt state}.
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[array]
!     The {\tt ESMF\_Array} to be added.  This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the objects contained in it will
!     not be destroyed.   Also, the {\tt ESMF\_Array} cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
! \item[proxyflag]
!     Indicate whether this is a proxy object. 
! \item[replaceflag]
!     If set, indicates a replacement operation where there must be an
!     pre-existing item with the same name.  If there is no pre-existing
!     item, an error is returned.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
      type(ESMF_Array) :: temp_list(1)
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange
      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = array

      call ESMF_StateClsAddArrayList(state%statep, 1, temp_list, &
        replaceflag=replaceflag, proxyflag=proxyflag, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(state%statep%base, array, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'array'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_StateAddOneArrayX

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneArrayBundle"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add an ArrayBundle to a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateAdd()   
  subroutine ESMF_StateAddOneArrayBundle(state, arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_State),       intent(inout)          :: state
    type(ESMF_ArrayBundle), intent(in)             :: arraybundle
    integer,                intent(out),  optional :: rc
!     
! !DESCRIPTION:
!  Add a single {\tt arraybundle} reference to an existing 
!  {\tt state}.  The {\tt arraybundle} name must be unique 
!  within the {\tt state}.
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[arraybundle]
!     The {\tt ESMF\_ArrayBundle} to be added.  This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the objects contained in it will
!     not be destroyed.   Also, the {\tt ESMF\_ArrayBundle} cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, arraybundle,  &
                        proxyflag=.false., rc=rc)

  end subroutine ESMF_StateAddOneArrayBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneArrayBundleX"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add an ArrayBundle to a State with proxyflag
!
! !INTERFACE:
  ! Private name; call using ESMF_StateAdd()   
  subroutine ESMF_StateAddOneArrayBundleX(state, arraybundle,  &
                                          proxyflag, replaceflag, rc)
!
! !ARGUMENTS:
    type(ESMF_State),       intent(inout)          :: state
    type(ESMF_ArrayBundle), intent(in)             :: arraybundle
    logical,                intent(in)             :: proxyflag
    logical,                intent(in),   optional :: replaceflag
    integer,                intent(out),  optional :: rc
!     
! !DESCRIPTION:
!  Add a single {\tt arraybundle} reference to an existing 
!  {\tt state}.  The {\tt arraybundle} name must be unique 
!  within the {\tt state}.
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[arraybundle]
!     The {\tt ESMF\_ArrayBundle} to be added.  This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the objects contained in it will
!     not be destroyed.   Also, the {\tt ESMF\_ArrayBundle} cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
! \item[proxyflag]
!     Indicate whether this is a proxy object. 
! \item[replaceflag]
!     If set, indicates a replacement operation where there must be an
!     pre-existing item with the same name.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
      type(ESMF_ArrayBundle) :: temp_list(1)
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange
      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayBundleGetInit,arraybundle,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = arraybundle

      call ESMF_StateClsAddArrayBundleList(state%statep, 1, temp_list, &
        replaceflag=replaceflag, proxyflag=proxyflag, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(state%statep%base, arraybundle, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! set the import and export Attributes on any Array connected to this State
      lobject = 'array'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_StateAddOneArrayBundleX

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneField"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a Field to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneField(state, field, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      type(ESMF_Field), intent(in)            :: field
      integer,          intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt field} reference to an existing 
!      {\tt state}.
!      The {\tt field} name must be unique within the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[field]
!      The {\tt ESMF\_Field} to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Field} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

        call ESMF_StateAdd (state, field,  &
                            proxyflag=.false., rc=rc)

      end subroutine ESMF_StateAddOneField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneFieldX"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a Field to a State with proxyflag
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneFieldX(state, field,  &
                                        proxyflag, replaceflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      type(ESMF_Field), intent(in)            :: field
      logical,          intent(in)            :: proxyflag
      logical,          intent(in),  optional :: replaceflag
      integer,          intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt field} reference to an existing 
!      {\tt state}.
!      The {\tt field} name must be unique within the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[field]
!      The {\tt ESMF\_Field} to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Field} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[proxyflag]
!      Indicate whether this is a proxy object. 
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
      type(ESMF_Field) :: temp_list(1)
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange
      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = field

      call ESMF_StateClsAddFieldList(state%statep, 1, temp_list, &
        replaceflag=replaceflag, proxyflag=proxyflag, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(state%statep%base, field%ftypep%base, &
        linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'field'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddOneFieldX

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneFieldBundle"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a FieldBundle to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneFieldBundle(state, fieldbundle, rc)
!
! !ARGUMENTS:
      type(ESMF_State),       intent(inout)         :: state
      type(ESMF_FieldBundle), intent(in)            :: fieldbundle
      integer,                intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt fieldbundle} reference to an existing 
!      {\tt state}.
!      The {\tt fieldbundle} name must be unique within the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      The {\tt ESMF\_State} object.
!     \item[fieldbundle]
!      The {\tt ESMF\_FieldBundle} to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_FieldBundle} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

        call ESMF_StateAdd (state, fieldBundle,  &
                            proxyflag=.false., rc=rc)

      end subroutine ESMF_StateAddOneFieldBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneFieldBundleX"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a FieldBundle to a State with proxyflag
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneFieldBundleX(state, fieldbundle,  &
                                              proxyflag, replaceflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State),       intent(inout)         :: state
      type(ESMF_FieldBundle), intent(in)            :: fieldbundle
      logical,                intent(in)            :: proxyflag
      logical,                intent(in),  optional :: replaceflag
      integer,                intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt fieldbundle} reference to an existing 
!      {\tt state}.
!      The {\tt fieldbundle} name must be unique within the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      The {\tt ESMF\_State} object.
!     \item[fieldbundle]
!      The {\tt ESMF\_FieldBundle} to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_FieldBundle} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[proxyflag]
!      Indicate whether this is a proxy object. 
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

      type(ESMF_FieldBundle) :: temp_list(1)
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange
      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = fieldbundle

      call ESMF_StateClAddFieldBundleList(state%statep, 1, temp_list, &
        replaceflag=replaceflag, proxyflag=proxyflag, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(state%statep%base, &
        fieldbundle%btypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'field'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddOneFieldBundleX

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneName"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a name to a State as a placeholder
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneName(state, name, rc)
!
! !ARGUMENTS:
      type(ESMF_State),  intent(inout)         :: state
      character (len=*), intent(in)            :: name
      integer,           intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add the character string {\tt name} to an existing {\tt state}.
!      It can subsequently be replaced by an actual object with the
!      same name.
!      The {\tt name} must be unique within the {\tt state}.
!      It is available to be marked needed by the
!      consumer of the export {\tt ESMF\_State}. Then the data 
!      provider can replace the name with the actual {\tt ESMF\_FieldBundle},
!      {\tt ESMF\_Field}, or {\tt ESMF\_Array} which carries the needed data.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[name]
!      The name to be added as a placeholder for a data object.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
      integer :: localrc
      character(len=ESMF_MAXSTR) :: temp_list(1)
      
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = name

      call ESMF_StateAddNameList(state, temp_list, 1, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddOneName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneRouteHandle"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a RouteHandle to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneRouteHandle(state, routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_State),       intent(inout)         :: state
      type(ESMF_RouteHandle), intent(in)            :: routehandle
      integer,                intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single RouteHandle reference to an existing 
!      State.  The name of {\tt routehandle} must be unique 
!      within the State.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[routehandle]
!      The {\tt ESMF\_RouteHandle} to be added.  This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_RouteHandle} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
      type(ESMF_RouteHandle) :: temp_list(1)
      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit,routehandle,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = routehandle

      call ESMF_StateClsAddRHandleList(state%statep, 1, temp_list, &
        rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddOneRouteHandle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneState"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a State to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneState(state, nestedState, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      type(ESMF_State), intent(in)            :: nestedState
      integer,          intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a {\tt nestedState} reference to an existing 
!      {\tt state}.
!      The {\tt nestedState} name must be unique within the 
!      container {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.  This is the container object.
!     \item[nestedState]
!      The {\tt ESMF\_State} to be added.  This is the nested object.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, nested {\tt ESMF\_State}s cannot be safely 
!      destroyed before the container {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

        call ESMF_StateAdd (state, nestedState,  &
                            proxyflag=.false., rc=rc)

      end subroutine ESMF_StateAddOneState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneStateX"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a State to a State with proxyflag
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddOneStateX(state, nestedState,  &
                                        proxyflag, replaceflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      type(ESMF_State), intent(in)            :: nestedState
      logical,          intent(in)            :: proxyflag
      logical,          intent(in),  optional :: replaceflag
      integer,          intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a {\tt nestedState} reference to an existing 
!      {\tt state}.
!      The {\tt nestedState} name must be unique within the 
!      container {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.  This is the container object.
!     \item[nestedState]
!      The {\tt ESMF\_State} to be added.  This is the nested object.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, nested {\tt ESMF\_State}s cannot be safely 
!      destroyed before the container {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[proxyflag]
!      Indicate whether this is a proxy object. 
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
      type(ESMF_State) :: temp_list(1)
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange
      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,nestedState,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      temp_list(1) = nestedState

      call ESMF_StateClsAddStateList(state%statep, 1, temp_list, &
        replaceflag=replaceflag, proxyflag=proxyflag, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(state%statep%base, nestedstate%statep%base, &
        linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'field'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddOneStateX

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAdd - Add a list of items to a State
!
! !INTERFACE:
!  subroutine ESMF_StateAdd(state, <itemList>, count, rc)
!
! !ARGUMENTS:
!    type(ESMF_State), intent(inout)          :: state 
!    <itemList>, see below for supported values
!    integer,          intent(in),   optional :: count
!    integer,          intent(out),  optional :: rc     
!
! !DESCRIPTION:
! Add a list of items to an {\tt ESMF\_State}.
!    
!    Supported values for <itemList> are:   
!    \begin{description}
!    \item type(ESMF\_Array), intent(in)             :: arrayList(:)
!    \item type(ESMF\_ArrayBundle), intent(in)       :: arraybundleList(:)
!    \item type(ESMF\_Field), intent(in)             :: fieldList(:)
!    \item type(ESMF\_FieldBundle), intent(in)       :: fieldbundleList(:)
!    \item character (len=*), intent(in)             :: nameList(:)
!    \item type(ESMF\_RouteHandle), intent(in)       :: routehandleList(:)
!    \item type(ESMF\_State), intent(in)             :: stateList(:)
!    \end{description}
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} to which the <itemList> will be added.
! \item[<itemList>]
!     The list of items to be added.
!     This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the <itemList> contained in it will
!     not be destroyed.   Also, the <itemList> cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since <itemList>s can be added to multiple containers, it remains
!     the user's responsibility to manage their
!     destruction when they are no longer in use.
! \item[{[count]}]
!     The number of items to be added. By default equal to the
!     size of the <itemList> argument.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddArrayList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of Arrays to a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateAdd()   
  subroutine ESMF_StateAddArrayList(state, arrayList, count, rc)
!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)          :: state 
    type(ESMF_Array), intent(in)             :: arrayList(:)
    integer,          intent(in),   optional :: count
    integer,          intent(out),  optional :: rc     
!
! !DESCRIPTION:
! Add multiple {\tt ESMF\_Array}s to an {\tt ESMF\_State}.
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[arrayList]
!     The list (Fortran array) of {\tt ESMF\_Array}s to be added.
!     This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the objects contained in it will
!     not be destroyed.   Also, the {\tt ESMF\_Array}s cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
! \item[{[count]}]
!     The number of {\tt ESMF\_Array}s to be added. By default equal to the
!     size of {\tt arrayList} argument.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
      integer :: localrc, i
      integer :: localcount
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
                                  
      localcount = size(arrayList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of arrayList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      do i=1,localcount
         ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,arrayList(i),rc)
      enddo

      call ESMF_StateClsAddArrayList(state%statep, localcount, arrayList, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return

      ! link the Attribute hierarchies
      linkChange = ESMF_TRUE
      do i=1,count
         call c_ESMC_AttributeLink(state%statep%base, &
          arrayList(i), linkChange, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
      enddo

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'array'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_StateAddArrayList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddArrayBundleList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of ArrayBundles to a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateAdd()   
  subroutine ESMF_StateAddArrayBundleList(state, arraybundleList, &
    count, rc)
!
! !ARGUMENTS:
    type(ESMF_State),       intent(inout)          :: state 
    type(ESMF_ArrayBundle), intent(in)             :: arraybundleList(:)
    integer,                intent(in),   optional :: count
    integer,                intent(out),  optional :: rc     
!
! !DESCRIPTION:
! Add multiple {\tt ESMF\_ArrayBundle}s to an {\tt ESMF\_State}.
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[arraybundleList]
!     The list (Fortran array) of {\tt ESMF\_ArrayBundle}s to be added.
!     This is a reference only; when
!     the {\tt ESMF\_State} is destroyed the objects contained in it will
!     not be destroyed.   Also, the {\tt ESMF\_ArrayBundle}s cannot be safely 
!     destroyed before the {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
! \item[{[count]}]
!     The number of {\tt ESMF\_ArrayBundle}s to be added. By default equal to the
!     size of {\tt arraybundleList} argument.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
      integer :: localrc, i
      integer :: localcount
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
                                  
      localcount = size(arraybundleList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of arraybundleList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      do i=1,localcount
         ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit,arraybundleList(i),rc)
      enddo

      call ESMF_StateClsAddArrayBundleList(state%statep, localcount,&
        arraybundleList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return

      ! link the Attribute hierarchies
      linkChange = ESMF_TRUE
      do i=1,localcount
         call c_ESMC_AttributeLink(state%statep%base, &
          arraybundleList(i), linkChange, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
      enddo

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'array'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_StateAddArrayBundleList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddFieldList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of Fields to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddFieldList(state, fieldList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      type(ESMF_Field), dimension(:), intent(inout) :: fieldList
      integer, intent(in),  optional :: count
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple {\tt ESMF\_Field}s to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[fieldList]
!      The list (Fortran array) of {\tt ESMF\_Field}s to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Field}s cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[count]
!      The number of {\tt ESMF\_Field}s to be added.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc,i
      integer :: localcount
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input variables
                                  
      localcount = size(fieldList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of fieldList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      do i=1,localcount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fieldList(i),rc)
      enddo

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClsAddFieldList(state%statep, localcount, fieldList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! link the Attribute hierarchies
      linkChange = ESMF_TRUE
      do i=1,localcount
         call c_ESMC_AttributeLink(state%statep%base, &
          fieldList(i)%ftypep%base, linkChange, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
      enddo

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'field'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddFieldBundleList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of FieldBundles to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddFieldBundleList(state, fieldbundleList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      type(ESMF_FieldBundle), dimension(:), intent(inout) :: fieldbundleList
      integer, intent(in),  optional :: count
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple {\tt ESMF\_FieldBundle}s to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[fieldbundleList]
!      The list (Fortran array) of {\tt ESMF\_FieldBundle}s to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_FieldBundle}s cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[count]
!      The number of {\tt ESMF\_FieldBundle}s to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc,i
      integer :: localcount
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input variables
                                  
      localcount = size(fieldbundleList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of fieldbundleList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      do i=1,localcount
         ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit,fieldbundleList(i),rc)
      enddo

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClAddFieldBundleList(state%statep, &
                                          localcount, fieldbundleList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! link the Attribute hierarchies
      linkChange = ESMF_TRUE
      do i=1,localcount
         call c_ESMC_AttributeLink(state%statep%base, &
          fieldbundleList(i)%btypep%base, linkChange, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
      enddo

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'field'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddFieldBundleList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddNameList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of names to a State 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddNameList(state, nameList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      character (len=*), intent(in) :: nameList(:)
      integer, intent(in), optional :: count
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a list of names to an existing {\tt state}.
!      They can subsequently be replaced by actual objects with 
!      the same name.
!      Each name in the {\tt nameList} must be unique within 
!      the {\tt state}
!      It is available to be marked needed by the
!      consumer of the export {\tt ESMF\_State}. Then the data 
!      provider can replace the name with the actual {\tt ESMF\_FieldBundle},
!      {\tt ESMF\_Field}, or {\tt ESMF\_Array} which carries the needed data.
!      Unneeded data need not be generated.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[nameList]
!      A list (Fortran array) of character strings to be added
!      as placeholders for data objects.
!     \item[count]
!      The count of names in the {\tt nameList}.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: localrc
      integer :: localcount

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      localcount = size(nameList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of State nameList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      call ESMF_StateClsAddDataNameList(state%statep, localcount, &
                  namelist, rc=localrc)      
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddNameList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddRouteHandleList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of RouteHandles to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddRouteHandleList(state, &
        routehandleList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      type(ESMF_RouteHandle), dimension(:), intent(in) :: routehandleList
      integer, intent(in),  optional :: count
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!     Add multiple {\tt ESMF\_RouteHandle}s to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[routehandleList]
!      The list (Fortran array) of {\tt ESMF\_RouteHandle}s to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_RouteHandle}s cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[count]
!      The number of {\tt ESMF\_RouteHandle}s to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc,i
      integer :: localcount

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      do i=1,count
         ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit,routehandleList(i),rc)
      enddo

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      localcount = size(routehandleList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of routehandleList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      call ESMF_StateClsAddRHandleList(state%statep, localcount, &
        routehandleList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddRouteHandleList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddStateList"
!BOPI
! !IROUTINE: ESMF_StateAdd - Add a list of States to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAdd()   
      subroutine ESMF_StateAddStateList(state, nestedStateList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      type(ESMF_State), dimension(:), intent(in) :: nestedStateList
      integer, intent(in),  optional :: count
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!     Add multiple nested {\tt ESMF\_State}s to a container {\tt ESMF\_State}.
!     The nested {\tt ESMF\_State} names must be unique within the 
!     container {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.  This is the container object.
!     \item[nestedStateList]
!      The list (Fortran array) of {\tt ESMF\_State}s to be added.
!      This is a reference only; when the container {\tt state} is 
!      destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt nestedStateList} cannot be safely 
!      destroyed before the container {\tt state} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[count]
!      The number of {\tt ESMF\_State}s to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc,i
      integer :: localcount
      character(ESMF_MAXSTR) :: lobject, lname, lvalue1, lvalue2
      type(ESMF_Logical) :: linkChange

      ! check input variables

      localcount = size(nestedStateList)
      if (present(count)) then
        if (count < 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be positive", &
            ESMF_CONTEXT, rc)
          return
        else if (count > localcount) then
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            "- count must be smaller than or equal to the size of nestedStateList", &
            ESMF_CONTEXT, rc)
          return
        else
          localcount = count
        end if
      end if

      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
      do i=1,localcount
         ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,nestedStateList(i),rc)
      enddo

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClsAddStateList(state%statep, localcount, &
                                      nestedStateList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! link the Attribute hierarchies
      linkChange = ESMF_TRUE
      do i=1,localcount
         call c_ESMC_AttributeLink(state%statep%base, &
          nestedStateList(i)%statep%base, linkChange, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
      enddo

      ! set the import and export Attributes on any Field connected to this State
      lobject = 'field'
      lvalue1 = 'Import' 
      lvalue2 = 'Export'
      lname  = 'VariableIntent'
      if (state%statep%st == ESMF_STATE_IMPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue1, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (state%statep%st == ESMF_STATE_EXPORT) then
        call c_ESMC_AttributeSetObjChrInTree(state%statep%base, lobject, &
          lname, lvalue2, localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_StateAddStateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateCreate"
!BOP
! !IROUTINE: ESMF_StateCreate - Create a new State

! !INTERFACE:
      function ESMF_StateCreate(stateName, statetype, &
                   fieldbundleList, fieldList, arrayList, nestedStateList, &
                   nameList, itemCount, &
                   neededflag, readyflag, validflag, reqforrestartflag, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreate
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: stateName 
      type(ESMF_StateType), intent(in), optional :: statetype
      type(ESMF_FieldBundle), dimension(:), intent(inout), optional :: fieldbundleList
      type(ESMF_Field), dimension(:), intent(inout), optional :: fieldList
      type(ESMF_Array), dimension(:), intent(in), optional :: arrayList
      type(ESMF_State), dimension(:), intent(in), optional :: nestedStateList
      character(len=*), dimension(:), intent(in), optional :: nameList
      integer, intent(in), optional :: itemCount
      type(ESMF_NeededFlag), optional :: neededflag
      type(ESMF_ReadyFlag), optional :: readyflag
      type(ESMF_ValidFlag), optional :: validflag
      type(ESMF_ReqForRestartFlag), optional :: reqforrestartflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_State}, set default characteristics for
!   objects added to it, and optionally add initial objects to it.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[stateName]}]
!    Name of this {\tt ESMF\_State} object.   A default name will be generated
!    if none is specified.
!   \item[{[statetype]}]
!    Import or Export {\tt ESMF\_State}.  Valid values are 
!    {\tt ESMF\_STATE\_IMPORT}, {\tt ESMF\_STATE\_EXPORT}, 
!    or {\tt ESMF\_STATE\_UNSPECIFIED} The default 
!    is {\tt ESMF\_STATE\_UNSPECIFIED}.
!   \item[{[fieldbundleList]}]
!    A list (Fortran array) of {\tt ESMF\_FieldBundle}s.
!   \item[{[fieldList]}]
!    A list (Fortran array) of {\tt ESMF\_Field}s.
!   \item[{[arrayList]}]
!    A list (Fortran array) of {\tt ESMF\_Array}s.
!   \item[{[nestedStateList]}]
!    A list (Fortran array) of {\tt ESMF\_State}s to be nested 
!    inside the outer {\tt ESMF\_State}.
!   \item[{[nameList]}]
!    A list (Fortran array) of character string name placeholders.
!   \item[{[itemCount]}]
!    The total number of things -- FieldBundles, Fields, 
!    Arrays, States, and Names -- to be added.
!    If {\tt itemCount} is not specified, it will be computed internally based
!    on the length of each object list.
!    If {\tt itemCount} is specified this routine
!    will do an error check to verify the total number of items found
!    in the argument lists matches this count of the expected number of items.
!   \item[{[neededflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.
!    Possible values are listed in Section~\ref{opt:neededflag}.  
!    If not specified, the default value is set to {\tt ESMF\_NEEDED}.
!   \item[{[readyflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Possible values are listed in Section~\ref{opt:readyflag}. 
!    If not specified, the default value is set to {\tt ESMF\_READYTOREAD}.
!   \item[{[validflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.
!    Possible values are listed in Section~\ref{opt:validflag}.   
!    If not specified, the default value is set to {\tt ESMF\_VALID}.
!   \item[{[reqforrestartflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}. 
!    Possible values are listed in Section~\ref{opt:reqforrestartflag}.    
!    If not specified, the default 
!    value is set to {\tt ESMF\_REQUIRED\_FOR\_RESTART}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_StateClass), pointer :: stypep
        integer :: localrc      ! local error status
        integer :: memstat      ! Stat from allocate/deallocate
        integer :: i

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        if (present(fieldbundleList)) then
           do i=1,size(fieldbundleList)
              ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundleList(i),rc)
           enddo
        endif
        if (present(fieldList)) then
           do i=1,size(fieldList)
              ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fieldList(i),rc)
           enddo
        endif
        if (present(arrayList)) then
           do i=1,size(arrayList)
              ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,arrayList(i),rc)
           enddo
        endif
        if (present(nestedStateList)) then
           do i=1,size(nestedStateList)
              ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,nestedStateList(i),rc)
           enddo
        endif


        ! Initialize the pointers to null.
        nullify(ESMF_StateCreate%statep)
        nullify(stypep)

        allocate(stypep, stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, "State type", &
                                       ESMF_CONTEXT, rc)) return
        
      !TODO: COLUMBIA_BUG: The following "if (present())" construct is a
      !      work-around for Intel's ifort version 9.1.045 and 9.1.051
      !      on NAS' columbia.
        if (present(nameList)) then 
          call ESMF_StateConstruct(stypep, stateName, statetype, &
                   fieldbundleList, fieldList, arrayList, nestedStateList, &
                   nameList, itemCount, &
                   neededflag, readyflag, validflag, reqforrestartflag, localrc)
        else
          call ESMF_StateConstruct(stypep, stateName, statetype, &
                   fieldbundleList, fieldList, arrayList, nestedStateList, &
                   itemcount=itemCount, &
                   neededflag=neededflag, readyflag=readyflag, &
                   validflag=validflag, reqforrestartflag=reqforrestartflag, &
                   rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then 
            deallocate(stypep, stat=memstat)
            return
        endif

        ! Set return values
        ESMF_StateCreate%statep => stypep

        ! Add reference to this object into ESMF garbage collection table
        call c_ESMC_VMAddFObject(ESMF_StateCreate, ESMF_ID_STATE%objectID)
      
        ! validate created state
        ESMF_INIT_SET_CREATED(ESMF_StateCreate)
 
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_StateCreate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateDestroy"
!BOP
! !IROUTINE: ESMF_StateDestroy - Release resources for a State
!
! !INTERFACE:
      recursive subroutine ESMF_StateDestroy(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)          :: state
      integer,          intent(out),  optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_State}. Actual
!     objects added to {\tt ESMF\_State}s will not be destroyed, it
!     remains the user's responsibility to destroy these objects in the correct
!     context. However, proxy objects automatically created during
!     {\tt ESMF\_StateReconcile()} are destroyed when the State is destroyed.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      Destroy contents of this {\tt ESMF\_State}.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! Local vars
        integer :: localrc                   ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

        if (.not.associated(state%statep)) then 
          call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed State: statep unassociated", &
            ESMF_CONTEXT, rc)
          return
        endif 

        ! Call Destruct to release resources
        call ESMF_StateDestruct(state%statep, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! mark object invalid
        call ESMF_BaseSetStatus(state%statep%base, ESMF_STATUS_INVALID, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

        ! Invalidate Destroyed State
        ESMF_INIT_SET_DELETED(state)

        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetInfo"
!BOP
! !IROUTINE: ESMF_StateGet - Get information about a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetInfo(state, itemSearch, nestedFlag, name, statetype, &
			       itemCount, itemNameList, stateitemtypeList, rc)
!
! !ARGUMENTS:
      type(ESMF_State),      intent(in) :: state
      character (len=*),     intent(in),  optional :: itemSearch
      logical,               intent(in),  optional :: nestedFlag
      character (len=*),     intent(out), optional :: name
      type(ESMF_StateType),  intent(out), optional :: statetype
      integer,               intent(out), optional :: itemCount
      character (len=*),     intent(out), optional :: itemNameList(:)
      type(ESMF_StateItemType), intent(out), optional :: stateitemtypeList(:)
      integer,               intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the requested information about this {\tt ESMF\_State}.
!     The optional {\tt itemSearch} argument may specify the name of
!     an individual item to search for.  When used in conjunction with
!     the {\tt nestedFlag}, nested States will also be searched.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       An {\tt ESMF\_State} object to be queried.
!     \item[{[itemSearch]}]
!       Query objects by name in the State.  When the {\tt nestedFlag}
!       option is set to .true., all nested States will also be searched
!       for the specified name.
!     \item[{[nestedFlag]}]
!       When set to {\tt .false.}, returns information at the current
!       State level only (default)
!       When set to {\tt .true.}, additionally returns information from
!       nested States
!     \item[{[name]}]
!       Name of this {\tt ESMF\_State}.
!     \item[{[statetype]}]
!       Import or Export of this {\tt ESMF\_State}.  Possible values are
!       listed in Section~\ref{opt:statetype}.
!     \item[{[itemCount]}]
!       Count of items in this {\tt ESMF\_State}, including all objects
!       as well as placeholder names.  When the {\tt nestedFlag} option is
!       set to {\tt .true.}, the count will include items present in nested
!       States.  When using {\tt itemSearch}, it will count the number of
!       items matching the specified name.
!     \item[{[itemNameList]}]
!       Array of item names in this {\tt ESMF\_State}, including placeholder
!       names.  When the {\tt nestedFlag} option is
!       set to {\tt .true.}, the list will include items present in nested
!       States.  When using {\tt itemSearch}, it will return the names of
!       items matching the specified name.  {\tt itemNameList} must be at least
!       {\tt itemCount} long.
!     \item[{[stateitemtypeList]}]  
!       Array of possible item object types in this {\tt ESMF\_State}, including
!       placeholder names.  When the {\tt nestedFlag} option is
!       set to {\tt .true.}, the list will include items present in nested
!       States.  When using {\tt itemSearch}, it will return the types of
!       items matching the specified name. Must be at least {\tt itemCount}
!       long.  Return values are listed in Section~\ref{opt:stateitemtype}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!     Typically, an {\tt ESMF\_StateGet()} information request will be performed
!     twice.  The first time, the {\tt itemCount} argument will be used to
!     query the size of arrays that are needed.  Arrays can then be allocated
!     to the correct size for {\tt itemNameList} and {\tt stateitemtypeList}
!     as needed.  A second call to {\tt ESMF\_StateGet()} will then fill in the
!     values.
!
!EOP
      integer :: ilpos
      integer :: localrc
      integer :: localitemcount
      logical :: localnestedflag
      type(ESMF_StateClass), pointer :: stypep

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      localnestedflag = .false.
      if (present (nestedFlag)) then
        localnestedflag = nestedFlag
      end if

      stypep => state%statep

      if (present(name)) call c_ESMC_GetName(stypep%base, name, localrc)
      if (present(statetype)) statetype = stypep%st

      ! TODO: indirect entries for Fields inside of FieldBundles complicates
      !  this code.  the count needs to be both primary objects and
      !  total objects.  perhaps the state derived type needs to bookkeep
      !  both numbers.  For now, return entire raw count.

      localitemcount = infoCountWorker (stypep)

      if (present(itemCount))  &
        itemCount = localitemcount 

      if (present(itemNameList)) then
        ilpos = 1
        call itemNameWorker (stypep, prefix="")
      endif

      if (present(stateitemtypeList)) then
        ilpos = 1
        call itemTypeWorker (stypep)
      endif

      if (present(rc)) rc = localrc

      contains
      
        recursive function infoCountWorker (sp) result (icount)
          type(ESMF_StateClass), pointer :: sp
          integer :: icount

          integer :: i1
          type(ESMF_StateItem) , pointer :: dp

          if (.not. present (itemSearch)) then
            icount = sp%datacount
          else
            icount = 0
            do, i1 = 1, sp%datacount
              dp => sp%datalist(i1)
              if (dp%namep == itemSearch) then
        	icount = icount + 1
              end if
            end do
          end if

          if (localnestedflag) then
            do, i1 = 1, sp%datacount
              dp => sp%datalist(i1)
              if (dp%otype%ot == ESMF_STATEITEM_STATE%ot) then
                icount = icount + infoCountWorker (dp%datap%spp)
              end if
            end do
          end if

        end function infoCountWorker
      
        recursive subroutine itemNameWorker (sp, prefix)
          type(ESMF_StateClass), pointer :: sp
          character(*), intent(in) :: prefix

        ! Copy as many names as will fit in the output array.

          integer :: i1
          type(ESMF_StateItem) , pointer :: dp

          do, i1 = 1, sp%datacount
            if (ilpos > size (itemNameList)) then
              localrc = ESMF_RC_ARG_SIZE
              exit
            end if

            dp => sp%datalist(i1)
            if (.not. present (itemSearch)) then
              itemNameList(ilpos) = prefix // dp%namep
              ilpos = ilpos + 1
            else
              if (dp%namep == itemSearch) then
                itemNameList(ilpos) = prefix // dp%namep
                ilpos = ilpos + 1
              end if
            end if

            if (dp%otype%ot == ESMF_STATEITEM_STATE%ot  &
                .and. localnestedflag) then
              call itemNameWorker (dp%datap%spp, prefix=prefix // trim (dp%namep) // '/')
            end if
          end do

        end subroutine itemNameWorker
      
        recursive subroutine itemTypeWorker (sp)
          type(ESMF_StateClass), pointer :: sp

        ! Copy as many type fields as will fit in the output array.

          integer :: i1
          type(ESMF_StateItem) , pointer :: dp

          do, i1 = 1, sp%datacount
            if (ilpos > size (stateitemtypeList)) then
              localrc = ESMF_RC_ARG_SIZE
              exit
            end if

            dp => sp%datalist(i1)
            if (.not. present (itemSearch)) then
              stateitemtypeList(ilpos) = dp%otype
              ilpos = ilpos + 1
            else
              if (dp%namep == itemSearch) then
              stateitemtypeList(ilpos) = dp%otype
              ilpos = ilpos + 1
              end if
            end if

            if (dp%otype%ot == ESMF_STATEITEM_STATE%ot  &
                .and. localnestedflag) then
              call itemTypeWorker (dp%datap%spp)
            end if
          end do

        end subroutine itemTypeWorker

      end subroutine ESMF_StateGetInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGet - Retrieve an item from a State
!
! !INTERFACE:
!      subroutine ESMF_StateGet(state, itemName, <item>,
!      nestedStateName, rc)
!
! !ARGUMENTS:
!      type(ESMF_State),      intent(in)            :: state
!      character (len=*),     intent(in)            :: itemName
!      <item>, see below for supported values
!      character (len=*),     intent(in),  optional :: nestedStateName
!      integer,               intent(out), optional :: rc             
!
!
! !DESCRIPTION:
!      Returns an <item> from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the <item> directly, only
!      {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!      An alternative technique for accessing a nested item which is only
!      one level down, is to specify the nested State with the
!      {\tt nestedStateName} argument.  While States can be nested to any
!      depth, this option only searches immediate descendents.
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!      It is an error to specify both {\tt nestedStateName} and
!      a fully qualified, nested State itemName.
!
!      Supported values for <item> are:
!      \begin{description}
!      \item type(ESMF\_Array),  intent(out)           :: array
!      \item type(ESMF\_ArrayBundle), intent(out)      :: arraybundle
!      \item type(ESMF\_Field), intent(out)            :: field
!      \item type(ESMF\_FieldBundle), intent(out)      :: fieldbundle
!      \item type(ESMF\_RouteHandle),  intent(out)     :: routehandle
!      \item type(ESMF\_State), intent(out)            :: nestedState
!      \end{description}
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!     State to query for an <item> named {\tt itemName}.
!     \item[itemName]
!     Name of <item> to be returned.  This name may be fully
!     qualified in order to access nested State items.
!     \item[<item>]
!     Returned reference to the <item>.
!     \item[{[nestedStateName]}]
!     Optional.  Used when the {\tt state} contains 
!     multiple nested {\tt ESMF\_State}s and the <item> being requested is
!     one level down in one of the nested {\tt ESMF\_State}.
!     An error if specified when the {\tt state} argument contains
!     no nested {\tt ESMF\_State}s.  It is also an error to use this option
!     with a fully qualified {\tt itemName}.  This option is retained for
!     compatibility with earlier versions of ESMF.
!     \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetArray"
!BOPI

! !IROUTINE: ESMF_StateGet - Retrieve an Array from a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetArray(state, itemName, array,  &
          nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State),  intent(in)	       :: state
      character (len=*), intent(in)	       :: itemName
      type(ESMF_Array),  intent(out)	       :: array
      character (len=*), intent(in),  optional :: nestedStateName
      integer,           intent(out), optional :: rc		 

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Array} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the {\tt ESMF\_Array} directly, only
!      {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!      An alternative technique for accessing a nested item which is only
!      one level down, is to specify the nested State with the
!      {\tt nestedStateName} argument.  While States can be nested to any
!      depth, this option only searches immediate descendents.
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!      It is an error to specify both {\tt nestedStateName} and
!      a fully qualified, nested State itemName.
!      
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for an {\tt ESMF\_Array} named {\tt itemName}.
!  \item[itemName]
!    Name of {\tt ESMF\_Array} to be returned.  This name may be fully
!     qualified in order to access nested State items.
!  \item[array]
!    Returned reference to the {\tt ESMF\_Array}.
!  \item[{[nestedStateName]}]
!     Optional.  Used when the {\tt state} contains 
!     multiple nested {\tt ESMF\_State}s and the <item> being requested is
!     one level down in one of the nested {\tt ESMF\_State}.
!     An error if specified when the {\tt state} argument contains
!     no nested {\tt ESMF\_State}s.  It is also an error to use this option
!     with a fully qualified {\tt itemName}.  This option is retained for
!     compatibility with earlier versions of ESMF.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOPI

      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      logical :: exists
      integer :: localrc
      character(len=ESMF_MAXSTR) :: errmsg

      localrc = ESMF_RC_NOT_IMPL
   
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty (or invalid) array to mark failure?

      if (present (nestedStateName) .and. index (itemName, '/') > 0) then
          errmsg = "both nestedStateName and fully qualified itemName were specified"
          if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP, errmsg,  &
                                     ESMF_CONTEXT, rc)) return
      end if

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep,  &
                                           dataname=nestedStateName, expected=.true., &
                                           dataitem=dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg,*) trim(nestedStateName), " found but not type State"
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep,   &
                                       dataname=itemName, expected=.true., &
                                       dataitem=dataitem,  &
                                       rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no Array found named ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_ARRAY) then
          write(errmsg, *) trim(itemName), " found but not type Array"
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      array = dataitem%datap%ap

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetArrayBundle"
!BOPI
! !IROUTINE: ESMF_StateGet - Retrieve an ArrayBundle from a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetArrayBundle(state, itemName, arraybundle, &
        nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State),       intent(in)            :: state
      character (len=*),      intent(in)            :: itemName
      type(ESMF_ArrayBundle), intent(out)           :: arraybundle
      character (len=*),      intent(in),  optional :: nestedStateName
      integer,                intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_ArrayBundle} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the {\tt ESMF\_ArrayBundle} directly,
!      only {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!      An alternative technique for accessing a nested item which is only
!      one level down, is to specify the nested State with the
!      {\tt nestedStateName} argument.  While States can be nested to any
!      depth, this option only searches immediate descendents.
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!      It is an error to specify both {\tt nestedStateName} and
!      a fully qualified, nested State itemName.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for an {\tt ESMF\_ArrayBundle} named {\tt itemName}.
!  \item[itemName]
!    Name of {\tt ESMF\_ArrayBundle} to be returned.
!  \item[arraybundl]
!    Returned reference to the {\tt ESMF\_ArrayBundle}.  This name may be fully
!     qualified in order to access nested State items.
!  \item[{[nestedStateName]}]
!     Optional.  Used when the {\tt state} contains 
!     multiple nested {\tt ESMF\_State}s and the <item> being requested is
!     one level down in one of the nested {\tt ESMF\_State}.
!     An error if specified when the {\tt state} argument contains
!     no nested {\tt ESMF\_State}s.  It is also an error to use this option
!     with a fully qualified {\tt itemName}.  This option is retained for
!     compatibility with earlier versions of ESMF.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOPI

      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      logical :: exists
      integer :: localrc
      character(len=ESMF_MAXSTR) :: errmsg

      localrc = ESMF_RC_NOT_IMPL
   
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty (or invalid) arraybundle to mark failure?

      if (present (nestedStateName) .and. index (itemName, '/') > 0) then
          errmsg = "both nestedStateName and fully qualified itemName were specified"
          if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP, errmsg,  &
                                     ESMF_CONTEXT, rc)) return
      end if

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep,  &
                                           dataname=nestedStateName, expected=.true., &
                                           dataitem=dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg,*) trim(nestedStateName), " found but not type State"
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep,   &
                                       dataname=itemName, expected=.true., &
                                       dataitem=dataitem,  &
                                       rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no ArrayBundle found named ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_ARRAYBUNDLE) then
          write(errmsg, *) trim(itemName), " found but not type ArrayBundle"
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      arraybundle = dataitem%datap%abp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetArrayBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetField"
!BOPI
! !IROUTINE: ESMF_StateGet - Retrieve a Field from a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetField(state, itemName, field, &
                                    nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State),      intent(in)            :: state
      character (len=*),     intent(in)            :: itemName
      type(ESMF_Field),      intent(out)           :: field
      character (len=*),     intent(in),  optional :: nestedStateName
      integer,               intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Field} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the {\tt ESMF\_Field} directly,
!      only {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!      An alternative technique for accessing a nested item which is only
!      one level down, is to specify the nested State with the
!      {\tt nestedStateName} argument.  While States can be nested to any
!      depth, this option only searches immediate descendents.
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!      It is an error to specify both {\tt nestedStateName} and
!      a fully qualified, nested State itemName.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for an {\tt ESMF\_Field} named {\tt itemName}.
!  \item[itemName]
!    Name of {\tt ESMF\_Field} to be returned.  This name may be fully
!     qualified in order to access nested State items.
!  \item[field]
!    Returned reference to the {\tt ESMF\_Field}.
!  \item[{[nestedStateName]}]
!     Optional.  Used when the {\tt state} contains 
!     multiple nested {\tt ESMF\_State}s and the <item> being requested is
!     one level down in one of the nested {\tt ESMF\_State}.
!     An error if specified when the {\tt state} argument contains
!     no nested {\tt ESMF\_State}s.  It is also an error to use this option
!     with a fully qualified {\tt itemName}.  This option is retained for
!     compatibility with earlier versions of ESMF.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOPI

      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty field to mark failure?

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present (nestedStateName) .and. index (itemName, '/') > 0) then
          errmsg = "both nestedStateName and fully qualified itemName were specified"
          if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP, errmsg,  &
                                     ESMF_CONTEXT, rc)) return
      end if

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep,  &
                                           dataname=nestedStateName, expected=.true., &
                                           dataitem=dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                         ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg, *) trim(nestedStateName), " found but not type State"
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep,   &
                                       dataname=itemName, expected=.true., &
                                       dataitem=dataitem,  &
                                       rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no Field found named ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_FIELD) then
          if (dataitem%otype .eq. ESMF_STATEITEM_INDIRECT) then
              ! TODO: how do we return the info that this is inside a fieldbundle?
              if (ESMF_LogFoundError(ESMF_RC_NOT_IMPL, &
                       "extracting Fields directly from FieldBundles in a State", &
                       ESMF_CONTEXT, rc)) return
          endif
          write(errmsg, *) trim(itemName), " found but not type Field"
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      field = dataitem%datap%fp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetFieldBundle"
!BOPI
! !IROUTINE: ESMF_StateGet - Retrieve a FieldBundle from a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetFieldBundle(state, itemName, fieldbundle, &
                                     nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State),       intent(in)            :: state
      character (len=*),      intent(in)            :: itemName
      type(ESMF_FieldBundle), intent(out)           :: fieldbundle
      character (len=*),      intent(in),  optional :: nestedStateName
      integer,                intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_FieldBundle} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the {\tt ESMF\_FieldBundle} directly,
!      only {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!      An alternative technique for accessing a nested item which is only
!      one level down, is to specify the nested State with the
!      {\tt nestedStateName} argument.  While States can be nested to any
!      depth, this option only searches immediate descendents.
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!      It is an error to specify both {\tt nestedStateName} and
!      a fully qualified, nested State itemName.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for a {\tt ESMF\_FieldBundle} named {\tt itemName}.
!  \item[itemName]
!    Name of {\tt ESMF\_FieldBundle} to be returned.  This name may be fully
!     qualified in order to access nested State items.
!  \item[fieldbundle]
!    Returned reference to the {\tt ESMF\_FieldBundle}.
!  \item[{[nestedStateName]}]
!     Optional.  Used when the {\tt state} contains 
!     multiple nested {\tt ESMF\_State}s and the <item> being requested is
!     one level down in one of the nested {\tt ESMF\_State}.
!     An error if specified when the {\tt state} argument contains
!     no nested {\tt ESMF\_State}s.  It is also an error to use this option
!     with a fully qualified {\tt itemName}.  This option is retained for
!     compatibility with earlier versions of ESMF.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty fieldbundle to mark failure?

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present (nestedStateName) .and. index (itemName, '/') > 0) then
          errmsg = "both nestedStateName and fully qualified itemName were specified"
          if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP, errmsg,  &
                                     ESMF_CONTEXT, rc)) return
      end if

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep,  &
                                           dataname=nestedStateName, expected=.true., &
                                           dataitem=dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg, *) trim(nestedStateName), " found but not type State"
             if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep,   &
                                       dataname=itemName, expected=.true., &
                                       dataitem=dataitem,  &
                                       rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no FieldBundle found named ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_FIELDBUNDLE) then
          write(errmsg, *) trim(itemName), " found but not type FieldBundle"
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      fieldbundle = dataitem%datap%fbp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetFieldBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetItemInfo"
!BOP
! !IROUTINE: ESMF_StateGet - Get information about an item in a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetItemInfo(state, name, stateitemtype, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: name
      type(ESMF_StateItemType), intent(out) :: stateitemtype
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the type for the item named
!     {\tt name} in this {\tt ESMF\_State}.  If no item with this name
!     exists, the value {\tt ESMF\_STATEITEM\_NOTFOUND} will be returned
!     and the error code will not be set to an error.  Thus this routine
!     can be used to safely query for the existance of items by name 
!     whether or not they are expected to be there.   The error code will
!     be set in case of other errors, for example if the {\tt ESMF\_State}
!     itself is invalid.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!        {\tt ESMF\_State} to be queried.
!      \item[name]
!        Name of the item to return information about.
!      \item[stateitemtype]
!        Returned item types for the item with the given name, including 
!        placeholder names.  Options are
!        listed in Section~\ref{opt:stateitemtype}.  If no item with the
!        given name is found, {\tt ESMF\_STATEITEM\_NOTFOUND} will be returned
!        and {\tt rc} will {\bf not} be set to an error.
!      \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
      integer :: i, localrc
      type(ESMF_StateClass), pointer :: stypep
      type(ESMF_StateItem), pointer :: nextitem

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      stypep => state%statep

      ! Start out assuming the name does not exist, and if it is found
      ! then overwrite the type and jump out of the loop.   It will not be
      ! an error to not find the name; an error will trigger a message to
      ! the log file, possibly an 'exit on error' condition.   It should be
      ! benign to query for a name which is not there - it might not be
      ! expected to exist yet.
      stateitemtype = ESMF_STATEITEM_NOTFOUND

      ! Find the object which matches this name
      do i=1, stypep%datacount
          nextitem => stypep%datalist(i)
          if (trim(name) .eq. trim(nextitem%namep)) then
              stateitemtype = nextitem%otype
              exit    ! jump out of the do loop here
          endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetItemInfo

!------------------------------------------------------------------------------
#if defined (ESMF_ENABLESTATENEEDED)
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetNeeded"
!BOP
! !IROUTINE: ESMF_StateGetNeeded - Query whether a data item is needed
!
! !INTERFACE:
      subroutine ESMF_StateGetNeeded(state, itemName, neededflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: itemName
      type(ESMF_NeededFlag), intent(out) :: neededflag
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the status of the {\tt neededflag} for the data item
!      named by {\tt itemName} in the {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       The {\tt ESMF\_State} to query.
!      \item[itemName]
!       Name of the data item to query.
!      \item[neededflag]
!       Whether state item is needed or not for a particular application
!       configuration.  Possible values are listed in 
!       Section~\ref{opt:neededflag}.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
                                      dataitem=dataitem, rc=localrc)
      if (.not. exists) then
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, trim(itemName), &
                                     ESMF_CONTEXT, rc)) return
      endif

      neededflag = dataitem%needed

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetNeeded
#endif
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetRouteHandle"
!BOPI
! !IROUTINE: ESMF_StateGet - Retrieve an RouteHandle from a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetRouteHandle(state, itemName, routehandle, &
                                     nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State),       intent(in)            :: state
      character (len=*),      intent(in)            :: itemName
      type(ESMF_RouteHandle), intent(out)           :: routehandle
      character (len=*),      intent(in),  optional :: nestedStateName
      integer,                intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_RouteHandle} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the {\tt ESMF\_RouteHandle} directly, only
!      {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!      An alternative technique for accessing a nested item which is only
!      one level down, is to specify the nested State with the
!      {\tt nestedStateName} argument.  While States can be nested to any
!      depth, this option only searches immediate descendents.
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!      It is an error to specify both {\tt nestedStateName} and
!      a fully qualified, nested State itemName.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for an {\tt ESMF\_RouteHandle} named {\tt itemName}.
!  \item[itemName]
!    Name of {\tt ESMF\_RouteHandle} to be returned.  This name may be fully
!     qualified in order to access nested State items.
!  \item[routehandle]
!    Returned reference to the {\tt ESMF\_RouteHandle}.
!  \item[{[nestedStateName]}]
!     Optional.  Used when the {\tt state} contains 
!     multiple nested {\tt ESMF\_State}s and the <item> being requested is
!     one level down in one of the nested {\tt ESMF\_State}.
!     An error if specified when the {\tt state} argument contains
!     no nested {\tt ESMF\_State}s.  It is also an error to use this option
!     with a fully qualified {\tt itemName}.  This option is retained for
!     compatibility with earlier versions of ESMF.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOPI

      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      logical :: exists
      integer :: localrc
      character(len=ESMF_MAXSTR) :: errmsg

      localrc = ESMF_RC_NOT_IMPL
   
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty (or invalid) routehandle to mark failure?

      if (present (nestedStateName) .and. index (itemName, '/') > 0) then
          errmsg = "both nestedStateName and fully qualified itemName were specified"
          if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP, errmsg,  &
                                     ESMF_CONTEXT, rc)) return
      end if

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep,  &
                                           dataname=nestedStateName, expected=.true., &
                                           dataitem=dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg,*) trim(nestedStateName), " found but not type State"
              if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep,   &
                                       dataname=itemName, expected=.true., &
                                       dataitem=dataitem,  &
                                       rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no RouteHandle found named ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_ROUTEHANDLE) then
          write(errmsg, *) trim(itemName), " found but not type RouteHandle"
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      routehandle = dataitem%datap%rp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetRouteHandle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetState"
!BOPI
! !IROUTINE: ESMF_StateGet - Retrieve a State nested in a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGet()   
      subroutine ESMF_StateGetState(state, itemName, nestedState, rc)
!
! !ARGUMENTS:
      type(ESMF_State),      intent(in)            :: state
      character (len=*),     intent(in)            :: itemName
      type(ESMF_State),      intent(out)           :: nestedState
      integer,               intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a nested {\tt ESMF\_State} from another {\tt ESMF\_State} 
!      by name.
!      If the {\tt ESMF\_State} contains the nested {\tt ESMF\_State} directly, only
!      {\tt itemName} is required.
!
!      If the {\tt state} contains nested {\tt ESMF\_State}s,
!      the {\tt itemName} argument may specify a fully qualified name
!      to access the desired item with a single call.  This is performed
!      using the ``/'' character to separate the names of the intermediate
!      State names leading to the desired item.  (E.g.,
!      {\tt itemName=``state1/state12/item''}.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       The {\tt ESMF\_State} to query for a nested {\tt ESMF\_State} 
!       named {\tt stateName}.
!     \item[itemName]
!       Name of nested {\tt ESMF\_State} to return.  This name may be fully
!       qualified in order to access nested State items.
!     \item[nestedState]
!       Returned {\tt ESMF\_State}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      type(ESMF_StateItem), pointer :: dataitem
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty state to mark failure?

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      exists = ESMF_StateClassFindData(state%statep,   &
                                       dataname=itemName, expected=.true., &
                                       dataitem=dataitem,  &
                                       rc=localrc)
      if (.not. exists) then
          write (errmsg,*) "no nested state found named ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
          write (errmsg, *) trim(itemName), " found but not type State"
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      nestedState%statep => dataitem%datap%spp

      ! validate created state
       ESMF_INIT_SET_CREATED(nestedState)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetState

!------------------------------------------------------------------------------
#if defined (ESMF_ENABLESTATENEEDED)
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateIsNeeded"
!BOP
! !IROUTINE: ESMF_StateIsNeeded -- Return logical true if data item needed
!
! !INTERFACE:
      function ESMF_StateIsNeeded(state, itemName, rc)
!
! !RETURN VALUE:
      logical :: ESMF_StateIsNeeded
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: itemName
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns true if the status of the {\tt needed} flag for the data item
!      named by {\tt itemName} in the {\tt ESMF\_State} is 
!      {\tt ESMF\_STATEITEM\_NEEDED}.  Returns false for no item found 
!      with the specified name or item marked not needed.  Also sets error
!      code if {\tt dataname} not found.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} to query.
!      \item[itemName]
!       Name of the data item to query.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      logical :: exists
      integer :: localrc

      ! Assume no unless we find it and it is needed.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      ESMF_StateIsNeeded = .FALSE.

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: decide on the behavior:
      ! should it be an error to ask about a state which doesn't exist?
      ! if the 3rd arg below is .true. then it's an error, if it's .false.
      ! then it's not.  for now, it's an error.
      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
                                      dataitem=dataitem, rc=localrc)
      if (.not. exists) then
          if (ESMF_LogFoundError(localrc, &
                                      "Item by that name not found", &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%needed .eq. ESMF_NEEDED) ESMF_StateIsNeeded = .TRUE.
  
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_StateIsNeeded
#endif
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StatePrint"
!BOP
! !IROUTINE: ESMF_StatePrint - Print the internal data for a State
!
! !INTERFACE:
      subroutine ESMF_StatePrint(state, options, nestedFlag, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      character (len = *),  intent(in), optional :: options
      logical, intent(in),  optional :: nestedFlag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Prints information about the {\tt state} to {\tt stdout}. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit {\tt ESMF\_IOstdout} to get coherent output.  \\
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to print.
!     \item[{[options]}]
!       Print options:
!         " ", or "brief" - print names and types of the objects within the state (default),
!         "long" - print additional information, such as proxy flags
!     \item[{[nestedFlag]}]
!       When set to {\tt .false.}, prints information about the current
!       State level only (default),
!       When set to {\tt .true.}, additionally prints information from
!       nested States
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!

!EOP

!
! TODO: this needs more code added to be complete
!
       character (len=8) :: localopts
       integer :: localrc                    ! local error status
       logical :: localnestedflag            ! Print nested states flag
       logical :: longflag                   ! Extended output
       
       ! Initialize return code; assume failure until success is certain
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! check input variables
       ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

       ! Validate options arg

       localnestedflag = .false.
       if (present (nestedFlag)) then
         localnestedflag = nestedFlag
       end if
       
       localopts = "brief"
       if (present (options)) then
         if (options /= " ")  &
           localopts = adjustl (options)
       end if

       longflag = .false.
       call ESMF_StringLowerCase (localopts)
       select case (localopts)
       case ("brief")
       case ("long")
           longflag = .true.
       case default
           write (ESMF_IOstderr,*) "ESMF_StatePrint: Illegal options arg: ", &
               trim (localopts)
           return 
       end select

       !nsc write(msgbuf,*) "StatePrint: "  
       !nsc call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
       write (ESMF_IOstdout,*) "StatePrint: "  
       if (.not.associated(state%statep)) then 
           !nsc call ESMF_LogWrite("Uninitialized or already destroyed State", &
           !nsc                   ESMF_LOG_INFO)
           write (ESMF_IOstdout,*) "Uninitialized or already destroyed State"
           if (present (rc)) rc = ESMF_SUCCESS
           return
       endif
       if (state%statep%st .eq. ESMF_STATE_INVALID) then
           !nsc call ESMF_LogWrite("Uninitialized or already destroyed State", &
           !nsc                   ESMF_LOG_INFO)
           write (ESMF_IOstdout,*) "Uninitialized or already destroyed State"
           if (present (rc)) rc = ESMF_SUCCESS
           return
       endif

       call statePrintWorker (state%statep, level=0, rc1=rc)

       ! Set return values 
       if (present(rc)) rc = ESMF_SUCCESS
       
   contains

     recursive subroutine statePrintWorker (sp, level, rc1)
       type(ESMF_StateClass), pointer :: sp
       integer, intent(in) :: level
       integer, intent(out), optional :: rc1

       type(ESMF_StateItem) , pointer :: dp

       character(len=2*level+1) :: nestr
       character(len=ESMF_MAXSTR) :: name
       character(len=ESMF_MAXSTR) :: msgbuf
       character(len=1024) :: outbuf

       integer :: i

       nestr = repeat("->", level) // " "

       ! TODO: Add code here
       ! print num of states, state type, etc

       call c_ESMC_GetName(sp%base, name, localrc)
       if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc1)) return
       write (ESMF_IOstdout,*) nestr, "State name: ", trim(name)
       if (sp%st == ESMF_STATE_IMPORT) then
         msgbuf = "Import State"
       else if (sp%st == ESMF_STATE_EXPORT) then
         msgbuf = "Export State"
       else if (sp%st == ESMF_STATE_UNSPECIFIED) then
         msgbuf = "Unspecified direction"
       else if (sp%st == ESMF_STATE_INVALID) then
         call ESMF_LogWrite("Uninitialized or already destroyed State", &
                                ESMF_LOG_INFO)
         if (present (rc1)) rc1 = ESMF_SUCCESS
         return
       else
         call ESMF_LogWrite ("error: unknown state",  &
                                ESMF_LOG_INFO)
       end if

       !nsc call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
       write (ESMF_IOstdout,'(1x,4a,i0)') nestr, "    status: ", trim(msgbuf),  &
           ", object count: ", sp%datacount

       !pli print attribute name/value pairs using c_esmc_baseprint() 
       call c_ESMC_BasePrint(sp%base, level, "brief", localrc)
       if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc1)) return
       
      
       do i=1, sp%datacount
         dp => sp%datalist(i)

         write (ESMF_IOstdout,'(1x,2a,i0,2a)') nestr, "    object: ", i, &
             ", name: ", trim(dp%namep)
         outbuf = "      type:"

         select case (dp%otype%ot)
         case (ESMF_STATEITEM_FIELDBUNDLE%ot)
             outbuf = trim (outbuf) // " FieldBundle"
         case (ESMF_STATEITEM_FIELD%ot)
             outbuf = trim (outbuf) // " Field"
         case (ESMF_STATEITEM_ARRAY%ot)
             outbuf = trim (outbuf) // " Array"
         case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
             outbuf = trim (outbuf) // " ArrayBundle"
         case (ESMF_STATEITEM_ROUTEHANDLE%ot)
             outbuf = trim (outbuf) // " Route handle"
         case (ESMF_STATEITEM_STATE%ot)
             outbuf = trim (outbuf) // " State"
         case (ESMF_STATEITEM_NAME%ot)
             outbuf = trim (outbuf) // " Placeholder name"
         case (ESMF_STATEITEM_INDIRECT%ot)
             outbuf = trim (outbuf) // " Indirect Field inside a FieldBundle"
         case (ESMF_STATEITEM_UNKNOWN%ot)
             outbuf = trim (outbuf) // " Unknown"
         case (ESMF_STATEITEM_NOTFOUND%ot)
             outbuf = trim (outbuf) // " Not found"
         case default
             outbuf = trim (outbuf) // " (bad type value)"
         end select

         if (longflag) &
           outbuf = trim (outbuf) // ", proxy flag: " // merge ("yes", "no ", dp%proxyFlag)

#if defined (ESMF_ENABLESTATENEEDED)
         select case (dp%needed%needed)
           case (ESMF_NEEDED%needed)
             outbuf = trim(outbuf) //  ", needed."
           case (ESMF_NOTNEEDED%needed)
             outbuf = trim(outbuf) //  ", marked as NOT needed."
         end select
#endif

         write (ESMF_IOstdout,*) nestr, trim(outbuf)

         if (localnestedflag .and. dp%otype%ot == ESMF_STATEITEM_STATE%ot) then
             call statePrintWorker (dp%datap%spp, level=level+1)
         end if
       enddo

       end subroutine statePrintWorker

     end subroutine ESMF_StatePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateRead"
!BOP
! !IROUTINE: ESMF_StateRead -- Read data items from a file into a State
!
! !INTERFACE:
      subroutine ESMF_StateRead(state, fileName, rc)
!
! !ARGUMENTS:
      type(ESMF_State)                                :: state 
      character (len=*),        intent(in)            :: fileName
      integer,                  intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Currently limited to read in all Arrays from a netCDF file and add them
!     to a State object.  Future releases will enable more items of a State
!     to be read from a file of various formats.
!
!     Only PET 0 reads the file; the States in other PETs remain empty.
!     Currently, the data is not decomposed or distributed; each PET
!     has only 1 DE and only PET 0 contains data after reading the file.
!     Future versions of ESMF will support data decomposition and distribution
!     upon reading a file.  See Section~\ref{example:StateRdWr} for
!     an example.
!
!     Note that the third party NetCDF library must be installed.  For more
!     details, see the "ESMF Users Guide",
!     "Building and Installing the ESMF, Third Party Libraries, NetCDF" and
!     the website http://www.unidata.ucar.edu/software/netcdf.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to add items read from file.  Currently only
!       Arrays are supported.
!     \item[fileName]
!       File to be read.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       Equals {\tt ESMF\_RC\_LIB\_NOT\_PRESENT} if the NetCDF library is
!       not present.
!     \end{description}
!
!EOP
!       TODO: use item flag ESMF_STATEITEM_ARRAY<BUNDLE>

        integer :: fileNameLen, localrc

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ! get length of given fileName for C++ validation
        fileNameLen = len_trim(fileName)

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

        ! invoke C to C++ entry point 
        call c_ESMC_StateRead(state, state%statep%base, &
                              fileNameLen, fileName, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_StateRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateReplace - Replace a single item to a State
!
! !INTERFACE:
!  subroutine ESMF_StateReplace(state, <item>, rc)
!
! !ARGUMENTS:
!    type(ESMF_State), intent(inout)          :: state
!    <item>, see below for supported values
!    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Replace an existing reference to a single <item> to an existing 
!      {\tt state}.  The name of the <item> must be unique within the
!      {\tt state}.  
!
!      Supported values for <item> are:
!      \begin{description}
!      \item type(ESMF\_Array), intent(in)            :: array
!      \item type(ESMF\_ArrayBundle), intent(in)      :: arraybundle
!      \item type(ESMF\_Field), intent(in)            :: field
!      \item type(ESMF\_FieldBundle), intent(in)      :: fieldbundle
!      \item character (len=*), intent(in)            :: name
!      \item type(ESMF\_RouteHandle), intent(in)      :: routehandle
!      \item type(ESMF\_State), intent(in)            :: nestedState
!      \end{description}
!
! The arguments are:
! \begin{description}
! \item[state]
!      The {\tt ESMF\_State} to which <item>s will be replaced.
! \item[<item>]
!      The replacement <item>.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateRepOneArray"
!BOPI
! !IROUTINE: ESMF_StateReplace - Replace an Array in a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateReplace()   
  subroutine ESMF_StateRepOneArray(state, array, rc)
!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)          :: state
    type(ESMF_Array), intent(in)             :: array
    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Replace an existing reference to a single <item> to an existing 
!      {\tt state}.  The name of the <item> must be unique within the
!      {\tt state}.  
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[array]
!      The replacement <item>.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, array,       &
                        proxyflag=.false.,  &
                        replaceflag=.true., &
                        rc=rc)

  end subroutine ESMF_StateRepOneArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateRepOneArrayBundle"
!BOPI
! !IROUTINE: ESMF_StateReplace - Replace an ArrayBundle in a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateReplace()   
  subroutine ESMF_StateRepOneArrayBundle(state, arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_State),       intent(inout)          :: state
    type(ESMF_ArrayBundle), intent(in)             :: arraybundle
    integer,                intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Replace an existing reference to a single <item> to an existing 
!      {\tt state}.  The name of the <item> must be unique within the
!      {\tt state}.  
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[arraybundle]
!      The replacement <item>.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, arraybundle,  &
                        proxyflag=.false.,   &
                        replaceflag=.true.,  &
                        rc=rc)

  end subroutine ESMF_StateRepOneArrayBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateRepOneField"
!BOPI
! !IROUTINE: ESMF_StateReplace - Replace a Field in a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateReplace()   
  subroutine ESMF_StateRepOneField(state, Field, rc)
!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)          :: state
    type(ESMF_Field), intent(in)             :: Field
    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Replace an existing reference to a single <item> to an existing 
!      {\tt state}.  The name of the <item> must be unique within the
!      {\tt state}.  
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[Field]
!      The replacement <item>.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, Field,       &
                        proxyflag=.false.,  &
                        replaceflag=.true., &
                        rc=rc)

  end subroutine ESMF_StateRepOneField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateRepOneFieldBundle"
!BOPI
! !IROUTINE: ESMF_StateReplace - Replace a FieldBundle in a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateReplace()   
  subroutine ESMF_StateRepOneFieldBundle(state, Fieldbundle, rc)
!
! !ARGUMENTS:
    type(ESMF_State),       intent(inout)          :: state
    type(ESMF_FieldBundle), intent(in)             :: Fieldbundle
    integer,                intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Replace an existing reference to a single <item> to an existing 
!      {\tt state}.  The name of the <item> must be unique within the
!      {\tt state}.  
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[Fieldbundle]
!      The replacement <item>.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, Fieldbundle,  &
                        proxyflag=.false.,   &
                        replaceflag=.true.,  &
                        rc=rc)

  end subroutine ESMF_StateRepOneFieldBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateRepOneState"
!BOPI
! !IROUTINE: ESMF_StateReplace - Replace an State in a State
!
! !INTERFACE:
  ! Private name; call using ESMF_StateReplace()   
  subroutine ESMF_StateRepOneState(state, nestedState, rc)
!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)          :: state
    type(ESMF_State), intent(in)             :: nestedState
    integer,          intent(out),  optional :: rc
!     
! !DESCRIPTION:
!      Replace an existing reference to a single <item> to an existing 
!      {\tt state}.  The name of the <item> must be unique within the
!      {\tt state}.  
!
! The arguments are:
! \begin{description}
! \item[state]
!     An {\tt ESMF\_State} object.
! \item[nestedState]
!      The replacement <item>.  This is a reference only; when
!      the {\tt state} is destroyed the <item>s contained in it will
!      not be destroyed.   Also, the <item> cannot be safely 
!      destroyed before the {\tt state} is destroyed.
!      Since <item>s can be added to multiple containers, it remains
!      the user's responsibility to manage their
!      destruction when they are no longer in use.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------

    call ESMF_StateAdd (state, nestedState=nestedState, &
                        proxyflag=.false.,  &
                        replaceflag=.true., &
                        rc=rc)

  end subroutine ESMF_StateRepOneState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReadRestart"
!BOPI
! !IROUTINE: ESMF_StateReadRestart -- ReadRestart the internal data from a State
!
! !INTERFACE:
      function ESMF_StateReadRestart(name, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!      Used to reinitialize all data associated with an
!      {\tt ESMF\_State} from the last call to WriteRestart.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!       Name of {\tt ESMF\_State} to reinitialize.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

!
! TODO: code goes here
!
        type (ESMF_State) :: a 

!       this is just to shut the compiler up
        type (ESMF_StateClass), target :: b 
        a%statep => b
        nullify(a%statep)

        ESMF_StateReadRestart = a 
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
        end function ESMF_StateReadRestart

!------------------------------------------------------------------------------
!#undef  ESMF_METHOD
!#define ESMF_METHOD "ESMF_StateWrite"
!BOPI
! !IROUTINE: ESMF_StateWrite -- Write single item from a State
!
! !INTERFACE:
!      subroutine ESMF_StateWrite(state, itemName, rc)
!
! !ARGUMENTS:
!      type(ESMF_State):: state 
!      character (len=*), intent(in), optional :: itemName
!      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to write out all or part of a State object.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to write.
!     \item[{[itemName]}]
!       Item to be written.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!        ! TODO: hardcoded for interoperability test
!        type(ESMF_Field) :: fred
!        integer :: localrc
!
!        localrc = ESMF_RC_NOT_IMPL
!
!        ! check input variables
!        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
!
!        if (present(itemName)) then
!            call ESMF_StateGetField(state, itemName=itemName, field=fred, rc=localrc)
!            call ESMF_FieldWrite(fred, rc=localrc) 
!        endif
!
!        if (ESMF_LogFoundError(localrc, &
!                                  ESMF_ERR_PASSTHRU, &
!                                  ESMF_CONTEXT, rc)) return
!  
!
!        if (present(rc)) rc = ESMF_SUCCESS
!        end subroutine ESMF_StateWrite
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWrite"
!BOP
! !IROUTINE: ESMF_StateWrite -- Write items from a State to file
!
! !INTERFACE:
      subroutine ESMF_StateWrite(state, fileName, rc)
!
! !ARGUMENTS:
      type(ESMF_State)                                :: state 
      character (len=*),        intent(in)            :: fileName
      integer,                  intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Currently limited to write out all Arrays of a State object to a
!     netCDF file.  Future releases will enable more item types of a State to
!     be written to files of various formats.
!
!     Writing is currently limited to PET 0; future versions of ESMF will allow
!     parallel writing, as well as parallel reading.
!
!     See Section~\ref{example:StateRdWr} for an example.
!
!     Note that the third party NetCDF library must be installed.  For more
!     details, see the "ESMF Users Guide",
!     "Building and Installing the ESMF, Third Party Libraries, NetCDF" and
!     the website http://www.unidata.ucar.edu/software/netcdf.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} from which to write items.  Currently limited to
!       Arrays.
!     \item[fileName]
!       File to be written.  
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       Equals {\tt ESMF\_RC\_LIB\_NOT\_PRESENT} if the NetCDF library is
!       not present.
!     \end{description}
!
!EOP
!       TODO: use item flag ESMF_STATEITEM_ARRAY<BUNDLE>

        integer :: fileNameLen, localrc

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ! get length of given fileName for C++ validation
        fileNameLen = len_trim(fileName)

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

        ! invoke C to C++ entry point 
        call c_ESMC_StateWrite(state, state%statep%base, &
                              fileNameLen, fileName, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_StateWrite

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWriteRestart"
!BOPI
! !IROUTINE: ESMF_StateWriteRestart -- Save the internal data for a State
!
! !INTERFACE:
      subroutine ESMF_StateWriteRestart(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State):: state 
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
!     \item[state]
!       {\tt ESMF\_State} to save contents of.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        integer :: localrc

        localrc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
!
! TODO: code goes here
!
! The flags BOP/EOP have been changed to BOPI/EOPI because
! the subroutine has not been implemented. When the code is
! completed change back to BOP/EOP.
!

        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        end subroutine ESMF_StateWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateConstruct"
!BOPI
! !IROUTINE: ESMF_StateConstruct -- Construct a new State

! !INTERFACE:
      subroutine ESMF_StateConstruct(stypep, statename, statetype, & 
                         fieldbundles, fields, arrays, states, names, itemcount, &
#if defined (ESMF_ENABLESTATENEEDED)
                         neededflag, readyflag, validflag, reqforrestartflag, &
#endif
                         rc)
!
! !ARGUMENTS:
      type (ESMF_StateClass), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateType), intent(in), optional :: statetype
      type(ESMF_FieldBundle), dimension(:), intent(inout), optional :: fieldbundles
      type(ESMF_Field), dimension(:), intent(inout), optional :: fields
      type(ESMF_Array), dimension(:), intent(in), optional :: arrays
      type(ESMF_State), dimension(:), intent(in), optional :: states
      character(len=*), dimension(:), intent(in), optional :: names
      integer, intent(in), optional :: itemcount
      type(ESMF_NeededFlag), optional :: neededflag
      type(ESMF_ReadyFlag), optional :: readyflag
      type(ESMF_ValidFlag), optional :: validflag
      type(ESMF_ReqForRestartFlag), optional :: reqforrestartflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Construct a new State and set the decomposition characteristics.
!  The return value is a new State.
!
!  The arguments are:
!  \begin{description}
!   \item[stypep]
!    Internal StateClass pointer.  Required.
!   \item[{[statename]}]
!    Name of this {\tt ESMF\_State} object. 
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Should be one of {\tt ESMF\_STATE\_IMPORT},
!    {\tt ESMF\_STATE\_EXPORT}, or {\tt ESMF\_STATE\_LIST}.   
!    {\tt ESMF\_STATE\_LIST} is the default if not specified.
!   \item[{[fieldbundles]}]
!    An array of {\tt FieldBundles}.
!   \item[{[fields]}]
!    An array of {\tt Fields}.
!   \item[{[arrays]}]
!    An array of {\tt Arrays}.
!   \item[{[states]}]
!    An array of nested {\tt ESMF\_State}s.
!   \item[{[names]}]
!    An array of name placeholders.
!   \item[{[itemcount]}]
!    The total number of FieldBundles, Fields, Arrays, States, and Names specified.
!    This argument is optional, and if specified is used as an error check
!    to verify that the actual total number of items found matches this count.
#if defined (ESMF_ENABLESTATENEEDED)
!   \item[{[neededflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEITEM\_NEEDED} or 
!    {\tt ESMF\_STATEITEM\_NOTNEEDED}.  If not specified, the default value is
!    set to {\tt ESMF\_STATEITEM\_NEEDED}.
!   \item[{[readyflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEITEM\_READYTOWRITE},
!    {\tt ESMF\_STATEITEM\_READYTOREAD}, or {\tt ESMF\_STATEITEMNOTREADY}.
!    If not specified, the default value is set to 
!    {\tt ESMF\_STATEITEM\_READYTOREAD}.
!   \item[{[validflag]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEITEM\_VALID},
!    {\tt ESMF\_STATEITEM\_INVALID}, or {\tt ESMF\_STATEITEM\_VALIDITYUNKNOWN}.
!    If not specified, the default value is set to 
!    {\tt ESMF\_STATEITEM\_VALID}.
!   \item[{[reqforrestartflag}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_REQUIRED\_FOR\_RESTART} or
!    {\tt ESMF\_NOTREQUIRED\_FOR\_RESTART}. If not specified, the default 
!    value is set to {\tt ESMF\_REQUIRED\_FOR\_RESTART}.
#endif
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

        ! Local vars
        integer :: count
        integer :: localrc                   ! local error status
        integer :: i

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ! check input variables
        if (present(fieldbundles)) then
           do i=1,size(fieldbundles)
              ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundles(i),rc)
           enddo
        endif
        if (present(fields)) then
           do i=1,size(fields)
              ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fields(i),rc)
           enddo
        endif
        if (present(arrays)) then
           do i=1,size(arrays)
              ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,arrays(i),rc)
           enddo
        endif
        if (present(states)) then
           do i=1,size(states)
              ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,states(i),rc)
           enddo
        endif


        ! Quick sanity check on the values
        count = 0
        if (present(fieldbundles)) count = count + size(fieldbundles)
        if (present(fields)) count = count + size(fields)
        if (present(arrays)) count = count + size(arrays)
        if (present(states)) count = count + size(states)
        if (present(names)) count = count + size(names)

        if (present(itemcount)) then
          if (count .ne. itemcount) then
              if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, &
                                  "itemcount does not match lists given", &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        ! Set initial values
        call ESMF_StateConstructEmpty(stypep, statename, statetype, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLESTATENEEDED)
        ! Set the defaults for objects added to this state
        if (present(neededflag)) then
            stypep%needed_default = neededflag
        else
            stypep%needed_default = ESMF_NEEDED
        endif

        if (present(readyflag)) then
            stypep%ready_default = readyflag
        else
            stypep%ready_default = ESMF_READYTOREAD
        endif

        if (present(validflag)) then
            stypep%stvalid_default = validflag
        else
            stypep%stvalid_default = ESMF_VALID
        endif

        if (present(reqforrestartflag)) then
            stypep%reqrestart_default = reqforrestartflag
        else
            stypep%reqrestart_default = ESMF_REQUIRED_FOR_RESTART
        endif
#endif

        ! Set the initial size of the datalist
        call ESMF_StateClassExtendList(stypep, count, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      
        ! For each item type, set the data values.  All the allocation 
        !  has already been done.
        if (present(fieldbundles)) then
          count = size(fieldbundles)
          if (count .gt. 0) then
            call ESMF_StateClAddFieldBundleList(stypep, count, fieldbundles, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(fields)) then
          count = size(fields)
          if (count .gt. 0) then
            call ESMF_StateClsAddFieldList(stypep, count, fields, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(arrays)) then
          count = size(arrays)
          if (count .gt. 0) then
            call ESMF_StateClsAddArrayList(stypep, count, arrays, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(states)) then
          count = size(states)
          if (count .gt. 0) then
            call ESMF_StateClsAddStateList(stypep, count, states, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(names)) then
          count = size(names)
          if (count .gt. 0) then
            call ESMF_StateClsAddDataNameList(stypep, count, names, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif


        ! Set return values
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StateConstruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateConstructEmpty"
!BOPI
! !IROUTINE: ESMF_StateConstructEmpty -- Create a new State specifying no data

! !INTERFACE:
      subroutine ESMF_StateConstructEmpty(stypep, statename, statetype, rc)
!
! !ARGUMENTS:
      type (ESMF_StateClass), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateType), intent(in), optional :: statetype
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Construct a new empty {\tt State}.  The return value is a new {\tt State}.
!    
!      The arguments are:
!      \begin{description}
!       \item[{[stypep]}]
!       Internal StateClass pointer.  Required.
!       \item[{[statetype]}]
!        Import or Export {\tt State}.  One of {\tt ESMF\_STATE\_IMPORT},
!        {\tt ESMF\_STATE\_EXPORT}, or {\tt ESMF\_STATE\_LIST}.  Default is 
!        {\tt ESMF\_STATE\_LIST}.
!       \item[{[statename]}]
!        Name of this {\tt ESMF\_State} object.  Optional.  If a name is not
!        specified one will be generated.
!       \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOPI

        ! Local vars
        integer :: localrc                   ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Initialize the base object, set the name, etc.
        call ESMF_BaseCreate(stypep%base, "State", statename, 0, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Fill in basic information
        if (present(statetype)) then
          stypep%st = statetype
        else
          stypep%st = ESMF_STATE_UNSPECIFIED
        endif
        stypep%alloccount = 0
        stypep%datacount = 0
        nullify(stypep%datalist)

#if defined (ESMF_ENABLENAMEMAP)
        call ESMF_UtilMapNameCreate (stypep%nameMap, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
#endif

        ! create methodTable object
        call c_ESMC_MethodTableCreate(stypep%methodTable, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set as created
        ESMF_INIT_SET_CREATED(stypep)

        ! set return values
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StateConstructEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateDestruct"
!BOPI
! !IROUTINE: ESMF_StateDestruct -- Internal routine to deallocate space
!
! !INTERFACE:
      recursive subroutine ESMF_StateDestruct(stypep, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt State}. In particular
!     all associated proxy objects are destroyed.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Destroy contents of this {\tt ESMF\_StateClass}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! Local vars
        integer :: localrc
        integer :: memstat      ! Stat from allocate/deallocate
        type(ESMF_Status):: status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        call ESMF_BaseGetStatus(stypep%base, status, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
        
        if (status .eq. ESMF_STATUS_READY) then
        
          ! mark object invalid, and free each of the blocks associated
          ! with each entry.  note that we are not freeing the objects
          ! themselves; they could be added to multiple states.  it is
          ! the user's responsibility to delete them when finished.
          stypep%st = ESMF_STATE_INVALID
          stypep%datacount = 0

          ! Now release the entire list
          if (associated(stypep%datalist)) then
            deallocate(stypep%datalist, stat=memstat)
            if (ESMF_LogFoundDeallocError(memstat, "data list", &
                                         ESMF_CONTEXT, rc)) return
            nullify(stypep%datalist)
          endif
          stypep%alloccount = 0

          ! destroy the methodTable object
          call c_ESMC_MethodTableDestroy(stypep%methodTable, localrc)
          if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLENAMEMAP)
          ! destroy the nameMap
          call ESMF_UtilMapNameDestroy (stypep%nameMap, rc=localrc)
          if (ESMF_LogFoundError (localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
#endif
        endif

        ! Set as deleted
        ESMF_INIT_SET_DELETED(stypep)

        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClsAddRHandleList"
!BOPI
! !IROUTINE: ESMF_StateClsAddRHandleList - Add a list of RouteHandles to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClsAddRHandleList(stypep, acount, routehandles, &
        replaceflag, proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer    :: stypep
      integer,               intent(in) :: acount
      type(ESMF_RouteHandle),intent(in) :: routehandles(:)
      logical,               intent(in),  optional :: replaceflag
      logical,               intent(in),  optional :: proxyflag
      integer,               intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple routehandles to an {\tt ESMF\_State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt ESMF\_StateClass}.
!     \item[acount]
!       The number of {\tt ESMF\_RouteHandle}s to be added.
!     \item[routehandles]
!       The array of {\tt ESMF\_RouteHandle}s to be added.
!     \item[proxyflag]
!       Indicate whether this is a proxy object. 
!     \item[replaceflag]
!       If set, indicates a replacement operation where there must be an
!       pre-existing item with the same name.  If there is no pre-existing
!       item, an error is returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      logical :: localrepflag             ! local replace-only flag
      integer :: memstat                  ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: rhname
      logical, allocatable, dimension(:) :: atodo
      integer :: i
      integer :: newcount, aindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)
      do i=1,acount
         ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit,routehandles(i),rc)
      enddo

      if (present (replaceflag)) then
        localrepflag = replaceflag
      else
        localrepflag = .false.
      end if

      rhname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (acount <= 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "acount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (acount > size (routehandles)) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
                                     "acount must be <= size (routehandles)", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the routehandles to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.  
      ! How can this happen?  is atodo some sort of static?
      if (allocated(atodo)) then
        call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
                                         "atodo already allocated", &
                                         ESMF_CONTEXT, rc)
        deallocate(atodo, stat=memstat)
        if (ESMF_LogFoundDeallocError (memstat, 'atodo',  &
                                         ESMF_CONTEXT, rc)) return
        return
      endif

      allocate(atodo(acount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, &
                                     "adding RouteHandles to a State", &
                                     ESMF_CONTEXT, rc)) return

      atodo = .false.

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the routehandle list.
      ! For each routehandle...
      do i=1, acount

        !todo: remove comment-out when RouteHandleValidate available
        !call ESMF_RouteHandleValidate(routehandles(i), rc=localrc)
        !if (localrc .ne. ESMF_SUCCESS) then
        !    write(errmsg, *) "item", i
        !    call ESMF_LogSetError(localrc, errmsg, &
        !                                ESMF_CONTEXT, rc)
        !    deallocate(atodo, stat=localrc)
        !    return
        !endif
        call ESMF_RouteHandleGet(routehandles(i), name=rhname, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=memstat)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, rhname, .false., &
                                        dataitem=dataitem, dataindex=aindex,  &
                                        rc=localrc)
        if (ESMF_LogFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=memstat)
          return
        endif
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            if (.not. localrepflag) then
              newcount = newcount + 1
              aindex = -1
              atodo(i) = .true.
            else
              if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP,  &
                                      "existing RouteHandle '" // trim (rhname) // "' not found", &
                                      ESMF_CONTEXT, rc)) then
                deallocate (atodo, stat=memstat)
                return
              end if
            end if
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn here that an existing object is being
                ! replaced...
            endif

            dataitem%otype = ESMF_STATEITEM_ROUTEHANDLE
            dataitem%datap%rp = routehandles(i)
        
            ! don't update flags on existing entry
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary routehandles first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new routehandles to the list.
      ! This is the start of the second pass through the routehandle list.
      do i=1, acount

        ! If routehandle wasn't already found in the list, we need to add it here.
        if (atodo(i)) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_ROUTEHANDLE
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            call ESMF_RouteHandleGet(routehandles(i), name=nextitem%namep, &
              rc=localrc)
            if (ESMF_LogFoundError(localrc, "getting name from routehandle", &
                                      ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#endif

            nextitem%datap%rp = routehandles(i)

            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag arrays
      deallocate(atodo, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, "deallocating internal list, 1c", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClsAddRHandleList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClsAddArrayList"
!BOPI
! !IROUTINE: ESMF_StateClsAddArrayList - Add a list of Arrays to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClsAddArrayList(stypep, acount, arrays, &
        replaceflag, proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass),  &
                        pointer    :: stypep
      integer,          intent(in) :: acount
      type(ESMF_Array), intent(in) :: arrays(:)
      logical,          intent(in),  optional :: replaceflag
      logical,          intent(in),  optional :: proxyflag
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!      Add multiple arrays to an {\tt ESMF\_State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt ESMF\_StateClass}.
!     \item[acount]
!       The number of {\tt ESMF\_Arrays} to be added.
!     \item[arrays]
!       The array of {\tt ESMF\_Arrays} to be added.
!     \item[proxyflag]
!       Indicate whether this is a proxy object. 
!     \item[replaceflag]
!       If set, indicates a replacement operation where there must be an
!       pre-existing item with the same name.  If there is no pre-existing
!       item, an error is returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      logical :: localrepflag             ! local replace-only flag
      integer :: memstat                  ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: aname
      logical, allocatable, dimension(:) :: atodo
      integer :: i
      integer :: newcount, aindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)
      do i=1,acount
         ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,arrays(i),rc)
      enddo

      if (present (replaceflag)) then
        localrepflag = replaceflag
      else
        localrepflag = .false.
      end if

      aname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (acount <= 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "acount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (acount > size (arrays)) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
                                     "acount must be <= size (arrays)", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the arrays to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.  
      ! How can this happen?  is atodo some sort of static?
      if (allocated(atodo)) then
        call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
                                         "atodo already allocated", &
                                         ESMF_CONTEXT, rc)
        deallocate(atodo, stat=memstat)
        return
      endif

      allocate(atodo(acount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, &
                                     "adding Arrays to a State", &
                                     ESMF_CONTEXT, rc)) return

      atodo = .false.

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the array list.
      ! For each array...
      do i=1, acount

        !todo: remove comment-out when ArrayValidate available
        !call ESMF_ArrayValidate(arrays(i), rc=localrc)
        !if (localrc .ne. ESMF_SUCCESS) then
        !    write(errmsg, *) "item", i
        !    call ESMF_LogSetError(localrc, errmsg, &
        !                                ESMF_CONTEXT, rc)
        !    deallocate(atodo, stat=localrc)
        !    return
        !endif
        call ESMF_ArrayGet(arrays(i), name=aname, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=memstat)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, aname, .false., &
                                        dataitem=dataitem, dataindex=aindex,  &
                                        rc=localrc)
        if (ESMF_LogFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=memstat)
          return
        endif
   
        ! If not, and replaceflag is not set, in the second pass we will
        ! need to add it.
        if (.not. exists) then
            if (.not. localrepflag) then
              newcount = newcount + 1
              aindex = -1
              atodo(i) = .true.
            else
              if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP,  &
                                      "existing Array '" // trim (aname) // "' not found", &
                                      ESMF_CONTEXT, rc)) then
                deallocate (atodo, stat=memstat)
                return
              end if
            end if
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn here that an existing object is being
                ! replaced...
            endif

            dataitem%otype = ESMF_STATEITEM_ARRAY
            dataitem%datap%ap = arrays(i)
        
            ! don't update flags on existing entry
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new arrays to the list.
      ! This is the start of the second pass through the array list.
      do i=1, acount

        ! If array wasn't already found in the list, we need to add it here.
        if (atodo(i)) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_ARRAY
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            call ESMF_ArrayGet(arrays(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogFoundError(localrc, "getting name from array", &
                                      ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#endif

            nextitem%datap%ap = arrays(i)
 
            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag arrays
      deallocate(atodo, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, "deallocating internal list, 1c", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClsAddArrayList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClsAddArrayBundleList"
!BOPI
! !IROUTINE: ESMF_StateClsAddArrayBundleList - Add a list of ArrayBundles to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClsAddArrayBundleList(stypep, acount, arraybundles, &
        replaceflag, proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass),  pointer    :: stypep
      integer,                intent(in) :: acount
      type(ESMF_ArrayBundle), intent(in) :: arraybundles(:)
      logical,                intent(in),  optional :: replaceflag
      logical,                intent(in),  optional :: proxyflag
      integer,                intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple arraybundles to an {\tt ESMF\_State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt ESMF\_StateClass}.
!     \item[acount]
!       The number of {\tt ESMF\_ArrayBundles} to be added.
!     \item[arraybundles]
!       The array of {\tt ESMF\_ArrayBundles} to be added.
!     \item[proxyflag]
!       Indicate whether this is a proxy object. 
!     \item[replaceflag]
!       If set, indicates a replacement operation where there must be an
!       pre-existing item with the same name.  If there is no pre-existing
!       item, an error is returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      logical :: localrepflag             ! local replace-only flag
      integer :: memstat                  ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: aname
      logical, allocatable :: atodo(:)
      integer :: i
      integer :: newcount, aindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)
      do i=1,acount
         ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit,arraybundles(i),rc)
      enddo

      if (present (replaceflag)) then
        localrepflag = replaceflag
      else
        localrepflag = .false.
      end if

      aname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (acount <= 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "acount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (acount > size (arraybundles)) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
                                     "acount must be <= size (arraybundles)", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the arraybundles to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.  
      ! How can this happen?  is atodo some sort of static?
      if (allocated(atodo)) then
        call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
                                         "atodo already allocated", &
                                         ESMF_CONTEXT, rc)
        deallocate(atodo, stat=memstat)
        return
      endif

      allocate(atodo(acount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, &
                                     "adding ArrayBundles to a State", &
                                     ESMF_CONTEXT, rc)) return

      atodo = .false.

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the arraybundle list.
      ! For each arraybundle...
      do i=1, acount

        !todo: remove comment-out when ArrayBundleValidate available
        !call ESMF_ArrayBundleValidate(arraybundles(i), rc=localrc)
        !if (localrc .ne. ESMF_SUCCESS) then
        !    write(errmsg, *) "item", i
        !    call ESMF_LogSetError(localrc, errmsg, &
        !                                ESMF_CONTEXT, rc)
        !    deallocate(atodo, stat=localrc)
        !    return
        !endif
        call ESMF_ArrayBundleGet(arraybundles(i), name=aname, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=memstat)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, aname, .false., &
                                        dataitem=dataitem, dataindex=aindex, &
                                        rc=localrc)
        if (ESMF_LogFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=memstat)
          return
        endif
   
        ! If not, and replaceflag is not set, in the second pass we will
        ! need to add it.
        if (.not. exists) then
            if (.not. localrepflag) then
              newcount = newcount + 1
              aindex = -1
              atodo(i) = .true.
            else
              if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP,  &
                                      "existing ArrayBundle '" // trim (aname) // "' not found", &
                                      ESMF_CONTEXT, rc)) then
                deallocate (atodo, stat=memstat)
                return
              end if
            end if
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn here that an existing object is being
                ! replaced...
            endif

            dataitem%otype = ESMF_STATEITEM_ARRAYBUNDLE
            dataitem%datap%abp = arraybundles(i)
        
            ! don't update flags on existing entry
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arraybundles first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new arraybundles to the list.
      ! This is the start of the second pass through the arraybundle list.
      do i=1, acount

        ! If arraybundle wasn't already found in the list, we need to add it here.
        if (atodo(i)) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_ARRAYBUNDLE
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            call ESMF_ArrayBundleGet(arraybundles(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogFoundError(localrc, "getting name from arraybundle", &
                                      ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#endif

            nextitem%datap%abp = arraybundles(i)
 
            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag arraybundles
      deallocate(atodo, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, "deallocating internal list, 1c", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClsAddArrayBundleList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClsAddFieldList"
!BOPI
! !IROUTINE: ESMF_StateClsAddFieldList - Add a list of Fields to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClsAddFieldList(stypep, fcount, fields, &
        replaceflag, proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer    :: stypep
      integer,               intent(in) :: fcount
      type(ESMF_Field),      intent(in) :: fields(:)
      logical,               intent(in),  optional :: replaceflag
      logical,               intent(in),  optional :: proxyflag
      integer,               intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple fields to a {\tt State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt ESMF\_StateClass}.
!     \item[fcount]
!       The number of {\tt ESMF\_Fields} to be added.
!     \item[fields]
!       The array of {\tt ESMF\_Fields} to be added.
!     \item[proxyflag]
!       Indicate whether this is a proxy object. 
!     \item[replaceflag]
!       If set, indicates a replacement operation where there must be an
!       pre-existing item with the same name.  If there is no pre-existing
!       item, an error is returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                   ! local error status
      logical :: localrepflag             ! local replace-only flag
      integer :: memstat                   ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: fname
      logical, allocatable :: ftodo(:)
      integer :: i
      integer :: newcount, findex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)
      do i=1,fcount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fields(i),rc)
      enddo

      if (present (replaceflag)) then
        localrepflag = replaceflag
      else
        localrepflag = .false.
      end if
 
      fname = ""

      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (fcount <= 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "fcount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (fcount > size (fields)) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
                                     "fcount must be <= size (fields)", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the fields to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
 
      ! How does this happen?  is ftodo some sort of static?
      if (allocated(ftodo)) then
        ! print *, "ftodo already allocated"
        deallocate(ftodo, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, &
                                  "deallocating fields from a state", &
                                  ESMF_CONTEXT, rc)) return
      endif

      allocate(ftodo(fcount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, &
                                  "adding fields to a state", &
                                  ESMF_CONTEXT, rc)) return
      ftodo = .false.

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the field list.
      ! For each field...
      do i=1, fcount

        call ESMF_FieldValidate(fields(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(ftodo, stat=memstat)
          return
        endif
        call ESMF_FieldGet(fields(i), name=fname, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(ftodo, stat=memstat)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, fname, .false., &
                                        dataitem=dataitem, dataindex=findex, &
                                        rc=localrc)
        if (ESMF_LogFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) then
          deallocate(ftodo, stat=memstat)
          return
        endif
   
        ! If not, and replaceflag is not set, in the second pass we will
        ! need to add it.
        if (.not. exists) then
            if (.not. localrepflag) then
              newcount = newcount + 1
              findex = -1
              ftodo(i) = .true.
            else
              if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP,  &
                                      "existing Field '" // trim (fname) // "' not found", &
                                      ESMF_CONTEXT, rc)) then
                deallocate (ftodo, stat=memstat)
                return
              end if
            end if
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn we are replacing a real object
                ! and not just a name...
            endif

            dataitem%otype = ESMF_STATEITEM_FIELD
            dataitem%datap%fp = fields(i)
        
            ! If we're replacing an existing item, then we shouldn't
            !  alter existing settings on the data state.
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new fields to the list.
      ! This is the start of the second pass through the array list.
      do i=1, fcount

        ! If field wasn't already found in the list, we need to add it here.
        if (ftodo(i)) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_FIELD
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            call ESMF_FieldGet(fields(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#endif

            nextitem%datap%fp = fields(i)
 
            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag array
      deallocate(ftodo, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
                                  "adding fields to a state", &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClsAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClAddFieldBundleList"
!BOPI
! !IROUTINE: ESMF_StateClAddFieldBundleList - Add a list of FieldBundles to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClAddFieldBundleList(stypep, bcount, fieldbundles, &
        replaceflag, proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass),  pointer       :: stypep
      integer,                intent(in)    :: bcount
      type(ESMF_FieldBundle), intent(inout) :: fieldbundles(:)
      logical,                intent(in),  optional :: replaceflag
      logical,                intent(in),  optional :: proxyflag
      integer,                intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple fieldbundles to an {\tt ESMF\_State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Internal StateClass pointer.  Required.
!     \item[fieldbundles]
!       The array of {\tt ESMF\_FieldBundles} to be added.
!     \item[bcount]
!       The number of {\tt ESMF\_FieldBundles} to be added.
!     \item[proxyflag]
!       Indicate whether this is a proxy object. 
!     \item[replaceflag]
!       If set, indicates a replacement operation where there must be an
!       pre-existing item with the same name.  If there is no pre-existing
!       item, an error is returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      logical :: localrepflag             ! local replace-only flag
      integer :: memstat                  ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      type(ESMF_Field) :: field
      character(len=ESMF_MAXSTR) :: bname, fname
      logical, allocatable :: btodo(:)
      integer, allocatable :: ftodo(:)
      integer :: bindex, findex 
      integer :: i, j
      integer :: fcount, fruncount, newcount
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      fname = ""

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)
      do i=1,bcount
         ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit,fieldbundles(i),rc)
      enddo

      if (present (replaceflag)) then
        localrepflag = replaceflag
      else
        localrepflag = .false.
      end if
      if (localrepflag) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "replace option not supported yet", &
                                     ESMF_CONTEXT, rc)) return
      endif
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (bcount <= 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "bcount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (bcount > size (fieldbundles)) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
                                     "bcount must be <= size (fieldbundles)", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the fieldbundles to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! get a count of all fields in all fieldbundles
      fruncount = 0
      do i=1, bcount
        call ESMF_FieldBundleValidate(fieldbundles(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        call ESMF_FieldBundleGet(fieldbundles(i), fieldCount=fcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        fruncount = fruncount + fcount
      enddo

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(btodo(bcount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, "btodo", &
                                     ESMF_CONTEXT, rc)) return

      ! IMPORTANT: from here down, do not return on error, but goto 10
      !  to at least try to deallocate the temp storage.

      btodo = .false.

      if (fruncount .ge. 0) then
        allocate(ftodo(fruncount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, "ftodo", &
                                       ESMF_CONTEXT, rc)) goto 10
        ftodo = 0
      endif

  
      ! Initialize counters to 0, indices to 1
      fruncount = 1
      newcount = 0

      ! This is the start of the first pass through the fieldbundle list.
      ! For each fieldbundle...
      do i=1, bcount

        call ESMF_FieldBundleGet(fieldbundles(i), name=bname, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) goto 10
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, bname, .false., &
                                        dataitem=dataitem, dataindex=bindex,  &
                                        rc=localrc)
        if (ESMF_LogFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) goto 10
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            bindex = -1
            btodo(i) = .true.
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn we are replacing a real object
                ! and not just a placeholder...
            endif

            dataitem%otype = ESMF_STATEITEM_FIELDBUNDLE
            dataitem%datap%fbp = fieldbundles(i)
        
            ! Don't change flags of existing entry
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif

        ! and now the same for each field in the fieldbundle
        call ESMF_FieldBundleGet(fieldbundles(i), fieldCount=fcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) goto 10

        do j=1, fcount
            ! get next field and query name
            call ESMF_FieldBundleGet(fieldbundles(i), j, field, localrc)
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            ! what is this?  leftover debugging code?
            !call ESMF_FieldPrint(field, "", localrc)

            call ESMF_FieldGet(field, name=fname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10
    
            exists = ESMF_StateClassFindData(stypep, fname, .false.,  &
                                      dataitem=dataitem, dataindex=findex, &
                                      rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            ! If the field is going to have to be added later,
            !  keep track of whether it belongs to a fieldbundle which has to
            !  be added new, or exists.  If it exists, note the index number
            !  so we don't have to search for it later.
            if (.not. exists) then
                newcount = newcount + 1
                ftodo(fruncount) = bindex
            else
                ! TODO: decide if we need to verify that this is only a
                ! placeholder, or if it's ok to silently overwrite an array
                ! or fieldbundle which had the same name.
                if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                  ! print *, "Warning: overwriting old entry"
                endif

                ! Set up the new entry.
                dataitem%otype = ESMF_STATEITEM_INDIRECT
                if (bindex .eq. -1) then
                    ! We found the field already in the state list but
                    ! not the fieldbundle, so we can't set the right index yet.
                    ! Set a flag so later we can update this in pass 2.
                    ftodo(fruncount) = -2
                    dataitem%indirect_index = 0   
                else
                    dataitem%indirect_index = bindex
                endif
            
                dataitem%needed = stypep%needed_default
                dataitem%ready = stypep%ready_default
                dataitem%valid = stypep%stvalid_default
                dataitem%reqrestart = stypep%reqrestart_default
            endif

            ! This is a total running count of all fields in all fieldbundles.
            fruncount = fruncount+1
    
        enddo
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0)  goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10


      ! There is enough space now to add new fieldbundles & fields to the list.
      ! This is the start of the second pass through the fieldbundle list.
      fruncount = 1     
      do i=1, bcount

        ! If fieldbundle wasn't already found in the list, we need to add it here.
        if (btodo(i)) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_FIELDBUNDLE
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            call ESMF_FieldBundleGet(fieldbundles(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

#if defined (ESMF_NAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#endif

           nextitem%datap%fbp = fieldbundles(i)

           nextitem%needed = stypep%needed_default
           nextitem%ready = stypep%ready_default
           nextitem%valid = stypep%stvalid_default
           nextitem%reqrestart = stypep%reqrestart_default

            ! save the current datalist index for the fields code below
            bindex = stypep%datacount

        endif

        ! Whether it was found in pass 1 or just added above, we still
        !  have to go through each field and see if any of them need to
        !  be added or updated.
        call ESMF_FieldBundleGet(fieldbundles(i), fieldCount=fcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) goto 10

        ! Skip empty fieldbundles
        if (fcount .le. 0) cycle

        ! for each field in the fieldbundle
        do j=1, fcount

          ! If new field entry needs to be added
          if (ftodo(fruncount) .eq. -1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_INDIRECT
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag
    
            ! get next field and query name
            call ESMF_FieldBundleGet(fieldbundles(i), j, field, localrc)
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            call ESMF_FieldGet(field, name=nextitem%namep, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10
    
            ! If we found the corresponding fieldbundle entry during pass 1,
            ! it was stored in the todo list.  Otherwise, we just added it
            ! above and we saved the new index to use here.
            if (ftodo(fruncount) .ge. 0) then
                nextitem%indirect_index = ftodo(fruncount)
            else
                nextitem%indirect_index = bindex
            endif

            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default

          ! If the field entry already existed but needs fieldbundle index updated,
          !  we do have to do a lookup on the field to see where it was
          !  found.  We just added the fieldbundle above, so bindex is the
          !  value to set.
          else if (ftodo(fruncount) .eq. -2) then
            exists = ESMF_StateClassFindData(stypep, fname, .true.,  &
                                            dataitem=dataitem, dataindex=findex, &
                                            rc=localrc)

            if (.not. exists) then
              call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
                                          "field/fieldbundle lists", &
                                          ESMF_CONTEXT, rc)
              localrc = ESMF_RC_INTNRL_INCONS
              goto 10
            endif
            dataitem%indirect_index = bindex
          endif
  
          ! Update the running count.
          fruncount = fruncount+1
        enddo

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
      ! If not, rc should be already set, and goto 10 to delete before
      ! returning the original error code.
      if (present(rc)) rc = ESMF_SUCCESS

10    continue

      ! Get rid of temp flag arrays.  only report yet another error if up to
      ! here we think everything is ok.  otherwise report the original error
      ! and ignore any wimpering from dealloc.

      if (allocated (btodo)) then
        deallocate(btodo, stat=memstat)
        if (localrc == ESMF_SUCCESS) then
          if (ESMF_LogFoundDeallocError(memstat, "fieldbundle list", &
                                         ESMF_CONTEXT, rc)) return
        end if
      end if

      if (allocated (ftodo)) then
        deallocate(ftodo, stat=memstat)
        if (localrc == ESMF_SUCCESS) then
          if (ESMF_LogFoundDeallocError(memstat, "field list", &
                                         ESMF_CONTEXT, rc)) return
        end if
      end if

      if (present(rc)) rc = localrc
      end subroutine ESMF_StateClAddFieldBundleList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClsAddStateList"
!BOPI
! !IROUTINE: ESMF_StateClsAddStateList - Add a list of States to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClsAddStateList(stypep, scount, states, &
        replaceflag, proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer,               intent(in) :: scount
      type(ESMF_State),      intent(in) :: states(:)
      logical,               intent(in),  optional :: replaceflag
      logical,               intent(in),  optional :: proxyflag
      integer,               intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple states to a {\tt State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt StateClass}.
!     \item[scount]
!       The number of {\tt ESMF\_State}s to be added.
!     \item[nestedstate]
!       The array of {\tt ESMF\_State}s to be added.
!     \item[proxyflag]
!       Indicate whether this is a proxy object. 
!     \item[replaceflag]
!       If set, indicates a replacement operation where there must be an
!       pre-existing item with the same name.  If there is no pre-existing
!       item, an error is returned.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      logical :: localrepflag             ! local replace-only flag
      integer :: memstat                  ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: sname
      logical, allocatable :: stodo(:)
      integer :: i
      integer :: newcount, sindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)
      do i=1,scount
         ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,states(i),rc)
      enddo

      if (present (replaceflag)) then
        localrepflag = replaceflag
      else
        localrepflag = .false.
      end if

      sname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (scount <= 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "scount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (scount > size (states)) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
                                     "scount must be <= size (states)", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the states to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(stodo(scount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, "adding States", &
                                       ESMF_CONTEXT, rc)) return
      stodo = .false.

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the state list.
      ! For each state...
      do i=1, scount

        call ESMF_StateValidate(states(i), options="", rc=localrc)
        ! TODO: add state number to error msg
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=memstat)
          return
        endif

        ! Do a one-level check for adding a State to itself, but no deeper.
        ! If a nested State is added to another State, and then the combined
        ! State is added to the original State, this code is not going to 
        ! detect that loop.
        if (associated(stypep, states(i)%statep)) then
           call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                                    "Cannot add a State to itself", &
                                    ESMF_CONTEXT, rc)
          deallocate(stodo, stat=memstat)
          return
        endif
   
        call c_ESMC_GetName(states(i)%statep%base, sname, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=memstat)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, sname, .false.,  &
                                         dataitem=dataitem, dataindex=sindex,  &
                                         rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=memstat)
          return
        endif
   
        ! If not, and replaceflag is not set, in the second pass we will
        ! need to add it.
        if (.not. exists) then
            if (.not. localrepflag) then
              newcount = newcount + 1
              sindex = -1
              stodo(i) = .true.
            else
              if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP,  &
                                      "existing nested State '" // trim (sname) // "' not found", &
                                      ESMF_CONTEXT, rc)) then
                deallocate (stodo, stat=memstat)
                return
              end if
            end if
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn we are replacing a real object
                ! and not just a placeholder
            endif

            dataitem%otype = ESMF_STATEITEM_STATE
            dataitem%datap%spp => states(i)%statep
        
            ! don't update flags on existing entry
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary states first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=memstat)
          return
      endif


      ! There is enough space now to add new states to the list.
      ! This is the start of the second pass through the state list.
      do i=1, scount

        ! If state wasn't already found in the list, we need to add it here.
        if (stodo(i)) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_STATE
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            call c_ESMC_GetName(states(i)%statep%base, nextitem%namep, localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
              deallocate(stodo, stat=memstat)
              return
            end if

#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) then
              deallocate(stodo, stat=memstat)
              return
            end if
#endif

            nextitem%datap%spp => states(i)%statep
 
            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag states
      deallocate(stodo, stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, &
                                       "Adding States to a State", &
                                       ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClsAddStateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassFindData"
!BOPI
! !IROUTINE: ESMF_StateClassFindData - internal routine to find data item by name
!
! !INTERFACE:
      function ESMF_StateClassFindData(stypep, dataname, expected,  &
                                       nestedFlag, dataitem, dataindex, rc)
!
! !RETURN VALUE:
      logical :: ESMF_StateClassFindData
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer               :: stypep
      character (len=*),     intent(in)            :: dataname
      logical,               intent(in)            :: expected
      logical,               intent(in),  optional :: nestedFlag
      type(ESMF_StateItem),  pointer,     optional :: dataitem
      integer,               intent(out), optional :: dataindex
      integer,               intent(out), optional :: rc             

! !DESCRIPTION:
!    Returns {\tt TRUE} if a data item with this name is found, and returns
!    a pointer to it in the {\tt dataitem} argument.  Returns {\tt FALSE}
!    if this name is not found.  If {\tt expected} is true and the name is
!    not found, sets error code on return.  Otherwise does NOT set error code
!    and the return value is simply the answer to a query.  Sets error code
!    in either case if true error conditions are found.
!
!     The arguments are:
!     \begin{description}     
!     \item[stypep]
!       {\tt ESMF\_StateClass} to query.
!      \item[dataname]
!       Name of the data item to query.
!      \item[expected]
!       Logical.  If set to {\tt true} the name must be found or an error code 
!       is set. The default is {\tt false} and the error code is not set if 
!       the name is not found.
!      \item[{[nestedFlag]}]
!       If set to {\tt .true.}, will search nested States for
!       {\tt dataname}.
!      \item[{[dataitem]}]
!       Pointer to the corresponding {\tt ESMF\_StateItem} item if one is
!       found with the right name.
!      \item[{[dataindex]}]
!       Index number in datalist where this name was found.  When nested
!       States are being searched, this index refers to the State where
!       the item was found.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      integer :: memstat                  ! Stat from allocate/deallocate
      integer :: itemindex
      logical :: itemfound
      type(ESMF_StateClass), pointer  :: nested_sp
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: usenested_lookup
#if !defined (ESMF_ENABLENAMEMAP)
      type(ESMF_StateItem), pointer :: nextitem0
      integer :: i0, dcount0
#endif

      ! Initialize return code.  Assume failure until success assured.
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      ESMF_StateClassFindData = .FALSE.

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)

      if (present (dataitem)) nullify (dataitem)

      usenested_lookup = .false.
      if (present (nestedFlag)) then
        usenested_lookup = nestedFlag
      end if

      if (usenested_lookup .and. index (dataname, '/') /= 0) then
        if (ESMF_LogFoundError (ESMF_RC_ARG_INCOMP,  &
                                 ESMF_ERR_PASSTHRU,  &
                                 ESMF_CONTEXT, rc)) return
      end if
  
      ! This function is only called internally, so we do not need to check
      ! the validity of the state - it has been checked before we get here.

      if (.not. usenested_lookup) then
         call find_pathed_item_worker (stypep, lpath=dataname,  &
             lfound=itemfound, lindex=itemindex, litem=dataitem)
      else
#if defined (ESMF_ENABLENAMEMAP)
        call ESMF_UtilMapNameLookup (stypep%nameMap, name=dataname,  &
            value=itemindex, foundFlag=itemfound,  &
            rc=localrc)
        if (ESMF_LogFoundError (localrc,  &
                                   ESMF_ERR_PASSTHRU,  &
                                   ESMF_CONTEXT, rc)) return
#else
        dcount0 = stypep%datacount
        do, i0=1, dcount0
          nextitem0 => stypep%datalist(i0)
          if (nextitem0%namep == dataname) then
            itemindex = i0
            exit
          end if
        end do
        itemfound = i0 <= dcount0
#endif

        if (itemfound) then
          if (present(dataitem)) dataitem => stypep%datalist(itemindex)
        else
           call find_nested_item_worker (stypep,  &
               lfound=itemfound, lindex=itemindex, litem=dataitem)
        end if
      end if

      if (itemfound) then
        ESMF_StateClassFindData = .TRUE.
        if (present(dataindex)) dataindex = itemindex
        localrc = ESMF_SUCCESS
      else   ! item not found
        ESMF_StateClassFindData = .FALSE.
        if (expected) then 
          localrc = ESMF_FAILURE
        else
          localrc = ESMF_SUCCESS
        end if
      end if

      if (present(rc)) rc = localrc

      contains

        recursive subroutine find_nested_item_worker (sp, lfound, lindex, litem)
          type(ESMF_StateClass), pointer  :: sp
          logical,            intent(out) :: lfound
          integer,            intent(out) :: lindex
          type(ESMF_StateItem), pointer, optional   :: litem

          type(ESMF_StateClass), pointer  :: sp_local
          integer       :: i1
          integer       :: lrc
#if !defined (ESMF_ENABLENAMEMAP)
          type(ESMF_StateItem), pointer :: nextitem1
          integer :: dcount1
#endif

!          print *, 'find_nested_item_worker: entered'

        ! Scan this levels list for States

          lfound = .false.

#if defined (ESMF_ENABLENAMEMAP)
          call ESMF_UtilMapNameLookup (sp%nameMap, name=dataname,  &
              value=lindex, foundFlag=lfound,  &
              rc=lrc)
          if (ESMF_LogFoundError (lrc,  &
                                     ESMF_ERR_PASSTHRU,  &
                                     ESMF_CONTEXT, rc)) return
#else
          dcount1 = sp%datacount
          do, i1=1, dcount1
            nextitem1 => sp%datalist(i1)
            if (nextitem1%namep == dataname) then
              lindex = i1
              exit
            end if
          end do
          lfound = i1 <= dcount1
#endif
          if (.not. lfound) then
            do, i1=1, sp%datacount
              if (sp%datalist(i1)%otype == ESMF_STATEITEM_STATE) then
                sp_local => sp%datalist(i1)%datap%spp
                call find_nested_item_worker (sp_local, lfound, lindex, litem)
                if (lfound) exit
              end if
            end do
            if (present (litem)) then
              if (lfound) then
                litem => sp_local%datalist(lindex)
              else
                litem => null ()
              end if
            end if
          end if

!          print *, 'find_nested_item_worker: exiting.  Found flag =', lfound


        end subroutine find_nested_item_worker

        recursive subroutine find_pathed_item_worker (sp, lpath, lfound, lindex, litem)
          type(ESMF_StateClass), pointer  :: sp
          character(*), intent(in)  :: lpath
          logical,      intent(out) :: lfound
          integer,      intent(out) :: lindex
          type(ESMF_StateItem), pointer, optional   :: litem

          type(ESMF_StateClass), pointer  :: sp_local
          character(len (lpath)) :: itempath_local
          integer :: slashpos
          integer :: i1
          integer :: lrc
#if !defined (ESMF_ENABLENAMEMAP)
          type(ESMF_StateItem), pointer :: nextitem1
          integer :: dcount1
#endif

          ! print *, 'find_pathed_item_worker: entered.  lpath = ', lpath

        ! Strip leading slashes

          do, i1=1, len (lpath)
            if (lpath(i1:i1) /= '/') exit
          end do
          if (i1 > len (lpath)) then
             itempath_local = lpath
          else
             itempath_local = lpath(i1:)
          end if

          slashpos = index (itempath_local, '/')

        ! Look for the name for this level

          lfound = .false.

          if (slashpos > 0) then
            ! Midway through the path, so only a State name is valid
#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameLookup (sp%nameMap, name=itempath_local(:slashpos-1),  &
                                       value=lindex, foundFlag=lfound,  &
                                       rc=lrc)
            if (ESMF_LogFoundError (lrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#else
            dcount1 = sp%datacount
            do, i1=1, dcount1
              nextitem1 => sp%datalist(i1)
              if (nextitem1%namep == itempath_local(:slashpos-1)) then
                lindex = i1
                exit
              end if
            end do
            lfound = i1 <= dcount1
#endif
            if (lfound) then
              if (sp%datalist(lindex)%otype == ESMF_STATEITEM_STATE) then
                sp_local => sp%datalist(lindex)%datap%spp
                call find_pathed_item_worker (sp_local, itempath_local(slashpos+1:),  &
                                              lfound, lindex, litem)
              else
                lfound = .false.
              end if
            end if

            if (.not. lfound) then
              if (present (litem)) then
                litem => null ()
              end if
            end if

          else
            ! End of path, so any item is OK
#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameLookup (sp%nameMap, name=itempath_local,  &
                                       value=lindex, foundFlag=lfound,  &
                                       rc=lrc)
            if (ESMF_LogFoundError (lrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#else
            dcount1 = sp%datacount
            do, i1=1, dcount1
              nextitem1 => sp%datalist(i1)
              if (nextitem1%namep == itempath_local) then
                lindex = i1
                exit
              end if
            end do
            lfound = i1 <= dcount1
#endif
            if (present (litem)) then
              if (lfound) then
                litem => sp%datalist(lindex)
              else
                litem => null ()
              end if
            end if
          end if

          ! print *, 'find_pathed_item_worker: exiting.  Found flag =', lfound


        end subroutine find_pathed_item_worker

      end function ESMF_StateClassFindData

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClsAddDataNameList"
!BOPI
! !IROUTINE: ESMF_StateClsAddDataNameList - internal routine
!
! !INTERFACE:
      subroutine ESMF_StateClsAddDataNameList(stypep, ncount, namelist, &
        proxyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: ncount
      character (len=*), intent(in) :: namelist(:)
      logical, optional :: proxyflag
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a list of {\tt name}s to an existing {\tt State}.
!      The {\tt name}s must be unique within the {\tt State}
!      They are available to be marked {\tt needed} by the
!      consumer of the export {\tt State}. Then the data 
!      provider can replace the name with the actual {\tt FieldBundle},
!      {\tt Field}, or {\tt Array} which carries the needed data.
!      Unneeded data need not be generated.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt ESMF\_StateClass}.
!     \item[name]
!       The name to be added.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                   ! local error status
      integer :: memstat                   ! Stat from allocate/deallocate
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      integer, allocatable, dimension(:) :: ntodo
      integer :: i
      integer :: newcount, nindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)

      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (ncount .le. 0) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, "ncount must be >= 0", &
                                      ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the fields to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(ntodo(ncount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, "adding names to a state", &
                                     ESMF_CONTEXT, rc)) return
      ntodo(1:ncount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the names list.
      ! For each name...
      do i=1, ncount

        ! Make sure name does not have a slash in it
        if (index (namelist(i), '/') > 0) then
          if (ESMF_LogFoundError (ESMF_RC_ARG_BAD,  &
                                    "namelist name must not have slashes in it",  &
                                    ESMF_CONTEXT, rc)) return
        end if
                                 

        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, namelist(i), .false., &
                                dataitem=dataitem, dataindex=nindex,  &
                                rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            nindex = -1
            ntodo(i) = 1
        else
            ! It does already exist.  
            ! TODO: should a name replace an existing data item?  In a sense
            !  this is a way to "delete" an entry.  So I am going to implement
            !  it that way.  But we should revisit this and see if it is
            !  how people want this to behave.
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
              ! optionally warn here
            endif

            dataitem%otype = ESMF_STATEITEM_NAME
            ! don't have to add name, we already matched it.

            dataitem%indirect_index = -1

            dataitem%needed = stypep%needed_default
            dataitem%ready = stypep%ready_default
            dataitem%valid = stypep%stvalid_default
            dataitem%reqrestart = stypep%reqrestart_default
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new names to the list.
      ! This is the start of the second pass through the array list.
      do i=1, ncount

        ! If name wasn't already found in the list, we need to add it here.
        if (ntodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_NAME
            nextitem%proxyFlag = .false.  ! default not a proxy
            if (present(proxyflag)) nextitem%proxyFlag = proxyflag

            ! Add name
            nextitem%namep = namelist(i)

#if defined (ESMF_ENABLENAMEMAP)
            call ESMF_UtilMapNameAdd (stypep%nameMap,  &
              name=nextitem%namep, value=stypep%datacount, rc=localrc)
            if (ESMF_LogFoundError (localrc,  &
                                       ESMF_ERR_PASSTHRU,  &
                                       ESMF_CONTEXT, rc)) return
#endif

            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag array
      deallocate(ntodo, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, "adding names to a state", &
                                     ESMF_CONTEXT, rc)) return


      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClsAddDataNameList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassExtendList"
!BOPI
! !IROUTINE: ESMF_StateClassExtendList - internal routine
!
! !INTERFACE:
      subroutine ESMF_StateClassExtendList(stypep, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: itemcount
      integer, intent(out) :: rc
!     
! !DESCRIPTION:
!      Make sure there is enough allocated space for {\tt itemcount}
!      more things in the datalist.  This is an internal-only routine;
!      {\tt rc} is NOT optional, especially since allocation can fail.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Pointer to {\tt ESMF\_StateClass}.
!     \item[itemcount]
!       The number of items that space is needed for.
!     \item[rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      type(ESMF_StateItem), dimension(:), pointer :: temp_list
      integer :: i
      integer :: allocsize 
      integer :: newsize
      integer :: localrc                           ! local error status
      integer :: memstat                           ! Stat from allocate/deallocate
      integer, parameter :: chunksize = 16         ! extend list by this
 
      ! Assume failure until success assured.  rc is not optional here.
      localrc = ESMF_RC_NOT_IMPL
      rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit,stypep,rc)

      ! Not an error to be called with 0 items - just return w/o error.
      if (itemcount .le. 0) then
          rc = ESMF_SUCCESS
          return
      endif

      ! An initially empty list. Simply allocate, no data copy needed.
      if (stypep%alloccount .eq. 0) then

          allocsize = itemcount + chunksize - mod(itemcount,chunksize)
          allocate(stypep%datalist(allocsize), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, "datalist", &
                                         ESMF_CONTEXT, rc)) return
          stypep%alloccount = allocsize

      ! Extend an existing list to the right length, including copy
      else if (stypep%alloccount .lt. stypep%datacount + itemcount) then

          newsize = stypep%datacount + itemcount
          allocsize = newsize + chunksize - mod(newsize,chunksize)
          allocate(temp_list(allocsize), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, "datalist realloc", &
                                         ESMF_CONTEXT, rc)) return
  
          ! Preserve old contents
          do i = 1, stypep%datacount
            temp_list(i) = stypep%datalist(i)
          enddo
  
          ! Delete old list
          deallocate(stypep%datalist, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat, "datalist dealloc", &
                                         ESMF_CONTEXT, rc)) return
  
          ! Now make this the permanent list
          stypep%datalist => temp_list

          stypep%alloccount = allocsize

      endif
   
      rc = ESMF_SUCCESS

      end subroutine ESMF_StateClassExtendList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSerialize"

!BOPI
! !IROUTINE: ESMF_StateSerialize - Serialize state info into a byte stream
!
! !INTERFACE:
      recursive subroutine ESMF_StateSerialize(state, buffer, length, offset, &
                                              attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state 
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFLag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_State} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_StateWrite()} and {\tt ESMF\_StateRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           {\tt ESMF\_State} object to be serialized.
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
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item[{[inquireflag]}}
!           Flag to tell if serialization is to be done (ESMF_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF_INQUIREONLY)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                             ! Error status
      integer :: i
      type(ESMF_StateClass), pointer :: sp           ! state type
      type(ESMF_StateItem), pointer :: sip           ! state item
      type(ESMF_State) :: wrapper
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag


      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      ! deal with optional attreconflag and inquireflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      ! shortcut to internals
      sp => state%statep

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      sp => state%statep

      call c_ESMC_BaseSerialize(sp%base, buffer, length, offset, lattreconflag,  &
                                 linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_StateSerialize(sp%st, sp%needed_default, &
                                 sp%ready_default, sp%stvalid_default, &
                                 sp%reqrestart_default, &
                                 sp%alloccount, sp%datacount, &
                                 buffer, length, offset, linquireflag, &
                                 localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      do i = 1, sp%datacount
          sip => sp%datalist(i)

          call c_ESMC_StateItemSerialize(sip%otype, sip%namep, &
                                         sip%indirect_index, sip%needed, &
                                         sip%ready, sip%valid, sip%reqrestart, &
                                         buffer, length, offset, &
                                         linquireflag, localrc)

          select case (sip%otype%ot)
            case (ESMF_STATEITEM_FIELDBUNDLE%ot)
             call ESMF_FieldBundleSerialize(sip%datap%fbp, buffer, length, &
                                       offset, attreconflag=lattreconflag, &
                                       inquireflag=linquireflag, rc=localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_FIELD%ot)
             call ESMF_FieldSerialize(sip%datap%fp, buffer, length, &
                                       offset, attreconflag=lattreconflag, &
                                       inquireflag=linquireflag, rc=localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_ArraySerialize(sip%datap%ap, buffer(1), &
                                       length, offset, lattreconflag, &
                                       linquireflag, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
             call c_ESMC_ArrayBundleSerialize(sip%datap%abp, buffer(1), &
                                       length, offset, lattreconflag, &
                                       linquireflag, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_STATE%ot)
             wrapper%statep => sip%datap%spp
             ESMF_INIT_SET_CREATED(wrapper)             
             call ESMF_StateSerialize(wrapper, buffer, length, offset, &
                                       attreconflag=lattreconflag, &
                                       inquireflag=linquireflag, rc=localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_NAME%ot)
             call c_ESMC_StringSerialize(sip%namep, buffer(1), &
                                         length, offset, linquireflag, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_INDIRECT%ot)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_UNKNOWN%ot)
              continue ! TODO: serialize
          end select

      enddo

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateDeserialize"

!BOPI
! !IROUTINE: ESMF_StateDeserialize - Deserialize a byte stream into a State
!
! !INTERFACE:
    recursive function ESMF_StateDeserialize(vm, buffer, offset, &
                                            attreconflag, rc) &
              result (substate)
!
! !RETURN VALUE:
      type(ESMF_State) :: substate   
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a State object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_StateWrite()} and {\tt ESMF\_StateRead()}.
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
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! Error status
      integer :: memstat                  ! Stat from allocate/deallocate
      integer :: i
      type(ESMF_StateClass), pointer :: sp           ! state type
      type(ESMF_StateItem), pointer :: sip           ! state item
      type(ESMF_State) :: subsubstate
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_Logical) :: linkChange

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

      ! deal with optional attreconflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif
      ! linkChanges is true for all but Component
      linkChange = ESMF_TRUE

      ! in case of error, make sure this is invalid.
      !nullify(ESMF_StateDeserialize%statep)
      nullify(substate%statep)

      allocate(sp, stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, &
                                     "space for new State object", &
                                     ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseDeserialize(sp%base, buffer, offset, lattreconflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
      call ESMF_BaseSetInitCreated(sp%base, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

      call c_ESMC_StateDeserialize(sp%st, sp%needed_default, &
                                 sp%ready_default, sp%stvalid_default, &
                                 sp%reqrestart_default, &
                                 sp%alloccount, sp%datacount, &
                                 buffer, offset, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

#if defined (ESMF_ENABLENAMEMAP)
      call ESMF_UtilMapNameCreate (sp%nameMap, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
#endif

      allocate(sp%datalist(sp%alloccount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, "State type", &
                                       ESMF_CONTEXT, rc)) return

      do i = 1, sp%datacount
          sip => sp%datalist(i)

          call c_ESMC_StateItemDeserialize(sip%otype, sip%namep, &
                                         sip%indirect_index, sip%needed, &
                                         sip%ready, sip%valid, sip%reqrestart, &
                                         buffer, offset, localrc)

#if defined (ESMF_ENABLENAMEMAP)
! print *, 'StateDeserialize: Adding name: ', trim (sip%namep), ' at location: ', i
          call ESMF_UtilMapNameAdd (sp%nameMap,  &
            name=sip%namep, value=i, rc=localrc)
          if (ESMF_LogFoundError (localrc,  &
                                     ESMF_ERR_PASSTHRU,  &
                                     ESMF_CONTEXT, rc)) return
#endif

          select case (sip%otype%ot)
            case (ESMF_STATEITEM_FIELDBUNDLE%ot)
              sip%datap%fbp = ESMF_FieldBundleDeserialize(buffer, offset, &
                attreconflag=lattreconflag, rc=localrc)
              sip%proxyflag = .true.  ! indicate that this is proxy object
              !  here we relink the State Attribute hierarchy to the FieldBundle
              !  Attribute hierarchy, as they were linked before
              if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value) then
                call c_ESMC_AttributeLink(sp%base, sip%datap%fbp%btypep%base, &
                  linkChange, localrc)
                if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) then
#if defined (ESMF_ENABLENAMEMAP)
                  call ESMF_UtilMapNameDestroy (sp%nameMap)
#endif
                  deallocate(sp%datalist, stat=memstat)
                  return
                endif
              endif

            case (ESMF_STATEITEM_FIELD%ot)
              sip%datap%fp = ESMF_FieldDeserialize(buffer, offset, &
                attreconflag=lattreconflag, rc=localrc)
              sip%proxyflag = .true.  ! indicate that this is proxy object
              !  here we relink the State Attribute hierarchy to the Field
              !  Attribute hierarchy, as they were linked before
              if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value) then
                call c_ESMC_AttributeLink(sp%base, sip%datap%fp%ftypep%base, &
                  linkChange, localrc)
                if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) then
#if defined (ESMF_ENABLENAMEMAP)
                  call ESMF_UtilMapNameDestroy (sp%nameMap)
#endif
                  deallocate(sp%datalist, stat=memstat)
                  return
                endif
              endif

            case (ESMF_STATEITEM_ARRAY%ot)
              call c_ESMC_ArrayDeserialize(sip%datap%ap, buffer, offset, &
                lattreconflag, localrc)
              sip%proxyflag = .true.  ! indicate that this is proxy object

            case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
              call c_ESMC_ArrayBundleDeserialize(sip%datap%abp, buffer, offset,&
                lattreconflag, localrc)
              sip%proxyflag = .true.  ! indicate that this is proxy object

            case (ESMF_STATEITEM_STATE%ot)
              subsubstate = ESMF_StateDeserialize(vm, buffer, offset, &
                                              attreconflag=lattreconflag, rc=localrc)
              sip%datap%spp => subsubstate%statep
              sip%proxyflag = .true.  ! indicate that this is proxy object
              !  here we relink the State Attribute hierarchy to the subState
              !  Attribute hierarchy, as they were linked before
              if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value) then
                call c_ESMC_AttributeLink(sp%base, sip%datap%spp%base, &
                  linkChange, localrc)
                if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) then
#if defined (ESMF_ENABLENAMEMAP)
                  call ESMF_UtilMapNameDestroy (sp%nameMap)
#endif
                  deallocate(sp%datalist, stat=memstat)
                  return
                endif
              endif

            case (ESMF_STATEITEM_NAME%ot)
              call c_ESMC_StringDeserialize(sip%namep, buffer(1), offset, localrc)
              sip%proxyflag = .true.  ! indicate that this is proxy object

            case (ESMF_STATEITEM_INDIRECT%ot)
              continue ! TODO: deserialize

            case (ESMF_STATEITEM_UNKNOWN%ot)
              continue ! TODO: deserialize
          end select


      enddo

      !TODO: in the long run the correct thing will be to serialize/deserialize
      !      the methodTable object! For now just put an empty table into proxy.
      ! create methodTable object
      call c_ESMC_MethodTableCreate(sp%methodTable, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                  
      ESMF_INIT_SET_CREATED(sp)

      !ESMF_StateDeserialize%statep => sp
      substate%statep => sp
      
      ! Add reference to this object into ESMF garbage collection table
      call c_ESMC_VMAddFObject(substate, ESMF_ID_STATE%objectID)
      
      ESMF_INIT_SET_CREATED(substate)
      
      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_StateDeserialize
!------------------------------------------------------------------------------

end module ESMF_StateMod
