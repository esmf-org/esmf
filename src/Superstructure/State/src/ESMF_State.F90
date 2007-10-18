! $Id: ESMF_State.F90,v 1.91.2.4 2007/10/18 02:43:59 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_XformMod
      use ESMF_StateTypesMod
      implicit none

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateCreate, ESMF_StateDestroy

      public ESMF_StateAddNameOnly
      public ESMF_StateAddBundle, ESMF_StateAddField, ESMF_StateAddArray
      public ESMF_StateAddState
      public ESMF_StateGetBundle, ESMF_StateGetField, ESMF_StateGetArray
      public ESMF_StateGetState, ESMF_StateGetItemInfo

      public ESMF_StateGet
      public ESMF_StateSetNeeded, ESMF_StateGetNeeded
      public ESMF_StateIsNeeded

      !public ESMF_StateGetNeededList     ! returns an array of values
      !public ESMF_State{Get/Set}Ready    ! is data ready
      !public ESMF_State{Get/Set}Valid    ! has data been validated?
      !public ESMF_State{Get/Set}CompName ! normally set at create time

      public ESMF_StateTransform          ! execute xform on a state
      !public ESMF_StateTransformComplete ! is export state ok to update?
      ! TODO: this needs to be renamed.
      !public ESMF_StateValidate          ! is import state ready to read?
 
      public ESMF_StateSetAttribute       ! Set and Get Attributes
      public ESMF_StateGetAttribute       !  

      public ESMF_StateGetAttributeCount  ! number of Attributes
      public ESMF_StateGetAttributeInfo   ! get type, length by name or number

      public ESMF_StateWriteRestart
      public ESMF_StateReadRestart

      public ESMF_StateWrite
      public ESMF_StatePrint, ESMF_StateValidate
      public ESMF_StateSerialize, ESMF_StateDeserialize

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_State.F90,v 1.91.2.4 2007/10/18 02:43:59 cdeluca Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddData -- Add Data to a State

! !INTERFACE:
!!     interface ESMF_StateAddData

! !PRIVATE MEMBER FUNCTIONS:
!
!!        module procedure ESMF_StateAddOneArray
!!        module procedure ESMF_StateAddArrayList
!!        module procedure ESMF_StateAddOneField
!!        module procedure ESMF_StateAddFieldList
!!        module procedure ESMF_StateAddOneBundle
!!        module procedure ESMF_StateAddBundleList
!!        module procedure ESMF_StateAddOneState
!!        module procedure ESMF_StateAddStateList
!!        module procedure ESMF_StateAddOneName
!!        module procedure ESMF_StateAddNameList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddData} functions.   
!  
!EOPI 
!!end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddArray -- Add Arrays to a State

! !INTERFACE:
     interface ESMF_StateAddArray

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddOneArray
        module procedure ESMF_StateAddArrayList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
! types of {\tt ESMF\_StateAddArray} functions.   
!  
!EOPI 
end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddField -- Add Fields to a State

! !INTERFACE:
     interface ESMF_StateAddField

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddOneField
        module procedure ESMF_StateAddFieldList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddField} functions.   
!  
!EOPI 
end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddBundle -- Add Bundles to a State

! !INTERFACE:
     interface ESMF_StateAddBundle

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddOneBundle
        module procedure ESMF_StateAddBundleList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddBundle} functions.   
!  
!EOPI 
end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddState -- Add States to a State

! !INTERFACE:
     interface ESMF_StateAddState

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddOneState
        module procedure ESMF_StateAddStateList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddState} functions.   
!  
!EOPI 
end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddNameOnly -- Add Names to a State

! !INTERFACE:
     interface ESMF_StateAddNameOnly

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddOneName
        module procedure ESMF_StateAddNameList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddNameOnly} functions.   
!  
!EOPI 
end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateGetData -- Retrieve Bundles, Fields, or Arrays from a State

! !INTERFACE:
!!     interface ESMF_StateGetData

! !PRIVATE MEMBER FUNCTIONS:
!
!!      module procedure ESMF_StateGetBundle
!!      module procedure ESMF_StateGetField
!!      module procedure ESMF_StateGetArray
!!      module procedure ESMF_StateGetState

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateGetData} functions.   
!  
!EOPI 
!!end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateSetAttribute - Set a State attribute
!
! !INTERFACE:
      interface ESMF_StateSetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateSetInt4Attr
        module procedure ESMF_StateSetInt4ListAttr
        module procedure ESMF_StateSetInt8Attr
        module procedure ESMF_StateSetInt8ListAttr
        module procedure ESMF_StateSetReal4Attr
        module procedure ESMF_StateSetReal4ListAttr
        module procedure ESMF_StateSetReal8Attr
        module procedure ESMF_StateSetReal8ListAttr
        module procedure ESMF_StateSetLogicalAttr
        module procedure ESMF_StateSetLogicalListAttr
        module procedure ESMF_StateSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_State}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateGetAttribute - Get a State attribute
!
! !INTERFACE:
      interface ESMF_StateGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateGetInt4Attr
        module procedure ESMF_StateGetInt4ListAttr
        module procedure ESMF_StateGetInt8Attr
        module procedure ESMF_StateGetInt8ListAttr
        module procedure ESMF_StateGetReal4Attr
        module procedure ESMF_StateGetReal4ListAttr
        module procedure ESMF_StateGetReal8Attr
        module procedure ESMF_StateGetReal8ListAttr
        module procedure ESMF_StateGetLogicalAttr
        module procedure ESMF_StateGetLogicalListAttr
        module procedure ESMF_StateGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_State}.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateGetAttributeInfo - Get type, count from a State attribute
!
! !INTERFACE:
      interface ESMF_StateGetAttributeInfo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateGetAttrInfoByName
        module procedure ESMF_StateGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_State}.
 
!EOPI
      end interface


!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneArray"
!BOP
! !IROUTINE: ESMF_StateAddArray - Add an Array to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddArray()   
      subroutine ESMF_StateAddOneArray(state, array, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Array), intent(in) :: array
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt array} reference to an existing 
!      {\tt state}.  The {\tt array} name must be unique 
!      within the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[array]
!      The {\tt ESMF\_Array} to be added.  This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Array} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_Array) :: temp_list(1)
      integer :: localrc

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = array

      call ESMF_StateClassAddArrayList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddArrayList"
!BOP
! !IROUTINE: ESMF_StateAddArray - Add a list of Arrays to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddArray()   
      subroutine ESMF_StateAddArrayList(state, arrayCount, arrayList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: arrayCount
      type(ESMF_Array), dimension(:), intent(in) :: arrayList
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!     Add multiple {\tt ESMF\_Array}s to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[arrayCount]
!      The number of {\tt ESMF\_Array}s to be added.
!     \item[arrayList]
!      The list (Fortran array) of {\tt ESMF\_Array}s to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Array}s cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClassAddArrayList(state%statep, arrayCount, arrayList, rc)

      end subroutine ESMF_StateAddArrayList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneBundle"
!BOP
! !IROUTINE: ESMF_StateAddBundle - Add a Bundle to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddBundle()   
      subroutine ESMF_StateAddOneBundle(state, bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Bundle), intent(in) :: bundle
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt bundle} reference to an existing 
!      {\tt state}.
!      The {\tt bundle} name must be unique within the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      The {\tt ESMF\_State} object.
!     \item[bundle]
!      The {\tt ESMF\_Bundle} to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Bundle} cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc
      type(ESMF_Bundle) :: temp_list(1)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = bundle

      call ESMF_StateClassAddBundleList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddBundleList"
!BOP
! !IROUTINE: ESMF_StateAddBundle - Add a list of Bundles to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddBundle()   
      subroutine ESMF_StateAddBundleList(state, bundleCount, bundleList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: bundleCount
      type(ESMF_Bundle), dimension(:), intent(in) :: bundleList
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple {\tt ESMF\_Bundle}s to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[bundleCount]
!      The number of {\tt ESMF\_Bundle}s to be added.
!     \item[bundleList]
!      The list (Fortran array) of {\tt ESMF\_Bundle}s to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Bundle}s cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClassAddBundleList(state%statep, bundleCount, &
                                          bundleList, rc)

      end subroutine ESMF_StateAddBundleList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneField"
!BOP
! !IROUTINE: ESMF_StateAddField - Add a Field to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddField()   
      subroutine ESMF_StateAddOneField(state, field, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(in) :: field
      integer, intent(out), optional :: rc
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
!EOP

      integer :: localrc
      type(ESMF_Field) :: temp_list(1)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = field

      call ESMF_StateClassAddFieldList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddFieldList"
!BOP
! !IROUTINE: ESMF_StateAddField - Add a list of Fields to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddField()   
      subroutine ESMF_StateAddFieldList(state, fieldCount, fieldList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: fieldCount
      type(ESMF_Field), dimension(:), intent(in) :: fieldList
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple {\tt ESMF\_Field}s to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[fieldCount]
!      The number of {\tt ESMF\_Field}s to be added.
!     \item[fieldList]
!      The list (Fortran array) of {\tt ESMF\_Field}s to be added.
!      This is a reference only; when
!      the {\tt ESMF\_State} is destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt ESMF\_Field}s cannot be safely 
!      destroyed before the {\tt ESMF\_State} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClassAddFieldList(state%statep, fieldCount, fieldList, rc)

      end subroutine ESMF_StateAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneName"
!BOP
! !IROUTINE: ESMF_StateAddNameOnly - Add a name to a State as a placeholder
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddNameOnly()   
      subroutine ESMF_StateAddOneName(state, name, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      character (len=*), intent(in) :: name
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add the character string {\tt name} to an existing {\tt state}.
!      It can subsequently be replaced by an actual object with the
!      same name.
!      The {\tt name} must be unique within the {\tt state}.
!      It is available to be marked needed by the
!      consumer of the export {\tt ESMF\_State}. Then the data 
!      provider can replace the name with the actual {\tt ESMF\_Bundle},
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
!EOP

      integer :: localrc
      character(len=ESMF_MAXSTR) :: temp_list(1)
      
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      temp_list(1) = name

      call ESMF_StateAddNameList(state, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddNameList"
!BOP
! !IROUTINE: ESMF_StateAddNameOnly - Add a list of names to a State 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddNameOnly()   
      subroutine ESMF_StateAddNameList(state, nameCount, nameList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      integer, intent(in) :: nameCount
      character (len=*), intent(in) :: nameList(:)
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
!      provider can replace the name with the actual {\tt ESMF\_Bundle},
!      {\tt ESMF\_Field}, or {\tt ESMF\_Array} which carries the needed data.
!      Unneeded data need not be generated.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      An {\tt ESMF\_State} object.
!     \item[nameCount]
!      The count of names in the {\tt nameList}.
!     \item[nameList]
!      A list (Fortran array) of character strings to be added
!      as placeholders for data objects.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
      integer :: localrc

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      call ESMF_StateClassAddDataNameList(state%statep, namecount, namelist, rc)      
      end subroutine ESMF_StateAddNameList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneState"
!BOP
! !IROUTINE: ESMF_StateAddState - Add a State to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddState()   
      subroutine ESMF_StateAddOneState(state, nestedState, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_State), intent(in) :: nestedState
      integer, intent(out), optional :: rc
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
!EOP

      integer :: localrc
      type(ESMF_State) :: temp_list(1)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      temp_list(1) = nestedState

      call ESMF_StateClassAddStateList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddStateList"
!BOP
! !IROUTINE: ESMF_StateAddState - Add a list of States to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddState()   
      subroutine ESMF_StateAddStateList(state, nestedStateCount, nestedStateList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: nestedStateCount
      type(ESMF_State), dimension(:), intent(in) :: nestedStateList
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
!     \item[nestedStateCount]
!      The number of {\tt ESMF\_State}s to be added.
!     \item[nestedStateList]
!      The list (Fortran array) of {\tt ESMF\_State}s to be added.
!      This is a reference only; when the container {\tt state} is 
!      destroyed the objects contained in it will
!      not be destroyed.   Also, the {\tt nestedStateList} cannot be safely 
!      destroyed before the container {\tt state} is destroyed.
!      Since objects can be added to multiple containers, it remains
!      the user's responsibility to manage the
!      destruction of objects when they are no longer in use.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_StateClassAddStateList(state%statep, nestedStateCount, &
                                                         nestedStateList, rc)

      end subroutine ESMF_StateAddStateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateCreate"
!BOP
! !IROUTINE: ESMF_StateCreate - Create a new State

! !INTERFACE:
      function ESMF_StateCreate(stateName, statetype, &
                   bundleList, fieldList, arrayList, nestedStateList, &
                   nameList, itemCount, &
                   neededflag, readyflag, validflag, reqforrestartflag, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreate
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: stateName 
      type(ESMF_StateType), intent(in), optional :: statetype
      type(ESMF_Bundle), dimension(:), intent(in), optional :: bundleList
      type(ESMF_Field), dimension(:), intent(in), optional :: fieldList
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
!   \item[{[bundleList]}]
!    A list (Fortran array) of {\tt ESMF\_Bundle}s.
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
!    The total number of things -- Bundles, Fields, 
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
        integer :: localrc                          ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Initialize the pointers to null.
        nullify(ESMF_StateCreate%statep)
        nullify(stypep)

        allocate(stypep, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "State type", &
                                       ESMF_CONTEXT, rc)) return
        
      !TODO: COLUMBIA_BUG: The following "if (present())" construct is a
      !      work-around for Intel's ifort version 9.1.045 and 9.1.051
      !      on NAS' columbia.
        if (present(nameList)) then 
          call ESMF_StateConstruct(stypep, stateName, statetype, &
                   bundleList, fieldList, arrayList, nestedStateList, &
                   nameList, itemCount, &
                   neededflag, readyflag, validflag, reqforrestartflag, localrc)
        else
          call ESMF_StateConstruct(stypep, stateName, statetype, &
                   bundleList, fieldList, arrayList, nestedStateList, &
                   itemcount=itemCount, &
		   neededflag=neededflag, readyflag=readyflag, &
		   validflag=validflag, reqforrestartflag=reqforrestartflag, &
                   rc=localrc)
        endif
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then 
            ! do not overwrite the rc from the real error
            deallocate(stypep, stat=localrc)
            return
        endif

        ! Set return values
        ESMF_StateCreate%statep => stypep 
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_StateCreate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateDestroy"
!BOP
! !IROUTINE: ESMF_StateDestroy - Release resources for a State
!
! !INTERFACE:
      subroutine ESMF_StateDestroy(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_State}.
!     {\tt ESMF\_State}s contain references only to other objects;
!     when the {\tt ESMF\_State} is destroyed objects contained in it will
!     not be destroyed.  Objects inside a {\tt ESMF\_State} cannot be
!     destroyed before the container {\tt ESMF\_State} is destroyed.
!     Since objects can be added to multiple containers, it remains
!     the user's responsibility to manage the
!     destruction of objects when they are no longer in use.
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
        if (present(rc)) rc = ESMF_FAILURE

        ! Simple sanity checks
        call ESMF_StateValidate(state, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


        ! Call Destruct to release resources
        call ESMF_StateDestruct(state%statep, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Release space
 	deallocate(state%statep, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate State", &
                                       ESMF_CONTEXT, rc)) return
        nullify(state%statep)

        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGet"
!BOP
! !IROUTINE: ESMF_StateGet - Get information about a State
!
! !INTERFACE:
      subroutine ESMF_StateGet(state, name, statetype, itemCount, &
                               itemNameList, stateitemtypeList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(out), optional :: name
      type(ESMF_StateType), intent(out), optional :: statetype
      integer, intent(out), optional :: itemCount
      character (len=*), intent(out), optional :: itemNameList(:)
      type(ESMF_StateItemType), intent(out), optional :: stateitemtypeList(:)
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the requested information about this {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       An {\tt ESMF\_State} object to be queried.
!      \item[{[name]}]
!       Name of this {\tt ESMF\_State}.
!      \item[{[statetype]}]
!       Import or Export {\tt ESMF\_State}.  Possible values are 
!       listed in Section~\ref{opt:statetype}.
!      \item[{[itemCount]}]
!        Count of items in {\tt state}, including all objects
!        as well as placeholder names.
!      \item[{[itemNameList]}]
!        Array of item names in {\tt state}, 
!        including placeholder names.  {\tt itemNameList} must be at least
!        {\tt itemCount} long.
!      \item[{[stateitemtypeList]}]
!        Array of possible item object types in {\tt state}, including 
!        placeholder 
!        names. Must be at least {\tt itemCount} long.  Options are
!        listed in Section~\ref{opt:stateitemtype}.
!      \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
      integer :: i, localrc
      type(ESMF_StateClass), pointer :: stypep
      type(ESMF_StateItem), pointer :: nextitem

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      stypep => state%statep

      if (present(name)) call c_ESMC_GetName(stypep%base, name, localrc)
      if (present(statetype)) statetype = stypep%st

      ! TODO: indirect entries for Fields inside of Bundles complicates
      !  this code.  the count needs to be both primary objects and
      !  total objects.  perhaps the state derived type needs to bookkeep
      !  both numbers.  For now, return entire raw count.

      if (present(itemCount)) itemCount = stypep%datacount 

      if (present(itemnameList)) then
          do i=1, stypep%datacount
              nextitem => stypep%datalist(i)
              itemnameList(i) = nextitem%namep
          enddo
      endif

      if (present(stateitemtypeList)) then
          do i=1, stypep%datacount
              nextitem => stypep%datalist(i)
              stateitemtypeList(i) = nextitem%otype
          enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetArray"
!BOP
! !IROUTINE: ESMF_StateGetArray - Retrieve a data Array from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetArray(state, arrayName, array, nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: arrayName
      type(ESMF_Array), intent(out) :: array
      character (len=*), intent(in), optional :: nestedStateName
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Array} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the object directly, only
!      {\tt arrayName} is required.
!      If the {\tt state} contains multiple nested {\tt ESMF\_State}s
!      and the object is one level down, this routine can return the object
!      in a single call by specifing the proper {\tt nestedStateName}.
!      {\tt ESMF\_State}s can be nested to any depth, but this routine 
!      only searches in immediate descendents.  
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for an {\tt ESMF\_Array} named {\tt arrayName}.
!  \item[arrayName]
!    Name of {\tt ESMF\_Array} to be returned.
!  \item[array]
!    Returned reference to the {\tt ESMF\_Array}.
!  \item[{[nestedStateName]}]
!    Optional.  An error if specified when the {\tt state} argument contains
!    no nested {\tt ESMF\_State}s.  Required if the {\tt state} contains 
!    multiple nested {\tt ESMF\_State}s and the object being requested is
!    in one level down in one of the nested {\tt ESMF\_State}.
!    {\tt ESMF\_State} must be selected by this {\tt nestedStateName}.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      logical :: exists
      integer :: localrc
      character(len=ESMF_MAXSTR) :: errmsg

      localrc = ESMF_FAILURE
       
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE
      ! TODO: do we need an empty (or invalid) array to mark failure?

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep, nestedStateName, .true., &
                                                          dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg,*) trim(nestedStateName), " found but not type State"
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep, arrayName, .true., &
                                                          dataitem, rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no Array found named ", trim(arrayName)
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_ARRAY) then
          write(errmsg, *) trim(arrayName), " found but not type Array"
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      array = dataitem%datap%ap

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetInt4Attr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a 4-byte integer attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetInt4Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 4-byte integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetInt4ListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetInt4ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 4-byte integer values of the named attribute.
!      The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetInt8Attr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve an 8-byte integer attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetInt8Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 8-byte integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetInt8ListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetInt8ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 8-byte integer values of the named attribute.
!      The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetReal4Attr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetReal4Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 4-byte real value of the named attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetReal4ListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetReal4ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a list of 4-byte real attributes from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 4-byte real values of the named attribute.  
!      The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetReal8Attr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetReal8Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 8-byte real value of the named attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetReal8ListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetReal8ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a list of 8-byte real attributes from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 8-byte real values of the named attribute.  
!      The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetLogicalAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetLogicalAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The logical value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetLogicalListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetLogicalListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The logical values of the named attribute.
!      The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetCharAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldGetAttribute()
      subroutine ESMF_StateGetCharAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The character value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetChar(state%statep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetAttributeCount"
!BOP
! !IROUTINE: ESMF_StateGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_StateGetAttributeCount(state, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of attributes associated with the given
!      {\tt state} in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [count]
!      The number of attributes associated with this object.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetCount(state%statep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetAttrInfoByName"
!BOP
! !IROUTINE: ESMF_StateGetAttributeInfo - Query State attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttributeInfo()
      subroutine ESMF_StateGetAttrInfoByName(state, name, datatype, &
                                             datakind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character(len=*), intent(in) :: name
      type(ESMF_DataType), intent(out), optional :: datatype
      type(ESMF_DataKind), intent(out), optional :: datakind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the named attribute,
!      including {\tt datatype}, {\tt datakind} (if applicable),
!      and item {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[datatype]}]
!           The data type of the attribute. One of the values
!           {\tt ESMF\_DATA\_INTEGER}, {\tt ESMF\_DATA\_REAL},
!           {\tt ESMF\_DATA\_LOGICAL}, or {\tt ESMF\_DATA\_CHARACTER}.
!     \item [{[datakind]}]
!           The datakind of the attribute, if attribute is type
!           {\tt ESMF\_DATA\_INTEGER} or {\tt ESMF\_DATA\_REAL}.
!           One of the values {\tt ESMF\_I4}, {\tt ESMF\_I8}, {\tt ESMF\_R4},
!           or {\tt ESMF\_R8}.
!           For all other types the value {\tt ESMF\_NOKIND} is returned.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      type(ESMF_DataType) :: localDt
      type(ESMF_DataKind) :: localDk
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetAttrInfoName(state%statep%base, name, &
                                           localDt, localDk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(datatype)) datatype = localDt
      if (present(datakind)) datakind = localDk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetAttrInfoByNum"
!BOP
! !IROUTINE: ESMF_StateGetAttributeInfo - Query State attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttributeInfo()
      subroutine ESMF_StateGetAttrInfoByNum(state, attributeIndex, name, &
                                            datatype, datakind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_DataType), intent(out), optional :: datatype
      type(ESMF_DataKind), intent(out), optional :: datakind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute,
!      including {\tt datatype}, {\tt datakind} (if applicable),
!      and item {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[datatype]}]
!           The data type of the attribute. One of the values
!           {\tt ESMF\_DATA\_INTEGER}, {\tt ESMF\_DATA\_REAL},
!           {\tt ESMF\_DATA\_LOGICAL}, or {\tt ESMF\_DATA\_CHARACTER}.
!     \item [{[datakind]}]
!           The datakind of the attribute, if attribute is type
!           {\tt ESMF\_DATA\_INTEGER} or {\tt ESMF\_DATA\_REAL}.
!           One of the values {\tt ESMF\_I4}, {\tt ESMF\_I8}, {\tt ESMF\_R4},
!           or {\tt ESMF\_R8}.
!           For all other types the value {\tt ESMF\_NOKIND} is returned.
!     \item [{[count]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_DataType) :: localDt
      type(ESMF_DataKind) :: localDk
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetAttrInfoNum(state%statep%base, attributeIndex, &
                                          localName, localDt, localDk, &
                                          localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(datatype)) datatype = localDt
      if (present(datakind)) datakind = localDk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetBundle"
!BOP
! !IROUTINE: ESMF_StateGetBundle - Retrieve a Bundle from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetBundle(state, bundleName, bundle, &
                                     nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: bundleName
      type(ESMF_Bundle), intent(out) :: bundle
      character (len=*), intent(in), optional :: nestedStateName
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Bundle} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the object directly, only
!      {\tt bundleName} is required.
!      If the {\tt state} contains multiple nested {\tt ESMF\_State}s
!      and the object is one level down, this routine can return the object
!      in a single call by specifing the proper {\tt nestedStateName}.
!      {\tt ESMF\_State}s can be nested to any depth, but this routine 
!      only searches in immediate descendents.  
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for a {\tt ESMF\_Bundle} named {\tt bundlename}.
!  \item[bundleName]
!    Name of {\tt ESMF\_Bundle} to be returned.
!  \item[bundle]
!    Returned reference to the {\tt ESMF\_Bundle}.
!  \item[{[nestedStateName]}]
!    Optional.  An error if specified when the {\tt state} argument contains
!    no nested {\tt ESMF\_State}s.  Required if the {\tt state} contains 
!    multiple nested {\tt ESMF\_State}s and the object being requested is
!    in one level down in one of the nested {\tt ESMF\_State}.
!    {\tt ESMF\_State} must be selected by this {\tt nestedStateName}.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP

      integer :: localrc
      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE
      ! TODO: do we need an empty bundle to mark failure?

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep, nestedStateName, .true., &
                                                          dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg, *) trim(nestedStateName), " found but not type State"
             if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep, bundleName, .true., &
                                                          dataitem, rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no Bundle found named ", trim(bundleName)
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_BUNDLE) then
          write(errmsg, *) trim(bundleName), " found but not type Bundle"
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      bundle = dataitem%datap%bp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetField"
!BOP
! !IROUTINE: ESMF_StateGetField - Retrieve a Field from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetField(state, fieldName, field, &
                                    nestedStateName, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: fieldName
      type(ESMF_Field), intent(out) :: field
      character (len=*), intent(in), optional :: nestedStateName
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Field} from an {\tt ESMF\_State} by name.  
!      If the {\tt ESMF\_State} contains the object directly, only
!      {\tt fieldname} is required.
!      If the {\tt state} contains multiple nested {\tt ESMF\_State}s
!      and the object is one level down, this routine can return the object
!      in a single call by specifing the proper {\tt nestedStateName}.
!      {\tt ESMF\_State}s can be nested to any depth, but this routine 
!      only searches in immediate descendents.  
!      It is an error to specify a {\tt nestedStateName} if the
!      {\tt state} contains no nested {\tt ESMF\_State}s.
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for an {\tt ESMF\_Field} named {\tt fieldName}.
!  \item[fieldName]
!    Name of {\tt ESMF\_Field} to be returned.
!  \item[field]
!    Returned reference to the {\tt ESMF\_Field}.
!  \item[{[nestedStateName]}]
!    Optional.  An error if specified when the {\tt state} argument contains
!    no nested {\tt ESMF\_State}s.  Required if the {\tt state} contains 
!    multiple nested {\tt ESMF\_State}s and the object being requested is
!    in one level down in one of the nested {\tt ESMF\_State}.
!    {\tt ESMF\_State} must be selected by this {\tt nestedStateName}.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      type(ESMF_State) :: top
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE
      ! TODO: do we need an empty field to mark failure?

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(nestedStateName)) then
          exists = ESMF_StateClassFindData(state%statep, nestedStateName, .true., &
                                                          dataitem, rc=localrc)
          if (.not. exists) then
              write(errmsg, *) "no nested state found named ", trim(nestedStateName)
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                         ESMF_CONTEXT, rc)) return
          endif
    
          if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
              write(errmsg, *) trim(nestedStateName), " found but not type State"
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)) return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateClassFindData(top%statep, fieldName, .true., &
                                                          dataitem, rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no Field found named ", trim(fieldName)
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_FIELD) then
          if (dataitem%otype .eq. ESMF_STATEITEM_INDIRECT) then
              ! TODO: how do we return the info that this is inside a bundle?
              if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                       "extracting Fields directly from Bundles in a State", &
                       ESMF_CONTEXT, rc)) return
          endif
          write(errmsg, *) trim(fieldname), " found but not type Field"
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      field = dataitem%datap%fp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetItemInfo"
!BOP
! !IROUTINE: ESMF_StateGetItemInfo - Get information about a State
!
! !INTERFACE:
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

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
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
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
                                      dataitem, rc=localrc)
      if (.not. exists) then
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, trim(itemName), &
                                     ESMF_CONTEXT, rc)) return
      endif

      neededflag = dataitem%needed

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetNeeded

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetState"
!BOP
! !IROUTINE: ESMF_StateGetState - Retrieve a State nested in a State
!
! !INTERFACE:
      subroutine ESMF_StateGetState(state, nestedStateName, nestedState, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: nestedStateName
      type(ESMF_State), intent(out) :: nestedState
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a nested {\tt ESMF\_State} from another {\tt ESMF\_State} 
!      by name.  This does not allow the caller to
!      retrieve an {\tt ESMF\_State} from two levels down.  It returns
!      immediate child objects only.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       The {\tt ESMF\_State} to query for a nested {\tt ESMF\_State} 
!       named {\tt stateName}.
!     \item[nestedStateName]
!       Name of nested {\tt ESMF\_State} to return.
!     \item[nestedState]
!       Returned {\tt ESMF\_State}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE
      ! TODO: do we need an empty state to mark failure?

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      exists = ESMF_StateClassFindData(state%statep, nestedStateName, .true., &
                                                         dataitem, rc=localrc)
      if (.not. exists) then
          write (errmsg,*) "no nested state found named ", trim(nestedStateName)
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_STATE) then
          write (errmsg, *) trim(nestedStateName), " found but not type State"
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)) return
      endif

      nestedState%statep => dataitem%datap%spp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetState

!------------------------------------------------------------------------------
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
      if (present(rc)) rc = ESMF_FAILURE
      ESMF_StateIsNeeded = .FALSE.

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: decide on the behavior:
      ! should it be an error to ask about a state which doesn't exist?
      ! if the 3rd arg below is .true. then it's an error, if it's .false.
      ! then it's not.  for now, it's an error.
      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
                                      dataitem, rc=localrc)
      if (.not. exists) then
          if (ESMF_LogMsgFoundError(localrc, &
                                      "Item by that name not found", &
                                      ESMF_CONTEXT, rc)) return
      endif

      if (dataitem%needed .eq. ESMF_NEEDED) ESMF_StateIsNeeded = .TRUE.
  
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_StateIsNeeded

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StatePrint"
!BOP
! !IROUTINE: ESMF_StatePrint - Print the internal data for a State
!
! !INTERFACE:
      subroutine ESMF_StatePrint(state, options, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Prints information about the {\tt state} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to print.
!     \item[{[options]}]
!       Print options are not yet supported.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!

!EOP

!
! TODO: this needs more code added to be complete
!
       character (len=6) :: defaultopts
       type(ESMF_StateClass), pointer :: sp
       type(ESMF_StateItem), pointer :: dp
       character(len=ESMF_MAXSTR) :: name
       character (len=1024) :: outbuf
       integer :: localrc                          ! local error status
       integer :: i
       character(len=ESMF_MAXSTR) :: msgbuf

       defaultopts = "brief"
       ! Initialize return code; assume failure until success is certain
       if (present(rc)) rc = ESMF_FAILURE

       ! TODO: Add code here
       ! print num of states, state type, etc

       !nsc write(msgbuf,*) "StatePrint: "  
       !nsc call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
       print *, "StatePrint: "  
       if (.not.associated(state%statep)) then 
           !nsc call ESMF_LogWrite("Uninitialized or already destroyed State", &
           !nsc                   ESMF_LOG_INFO)
           print *, "Uninitialized or already destroyed State"
           rc = ESMF_SUCCESS
           return
       endif
       if (state%statep%st .eq. ESMF_STATE_INVALID) then
           !nsc call ESMF_LogWrite("Uninitialized or already destroyed State", &
           !nsc                   ESMF_LOG_INFO)
           print *, "Uninitialized or already destroyed State"
           rc = ESMF_SUCCESS
           return
       endif

       sp => state%statep

       call c_ESMC_GetName(sp%base, name, localrc)
       !call ESMF_BasePrint(sp%base, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
       print *, "  State name = ", trim(name)
       if (sp%st .eq. ESMF_STATE_IMPORT) write(msgbuf, *) " Import State"
       if (sp%st .eq. ESMF_STATE_EXPORT) write(msgbuf, *) " Export State"
       if (sp%st .eq. ESMF_STATE_UNSPECIFIED) write(msgbuf, *) " State Type Unspecified"
       if (sp%st .eq. ESMF_STATE_INVALID) then
           call ESMF_LogWrite("Uninitialized or already destroyed State", &
                                ESMF_LOG_INFO)
           rc = ESMF_SUCCESS
           return
       endif
       !nsc call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
       print *, trim(msgbuf)
       !nsc write(msgbuf, *) "  Number of members: ", sp%datacount
       !nsc call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
       print *, "  Number of members: ", sp%datacount
      
       do i=1, sp%datacount
         dp => sp%datalist(i)

         !nsc write(msgbuf, *) "  Item ", i, ":"
         !nsc call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
         print *, "  Item ", i, ":"
         outbuf = "    Name= " // trim(dp%namep) // ", "

         select case (dp%otype%ot)
           case (ESMF_STATEITEM_BUNDLE%ot)
             outbuf = trim(outbuf) //  " type Bundle,"
           case (ESMF_STATEITEM_FIELD%ot)
             outbuf = trim(outbuf) //  " type Field,"
           case (ESMF_STATEITEM_ARRAY%ot)
             outbuf = trim(outbuf) //  " type Array,"
           case (ESMF_STATEITEM_STATE%ot)
             outbuf = trim(outbuf) //  " type State,"
           case (ESMF_STATEITEM_NAME%ot)
             outbuf = trim(outbuf) //  " placeholder name,"
           case (ESMF_STATEITEM_INDIRECT%ot)
             outbuf = trim(outbuf) //  " field inside a bundle,"
           case (ESMF_STATEITEM_UNKNOWN%ot)
             outbuf = trim(outbuf) //  " unknown type,"
         end select

         select case (dp%needed%needed)
           case (ESMF_NEEDED%needed)
             outbuf = trim(outbuf) //  " marked as needed."
           case (ESMF_NOTNEEDED%needed)
             outbuf = trim(outbuf) //  " marked as NOT needed."
         end select

        !nsc call ESMF_LogWrite(outbuf, ESMF_LOG_INFO)
        print *, trim(outbuf)

        ! TODO: finish printing more info here
        !type(ESMF_ReadyFlag) :: ready
        !type(ESMF_ValidFlag) :: valid

        !type(ESMF_DataHolder), pointer :: datap

        !write(msgbuf, *) trim(outbuf)

       enddo


       ! Set return values
       if (present(rc)) rc = ESMF_SUCCESS

       end subroutine ESMF_StatePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReadRestart"
!BOPI
! !IROUTINE: ESMF_StateReadRestart -- ReadRestart the internal data from a State
!
! !INTERFACE:
      function ESMF_StateReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              
      type(ESMF_IOSpec), intent(in), optional :: iospec   
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
!     \item[iospec]
!       An {\tt ESMF\_IOSpec} which specifies I/O information.
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
        if (present(rc)) rc = ESMF_FAILURE
 
        end function ESMF_StateReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetInt4Attr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetInt4Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 4-byte integer value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetInt4ListAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetInt4ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of integers in the {\tt valueList}.
!     \item [valueList]
!       The 4-byte integer values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
  
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetInt8Attr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetInt8Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 8-byte integer value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetInt8ListAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetInt8ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of integers in the {\tt valueList}.
!     \item [valueList]
!       The 8-byte integer values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
  
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetReal4Attr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetReal4Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
!      
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 4-byte real value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetReal4ListAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetReal4ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of reals in the {\tt valueList}.
!     \item [value]
!       The 4-byte real values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetReal8Attr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetReal8Attr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
!      
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 8-byte real value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetReal8ListAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetReal8ListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of reals in the {\tt valueList}.
!     \item [value]
!       The 8-byte real values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetLogicalAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a logical attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetLogicalAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to set.
!     \item [value]
!       The logical true/false value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetLogicalListAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetLogicalListAttr(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of logicals in the {\tt valueList}.
!     \item [valueList]
!       The logical true/false values of the attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetCharAttr"
!BOP
! !IROUTINE: ESMF_StateSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateSetAttribute()
      subroutine ESMF_StateSetCharAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a character attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to set.
!     \item [value]
!      The character value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetChar(state%statep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetNeeded"
!BOP
! !IROUTINE: ESMF_StateSetNeeded - Set if a data item is needed
!
! !INTERFACE:
      subroutine ESMF_StateSetNeeded(state, itemName, neededflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      character (len=*), intent(in) :: itemName
      type(ESMF_NeededFlag), intent(in) :: neededflag
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets the status of the {\tt needed} flag for the data item
!      named by {\tt itemName} in the {\tt ESMF\_State}.
!
!     The arguments are:
!      \begin{description}     
!      \item[state]
!        The {\tt ESMF\_State} to set.
!       \item[itemName]
!        Name of the data item to set.
!       \item[neededflag]
!        Set status of data item to this.  See Section~\ref{opt:neededflag}
!        for possible values.
!       \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
                                      dataitem, rc=localrc)
      if (.not. exists) then
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, itemName, &
                                      ESMF_CONTEXT, rc)) return
      endif

      dataitem%needed = neededflag

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetNeeded

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateValidate"
!BOP
! !IROUTINE: ESMF_StateValidate - Check validity of a State
!
! !INTERFACE:
      subroutine ESMF_StateValidate(state, options, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Validates that the {\tt state} is internally consistent.
!      Currently this method determines if the {\tt state} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to validate.
!     \item[{[options]}]
!       Validation options are not yet supported.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!
! TODO: code goes here
!
      character (len=6) :: defaultopts

      defaultopts = "brief"
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      if (.not.associated(state%statep)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (state%statep%st .eq. ESMF_STATE_INVALID) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTransform"
!BOPI
! !IROUTINE: ESMF_StateTransform - Apply a Transform to State Data
!
! !INTERFACE:
      subroutine ESMF_StateTransform(state, xformname, xform, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      character(len=*), intent(in) :: xformname
      type(ESMF_Xform), dimension(:), intent(in) :: xform
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!  Apply an {\tt ESMF\_Transform} to an {\tt ESMF\_State}.  
!  This routine is intended to be called
!  from within an {\tt ESMF\_Component} when it is not practical 
!  to return to the
!  calling layer in order to do the coupling directly.  
!  This call 
!  passes through the framework back into user-written coupling code
!  to allow exchange of data between {\tt ESMF\_Component}s.  
!  It returns back to
!  the calling location and allows the {\tt ESMF\_Component} to continue 
!  execution from that place.  
!  {\tt ESMF\_Component}s which run in sequential mode have no need to use
!  this routine; Coupling code is called after an {\tt ESMF\_Component}
!  returns to the calling code.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} to apply {\tt ESMF\_Xform} to.
!     \item[xformname]
!       {\tt ESMF\_Xform} name to be called.
!     \item[xform]
!       {\tt ESMF\_Xform} object to be applied..
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

!
! TODO: code goes here
!

        ! This is a wrapper which turns around and calls into the
        ! transform code to execute the callback.

        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_StateTransform

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWrite"
!BOPI
! !IROUTINE: ESMF_StateWrite -- Write all or part of a State
!
! !INTERFACE:
      subroutine ESMF_StateWrite(state, iospec, itemname, rc)
!
! !ARGUMENTS:
      type(ESMF_State):: state 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      character (len=*), intent(in), optional :: itemname
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to write out all or part of a State object.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to write.
!     \item[{[iospec]}]
!       An {\tt ESMF\_IOSpec} object which specifies I/O information.
!     \item[{[itemname]}]
!       Item to be written.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        ! TODO: hardcoded for interoperability test
        type(ESMF_Field) :: fred
        integer :: localrc

        localrc = ESMF_FAILURE

        if (present(itemname)) then
            call ESMF_StateGetField(state, fieldname=itemname, field=fred, rc=localrc)
            call ESMF_FieldWrite(fred, iospec=iospec, rc=localrc) 
        endif

        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
  

        end subroutine ESMF_StateWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWriteRestart"
!BOPI
! !IROUTINE: ESMF_StateWriteRestart -- Save the internal data for a State
!
! !INTERFACE:
      subroutine ESMF_StateWriteRestart(state, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_State):: state 
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
!     \item[state]
!       {\tt ESMF\_State} to save contents of.
!     \item[{[iospec]}]
!       An {\tt ESMF\_IOSpec} object which contains I/O information and options.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

!
! TODO: code goes here
!
! The flags BOP/EOP have been changed to BOPI/EOPI because
! the subroutine has not been implemented. When the code is
! completed change back to BOP/EOP.
!
        if (present(rc)) rc = ESMF_FAILURE
        end subroutine ESMF_StateWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateConstruct"
!BOPI
! !IROUTINE: ESMF_StateConstruct -- Construct a new State

! !INTERFACE:
      subroutine ESMF_StateConstruct(stypep, statename, statetype, & 
                         bundles, fields, arrays, states, names, itemcount, &
                         neededflag, readyflag, validflag, reqforrestartflag, rc)
!
! !ARGUMENTS:
      type (ESMF_StateClass), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateType), intent(in), optional :: statetype
      type(ESMF_Bundle), dimension(:), intent(in), optional :: bundles
      type(ESMF_Field), dimension(:), intent(in), optional :: fields
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
!   \item[{[bundles]}]
!    An array of {\tt Bundles}.
!   \item[{[fields]}]
!    An array of {\tt Fields}.
!   \item[{[arrays]}]
!    An array of {\tt Arrays}.
!   \item[{[states]}]
!    An array of nested {\tt ESMF\_State}s.
!   \item[{[names]}]
!    An array of name placeholders.
!   \item[{[itemcount]}]
!    The total number of Bundles, Fields, Arrays, States, and Names specified.
!    This argument is optional, and if specified is used as an error check
!    to verify that the actual total number of items found matches this count.
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
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

        ! Local vars
        integer :: count
        integer :: localrc                   ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE
        localrc = ESMF_FAILURE 

        ! Quick sanity check on the values

        count = 0
        if (present(bundles)) count = count + size(bundles)
        if (present(fields)) count = count + size(fields)
        if (present(arrays)) count = count + size(arrays)
        if (present(states)) count = count + size(states)
        if (present(names)) count = count + size(names)

        if (present(itemcount)) then
          if (count .ne. itemcount) then
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "itemcount does not match lists given", &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        ! Set initial values
        call ESMF_StateConstructEmpty(stypep, statename, statetype, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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

        ! Set the initial size of the datalist
        call ESMF_StateClassExtendList(stypep, count, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      
        ! For each item type, set the data values.  All the allocation 
        !  has already been done.
        if (present(bundles)) then
          count = size(bundles)
          if (count .gt. 0) then
            call ESMF_StateClassAddBundleList(stypep, count, bundles, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(fields)) then
          count = size(fields)
          if (count .gt. 0) then
            call ESMF_StateClassAddFieldList(stypep, count, fields, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(arrays)) then
          count = size(arrays)
          if (count .gt. 0) then
            call ESMF_StateClassAddArrayList(stypep, count, arrays, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(states)) then
          count = size(states)
          if (count .gt. 0) then
            call ESMF_StateClassAddStateList(stypep, count, states, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          endif
        endif

        if (present(names)) then
          count = size(names)
          if (count .gt. 0) then
            call ESMF_StateClassAddDataNameList(stypep, count, names, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
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
        integer :: status                   ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Initialize the base object, set the name, etc.
        call ESMF_BaseCreate(stypep%base, "State", statename, 0, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Fill in basic information
        if (present(statetype)) then
          stypep%st = statetype
        else
          stypep%st = ESMF_STATE_UNSPECIFIED
        endif
        stypep%statestatus = ESMF_STATUS_READY
        stypep%alloccount = 0
        stypep%datacount = 0
        nullify(stypep%datalist)

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
      subroutine ESMF_StateDestruct(stypep, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt State}.
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

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! mark object invalid, and free each of the blocks associated
        ! with each entry.  note that we are not freeing the objects
        ! themselves; they could be added to multiple states.  it is
        ! the user's responsibility to delete them when finished.
        stypep%st = ESMF_STATE_INVALID
        stypep%statestatus = ESMF_STATUS_INVALID
        stypep%datacount = 0

        ! Now release the entire list
        if (associated(stypep%datalist)) then
          deallocate(stypep%datalist, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "data list", &
                                         ESMF_CONTEXT, rc)) return
          nullify(stypep%datalist)
        endif
        stypep%alloccount = 0

        ! Release the base object
        call ESMF_BaseDestroy(stypep%base, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassAddArrayList"
!BOPI
! !IROUTINE: ESMF_StateClassAddArrayList - Add a list of Arrays to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClassAddArrayList(stypep, acount, arrays, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: acount
      type(ESMF_Array), dimension(:), intent(in) :: arrays
      integer, intent(out), optional :: rc     
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
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                  ! local error status
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: aname
      character(len=ESMF_MAXSTR) :: errmsg
      integer, allocatable, dimension(:) :: atodo
      integer :: i
      integer :: newcount, aindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
      aname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (acount .le. 0) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "acount must be >= 0", &
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
        call ESMF_LogMsgSetError(ESMF_RC_INTNRL_INCONS, &
                                         "atodo already allocated", &
                                         ESMF_CONTEXT, rc)
        deallocate(atodo, stat=localrc)
        return
      endif

      allocate(atodo(acount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                     "adding Arrays to a State", &
                                     ESMF_CONTEXT, rc)) return

      atodo(1:acount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the array list.
      ! For each array...
      do i=1, acount

        call ESMF_ArrayValidate(arrays(i), rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) then
            write(errmsg, *) "item", i
            call ESMF_LogMsgSetError(localrc, errmsg, &
                                        ESMF_CONTEXT, rc)
            deallocate(atodo, stat=localrc)
            return
        endif
        call ESMF_ArrayGet(arrays(i), name=aname, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=localrc)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, aname, .false., &
                                        dataitem, aindex, localrc)
        if (ESMF_LogMsgFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) then
          deallocate(atodo, stat=localrc)
          return
        endif
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            aindex = -1
            atodo(i) = 1
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
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new arrays to the list.
      ! This is the start of the second pass through the array list.
      do i=1, acount

        ! If array wasn't already found in the list, we need to add it here.
        if (atodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_ARRAY

            ! Add name
            call ESMF_ArrayGet(arrays(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, "getting name from array", &
                                      ESMF_CONTEXT, rc)) return

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
      deallocate(atodo, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating internal list, 1c", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClassAddArrayList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassAddFieldList"
!BOPI
! !IROUTINE: ESMF_StateClassAddFieldList - Add a list of Fields to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClassAddFieldList(stypep, fcount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: fcount
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc     
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
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                   ! local error status
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: fname
      integer, allocatable, dimension(:) :: ftodo
      integer :: i
      integer :: newcount, findex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE 
      fname = ""

      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (fcount .le. 0) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "fcount must be >= 0", &
                                      ESMF_CONTEXT, rc)) return
      endif
      
      ! make sure sizes are consistent
      newcount = size(fields)
      if (fcount .gt. newcount) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "count must not be >= list length", &
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
        deallocate(ftodo, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "deallocating fields from a state", &
                                  ESMF_CONTEXT, rc)) return
      endif

      allocate(ftodo(fcount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "adding fields to a state", &
                                  ESMF_CONTEXT, rc)) return
      ftodo(1:fcount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the field list.
      ! For each field...
      do i=1, fcount

        call ESMF_FieldValidate(fields(i), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(ftodo, stat=localrc)
          return
        endif
        call ESMF_FieldGet(fields(i), name=fname, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(ftodo, stat=localrc)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, fname, .false., &
                                        dataitem, findex, localrc)
        if (ESMF_LogMsgFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) then
          deallocate(ftodo, stat=localrc)
          return
        endif
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            findex = -1
            ftodo(i) = 1
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
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      ! There is enough space now to add new fields to the list.
      ! This is the start of the second pass through the array list.
      do i=1, fcount

        ! If field wasn't already found in the list, we need to add it here.
        if (ftodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_FIELD

            ! Add name
            call ESMF_FieldGet(fields(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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
      deallocate(ftodo, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "adding fields to a state", &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClassAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassAddBundleList"
!BOPI
! !IROUTINE: ESMF_StateClassAddBundleList - Add a list of Bundles to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClassAddBundleList(stypep, bcount, bundles, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: bcount
      type(ESMF_Bundle), dimension(:), intent(in) :: bundles
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple bundles to an {\tt ESMF\_State}.  Internal routine only.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Internal StateClass pointer.  Required.
!     \item[bcount]
!       The number of {\tt ESMF\_Bundles} to be added.
!     \item[bundles]
!       The array of {\tt ESMF\_Bundles} to be added.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                   ! local error status
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      type(ESMF_Field) :: field
      character(len=ESMF_MAXSTR) :: bname, fname
      integer, allocatable, dimension(:) :: btodo, ftodo
      integer :: bindex, findex 
      integer :: i, j
      integer :: fcount, fruncount, newcount
      logical :: exists, fneedsdealloc

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
      fneedsdealloc = .FALSE.
      fname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (bcount .le. 0) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "bcount must be >= 0", &
                                      ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the bundles to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! get a count of all fields in all bundles
      fruncount = 0
      do i=1, bcount
        call ESMF_BundleValidate(bundles(i), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        call ESMF_BundleGet(bundles(i), fieldCount=fcount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        fruncount = fruncount + fcount
      enddo

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(btodo(bcount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "btodo", &
                                     ESMF_CONTEXT, rc)) return

      ! IMPORTANT: from here down, do not return on error, but goto 10
      !  to at least try to deallocate the temp storage.

      btodo(1:bcount) = 0

      if (fruncount .ge. 0) then
        allocate(ftodo(fruncount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "ftodo", &
                                       ESMF_CONTEXT, rc)) goto 10
        fneedsdealloc = .TRUE.
        ftodo(1:fruncount) = 0
      endif

  
      ! Initialize counters to 0, indices to 1
      fruncount = 1
      newcount = 0

      ! This is the start of the first pass through the bundle list.
      ! For each bundle...
      do i=1, bcount

        call ESMF_BundleGet(bundles(i), name=bname, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) goto 10
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, bname, .false., &
                                        dataitem, bindex, localrc)
        if (ESMF_LogMsgFoundError(localrc, "looking for preexisting entry", &
                                  ESMF_CONTEXT, rc)) goto 10
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            bindex = -1
            btodo(i) = 1
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                ! optionally warn we are replacing a real object
                ! and not just a placeholder...
            endif

            dataitem%otype = ESMF_STATEITEM_BUNDLE
            dataitem%datap%bp = bundles(i)
        
            ! Don't change flags of existing entry
            !dataitem%needed = ESMF_NEEDED
            !dataitem%ready = ESMF_READYTOREAD
            !dataitem%valid = ESMF_VALIDITYUNKNOWN
        endif

        ! and now the same for each field in the bundle
        call ESMF_BundleGet(bundles(i), fieldCount=fcount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) goto 10

        do j=1, fcount
            ! get next field and query name
            call ESMF_BundleGetField(bundles(i), j, field, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            ! what is this?  leftover debugging code?
            !call ESMF_FieldPrint(field, "", localrc)

            call ESMF_FieldGet(field, name=fname, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10
    
            exists = ESMF_StateClassFindData(stypep, fname, .false., dataitem, &
                                                              findex, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            ! If the field is going to have to be added later,
            !  keep track of whether it belongs to a bundle which has to
            !  be added new, or exists.  If it exists, note the index number
            !  so we don't have to search for it later.
            if (.not. exists) then
                newcount = newcount + 1
                ftodo(fruncount) = bindex
            else
                ! TODO: decide if we need to verify that this is only a
                ! placeholder, or if it's ok to silently overwrite an array
                ! or bundle which had the same name.
                if (dataitem%otype .ne. ESMF_STATEITEM_NAME) then
                  ! print *, "Warning: overwriting old entry"
                endif

                ! Set up the new entry.
                dataitem%otype = ESMF_STATEITEM_INDIRECT
                if (bindex .eq. -1) then
                    ! We found the field already in the state list but
                    ! not the bundle, so we can't set the right index yet.
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

            ! This is a total running count of all fields in all bundles.
            fruncount = fruncount+1
    
        enddo
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0)  goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateClassExtendList(stypep, newcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10


      ! There is enough space now to add new bundles & fields to the list.
      ! This is the start of the second pass through the bundle list.
      fruncount = 1     
      do i=1, bcount

        ! If bundle wasn't already found in the list, we need to add it here.
        if (btodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_BUNDLE

            ! Add name
            call ESMF_BundleGet(bundles(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

           nextitem%datap%bp = bundles(i)

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
        call ESMF_BundleGet(bundles(i), fieldCount=fcount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) goto 10

        ! Skip empty bundles
        if (fcount .le. 0) cycle

        ! for each field in the bundle
        do j=1, fcount

          ! If new field entry needs to be added
          if (ftodo(fruncount) .eq. -1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_INDIRECT
    
            ! get next field and query name
            call ESMF_BundleGetField(bundles(i), j, field, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            call ESMF_FieldGet(field, name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10
    
            ! If we found the corresponding bundle entry during pass 1,
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

          ! If the field entry already existed but needs bundle index updated,
          !  we do have to do a lookup on the field to see where it was
          !  found.  We just added the bundle above, so bindex is the
          !  value to set.
          else if (ftodo(fruncount) .eq. -2) then
            exists = ESMF_StateClassFindData(stypep, fname, .true., dataitem, &
                                                              findex, localrc)

            if (.not. exists) then
              call ESMF_LogMsgSetError(ESMF_RC_INTNRL_INCONS, &
                                          "field/bundle lists", &
                                          ESMF_CONTEXT, rc)
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
  
      deallocate(btodo, stat=localrc)
      if ((localrc .ne. 0) .and. (rc .eq. ESMF_SUCCESS)) then         ! F90 rc
        call ESMF_LogMsgSetError(ESMF_RC_INTNRL_BAD, &
                                         "deallocating internal bundlelist", &
                                         ESMF_CONTEXT, rc)
      endif
      if (fneedsdealloc) then
        deallocate(ftodo, stat=localrc)
        if ((localrc .ne. 0) .and. (rc .eq. ESMF_SUCCESS)) then      ! F90 rc
          call ESMF_LogMsgSetError(ESMF_RC_INTNRL_BAD, &
                                         "deallocating internal fieldlist", &
                                          ESMF_CONTEXT, rc)
        endif
      endif

      ! do not reset rc here - it should already have a valid value.

      end subroutine ESMF_StateClassAddBundleList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassAddStateList"
!BOPI
! !IROUTINE: ESMF_StateClassAddStateList - Add a list of States to a StateClass
!
! !INTERFACE:
      subroutine ESMF_StateClassAddStateList(stypep, scount, states, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: scount
      type(ESMF_State), dimension(:), intent(in) :: states
      integer, intent(out), optional :: rc     
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
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status                   ! local error status
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: sname
      integer, allocatable, dimension(:) :: stodo
      integer :: i
      integer :: newcount, sindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
      sname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (scount .le. 0) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "scount must be >= 0", &
                                     ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the states to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(stodo(scount), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "adding States", &
                                       ESMF_CONTEXT, rc)) return
      stodo(1:scount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the state list.
      ! For each state...
      do i=1, scount

        call ESMF_StateValidate(states(i), "", status)
        ! TODO: add state number to error msg
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=status)
          return
        endif

        ! Do a one-level check for adding a State to itself, but no deeper.
        ! If a nested State is added to another State, and then the combined
        ! State is added to the original State, this code is not going to 
        ! detect that loop.
        if (associated(stypep, states(i)%statep)) then
           call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
                                    "Cannot add a State to itself", &
                                    ESMF_CONTEXT, rc)
          deallocate(stodo, stat=status)
          return
        endif
   
        call c_ESMC_GetName(stypep%base, sname, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=status)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, sname, .false., dataitem, &
                                         sindex, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=status)
          return
        endif
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            sindex = -1
            stodo(i) = 1
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
      call ESMF_StateClassExtendList(stypep, newcount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(stodo, stat=status)
          return
      endif


      ! There is enough space now to add new states to the list.
      ! This is the start of the second pass through the state list.
      do i=1, scount

        ! If state wasn't already found in the list, we need to add it here.
        if (stodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEITEM_STATE

            ! Add name
            call c_ESMC_GetName(states(i)%statep%base, nextitem%namep, status)
            if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
              deallocate(stodo, stat=status)
              return
            endif

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
      deallocate(stodo, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                       "Adding States to a State", &
                                       ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClassAddStateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassFindData"
!BOPI
! !IROUTINE: ESMF_StateClassFindData - internal routine to find data item by name
!
! !INTERFACE:
      function ESMF_StateClassFindData(stypep, dataname, expected, dataitem, &
                                                                     index, rc)
!
! !RETURN VALUE:
      logical :: ESMF_StateClassFindData
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      character (len=*), intent(in) :: dataname
      logical, intent(in) :: expected
      type(ESMF_StateItem), pointer, optional :: dataitem
      integer, intent(out), optional :: index
      integer, intent(out), optional :: rc             

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
!      \item[{[dataitem]}]
!       Pointer to the corresponding {\tt ESMF\_StateItem} item if one is
!       found with the right name.
!      \item[{[index]}]
!       Index number in datalist where this name was found.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOPI

      integer :: localrc                   ! local error status
      integer :: i, dcount, itemindex
      logical :: itemfound
      type(ESMF_StateItem), pointer :: nextitem

      ! Initialize return code.  Assume failure until success assured.
      localrc = ESMF_FAILURE 
      if (present(rc)) rc = ESMF_FAILURE

      itemfound = .FALSE.
  
      ! This function is only called internally, so we do not need to check
      ! the validity of the state - it has been checked before we get here.

      ! For each item in the array, check the name
      dcount = stypep%datacount
           
      do i=1, dcount
        nextitem => stypep%datalist(i)
        if (trim(nextitem%namep) .eq. trim(dataname)) then
           itemfound = .TRUE.
           itemindex = i
           exit             ! leave loop at this point
        endif
      enddo
  
      if (itemfound) then
        ESMF_StateClassFindData = .TRUE.
        if (present(dataitem)) dataitem => stypep%datalist(itemindex) 
        if (present(index)) index = itemindex
        localrc = ESMF_SUCCESS
      else   ! item not found
        ESMF_StateClassFindData = .FALSE.
        nullify(dataitem)
        if (expected) then 
          localrc = ESMF_FAILURE
        else
          localrc = ESMF_SUCCESS
        endif
      endif

      if (present(rc)) rc = localrc

      end function ESMF_StateClassFindData

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassAddDataNameList"
!BOPI
! !IROUTINE: ESMF_StateClassAddDataNameList - internal routine
!
! !INTERFACE:
      subroutine ESMF_StateClassAddDataNameList(stypep, ncount, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_StateClass), pointer :: stypep
      integer, intent(in) :: ncount
      character (len=*), intent(in) :: namelist(:)
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a list of {\tt name}s to an existing {\tt State}.
!      The {\tt name}s must be unique within the {\tt State}
!      They are available to be marked {\tt needed} by the
!      consumer of the export {\tt State}. Then the data 
!      provider can replace the name with the actual {\tt Bundle},
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
      type(ESMF_StateItem), pointer :: nextitem, dataitem
      integer, allocatable, dimension(:) :: ntodo
      integer :: i
      integer :: newcount, nindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (ncount .le. 0) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "ncount must be >= 0", &
                                      ESMF_CONTEXT, rc)) return
      endif
      
      ! Add the fields to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(ntodo(ncount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "adding names to a state", &
                                     ESMF_CONTEXT, rc)) return
      ntodo(1:ncount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the names list.
      ! For each name...
      do i=1, ncount

        ! See if this name is already in the state
        exists = ESMF_StateClassFindData(stypep, namelist(i), .false., &
                                        dataitem, nindex, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
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
      if (ESMF_LogMsgFoundError(localrc, &
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

            ! Add name
            nextitem%namep = namelist(i)

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
      deallocate(ntodo, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "adding names to a state", &
                                     ESMF_CONTEXT, rc)) return


      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateClassAddDataNameList

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
      integer, parameter :: chunksize = 16         ! extend list by this
 
      ! Assume failure until success assured.  rc is not optional here.
      localrc = ESMF_FAILURE
      rc = ESMF_FAILURE

      ! Not an error to be called with 0 items - just return w/o error.
      if (itemcount .le. 0) then
          rc = ESMF_SUCCESS
          return
      endif

      ! An initially empty list. Simply allocate, no data copy needed.
      if (stypep%alloccount .eq. 0) then

          allocsize = itemcount + chunksize - mod(itemcount,chunksize)
          allocate(stypep%datalist(allocsize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "datalist", &
                                         ESMF_CONTEXT, rc)) return
          stypep%alloccount = allocsize

      ! Extend an existing list to the right length, including copy
      else if (stypep%alloccount .lt. stypep%datacount + itemcount) then

          newsize = stypep%datacount + itemcount
          allocsize = newsize + chunksize - mod(newsize,chunksize)
          allocate(temp_list(allocsize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "datalist realloc", &
                                         ESMF_CONTEXT, rc)) return
  
          ! Preserve old contents
          do i = 1, stypep%datacount
            temp_list(i) = stypep%datalist(i)
          enddo
  
          ! Delete old list
          deallocate(stypep%datalist, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "datalist dealloc", &
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
      recursive subroutine ESMF_StateSerialize(state, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
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

      ! shortcut to internals
      sp => state%statep

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      sp => state%statep

      call c_ESMC_BaseSerialize(sp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_StateSerialize(sp%statestatus, sp%st, sp%needed_default, &
                                 sp%ready_default, sp%stvalid_default, &
                                 sp%reqrestart_default, &
                                 sp%alloccount, sp%datacount, &
                                 buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      do i = 1, sp%datacount
          sip => sp%datalist(i)

          call c_ESMC_StateItemSerialize(sip%otype, sip%namep, &
                                         sip%indirect_index, sip%needed, &
                                         sip%ready, sip%valid, sip%reqrestart, &
                                         buffer(1), length, offset, localrc)

          select case (sip%otype%ot)
            case (ESMF_STATEITEM_BUNDLE%ot)
             call ESMF_BundleSerialize(sip%datap%bp, buffer, length, &
                                       offset, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_FIELD%ot)
             call ESMF_FieldSerialize(sip%datap%fp, buffer, &
                                       length, offset, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_ArraySerializeNoData(sip%datap%ap, buffer(1), &
                                       length, offset, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_STATE%ot)
             wrapper%statep => sip%datap%spp
             call ESMF_StateSerialize(wrapper, buffer, length, offset, localrc)
              continue ! TODO: serialize
            case (ESMF_STATEITEM_NAME%ot)
             call c_ESMC_StringSerialize(sip%namep, buffer(1), &
                                         length, offset, localrc)
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
    recursive function ESMF_StateDeserialize(vm, buffer, offset, rc) &
              result (substate)
!
! !RETURN VALUE:
      type(ESMF_State) :: substate   
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
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
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc, status                     ! Error status
      integer :: i
      type(ESMF_StateClass), pointer :: sp           ! state type
      type(ESMF_StateItem), pointer :: sip           ! state item
      type(ESMF_State) :: subsubstate


      ! in case of error, make sure this is invalid.
      !nullify(ESMF_StateDeserialize%statep)
      nullify(substate%statep)

      allocate(sp, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                     "space for new State object", &
                                     ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseDeserialize(sp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_StateDeserialize(sp%statestatus, sp%st, sp%needed_default, &
                                 sp%ready_default, sp%stvalid_default, &
                                 sp%reqrestart_default, &
                                 sp%alloccount, sp%datacount, &
                                 buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      allocate(sp%datalist(sp%alloccount), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "State type", &
                                       ESMF_CONTEXT, rc)) return

      do i = 1, sp%datacount
          sip => sp%datalist(i)

          call c_ESMC_StateItemDeserialize(sip%otype, sip%namep, &
                                         sip%indirect_index, sip%needed, &
                                         sip%ready, sip%valid, sip%reqrestart, &
                                         buffer(1), offset, localrc)

          select case (sip%otype%ot)
            case (ESMF_STATEITEM_BUNDLE%ot)
              sip%datap%bp = ESMF_BundleDeserialize(vm, buffer, offset, localrc)
              continue ! TODO: deserialize
            case (ESMF_STATEITEM_FIELD%ot)
              sip%datap%fp = ESMF_FieldDeserialize(vm, buffer, offset, localrc)
              continue ! TODO: deserialize
            case (ESMF_STATEITEM_ARRAY%ot)
              call c_ESMC_ArrayDeserializeNoData(sip%datap%ap, buffer, &
                                                 offset, localrc)
              continue ! TODO: deserialize
            case (ESMF_STATEITEM_STATE%ot)
              subsubstate = ESMF_StateDeserialize(vm, buffer, offset, localrc)
              sip%datap%spp => subsubstate%statep
              continue ! TODO: deserialize
            case (ESMF_STATEITEM_NAME%ot)
              call c_ESMC_StringDeserialize(sip%namep, buffer(1), offset, localrc)
              continue ! TODO: deserialize
            case (ESMF_STATEITEM_INDIRECT%ot)
              continue ! TODO: deserialize
            case (ESMF_STATEITEM_UNKNOWN%ot)
              continue ! TODO: deserialize
          end select


      enddo

      !ESMF_StateDeserialize%statep => sp
      substate%statep => sp
      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_StateDeserialize
!------------------------------------------------------------------------------



      end module ESMF_StateMod





