! $Id: ESMF_State.F90,v 1.41 2004/05/21 09:24:09 nscollins Exp $
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
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_LogErrMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_XformMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StateImpExpType
!     !   Enumerated value for storing Import or Export State type.
!
      type ESMF_StateImpExpType
      sequence
      private
         integer :: state
      end type

      type(ESMF_StateImpExpType), parameter :: &
                ESMF_STATEIMPORT   = ESMF_StateImpExpType(1), &
                ESMF_STATEEXPORT   = ESMF_StateImpExpType(2), &
                ESMF_STATELIST     = ESMF_StateImpExpType(3), &
                ESMF_STATEINVALID  = ESMF_StateImpExpType(4)

!------------------------------------------------------------------------------
!     ! ESMF_StateObjectType
!     !   Each entry in the list of states is either simply a name placeholder
!     !   or an actual data item - Bundle, Field, Array, or State. 
!
      type ESMF_StateObjectType
      sequence
      private
         integer :: ot
      end type

      type(ESMF_StateObjectType), parameter :: &
                ESMF_STATEBUNDLE = ESMF_StateObjectType(1), &
                ESMF_STATEFIELD = ESMF_StateObjectType(2), &
                ESMF_STATEARRAY = ESMF_StateObjectType(3), &
                ESMF_STATESTATE = ESMF_StateObjectType(4), &
                ESMF_STATEDATANAME = ESMF_StateObjectType(5), &
                ESMF_STATEINDIRECT = ESMF_StateObjectType(6), &
                ESMF_STATEOBJTYPEUNKNOWN = ESMF_StateObjectType(7)

!------------------------------------------------------------------------------
!     ! ESMF_StateDataNeeded
!     !   For an Export State if all data which can potentially be created is
!     !   not needed, this flag can be used to mark data which does not need
!     !   to be created by the Component.
!
      type ESMF_StateDataNeeded
      sequence
      private
         integer :: needed
      end type

      type(ESMF_StateDataNeeded), parameter :: &
                ESMF_STATEDATAISNEEDED = ESMF_StateDataNeeded(1), &
                ESMF_STATEDATANOTNEEDED = ESMF_StateDataNeeded(2)

!------------------------------------------------------------------------------
!     ! ESMF_StateDataReady
!
      type ESMF_StateDataReady
      sequence
      private
         integer :: ready
      end type

      type(ESMF_StateDataReady), parameter :: &
                ESMF_STATEDATAREADYTOWRITE = ESMF_StateDataReady(1), &
                ESMF_STATEDATAREADYTOREAD = ESMF_StateDataReady(2), &
                ESMF_STATEDATANOTREADY = ESMF_StateDataReady(3)


!------------------------------------------------------------------------------
!     ! ESMF_StateDataReqRestart
!
      type ESMF_StateDataReqRestart
      sequence
      private
         integer :: required4restart
      end type

      type(ESMF_StateDataReqRestart), parameter :: &
                ESMF_STATEDATAISRESTART = ESMF_StateDataReqRestart(1), &
                ESMF_STATEDATANORESTART = ESMF_StateDataReqRestart(2)


!------------------------------------------------------------------------------
!     ! ESMF_StateDataValid
!
      type ESMF_StateDataValid
      sequence
      private
         integer :: valid
      end type

      type(ESMF_StateDataValid), parameter :: &
                ESMF_STATEDATAISVALID = ESMF_StateDataValid(1), &
                ESMF_STATEDATAINVALID= ESMF_StateDataValid(2), &
                ESMF_STATEDATAVALIDITYUNKNOWN = ESMF_StateDataValid(3)


!------------------------------------------------------------------------------
!     ! ESMF_DataHolder
!
!     ! Make a single data type for Bundles, Fields, and Arrays.
!     !  The ObjectType is one level up, because this structure is not
!     !  allocated until it is actually needed.  This is a private type.

!     ! state has to be different because it's a forward reference.

      type ESMF_DataHolder
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      private
          type(ESMF_Bundle) :: bp
          type(ESMF_Field)  :: fp 
          type(ESMF_Array)  :: ap
          type(ESMF_StateType), pointer  :: spp
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateData
!
!     ! Description of next Data item in list, or simply a name
!     !  which holds the place for an optional Data item.

      type ESMF_StateData
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      private
        type(ESMF_StateObjectType) :: otype
        character(len=ESMF_MAXSTR) :: namep
        type(ESMF_DataHolder), pointer :: datap
        integer :: indirect_index
        type(ESMF_StateDataNeeded) :: needed
        type(ESMF_StateDataReady) :: ready
        type(ESMF_StateDataValid) :: valid
        type(ESMF_StateDataReqRestart) :: reqrestart
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateType
!
!     ! Internal State data type.

      type ESMF_StateType
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      private
        type(ESMF_Base) :: base
        type(ESMF_Status) :: statestatus
        type(ESMF_StateImpExpType) :: st
        type(ESMF_StateDataNeeded) :: needed_default
        type(ESMF_StateDataReady) :: ready_default
        type(ESMF_StateDataValid) :: stvalid_default
        type(ESMF_StateDataReqRestart) :: reqrestart_default
        integer :: alloccount
        integer :: datacount
        type(ESMF_StateData), dimension(:), pointer :: datalist
      end type

!------------------------------------------------------------------------------
!     ! ESMF_State
!
!     ! State data type.

      type ESMF_State
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_StateType), pointer :: statep => NULL()
#else
        type(ESMF_StateType), pointer :: statep
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State
      public ESMF_StateObjectType, ESMF_STATEBUNDLE, ESMF_STATEFIELD, &
                                   ESMF_STATEARRAY, ESMF_STATESTATE, &
                                   ESMF_STATEDATANAME
      public ESMF_StateImpExpType, ESMF_STATEIMPORT, ESMF_STATEEXPORT, &
                                   ESMF_STATELIST
      public ESMF_StateDataNeeded, ESMF_STATEDATAISNEEDED, &
                                   ESMF_STATEDATANOTNEEDED
      public ESMF_StateDataReady,  ESMF_STATEDATAREADYTOWRITE, &
                                   ESMF_STATEDATAREADYTOREAD, &
                                   ESMF_STATEDATANOTREADY
      public ESMF_StateDataReqRestart,  ESMF_STATEDATAISRESTART, &
                                   ESMF_STATEDATANORESTART
      public ESMF_StateDataValid,  ESMF_STATEDATAISVALID, &
                                   ESMF_STATEDATAINVALID, &
                                   ESMF_STATEDATAVALIDITYUNKNOWN
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateCreate, ESMF_StateDestroy

      public ESMF_StateAddNameOnly
      public ESMF_StateAddData, ESMF_StateGetData
      public ESMF_StateAddBundle, ESMF_StateAddField, ESMF_StateAddArray
      public ESMF_StateAddState
      public ESMF_StateGetBundle, ESMF_StateGetField, ESMF_StateGetArray
      public ESMF_StateGetState
      !public ESMF_StateQueryData         ! returns ESMF type for this entry
      !public ESMF_StateGetFromList

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
 
      public ESMF_StateAddAttribute       ! Set and Get Attributes
      public ESMF_StateGetAttribute       !  

      public ESMF_StateGetAttributeCount  ! number of Attributes
      public ESMF_StateGetAttributeInfo   ! get type, length by name or number

      public ESMF_StateWriteRestart
      public ESMF_StateReadRestart

      public ESMF_StateWrite
      public ESMF_StatePrint, ESMF_StateValidate

      public operator(.eq.), operator(.ne.)
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_State.F90,v 1.41 2004/05/21 09:24:09 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddData -- Add Data to a State

! !INTERFACE:
     interface ESMF_StateAddData

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddOneArray
        module procedure ESMF_StateAddArrayList
        module procedure ESMF_StateAddOneField
        module procedure ESMF_StateAddFieldList
        module procedure ESMF_StateAddOneBundle
        module procedure ESMF_StateAddBundleList
        module procedure ESMF_StateAddOneState
        module procedure ESMF_StateAddStateList
        module procedure ESMF_StateAddOneName
        module procedure ESMF_StateAddNameList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddData} functions.   
!  
!EOPI 
end interface

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
!  types of {\tt ESMF\_StateAddArray} functions.   
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
     interface ESMF_StateGetData

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateGetBundle
        module procedure ESMF_StateGetField
        module procedure ESMF_StateGetArray
        module procedure ESMF_StateGetState

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateGetData} functions.   
!  
!EOPI 
end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAddAttribute - Set a State Attribute
!
! !INTERFACE:
      interface ESMF_StateAddAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateAddIntAttr
        module procedure ESMF_StateAddIntListAttr
        module procedure ESMF_StateAddRealAttr
        module procedure ESMF_StateAddRealListAttr
        module procedure ESMF_StateAddLogicalAttr
        module procedure ESMF_StateAddLogicalListAttr
        module procedure ESMF_StateAddCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_State}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateGetAttribute - Get a State Attribute
!
! !INTERFACE:
      interface ESMF_StateGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateGetIntAttr
        module procedure ESMF_StateGetIntListAttr
        module procedure ESMF_StateGetRealAttr
        module procedure ESMF_StateGetRealListAttr
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
! !IROUTINE: ESMF_StateGetAttributeInfo - Get type, count from a State Attribute
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

!
!------------------------------------------------------------------------------

interface operator (.eq.)
 module procedure ESMF_oteq
 module procedure ESMF_imexeq
 module procedure ESMF_needeq
 module procedure ESMF_redyeq
 module procedure ESMF_valideq
end interface

interface operator (.ne.)
 module procedure ESMF_otne
 module procedure ESMF_imexne
 module procedure ESMF_needne
 module procedure ESMF_redyne
 module procedure ESMF_validne
end interface


!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF types to see if they're the same or not

function ESMF_oteq(s1, s2)
 logical ESMF_oteq
 type(ESMF_StateObjectType), intent(in) :: s1, s2

 ESMF_oteq = (s1%ot .eq. s2%ot)
end function

function ESMF_otne(s1, s2)
 logical ESMF_otne
 type(ESMF_StateObjectType), intent(in) :: s1, s2

 ESMF_otne = (s1%ot .ne. s2%ot)
end function


function ESMF_imexeq(s1, s2)
 logical ESMF_imexeq
 type(ESMF_StateImpExpType), intent(in) :: s1, s2

 ESMF_imexeq = (s1%state .eq. s2%state)
end function

function ESMF_imexne(s1, s2)
 logical ESMF_imexne
 type(ESMF_StateImpExpType), intent(in) :: s1, s2

 ESMF_imexne = (s1%state .ne. s2%state)
end function


function ESMF_needeq(s1, s2)
 logical ESMF_needeq
 type(ESMF_StateDataNeeded), intent(in) :: s1, s2

 ESMF_needeq = (s1%needed .eq. s2%needed)
end function

function ESMF_needne(s1, s2)
 logical ESMF_needne
 type(ESMF_StateDataNeeded), intent(in) :: s1, s2

 ESMF_needne = (s1%needed .ne. s2%needed)
end function


function ESMF_redyeq(s1, s2)
 logical ESMF_redyeq
 type(ESMF_StateDataReady), intent(in) :: s1, s2

 ESMF_redyeq = (s1%ready .eq. s2%ready)
end function

function ESMF_redyne(s1, s2)
 logical ESMF_redyne
 type(ESMF_StateDataReady), intent(in) :: s1, s2

 ESMF_redyne = (s1%ready .ne. s2%ready)
end function


function ESMF_valideq(s1, s2)
 logical ESMF_valideq
 type(ESMF_StateDataValid), intent(in) :: s1, s2

 ESMF_valideq = (s1%valid .eq. s2%valid)
end function

function ESMF_validne(s1, s2)
 logical ESMF_validne
 type(ESMF_StateDataValid), intent(in) :: s1, s2

 ESMF_validne = (s1%valid .ne. s2%valid)
end function



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
!      Add a single {\tt Array} reference to an existing {\tt State}.
!      The {\tt Array} name must be unique within the {\tt State}
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[array]
!      The {\tt ESMF\_Fields} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_Array) :: temp_list(1)

      temp_list(1) = array

      call ESMF_StateTypeAddArrayList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddArrayList"
!BOP
! !IROUTINE: ESMF_StateAddArray - Add a list of Arrays to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddArray()   
      subroutine ESMF_StateAddArrayList(state, acount, arrays, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: acount
      type(ESMF_Array), dimension(:), intent(in) :: arrays
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple arrays to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[acount]
!      The number of {\tt ESMF\_Arrays} to be added.
!     \item[arrays]
!      The array of {\tt ESMF\_Arrays} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        call ESMF_StateTypeAddArrayList(state%statep, acount, arrays, rc)

        end subroutine ESMF_StateAddArrayList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddIntAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set an integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddIntAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an integer attribute to an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the Attribute to set.
!     \item [value]
!       The integer value of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddIntAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddIntListAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set an integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddIntListAttr(state, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer, dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an integer list attribute to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the Attribute to set.
!     \item [count]
!       The number of values to be set.
!     \item [value]
!       The integer values of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status
      integer :: limit
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
  
      limit = size(value)
      if (count > limit) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than list", &
                                      ESMF_CONTEXT, rc)
          return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, count, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddIntListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddRealAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set a real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddRealAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      real, intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a real attribute to an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the Attribute to set.
!     \item [value]
!       The real value of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddRealAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddRealListAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set a real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddRealListAttr(state, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real, dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a real list attribute to an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the Attribute to set.
!     \item [count]
!       The number of values to be set.
!     \item [value]
!       The real values of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(value)
      if (count > limit) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than list", &
                                      ESMF_CONTEXT, rc)
          return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, count, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddRealListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddLogicalAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set a logical Attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddLogicalAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an logical attribute to an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to set.
!     \item [value]
!       The logical true/false value of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddLogicalListAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddLogicalListAttr(state, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an logical list attribute to an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the Attribute to set.
!     \item [count]
!       The number of values to be set.
!     \item [value]
!       The logical true/false values of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(value)
      if (count > limit) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than list", &
                                      ESMF_CONTEXT, rc)
          return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, count, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddCharAttr"
!BOP
! !IROUTINE: ESMF_StateAddAttribute - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddAttribute()
      subroutine ESMF_StateAddCharAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a character attribute to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to set.
!     \item [value]
!      The character value of the Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetChar(state%statep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAddCharAttr

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
!      Add a single {\tt Bundle} reference to an existing {\tt State}.
!      The {\tt Bundle} name must be unique within the {\tt State}
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      The {\tt ESMF\_State} object.
!     \item[bundle]
!      The {\tt ESMF\_Bundle} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_Bundle) :: temp_list(1)

      temp_list(1) = bundle

      call ESMF_StateTypeAddBundleList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneBundle

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddBundleList"
!BOP
! !IROUTINE: ESMF_StateAddBundle - Add a list of Bundles to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddBundle()   
      subroutine ESMF_StateAddBundleList(state, bcount, bundles, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: bcount
      type(ESMF_Bundle), dimension(:), intent(in) :: bundles
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple bundles to a {\tt State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[bcount]
!      The number of {\tt ESMF\_Bundles} to be added.
!     \item[bundles]
!      The array of {\tt ESMF\_Bundles} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        call ESMF_StateTypeAddBundleList(state%statep, bcount, bundles, rc)

        end subroutine ESMF_StateAddBundleList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneField"
!BOP
! !IROUTINE: ESMF_StateAddField - Add a Field to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddFields()   
      subroutine ESMF_StateAddOneField(state, field, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(in) :: field
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt Field} reference to an existing {\tt State}.
!      The {\tt Field} name must be unique within the {\tt State}
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[field]
!      The {\tt ESMF\_Field} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_Field) :: temp_list(1)

      temp_list(1) = field

      call ESMF_StateTypeAddFieldList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddFieldList"
!BOP
! !IROUTINE: ESMF_StateAddField - Add a list of Fields to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddFields()   
      subroutine ESMF_StateAddFieldList(state, fcount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: fcount
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple fields to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[fcount]
!      The number of {\tt ESMF\_Fields} to be added.
!     \item[fields]
!      The array {\tt ESMF\_Fields} to be added.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_StateTypeAddFieldList(state%statep, fcount, fields, rc)

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
!      Add a {\tt name} to an existing {\tt state}.
!      The {\tt name} must be unique within the {\tt state}
!      It is available to be marked {\tt needed} by the
!      consumer of the export {\tt state}. Then the data 
!      provider can replace the name with the actual {\tt ESMF\_Bundle},
!      {\tt ESMF\_Field}, or {\tt ESMF\_Array} which carries the needed data.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[name]
!      The name to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      character(len=ESMF_MAXSTR) :: temp_list(1)
      
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
      subroutine ESMF_StateAddNameList(state, namecount, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      integer, intent(in) :: namecount
      character (len=*), intent(in) :: namelist(:)
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a {\tt name} to an existing {\tt State}.
!      The {\tt name} must be unique within the {\tt State}
!      It is available to be marked {\tt needed} by the
!      consumer of the export {\tt State}. Then the data 
!      provider can replace the name with the actual {\tt Bundle},
!      {\tt Field}, or {\tt Array} which carries the needed data.
!      Unneeded data need not be generated.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[namecount]
!      The number of names.
!     \item[namelist]
!      The list of names
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      call ESMF_StateTypeAddDataNameList(state%statep, namecount, namelist, rc)      
      end subroutine ESMF_StateAddNameList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddOneState"
!BOP
! !IROUTINE: ESMF_StateAddState - Add a State to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddState()   
      subroutine ESMF_StateAddOneState(state, nestedstate, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_State), intent(in) :: nestedstate
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a single {\tt State} reference to an existing {\tt State}.
!      The {\tt State} name must be unique within the {\tt State}
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[nestedstate]
!      The {\tt ESMF\_State} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_State) :: temp_list(1)

      temp_list(1) = nestedstate

      call ESMF_StateTypeAddStateList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddOneState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAddStateList"
!BOP
! !IROUTINE: ESMF_StateAddState - Add a list of States to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAddState()   
      subroutine ESMF_StateAddStateList(state, scount, nestedstates, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: scount
      type(ESMF_State), dimension(:), intent(in) :: nestedstates
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple states to an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!      {\tt ESMF\_State} object.
!     \item[scount]
!      The number of {\tt ESMF\_States} to be added.
!     \item[nestedstate]
!      The array of {\tt ESMF\_States} to be added.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        call ESMF_StateTypeAddStateList(state%statep, scount, nestedstates, rc)

        end subroutine ESMF_StateAddStateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateCreate"
!BOP
! !IROUTINE: ESMF_StateCreate - Create a new State

! !INTERFACE:
      function ESMF_StateCreate(statename, statetype, &
                   bundles, fields, arrays, nestedstates, names, itemcount, &
                   dataneeded, dataready, datavalid, datareqrestart, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreate
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateImpExpType), intent(in), optional :: statetype
      type(ESMF_Bundle), dimension(:), intent(in), optional :: bundles
      type(ESMF_Field), dimension(:), intent(in), optional :: fields
      type(ESMF_Array), dimension(:), intent(in), optional :: arrays
      type(ESMF_State), dimension(:), intent(in), optional :: nestedstates
      character(len=*), dimension(:), intent(in), optional :: names
      integer, intent(in), optional :: itemcount
      type(ESMF_StateDataNeeded), optional :: dataneeded
      type(ESMF_StateDataReady), optional :: dataready
      type(ESMF_StateDataValid), optional :: datavalid
      type(ESMF_StateDataReqRestart), optional :: datareqrestart
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_State} and optionally add initial objects to it.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[statename]}]
!    Name of this {\tt ESMF\_State} object.   A default name will be generated
!    if none is specified.
!   \item[{[statetype]}]
!    Import or Export {\tt ESMF\_State}.  Valid values are 
!    {\tt ESMF\_STATEIMPORT}, {\tt ESMF\_STATEEXPORT}, 
!    or {\tt ESMF\_STATELIST} The default is {\tt ESMF\_STATELIST}.
!   \item[{[bundles]}]
!    A list (fortran array) of {\tt ESMF\_Bundle}s.
!   \item[{[fields]}]
!    A list (fortran array) of {\tt ESMF\_Field}s.
!   \item[{[arrays]}]
!    A list (fortran array) of {\tt ESMF\_Array}s.
!   \item[{[nestedstates]}]
!    A list (fortran array) of {\tt ESMF\_State}s to be nested 
!    inside the outer {\tt ESMF\_State}.
!   \item[{[names]}]
!    A list (fortran array) of character string name placeholders.
!   \item[{[itemcount]}]
!    The total number of Bundles, Fields, Arrays, States, and Names to be added.
!    If not specified the lengths of each array will be used to compute the
!    actual number of items added to the State.  If the count is specified
!    it will do an error check to verify the total number of items found
!    in the argument lists matches this count of the expected number of items.
!   \item[{[dataneeded]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAISNEEDED} or 
!    {\tt ESMF\_STATEDATANOTNEEDED}.  If not specified, the default value is
!    set to {\tt ESMF\_STATEDATAISNEEDED}.
!   \item[{[dataready]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAREADYTOWRITE},
!    {\tt ESMF\_STATEDATAREADYTOREAD}, or {\tt ESMF\_STATEDATANOTREADY}.
!    If not specified, the default value is set to 
!    {\tt ESMF\_STATEDATAREADYTOREAD}.
!   \item[{[datavalid]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAISVALID},
!    {\tt ESMF\_STATEDATAINVALID}, or {\tt ESMF\_STATEDATAVALIDITYUNKNOWN}.
!    If not specified, the default value is set to 
!    {\tt ESMF\_STATEDATAISVALID}.
!   \item[{[datareqrestart]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAISRESTART} or
!    {\tt ESMF\_STATEDATANORESTART}. If not specified, the default 
!    value is set to {\tt ESMF\_STATEDATAISRESTART}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP


        ! local vars
        type (ESMF_StateType), pointer :: stypep
        integer :: localrc                          ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Initialize the pointers to null.
        nullify(ESMF_StateCreate%statep)
        nullify(stypep)

        allocate(stypep, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "State type", &
                                       ESMF_CONTEXT, rc)) return
      
        call ESMF_StateConstruct(stypep, statename, statetype, &
                   bundles, fields, arrays, nestedstates, names, itemcount, &
                   dataneeded, dataready, datavalid, datareqrestart, rc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_StateCreate%statep => stypep 
        if (present(rc)) rc = ESMF_FAILURE

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
        logical :: dummy

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Simple sanity checks
        if (.not.associated(state%statep)) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "State uninitialized or already destroyed", &
                                 ESMF_CONTEXT, rc)
          return
        endif
        if (state%statep%st .eq. ESMF_STATEINVALID) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "State uninitialized or already destroyed", &
                                 ESMF_CONTEXT, rc)
          return
        endif

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
      subroutine ESMF_StateGet(state, name, stateType, &
                                     itemCount, itemNames, objTypes, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(out), optional :: name
      type(ESMF_StateImpExpType), intent(out), optional :: stateType
      integer, intent(out), optional :: itemCount
      character (len=*), intent(out), optional :: itemNames(:)
      type(ESMF_StateObjectType), intent(out), optional :: objTypes(:)
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the requested information about this {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} object.
!      \item[{[name]}]
!       Name of this {\tt ESMF\_State}.
!      \item[{[stateType]}]
!       Import or Export {\tt ESMF\_State}.  Returns either {\tt ESMF\_STATEIMPORT},
!       {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.
!      \item[{[itemCount]}]
!        Count of items in this {\tt ESMF\_State}, including placeholder names.
!      \item[{[itemNames]}]
!        Array of item names in this {\tt ESMF\_State}, including placeholder names,
!        {\tt itemCount} long.
!      \item[{[objtypes]}]
!        Array of item object types in this {\tt state}, including placeholder 
!        names, {\tt itemCount} long.  State object types include
!        {\tt ESMF\_STATEBUNDLE}, {\tt ESMF\_STATEFIELD}, {\tt ESMF\_STATEARRAY}, 
!        {\tt ESMF\_STATESTATE}, {\tt ESMF\_STATEDATANAME}, or
!        {\tt ESMF\_STATEOBJTYPEUNKNOWN}.
!       \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!
!EOP
      integer :: i, localrc
      type(ESMF_StateType), pointer :: stypep
      type(ESMF_StateData), pointer :: nextitem

      if (.not.associated(state%statep)) then
        print *, "Error: uninitialized or invalid State"
        if (present(rc)) rc=ESMF_FAILURE
        return
      endif
      stypep => state%statep
      if (stypep%statestatus .ne. ESMF_STATE_READY) then
        print *, "Error: uninitialized or invalid State"
        if (present(rc)) rc=ESMF_FAILURE
        return
      endif

      if (present(name)) call c_ESMC_GetName(stypep%base, name, localrc)
      if (present(statetype)) statetype = stypep%st

      ! TODO: indirect entries for Fields inside of Bundles complicates
      !  this code.  the count needs to be both primary objects and
      !  total objects.  perhaps the state derived type needs to bookkeep
      !  both numbers.  For now, return entire raw count.

      if (present(itemcount)) itemcount = stypep%datacount 

      if (present(itemnames)) then
          do i=1, stypep%datacount
              nextitem => stypep%datalist(i)
              itemnames(i) = nextitem%namep
          enddo
      endif

      if (present(objtypes)) then
          do i=1, stypep%datacount
              nextitem => stypep%datalist(i)
              objtypes(i) = nextitem%otype
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
      subroutine ESMF_StateGetArray(state, arrayname, array, statename, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: arrayname
      type(ESMF_Array), intent(out) :: array
      character (len=*), intent(in), optional :: statename
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Array} from an {\tt ESMF\_State} by name.  
!      If the {\tt State} is a simple {\tt ESMF\_State}, only the
!      {\tt arrayname} is required.
!      If the {\tt ESMF\_State} contains multiple {\tt States}, 
!      an additional argument, the {\tt statename}, is required to 
!      specify which nested State to select.  {\tt ESMF\_State}s can
!      be nested to any depth, but this routine only searches for immediate
!      descendents.  It is an error to specify a {\tt statename} if the
!      {\tt state} contains no nested {\tt ESMF\_States}.
!
!      Returns a {\tt Array} from a {\tt State} by name.
!
!
!     The arguments are:
!  \begin{description}     
!  \item[state]
!   State to query for a {\tt Array} named {\tt name}.
!  \item[arrayname]
!    Name of {\tt ESMF\_Array} to return.
!  \item[array]
!    Returned {\tt ESMF\_Array}.
!  \item[{[statename]}]
!    Optional.  An error if specified when the {\tt state} argument is a
!    a simple {\tt ESMF\_State}.  Required if the {\tt state} contains 
!    multiple nested {\tt ESMF\_State}s, and therefore the exact 
!    {\tt ESMF\_State} must be selected by this {\tt statename}.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP

      type(ESMF_StateData), pointer :: dataitem
      type(ESMF_State) :: top
      logical :: exists
      integer :: localrc
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: dummy

      localrc = ESMF_FAILURE
       
      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE
      ! TODO: do we need an empty array to mark failure?

      if (present(statename)) then
          exists = ESMF_StateTypeFindData(state%statep, statename, .true., &
                                                          dataitem, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (.not. exists) then
              print errmsg, "no nested state named", trim(statename), "found"
              dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)
              return
          endif
    
          if (dataitem%otype .ne. ESMF_STATESTATE) then
              print errmsg, trim(statename), "found but not type State"
              dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)
              return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateTypeFindData(top%statep, arrayname, .true., &
                                                          dataitem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (.not. exists) then
          print errmsg, "no Array found named", trim(arrayname)
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      if (dataitem%otype .ne. ESMF_STATEARRAY) then
          print errmsg, trim(arrayname), "found but not type Array"
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      array = dataitem%datap%ap

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetIntAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve an integer Attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetIntAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from an {\tt ESMF\_State}.
!
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [value]
!      The integer value of the named Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_INTEGER, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetIntAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetIntListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve an integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetIntListAttr(state, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer, dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer list attribute from an {\tt ESMF\_State}.
!
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [count]
!      The number of values to be retrieved.
!     \item [value]
!      The integer values of the named Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(value)
      if (count > limit) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than list", &
                                      ESMF_CONTEXT, rc)
          return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, count, &
                                    ESMF_DATA_INTEGER, count, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetIntListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetRealAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetRealAttr(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      real, intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a real attribute from an {\tt ESMF\_State}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [value]
!      The real value of the named Attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetRealAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetRealListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetRealListAttr(state, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real, dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a real attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [count]
!      The number of values to be set.
!     \item [value]
!      The real values of the named Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(value)
      if (count > limit) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than list", &
                                      ESMF_CONTEXT, rc)
          return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_REAL, count, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetRealListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetLogicalAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a logical Attribute
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
!      Returns an logical attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [value]
!      The logical value of the named Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetLogicalListAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttribute()
      subroutine ESMF_StateGetLogicalListAttr(state, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an logical list attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [count]
!      The number of values to be set.
!     \item [value]
!      The logical values of the named Attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      integer :: limit
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(value)
      if (count > limit) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than list", &
                                      ESMF_CONTEXT, rc)
          return
      endif

      call c_ESMC_AttributeGetValue(state%statep%base, name, &
                                    ESMF_DATA_LOGICAL, count, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetCharAttr"
!BOP
! !IROUTINE: ESMF_StateGetAttribute - Retrieve a character Attribute
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
!      Returns an integer attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to retrieve.
!     \item [value]
!      The character value of the named Attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

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
! !IROUTINE: ESMF_StateGetAttributeCount - Query the number of Attributes
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
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [count]
!      The number of attributes on this object.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

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
! !IROUTINE: ESMF_StateGetAttributeInfo - Query State Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttributeInfo()
      subroutine ESMF_StateGetAttrInfoByName(state, name, type, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
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
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the Attribute to query.
!     \item [type]
!      The  data type of the Attribute, which includes ESMF\_DATA\_INTEGER,
!      ESMF\_DATA\_REAL, ESMF\_DATA\_LOGICAL, ESMF\_DATA\_CHARACTER.
!     \item [count]
!      The number of items in this Attribute.  For character types,
!       the length of the character string.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      type(ESMF_DataType) :: localDt
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeGetAttrInfoName(state%statep%base, name, &
                                           localDt, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(type)) type = localDt
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetAttrInfoByNum"
!BOP
! !IROUTINE: ESMF_StateGetAttributeInfo - Query State Attributes by number
!
! !INTERFACE:
      ! Private name; call using ESMF_StateGetAttributeInfo()
      subroutine ESMF_StateGetAttrInfoByNum(state, num, name, type, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state  
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
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [num]
!      The number of the Attribute to query.
!     \item [name]
!       Returns the name of the Attribute.
!     \item [type]
!       Returns the type of the Attribute.
!     \item [count]
!       Returns the number of items in this Attribute.  For character types,
!       this is the length of the character string.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_DataType) :: localDt
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeGetAttrInfoNum(state%statep%base, num, &
                                        localName, localDt, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(type)) type = localDt
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
      subroutine ESMF_StateGetBundle(state, bundlename, bundle, statename, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: bundlename
      type(ESMF_Bundle), intent(out) :: bundle
      character (len=*), intent(in), optional :: statename
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Bundle} from an {\tt ESMF\_State} by name.  
!      If the {\tt state} is a simple {\tt ESMF\_State} only the
!      {\tt bundlename} is required.
!      If the {\tt state} contains multiple {\tt ESMF\_States} 
!      an additional argument, the {\tt statename}, is required to 
!      specify which nested State to select.  {\tt ESMF\_State}s can
!      be nested to any depth, but this routine only searches immediate
!      descendents.  It is an error to specify a {\tt statename} if the
!      {\tt state} contains no nested {\tt ESMF\_States}.
!
!     The arguments are:
!      \begin{description}     
!      \item[state]
!       {\tt ESMF\_State} to query.
!      \item[bundlename]
!       Name of {\tt ESMF\_Bundle} to return.
!      \item[bundle]
!       Returned {\tt ESMF\_Bundle}.
!      \item[{[statename]}]
!       Optional.  An error if specified when the {\tt state} argument is a
!       a simple {\tt ESMF\_State}.  Required if the {\tt state} contains 
!       multiple nested {\tt ESMF\_State}s, and therefore the exact 
!       {\tt ESMF\_State} must be selected by this {\tt statename}.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

      integer :: localrc
      type(ESMF_StateData), pointer :: dataitem
      type(ESMF_State) :: top
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      logical :: dummy

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE
      ! TODO: do we need an empty bundle to mark failure?

      if (present(statename)) then
          exists = ESMF_StateTypeFindData(state%statep, statename, .true., &
                                                          dataitem, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (.not. exists) then
              print errmsg, "no nested state named", trim(statename), "found"
              dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)
              return
          endif
    
          if (dataitem%otype .ne. ESMF_STATESTATE) then
              print errmsg, trim(statename), "found but not type State"
              dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)
              return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateTypeFindData(top%statep, bundlename, .true., &
                                                          dataitem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (.not. exists) then
          print errmsg, "no Bundle found named", trim(bundlename)
          dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      if (dataitem%otype .ne. ESMF_STATEBUNDLE) then
          print errmsg, trim(bundlename), "found but not type Bundle"
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
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
      subroutine ESMF_StateGetField(state, fieldname, field, statename, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: fieldname
      type(ESMF_Field), intent(out) :: field
      character (len=*), intent(in), optional :: statename
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns an {\tt ESMF\_Field} from an {\tt ESMF\_State} by name.  
!      If {\tt state} is a simple {\tt ESMF\_State}, only the
!      {\tt fieldname} is required.
!      If {\tt state} contains nested {\tt ESMF\_States} 
!      an additional argument, the {\tt statename}, is required to 
!      specify which nested State to select.  {\tt ESMF\_State}s can
!      be nested to any depth, but this routine only searches for immediate
!      descendents.  It is an error to specify a {\tt statename} if the
!      {\tt state} contains no nested {\tt ESMF\_States}.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} to query.
!     \item[fieldname]
!       Name of {\tt ESMF\_Field} to return.
!     \item[field]
!       Returned {\tt ESMF\_Field}.
!     \item[{[statename]}]
!       Optional.  An error if specified when the {\tt state} argument is a
!       a simple {\tt ESMF\_State}.  Required if the {\tt state} contains 
!       multiple nested {\tt ESMF\_State}s, and therefore the exact 
!       {\tt ESMF\_State} must be selected by this {\tt statename}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_StateData), pointer :: dataitem
      type(ESMF_State) :: top
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      integer :: localrc
      logical :: dummy

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE
      ! TODO: do we need an empty field to mark failure?

      if (present(statename)) then
          exists = ESMF_StateTypeFindData(state%statep, statename, .true., &
                                                          dataitem, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (.not. exists) then
              print errmsg, "no nested state named", trim(statename), "found"
              dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                         ESMF_CONTEXT, rc)
              return
          endif
    
          if (dataitem%otype .ne. ESMF_STATESTATE) then
              print errmsg, trim(statename), "found but not type State"
              dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                          ESMF_CONTEXT, rc)
              return
          endif
          
          top%statep => dataitem%datap%spp
      else
          top%statep => state%statep
      endif


      exists = ESMF_StateTypeFindData(top%statep, fieldname, .true., &
                                                          dataitem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (.not. exists) then
          print errmsg, "no Field found named", trim(fieldname)
          dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                     ESMF_CONTEXT, rc)
          return
      endif

      if (dataitem%otype .ne. ESMF_STATEFIELD) then
          if (dataitem%otype .eq. ESMF_STATEINDIRECT) then
              ! TODO: how do we return the info that this is inside a bundle?
              dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                       "extracting Fields directly from Bundles in a State", &
                       ESMF_CONTEXT, rc)
              return
          endif
          print errmsg, trim(fieldname), "found but not type Field"
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_INCOMP, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      field = dataitem%datap%fp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetField



#if 0
!! TODO - remove this code when we finalize on nested State objects.
!!  but this has shifted around so many times, i'm not removing this code
!!  until we're sure...
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetFromList"
!BOPI
! !IROUTINE: ESMF_StateGetFromList -- Get a State by name from an array of States
!
! !INTERFACE:
      function ESMF_StateGetFromList(statelist, statename, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateGetFromList
!
!
! !ARGUMENTS:
      type(ESMF_State), dimension(:), intent(in) :: statelist
      character (len=*), intent(in) :: statename
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the State from this array of {\tt State}s by name.
!
!     The arguments are:
!     \begin{description}     
!     \item[statelist]
!       Fortran array of {\tt ESMF\_State}s to query.
!      \item[statename]
!       State Name to return.
!      \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOPI

      integer :: localrc
      integer :: nstates, i
      type(ESMF_StateType), pointer :: stypep
      character (len=ESMF_MAXSTR) :: tryname
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: dummy

      ! assume failure until success assured
      if (present(rc)) rc = ESMF_FAILURE

      ESMF_StateGetFromList%statep => NULL()
   

      nstates = size(statelist)
      do i = 1, nstates
  
        stypep => statelist(i)%statep
  
        if (.not. associated(stypep)) then
          print errmsg, "uninitialized State in statelist, number", i
          dummy=EM_LogMsgFoundError(ESMF_RC_OBJ_BAD, errmsg, rc)
          return
        endif
  
        if (stypep%st .eq. ESMF_STATEINVALID) then
          print errmsg, "invalid State in statelist, number", i
          dummy=EM_LogMsgFoundError(ESMF_RC_OBJ_BAD, errmsg, rc)
          return
        endif
  
        call c_ESMC_GetName(stypep%base, tryname, localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          print errmsg, "cannot get name from State number", i, "in list"
          dummy=EM_LogMsgFoundError(localrc, errmsg, rc)
          return
        endif
  
        if (statename .eq. tryname) then
          ESMF_StateGetFromList%statep => stypep
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif
  
      enddo

      print errmsg, "State with name", trim(statename), &
                     "not found in list of States"
      dummy=EM_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, rc)

      end function ESMF_StateGetFromList
#endif

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetNeeded"
!BOP
! !IROUTINE: ESMF_StateGetNeeded - Query whether a data item is needed
!
! !INTERFACE:
      subroutine ESMF_StateGetNeeded(state, dataname, needed, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: dataname
      type(ESMF_StateDataNeeded), intent(out) :: needed
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the status of the {\tt needed} flag for the data item
!      named by {\tt dataname} in the {\tt State}.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} to query.
!      \item[dataname]
!       Name of the data item to query.
!      \item[needed]
!       Status of data item.  Returns either {\tt ESMF\_STATEDATAISNEEDED},
!       or {\tt ESMF\_STATEDATANOTNEEDED}.
!       When data is added to a {\tt State} the default status of this flag
!       is {\tt ESMF\_STATEDATANOTNEEDED}.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: localrc
      logical :: dummy

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE

      exists = ESMF_StateTypeFindData(state%statep, dataname, .true., &
                                      dataitem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (.not. exists) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, trim(dataname), &
                                     ESMF_CONTEXT, rc)
          return
      endif

      needed = dataitem%needed

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetNeeded

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetState"
!BOP
! !IROUTINE: ESMF_StateGetState - Retrieve a State nested in a State
!
! !INTERFACE:
      subroutine ESMF_StateGetState(state, statename, nestedstate, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: statename
      type(ESMF_State), intent(out) :: nestedstate
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a nested {\tt ESMF\_State} from another {\tt ESMF\_State} 
!      by name.
!      Unlike the other routines, this does not allow the caller to
!      retrieve an {\tt ESMF\_State} from two levels down.  It returns
!      immediate child objects only.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} to query for a nested {\tt ESMF\_State} 
!       named {\tt statename}.
!     \item[statename]
!       Name of {\tt ESMF\_State} to return.
!     \item[nestedstate]
!       Returned {\tt ESMF\_State}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      type(ESMF_StateData), pointer :: dataitem
      character(len=ESMF_MAXSTR) :: errmsg
      logical :: exists
      integer :: localrc
      logical :: dummy

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE
      ! TODO: do we need an empty state to mark failure?

      exists = ESMF_StateTypeFindData(state%statep, statename, .true., &
                                                         dataitem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (.not. exists) then
          print errmsg, "no nested state named", trim(statename), "found"
          dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      if (dataitem%otype .ne. ESMF_STATESTATE) then
          print errmsg, trim(statename), "found but not type State"
          dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, errmsg, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      nestedstate%statep => dataitem%datap%spp

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateIsNeeded"
!BOP
! !IROUTINE: ESMF_StateIsNeeded -- Return logical true if data item needed
!
! !INTERFACE:
      function ESMF_StateIsNeeded(state, dataname, rc)
!
! !RETURN VALUE:
      logical :: ESMF_StateIsNeeded
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: dataname
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns true if the status of the {\tt needed} flag for the data item
!      named by {\tt dataname} in the {\tt State} is 
!      {\tt ESMF\_STATEDATAISNEEDED}.  Returns false for no state found 
!      with the specified name or state unknown or not needed.  Sets error
!      code tt {\tt ESMF\_FAILURE} if name not found.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt ESMF\_State} to query.
!      \item[dataname]
!       Name of the data item to query.
!      \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: localrc
      logical :: dummy

      ! Assume no unless we find it and it is needed.
      if (present(rc)) rc = ESMF_FAILURE
      ESMF_StateIsNeeded = .FALSE.

      ! TODO: decide on the behavior:
      ! should it be an error to ask about a state which doesn't exist?
      ! if the 3rd arg below is .true. then it's an error, if it's .false.
      ! then it's not.  for now, it's an error.
      exists = ESMF_StateTypeFindData(state%statep, dataname, .true., &
                                      dataitem, rc=localrc)
      if (.not. exists) then
          dummy=ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      if (dataitem%needed .eq. ESMF_STATEDATAISNEEDED) ESMF_StateIsNeeded = .TRUE.
  
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
!      Routine to print information about an state.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to print.
!     \item[{[options]}]
!       Print options
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!

!EOP

!
! TODO: this needs more code added to be complete
!
       character (len=6) :: defaultopts
       type(ESMF_StateType), pointer :: sp
       type(ESMF_StateData), pointer :: dp
       character (len=1024) :: outbuf
       integer :: localrc                          ! local error status
       integer :: i
       logical :: dummy

       defaultopts = "brief"
       ! Initialize return code; assume failure until success is certain
       if (present(rc)) rc = ESMF_FAILURE

       ! TODO: Add code here
       ! print num of states, state type, etc

       print *, "StatePrint: "  
       if (.not.associated(state%statep)) then 
           dummy=ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "Uninitialized or already destroyed State", &
                                  ESMF_CONTEXT, rc)
           return
       endif
       sp => state%statep

       call ESMF_BasePrint(sp%base, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
       if (sp%st .eq. ESMF_STATEIMPORT) print *, "  Import State"
       if (sp%st .eq. ESMF_STATEEXPORT) print *, "  Export State"
       if (sp%st .eq. ESMF_STATELIST) print *, "  State List"
       if (sp%st .eq. ESMF_STATEINVALID) then
           dummy=ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "Uninitialized or already destroyed State", &
                                  ESMF_CONTEXT, rc)
           return
       endif
       print *, "  Number of members: ", sp%datacount
      
       do i=1, sp%datacount
         dp => sp%datalist(i)

         print *, "  Item ", i, ":"
         outbuf = "    Name= " // trim(dp%namep) // ", "

         select case (dp%otype%ot)
           case (ESMF_STATEBUNDLE%ot)
             outbuf = trim(outbuf) //  " type Bundle,"
           case (ESMF_STATEFIELD%ot)
             outbuf = trim(outbuf) //  " type Field,"
           case (ESMF_STATEARRAY%ot)
             outbuf = trim(outbuf) //  " type Array,"
           case (ESMF_STATESTATE%ot)
             outbuf = trim(outbuf) //  " type State,"
           case (ESMF_STATEDATANAME%ot)
             outbuf = trim(outbuf) //  " placeholder name,"
           case (ESMF_STATEINDIRECT%ot)
             outbuf = trim(outbuf) //  " field inside a bundle,"
           case (ESMF_STATEOBJTYPEUNKNOWN%ot)
             outbuf = trim(outbuf) //  " unknown type,"
         end select

         select case (dp%needed%needed)
           case (ESMF_STATEDATAISNEEDED%needed)
             outbuf = trim(outbuf) //  " marked as needed."
           case (ESMF_STATEDATANOTNEEDED%needed)
             outbuf = trim(outbuf) //  " marked as NOT needed."
         end select

        print *, trim(outbuf)

        ! TODO: finish printing more info here
        !type(ESMF_StateDataReady) :: ready
        !type(ESMF_StateDataValid) :: valid

        !type(ESMF_DataHolder), pointer :: datap

        !print *, trim(outbuf)

       enddo


       ! Set return values
       if (present(rc)) rc = ESMF_SUCCESS

       end subroutine ESMF_StatePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReadRestart"
!BOP
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
!      Used to reinitialize
!      all data associated with a State from the last call to WriteRestart.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!       Name of {\tt ESMF\_State} to reinitialize.
!     \item[iospec]
!       {\tt IOSpec} to restart from.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!
! TODO: code goes here
!
        type (ESMF_State) :: a 

!       this is just to shut the compiler up
        type (ESMF_StateType), target :: b 
        a%statep => b
        nullify(a%statep)

        ESMF_StateReadRestart = a 
 
        end function ESMF_StateReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSetNeeded"
!BOP
! !IROUTINE: ESMF_StateSetNeeded - Set if a data item is needed
!
! !INTERFACE:
      subroutine ESMF_StateSetNeeded(state, dataname, needed, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      character (len=*), intent(in) :: dataname
      type(ESMF_StateDataNeeded), intent(in) :: needed
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets the status of the {\tt needed} flag for the data item
!      named by {\tt dataname} in the {\tt State}.
!
!     The arguments are:
!      \begin{description}     
!      \item[state]
!        {\tt ESMF\_State} to set.
!       \item[dataname]
!        Name of the data item to set.
!       \item[needed]
!        Set status of data item to this.  Valid values are 
!        {\tt ESMF\_STATEDATAISNEEDED}, or {\tt ESMF\_STATEDATANOTNEEDED}. 
!       \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: localrc
      logical :: dummy

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_FAILURE

      exists = ESMF_StateTypeFindData(state%statep, dataname, .true., &
                                      dataitem, rc=localrc)
      if (.not. exists) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, dataname, &
                                      ESMF_CONTEXT, rc)
          return
      endif

      dataitem%needed = needed

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetNeeded

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateValidate"
!BOP
! !IROUTINE: ESMF_StateValidate -- Validate the internal data for a State
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
!      Routine to validate information inside an state.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to validate.
!     \item[{[options]}]
!       Validation options
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
      logical :: dummy

      defaultopts = "brief"
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      if (.not.associated(state%statep)) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rc)
          return
      endif

      if (state%statep%st .eq. ESMF_STATEINVALID) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rc)
          return
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
!  Apply a {\tt Transform} to a {\tt State}.  
!  This routine is intended to be called
!  from within a {\tt Component} when it is not practical to return to the
!  calling layer in order to do the coupling directly.  This call 
!  passes through the framework back into user-written coupling code
!  to allow exchange of data between {\tt Component}s.  It returns back to
!  the calling location and allows the {\tt Component} to continue 
!  executionfrom that place.  
!  {\tt Component}s which run in sequential mode have no need to use
!  this routine; Coupling code is called directly from the Application
!  level code.
!
!     The arguments are:
!     \begin{description}     
!     \item[state]
!       {\tt State} to apply {\tt ESMF\_Xform} to.
!     \item[xformname]
!       {\tt Xform} name to be called.
!     \item[xform]
!       {\tt Xform} object to be called.
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

        end subroutine ESMF_StateTransform

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateWrite"
!BOP
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
!       {\tt ESMF\_IOSpec} to write to.
!     \item[{[itemname]}]
!       Item to be written.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        ! FIXME: hardcoded for interopability test
        type(ESMF_Field) :: fred
        integer :: localrc

        if (present(itemname)) then
            call ESMF_StateGetField(state, fieldname=itemname, field=fred, rc=localrc)
            call ESMF_FieldWrite(fred, iospec=iospec, rc=localrc) 
        endif
  

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
!       {\tt ESMF\_IOSpec} to be used.
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
        end subroutine ESMF_StateWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateConstruct"
!BOPI
! !IROUTINE: ESMF_StateConstruct -- Construct a new State

! !INTERFACE:
      subroutine ESMF_StateConstruct(stypep, statename, statetype, & 
                         bundles, fields, arrays, states, names, itemcount, &
                         dataneeded, dataready, datavalid, datareqrestart, rc)
!
! !ARGUMENTS:
      type (ESMF_StateType), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateImpExpType), intent(in), optional :: statetype
      type(ESMF_Bundle), dimension(:), intent(in), optional :: bundles
      type(ESMF_Field), dimension(:), intent(in), optional :: fields
      type(ESMF_Array), dimension(:), intent(in), optional :: arrays
      type(ESMF_State), dimension(:), intent(in), optional :: states
      character(len=*), dimension(:), intent(in), optional :: names
      integer, intent(in), optional :: itemcount
      type(ESMF_StateDataNeeded), optional :: dataneeded
      type(ESMF_StateDataReady), optional :: dataready
      type(ESMF_StateDataValid), optional :: datavalid
      type(ESMF_StateDataReqRestart), optional :: datareqrestart
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Construct a new State and set the decomposition characteristics.
!  The return value is a new State.
!
!  The arguments are:
!  \begin{description}
!   \item[stypep]
!    Internal StateType pointer.  Required.
!   \item[{[statename]}]
!    Name of this {\tt ESMF\_State} object. 
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Should be one of {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.   
!    {\tt ESMF\_STATELIST} is the default if not specified.
!   \item[{[bundles]}]
!    An array of {\tt Bundles}.
!   \item[{[fields]}]
!    An array of {\tt Fields}.
!   \item[{[arrays]}]
!    An array of {\tt Arrays}.
!   \item[{[states]}]
!    An array of nested {\tt ESMF\_States}.
!   \item[{[names]}]
!    An array of name placeholders.
!   \item[{[itemcount]}]
!    The total number of Bundles, Fields, Arrays, States, and Names specified.
!    This argument is optional, and if specified is used as an error check
!    to verify that the actual total number of items found matches this count.
!   \item[{[dataneeded]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAISNEEDED} or 
!    {\tt ESMF\_STATEDATANOTNEEDED}.  If not specified, the default value is
!    set to {\tt ESMF\_STATEDATAISNEEDED}.
!   \item[{[dataready]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAREADYTOWRITE},
!    {\tt ESMF\_STATEDATAREADYTOREAD}, or {\tt ESMF\_STATEDATANOTREADY}.
!    If not specified, the default value is set to 
!    {\tt ESMF\_STATEDATAREADYTOREAD}.
!   \item[{[datavalid]}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAISVALID},
!    {\tt ESMF\_STATEDATAINVALID}, or {\tt ESMF\_STATEDATAVALIDITYUNKNOWN}.
!    If not specified, the default value is set to 
!    {\tt ESMF\_STATEDATAISVALID}.
!   \item[{[datareqrestart}]
!    Set the default value for new items added to an {\tt ESMF\_State}.  
!    Valid values are {\tt ESMF\_STATEDATAISRESTART} or
!    {\tt ESMF\_STATEDATANORESTART}. If not specified, the default 
!    value is set to {\tt ESMF\_STATEDATAISRESTART}.
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
            print *, "itemcount does not match lists given"
            return
          endif
        endif

        ! Set initial values
        call ESMF_StateConstructEmpty(stypep, statename, statetype, localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif

        ! Set the defaults for objects added to this state
        if (present(dataneeded)) then
            stypep%needed_default = dataneeded
        else
            stypep%needed_default = ESMF_STATEDATAISNEEDED
        endif

        if (present(dataready)) then
            stypep%ready_default = dataready
        else
            stypep%ready_default = ESMF_STATEDATAREADYTOREAD
        endif

        if (present(datavalid)) then
            stypep%stvalid_default = datavalid
        else
            stypep%stvalid_default = ESMF_STATEDATAISVALID
        endif

        if (present(datareqrestart)) then
            stypep%reqrestart_default = datareqrestart
        else
            stypep%reqrestart_default = ESMF_STATEDATAISRESTART
        endif

        ! Set the initial size of the datalist
        call ESMF_StateTypeExtendList(stypep, count, localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif
      
        ! For each item type, set the data values.  All the allocation 
        !  has already been done.
        if (present(bundles)) then
          count = size(bundles)
          if (count .gt. 0) then
            call ESMF_StateTypeAddBundleList(stypep, count, bundles, localrc)
            if (localrc .ne. ESMF_SUCCESS) then
              print *, "State construction error adding bundles"
              return
            endif
          endif
        endif

        if (present(fields)) then
          count = size(fields)
          if (count .gt. 0) then
            call ESMF_StateTypeAddFieldList(stypep, count, fields, localrc)
            if (localrc .ne. ESMF_SUCCESS) then
              print *, "State construction error adding fields"
              return
            endif
          endif
        endif

        if (present(arrays)) then
          count = size(arrays)
          if (count .gt. 0) then
            call ESMF_StateTypeAddArrayList(stypep, count, arrays, localrc)
            if (localrc .ne. ESMF_SUCCESS) then
              print *, "State construction error adding arrays"
              return
            endif
          endif
        endif

        if (present(states)) then
          count = size(states)
          if (count .gt. 0) then
            call ESMF_StateTypeAddStateList(stypep, count, states, localrc)
            if (localrc .ne. ESMF_SUCCESS) then
              print *, "State construction error adding states"
              return
            endif
          endif
        endif

        if (present(names)) then
          count = size(names)
          if (count .gt. 0) then
            call ESMF_StateTypeAddDataNameList(stypep, count, names, localrc)
            if (localrc .ne. ESMF_SUCCESS) then
              print *, "State construction error adding names"
              return
            endif
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
      type (ESMF_StateType), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateImpExpType), intent(in), optional :: statetype
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Construct a new empty {\tt State}.  The return value is a new {\tt State}.
!    
!      The arguments are:
!      \begin{description}
!       \item[{[stypep]}]
!       Internal StateType pointer.  Required.
!       \item[{[statetype]}]
!        Import or Export {\tt State}.  One of {\tt ESMF\_STATEIMPORT},
!        {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.  Default is 
!        {\tt ESMF\_STATELIST}.
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
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateConstructEmpty: BaseCreate failed"
          return
        endif

        ! Fill in basic information
        if (present(statetype)) then
          stypep%st = statetype
        else
          stypep%st = ESMF_STATELIST
        endif
        stypep%statestatus = ESMF_STATE_READY
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
      type(ESMF_StateType), pointer :: stypep
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt State}.
!
!     The arguments are:
!     \begin{description}
!     \item[stypep]
!       Destroy contents of this {\tt ESMF\_StateType}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! Local vars
        type(ESMF_StateData), pointer :: nextitem
        integer :: i
        integer :: localrc                   ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! mark object invalid, and free each of the blocks associated
        ! with each entry.  note that we are not freeing the objects
        ! themselves; they could be added to multiple states.  it is
        ! the user's responsibility to delete them when finished.
        stypep%st = ESMF_STATEINVALID
        stypep%statestatus = ESMF_STATE_INVALID
        if (stypep%datacount .gt. 0) then
          do i = 1, stypep%datacount
            ! free anything allocated here
            nextitem => stypep%datalist(i)
            if (associated(nextitem%datap)) then
              deallocate(nextitem%datap, stat=localrc)
              if (ESMF_LogMsgFoundAllocError(localrc, "data item", &
                                             ESMF_CONTEXT, rc)) return
              nullify(nextitem%datap)
            endif
          enddo
        endif
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
#define ESMF_METHOD "ESMF_StateTypeAddArrayList"
!BOPI
! !IROUTINE: ESMF_StateTypeAddArrayList - Add a list of Arrays to a StateType
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddArrayList(stypep, acount, arrays, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
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
!       Pointer to {\tt ESMF\_StateType}.
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
      type(ESMF_StateData), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: aname
      character(len=ESMF_MAXSTR) :: errmsg
      integer, allocatable, dimension(:) :: atodo
      integer :: i
      integer :: newcount, aindex
      logical :: exists
      logical :: dummy

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
      aname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (acount .le. 0) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "acount must be >= 0", &
                                     ESMF_CONTEXT, rc)
          return
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
        dummy=ESMF_LogMsgFoundAllocError(ESMF_RC_INTNRL_INCONS, &
                                         "atodo already allocated", &
                                         ESMF_CONTEXT, rc)
        deallocate(atodo, stat=localrc)
        return
      endif

      allocate(atodo(acount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(ESMF_RC_INTNRL_INCONS, &
                                     "adding Arrays to a State", &
                                     ESMF_CONTEXT, rc)) return

      atodo(1:acount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the array list.
      ! For each array...
      do i=1, acount

        call ESMF_ArrayValidate(arrays(i), "", localrc)
        if (localrc .ne. ESMF_SUCCESS) then
            print errmsg, "item", i
            dummy=ESMF_LogMsgFoundError(localrc, errmsg, &
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
        exists = ESMF_StateTypeFindData(stypep, aname, .false., &
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
            if (dataitem%otype .eq. ESMF_STATEDATANAME) then
                allocate(dataitem%datap, stat=localrc)
                if (ESMF_LogMsgFoundAllocError(localrc, "adding array over name", &
                                     ESMF_CONTEXT, rc)) then
                    deallocate(atodo, stat=localrc)
                    return
                endif
            endif

            dataitem%otype = ESMF_STATEARRAY
            dataitem%datap%ap = arrays(i)
        
            ! don't update flags on existing entry
            !dataitem%needed = ESMF_STATEDATAISNEEDED
            !dataitem%ready = ESMF_STATEDATAREADYTOREAD
            !dataitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateTypeExtendList(stypep, newcount, localrc)
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
            nextitem%otype = ESMF_STATEARRAY

            ! Add name
            call ESMF_ArrayGet(arrays(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, "getting name from array", &
                                      ESMF_CONTEXT, rc)) return

            allocate(nextitem%datap, stat=localrc)
            if (ESMF_LogMsgFoundAllocError(localrc, "adding array to state", &
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

      end subroutine ESMF_StateTypeAddArrayList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTypeAddFieldList"
!BOPI
! !IROUTINE: ESMF_StateTypeAddFieldList - Add a list of Fields to a StateType
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddFieldList(stypep, fcount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
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
!       Pointer to {\tt ESMF\_StateType}.
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
      type(ESMF_StateData), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: fname
      integer, allocatable, dimension(:) :: ftodo
      integer :: i
      integer :: newcount, findex
      logical :: exists
      logical :: dummy

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE 
      fname = ""

      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (fcount .le. 0) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "fcount must be >= 0", &
                                      ESMF_CONTEXT, rc)
          return
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
        print *, "ftodo already allocated"
        deallocate(ftodo, stat=localrc)
        if (localrc .ne. 0) then    ! F90 return code
          print *, "status = ", localrc
          print *, "Error: 1b deallocating fields to a state"
          return
        endif
      endif

      allocate(ftodo(fcount), stat=localrc)
      if (localrc .ne. 0) then    ! F90 return code
        print *, "status = ", localrc
        print *, "Error: 1 adding fields to a state"
        return
      endif
      ftodo(1:fcount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the field list.
      ! For each field...
      do i=1, fcount

        call ESMF_FieldValidate(fields(i), "", localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          print *, "Bad Field ", i
          deallocate(ftodo, stat=localrc)
          return
        endif
        call ESMF_FieldGet(fields(i), name=fname, rc=localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddFieldList: get field name"
          deallocate(ftodo, stat=localrc)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateTypeFindData(stypep, fname, .false., &
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
            if (dataitem%otype .eq. ESMF_STATEDATANAME) then
                allocate(dataitem%datap, stat=localrc)
                if (localrc .ne. 0) then    ! F90 return code
                  print *, "Error: 2 adding fields to a state"
                  deallocate(ftodo, stat=localrc)
                  return
                endif
            endif

            dataitem%otype = ESMF_STATEFIELD
            dataitem%datap%fp = fields(i)
        
            ! If we're replacing an existing item, then we shouldn't
            !  alter existing settings on the data state.
            !dataitem%needed = ESMF_STATEDATAISNEEDED
            !dataitem%ready = ESMF_STATEDATAREADYTOREAD
            !dataitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateTypeExtendList(stypep, newcount, localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_StateTypeAddFieldList: datalist allocate"
        return
      endif


      ! There is enough space now to add new fields to the list.
      ! This is the start of the second pass through the array list.
      do i=1, fcount

        ! If field wasn't already found in the list, we need to add it here.
        if (ftodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEFIELD

            ! Add name
            call ESMF_FieldGet(fields(i), name=nextitem%namep, rc=localrc)
            if (localrc .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddFieldList: get field name"
              return
            endif

            allocate(nextitem%datap, stat=localrc)
            if (localrc .ne. 0) then    ! F90 return code
              print *, "Error: 4 adding fields to a state"
              return
            endif
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
      if (localrc .ne. 0) then    ! F90 return code
        print *, "Error: 5 adding fields to a state"
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTypeAddBundleList"
!BOPI
! !IROUTINE: ESMF_StateTypeAddBundleList - Add a list of Bundles to a StateType
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddBundleList(stypep, bcount, bundles, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
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
!       Internal StateType pointer.  Required.
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
      type(ESMF_StateData), pointer :: nextitem, dataitem
      type(ESMF_Field) :: field
      character(len=ESMF_MAXSTR) :: bname, fname
      integer, allocatable, dimension(:) :: btodo, ftodo
      integer :: bindex, findex 
      integer :: i, j
      integer :: fcount, fruncount, newcount
      logical :: exists, fneedsdealloc
      logical :: dummy

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
      fneedsdealloc = .FALSE.
      fname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (bcount .le. 0) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "bcount must be >= 0", &
                                      ESMF_CONTEXT, rc)
          return
      endif
      
      ! Add the bundles to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! get a count of all fields in all bundles
      fruncount = 0
      do i=1, bcount
        print *, "calling bundle validate "
        call ESMF_BundleValidate(bundles(i), "", localrc)
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
        exists = ESMF_StateTypeFindData(stypep, bname, .false., &
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
            if (dataitem%otype .eq. ESMF_STATEDATANAME) then
                allocate(dataitem%datap, stat=localrc)
                if (ESMF_LogMsgFoundAllocError(localrc, &
                               "adding bundles to a state", &
                               ESMF_CONTEXT, rc)) goto 10
            endif

            dataitem%otype = ESMF_STATEBUNDLE
            dataitem%datap%bp = bundles(i)
        
            ! Don't change flags of existing entry
            !dataitem%needed = ESMF_STATEDATAISNEEDED
            !dataitem%ready = ESMF_STATEDATAREADYTOREAD
            !dataitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
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
    
            exists = ESMF_StateTypeFindData(stypep, fname, .false., dataitem, &
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
                if (dataitem%otype .ne. ESMF_STATEDATANAME) then
                  ! print *, "Warning: overwriting old entry"
                endif
   
                ! If there was previously associated data, deallocate it.
	        if (associated(dataitem%datap)) then
                    deallocate(dataitem%datap, stat=localrc)
                    if (ESMF_LogMsgFoundAllocError(localrc, &
                                                 "deallocating old object", &
                                                 ESMF_CONTEXT, rc)) goto 10
                    nullify(dataitem%datap)
                endif

                ! Set up the new entry.
                dataitem%otype = ESMF_STATEINDIRECT
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
      call ESMF_StateTypeExtendList(stypep, newcount, localrc)
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
            nextitem%otype = ESMF_STATEBUNDLE

            ! Add name
            call ESMF_BundleGet(bundles(i), name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            allocate(nextitem%datap, stat=localrc)
            if (ESMF_LogMsgFoundAllocError(localrc, &
                                          "adding bundles to a state",  &
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
            nextitem%otype = ESMF_STATEINDIRECT
    
            ! get next field and query name
            call ESMF_BundleGetField(bundles(i), i, field, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10

            call ESMF_FieldGet(field, name=nextitem%namep, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) goto 10
    
            nullify(nextitem%datap)
    
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
            exists = ESMF_StateTypeFindData(stypep, fname, .true., dataitem, &
                                                              findex, localrc)

            if (.not. exists) then
              dummy=ESMF_LogMsgFoundError(ESMF_RC_INTNRL_INCONS, &
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
        dummy=ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating internal bundlelist", &
                                         ESMF_CONTEXT, rc)
      endif
      if (fneedsdealloc) then
        deallocate(ftodo, stat=localrc)
        if ((localrc .ne. 0) .and. (rc .eq. ESMF_SUCCESS)) then      ! F90 rc
          dummy=ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating internal fieldlist", &
                                          ESMF_CONTEXT, rc)
        endif
      endif

      ! do not reset rc here - it should already have a valid value.

      end subroutine ESMF_StateTypeAddBundleList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTypeAddStateList"
!BOPI
! !IROUTINE: ESMF_StateTypeAddStateList - Add a list of States to a StateType
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddStateList(stypep, scount, states, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
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
!       Pointer to {\tt StateType}.
!     \item[scount]
!       The number of {\tt ESMF\_States} to be added.
!     \item[nestedstate]
!       The array of {\tt ESMF\_States} to be added.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status                   ! local error status
      type(ESMF_StateData), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: sname
      integer, allocatable, dimension(:) :: stodo
      integer :: i
      integer :: newcount, sindex
      logical :: exists
      logical :: dummy

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
      sname = ""
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (scount .le. 0) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "scount must be >= 0", &
                                     ESMF_CONTEXT, rc)
          return
      endif
      
      ! Add the states to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(stodo(scount), stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding states to a state"
        return
      endif
      stodo(1:scount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the state list.
      ! For each state...
      do i=1, scount

        call ESMF_StateValidate(states(i), "", status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Bad State ", i
          return
        endif
        call ESMF_StateGet(states(i), name=sname, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddStateList: get state name"
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateTypeFindData(stypep, sname, .false., dataitem, sindex, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddStateList: looking for preexisting entry"
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
            if (dataitem%otype .eq. ESMF_STATEDATANAME) then
                allocate(dataitem%datap, stat=status)
                if (status .ne. 0) then    ! F90 return code
                  print *, "Error: adding states to a state"
                  return
                endif
            endif

            dataitem%otype = ESMF_STATESTATE
            dataitem%datap%spp => states(i)%statep
        
            ! don't update flags on existing entry
            !dataitem%needed = ESMF_STATEDATAISNEEDED
            !dataitem%ready = ESMF_STATEDATAREADYTOREAD
            !dataitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary states first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateTypeExtendList(stypep, newcount, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_StateTypeAddStateList: datalist allocate"
        return
      endif


      ! There is enough space now to add new states to the list.
      ! This is the start of the second pass through the state list.
      do i=1, scount

        ! If state wasn't already found in the list, we need to add it here.
        if (stodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATESTATE

            ! Add name
            call ESMF_StateGet(states(i), name=nextitem%namep, rc=status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddStateList: get state name"
              return
            endif

            allocate(nextitem%datap, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding state to a state"
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
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding states to a state"
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddStateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTypeFindData"
!BOPI
! !IROUTINE: ESMF_StateTypeFindData - internal routine to find data item by name
!
! !INTERFACE:
      function ESMF_StateTypeFindData(stypep, dataname, expected, dataitem, &
                                                                     index, rc)
!
! !RETURN VALUE:
      logical :: ESMF_StateTypeFindData
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
      character (len=*), intent(in) :: dataname
      logical, intent(in) :: expected
      type(ESMF_StateData), pointer, optional :: dataitem
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
!       {\tt ESMF\_StateType} to query.
!      \item[dataname]
!       Name of the data item to query.
!      \item[expected]
!       Logical.  If set to {\tt true} the name must be found or an error code 
!       is set. The default is {\tt false} and the error code is not set if 
!       the name is not found.
!      \item[{[dataitem]}]
!       Pointer to the corresponding {\tt ESMF\_StateData} item if one is
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
      type(ESMF_StateData), pointer :: nextitem

      ! Initialize return code.  Assume failure until success assured.
      localrc = ESMF_FAILURE 
      if (present(rc)) rc = ESMF_FAILURE

      itemfound = .FALSE.
  
      ! Check for invalid state pointers
      if (.not.associated(stypep)) then
        print *, "Error: invalid or uninitialized state object"
        return
      endif
      if (stypep%statestatus .ne. ESMF_STATE_READY) then
        print *, "Error: invalid or uninitialized state object"
        return
      endif

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
        ESMF_StateTypeFindData = .TRUE.
        if (present(dataitem)) dataitem => stypep%datalist(itemindex) 
        if (present(index)) index = itemindex
        localrc = ESMF_SUCCESS
      else   ! item not found
        ESMF_StateTypeFindData = .FALSE.
        nullify(dataitem)
        if (expected) then 
          localrc = ESMF_FAILURE
        else
          localrc = ESMF_SUCCESS
        endif
      endif

      if (present(rc)) rc = localrc

      end function ESMF_StateTypeFindData

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTypeAddDataNameList"
!BOPI
! !IROUTINE: ESMF_StateTypeAddDataNameList - internal routine
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddDataNameList(stypep, ncount, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
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
!       Pointer to {\tt ESMF\_StateType}.
!     \item[name]
!       The name to be added.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                   ! local error status
      type(ESMF_StateData), pointer :: nextitem, dataitem
      integer, allocatable, dimension(:) :: ntodo
      integer :: i
      integer :: newcount, nindex
      logical :: exists, dummy

      ! Initialize return code.  Assume failure until success assured.
      if (present(rc)) rc = ESMF_FAILURE
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (ncount .le. 0) then
          dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, "ncount must be >= 0", &
                                      ESMF_CONTEXT, rc)
          return
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
        exists = ESMF_StateTypeFindData(stypep, namelist(i), .false., &
                                        dataitem, nindex, localrc)
        if (localrc .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddNameList: looking for preexisting entry"
          return
        endif
   
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
            if (dataitem%otype .ne. ESMF_STATEDATANAME) then
              if (associated(dataitem%datap)) then
                deallocate(dataitem%datap, stat=localrc)
                if (localrc .ne. 0) then    ! F90 return code
                  print *, "Error: removing an entry from a state"
                  return
                endif
                nullify(dataitem%datap)
              endif
            endif

            nextitem%otype = ESMF_STATEDATANAME
            ! don't have to add name, we already matched it.

            nullify(nextitem%datap)
            nextitem%indirect_index = -1

            nextitem%needed = stypep%needed_default
            nextitem%ready = stypep%ready_default
            nextitem%valid = stypep%stvalid_default
            nextitem%reqrestart = stypep%reqrestart_default
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateTypeExtendList(stypep, newcount, localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_StateTypeAddNameList: datalist allocate"
        return
      endif


      ! There is enough space now to add new names to the list.
      ! This is the start of the second pass through the array list.
      do i=1, ncount

        ! If name wasn't already found in the list, we need to add it here.
        if (ntodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEDATANAME

            ! Add name
            nextitem%namep = namelist(i)

            nullify(nextitem%datap)
 
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

      end subroutine ESMF_StateTypeAddDataNameList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateTypeExtendList"
!BOPI
! !IROUTINE: ESMF_StateTypeExtendList - internal routine
!
! !INTERFACE:
      subroutine ESMF_StateTypeExtendList(stypep, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), pointer :: stypep
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
!       Pointer to {\tt ESMF\_StateType}.
!     \item[itemcount]
!       The number of items that space is needed for.
!     \item[rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      type(ESMF_StateData), dimension(:), pointer :: temp_list
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

      end subroutine ESMF_StateTypeExtendList



      end module ESMF_StateMod

