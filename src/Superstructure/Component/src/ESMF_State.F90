! $Id: ESMF_State.F90,v 1.46 2003/09/09 20:47:31 nscollins Exp $
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
      use ESMF_IOMod
      use ESMF_ArrayBaseMod
      use ESMF_ArrayExpandMod
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
      sequence
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
      sequence
      private
        type(ESMF_StateObjectType) :: otype
        character(len=ESMF_MAXSTR), pointer :: namep
        type(ESMF_DataHolder), pointer :: datap
        integer :: indirect_index
        type(ESMF_StateDataNeeded) :: needed
        type(ESMF_StateDataReady) :: ready
        type(ESMF_StateDataValid) :: valid
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateType
!
!     ! Internal State data type.

      type ESMF_StateType
      sequence
      private
        type(ESMF_Base) :: base
        type(ESMF_StateImpExpType) :: st
        character (len=ESMF_MAXSTR) :: compname
        type(ESMF_StateDataValid) :: stvalid
        type(ESMF_StateDataReqRestart) :: reqrestart
        integer :: alloccount
        integer :: datacount
        type(ESMF_StateData), dimension(:), pointer :: datalist
      end type

!------------------------------------------------------------------------------
!     ! ESMF_State
!
!     ! State data type.

      type ESMF_State
      sequence
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

      public ESMF_StateAddData, ESMF_StateGetData
      !public ESMF_StateQueryData       ! returns ESMF type for this entry

      public ESMF_StateGetInfo, ESMF_StateGetName
      public ESMF_StateSetNeeded, ESMF_StateGetNeeded
      public ESMF_StateIsNeeded

      !public ESMF_StateGetNeededList   ! returns an array of values
      !public ESMF_State{Get/Set}Ready  ! is data ready
      !public ESMF_State{Get/Set}Valid  ! has data been validated?
      !public ESMF_State{Get/Set}CompName  ! normally set at create time

      public ESMF_StateTransform          ! execute xform on a state
      !public ESMF_StateTransformComplete ! is export state ok to update?
      ! TODO: this needs to be renamed.
      !public ESMF_StateValidate          ! is import state ready to read?
 
      public ESMF_StateWriteRestart
      public ESMF_StateReadRestart
 
      public ESMF_StatePrint, ESMF_StateValidate

      public operator(.eq.), operator(.ne.)
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_State.F90,v 1.46 2003/09/09 20:47:31 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddData -- Add Bundles, Fields, and Arrays to a State

! !INTERFACE:
     interface ESMF_StateAddData

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddBundle
        module procedure ESMF_StateAddBundleList
        module procedure ESMF_StateAddField
        module procedure ESMF_StateAddFieldList
        module procedure ESMF_StateAddArray
        module procedure ESMF_StateAddArrayList
        module procedure ESMF_StateAddState
        module procedure ESMF_StateAddStateList
        module procedure ESMF_StateAddDataName
        module procedure ESMF_StateAddDataNameList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddData} functions.   
!  
!EOP 
end interface


!------------------------------------------------------------------------------
!BOP
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
!EOP 
end interface


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
!------------------------------------------------------------------------------
!
! This section includes the State Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateCreate -- Create a new State

! !INTERFACE:
      function ESMF_StateCreate(statename, statetype, compname, &
                   bundles, fields, arrays, nestedstates, names, itemcount, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreate
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateImpExpType), intent(in), optional :: statetype
      character(len=*), intent(in), optional :: compname 
      type(ESMF_Bundle), dimension(:), intent(in), optional :: bundles
      type(ESMF_Field), dimension(:), intent(in), optional :: fields
      type(ESMF_Array), dimension(:), intent(in), optional :: arrays
      type(ESMF_State), dimension(:), intent(in), optional :: nestedstates
      character(len=*), dimension(:), intent(in), optional :: names
      integer, intent(in), optional :: itemcount
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new State and set the decomposition characteristics.
!
!  The return value is a new State.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[{[statename]}]
!    Name of this {\tt State} object.   A default name will be generated
!     if none is specified.
!
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Values can be {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.  The default is
!    {\tt ESMF\_STATELIST}.
!
!   \item[{[compname]}]
!    Name of the {\tt Component} this {\tt State} is associated with.
!
!   \item[{[bundles]}]
!    An array of Bundles.
!
!   \item[{[fields]}]
!    An array of Fields.
!
!   \item[{[arrays]}]
!    An array of Arrays.
!
!   \item[{[nestedstates]}]
!    An array of States to be nested inside the outer {\tt State}.
!
!   \item[{[names]}]
!    An array of name placeholders.
!
!   \item[{[itemcount]}]
!    The total number of Bundles, Fields, Arrays, States, and Names to be added.
!    If not specified the lengths of each array will be used to compute the
!    actual number of items added to the State.  If the count is specified
!    it will do an error check to verify the total number of items found
!    in the argument lists matches this count of the expected number of items.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_StateType), pointer :: stypep
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE 
        rcpresent = .FALSE.

!       Initialize the pointers to null.
        nullify(ESMF_StateCreate%statep)
        nullify(stypep)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        allocate(stypep, stat=status)
        if(status .NE. 0) then     ! this is an F90 rc, not esmf's
          print *, "ERROR in ESMF_StateCreateEmpty: allocation"
          return
        endif
      
        call ESMF_StateConstruct(stypep, statename, statetype, compname, &
               bundles, fields, arrays, nestedstates, names, itemcount, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif

!       set return values
        ESMF_StateCreate%statep => stypep 
        if (rcpresent) rc = status

        end function ESMF_StateCreate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateDestroy -- Release resources for this State
!
! !INTERFACE:
      subroutine ESMF_StateDestroy(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt State}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[state]
!       Destroy contents of this {\tt State}.
!
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

        ! Local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE 
        rcpresent = .FALSE.


        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Simple sanity checks
        if (.not.associated(state%statep)) then
          print *, "calling destroy on uninitialized or already destroyed state"
          return
        endif
        if (state%statep%st .eq. ESMF_STATEINVALID) then
          print *, "calling destroy on uninitialized or already destroyed state"
          return
        endif

        ! Call Destruct to release resources
        call ESMF_StateDestruct(state%statep, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State contents destruction error"
          return
        endif

        ! Release space
 	deallocate(state%statep, stat=status)
        if (status .ne. 0) then    ! F90 return code
          print *, "State contents destruction error"
          return
        endif
        nullify(state%statep)

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestroy



!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateConstruct -- Construct a new State

! !INTERFACE:
      subroutine ESMF_StateConstruct(stypep, statename, statetype, compname, & 
                         bundles, fields, arrays, states, names, itemcount, rc)
!
! !ARGUMENTS:
      type (ESMF_StateType), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateImpExpType), intent(in), optional :: statetype
      character(len=*), intent(in), optional :: compname 
      type(ESMF_Bundle), dimension(:), intent(in), optional :: bundles
      type(ESMF_Field), dimension(:), intent(in), optional :: fields
      type(ESMF_Array), dimension(:), intent(in), optional :: arrays
      type(ESMF_State), dimension(:), intent(in), optional :: states
      character(len=*), dimension(:), intent(in), optional :: names
      integer, intent(in), optional :: itemcount
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Construct a new State and set the decomposition characteristics.
!
!  The return value is a new State.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[stypep]
!    Internal StateType pointer.  Required.
!
!   \item[{[statename]}]
!    Name of this {\tt State} object. 
!
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Should be one of {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.   
!    {\tt ESMF\_STATELIST} is the default if not specified.
!
!   \item[{[compname]}]
!    Name of the {\tt Component} this {\tt State} is associated with.
!
!   \item[{[bundles]}]
!    An array of {\tt Bundles}.
!
!   \item[{[fields]}]
!    An array of {\tt Fields}.
!
!   \item[{[arrays]}]
!    An array of {\tt Arrays}.
!
!   \item[{[states]}]
!    An array of nested {\tt States}.
!
!   \item[{[names]}]
!    An array of name placeholders.
!
!   \item[{[itemcount]}]
!    The total number of Bundles, Fields, Arrays, States, and Names specified.
!    This argument is optional, and if specified is used as an error check
!    to verify that the actual total number of items found matches this count.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI
! !REQUIREMENTS:


        ! Local vars
        integer :: count
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE 
        rcpresent = .FALSE.


        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
  
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
        call ESMF_StateConstructEmpty(stypep, statename, statetype, &
                                                             compname, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif

        ! Set the initial size of the datalist
        call ESMF_StateTypeExtendList(stypep, count, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif
      
        ! For each item type, set the data values.  All the allocation 
        !  has already been done.
        if (present(bundles)) then
          count = size(bundles)
          if (count .gt. 0) then
            call ESMF_StateTypeAddBundleList(stypep, count, bundles, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "State construction error adding bundles"
              return
            endif
          endif
        endif

        if (present(fields)) then
          count = size(fields)
          if (count .gt. 0) then
            call ESMF_StateTypeAddFieldList(stypep, count, fields, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "State construction error adding fields"
              return
            endif
          endif
        endif

        if (present(arrays)) then
          count = size(arrays)
          if (count .gt. 0) then
            call ESMF_StateTypeAddArrayList(stypep, count, arrays, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "State construction error adding arrays"
              return
            endif
          endif
        endif

        if (present(states)) then
          count = size(states)
          if (count .gt. 0) then
            call ESMF_StateTypeAddStateList(stypep, count, states, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "State construction error adding states"
              return
            endif
          endif
        endif

        if (present(names)) then
          count = size(names)
          if (count .gt. 0) then
            call ESMF_StateTypeAddDataNameList(stypep, count, names, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "State construction error adding names"
              return
            endif
          endif
        endif


!       ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateConstruct

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateConstructEmpty -- Create a new State specifying no data

! !INTERFACE:
      subroutine ESMF_StateConstructEmpty(stypep, statename, statetype, &
                                                               compname, rc)
!
! !ARGUMENTS:
      type (ESMF_StateType), pointer :: stypep
      character(len=*), intent(in), optional :: statename 
      type(ESMF_StateImpExpType), intent(in), optional :: statetype
      character(len=*), intent(in), optional :: compname 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Construct a new empty {\tt State}.  The return value is a new {\tt State}.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[{[stypep]}]
!    Pointer to an internal StateType derived type.
!
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  One of {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.  Default is 
!    {\tt ESMF\_STATELIST}.
!
!   \item[{[statename]}]
!    Name of this {\tt State} object.  Optional.  If a name is not
!    specified one will be generated.
!
!   \item[{[compname]}]
!    Name of the {\tt Component} this {\tt State} is associated with.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI
! !REQUIREMENTS:


        ! Local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE 
        rcpresent = .FALSE.


        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Initialize the base object.
        call ESMF_BaseInit(stypep%base)

        ! Set statename on base object
        call ESMF_SetName(stypep%base, statename, "States", status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateConstructEmpty: SetName"
          return
        endif

        ! Fill in basic information
        if (present(statetype)) then
          stypep%st = statetype
        else
          stypep%st = ESMF_STATELIST
        endif
        if (present(compname)) then
          stypep%compname = compname
        else
          stypep%compname = "no component name specified"
        endif
        stypep%stvalid = ESMF_STATEDATAISVALID
        stypep%alloccount = 0
        stypep%datacount = 0
        nullify(stypep%datalist)

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateConstructEmpty

!------------------------------------------------------------------------------
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
!
!     \item[stypep]
!       Destroy contents of this {\tt StateType}.
!
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

!       local vars
        type(ESMF_StateData), pointer :: nextitem
        integer :: i
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE 
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! TODO: add code here
        stypep%st = ESMF_STATEINVALID
        stypep%compname = ""
        stypep%stvalid = ESMF_STATEDATAINVALID
        if (stypep%datacount .gt. 0) then
          do i = 1, stypep%datacount
            ! free anything allocated here
            nextitem => stypep%datalist(i)
            if (associated(nextitem%datap)) then
              deallocate(nextitem%datap, stat=status)
              if(status .NE. 0) then     ! this is an F90 rc, not esmf's
                 print *, "ERROR in ESMF_StateDestruct: datap deallocation"
                 return
              endif
              nullify(nextitem%datap)
            endif
            if (associated(nextitem%namep)) then
              deallocate(nextitem%namep, stat=status)
              if(status .NE. 0) then     ! this is an F90 rc, not esmf's
                 print *, "ERROR in ESMF_StateDestruct: namep deallocation"
                 return
              endif
              nullify(nextitem%namep)
            endif
          enddo
        endif
        stypep%datacount = 0

        ! Now release the entire list
        if (associated(stypep%datalist)) then
          deallocate(stypep%datalist, stat=status)
          if(status .NE. 0) then     ! this is an F90 rc, not esmf's
            print *, "ERROR in ESMF_StateDestruct: deallocation"
            return
          endif
          nullify(stypep%datalist)
        endif
        stypep%alloccount = 0

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestruct


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Add data (bundles, fields, arrays, or names) to the state.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddBundle - Add a Bundle to a State.
!
! !INTERFACE:
      subroutine ESMF_StateAddBundle(state, bundle, rc)
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
! !REQUIREMENTS: 
!EOP
      type(ESMF_Bundle) :: temp_list(1)

      temp_list(1) = bundle

      call ESMF_StateTypeAddBundleList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddBundleList - Add a list of Bundles to a State
!
! !INTERFACE:
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
!EOP
! !REQUIREMENTS:

        call ESMF_StateTypeAddBundleList(state%statep, bcount, bundles, rc)

        end subroutine ESMF_StateAddBundleList

!------------------------------------------------------------------------------
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
!      Add multiple bundles to a {\tt State}.  Internal routine only.
!
!EOPI
! !REQUIREMENTS:

      integer :: status                   ! local error status
      logical :: rcpresent                ! did user specify rc?
      type(ESMF_StateData), pointer :: nextitem, dataitem
      type(ESMF_Field) :: field
      character(len=ESMF_MAXSTR) :: bname, fname
      integer, allocatable, dimension(:) :: btodo, ftodo
      integer :: bindex, findex 
      integer :: i, j
      integer :: fcount, fruncount, newcount
      logical :: exists, fneedsdealloc

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE 
      rcpresent = .FALSE.
      fneedsdealloc = .FALSE.
      fname = ""
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (bcount .le. 0) return
      
      ! Add the bundles to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! get a count of all fields in all bundles
      fruncount = 0
      do i=1, bcount
        print *, "calling bundle validate "
        call ESMF_BundleValidate(bundles(i), "", status)
        print *, "status is ", status
        if (status .ne. ESMF_SUCCESS) then
          print *, "Invalid bundle ", i
          return
        endif
        call ESMF_BundleGetFieldCount(bundles(i), fcount, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Cannot get Field count from bundle ", i
          return
        endif
        fruncount = fruncount + fcount
      enddo

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(btodo(bcount), stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding bundles to a state"
        return
      endif
      btodo(1:bcount) = 0

      if (fruncount .ge. 0) then
        allocate(ftodo(fruncount), stat=status)
        if (status .ne. 0) then    ! F90 return code
          print *, "Error: adding bundles to a state"
          return
        endif
        fneedsdealloc = .TRUE.
        ftodo(1:fruncount) = 0
      endif

  
      ! Initialize counters to 0, indices to 1
      fruncount = 1
      newcount = 0

      ! This is the start of the first pass through the bundle list.
      ! For each bundle...
      do i=1, bcount

        call ESMF_BundleGetName(bundles(i), bname, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddBundleList: get bundle name"
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateTypeFindData(stypep, bname, .false., dataitem, bindex, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddBundleList: looking for preexisting entry"
          return
        endif
   
        ! If not, in the second pass we will need to add it.
        if (.not. exists) then
            newcount = newcount + 1
            bindex = -1
            btodo(i) = 1
        else
            ! It does already exist.  
            ! Check to see if this is a placeholder, and if so, replace it
            if (dataitem%otype .eq. ESMF_STATEDATANAME) then
                allocate(dataitem%datap, stat=status)
                if (status .ne. 0) then    ! F90 return code
                  print *, "Error: adding bundles to a state"
                  return
                endif
            endif

            dataitem%otype = ESMF_STATEBUNDLE
            dataitem%datap%bp = bundles(i)
        
            ! Don't change flags of existing entry
            !dataitem%needed = ESMF_STATEDATAISNEEDED
            !dataitem%ready = ESMF_STATEDATAREADYTOREAD
            !dataitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
        endif

        ! and now the same for each field in the bundle
        call ESMF_BundleGetFieldCount(bundles(i), fcount, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddBundleList: get field from bundle"
          return
        endif
    

        do j=1, fcount
            ! get next field and query name
            call ESMF_BundleGetFields(bundles(i), j, field, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddBundleList: get field from bundle"
              return
            endif

            call ESMF_FieldPrint(field, "", status)

            call ESMF_FieldGetName(field, fname, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddBundleList: get name from field"
              return
            endif
    
            exists = ESMF_StateTypeFindData(stypep, fname, .false., dataitem, &
                                                              findex, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddBundleList: get field from bundle"
              return
            endif

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
                    deallocate(dataitem%datap, stat=status)
                    if (status .ne. 0) then    ! F90 return code
                      print *, "Error: adding bundles to a state"
                      return
                    endif
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
            
                dataitem%needed = ESMF_STATEDATAISNEEDED
                dataitem%ready = ESMF_STATEDATAREADYTOREAD
                dataitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
            endif

            ! This is a total running count of all fields in all bundles.
            fruncount = fruncount+1
    
        enddo
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateTypeExtendList(stypep, newcount, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_StateTypeAddBundleList: datalist allocate"
        return
      endif


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
            allocate(nextitem%namep, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding bundles to a state"
              return
            endif

            call ESMF_BundleGetName(bundles(i), nextitem%namep, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddBundleList: get bundle name"
              return
            endif

            allocate(nextitem%datap, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding bundles to a state"
              return
            endif
            nextitem%datap%bp = bundles(i)
 
            nextitem%needed = ESMF_STATEDATAISNEEDED
            nextitem%ready = ESMF_STATEDATAREADYTOREAD
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
 
            ! save the current datalist index for the fields code below
            bindex = stypep%datacount

        endif

        ! Whether it was found in pass 1 or just added above, we still
        !  have to go through each field and see if any of them need to
        !  be added or updated.
        call ESMF_BundleGetFieldCount(bundles(i), fcount, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddBundleList: get field count from bundle"
          return
        endif

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
            call ESMF_BundleGetFields(bundles(i), i, field, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddBundleList: get field from bundle"
              return
            endif

            allocate(nextitem%namep, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding field pointers to a state"
              return
            endif
            call ESMF_FieldGetName(field, nextitem%namep)
    
            nullify(nextitem%datap)
    
            ! If we found the corresponding bundle entry during pass 1,
            ! it was stored in the todo list.  Otherwise, we just added it
            ! above and we saved the new index to use here.
            if (ftodo(fruncount) .ge. 0) then
                nextitem%indirect_index = ftodo(fruncount)
            else
                nextitem%indirect_index = bindex
            endif

            nextitem%needed = ESMF_STATEDATAISNEEDED
            nextitem%ready = ESMF_STATEDATAREADYTOREAD
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN

          ! If the field entry already existed but needs bundle index updated,
          !  we do have to do a lookup on the field to see where it was
          !  found.  We just added the bundle above, so bindex is the
          !  value to set.
          else if (ftodo(fruncount) .eq. -2) then
            exists = ESMF_StateTypeFindData(stypep, fname, .true., dataitem, &
                                                              findex, status)

            if (.not. exists) then
              print *, "Error: internally inconsistent"
            endif
            dataitem%indirect_index = bindex
          endif
  
          ! Update the running count.
          fruncount = fruncount+1
        enddo

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag arrays
      deallocate(btodo, stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding bundles to a state"
        return
      endif
      if (fneedsdealloc) then
        deallocate(ftodo, stat=status)
        if (status .ne. 0) then    ! F90 return code
          print *, "Error: adding bundles to a state"
          return
        endif
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddBundleList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddField - Add a Field to a State.
!
! !INTERFACE:
      subroutine ESMF_StateAddField(state, field, rc)
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
! !REQUIREMENTS: 
!EOP
      type(ESMF_Field) :: temp_list(1)

      temp_list(1) = field

      call ESMF_StateTypeAddFieldList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddFieldList - Add a list of Fields to a State
!
! !INTERFACE:
      subroutine ESMF_StateAddFieldList(state, fcount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: fcount
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple fields to a {\tt State}.
!
!EOP
! !REQUIREMENTS:

      call ESMF_StateTypeAddFieldList(state%statep, fcount, fields, rc)

      end subroutine ESMF_StateAddFieldList

!------------------------------------------------------------------------------
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
!EOPI
! !REQUIREMENTS:

      integer :: status                   ! local error status
      logical :: rcpresent                ! did user specify rc?
      type(ESMF_StateData), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: fname
      integer, allocatable, dimension(:) :: ftodo
      integer :: i, j
      integer :: newcount, findex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE 
      rcpresent = .FALSE.
      
      fname = ""
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (fcount .le. 0) return
      
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
        deallocate(ftodo, stat=status)
        if (status .ne. 0) then    ! F90 return code
          print *, "status = ", status
          print *, "Error: 1b deallocating fields to a state"
          return
        endif
      endif

      allocate(ftodo(fcount), stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "status = ", status
        print *, "Error: 1 adding fields to a state"
        return
      endif
      ftodo(1:fcount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the field list.
      ! For each field...
      do i=1, fcount

        call ESMF_FieldValidate(fields(i), "", status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Bad Field ", i
          deallocate(ftodo, stat=status)
          return
        endif
        call ESMF_FieldGetName(fields(i), fname, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddFieldList: get field name"
          deallocate(ftodo, stat=status)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateTypeFindData(stypep, fname, .false., dataitem, findex, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddFieldList: looking for preexisting entry"
          deallocate(ftodo, stat=status)
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
                allocate(dataitem%datap, stat=status)
                if (status .ne. 0) then    ! F90 return code
                  print *, "Error: 2 adding fields to a state"
                  deallocate(ftodo, stat=status)
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
      call ESMF_StateTypeExtendList(stypep, newcount, status)
      if (status .ne. ESMF_SUCCESS) then
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
            allocate(nextitem%namep, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: 3 adding fields to a state"
              return
            endif

            call ESMF_FieldGetName(fields(i), nextitem%namep, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddFieldList: get field name"
              return
            endif

            allocate(nextitem%datap, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: 4 adding fields to a state"
              return
            endif
            nextitem%datap%fp = fields(i)
 
            nextitem%needed = ESMF_STATEDATAISNEEDED
            nextitem%ready = ESMF_STATEDATAREADYTOREAD
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag array
      deallocate(ftodo, stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: 5 adding fields to a state"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddFieldList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddArray - Add a Array to a State.
!
! !INTERFACE:
      subroutine ESMF_StateAddArray(state, array, rc)
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
! !REQUIREMENTS: 
!EOP
      type(ESMF_Array) :: temp_list(1)

      temp_list(1) = array

      call ESMF_StateTypeAddArrayList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddArrayList - Add a list of Arrays to a State
!
! !INTERFACE:
      subroutine ESMF_StateAddArrayList(state, acount, arrays, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: acount
      type(ESMF_Array), dimension(:), intent(in) :: arrays
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple arrays to a {\tt State}.
!
!EOP
! !REQUIREMENTS:

        call ESMF_StateTypeAddArrayList(state%statep, acount, arrays, rc)

        end subroutine ESMF_StateAddArrayList

!------------------------------------------------------------------------------
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
!      Add multiple arrays to a {\tt State}.  Internal routine only.
!
!EOPI
! !REQUIREMENTS:


      integer :: status                   ! local error status
      logical :: rcpresent                ! did user specify rc?
      type(ESMF_StateData), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: aname
      integer, allocatable, dimension(:) :: atodo
      integer :: i, j
      integer :: newcount, aindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE 
      rcpresent = .FALSE.
      
      aname = ""
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (acount .le. 0) return
      
      ! Add the arrays to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.

      ! How does this happen?  is atodo some sort of static?
      if (allocated(atodo)) then
        print *, "atodo already allocated"
        deallocate(atodo, stat=status)
        if (status .ne. 0) then    ! F90 return code
          print *, "status = ", status
          print *, "Error: 1b deallocating arrays to a state"
          return
        endif
      endif

      allocate(atodo(acount), stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding arrays to a state"
        return
      endif
      atodo(1:acount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the array list.
      ! For each array...
      do i=1, acount

        call ESMF_ArrayValidate(arrays(i), "", status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Bad Array ", i
          deallocate(atodo, stat=status)
          return
        endif
        call ESMF_ArrayGetName(arrays(i), aname, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddArrayList: get array name"
          deallocate(atodo, stat=status)
          return
        endif
    
        ! See if this name is already in the state
        exists = ESMF_StateTypeFindData(stypep, aname, .false., dataitem, aindex, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_StateTypeAddArrayList: looking for preexisting entry"
          deallocate(atodo, stat=status)
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
                allocate(dataitem%datap, stat=status)
                if (status .ne. 0) then    ! F90 return code
                  print *, "Error: adding arrays to a state"
                  deallocate(atodo, stat=status)
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
      call ESMF_StateTypeExtendList(stypep, newcount, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_StateTypeAddArrayList: datalist allocate"
        return
      endif


      ! There is enough space now to add new arrays to the list.
      ! This is the start of the second pass through the array list.
      do i=1, acount

        ! If array wasn't already found in the list, we need to add it here.
        if (atodo(i) .eq. 1) then
            stypep%datacount = stypep%datacount + 1

            nextitem => stypep%datalist(stypep%datacount)
            nextitem%otype = ESMF_STATEARRAY

            ! Add name
            allocate(nextitem%namep, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding arrays to a state"
              return
            endif

            call ESMF_ArrayGetName(arrays(i), nextitem%namep, status)
            if (status .ne. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_StateTypeAddArrayList: get array name"
              return
            endif

            allocate(nextitem%datap, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding array to a state"
              return
            endif
            nextitem%datap%ap = arrays(i)
 
            nextitem%needed = ESMF_STATEDATAISNEEDED
            nextitem%ready = ESMF_STATEDATAREADYTOREAD
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag arrays
      deallocate(atodo, stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding arrays to a state"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddArrayList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddState - Add a State to a State.
!
! !INTERFACE:
      subroutine ESMF_StateAddState(state, nestedstate, rc)
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
! !REQUIREMENTS: 
!EOP
      type(ESMF_State) :: temp_list(1)

      temp_list(1) = nestedstate

      call ESMF_StateTypeAddStateList(state%statep, 1, temp_list, rc)      

      end subroutine ESMF_StateAddState

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddStateList - Add a list of States to a State
!
! !INTERFACE:
      subroutine ESMF_StateAddStateList(state, scount, nestedstates, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      integer, intent(in) :: scount
      type(ESMF_State), dimension(:), intent(in) :: nestedstates
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple states to a {\tt State}.
!
!EOP
! !REQUIREMENTS:

        call ESMF_StateTypeAddStateList(state%statep, scount, nestedstates, rc)

        end subroutine ESMF_StateAddStateList

!------------------------------------------------------------------------------
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
!EOPI
! !REQUIREMENTS:


      integer :: status                   ! local error status
      logical :: rcpresent                ! did user specify rc?
      type(ESMF_StateData), pointer :: nextitem, dataitem
      character(len=ESMF_MAXSTR) :: sname
      integer, allocatable, dimension(:) :: stodo
      integer :: i, j
      integer :: newcount, sindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE 
      rcpresent = .FALSE.
      
      sname = ""
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (scount .le. 0) return
      
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
        call ESMF_StateGetName(states(i), sname, status)
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
            allocate(nextitem%namep, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: adding states to a state"
              return
            endif

            call ESMF_StateGetName(states(i), nextitem%namep, status)
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
 
            nextitem%needed = ESMF_STATEDATAISNEEDED
            nextitem%ready = ESMF_STATEDATAREADYTOREAD
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
 
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

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddStateList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddDataName - Add a Name as placeholder to a State.
!
! !INTERFACE:
      subroutine ESMF_StateAddDataName(state, name, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      character (len=*), intent(in) :: name
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!      Add a {\tt name} to an existing {\tt State}.
!      The {\tt name} must be unique within the {\tt State}
!      It is available to be marked {\tt needed} by the
!      consumer of the export {\tt State}. Then the data 
!      provider can replace the name with the actual {\tt Bundle},
!      {\tt Field}, or {\tt Array} which carries the needed data.
!
! !REQUIREMENTS: 
!EOP
      character(len=ESMF_MAXSTR) :: temp_list(1)
      
      temp_list(1) = name

      call ESMF_StateAddDataNameList(state, 1, temp_list, rc)      

      end subroutine ESMF_StateAddDataName

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddDataNameList - Add a list of Names as placeholder to a State.
!
! !INTERFACE:
      subroutine ESMF_StateAddDataNameList(state, namecount, namelist, rc)
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
! !REQUIREMENTS: 
!EOP
      call ESMF_StateTypeAddDataNameList(state%statep, namecount, namelist, rc)      

      end subroutine ESMF_StateAddDataNameList

!------------------------------------------------------------------------------
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
! !REQUIREMENTS: 
!EOPI
      integer :: status                   ! local error status
      logical :: rcpresent                ! did user specify rc?
      type(ESMF_StateData), pointer :: nextitem, dataitem
      integer, allocatable, dimension(:) :: ntodo
      integer :: i, j
      integer :: newcount, nindex
      logical :: exists

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE 
      rcpresent = .FALSE.
      
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
  
      ! Return with error if list is empty.  
      ! TODO: decide if this should *not* be an error.
      if (ncount .le. 0) return
      
      ! Add the fields to the state, checking for name clashes
      !  and name placeholders

      ! TODO: check for existing name, if placeholder, replace it
      !       if existing object - what?  replace it silently?

      ! Allocate some flags to mark whether this is a new item which
      !  needs to be added to the end of the list, or if it replaces an
      !  existing entry or placeholder.  Set all entries to 0.
      allocate(ntodo(ncount), stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding names to a state"
        return
      endif
      ntodo(1:ncount) = 0

      ! Initialize counters to 0, indices to 1
      newcount = 0

      ! This is the start of the first pass through the names list.
      ! For each name...
      do i=1, ncount

        ! See if this name is already in the state
        exists = ESMF_StateTypeFindData(stypep, namelist(i), .false., dataitem, nindex, status)
        if (status .ne. ESMF_SUCCESS) then
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
                deallocate(dataitem%datap, stat=status)
                if (status .ne. 0) then    ! F90 return code
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
            nextitem%needed = ESMF_STATEDATANOTNEEDED
            nextitem%ready = ESMF_STATEDATANOTREADY
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
        endif
      enddo

      ! If all things to be added are replacing existing entries, 
      !  we are done now.  But this cannot be a simple return here;
      !  we have to delete the temporary arrays first.  Go to the subr end.
      if (newcount .eq. 0) goto 10

      ! We now know how many total new items need to be added
      call ESMF_StateTypeExtendList(stypep, newcount, status)
      if (status .ne. ESMF_SUCCESS) then
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
            allocate(nextitem%namep, stat=status)
            if (status .ne. 0) then    ! F90 return code
              print *, "Error: 6 adding fields to a state"
              return
            endif

            nextitem%namep = namelist(i)

            nullify(nextitem%datap)
 
            nextitem%needed = ESMF_STATEDATANOTNEEDED
            nextitem%ready = ESMF_STATEDATANOTREADY
            nextitem%valid = ESMF_STATEDATAVALIDITYUNKNOWN
 
        endif

      enddo

      ! We come here from above if there were no new entries that needed
      ! to be added.  We can just clean up and exit.
10    continue

      ! Get rid of temp flag array
      deallocate(ntodo, stat=status)
      if (status .ne. 0) then    ! F90 return code
        print *, "Error: adding names to a state"
        return
      endif


      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddDataNameList

!------------------------------------------------------------------------------
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
! !REQUIREMENTS: 
!EOPI

      type(ESMF_StateData), dimension(:), pointer :: temp_list
      type(ESMF_StateData), pointer :: nextitem
      integer :: i
      integer :: allocsize 
      integer :: newsize
      integer :: status                            ! local error status
      integer, parameter :: chunksize = 16         ! extend list by this
 
      ! Assume failure until success assured.
      status = ESMF_FAILURE
      rc = ESMF_FAILURE

      ! Not an error to be called with 0 items - just return w/o error.
      if (itemcount .le. 0) then
          rc = ESMF_SUCCESS
          return
      endif

      ! An initially empty list. Simply allocate, no data copy needed.
      if (stypep%alloccount .eq. 0) then

          allocsize = itemcount + chunksize - mod(itemcount,chunksize)
          allocate(stypep%datalist(allocsize), stat=status)
          if(status .NE. 0) then
            print *, "ERROR in ESMF_StateTypeExtendList: datalist allocate"
            return
          endif
          stypep%alloccount = allocsize

      ! Extend an existing list to the right length, including copy
      else if (stypep%alloccount .lt. stypep%datacount + itemcount) then

          newsize = stypep%datacount + itemcount
          allocsize = newsize + chunksize - mod(newsize,chunksize)
          allocate(temp_list(allocsize), stat=status)
          if(status .NE. 0) then
            print *, "ERROR in ESMF_StateTypeExtendList: datalist reallocate"
            return
          endif
  
          ! Preserve old contents
          do i = 1, stypep%datacount
            temp_list(i) = stypep%datalist(i)
          enddo
  
          ! Delete old list
          deallocate(stypep%datalist, stat=status)
          if(status .NE. 0) then
            print *, "ERROR in ESMF_StateTypeExtendList: datalist deallocate"
            return
          endif
  
          ! Now make this the permanent list
          stypep%datalist => temp_list

          stypep%alloccount = allocsize

      endif
   
      rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeExtendList

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the state.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetInfo -- Get information about a State
!
! !INTERFACE:
      subroutine ESMF_StateGetInfo(state, statename, statetype, compname, &
                                     itemcount, itemnames, objtypes, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(out), optional :: statename
      type(ESMF_StateImpExpType), intent(out), optional :: statetype
      character (len=*), intent(out), optional :: compname
      integer, intent(out), optional :: itemcount
      character (len=*), intent(out), optional :: itemnames(:)
      type(ESMF_StateObjectType), intent(out), optional :: objtypes(:)
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the requested information about this {\tt State}.
!
!  \begin{description}     
!
!  \item[state]
!    {\tt State} to query.
!
!   \item[{[statename]}]
!    Name of this {\tt State}.
!
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Returns either {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATELIST}.
!
!   \item[{[compname]}]
!    Name of the {\tt Component} this {\tt State} is associated with.
!
!   \item[{[itemcount]}]
!    Count of items in this {\tt State}, including placeholder names.
!
!   \item[{[itemnames]}]
!    Array of item names in this {\tt State}, including placeholder names,
!    {\tt itemcount} long.
!
!   \item[{[objtypes]}]
!    Array of item object types in this {\tt state}, including placeholder 
!    names, {\tt itemcount} long.  State object types include
!    {\tt ESMF\_STATEBUNDLE}, {\tt ESMF\_STATEFIELD}, {\tt ESMF\_STATEARRAY}, 
!    {\tt ESMF\_STATESTATE}, {\tt ESMF\_STATEDATANAME}, or
!    {\tt ESMF\_STATEOBJTYPEUNKNOWN}.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      integer :: i, status
      type(ESMF_StateType), pointer :: stypep
      type(ESMF_StateData), pointer :: nextitem

      if (.not.associated(state%statep)) then
        print *, "Error: uninitialized or invalid State"
        if (present(rc)) rc=ESMF_FAILURE
        return
      endif
      stypep => state%statep
      if (stypep%stvalid .eq. ESMF_STATEDATAINVALID) then
        print *, "Error: uninitialized or invalid State"
        if (present(rc)) rc=ESMF_FAILURE
        return
      endif

      if (present(statename)) call ESMF_GetName(stypep%base, statename, status)
      if (present(statetype)) statetype = stypep%st
      if (present(compname)) compname = stypep%compname

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

      end subroutine ESMF_StateGetInfo


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetName -- Get the name of a State
!
! !INTERFACE:
      subroutine ESMF_StateGetName(state, statename, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(out) :: statename
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the name of this {\tt State}.
!
!  \begin{description}     
!  \item[state]
!    {\tt State} to query.
!   \item[statename]
!    State Name.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      integer :: status
      type(ESMF_StateType), pointer :: stypep

      stypep => state%statep
      if (.not. associated(stypep)) then
        if (present(rc)) rc=ESMF_FAILURE
        return
      endif

      if (stypep%st .eq. ESMF_STATEINVALID) then
        if (present(rc)) rc=ESMF_FAILURE
        return
      endif

      call ESMF_GetName(stypep%base, statename, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "cannot get name from State"
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateGetName


!------------------------------------------------------------------------------
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
!  \begin{description}     
!  \item[stypep]
!    {\tt StateType} to query.
!   \item[dataname]
!    Name of the data item to query.
!   \item[expected]
!    Logical.  If set to {\tt true} the name must be found or an error code 
!    is set. The default is {\tt false} and the error code is not set if 
!    the name is not found.
!   \item[{[dataitem]}]
!    Pointer to the corresponding {\tt ESMF\_StateData} item if one is
!    found with the right name.
!   \item[{[index]}]
!    Index number in datalist where this name was found.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      integer :: status                   ! local error status
      logical :: rcpresent                ! did user specify rc?
      integer :: i, dcount, itemindex
      logical :: itemfound
      type(ESMF_StateData), pointer :: nextitem

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE 
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      itemfound = .FALSE.
  
      ! Check for invalid state pointers
      if (.not.associated(stypep)) then
        print *, "Error: invalid or uninitialized state object"
        return
      endif
      if (stypep%stvalid .eq. ESMF_STATEDATAINVALID) then
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
        status = ESMF_SUCCESS
      else   ! item not found
        ESMF_StateTypeFindData = .FALSE.
        nullify(dataitem)
        if (expected) then 
          status = ESMF_FAILURE
        else
          status = ESMF_SUCCESS
        endif
      endif

      if(rcpresent) rc = status

      end function ESMF_StateTypeFindData

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateIsNeeded -- Return logical true if state needed
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
!  \begin{description}     
!  \item[state]
!    {\tt State} to query.
!   \item[dataname]
!    Name of the data item to query.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE
      ! Assume no unless we find it and it is needed.
      ESMF_StateIsNeeded = .FALSE.
      if (present(rc)) rc=ESMF_FAILURE

      ! TODO: decide on the behavior:
      ! should it be an error to ask about a state which doesn't exist?
      ! if the 3rd arg below is .true. then it's an error, if it's .false.
      ! then it's not.  for now, it's an error.
      exists = ESMF_StateTypeFindData(state%statep, dataname, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      if (dataitem%needed .eq. ESMF_STATEDATAISNEEDED) then
        ESMF_StateIsNeeded = .TRUE.
      endif
  
      if (present(rc)) rc=ESMF_SUCCESS

      end function ESMF_StateIsNeeded

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetNeeded -- Query whether a data item is needed
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
!  \begin{description}     
!  \item[state]
!    {\tt State} to query.
!   \item[dataname]
!    Name of the data item to query.
!   \item[needed]
!    Status of data item.  Returns either {\tt ESMF\_STATEDATAISNEEDED},
!    or {\tt ESMF\_STATEDATANOTNEEDED}.
!    When data is added to a {\tt State} the default status of this flag
!    is {\tt ESMF\_STATEDATANOTNEEDED}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE
      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE

      exists = ESMF_StateTypeFindData(state%statep, dataname, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      needed = dataitem%needed

      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_StateGetNeeded

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateSetNeeded -- Set if a data item is needed
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
!  \begin{description}     
!  \item[state]
!    {\tt State} to set..
!   \item[dataname]
!    Name of the data item to set..
!   \item[needed]
!    Set status of data item to this.  Valid values are 
!    {\tt ESMF\_STATEDATAISNEEDED}, or {\tt ESMF\_STATEDATANOTNEEDED}. 
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE

      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE

      exists = ESMF_StateTypeFindData(state%statep, dataname, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      dataitem%needed = needed

      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_StateSetNeeded

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Get objects out of a State.
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetBundle -- Retrieve a data Bundle from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetBundle(state, name, bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: name
      type(ESMF_Bundle), intent(out) :: bundle
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a {\tt Bundle} from a {\tt State} by name.
!
!  \begin{description}     
!  \item[state]
!    State to query for a {\tt Bundle} named {\tt name}.
!  \item[name]
!    {\tt Bundle} name to be returned.
!  \item[bundle]
!    Where the {\tt bundle} is returned.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE
       
      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE
      ! TODO: do we need an empty bundle to mark failure?

      exists = ESMF_StateTypeFindData(state%statep, name, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      if (dataitem%otype .ne. ESMF_STATEBUNDLE) then
          return
      endif

      bundle = dataitem%datap%bp

      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_StateGetBundle

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetField -- Retrieve a data Field from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetField(state, name, field, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: name
      type(ESMF_Field), intent(out) :: field
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a {\tt Field} from a {\tt State} by name.
!
!  \begin{description}     
!  \item[state]
!    State to query for a {\tt Field} named {\tt name}.
!  \item[name]
!    {\tt Field} name to be returned.
!  \item[field]
!    Where the {\tt field} is returned.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE
       
      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE
      ! TODO: do we need an empty field to mark failure?

      exists = ESMF_StateTypeFindData(state%statep, name, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      if (dataitem%otype .ne. ESMF_STATEFIELD) then
          if (dataitem%otype .eq. ESMF_STATEINDIRECT) then
              ! TODO: how do we return the info that this is inside a bundle?
              print *, "found indirect pointer to bundle, need to do what?"
          endif
          return
      endif

      field = dataitem%datap%fp

      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_StateGetField

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetArray -- Retrieve a data Array from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetArray(state, name, array, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: name
      type(ESMF_Array), intent(out) :: array
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a {\tt Array} from a {\tt State} by name.
!
!  \begin{description}     
!  \item[state]
!    State to query for a {\tt Array} named {\tt name}.
!  \item[name]
!    {\tt Array} name to be returned.
!  \item[array]
!    Where the {\tt array} is returned.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE
       
      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE
      ! TODO: do we need an empty array to mark failure?

      exists = ESMF_StateTypeFindData(state%statep, name, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      if (dataitem%otype .ne. ESMF_STATEARRAY) then
          return
      endif

      array = dataitem%datap%ap

      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_StateGetArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateGetState -- Retrieve a data State from a State
!
! !INTERFACE:
      subroutine ESMF_StateGetState(state, name, nestedstate, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(in) :: name
      type(ESMF_State), intent(out) :: nestedstate
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns a nested {\tt State} from a {\tt State} by name.
!
!  \begin{description}     
!  \item[state]
!    State to query for a nested {\tt State} named {\tt name}.
!  \item[name]
!    {\tt State} name to be returned.
!  \item[nestedstate]
!    Where the nested {\tt state} is returned.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

      type(ESMF_StateData), pointer :: dataitem
      logical :: exists
      integer :: status

      status = ESMF_FAILURE
       
      ! Assume failure until we know we will succeed
      if (present(rc)) rc=ESMF_FAILURE
      ! TODO: do we need an empty state to mark failure?

      exists = ESMF_StateTypeFindData(state%statep, name, .true., dataitem, rc=status)
      if (.not. exists) then
          if (present(rc)) rc = status
          return
      endif

      if (dataitem%otype .ne. ESMF_STATESTATE) then
          return
      endif

      nestedstate%statep => dataitem%datap%spp

      if (present(rc)) rc=ESMF_SUCCESS

      end subroutine ESMF_StateGetState

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is for State Transform routines.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateTransform - Apply a Transform to State Data
!
! !INTERFACE:
      subroutine ESMF_StateTransform(state, xformname, xform, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state 
      character(len=*), intent(in) :: xformname
      type(ESMF_Xform), intent(in) :: xform
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
!  \begin{description}     
!  \item[state]
!    {\tt State} to apply {\tt Xform} to.
!  \item[xformname]
!    {\tt Xform} name to be called.
!  \item[xform]
!    {\tt Xform} object to be called.
!  \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!

        ! This is a wrapper which turns around and calls into the
        ! transform code to execute the callback.

        end subroutine ESMF_StateTransform


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for States
!
!------------------------------------------------------------------------------
!BOP
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
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_StateWriteRestart


!------------------------------------------------------------------------------
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
      character (len = *), intent(in) :: name              ! state name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a State from the last call to WriteRestart.
!
!EOP
! !REQUIREMENTS:

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
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts
       type(ESMF_StateType), pointer :: sp
       type(ESMF_StateData), pointer :: dp
       character (len=ESMF_MAXSTR) :: sname
       character (len=1024) :: outbuf
       integer :: status                          ! local error status
       logical :: rcpresent
       integer :: i, nitems

       defaultopts = "brief"
!      Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

      if (.not.associated(state%statep)) then
          print *, "Uninitialized or Destroyed State"
          return
      endif

      if (state%statep%st .eq. ESMF_STATEINVALID) then
          print *, "Uninitialized or Destroyed Field"
          return
      endif

       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_StateValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StatePrint -- Print the internal data for a State
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
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts
       type(ESMF_StateType), pointer :: sp
       type(ESMF_StateData), pointer :: dp
       character (len=ESMF_MAXSTR) :: sname
       character (len=1024) :: outbuf
       integer :: status                          ! local error status
       logical :: rcpresent
       integer :: i, nitems

       defaultopts = "brief"
!      Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

!      ! TODO: Add code here
       ! print num of states, state type, etc

       print *, "StatePrint: "  
       if (.not.associated(state%statep)) then 
           print *, "Not a valid State object"
           return
       endif
       sp => state%statep

       call ESMF_GetName(sp%base, sname, status)
       print *, "  State name '", trim(sname), "'"
       if (sp%st .eq. ESMF_STATEIMPORT) print *, "  Import State"
       if (sp%st .eq. ESMF_STATEEXPORT) print *, "  Export State"
       if (sp%st .eq. ESMF_STATELIST) print *, "  State List"
       if (sp%st .eq. ESMF_STATEINVALID) then
           print *, "  Invalid State"
           return
       endif
       print *, "  Component name '", trim(sp%compname), "'"
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


!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_StatePrint


       end module ESMF_StateMod

