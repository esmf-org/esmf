! $Id: ESMF_State.F90,v 1.5 2003/02/03 17:10:19 nscollins Exp $
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
!BOP
! !MODULE: ESMF_StateMod - Manage data states uniformly between F90 and C++     
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
      use ESMF_ArrayMod
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
                ESMF_STATEIMPORT = ESMF_StateImpExpType(1), &
                ESMF_STATEEXPORT = ESMF_StateImpExpType(2), &
                ESMF_STATEUNKNOWN = ESMF_StateImpExpType(3)

!------------------------------------------------------------------------------
!     ! ESMF_StateObjectType
!     !   Each entry in the list of states is either simply a name placeholder
!     !   or an actual data item - Bundle, Field, or Array.  The state list
!     !   can either be a linked list or an array of derived types.
!     !   For a linked list, the Last is the end of the list.
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
                ESMF_STATEDATANAME = ESMF_StateObjectType(4), &
                ESMF_STATELAST = ESMF_StateObjectType(5), &
                ESMF_STATEOBJTYPEUNKNOWN = ESMF_StateObjectType(6)

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
                ESMF_STATEDATANOTNEEDED= ESMF_StateDataNeeded(2), &
                ESMF_STATEDATADONOTCARE = ESMF_StateDataNeeded(3), &
                ESMF_STATEDATANEEDUNKNOWN = ESMF_StateDataNeeded(4)

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
                ESMF_STATEDATAREADYTOREAD= ESMF_StateDataReady(2), &
                ESMF_STATEDATAREADYUNKNOWN = ESMF_StateDataReady(3)


!------------------------------------------------------------------------------
!     ! ESMF_DataHolder
!
!     ! Make a single data type for Bundles, Fields, and Arrays.
!     !  The ObjectType is one level up, because this structure is not
!     !  allocated until it is actually needed.  This is a private type.

      type ESMF_DataHolder
      sequence
      private
          type(ESMF_Bundle), pointer :: bp
          type(ESMF_Field), pointer :: fp 
          type(ESMF_Array), pointer :: ap
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
        character, pointer :: namep
        type(ESMF_DataHolder), pointer :: datap
        type(ESMF_StateDataNeeded) :: needed
        type(ESMF_StateDataReady) :: ready
        type(ESMF_StateData), pointer :: nextdata
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
        type(ESMF_StateType), pointer :: statep
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State
      public ESMF_StateImpExpType, ESMF_STATEIMPORT, ESMF_STATEEXPORT
      public ESMF_StateDataNeeded, ESMF_STATEDATAISNEEDED, &
                                   ESMF_STATEDATANOTNEEDED, &
                                   ESMF_STATEDATADONOTCARE
      public ESMF_StateDataReady,  ESMF_STATEDATAREADYTOWRITE, &
                                   ESMF_STATEDATAREADYTOREAD, &
                                   ESMF_STATEDATAREADYUNKNOWN
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateCreate, ESMF_StateDestroy

      public ESMF_StateAddData, ESMF_StateGetData
      public ESMF_StateGetInfo
      public ESMF_StateAddNameOnly 
      public ESMF_StateSetNeeded, ESMF_StateGetNeeded
      !public ESMF_StateGetNeededList   ! returns an array of values
      !public ESMF_State{Get/Set}Ready  ! is data ready
      !public ESMF_State{Get/Set}CompName  ! normally set at create time
      public ESMF_StateTransform
 
      public ESMF_StateCheckpoint
      public ESMF_StateRestore
 
      public ESMF_StatePrint
      public ESMF_imexeq, ESMF_needeq, ESMF_redyeq
      public ESMF_imexne, ESMF_needne, ESMF_redyne
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_State.F90,v 1.5 2003/02/03 17:10:19 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_StateCreate -- Generic interface to create an State

! !INTERFACE:
     interface ESMF_StateCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateCreateNew
        module procedure ESMF_StateCreateEmpty

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateCreate} functions.   
!  
!EOP 
end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddData -- Add Bundles, Fields, and Arrays to a State

! !INTERFACE:
     interface ESMF_StateAddData

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddBundle
        module procedure ESMF_StateAddBundleList
        !module procedure ESMF_StateAddField
        !module procedure ESMF_StateAddFieldList
        !module procedure ESMF_StateAddArray
        !module procedure ESMF_StateAddArrayList

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

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateGetData} functions.   
!  
!EOP 
end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAddNameOnly -- Add names as placeholders to the State

! !INTERFACE:
     interface ESMF_StateAddNameOnly

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateAddDataName
        module procedure ESMF_StateAddDataNameList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateAddNameOnly} functions.   
!  
!EOP 
end interface


!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure ESMF_imexeq
 module procedure ESMF_needeq
 module procedure ESMF_redyeq
end interface

interface operator (.ne.)
 module procedure ESMF_imexne
 module procedure ESMF_needne
 module procedure ESMF_redyne
end interface


!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF types to see if they're the same or not

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



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the State Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateCreateNew -- Create a new State specifying all options.

! !INTERFACE:
      function ESMF_StateCreateNew(compname, statetype, datacount, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: compname 
      type(ESMF_StateImpExpType), intent(in) :: statetype
      integer, intent(in) :: datacount
      ! more to be added here
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
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_StateType), target :: newstate 
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        nullify(ESMF_StateCreateNew%statep)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       TODO: add working code here
        status = ESMF_SUCCESS
        if (status .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif

!       set return values
        ESMF_StateCreateNew%statep => newstate 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_StateCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateCreateEmpty -- Create a new State specifying no data

! !INTERFACE:
      function ESMF_StateCreateEmpty(rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreateEmpty
!
! !ARGUMENTS:
      !character(len=*), intent(in), optional :: compname 
      !type(ESMF_StateImpExpType), intent(in), optional :: statetype
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt State}.  The return value is a new {\tt State}.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[{[compname]}]
!    Name of the {\tt Component} this {\tt State} is associated with.
!
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Returns either {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATEUNKNOWN}.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_StateType), target :: newstate 
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        nullify(ESMF_StateCreateEmpty%statep)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       TODO: add working code here
        status = ESMF_SUCCESS
        if (status .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif

!       set return values
        ESMF_StateCreateEmpty%statep => newstate 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_StateCreateEmpty


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

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       call Destruct to release resources
        call ESMF_StateDestruct(state%statep, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State contents destruction error"
          return
        endif

!       Release space
 	deallocate(state%statep, stat=status)
        if (status .ne. 0) then    ! F90 return code
          print *, "State contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestroy



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateDestruct -- Internal routine to deallocate space
!
! !INTERFACE:
      subroutine ESMF_StateDestruct(statetype, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType) :: statetype
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt State}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[statetype]
!       Destroy contents of this {\tt State}.
!
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       !TODO: add code here

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestruct


!------------------------------------------------------------------------------
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
!BOP
! !IROUTINE: ESMF_StateTypeAddBundleList - Add a list of Bundles to a StateType
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddBundleList(statep, bcount, bundles, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), intent(inout) :: statep
      integer, intent(in) :: bcount
      type(ESMF_Bundle), dimension(:), intent(in) :: bundles
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Add multiple bundles to a {\tt State}.  Internal routine only.
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_FAILURE              ! Error status
      integer :: i                                ! temp var
      logical :: rcpresent=.FALSE.                ! Return code present

!     ! Initialize return code.  Assume failure until success assured.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!!!     ! TODO: check for name collisions
!!
!!!     ! Add the bundles to the state, checking for name clashes
!!      if(btype%field_count .eq. 0) then
!!          allocate(btype%flist(fieldcount), stat=status)
!!          if(status .NE. 0) then
!!            print *, "ERROR in ESMF_BundleAddFields: Fieldlist allocate"
!!            return
!!          endif
!!         
!!!         now add the fields to the new list
!!          do i=1, fieldcount
!!            btype%flist(i) = fields(i)
!!          enddo
!!
!!          btype%field_count = fieldcount
!!      else
!!!         make a list the right length
!!          allocate(temp_flist(btype%field_count + fieldcount), stat=status)
!!          if(status .NE. 0) then
!!            print *, "ERROR in ESMF_BundleConstructNew: temporary Fieldlist allocate"
!!            return
!!          endif
!!
!!!         preserve old contents
!!          do i = 1, btype%field_count
!!            temp_flist(i) = btype%flist(i)
!!          enddo
!!
!!!         and append the new fields to the list
!!          do i=btype%field_count+1, btype%field_count + fieldcount
!!            temp_flist(i) = fields(i)
!!          enddo
!!
!!!         delete old list
!!          deallocate(btype%flist, stat=status)
!!          if(status .NE. 0) then
!!            print *, "ERROR in ESMF_BundleConstructNew: Fieldlist deallocate"
!!          endif
!!
!!!         and now make this the permanent list
!!          btype%flist => temp_flist
!!          btype%field_count = btype%field_count + fieldcount
!!
!!      endif
!!
!!!     If packed data buffer requested, create or update it here.
!!      if (btype%pack_flag .eq. ESMF_PACK_FIELD_DATA) then
!!
!!         call ESMF_BundleTypeRepackData(btype, rc=rc)
!!
!!      endif

      if(rcpresent) rc = ESMF_SUCCESS


        end subroutine ESMF_StateTypeAddBundleList

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
!BOP
! !IROUTINE: ESMF_StateTypeAddDataNameList - internal routine
!
! !INTERFACE:
      subroutine ESMF_StateTypeAddDataNameList(statep, namecount, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_StateType), intent(inout) :: statep
      integer, intent(in) :: namecount
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
!EOP

      integer :: status=ESMF_FAILURE              ! Error status
      integer :: i                                ! temp var
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code.  Assume failure until success assured.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!!!     Add the names in the list, checking for collisions.
!!      if(btype%field_count .eq. 0) then
!!          allocate(btype%flist(fieldcount), stat=status)
!!          if(status .NE. 0) then
!!            print *, "ERROR in ESMF_BundleAddFields: Fieldlist allocate"
!!            return
!!          endif
!!         
!!!         now add the fields to the new list
!!          do i=1, fieldcount
!!            btype%flist(i) = fields(i)
!!          enddo
!!
!!          btype%field_count = fieldcount
!!      else
!!!         make a list the right length
!!          allocate(temp_flist(btype%field_count + fieldcount), stat=status)
!!          if(status .NE. 0) then
!!            print *, "ERROR in ESMF_BundleConstructNew: temporary Fieldlist allocate"
!!            return
!!          endif
!!
!!!         preserve old contents
!!          do i = 1, btype%field_count
!!            temp_flist(i) = btype%flist(i)
!!          enddo
!!
!!!         and append the new fields to the list
!!          do i=btype%field_count+1, btype%field_count + fieldcount
!!            temp_flist(i) = fields(i)
!!          enddo
!!
!!!         delete old list
!!          deallocate(btype%flist, stat=status)
!!          if(status .NE. 0) then
!!            print *, "ERROR in ESMF_BundleConstructNew: Fieldlist deallocate"
!!          endif
!!
!!!         and now make this the permanent list
!!          btype%flist => temp_flist
!!          btype%field_count = btype%field_count + fieldcount
!!      endif 
!!!     If packed data buffer requested, create or update it here.
!!      if (btype%pack_flag .eq. ESMF_PACK_FIELD_DATA) then 
!!         call ESMF_BundleTypeRepackData(btype, rc=rc)
!!      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_StateTypeAddDataNameList

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
      subroutine ESMF_StateGetInfo(state, compname, statetype, statecount, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      character (len=*), intent(out), optional :: compname
      type(ESMF_StateImpExpType), intent(out), optional :: statetype
      integer, intent(out), optional :: statecount
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns the name of the {\tt Component} this state is associated with.
!      Also returns the type of {\tt State}, either {\tt Import} or 
!      {\tt Export}.
!
!  \begin{description}     
!  \item[state]
!    {\tt State} to query.
!   \item[{[compname]}]
!    Name of the {\tt Component} this {\tt State} is associated with.
!   \item[{[statetype]}]
!    Import or Export {\tt State}.  Returns either {\tt ESMF\_STATEIMPORT},
!    {\tt ESMF\_STATEEXPORT}, or {\tt ESMF\_STATEUNKNOWN}.
!   \item[{[statecount]}]
!    Count of data items in this {\tt state}, including placeholder names.
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
        end subroutine ESMF_StateGetInfo

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
!    {\tt ESMF\_STATEDATANOTNEEDED}, or {\tt ESMF\_STATEDATADONOTCARE}.
!    When data is added to a {\tt State} the default status of this flag
!    is {\tt ESMF\_STATEDATADONOTCARE}.
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
!    {\tt ESMF\_STATEDATAISNEEDED}, {\tt ESMF\_STATEDATANOTNEEDED}, 
!    or {\tt ESMF\_STATEDATADONOTCARE}.
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
        if (needed .ne. ESMF_STATEDATAISNEEDED) print *, "hi"
        if (needed .eq. ESMF_STATEDATAISNEEDED) print *, "hi"
        !state%statep%needed = needed
        end subroutine ESMF_StateSetNeeded

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

!
! TODO: code goes here
!
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

!
! TODO: code goes here
!
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

!
! TODO: code goes here
!
        end subroutine ESMF_StateGetArray

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
!      Apply a Transform to a State.  This routine is intended to be called
!      from within a Component when it is not practical to return to the
!      calling layer in order to do the coupling directly.  This call 
!      passes through the framework back into user-written coupling code
!      to allow exchange of data between Components.  It returns back to
!      the calling location and allows the Component to continue from that
!      place.  Components which run in sequential mode have no need to use
!      this routine; Coupling code is called directly from the Application
!      level code.
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
! !IROUTINE: ESMF_StateCheckpoint -- Save the internal data for a State
!
! !INTERFACE:
      subroutine ESMF_StateCheckpoint(state, iospec, rc)
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
        end subroutine ESMF_StateCheckpoint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateRestore -- Restore the internal data from a State
!
! !INTERFACE:
      function ESMF_StateRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! state name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a State from the last call to Checkpoint.
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

        ESMF_StateRestore = a 
 
        end function ESMF_StateRestore


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
       character (len=6) :: defaultopts="brief"
       integer :: status=ESMF_FAILURE      ! local error status
       type(ESMF_StateType), pointer :: sp
       logical :: rcpresent=.FALSE.

!      Initialize return code; assume failure until success is certain
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

!      ! TODO: Add code here
       ! print num of states, state type, etc
       sp => state%statep
       print *, "StatePrint: "  
       print *, "  Component ", trim(sp%compname)
       print *, "  Number of members: ", sp%datacount
       if (sp%st .eq. ESMF_STATEIMPORT) print *, "  Import State"
       if (sp%st .eq. ESMF_STATEEXPORT) print *, "  Export State"
       if (sp%st .eq. ESMF_STATEUNKNOWN) print *, "  Unknown State Type"


!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_StatePrint


       end module ESMF_StateMod

