! $Id: ESMF_Comp.F90,v 1.30 2003/03/05 17:09:29 nscollins Exp $
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
!     ESMF Component module
      module ESMF_CompMod
!
!==============================================================================
!
! This file contains the Component class definition and all Component
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_CompMod - Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Component} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_LayoutMod
      use ESMF_ClockMod
      use ESMF_StateMod
      !use ESMF_RouteMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_CompType
!
!     ! Component type: Application, Gridded Component, or Coupler
!
      type ESMF_CompType
      sequence
      private
        integer :: ctype
      end type

      ! these values need to be composable
      type(ESMF_CompType), parameter :: &
                  ESMF_APPCOMP       = ESMF_CompType(1), &   ! binary 0001
                  ESMF_GRIDCOMP      = ESMF_CompType(2), &   ! binary 0010
                  ESMF_CPLCOMP       = ESMF_CompType(4), &   ! binary 0100
                  ESMF_GENERALCOMP   = ESMF_CompType(8)      ! binary 1000

!------------------------------------------------------------------------------
!     ! ESMF_ModelType
!
!     ! Model type: Atmosphere, Land, Ocean, SeaIce, River runoff.
!
      type ESMF_ModelType
      sequence
      private
        integer :: mtype
      end type

      type(ESMF_ModelType), parameter :: &
                  ESMF_ATM = ESMF_ModelType(1), &
                  ESMF_LAND = ESMF_ModelType(2), &
                  ESMF_OCEAN = ESMF_ModelType(3), &
                  ESMF_SEAICE = ESMF_ModelType(4), &
                  ESMF_RIVER = ESMF_ModelType(5), &
                  ESMF_OTHER = ESMF_ModelType(6)

!------------------------------------------------------------------------------
!     ! Callback types, simple integers
!
      integer, parameter :: ESMF_CALLINIT=1, ESMF_CALLRUN=2, ESMF_CALLFINAL=3

!------------------------------------------------------------------------------
!     ! ESMF_CompClass
!
!     ! Component class data.   (Unlike other internal names, this one is
!     !  class because CompType is a public name for the component type.)

      type ESMF_CompClass
      sequence
      private
         type(ESMF_Pointer) :: this      ! C++ ftable pointer - MUST BE FIRST
         type(ESMF_Base) :: base                       ! base class
         type(ESMF_CompType) :: ctype                  ! component type
         type(ESMF_ModelType) :: mtype                 ! model type
         type(ESMF_State) :: importstate               ! import state
         type(ESMF_State) :: exportstate               ! export state
         type(ESMF_State), dimension(:), pointer :: statelist  ! coupling list
         integer :: statecount                         ! length of statelist
         !type(ESMF_Route) :: route                     ! route 
         type(ESMF_Layout) :: layout                   ! component layout
         type(ESMF_Clock) :: clock                     ! component clock
         character(len=ESMF_MAXSTR) :: filepath        ! resource filepath
         integer :: instance_id                        ! for ensembles
      end type


!     ! ESMF_Comp
!
!     ! Component wrapper

      type ESMF_Comp
      sequence
      private
         type(ESMF_CompClass), pointer :: compp
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Comp
      public ESMF_CompType, ESMF_APPCOMP, ESMF_GRIDCOMP, ESMF_CPLCOMP
      public ESMF_ModelType, ESMF_ATM, ESMF_LAND, ESMF_OCEAN, &
                             ESMF_SEAICE, ESMF_RIVER, ESMF_OTHER
      public ESMF_CALLINIT, ESMF_CALLRUN, ESMF_CALLFINAL
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_CompCreate
      public ESMF_CompDestroy

      !public ESMF_CompGetState  ! (component, "import"/"export"/"list", state)
      !public ESMF_CompSetState  ! (component, "import"/"export"/"list", state)
      !public ESMF_CompQueryState 
      public ESMF_CompGet      ! Clock, Layout, CompType, ModelType, Filepath
      public ESMF_CompSet      ! Clock, Layout, CompType, ModelType, Filepath
 
      public ESMF_CompValidate
      public ESMF_CompPrint
 
      ! These do argument processing, layout checking, and then
      !  call the user-provided routines.
      public ESMF_CompInitialize      
      public ESMF_CompRun      
      public ESMF_CompFinalize 

      ! Other routines the user might request to setup.
      !public ESMF_CompCheckpoint
      !public ESMF_CompRestore
      !public ESMF_CompWrite
      !public ESMF_CompRead
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comp.F90,v 1.30 2003/03/05 17:09:29 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_CompCreate -- Generic interface to create an Component

! !INTERFACE:
     interface ESMF_CompCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_CompCreateNew
!       !module procedure ESMF_CompCreateOtherOptions

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ComponentCreate} functions, if needed.
!  
!EOP 
end interface

!------------------------------------------------------------------------------


!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes Component Create/Destroy, Construct/Destruct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompCreateNew -- Create a new Component.

! !INTERFACE:
      function ESMF_CompCreateNew(name, layout, ctype, mtype, clock, &
                                                            filepath, parent, rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_Layout), intent(in), optional :: layout
      type(ESMF_CompType), intent(in), optional :: ctype
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Clock), intent(in), optional :: clock
      character(len=*), intent(in), optional :: filepath
      type(ESMF_Comp), intent(in), optional :: parent
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Component and set the decomposition characteristics.
!
!  The return value is a new Component.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[name]
!    Component name.
!
!   \item[{[layout]}]
!    Component layout.
!
!   \item[{[ctype]}]
!    Component type, where valid types include ESMF\_APPCOMP, ESMF\_GRIDCOMP, 
!    and ESMF\_CPLCOMP for Applications, Gridded Components, and Couplers,
!    respectively.
!
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!
!   \item[{[clock]}]
!    Clock for coordinating component and model time and timesteps.
!
!   \item[{[filepath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!
!   \item[{[parent]}]
!    Parent component if nested components.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! the new Component
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_CompCreateNew%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Allocate a new comp class
        allocate(compclass, stat=status)
        if(status .NE. 0) then
          print *, "ERROR in ESMF_ComponentCreateNew: Allocate"
          return
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, name, layout, ctype, mtype, &
                                                     clock, filepath, parent, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_CompCreateNew%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CompCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompDestroy -- Release resources for a Component

! !INTERFACE:
      subroutine ESMF_CompDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Component}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[component]
!       Destroy contents of this {\tt Component}.
!
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(component%compp, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component contents destruction error"
          return
        endif

        ! Deallocate the component struct itself
        deallocate(component%compp, stat=status)
        if (status .ne. 0) then
          print *, "Component contents destruction error"
          return
        endif
        nullify(component%compp)
 
        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompConstruct - Internal routine to fill in a comp struct

! !INTERFACE:
      subroutine ESMF_CompConstruct(compp, name, layout, ctype, mtype, &
                                                      clock, filepath, parent, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(in) :: name
      type(ESMF_Layout), intent(in), optional :: layout
      type(ESMF_CompType), intent(in), optional :: ctype
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Clock), intent(in), optional :: clock
      character(len=*), intent(in), optional :: filepath
      type(ESMF_Comp), intent(in), optional :: parent
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a new component datatype and fill in the contents.
!
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component internal structure to be filled in.
!
!   \item[name]
!    Component name.
!
!   \item[{[layout]}]
!    Component layout.
!
!   \item[{[ctype]}]
!    Component type, where valid types include ESMF\_APPCOMP, ESMF\_GRIDCOMP, 
!    and ESMF\_CPLCOMP for Applications, Gridded Components, and Couplers,
!    respectively.
!
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!
!   \item[{[clock]}]
!    Clock for coordinating component and model time and timesteps.
!
!   \item[{[filepath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!
!   \item[{[parent]}]
!    Parent component if nested.
!
!   \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! local vars
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! TODO: fill in values here.
        call ESMF_SetName(compp%base, name, "Component", status)
        if (present(ctype)) then
	  compp%ctype = ctype
        else
	  compp%ctype = ESMF_GENERALCOMP
        endif

        if (present(mtype)) then
	  compp%mtype = mtype
        else
	  compp%mtype = ESMF_OTHER
        endif

        if (present(clock)) then
          compp%clock = clock   
        else
          !compp%clock = ESMF_ClockInit()
        endif

        if (present(layout)) then
          compp%layout = layout
        else
          ! query for # processors and create a layout
          ! compp%layout = ESMF_LayoutCreate()
        endif 

        if (present(filepath)) then
          compp%filepath = filepath
        else
          compp%filepath = "."
        endif

        compp%importstate = ESMF_StateCreate(name, ESMF_STATEIMPORT, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "CompConstruct: State create error"
          return
        endif
        compp%exportstate = ESMF_StateCreate(name, ESMF_STATEEXPORT, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "CompConstruct: State create error"
          return
        endif

        nullify(compp%statelist)

        compp%instance_id = 1

        ! Create an empty table.
        call c_ESMC_FTableCreate(compp%this, status) 
        if (status .ne. ESMF_SUCCESS) then
          print *, "CompConstruct: Table create error"
          return
        endif
   
        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompConstruct


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompDestruct - Internal routine for freeing resources

! !INTERFACE:
      subroutine ESMF_CompDestruct(compp, rc)
!
! !ARGUMENTS:
      type(ESMF_CompClass), pointer :: compp
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Component}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[compp]
!      Component internal structure to be freed.
!
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! release any storage that was allocated
        if (associated(compp%statelist)) then
           deallocate(compp%statelist, stat=status)
           if (status .ne. 0) then
             print *, "Component contents destruction error"
             return
           endif
           nullify(compp%statelist)
        endif
        
        ! call C++ to release function and data pointer tables.
        call c_ESMC_FTableDestroy(compp%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component contents destruction error"
          return
        endif

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompDestruct



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Component Init, Run, and Finalize methods
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompInitialize -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_CompInitialize(component, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_Comp) :: component
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to call Initialization routine for.
!
!   \item[{[phase]}]  If multiple-phase init, which phase this is.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: de_id                        ! the current DE

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! See if this is currently running on a DE which is part of the
        ! proper Layout.
	call ESMF_LayoutGetDEId(component%compp%layout, de_id, status)
        if (status .ne. ESMF_SUCCESS) then
          ! this is not our DE
          if (rcpresent) rc = ESMF_SUCCESS
          return
        endif

        ! TODO: handle optional args, do framework setup for this comp.
        ! Call user-supplied init routine.

        ! TODO: add stringcat for phase
        ! TODO: test this
        call c_ESMC_FTableCallRoutine(component%compp%this, ESMF_CALLINIT, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component initialization error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompInitialize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompRun -- Call the Component's run routine

! !INTERFACE:
      subroutine ESMF_CompRun(component, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_Comp) :: component 
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component for which to call Run routine.
!
!   \item[{[phase]}]  If multiple-phase run, which phase this is.
!
!   %\item[timesteps]
!   % How long the Run interval is.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: de_id                        ! the current DE

        ! Initalize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! See if this is currently running on a DE which is part of the
        ! proper Layout.
	call ESMF_LayoutGetDEId(component%compp%layout, de_id, status)
        if (status .ne. ESMF_SUCCESS) then
          ! this is not our DE
          if (rcpresent) rc = ESMF_SUCCESS
          return
        endif

        ! TODO: handle optional args, do framework setup for this comp.
        ! Call user-supplied init routine.

        ! TODO: add code to handle phases
       
        ! TODO: test this
        call c_ESMC_FTableCallRoutine(component%compp%this, ESMF_CALLRUN, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component run error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompRun


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompFinalize -- Call the Component's finalization routine

! !INTERFACE:
      subroutine ESMF_CompFinalize(component, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_Comp) :: component 
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalization code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to call finalization routine for.
!
!   \item[{[phase]}]  If multiple-phase finalize, which phase this is.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: de_id                        ! the current DE

!       Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! See if this is currently running on a DE which is part of the
        ! proper Layout.
	call ESMF_LayoutGetDEId(component%compp%layout, de_id, status)
        if (status .ne. ESMF_SUCCESS) then
          ! this is not our DE
          if (rcpresent) rc = ESMF_SUCCESS
          return
        endif

        ! TODO: handle optional args, do framework setup for this comp.
        ! Call user-supplied init routine.

        ! TODO: add code to handle multiphase here

        call c_ESMC_FTableCallRoutine(component%compp%this, ESMF_CALLFINAL, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component finalization error"
          return
        endif

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompFinalize


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query/Set information from/in the component.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompGet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_CompGet(comp, import, export, layout, clock, &
                                instanceid, statelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp), intent(in) :: comp
      type(ESMF_State), intent(out), optional :: import
      type(ESMF_State), intent(out), optional :: export
      type(ESMF_Layout), intent(out), optional :: layout
      type(ESMF_Clock), intent(out), optional :: clock
      integer, intent(out), optional :: instanceid
      type(ESMF_State), intent(out), optional :: statelist(:)
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the component.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOP
! !REQUIREMENTS:

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        type(ESMF_CompClass), pointer :: cp     ! comp class pointer

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        cp => comp%compp

        if (present(import)) then
          import = cp%importstate
        endif
 
        if (present(export)) then
          export = cp%exportstate
        endif
 
        if (present(layout)) then
          layout = cp%layout
        endif

        if (present(clock)) then
          clock = cp%clock
        endif

        if (present(instanceid)) then
          instanceid = cp%instance_id
        endif

        ! TODO: does this need to be allocated before copy?
        if (present(statelist)) then
          if (size(cp%statelist) .gt. size(statelist)) then
            print *, "statelist array not long enough"
          endif
          statelist = cp%statelist
        endif

        ! TODO: add rest of comp contents here

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompSet -- Set various information in a Component
!
! !INTERFACE:
      subroutine ESMF_CompSet(comp, import, export, layout, clock, &
                                instanceid, statelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp), intent(inout) :: comp
      type(ESMF_State), intent(in), optional :: import
      type(ESMF_State), intent(in), optional :: export
      type(ESMF_Layout), intent(in), optional :: layout
      type(ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: instanceid
      type(ESMF_State), intent(in), optional :: statelist(:)
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Reset information in a component.  For cases where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOP
! !REQUIREMENTS:

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        type(ESMF_CompClass), pointer :: cp     ! comp class pointer

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        cp => comp%compp
        if (present(import)) then
          cp%importstate = import
        endif
 
        if (present(export)) then
          cp%exportstate = export
        endif
 
        if (present(layout)) then
          cp%layout = layout
        endif

        if (present(clock)) then
          cp%clock = clock
        endif

        if (present(instanceid)) then
          cp%instance_id = instanceid
        endif

        ! TODO: does this do the right thing?
        if (present(statelist)) then
           if (associated(cp%statelist)) then
             deallocate(cp%statelist, stat=status)
             if (status .ne. 0) then
               print *, "Component statelist deallocate error"
               return
             endif
           endif
           allocate(cp%statelist(size(statelist)), stat=status)
           if (status .ne. 0) then
             print *, "Component statelist allocate error"
             return
           endif
           cp%statelist = statelist
        endif

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompSet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Components
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompCheckpoint - Save a Component's state to disk
!
! !INTERFACE:
      subroutine ESMF_CompCheckpoint(component, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp):: component 
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
        end subroutine ESMF_CompCheckpoint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompRestore - Restore a Component's state from disk
!
! !INTERFACE:
      function ESMF_CompRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Component from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Comp) :: a 

!       this is just to stop compiler warnings
        nullify(a%compp)

        ESMF_CompRestore = a 
 
        end function ESMF_CompRestore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompWrite - Write a Component to disk
!
! !INTERFACE:
      subroutine ESMF_CompWrite(component, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_CompWrite


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompRead - Read a Component from disk
!
! !INTERFACE:
      function ESMF_CompRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Comp) :: a

!       this is just to stop compiler warnings
        nullify(a%compp)

        ESMF_CompRead = a 
 
        end function ESMF_CompRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
      subroutine ESMF_CompValidate(component, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts
       integer :: status                       ! local error status
       logical :: rcpresent                    ! did user specify rc?

!      Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

!      ! Interface to call the C++ validate code
       if(present(options)) then
           !call c_ESMC_CompValidate(component, options, status) 
       else
           !call c_ESMC_CompValidate(component, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Component validate error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CompPrint -- Print the contents of a Component
!
! !INTERFACE:
      subroutine ESMF_CompPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
!
!EOP
! !REQUIREMENTS:

       integer :: status                       ! local error status
       logical :: rcpresent                    ! did user specify rc?
       character (len=6) :: defaultopts
       character (len=ESMF_MAXSTR) :: cname

       ! Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

       ! Parse options and decide what to print
       if(present(options)) then
           ! TODO:  decide what to print
       endif

       call ESMF_GetName(component%compp%base, cname, status)
       print *, "Component print:"
       print *, "  name = ", trim(cname)
       
       ! TODO: add more info here

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompPrint


       end module ESMF_CompMod

