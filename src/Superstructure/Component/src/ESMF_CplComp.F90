! $Id: ESMF_CplComp.F90,v 1.5 2003/06/27 16:46:13 nscollins Exp $
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
!     ESMF Coupler Component module
      module ESMF_CplCompMod
!
!==============================================================================
!
! This file contains the Coupler Component class definition and all 
!   Coupler Component class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_CplCompMod - Coupler Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Coupler Component} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_MachineMod
      use ESMF_ConfigMod
      use ESMF_DELayoutMod
      use ESMF_ClockMod
      use ESMF_GridMod
      use ESMF_StateMod
      use ESMF_CompMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private


!------------------------------------------------------------------------------
!     ! ESMF_CplComp
!
!     ! Component wrapper

      type ESMF_CplComp
      sequence
      private
         type(ESMF_CompClass), pointer :: compp => NULL()
      end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CplComp

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_CplCompCreate
      public ESMF_CplCompDestroy

      public ESMF_CplCompGet
      public ESMF_CplCompSet
 
      public ESMF_CplCompValidate
      public ESMF_CplCompPrint
 
      ! These do argument processing, layout checking, and then
      !  call the user-provided routines.
      public ESMF_CplCompInitialize
      public ESMF_CplCompRun
      public ESMF_CplCompFinalize

      ! Other routines the user might request to setup.
      public ESMF_CplCompCheckpoint
      public ESMF_CplCompRestore
      !public ESMF_CplCompWrite
      !public ESMF_CplCompRead
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_CplComp.F90,v 1.5 2003/06/27 16:46:13 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a Coupler Component
!
! !INTERFACE:
      interface ESMF_CplCompCreate

! !PRIVATE MEMBER FUNCTIONS:
        !module procedure ESMF_CplCompCreateNew
        module procedure ESMF_CplCompCreateConf

! !DESCRIPTION:
!     This interface provides an entry point for methods that create a 
!     Coupler {\tt Component}.  The difference is whether an already
!     created configuration object is passed in, or a filename of a new
!     config file which needs to be opened.
!

!EOP
      end interface

!==============================================================================
!BOP
! !IROUTINE: ESMF_CplCompInitialize
!
! !INTERFACE:
      interface ESMF_CplCompInitialize

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_CplCompInitStateList
        module procedure ESMF_CplCompInitTwoStates

! !DESCRIPTION:
!     This interface provides an entry point for methods that want to
!     call a Coupler's {\tt Initialization} routine.  Simple couplers
!     can be called with separate import and export state as arguments
!     and the caller does not have to create a combined statelist object.  
!     Couplers with more complex combinations of states should be called with
!     a single combined state object containing all needed states.
!

!EOP
      end interface

!==============================================================================
!BOP
! !IROUTINE: ESMF_CplCompRun
!
! !INTERFACE:
      interface ESMF_CplCompRun

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_CplCompRunStateList
        module procedure ESMF_CplCompRunTwoStates

! !DESCRIPTION:
!     This interface provides an entry point for methods that want to
!     call a Coupler's {\tt Run} routine.  Simple couplers
!     can be called with separate import and export state as arguments
!     and the caller does not have to create a combined statelist object.  
!     Couplers with more complex combinations of states should be called with
!     a single combined state object containing all needed states.
!

!EOP
      end interface

!==============================================================================
!BOP
! !IROUTINE: ESMF_CplCompFinalize
!
! !INTERFACE:
      interface ESMF_CplCompFinalize

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_CplCompFinalStateList
        module procedure ESMF_CplCompFinalTwoStates

! !DESCRIPTION:
!     This interface provides an entry point for methods that want to
!     call a Coupler's {\tt Finalize} routine.  Simple couplers
!     can be called with separate import and export state as arguments
!     and the caller does not have to create a combined statelist object.  
!     Couplers with more complex combinations of states should be called with
!     a single combined state object containing all needed states.
!

!EOP
      end interface

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
! !IROUTINE: ESMF_CplCompCreateNew -- Create a new Component.

! !INTERFACE:
      function ESMF_CplCompCreateNew(name, layout, config, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_DELayout), intent(in) :: layout
      type(ESMF_Config), intent(in) :: config
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
!   \item[layout]
!    Component layout.
!
!   \item[config]
!    Component-specific configuration object.  
!  
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateNew%compp)
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
          print *, "ERROR in ESMF_CplComponentCreate: Allocate"
          return
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_CPLCOMPTYPE, name, layout, &
                                                    config=config, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateNew%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompCreateConf -- Create a new Component.

! !INTERFACE:
      function ESMF_CplCompCreateConf(name, layout, config, configfile, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateConf
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configfile
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
!   \item[{[name]}]
!    Component name.
!
!   \item[{[layout]}]
!    Component layout.
!
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
!  
!   \item[{[configfile]}]
!    Component-specific configuration filename. 
!  
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateConf%compp)
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
          print *, "ERROR in ESMF_CplComponentCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_CPLCOMPTYPE, name, layout, &
                                configfile=configfile, config=config, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateConf%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateConf
    

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompInitStateList -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_CplCompInitStateList(component, statelist, &
                                                        clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      type (ESMF_State), intent(inout) :: statelist
      type (ESMF_Clock), intent(in), optional :: clock
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
!   \item[statelist]  
!       State containing list of nested import and export states for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase init, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompInitialize(component%compp, 1, statelist=statelist, &
                                              clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompInitStateList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompInitTwoStates -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_CplCompInitTwoStates(component, importstate, &
                                             exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      type (ESMF_State), intent(inout) :: importstate
      type (ESMF_State), intent(inout) :: exportstate
      type (ESMF_Clock), intent(in), optional :: clock
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
!   \item[{[importstate]}]  
!       State containing source data for coupling.
!
!   \item[{[exportstate]}]  
!       State containing destination data for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase init, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompInitialize(component%compp, 2, importstate=importstate, &
                                 exportstate=exportstate, clock=clock,     &
                                 phase=phase, rc=rc)

        end subroutine ESMF_CplCompInitTwoStates


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompRunStateList -- Call the Component's run routine

! !INTERFACE:
      subroutine ESMF_CplCompRunStateList(component, statelist, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      type (ESMF_State), intent(inout) :: statelist
      type (ESMF_Clock), intent(in), optional :: clock
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
!    Component to call Run routine for.
!
!   \item[statelist]  
!       State containing list of nested import and export states for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase run, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompRun(component%compp, 1, statelist=statelist, &
                                            clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompRunStateList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompRunTwoStates -- Call Component Run routine with 2 States

! !INTERFACE:
      subroutine ESMF_CplCompRunTwoStates(component, importstate, exportstate, &
                                                                  clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      type (ESMF_State), intent(inout) :: importstate
      type (ESMF_State), intent(inout) :: exportstate
      type (ESMF_Clock), intent(in), optional :: clock
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
!    Component to call Run routine for.
!
!   \item[importstate]  
!       State containing import data for coupling.
!
!   \item[importstate]  
!       State containing export data for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase run, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompRun(component%compp, 2, importstate=importstate,  &
                          exportstate=exportstate, clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompRunTwoStates


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompGet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_CplCompGet(component, name, layout, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(in) :: component
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      character(len=*), intent(out), optional :: configfile
      type(ESMF_Config), intent(out), optional :: config
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

        call ESMF_CompGet(component%compp, name, layout, &
                          configfile=configfile, config=config, rc=rc)

        end subroutine ESMF_CplCompGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_CplCompSet(component, name, layout, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: component
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      character(len=*), intent(in), optional :: configfile
      type(ESMF_Config), intent(in), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets or resets information about the component.  When the caller
!      only wants to set a single value specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompSet(component%compp, name, layout, &
                          configfile=configfile, config=config, rc=rc)

        end subroutine ESMF_CplCompSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
      subroutine ESMF_CplCompValidate(component, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(component%compp, options, rc)
 
       end subroutine ESMF_CplCompValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CplCompPrint -- Print the contents of a Component
!
! !INTERFACE:
      subroutine ESMF_CplCompPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
!
!EOP
! !REQUIREMENTS:

       print *, "Coupler Component:"
       call ESMF_CompPrint(component%compp, options, rc)

       end subroutine ESMF_CplCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompFinalStatelist -- Call the Component's finalize routine

! !INTERFACE:
      subroutine ESMF_CplCompFinalStatelist(component, statelist, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      type (ESMF_State), intent(inout) :: statelist
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalize code for a component.
!
!    
!  The arguments are: 
!  \begin{description} 
!  
!   \item[component]
!    Component to call Finalize routine for.
!
!   \item[statelist]  
!       State containing list of nested import and export states for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase finalize, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompFinalize(component%compp, 1, statelist=statelist, &
                                          clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompFinalStatelist


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompFinalTwoStates -- Call the Component's finalize routine

! !INTERFACE:
      subroutine ESMF_CplCompFinalTwoStates(component, importstate, &
                                                   exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      type (ESMF_State), intent(inout) :: importstate
      type (ESMF_State), intent(inout) :: exportstate
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalize code for a component.
!
!    
!  The arguments are: 
!  \begin{description} 
!  
!   \item[component]
!    Component to call Finalize routine for.
!
!   \item[importstate]  
!       State containing import data for coupling.
!
!   \item[exportstate]  
!       State containing export data for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase finalize, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompFinalize(component%compp, 2, importstate=importstate, &
                          exportstate=exportstate, clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompFinalTwoStates


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompCheckpoint -- Call the Component's checkpoint routine

! !INTERFACE:
      subroutine ESMF_CplCompCheckpoint(component, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: component
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for a component.
!    
!  The arguments are: 
!  \begin{description} 
!  
!   \item[component]
!    Component to call Checkpoint routine for.
!
!   \item[{[iospec]}]
!       {\tt IOSpec} object which describes I/O options.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase checkpoint, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompCheckpoint(component%compp, iospec, clock, phase, rc)

        end subroutine ESMF_CplCompCheckpoint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompRestore -- Call the Component's restore routine

! !INTERFACE:
      subroutine ESMF_CplCompRestore(component, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: component
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for a component.
!    
!  The arguments are: 
!  \begin{description} 
!  
!   \item[component]
!    Component to call Restore routine for.
!
!   \item[{[iospec]}]
!       {\tt IOSpec} object which describes I/O options.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase restore, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompRestore(component%compp, iospec, clock, phase, rc)

        end subroutine ESMF_CplCompRestore


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompDestroy -- Release resources for a Component

! !INTERFACE:
      subroutine ESMF_CplCompDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: component
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

        ! Check to see if already destroyed
        if (.not.associated(component%compp)) then  
          print *, "Component already destroyed"
          return
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

        end subroutine ESMF_CplCompDestroy

end module ESMF_CplCompMod

