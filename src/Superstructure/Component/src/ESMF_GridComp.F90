! $Id: ESMF_GridComp.F90,v 1.23 2004/03/09 12:48:31 theurich Exp $
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
!     ESMF Gridded Component module
      module ESMF_GridCompMod
!
!==============================================================================
!
! This file contains the Gridded Component class definition and all 
!  Gridded Component class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_GridCompMod - Gridded Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Gridded Component} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_MachineMod
      use ESMF_ConfigMod
      use ESMF_DELayoutMod
      use ESMF_ClockMod
      use ESMF_GridTypesMod
      use ESMF_StateMod
      use ESMF_CompMod

#ifdef ESMF_ENABLE_VM
      use ESMF_VMMod
#endif
      
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private


!------------------------------------------------------------------------------
!     ! ESMF_GridComp
!
!     ! Grid Component wrapper

      type ESMF_GridComp
      sequence
      !private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_CompClass), pointer :: compp => NULL()
#else
         type(ESMF_CompClass), pointer :: compp 
#endif
      end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      public ESMF_GridComp

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:


      public ESMF_GridCompCreate
      public ESMF_GridCompDestroy

      public ESMF_GridCompGet
      public ESMF_GridCompSet
 
      public ESMF_GridCompValidate
      public ESMF_GridCompPrint
 
      ! These do argument processing, layout checking, and then
      !  call the user-provided routines.
      public ESMF_GridCompInitialize
      public ESMF_GridCompRun
      public ESMF_GridCompFinalize

      ! Other routines the user might request to setup.
      public ESMF_GridCompWriteRestart
      public ESMF_GridCompReadRestart
      !public ESMF_GridCompWrite
      !public ESMF_GridCompRead

#ifdef ESMF_ENABLE_VM
      ! Procedures for VM-enabled mode      
      public ESMF_GridCompVMDefMaxThreads
      public ESMF_GridCompVMDefMinThreads
      public ESMF_GridCompVMDefMaxPEs
      ! Return from user-provided routines
      public ESMF_GridCompReturn
#endif
      
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GridComp.F90,v 1.23 2004/03/09 12:48:31 theurich Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_GridCompCreate - Create a Gridded Component
!
! !INTERFACE:
      interface ESMF_GridCompCreate

! !PRIVATE MEMBER FUNCTIONS:
        !module procedure ESMF_GridCompCreateNew
        module procedure ESMF_GridCompCreateConf
#ifdef ESMF_ENABLE_VM
        module procedure ESMF_GridCompCreateVM
#endif
        
! !DESCRIPTION:
!     This interface provides an entry point for methods that create a 
!     Gridded {\tt Component}.  The difference is whether an already
!     created configuration object is passed in, or a filename of a new
!     config file which needs to be opened.
!

!EOPI
      end interface

      interface
          subroutine services(comp, rc)
            use ESMF_CompMod
            type(ESMF_CompClass) :: comp 
            integer :: rc
          end subroutine
      end interface

!==============================================================================

      contains

!==============================================================================

!BOP
! !IROUTINE: ESMF_GridCompCreate - Create a new Component

! !INTERFACE:
      ! Private name; call using ESMF_GridCompCreate()      
      function ESMF_GridCompCreateNew(name, layout, mtype, grid, config, clock, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_DELayout), intent(in) :: layout
      type(ESMF_ModelType), intent(in) :: mtype 
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Config), intent(in) :: config
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Component and set the decomposition characteristics.
!
!  The return value is a new Component.
!    
!  The arguments are:
!  \begin{description}
!   \item[name]
!    Component name.
!   \item[layout]
!    Component layout.
!   \item[mtype]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[grid]
!    Default grid associated with this component.
!   \item[config]
!    Component-specific configuration object.  
!   \item[clock]
!    Component-specific clock object.  
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_GridCompCreateNew%compp)
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
          print *, "ERROR in ESMF_GridComponentCreate: Allocate"
          return
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_GRIDCOMPTYPE, name, layout, &
                      mtype, config=config, grid=grid, clock=clock, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateNew%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompCreate - Create a new Component from a Config file

! !INTERFACE:
      ! Private name; call using ESMF_GridCompCreate()      
      function ESMF_GridCompCreateConf(name, layout, mtype, grid, clock, &
                                                        config, configfile, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateConf
!
! !ARGUMENTS:
      !external :: services
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(inout), optional :: clock
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
!   \item[{[name]}]
!    Component name.
!   \item[{[layout]}]
!    Component layout.
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[{[grid]}]
!    Default grid associated with this component.
!   \item[{[clock]}]
!    Private clock associated with this component.
!   \item[{[config]}]
!    Already created {\tt Config} object.   If specified, takes
!    priority over filename.
!   \item[{[configfile]}]
!    Component-specific configuration filename. 
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_GridCompCreateConf%compp)
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
          print *, "ERROR in ESMF_GridComponentCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_GRIDCOMPTYPE, name, layout, &
                                mtype=mtype, configfile=configfile, &
                                config=config, grid=grid, clock=clock, &
                                rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateConf%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateConf
    

#ifdef ESMF_ENABLE_VM
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompCreate - Create a new Component with VM enabled

! !INTERFACE:
      ! Private name; call using ESMF_GridCompCreate()      
      function ESMF_GridCompCreateVM(vm, &
        name, layout, mtype, grid, clock, config, configfile, petlist, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateVM
!
! !ARGUMENTS:
      !external :: services
      type(ESMF_VM),        intent(in)              :: vm
      character(len=*),     intent(in),    optional :: name
      type(ESMF_DELayout),  intent(in),    optional :: layout
      type(ESMF_ModelType), intent(in),    optional :: mtype 
      type(ESMF_Grid),      intent(in),    optional :: grid
      type(ESMF_Clock),     intent(inout), optional :: clock
      type(ESMF_Config),    intent(in),    optional :: config
      character(len=*),     intent(in),    optional :: configfile
      integer,              intent(in),    optional :: petlist(:)
      integer,              intent(out),   optional :: rc 
!
! !DESCRIPTION:
!  Create a new Component and set the decomposition characteristics.
!
!  The return value is a new Component.
!    
!  The arguments are:
!  \begin{description}
!   \item[name]
!    Component name.
!   \item[{[name]}]
!    Component name.
!   \item[{[layout]}]
!    Component layout.
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[{[grid]}]
!    Default grid associated with this component.
!   \item[{[clock]}]
!    Private clock associated with this component.
!   \item[{[config]}]
!    Already created {\tt Config} object.   If specified, takes
!    priority over filename.
!   \item[{[configfile]}]
!    Component-specific configuration filename. 
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_GridCompCreateVM%compp)
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
          print *, "ERROR in ESMF_GridComponentCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_GRIDCOMPTYPE, name, layout, &
                                mtype=mtype, configfile=configfile, &
                                config=config, grid=grid, clock=clock, &
                                vm=vm, petlist=petlist, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateVM%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateVM
#endif    

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompDestroy - Release resources for a Component

! !INTERFACE:
      subroutine ESMF_GridCompDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: component
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Component}.
!
!     The arguments are:
!     \begin{description}
!     \item[component]
!      Destroy contents of this {\tt Component}.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

        end subroutine ESMF_GridCompDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompFinalize - Call the Component's finalize routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompFinalize(component, importstate, &
                                           exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp) :: component
      type (ESMF_State), intent(inout), optional :: importstate
      type (ESMF_State), intent(inout), optional :: exportstate
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
!   \item[{[importstate]}]  
!    Import data for finalize.
!   \item[{[exportstate]}]  
!     Export data for finalize.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase finalize, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompFinalize(component%compp, importstate, exportstate, &
                                              clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_GridCompFinalize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGet - Query a Component for information
!
! !INTERFACE:
      subroutine ESMF_GridCompGet(component, name, layout, mtype, grid, clock, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(in) :: component
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      type(ESMF_ModelType), intent(out), optional :: mtype 
      type(ESMF_Grid), intent(out), optional :: grid
      type(ESMF_Clock), intent(out), optional :: clock
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
!  The arguments are:
!  \begin{description}
!   \item[name]
!    Component to query.
!   \item[{[name]}]
!    Component name.
!   \item[{[layout]}]
!    Component layout.
!   \item[{[mtype]}]
!    Component Model Type, where model includes {\tt ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER}.
!   \item[{[grid]}]
!    Default grid associated with this component.
!   \item[{[clock]}]
!    Component-specific clock object.
!   \item[{[configfile]}]
!    Component-specific configuration object.
!   \item[{[config]}]
!    Already created {\tt Config} object. If specified, takes
!    priority over filename.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!

!
!EOP
! !REQUIREMENTS:

        call ESMF_CompGet(component%compp, name, layout, &
                          mtype=mtype, grid=grid, clock=clock, &
                          configfile=configfile, config=config, rc=rc)

        end subroutine ESMF_GridCompGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompInitialize - Call the Component's initialize routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompInitialize(component, importstate, &
                                           exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp) :: component
      type (ESMF_State), intent(inout), optional :: importstate
      type (ESMF_State), intent(inout), optional :: exportstate
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
!   \item[component]
!    Component to call Initialization routine for.
!   \item[{[importstate]}]  
!    Import data for initialization.
!   \item[{[exportstate]}]  
!    Export data for initialization.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase init, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompInitialize(component%compp, importstate, exportstate, &
                                              clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_GridCompInitialize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GridCompPrint - Print the contents of a Component
!
! !INTERFACE:
      subroutine ESMF_GridCompPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
!
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component to print.
!   \item[{[options]}]
!    Print options.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

       print *, "Gridded Component:"
       call ESMF_CompPrint(component%compp, options, rc)

       end subroutine ESMF_GridCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompReadRestart - Call the Component's restore routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompReadRestart(component, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp), intent(inout) :: component
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component to call ReadRestart routine for.
!   \item[{[iospec]}]  
!    I/O options.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase finalize, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompReadRestart(component%compp, iospec, clock, phase, rc)

        end subroutine ESMF_GridCompReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompRun - Call the Component's run routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompRun(component, importstate, &
                                           exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp) :: component
      type (ESMF_State), intent(inout), optional :: importstate
      type (ESMF_State), intent(inout), optional :: exportstate
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
!   \item[component]
!    Component to call Run routine for.
!   \item[{[importstate]}]  
!    Import data for run.
!   \item[{[exportstate]}]  
!     Export data for run.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase run, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompRun(component%compp, importstate, exportstate, &
                                             clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_GridCompRun


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSet - Set or reset information about the Component
!
! !INTERFACE:
      subroutine ESMF_GridCompSet(component, name, layout, mtype, grid, clock, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: component
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(in), optional :: clock
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
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component to set value for.
!   \item[{[name]}]
!    Component name.
!   \item[{[layout]}]
!    Component layout.
!   \item[{[mtype]}]
!    Component Model Type, where model includes {\tt ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER}.
!   \item[{[grid]}]
!    Default grid associated with this component.
!   \item[{[clock]}]
!    Private clock associated with this component.
!   \item[{[config]}]
!    Already created {\tt Config} object.   If specified, takes
!    priority over filename.
!   \item[{[configfile]}]
!    Component-specific configuration filename.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompSet(component%compp, name, layout, &
                          mtype=mtype, grid=grid, clock=clock, &
                          configfile=configfile, config=config, rc=rc)

        end subroutine ESMF_GridCompSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompValidate - Ensure the Component is internally consistent
!
! !INTERFACE:
      subroutine ESMF_GridCompValidate(component, options, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component to validate.
!   \item[{[options]}]  
!    Object to be validated.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(component%compp, options, rc)

       ! TODO: also need to validate grid if it's associated here
 
       end subroutine ESMF_GridCompValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompWriteRestart - Call the Component's save routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompWriteRestart(component, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp), intent(inout) :: component
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user save code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component to call WriteRestart routine for.
!   \item[{[iospec]}]  
!    I/O options.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase finalize, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompWriteRestart(component%compp, iospec, clock, phase, rc)

        end subroutine ESMF_GridCompWriteRestart



#ifdef ESMF_ENABLE_VM
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompVMDefMaxThreads - Define a VM for this GridComp

! !INTERFACE:
  subroutine ESMF_GridCompVMDefMaxThreads(component, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: component
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[component] 
!      gridded component object
!     \item[{[max]}] 
!      Maximum threading level
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! call CompClass method
    call ESMF_CompVMDefMaxThreads(component%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompVMDefMaxThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompVMDefMaxThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompVMDefMinThreads - Define a VM for this GridComp

! !INTERFACE:
  subroutine ESMF_GridCompVMDefMinThreads(component, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: component
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[component] 
!      gridded component object
!     \item[{[max]}] 
!      Maximum threading level
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! call CompClass method
    call ESMF_CompVMDefMinThreads(component%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompVMDefMinThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompVMDefMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompVMDefMaxPEs - Define a VM for this GridComp

! !INTERFACE:
  subroutine ESMF_GridCompVMDefMaxPEs(component, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: component
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[component] 
!      gridded component object
!     \item[{[max]}] 
!      Maximum threading level
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! call CompClass method
    call ESMF_CompVMDefMaxPEs(component%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompVMDefMaxPEs error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompVMDefMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompReturn - Wait for a GridComp to return

! !INTERFACE:
  subroutine ESMF_GridCompReturn(component, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in) ::            component
    integer, intent(out), optional  ::            rc           
!
! !DESCRIPTION:
!     Wait for a GridComp to return
!
!     The arguments are:
!     \begin{description}
!     \item[component] 
!      gridded component object
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! call CompClass method
    call ESMF_CompReturn(component%compp, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompReturn error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompReturn
!------------------------------------------------------------------------------
#endif

end module ESMF_GridCompMod

