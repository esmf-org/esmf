! $Id: ESMF_CplComp.F90,v 1.17 2004/03/02 16:52:48 cdeluca Exp $
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
      !private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_CompClass), pointer :: compp => NULL()
#else
         type(ESMF_CompClass), pointer :: compp 
#endif
      end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CplComp

!------------------------------------------------------------------------------
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
      public ESMF_CplCompWriteRestart
      public ESMF_CplCompReadRestart
      !public ESMF_CplCompWrite
      !public ESMF_CplCompRead

      !public operator(.eq.), operator(.ne.), assignment(=)

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_CplComp.F90,v 1.17 2004/03/02 16:52:48 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
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

!EOPI
      end interface

!------------------------------------------------------------------------------
! out for now.
!      interface assignment(=)
!        module procedure ESMF_cpas
!      end interface

!==============================================================================

      contains

!==============================================================================

!subroutine ESMF_cpas(lval, rval)
! type(ESMF_CompClass), intent(out) :: lval
! type(ESMF_CplClass), intent(in) :: rval
!
! compp = rval%compp
!end subroutine


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a new Component

! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateNew(name, layout, config, clock, rc)
!
! !RETURN VALUE:
      ! Private name; call using ESMF_CplCompCreate()      
      type(ESMF_CplComp) :: ESMF_CplCompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_DELayout), intent(in) :: layout
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
!   \item[clock]
!    Component-specific clock object.  
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
                                    config=config, clock=clock, rc=status)
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
! !IROUTINE: ESMF_CplCompCreate - Create a new Component from a Config file

! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateConf(name, layout, config, configfile, &
                                      clock, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateConf
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configfile
      type(ESMF_Clock), intent(in), optional :: clock
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
!   \item[{[clock]}]
!    Component-specific clock.
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
                                configfile=configfile, config=config, &
                                clock=clock, rc=status)
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
! !IROUTINE: ESMF_CplCompDestroy - Release resources for a Component

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

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompFinalize - Call the Component's finalize routine

! !INTERFACE:
    recursive subroutine ESMF_CplCompFinalize(component, importstate, &
                                              exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
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
!
!   \item[{[importstate]}]
!       ESMF\_State containing import data for coupling.
!
!   \item[{[exportstate]}]
!       ESMF\_State containing export data for coupling.
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

        call ESMF_CompFinalize(component%compp, importstate=importstate, &
                      exportstate=exportstate, clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompFinalize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompGet - Query a Component for information
!
! !INTERFACE:
      subroutine ESMF_CplCompGet(component, name, layout, clock, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(in) :: component
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
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
!  
!   \item[component]
!    Component to query.
!
!   \item[{[name]}]
!    Component name.
!    
!   \item[{[layout]}]
!    Component layout.
!    
!   \item[{[clock]}]
!    Component-specific clock.
!
!   \item[{[configfile]}]
!    Component-specific configuration filename.
!
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
!    
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompGet(component%compp, name, layout, clock=clock, &
                          configfile=configfile, config=config, rc=rc)

        end subroutine ESMF_CplCompGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompInitialize - Call the Component's initialize routine

! !INTERFACE:
      recursive subroutine ESMF_CplCompInitialize(component, importstate, &
                                                  exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
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
!  
!   \item[component]
!    Component to call Initialization routine for.
!
!   \item[{[importstate]}]  
!       ESMF\_State containing source data for coupling.
!
!   \item[{[exportstate]}]  
!       ESMF\_State containing destination data for coupling.
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

        call ESMF_CompInitialize(component%compp, importstate=importstate, &
                                 exportstate=exportstate, clock=clock,     &
                                 phase=phase, rc=rc)

        end subroutine ESMF_CplCompInitialize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CplCompPrint - Print the contents of a Component
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
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to print.
!
!   \item[{[options]}]
!    Options on print.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

       print *, "Coupler Component:"
       call ESMF_CompPrint(component%compp, options, rc)

       end subroutine ESMF_CplCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompReadRestart -- Call the Component's restore routine

! !INTERFACE:
     recursive subroutine ESMF_CplCompReadRestart(component, iospec, clock, phase, rc)
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
!    Component to call ReadRestart routine for.
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

        call ESMF_CompReadRestart(component%compp, iospec, clock, phase, rc)

        end subroutine ESMF_CplCompReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompRun - Call Component run routine with two States

! !INTERFACE:
    recursive subroutine ESMF_CplCompRun(component, importstate, exportstate, &
                                                                  clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
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
!  
!   \item[component]
!    Component to call Run routine for.
!
!   \item[{[importstate]}]
!       ESMF\_State containing import data for coupling.
!
!   \item[{[exportstate]}]
!       ESMF\_State containing export data for coupling.
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

        call ESMF_CompRun(component%compp, importstate=importstate,  &
                      exportstate=exportstate, clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompRun


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSet - Set or reset information about the Component
!
! !INTERFACE:
      subroutine ESMF_CplCompSet(component, name, layout, clock, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: component
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
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
!
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to set information for. 
!
!   \item[{[name]}]
!    Component name.
!
!   \item[{[layout]}]
!    Component layout.
!
!   \item[{[clock]}]
!    Component-specific clock.
!
!   \item[{[configfile]}]
!    Component-specific configuration filename.
!
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompSet(component%compp, name, layout, clock=clock, &
                          configfile=configfile, config=config, rc=rc)

        end subroutine ESMF_CplCompSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompValidate -- Ensure the Component is internally consistent
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
!
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to validate.
!
!   \item[{[options]}]
!    Object to be validated.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(component%compp, options, rc)
 
       end subroutine ESMF_CplCompValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompWriteRestart -- Call the Component's checkpoint routine

! !INTERFACE:
    recursive subroutine ESMF_CplCompWriteRestart(component, iospec, clock, phase, rc)
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
!    Component to call WriteRestart routine for.
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

        call ESMF_CompWriteRestart(component%compp, iospec, clock, phase, rc)

        end subroutine ESMF_CplCompWriteRestart




end module ESMF_CplCompMod

