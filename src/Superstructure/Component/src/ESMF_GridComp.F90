! $Id: ESMF_GridComp.F90,v 1.28 2004/03/19 14:08:28 theurich Exp $
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
      use ESMF_IOSpecMod
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
 
      ! These do argument processing, delayout checking, and then
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
      public ESMF_GridCompSetVMMaxThreads
      public ESMF_GridCompSetVMMinThreads
      public ESMF_GridCompSetVMMaxPEs
      ! Return from user-provided routines
      public ESMF_GridCompWait
#endif
      
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GridComp.F90,v 1.28 2004/03/19 14:08:28 theurich Exp $'

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
! !IROUTINE: ESMF_GridCompCreate - Create a new GridComp

! !INTERFACE:
      ! Private name; call using ESMF_GridCompCreate()      
      function ESMF_GridCompCreateNew(name, delayout, gridcomptype, grid, config, clock, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_DELayout), intent(in) :: delayout
      type(ESMF_GridCompType), intent(in) :: gridcomptype 
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Config), intent(in) :: config
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_GridComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[name]
!    GridComp name.
!   \item[layout]
!    GridComp delayout.
!   \item[gridcomptype]
!    GridComp Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[grid]
!    Default grid associated with this gridcomp.
!   \item[config]
!    GridComp-specific configuration object.  
!   \item[clock]
!    GridComp-specific clock object.  
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
          print *, "ERROR in ESMF_GridGridCompCreate: Allocate"
          return
        endif

        ! Call construction method to initialize gridcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_GRID, name, delayout, &
                      gridcomptype, config=config, grid=grid, clock=clock, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "GridComp construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateNew%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompCreate - Create a new GridComp from a Config file

! !INTERFACE:
      ! Private name; call using ESMF_GridCompCreate()      
      function ESMF_GridCompCreateConf(name, delayout, gridcomptype, grid, &
        clock, config, configFile, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateConf
!
! !ARGUMENTS:
      !external :: services
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: delayout
      type(ESMF_GridCompType), intent(in), optional :: gridcomptype 
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(inout), optional :: clock
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configFile
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_GridComp} using a configuration file
!  and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[name]}]
!    GridComp name.
!   \item[{[layout]}]
!    GridComp delayout.
!   \item[{[gridcomptype]}]
!    GridComp Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[{[grid]}]
!    Default grid associated with this gridcomp.
!   \item[{[clock]}]
!    Private clock associated with this gridcomp.
!   \item[{[config]}]
!    Already created {\tt Config} object.   If specified, takes
!    priority over filename.
!   \item[{[configFile]}]
!    GridComp-specific configuration filename. 
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
          print *, "ERROR in ESMF_GridGridCompCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize gridcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_GRID, name, delayout, &
                                gridcomptype=gridcomptype, &
                                configFile=configFile, &
                                config=config, grid=grid, clock=clock, &
                                rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "GridComp construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateConf%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateConf
    

#ifdef ESMF_ENABLE_VM
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompCreate - Create a new GridComp with VM enabled

! !INTERFACE:
      ! Private name; call using ESMF_GridCompCreate()      
      function ESMF_GridCompCreateVM(vm, &
        name, delayout, gridcomptype, grid, clock, config, configFile, &
        petList, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateVM
!
! !ARGUMENTS:
      !external :: services
      type(ESMF_VM),        intent(in)              :: vm
      character(len=*),     intent(in),    optional :: name
      type(ESMF_DELayout),  intent(in),    optional :: delayout
      type(ESMF_GridCompType), intent(in),    optional :: gridcomptype 
      type(ESMF_Grid),      intent(in),    optional :: grid
      type(ESMF_Clock),     intent(inout), optional :: clock
      type(ESMF_Config),    intent(in),    optional :: config
      character(len=*),     intent(in),    optional :: configFile
      integer,              intent(in),    optional :: petList(:)
      integer,              intent(out),   optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_GridComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[name]
!    GridComp name.
!   \item[{[name]}]
!    GridComp name.
!   \item[{[layout]}]
!    GridComp delayout.
!   \item[{[gridcomptype]}]
!    GridComp model type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[{[grid]}]
!    Default grid associated with this gridcomp.
!   \item[{[clock]}]
!    Private clock associated with this gridcomp.
!   \item[{[config]}]
!    Already created {\tt Config} object.   If specified, takes
!    priority over filename.
!   \item[{[configFile]}]
!    GridComp-specific configuration filename. 
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
          print *, "ERROR in ESMF_GridGridCompCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize gridcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_GRID, name, delayout, &
                                gridcomptype=gridcomptype, &
                                configFile=configFile, &
                                config=config, grid=grid, clock=clock, &
                                vm=vm, petList=petList, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "GridComp construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateVM%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateVM
#endif    

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompDestroy - Release resources for a GridComp

! !INTERFACE:
      subroutine ESMF_GridCompDestroy(gridcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with the {\tt gridcomp}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp]
!      Destroy contents of this {\tt GridComp}.
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
        if (.not.associated(gridcomp%compp)) then
          print *, "GridComp already destroyed"
          return
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(gridcomp%compp, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "GridComp contents destruction error"
          return
        endif

        ! Deallocate the gridcomp struct itself
        deallocate(gridcomp%compp, stat=status)
        if (status .ne. 0) then
          print *, "GridComp contents destruction error"
          return
        endif
        nullify(gridcomp%compp)
 
        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_GridCompDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompFinalize - Call the GridComp's finalize routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompFinalize(gridcomp, importState, &
                                           exportState, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp) :: gridcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalize code for {\tt gridcomp}.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[gridcomp]
!    GridComp to call Finalize routine for.
!   \item[{[importState]}]  
!    Import data for finalize.
!   \item[{[exportState]}]  
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

        call ESMF_CompFinalize(gridcomp%compp, importState, exportState, &
                                              clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_GridCompFinalize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGet - Query a GridComp for information
!
! !INTERFACE:
      subroutine ESMF_GridCompGet(gridcomp, name, delayout, gridcomptype, grid, clock, &
                                                       configFile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(in) :: gridcomp
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: delayout
      type(ESMF_GridCompType), intent(out), optional :: gridcomptype 
      type(ESMF_Grid), intent(out), optional :: grid
      type(ESMF_Clock), intent(out), optional :: clock
      character(len=*), intent(out), optional :: configFile
      type(ESMF_Config), intent(out), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the gridcomp.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the gridcomp input are optional 
!      to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[name]
!    GridComp to query.
!   \item[{[name]}]
!    GridComp name.
!   \item[{[layout]}]
!    GridComp delayout.
!   \item[{[gridcomptype]}]
!    GridComp Model Type, where model includes {\tt ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER}.
!   \item[{[grid]}]
!    Default grid associated with this gridcomp.
!   \item[{[clock]}]
!    GridComp-specific clock object.
!   \item[{[configFile]}]
!    GridComp-specific configuration object.
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

        call ESMF_CompGet(gridcomp%compp, name, delayout, &
                          gridcomptype=gridcomptype, grid=grid, clock=clock, &
                          configFile=configFile, config=config, rc=rc)

        end subroutine ESMF_GridCompGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompInitialize - Call the GridComp's initialize routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompInitialize(gridcomp, importState, &
                                           exportState, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp) :: gridcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a gridcomp.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to call Initialization routine for.
!   \item[{[importState]}]  
!    Import data for initialization.
!   \item[{[exportState]}]  
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

        call ESMF_CompInitialize(gridcomp%compp, importState, exportState, &
                                              clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_GridCompInitialize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GridCompPrint - Print the contents of a GridComp
!
! !INTERFACE:
      subroutine ESMF_GridCompPrint(gridcomp, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gridcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about an {\tt ESMF\_GridComp}.
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to print.
!   \item[{[options]}]
!    Print options.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

       print *, "Gridded GridComp:"
       call ESMF_CompPrint(gridcomp%compp, options, rc)

       end subroutine ESMF_GridCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompReadRestart - Call the GridComp's restore routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompReadRestart(gridcomp, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp), intent(inout) :: gridcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for a {\tt gridcomp}.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to call ReadRestart routine for.
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

        call ESMF_CompReadRestart(gridcomp%compp, iospec, clock, phase, rc)

        end subroutine ESMF_GridCompReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompRun - Call the GridComp's run routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompRun(gridcomp, importState, &
                                           exportState, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp) :: gridcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for the {\tt gridcomp}.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to call Run routine for.
!   \item[{[importState]}]  
!    Import data for run.
!   \item[{[exportState]}]  
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

        call ESMF_CompRun(gridcomp%compp, importState, exportState, &
                                             clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_GridCompRun


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSet - Set or reset information about the GridComp
!
! !INTERFACE:
      subroutine ESMF_GridCompSet(gridcomp, name, delayout, gridcomptype, grid, clock, &
                                                       configFile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: delayout
      type(ESMF_GridCompType), intent(in), optional :: gridcomptype 
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(in), optional :: clock
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Config), intent(in), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets or resets information about the {\tt gridcomp}.  When the caller
!      only wants to set a single value specify the argument by name.
!      All the arguments after the {\tt gridcomp} input are optional 
!      to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to set value for.
!   \item[{[name]}]
!    GridComp name.
!   \item[{[layout]}]
!    GridComp delayout.
!   \item[{[gridcomptype]}]
!    GridComp Model Type, where model includes {\tt ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER}.
!   \item[{[grid]}]
!    Default grid associated with this gridcomp.
!   \item[{[clock]}]
!    Private clock associated with this gridcomp.
!   \item[{[config]}]
!    Already created {\tt ESMF\_Config} object.   If specified, takes
!    priority over filename.
!   \item[{[configFile]}]
!    GridComp-specific configuration filename.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompSet(gridcomp%compp, name, delayout, &
                          gridcomptype=gridcomptype, grid=grid, clock=clock, &
                          configFile=configFile, config=config, rc=rc)

        end subroutine ESMF_GridCompSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompValidate - Ensure the GridComp is internally consistent
!
! !INTERFACE:
      subroutine ESMF_GridCompValidate(gridcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gridcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure {\tt gridcomp} is valid.
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to validate.
!   \item[{[options]}]  
!    Object to be validated.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(gridcomp%compp, options, rc)

       ! TODO: also need to validate grid if it's associated here
 
       end subroutine ESMF_GridCompValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompWriteRestart - Call the GridComp's save routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompWriteRestart(gridcomp, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_GridComp), intent(inout) :: gridcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user save code for {\tt gridcomp}.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    GridComp to call WriteRestart routine for.
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

        call ESMF_CompWriteRestart(gridcomp%compp, iospec, clock, phase, rc)

        end subroutine ESMF_GridCompWriteRestart



#ifdef ESMF_ENABLE_VM
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompSetVMMaxThreads - Define a VM for this GridComp

! !INTERFACE:
  subroutine ESMF_GridCompSetVMMaxThreads(gridcomp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
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
!     \item[gridcomp] 
!      gridded gridcomp object
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
    call ESMF_CompSetVMMaxThreads(gridcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompSetVMMaxThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompSetVMMaxThreads

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompSetVMMinThreads - Define a VM for this GridComp

! !INTERFACE:
  subroutine ESMF_GridCompSetVMMinThreads(gridcomp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
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
!     \item[gridcomp] 
!      gridded gridcomp object
!     \item[{[max]}] 
!      Maximum number of PEs per PET
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
    call ESMF_CompSetVMMinThreads(gridcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompSetVMMinThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompSetVMMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompSetVMMaxPEs - Define a VM for this GridComp

! !INTERFACE:
  subroutine ESMF_GridCompSetVMMaxPEs(gridcomp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
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
!     \item[gridcomp] 
!      gridded gridcomp object
!     \item[{[max]}] 
!      Maximum number of PEs per PET
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
    call ESMF_CompSetVMMaxPEs(gridcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompSetVMMaxPEs error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompSetVMMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridCompWait - Wait for a GridComp to return

! !INTERFACE:
  subroutine ESMF_GridCompWait(gridcomp, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in) ::            gridcomp
    integer, intent(out), optional  ::            rc           
!
! !DESCRIPTION:
!     Wait for an {\tt ESMF\_GridComp} to return.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp] 
!      gridded gridcomp object
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

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
    call ESMF_CompWait(gridcomp%compp, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompWait error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompWait
!------------------------------------------------------------------------------
#endif

end module ESMF_GridCompMod

