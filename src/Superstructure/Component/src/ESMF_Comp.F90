! $Id: ESMF_Comp.F90,v 1.40 2003/04/15 16:12:17 cdeluca Exp $
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
      use ESMF_MachineMod
      !use ESMF_ConfigMod
      use ESMF_DELayoutMod
      use ESMF_ClockMod
      use ESMF_GridMod
      use ESMF_StateMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_CompType
!
      type ESMF_CompType
      sequence
      private
        integer :: ctype
      end type

      type(ESMF_CompType), parameter :: ESMF_APPCOMPTYPE = ESMF_CompType(1), &
                                        ESMF_GRIDCOMPTYPE = ESMF_CompType(2), &
                                        ESMF_CPLCOMPTYPE = ESMF_CompType(3)

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
!     ! ESMF Entry Point Names

      character(len=16), parameter :: ESMF_SETINIT  = "ESMF_Initialize"
      character(len=16), parameter :: ESMF_SETRUN   = "ESMF_Run"
      character(len=16), parameter :: ESMF_SETFINAL = "ESMF_Finalize"
 
!------------------------------------------------------------------------------
!     ! ESMF Phase number
      integer, parameter :: ESMF_SINGLEPHASE = 0

!------------------------------------------------------------------------------
!     ! wrapper for statelist objects going across F90/C++ boundary
      type ESMF_SWrap
      sequence
      private
          type(ESMF_State), dimension(:), pointer :: ptr
      end type

      
!------------------------------------------------------------------------------
!     ! Configuration placeholder - TODO: replace with real config object
!
      type ESMF_Config
      sequence
      private
          integer :: dummy
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CompClass
!
!     ! Component internal class data.

      type ESMF_CompClass
      sequence
      private
         type(ESMF_Pointer) :: this        ! C++ ftable pointer - MUST BE FIRST
         type(ESMF_Base) :: base                  ! base class
         type(ESMF_CompType) :: ctype             ! component type
         type(ESMF_Config) :: config              ! configuration object
         type(ESMF_DELayout) :: layout            ! component layout
         logical :: multiphaseinit                ! multiple init, run, final
         integer :: initphasecount                ! max inits, for error check
         logical :: multiphaserun                 ! multiple init, run, final
         integer :: runphasecount                 ! max runs, for error check
         logical :: multiphasefinal               ! multiple init, run, final
         integer :: finalphasecount               ! max finals, for error check
         character(len=ESMF_MAXSTR) :: configfile ! resource filename
         character(len=ESMF_MAXSTR) :: dirpath    ! relative dirname, app only
         type(ESMF_Grid) :: grid                  ! default grid, gcomp only
         type(ESMF_ModelType) :: mtype            ! model type, gcomp only
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridComp
!
!     ! Grid Component wrapper

      type ESMF_GridComp
      sequence
      private
         type(ESMF_CompClass), pointer :: compp      ! common comp section
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CplComp
!
!     ! Component wrapper

      type ESMF_CplComp
      sequence
      private
         type(ESMF_CompClass), pointer :: compp       ! common comp section
      end type

!------------------------------------------------------------------------------
!     ! ESMF_AppComp
!
!     ! Application Component wrapper

      type ESMF_AppComp
      sequence
      private
         type(ESMF_CompClass), pointer :: compp       ! common comp section
      end type

!------------------------------------------------------------------------------
!     ! Private global variables

      ! Has framework init routine been run?
      logical :: frameworknotinit = .true. 
 
      ! A 1 x N global layout, currently only for debugging but maybe
      !  has more uses?
      type(ESMF_DELayout) :: GlobalLayout

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Config   ! TODO: move to its own file 
      public ESMF_AppComp, ESMF_GridComp, ESMF_CplComp
      public ESMF_ModelType, ESMF_ATM, ESMF_LAND, ESMF_OCEAN, &
                             ESMF_SEAICE, ESMF_RIVER, ESMF_OTHER
      public ESMF_SETINIT, ESMF_SETRUN, ESMF_SETFINAL, ESMF_SINGLEPHASE
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FrameworkInitialize, ESMF_FrameworkFinalize

      public ESMF_AppCompCreate, ESMF_GridCompCreate, ESMF_CplCompCreate
      public ESMF_AppCompDestroy, ESMF_GridCompDestroy, ESMF_CplCompDestroy

      public ESMF_AppCompGet, ESMF_GridCompGet, ESMF_CplCompGet
      public ESMF_AppCompSet, ESMF_GridCompSet, ESMF_CplCompSet
 
      public ESMF_AppCompValidate, ESMF_GridCompValidate, ESMF_CplCompValidate
      public ESMF_AppCompPrint, ESMF_GridCompPrint, ESMF_CplCompPrint
 
      ! These do argument processing, layout checking, and then
      !  call the user-provided routines.
      public ESMF_GridCompInitialize, ESMF_CplCompInitialize
      public ESMF_GridCompRun, ESMF_CplCompRun
      public ESMF_GridCompFinalize, ESMF_CplCompFinalize

      ! Other routines the user might request to setup.
      !public ESMF_CompCheckpoint
      !public ESMF_CompRestore
      !public ESMF_CompWrite
      !public ESMF_CompRead
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comp.F90,v 1.40 2003/04/15 16:12:17 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE: ESMF_GridCompCreate - Create a Gridded Component
!
! !INTERFACE:
      interface ESMF_GridCompCreate

! !PRIVATE MEMBER FUNCTIONS:
        !module procedure ESMF_GridCompCreateNew
        module procedure ESMF_GridCompCreateConf

! !DESCRIPTION:
!     This interface provides an entry point for methods that create a 
!     Gridded {\tt Component}.  The difference is whether an already
!     created configuration object is passed in, or a filename of a new
!     config file which needs to be opened.
!

!EOP
      end interface

!------------------------------------------------------------------------------
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

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes Component Create/Destroy, Construct/Destruct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompCreate -- Create a new Component.

! !INTERFACE:
      function ESMF_AppCompCreate(name, layout, dirpath, configfile, rc)
!
! !RETURN VALUE:
      type(ESMF_AppComp) :: ESMF_AppCompCreate
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      character(len=*), intent(in), optional :: dirpath
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
!   \item[{[dirpath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!
!   \item[{[configfile]}]
!    File containing configuration information, either absolute filename
!    or relative to {\tt dirpath}.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic component
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_AppCompCreate%compp)
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
          print *, "ERROR in ESMF_AppComponentCreate: Allocate"
          return
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_APPCOMPTYPE, name, layout, &
                             dirpath=dirpath, configfile=configfile, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif
  

        ! Set return values
        ESMF_AppCompCreate%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_AppCompCreate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompGet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_AppCompGet(component, name, layout, &
                                           dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp), intent(in) :: component
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      character(len=*), intent(out), optional :: dirpath
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
                          dirpath=dirpath, configfile=configfile, &
                          config=config, rc=rc)

        end subroutine ESMF_AppCompGet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompSet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_AppCompSet(component, name, layout, &
                                           dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp), intent(inout) :: component
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      character(len=*), intent(in), optional :: dirpath
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
                          dirpath=dirpath, configfile=configfile, &
                          config=config, rc=rc)

        end subroutine ESMF_AppCompSet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
      subroutine ESMF_AppCompValidate(component, options, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(component%compp, options, rc)
 
       end subroutine ESMF_AppCompValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AppCompPrint -- Print the contents of a Component
!
! !INTERFACE:
      subroutine ESMF_AppCompPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_AppComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
!
!EOP
! !REQUIREMENTS:

       print *, "Application Component:"
       call ESMF_CompPrint(component%compp, options, rc)

       end subroutine ESMF_AppCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompDestroy -- Release resources for a Component

! !INTERFACE:
      subroutine ESMF_AppCompDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp) :: component
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
 
        ! When destroying App component, finalize framework
        call ESMF_FrameworkFinalize(status)

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_AppCompDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompCreateNew -- Create a new Component.

! !INTERFACE:
      function ESMF_GridCompCreateNew(name, layout, mtype, grid, config, rc)
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
!   \item[mtype]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!
!   \item[grid]
!    Default grid associated with this component.
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
                                 mtype, config=config, grid=grid, rc=status)
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
! !IROUTINE: ESMF_GridCompCreateConf -- Create a new Component.

! !INTERFACE:
      function ESMF_GridCompCreateConf(name, layout, mtype, grid, &
                                                    config, configfile, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreateConf
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Grid), intent(in), optional :: grid
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
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!
!   \item[{[grid]}]
!    Default grid associated with this component.
!
!   \item[{[config]}]
!    Already created {\tt Config} object.  
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
        type(ESMF_Config) :: lconfig                     ! config obj
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
   
        !!if (present(name)) print *, "name present"
        !!if (present(layout)) print *, "layout present"
        !!if (present(mtype)) print *, "mtype present"
        !!if (present(grid)) print *, "grid present"
        !!if (present(config)) print *, "config present"
        !!if (present(configfile)) print *, "configfile present"
        !!if (present(rc)) print *, "rc present"

        if (present(configfile)) then
          !lconfig = ESMF_CreateConfig(configfile, rc=status)
        else
          !lconfig = config
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_GRIDCOMPTYPE, name, layout, &
                                             mtype=mtype, grid=grid, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

        ! Set return values
        ESMF_GridCompCreateConf%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreateConf
    

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompInitialize -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_GridCompInitialize(component, importstate, &
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
!
!   \item[component]
!    Component to call Initialization routine for.
!
!   \item[{[importstate]}]  Import data for initialization.
!
!   \item[{[exportstate]}]  Export data for initialization.
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

        call ESMF_CompInitialize(component%compp, ESMF_GRIDCOMPTYPE, &
                    importstate, exportstate, clock=clock, phase=phase, &
                    gcomp=component, rc=rc)

        end subroutine ESMF_GridCompInitialize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompRun -- Call the Component's run routine

! !INTERFACE:
      subroutine ESMF_GridCompRun(component, importstate, &
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
!
!   \item[component]
!    Component to call Run routine for.
!
!   \item[{[importstate]}]  Import data for run.
!
!   \item[{[exportstate]}]  Export data for run.
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

        call ESMF_CompRun(component%compp, ESMF_GRIDCOMPTYPE, importstate, &
                                 exportstate, clock=clock, phase=phase, &
                                 gcomp=component, rc=rc)

        end subroutine ESMF_GridCompRun


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_GridCompGet(component, name, layout, mtype, grid, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(in) :: component
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      type(ESMF_ModelType), intent(out), optional :: mtype 
      type(ESMF_Grid), intent(out), optional :: grid
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
                          mtype=mtype, grid=grid, configfile=configfile, &
                          config=config, rc=rc)

        end subroutine ESMF_GridCompGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_GridCompSet(component, name, layout, mtype, grid, &
                                                       configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: component
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Grid), intent(in), optional :: grid
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
                          mtype=mtype, grid=grid, configfile=configfile, &
                          config=config, rc=rc)

        end subroutine ESMF_GridCompSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompValidate -- Ensure the Component internal data is valid.
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
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(component%compp, options, rc)

       ! TODO: also need to validate grid if it's associated here
 
       end subroutine ESMF_GridCompValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GridCompPrint -- Print the contents of a Component
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
!EOP
! !REQUIREMENTS:

       print *, "Gridded Component:"
       call ESMF_CompPrint(component%compp, options, rc)

       end subroutine ESMF_GridCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompFinalize -- Call the Component's finalize routine

! !INTERFACE:
      subroutine ESMF_GridCompFinalize(component, importstate, &
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
!
!   \item[{[importstate]}]  Import data for finalize.
!
!   \item[{[exportstate]}]  Export data for finalize.
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

        call ESMF_CompFinalize(component%compp, ESMF_GRIDCOMPTYPE, importstate, &
                                 exportstate, clock=clock, phase=phase, &
                                 gcomp=component, rc=rc)

        end subroutine ESMF_GridCompFinalize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompDestroy -- Release resources for a Component

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

        end subroutine ESMF_GridCompDestroy

!------------------------------------------------------------------------------
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
!    Already created {\tt Config} object.  
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
        type(ESMF_Config) :: lconfig                     ! config obj
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
   
        ! TODO: decide what the rules are when both config & configfile
        !   are specified.  the code is currently written to give configfile
        !   priority.
        if (present(configfile)) then
          !lconfig = ESMF_CreateConfig(configfile, rc=status)
        else
          !lconfig = config
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_CPLCOMPTYPE, name, layout, &
                                                     config=lconfig, rc=status)
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
! !IROUTINE: ESMF_CplCompInitialize -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_CplCompInitialize(component, statelist, &
                                                        clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      !type (ESMF_State), intent(inout), optional :: statelist(:)
      type (ESMF_State), intent(inout), optional :: statelist
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
!   %\item[{[statelist]}]  List of import and export states for coupling.
!   \item[{[statelist]}]  
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

        !!if (present(statelist)) print *, "statelist present"
        !!if (present(clock)) print *, "clock present"
        !!if (present(phase)) print *, "phase present"
        !!if (present(rc)) print *, "rc present"

        !!if (present(statelist)) print *, "statelength =", size(statelist)

        !!print *, "calling CompInitialize now, from cplinit"

        call ESMF_CompInitialize(component%compp, ESMF_CPLCOMPTYPE, &
                          statelist=statelist, clock=clock, phase=phase, &
                          ccomp=component, rc=rc)

        end subroutine ESMF_CplCompInitialize


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompRun -- Call the Component's run routine

! !INTERFACE:
      subroutine ESMF_CplCompRun(component, statelist, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      !type (ESMF_State), intent(inout), optional :: statelist(:)
      type (ESMF_State), intent(inout), optional :: statelist
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
!   %\item[{[statelist]}]  List of import and export states for coupling.
!   \item[{[statelist]}]  
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

        call ESMF_CompRun(component%compp, ESMF_CPLCOMPTYPE, &
                          statelist=statelist, clock=clock, phase=phase, &
                          ccomp=component, rc=rc)

        end subroutine ESMF_CplCompRun


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
! !IROUTINE: ESMF_CplCompFinalize -- Call the Component's finalize routine

! !INTERFACE:
      subroutine ESMF_CplCompFinalize(component, statelist, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: component
      !type (ESMF_State), intent(inout), optional :: statelist(:)
      type (ESMF_State), intent(inout), optional :: statelist
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
!   %\item[{[statelist]}]  List of import and export states for coupling.
!   \item[{[statelist]}]  
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

        call ESMF_CompFinalize(component%compp, ESMF_CPLCOMPTYPE, &
                               statelist=statelist, clock=clock, phase=phase, &
                               ccomp=component, rc=rc)

        end subroutine ESMF_CplCompFinalize


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
!BOPI
! !IROUTINE: ESMF_CompConstruct - Internal routine to fill in a comp struct

! !INTERFACE:
      subroutine ESMF_CompConstruct(compp, ctype, name, layout, mtype, &
                                    dirpath, configfile, config, grid, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_CompType), intent(in) :: ctype
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      character(len=*), intent(in), optional :: dirpath
      character(len=*), intent(in), optional :: configfile
      type(ESMF_Config), intent(in), optional :: config
      type(ESMF_Grid), intent(in), optional :: grid
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
!   \item[ctype]
!    Component type.
!
!   \item[{[name]}]
!    Component name.
!
!   \item[{[layout]}]
!    Component layout.
!
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!
!   \item[{[dirpath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!
!   \item[{[configfile]}]
!    File containing configuration information, either absolute filename
!    or relative to {\tt dirpath}.
!
!   \item[{[config]}]
!    Already created {\tt config} object.
!
!   \item[{[grid]}]
!    Default {\tt grid} for a Gridded {\tt Component}.
!
!   \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI
! !REQUIREMENTS:


        ! Local vars
        integer :: status                            ! local error status
        logical :: rcpresent                         ! did user specify rc?

        ! Has the Full ESMF Framework initialization been run?
        ! This only happens once per process at the start.
        if (frameworknotinit) then
          call ESMF_FrameworkInitialize(status)
          if (status .ne. ESMF_SUCCESS) then
            if (present(rc)) rc = ESMF_FAILURE
            return
          endif
          frameworknotinit = .false.    ! only called one time ever.
        endif

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Fill in values

        ! component type
        compp%ctype = ctype

        ! component name (stored in base object)
        call ESMF_SetName(compp%base, name, "Component", status)

        ! either store or create a layout
        if (present(layout)) then
          compp%layout = layout
        else
          ! Create a default 1xN layout over all processors.
          compp%layout = ESMF_DELayoutCreate(rc=status) 
        endif 

        ! for gridded components, the model type it represents
        if (present(mtype)) then
	  compp%mtype = mtype
        else
	  compp%mtype = ESMF_OTHER
        endif

        ! for config files, store a directory path and subsequent opens can
        !  be relative to this or absolute.
        if (present(dirpath)) then
          compp%dirpath = dirpath
        else
          compp%dirpath = "."
        endif

        ! name of a specific config file.  open it and store the config object.
        if (present(configfile)) then
          !compp%config = ESMF_ConfigCreate(configfile, status)
        else
          compp%config%dummy = 0
        endif

        ! store already opened config object
        if (present(config)) then
          compp%config = config
        else
          compp%config%dummy = 0
        endif

        ! default grid for a gridded component
        if (present(grid)) then
          compp%grid = grid
        else
          ! TODO: do we need an "empty grid" object?
        endif

        ! Create an empty subroutine/internal state table.
        call c_ESMC_FTableCreate(compp%this, status) 
        if (status .ne. ESMF_SUCCESS) then
          print *, "CompConstruct: Table create error"
          return
        endif
   
        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompConstruct


!------------------------------------------------------------------------------
!BOPI
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
!EOPI
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
!BOPI
! !IROUTINE: ESMF_CompInitialize -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_CompInitialize(compp, ctype, importstate, exportstate, &
                                      statelist, clock, phase, gcomp, ccomp, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_CompType), intent(in) :: ctype
      type (ESMF_State), intent(inout), optional :: importstate
      type (ESMF_State), intent(inout), optional :: exportstate
      !type (ESMF_State), intent(inout), target, optional :: statelist(:)
      type (ESMF_State), intent(inout), target, optional :: statelist
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_GridComp), optional :: gcomp
      type (ESMF_CplComp), optional :: ccomp
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component to call Initialization routine for.
!
!   \item[ctype]  Component type (Grid, Coupler, or Application).
!
!   \item[{[importstate]}]  Import data for initialization.
!
!   \item[{[exportstate]}]  Export data for initialization.
!
!   %\item[{[statelist]}]  List of import and export states for coupling.
!   \item[{[statelist]}]  
!       State containing list of nested import and export states for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase init, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!
!   \item[{[gcomp]}] If gridded, actual component.  Only one of this and
!      {\tt ccomp} can be specified.
!
!   \item[{[ccomp]}] If coupler, actual component.  Only one of this and
!      {\tt gcomp} can be specified.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI
! !REQUIREMENTS:


        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: gde_id                       ! the global DE
        integer :: lde_id                       ! the DE in the subcomp layout
        character(ESMF_MAXSTR) :: cname
        type(ESMF_SWrap) :: swrap

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! See if this is currently running on a DE which is part of the
        ! proper Layout.
	call ESMF_DELayoutGetDEID(GlobalLayout, gde_id, status)
	call ESMF_DELayoutGetDEID(compp%layout, lde_id, status)
        if (status .ne. ESMF_SUCCESS) then
          ! this is not our DE
          print *, "Global DE ", gde_id, " is not present in this layout"
          if (rcpresent) rc = ESMF_SUCCESS
          return
        endif
        print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Set up the arguments before the call     
        if (compp%ctype%ctype .eq. ESMF_GRIDCOMPTYPE%ctype) then
          call c_ESMC_FTableSetGridArgs(compp%this, ESMF_SETINIT, phase, &
                               gcomp, importstate, exportstate, clock, status)
        else
          !swrap%ptr => statelist
          !call c_ESMC_FTableSetCplArgs(compp%this, ESMF_SETINIT, phase, &
          !                                         ccomp, swrap, clock, status)
          call c_ESMC_FTableSetCplArgs(compp%this, ESMF_SETINIT, phase, &
                                              ccomp, statelist, clock, status)
        endif

        ! Call user-defined init routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETINIT, &
                                                                 phase, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component initialization error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompInitialize


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompRun -- Call the Component's run routine

! !INTERFACE:
      subroutine ESMF_CompRun(compp, ctype, importstate, exportstate, &
                                      statelist, clock, phase, gcomp, ccomp, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_CompType), intent(in) :: ctype
      type (ESMF_State), intent(inout), optional :: importstate
      type (ESMF_State), intent(inout), optional :: exportstate
      !type (ESMF_State), intent(inout), target, optional :: statelist(:)
      type (ESMF_State), intent(inout), target, optional :: statelist
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_GridComp), optional :: gcomp
      type (ESMF_CplComp), optional :: ccomp
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component to call Run routine for.
!
!   \item[ctype]  Component type (Grid, Coupler, or Application).
!
!   \item[{[importstate]}]  Import data for run.
!
!   \item[{[exportstate]}]  Export data for run.
!
!   %\item[{[statelist]}]  List of import and export states for coupling.
!   \item[{[statelist]}]  
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
!EOPI
! !REQUIREMENTS:


        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: gde_id                       ! the global DE
        integer :: lde_id                       ! the DE in the subcomp layout
        character(ESMF_MAXSTR) :: cname
        type(ESMF_SWrap) :: swrap

        ! Run return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! See if this is currently running on a DE which is part of the
        ! proper Layout.
	call ESMF_DELayoutGetDEID(GlobalLayout, gde_id, status)
	call ESMF_DELayoutGetDEID(compp%layout, lde_id, status)
        if (status .ne. ESMF_SUCCESS) then
          ! this is not our DE
          print *, "Global DE ", gde_id, " is not present in this layout"
          if (rcpresent) rc = ESMF_SUCCESS
          return
        endif
        print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Set up the arguments before the call     
        if (compp%ctype%ctype .eq. ESMF_GRIDCOMPTYPE%ctype) then
          call c_ESMC_FTableSetGridArgs(compp%this, ESMF_SETRUN, phase, gcomp, &
                                       importstate, exportstate, clock, status)
        else
          !swrap%ptr => statelist
          !call c_ESMC_FTableSetCplArgs(compp%this, ESMF_SETRUN, phase, &
          !                                         ccomp, swrap, clock, status)
          call c_ESMC_FTableSetCplArgs(compp%this, ESMF_SETRUN, phase, &
                                              ccomp, statelist, clock, status)
        endif

        ! Call user-defined run routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETRUN, &
                                                               phase, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component run error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompRun



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompFinalize -- Call the Component's finalize routine

! !INTERFACE:
      subroutine ESMF_CompFinalize(compp, ctype, importstate, exportstate, &
                                      statelist, clock, phase, gcomp, ccomp, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_CompType), intent(in) :: ctype
      type (ESMF_State), intent(inout), optional :: importstate
      type (ESMF_State), intent(inout), optional :: exportstate
      !type (ESMF_State), intent(inout), target, optional :: statelist(:)
      type (ESMF_State), intent(inout), target, optional :: statelist
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_GridComp), optional :: gcomp
      type (ESMF_CplComp), optional :: ccomp
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalize code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component to call Finalize routine for.
!
!   \item[ctype]  Component type (Grid, Coupler, or Application).
!
!   \item[{[importstate]}]  Import data for finalize.
!
!   \item[{[exportstate]}]  Export data for finalize.
!
!   %\item[{[statelist]}]  List of import and export states for coupling.
!   \item[{[statelist]}]  
!       State containing list of nested import and export states for coupling.
!
!   \item[{[clock]}]  External clock for passing in time information.
!
!   \item[{[phase]}]  If multiple-phase finalize, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHAS} for non-multiples.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI
! !REQUIREMENTS:


        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: gde_id                       ! the global DE
        integer :: lde_id                       ! the DE in the subcomp layout
        character(ESMF_MAXSTR) :: cname
        type(ESMF_SWrap) :: swrap

        ! Finalize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! See if this is currently finalizening on a DE which is part of the
        ! proper Layout.
	call ESMF_DELayoutGetDEID(GlobalLayout, gde_id, status)
	call ESMF_DELayoutGetDEID(compp%layout, lde_id, status)
        if (status .ne. ESMF_SUCCESS) then
          ! this is not our DE
          print *, "Global DE ", gde_id, " is not present in this layout"
          if (rcpresent) rc = ESMF_SUCCESS
          return
        endif
        print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Set up the arguments before the call     
        if (compp%ctype%ctype .eq. ESMF_GRIDCOMPTYPE%ctype) then
          call c_ESMC_FTableSetGridArgs(compp%this, ESMF_SETFINAL, phase, gcomp, &
                                       importstate, exportstate, clock, status)
        else
          !swrap%ptr => statelist
          !call c_ESMC_FTableSetCplArgs(compp%this, ESMF_SETFINAL, phase, &
          !                                        ccomp, swrap, clock, status)
          call c_ESMC_FTableSetCplArgs(compp%this, ESMF_SETFINAL, phase, &
                                              ccomp, statelist, clock, status)
        endif

        ! Call user-defined finalize routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETFINAL, &
                                                               phase, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component finalize error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompFinalize


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query/Set information from/in the component.
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompGet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_CompGet(compp, name, layout, mtype, grid, &
                                             dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      type(ESMF_ModelType), intent(out), optional :: mtype 
      type(ESMF_Grid), intent(out), optional :: grid
      character(len=*), intent(out), optional :: dirpath
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
!EOPI
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

        if (present(name)) then
          call ESMF_GetName(compp%base, name, status)
        endif

        if (present(layout)) then
          layout = compp%layout
        endif

        if (present(mtype)) then
          mtype = compp%mtype
        endif

        if (present(grid)) then
          grid = compp%grid
        endif

        if (present(dirpath)) then
          dirpath = compp%dirpath
        endif

        if (present(configfile)) then
          configfile = compp%configfile
        endif

        if (present(config)) then
          config = compp%config
        endif


        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompSet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_CompSet(compp, name, layout, mtype, grid, &
                                             dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Grid), intent(in), optional :: grid
      character(len=*), intent(in), optional :: dirpath
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
!EOPI
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

        if (present(name)) then
          call ESMF_SetName(compp%base, name, "Component", status)
        endif

        if (present(layout)) then
          compp%layout = layout
        endif

        if (present(mtype)) then
          compp%mtype = mtype
        endif

        if (present(grid)) then
          compp%grid = grid
        endif

        if (present(dirpath)) then
          compp%dirpath = dirpath
        endif

        if (present(configfile)) then
          compp%configfile = configfile
        endif

        if (present(config)) then
          compp%config = config
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
!BOPI
! !IROUTINE: ESMF_CompCheckpoint - Save a Component's state to disk
!
! !INTERFACE:
      subroutine ESMF_CompCheckpoint(compp, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_CompClass):: compp 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!EOPI
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_CompCheckpoint


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompRestore - Restore a Component's state from disk
!
! !INTERFACE:
      function ESMF_CompRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_CompClass) :: ESMF_CompRestore
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
!EOPI
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_CompClass) :: a 

        a%mtype = ESMF_OTHER

        ESMF_CompRestore = a 
 
        end function ESMF_CompRestore


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompWrite - Write a Component to disk
!
! !INTERFACE:
      subroutine ESMF_CompWrite(compp, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_CompClass) :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOPI
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_CompWrite


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompRead - Read a Component from disk
!
! !INTERFACE:
      function ESMF_CompRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_CompClass) :: ESMF_CompRead
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
!EOPI
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_CompClass) :: a

!       this is just to stop compiler warnings
        a%mtype = ESMF_OTHER

        ESMF_CompRead = a 
 
        end function ESMF_CompRead


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
      subroutine ESMF_CompValidate(compp, options, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOPI
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts
       integer :: status                       ! local error status
       logical :: rcpresent                    ! did user specify rc?

       ! Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

       ! Make sure object is internally consistent
       if(present(options)) then
           ! validate object at the requested level
       else
           ! do default validation
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Component validate error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompValidate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_CompPrint -- Print the contents of a Component
!
! !INTERFACE:
      subroutine ESMF_CompPrint(compp, options, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
!
!EOPI
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

       call ESMF_GetName(compp%base, cname, status)
       print *, "  name = ", trim(cname)
       
       call ESMF_DELayoutPrint(compp%layout, "", status)

       ! TODO: add more info here

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompPrint



!------------------------------------------------------------------------------
! 
! ESMF Framework wide initialization routine. Called exactly once per
!  execution by each participating process.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_FrameworkInitialize - Initialize the ESMF Framework.
!
! !INTERFACE:
      subroutine ESMF_FrameworkInitialize(rc)
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Initialize the ESMF framework.
!
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present   
      integer :: status

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize the machine model, the comms, etc.
      call ESMF_MachineInitialize()

      ! Create a global DELayout
      GlobalLayout = ESMF_DELayoutCreate(status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error creating global layout"
          return
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FrameworkInitialize

!------------------------------------------------------------------------------
! !IROUTINE:  ESMF_FrameworkFinalize - Clean up and close the ESMF Framework.
!
! !INTERFACE:
      subroutine ESMF_FrameworkFinalize(rc)
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Finalize the ESMF Framework.
!
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present   

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! currently nothing to do, but here is where MPI could be shut down,
      !  files closed, etc.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FrameworkFinalize

end module ESMF_CompMod

