! $Id: ESMF_Comp.F90,v 1.69 2004/03/05 17:42:11 svasquez Exp $
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
! A blank line to keep protex happy.
!BOP

!EOP
!
! This file contains the Component class definition and all Component
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
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
      use ESMF_ConfigMod
      use ESMF_DELayoutMod
      use ESMF_ClockMod
      use ESMF_GridTypesMod
      use ESMF_StateMod
      
#ifdef ESMF_ENABLE_VM      
      use ESMF_VMMod
#endif
      
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

      character(len=20), parameter :: ESMF_SETINIT         = "ESMF_Initialize"
      character(len=20), parameter :: ESMF_SETRUN          = "ESMF_Run"
      character(len=20), parameter :: ESMF_SETFINAL        = "ESMF_Finalize"
      character(len=20), parameter :: ESMF_SETWRITERESTART = "ESMF_WriteRestart"
      character(len=20), parameter :: ESMF_SETREADRESTART  = "ESMF_ReadRestart"
 
!------------------------------------------------------------------------------
!     ! ESMF Phase number
      integer, parameter :: ESMF_SINGLEPHASE = 0

!------------------------------------------------------------------------------
!     ! ESMF_CompClass
!
!     ! Component internal class data.

      type ESMF_CompClass
      sequence
      private
         type(ESMF_Pointer) :: this       ! C++ ftable pointer - MUST BE FIRST
         type(ESMF_Base) :: base                  ! base class
         type(ESMF_CompType) :: ctype             ! component type
         type(ESMF_Config) :: config              ! configuration object
         type(ESMF_DELayout) :: layout            ! component layout
         type(ESMF_Clock) :: clock                ! private component clock
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
#ifdef ESMF_ENABLE_VM
         type(ESMF_VM) :: vm_parent               ! reference to the parent VM
         integer(ESMF_KIND_I4):: npetlist         ! number of PETs in petlist
         integer(ESMF_KIND_I4),pointer::petlist(:)! list of usble parent PETs 
         type(ESMF_VMPlan) :: vmplan              ! reference to VMPlam
         type(ESMF_Pointer) :: vm_info            ! holding pointer to info
         type(ESMF_Pointer) :: vm_cargo           ! holding pointer to cargo
#endif         
      end type

!------------------------------------------------------------------------------
!     ! wrapper for Component objects going across F90/C++ boundary
      type ESMF_CWrap
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
          type(ESMF_CompClass), pointer :: compp => NULL()
#else
          type(ESMF_CompClass), pointer :: compp
#endif
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
      public ESMF_ModelType, ESMF_ATM, ESMF_LAND, ESMF_OCEAN, &
                             ESMF_SEAICE, ESMF_RIVER, ESMF_OTHER
      public ESMF_SETINIT, ESMF_SETRUN, ESMF_SETFINAL, ESMF_SINGLEPHASE
      
      ! These have to be public so other component types can use them, but 
      !  are not intended to be used outside the Framework code.

      public ESMF_CompClass
      public ESMF_CompType
      public ESMF_APPCOMPTYPE, ESMF_GRIDCOMPTYPE, ESMF_CPLCOMPTYPE 

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_Initialize, ESMF_Finalize
      public ESMF_FrameworkInternalInit   ! should be private to framework

      ! These have to be public so other component types can call them,
      !  but they are not intended to be used outside the Framework code.

      public ESMF_CompConstruct, ESMF_CompDestruct
      public ESMF_CompInitialize, ESMF_CompRun, ESMF_CompFinalize
      public ESMF_CompWriteRestart, ESMF_CompReadRestart
      public ESMF_CompGet, ESMF_CompSet 

      public ESMF_CompValidate, ESMF_CompPrint

#ifdef ESMF_ENABLE_VM
      public ESMF_CompVMDefMaxThreads
      public ESMF_CompVMDefMinThreads
      public ESMF_CompVMDefMaxPEs
      public ESMF_CompReturn
#endif

!EOPI

      public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comp.F90,v 1.69 2004/03/05 17:42:11 svasquez Exp $'
!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare     
!  them as if they were simple integers.

interface operator (.eq.)
 module procedure ESMF_cteq
 module procedure ESMF_mteq
end interface

interface operator (.ne.)
 module procedure ESMF_ctne
 module procedure ESMF_mtne
end interface


!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
! function to compare two ESMF_CompTypes to see if they're the same 

function ESMF_cteq(ct1, ct2)
 logical ESMF_cteq
 type(ESMF_CompType), intent(in) :: ct1, ct2

 ESMF_cteq = (ct1%ctype .eq. ct2%ctype)    
end function

function ESMF_ctne(ct1, ct2)
 logical ESMF_ctne
 type(ESMF_CompType), intent(in) :: ct1, ct2

 ESMF_ctne = (ct1%ctype .ne. ct2%ctype)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_ModelTypes to see if they're the same

function ESMF_mteq(mt1, mt2)
 logical ESMF_mteq
 type(ESMF_ModelType), intent(in) :: mt1, mt2

 ESMF_mteq = (mt1%mtype .eq. mt2%mtype)
end function

function ESMF_mtne(mt1, mt2)
 logical ESMF_mtne
 type(ESMF_ModelType), intent(in) :: mt1, mt2

 ESMF_mtne = (mt1%mtype .ne. mt2%mtype)
end function



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes Component Create/Destroy, Construct/Destruct methods.
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompConstruct - Internal routine to fill in a comp struct

! !INTERFACE:
      subroutine ESMF_CompConstruct(compp, ctype, name, layout, mtype, &
                          dirpath, configfile, config, grid, clock, &
#ifdef ESMF_ENABLE_VM                          
                          vm, petlist, &
#endif                          
                          rc)
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
      type(ESMF_Clock), intent(in), optional :: clock
#ifdef ESMF_ENABLE_VM      
      type(ESMF_VM), intent(in), optional :: vm
      integer(ESMF_KIND_I4), intent(in), optional :: petlist(:)
#endif      
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a new component datatype and fill in the contents.
!
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component internal structure to be filled in.
!   \item[ctype]
!    Component type, where types include: ESMF\_APPCOMP, ESMF\_GRIDCOMP,
!    ESMF\_CPLCOMP.
!   \item[{[name]}]
!    Component name.
!   \item[{[layout]}]
!    Component layout.
!   \item[{[mtype]}]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!   \item[{[dirpath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!   \item[{[configfile]}]
!    File containing configuration information, either absolute filename
!    or relative to {\tt dirpath}.
!   \item[{[config]}]
!    Already created {\tt config} object.
!   \item[{[grid]}]
!    Default {\tt grid} for a Gridded {\tt Component}.
!   \item[{[clock]}]
!    Private {\tt clock} for this {\tt Component}.
!   \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:


        ! Local vars
        integer :: status                            ! local error status
        logical :: rcpresent                         ! did user specify rc?
        character(len=ESMF_MAXSTR) :: fullpath       ! config file + dirpath

        ! Has the Full ESMF Framework initialization been run?
        ! This only happens once per process at the start.
        if (frameworknotinit) then
          call ESMF_Initialize(status)
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

        ! initialize base class, including component name
        call ESMF_BaseCreate(compp%base, "Component", name, 0, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "CompConstruct: Base create error"
          return
        endif

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

        ! sort out what happens if both a already created config object and
        ! a config filename are given.  the current rules are:  a config object
        ! gets priority over a name if both are specified.
        if (present(configfile) .and. present(config)) then
            print *, "Warning: only 1 of Config object or filename should be given."
            print *, "Using Config object; ignoring Config filename."
            compp%config = config

        ! name of a specific config file.  open it and store the config object.
        else if (present(configfile)) then
          compp%configfile = configfile
          compp%config = ESMF_ConfigCreate(status)
          call ESMF_ConfigLoadFile(compp%config, configfile, rc=status)
          ! TODO: rationalize return codes from config with ESMF codes
          if (status .ne. 0) then
              ! try again with the dirpath concatinated on front
              fullpath = trim(dirpath) // '/' // trim(configfile)
              call ESMF_ConfigLoadFile(compp%config, fullpath, rc=status)
              if (status .ne. 0) then
                  print *, "ERROR: loading config file, unable to open either"
                  print *, " name = ", trim(configfile), " or name = ", trim(fullpath)
                  return
              endif
          endif
        else if (present(config)) then
          ! store already opened config object
          compp%config = config
        else
          ! need a way to set config to 0/invalid
          ! compp%config = ?
        endif

        ! default grid for a gridded component
        if (present(grid)) then
          compp%grid = grid
        else
          ! TODO: do we need an "empty grid" object?
        endif

        ! default clock
        if (present(clock)) then
          compp%clock = clock
        else
          !! TODO: Fix this to work
          !!compp%clock = ESMF_NULL_POINTER
        endif

#ifdef ESMF_ENABLE_VM
        ! parent VM
        if (present(vm)) then
          compp%vm_parent = vm
!          print *, 'gjt: ESMF_CompConstruct, setting up compp%vm_parent'
        else
          ! When ESMF is fully VM-enabled this should be an error!
        endif
      
        ! petlist
        if (present(petlist)) then
          compp%npetlist = size(petlist)
          if (size(petlist) > 0) then
            allocate(compp%petlist(size(petlist)))
            compp%petlist = petlist
          endif
!          print *, 'gjt: ESMF_CompConstruct, setting up compp%petlist', &
!            size(petlist)
        else
          compp%npetlist = 0
        endif
      
        ! instantiate a default VMPlan
        if (present(vm)) then
          call ESMF_VMPlanConstruct(compp%vmplan, vm, compp%npetlist, &
            compp%petlist)
        endif
#endif

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
!     \item[compp]
!      Component internal structure to be freed.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

        ! Release attributes and other things on base class
        call ESMF_BaseDestroy(compp%base, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Base Component contents destruction error"
          return
        endif
        
#ifdef ESMF_ENABLE_VM
        ! Deallocate space held for petlist
        if (compp%npetlist > 0) then
          deallocate(compp%petlist)
!          print *, '------------------------- petlist is being deallocated --'
        endif

        ! destruct the VMPlan
        call ESMF_VMPlanDestruct(compp%vmplan)
#endif

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
      recursive subroutine ESMF_CompInitialize(compp, importstate, &
                                                exportstate, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
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
!   \item[compp]
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
!EOPI
! !REQUIREMENTS:


        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        integer :: gde_id                       ! the global DE
        integer :: lde_id                       ! the DE in the subcomp layout
        character(ESMF_MAXSTR) :: cname
        type(ESMF_CWrap) :: compw
        type(ESMF_State) :: is, es
        logical :: isdel, esdel

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
        !print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.
        if (present(importstate)) then
          is = importstate
          isdel = .FALSE.
        else
          ! create an empty state
          is = ESMF_StateCreate(rc=rc)
          isdel = .TRUE.
        endif

        if (present(importstate)) then
          es = exportstate
          esdel = .FALSE.
        else
          ! create an empty state
          es = ESMF_StateCreate(rc=rc)
          esdel = .TRUE.
        endif

        ! and something for clocks?
        if (present(clock)) then
            ! all is well
        else
            print *, "Warning: ESMF Component Initialize called without a clock"
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compw%compp => compp

        ! Set up the arguments, then make the call
        call c_ESMC_FTableSetStateArgs(compp%this, ESMF_SETINIT, phase, &
                                       compw, is, es, clock, status)
          
#ifdef ESMF_ENABLE_VM
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, ESMF_SETINIT, phase, &
          status)
#else
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETINIT, phase, &
          status)
#endif

        if (status .ne. ESMF_SUCCESS) then
          print *, "Component initialization error"
          ! fall thru intentionally
        endif

        ! if we created dummy states, delete them here.
        if (isdel) call ESMF_StateDestroy(is, rc=rc)
        if (esdel) call ESMF_StateDestroy(es, rc=rc)
      
        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompInitialize


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompWriteRestart -- Call the Component's internal save routine

! !INTERFACE:
      recursive subroutine ESMF_CompWriteRestart(compp, iospec, clock, &
                                                                     phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component to call WriteRestart routine for.
!   \item[{[iospec]}]  
!     Controls for how the component's data will be written.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase checkpoint, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
        type(ESMF_CWrap) :: compw

        ! WriteRestart return code; assume failure until success is certain
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
        !print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetIOArgs(compp%this, ESMF_SETWRITERESTART, phase, &
                                                compw, iospec, clock, status)

        ! Call user-defined run routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETWRITERESTART, &
                                                            phase, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component WriteRestart error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompWriteRestart


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompReadRestart -- Call the Component's internal restart routine

! !INTERFACE:
      recursive subroutine ESMF_CompReadRestart(compp, iospec, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user internal restart code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component to call ReadRestart routine for.
!   \item[{[iospec]}]  
!    Controls for how the component's data will be read back.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase restore, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
        type(ESMF_CWrap) :: compw

        ! ReadRestart return code; assume failure until success is certain
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
        !print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetIOArgs(compp%this, ESMF_SETREADRESTART, phase, &
                                                compw, iospec, clock, status)

        ! Call user-defined run routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETREADRESTART, &
                                                           phase, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component ReadRestart error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompReadRestart



!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompFinalize -- Call the Component's finalize routine

! !INTERFACE:
      recursive subroutine ESMF_CompFinalize(compp, importstate, exportstate, &
                                      clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
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
!   \item[compp]
!    Component to call Finalize routine for.
!   \item[{[importstate]}]  
!    Import data for finalize.
!   \item[{[exportstate]}]  
!    Export data for finalize.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase finalize, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHAS} for non-multiples.
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
        type(ESMF_CWrap) :: compw

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
        !print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetStateArgs(compp%this, ESMF_SETFINAL, phase, &
                                compw, importstate, exportstate, clock, status)
        
#ifdef ESMF_ENABLE_VM
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, ESMF_SETFINAL, phase, &
          status)
#else
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETFINAL, phase, &
          status)
#endif

        if (status .ne. ESMF_SUCCESS) then
          print *, "Component finalize error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompFinalize


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompRun -- Call the Component's run routine

! !INTERFACE:
      recursive subroutine ESMF_CompRun(compp, importstate, exportstate, &
                                                            clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
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
!   \item[compp]
!    Component to call Run routine for.
!   \item[{[importstate]}]  
!     Import data for run.
!   \item[{[exportstate]}]  
!     Export data for run.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase run, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
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
        type(ESMF_CWrap) :: compw

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
        !print *, "Global DE ", gde_id, " is ", lde_id, " in this layout"

        ! TODO: handle optional args, do framework setup for this comp.

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetStateArgs(compp%this, ESMF_SETRUN, phase, compw, &
                                       importstate, exportstate, clock, status)
        
#ifdef ESMF_ENABLE_VM
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, ESMF_SETRUN, phase, status)
#else
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETRUN, phase, &
          status)
#endif

        if (status .ne. ESMF_SUCCESS) then
          print *, "Component run error"
          return
        endif

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompRun



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
                                        clock, dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      type(ESMF_ModelType), intent(out), optional :: mtype 
      type(ESMF_Grid), intent(out), optional :: grid
      type(ESMF_Clock), intent(out), optional :: clock
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

        if (present(clock)) then
          clock = compp%clock
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
                                        clock, dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      type(ESMF_ModelType), intent(in), optional :: mtype 
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(in), optional :: clock
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

        if (present(clock)) then
          compp%clock = clock
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
! !IROUTINE: ESMF_CompWrite - Write a Component to disk
!
! !INTERFACE:
      recursive subroutine ESMF_CompWrite(compp, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_CompClass) :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOPI
! !REQUIREMENTS:

!
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_FAILURE
 
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
 
        if (present(rc)) rc = ESMF_FAILURE
 
        end function ESMF_CompRead


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
      recursive subroutine ESMF_CompValidate(compp, options, rc)
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
      recursive subroutine ESMF_CompPrint(compp, options, rc)
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

       if (.not.associated(compp)) then
         print *, "Invalid or uninitialized Component"
         return
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
! !IROUTINE:  ESMF_Initialize - Initialize the ESMF Framework.
!
! !INTERFACE:
      subroutine ESMF_Initialize(rc)
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

      call ESMF_FrameworkInternalInit(ESMF_MAIN_F90, rc)

      end subroutine ESMF_Initialize

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_FrameworkInternalInit - internal routine called by both F90 and C++
!
! !INTERFACE:
      subroutine ESMF_FrameworkInternalInit(lang, rc)
!
! !ARGUMENTS:
      integer, intent(in) :: lang     
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Initialize the ESMF framework.
!
!     \begin{description}
!
!     \item [lang]
!           Flag to say whether main program is F90 or C++.  Affects things
!           related to initialization, such as starting MPI.
!
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

      logical :: rcpresent                       ! Return code present   
      integer :: status
      logical, save :: already_init = .false.    ! Static, maintains state.

      ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if (already_init) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! Initialize the machine model, the comms, etc.
      call ESMF_MachineInitialize(lang, status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error initializing the machine characteristics"
          return
      endif

      ! Create a global DELayout
      GlobalLayout = ESMF_DELayoutCreate(status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error creating global layout"
          return
      endif

#ifdef ESMF_ENABLE_VM
      ! Initialize the VM. This creates the GlobalVM      
      call ESMF_VMInitialize(status);
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error initializing VM"
          return
      endif
#endif

      already_init = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FrameworkInternalInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_Finalize - Clean up and close the ESMF Framework.
!
! !INTERFACE:
      subroutine ESMF_Finalize(rc)
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

      logical :: rcpresent                        ! Return code present   
#ifdef ESMF_ENABLE_VM
      integer :: status
#endif
      logical, save :: already_final = .false.    ! Static, maintains state.

      ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if (already_final) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

#ifdef ESMF_ENABLE_VM
      ! Finalize the VM
      call ESMF_VMFinalize(status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error finalizing VM"
          return
      endif
#endif

      ! Where MPI is shut down, files closed, etc.
      call ESMF_MachineFinalize()

      already_final = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Finalize


#ifdef ESMF_ENABLE_VM
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompVMDefMaxThreads - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompVMDefMaxThreads(compp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type (ESMF_CompClass), pointer ::             compp
    integer(ESMF_KIND_I4), intent(in), optional:: max
    integer(ESMF_KIND_I4), intent(in), optional:: pref_intra_process
    integer(ESMF_KIND_I4), intent(in), optional:: pref_intra_ssi
    integer(ESMF_KIND_I4), intent(in), optional:: pref_inter_ssi
    integer, intent(out), optional ::             rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[max]}] 
!          Maximum threading level
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
    call ESMF_VMPlanMaxThreads(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_VMPlanMaxThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompVMDefMaxThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompVMDefMinThreads - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompVMDefMinThreads(compp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type (ESMF_CompClass), pointer ::             compp
    integer(ESMF_KIND_I4), intent(in), optional:: max
    integer(ESMF_KIND_I4), intent(in), optional:: pref_intra_process
    integer(ESMF_KIND_I4), intent(in), optional:: pref_intra_ssi
    integer(ESMF_KIND_I4), intent(in), optional:: pref_inter_ssi
    integer, intent(out), optional ::             rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[max]}] 
!          Maximum threading level
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
    call ESMF_VMPlanMinThreads(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_VMPlanMinThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompVMDefMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompVMDefMaxPEs - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompVMDefMaxPEs(compp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type (ESMF_CompClass), pointer ::             compp
    integer(ESMF_KIND_I4), intent(in), optional:: max
    integer(ESMF_KIND_I4), intent(in), optional:: pref_intra_process
    integer(ESMF_KIND_I4), intent(in), optional:: pref_intra_ssi
    integer(ESMF_KIND_I4), intent(in), optional:: pref_inter_ssi
    integer, intent(out), optional ::             rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[max]}] 
!          Maximum threading level
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
    call ESMF_VMPlanMaxPEs(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_VMPlanMaxPEs error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompVMDefMaxPEs
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CompReturn - Wait for component to return

! !INTERFACE:
  subroutine ESMF_CompReturn(compp, rc)
!
! !ARGUMENTS:
    type (ESMF_CompClass), pointer ::             compp
    integer, intent(out), optional ::             rc           
!
! !DESCRIPTION:
!     Wait for component to return
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

    ! call into C++ 
    call c_ESMC_CompReturn(compp%vm_parent, compp%vmplan, compp%vm_info, &
      compp%vm_cargo, status)
    if (status .ne. ESMF_SUCCESS) then
      print *, "c_ESMC_CompReturn error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompReturn
!------------------------------------------------------------------------------
#endif

end module ESMF_CompMod

