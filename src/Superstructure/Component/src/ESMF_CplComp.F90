! $Id: ESMF_CplComp.F90,v 1.51 2004/12/15 17:41:31 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_CplComp.F90"
!
!     ESMF Coupler Cplcomp module
      module ESMF_CplCompMod
!
!==============================================================================
!
! This file contains the Coupler Cplcomp class definition and all 
!   Coupler Cplcomp class methods.
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
! {\tt ESMF\_CplComp} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_VMTypesMod
      use ESMF_VMBaseMod
      use ESMF_VMCommMod
      use ESMF_LogErrMod
      use ESMF_ConfigMod
      use ESMF_ClockMod
      use ESMF_GridMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      use ESMF_CompMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CplCompCreate
      public ESMF_CplCompDestroy

      public ESMF_CplCompGet
      public ESMF_CplCompSet
 
      public ESMF_CplCompValidate
      public ESMF_CplCompPrint
 
      ! These do argument processing and then call the user-provided routines.
      public ESMF_CplCompInitialize
      public ESMF_CplCompRun
      public ESMF_CplCompFinalize

      ! Other routines the user might request to setup.
      public ESMF_CplCompWriteRestart
      public ESMF_CplCompReadRestart
      !public ESMF_CplCompWrite
      !public ESMF_CplCompRead

      ! Procedures for VM-enabled mode      
      public ESMF_CplCompSetVMMaxThreads
      public ESMF_CplCompSetVMMinThreads
      public ESMF_CplCompSetVMMaxPEs
      ! Return from user-provided routines
      public ESMF_CplCompWait

      !public operator(.eq.), operator(.ne.), assignment(=)

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_CplComp.F90,v 1.51 2004/12/15 17:41:31 nscollins Exp $'

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
        module procedure ESMF_CplCompCreateVM
        module procedure ESMF_CplCompCreateGPar
        module procedure ESMF_CplCompCreateCPar

! !DESCRIPTION:
!     This interface provides an entry point for methods that create an 
!     {\tt ESMF\_CplComp}.  The various varieties allow the resources
!     to default, to be specified explicitly, or inherited from a parent
!     component.
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreateNew"
!BOPI
! !IROUTINE: ESMF_CplCompCreate - Create a new CplComp
!
! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateNew(name, config, clock, rc)
!
! !RETURN VALUE:
      ! Private name; call using ESMF_CplCompCreate()      
      type(ESMF_CplComp) :: ESMF_CplCompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_Config), intent(in) :: config
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_CplComp}, specifying all arguments.
!
!  The return value is the new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[name]
!    Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!    from within the {\tt ESMF\_CplComp} code once the initialization routine
!    is called.
!   \item[config]
!    An already-created {\tt ESMF\_Config} configuration object 
!    from which the new {\tt ESMF\_CplComp}
!    can read in namelist-type information to set parameters for this run.
!   \item[clock]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
!    This should
!    not be the parent component clock, which should be maintained and passed
!    down to the initialize/run/finalize routines separately.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                               ! local error status

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateNew%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
                                       ESMF_CONTEXT, rc)) return

        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                    config=config, clock=clock, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CplCompCreateNew%compp => compclass
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateNew


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreateConf"
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a new CplComp from a Config file
!
! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateConf(name, config, configFile, clock, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateConf
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: name
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Clock), intent(in), optional :: clock
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_CplComp}, specifying optional configuration
!  file information.
!
!  The return value is the new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[name]}]
!    Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!    from within the {\tt ESMF\_CplComp} code once the initialization routine
!    is called.
!   \item[{[config]}]
!    An already-created {\tt ESMF\_Config} configuration object 
!    from which the new {\tt ESMF\_CplComp}
!    can read in namelist-type information to set parameters for this run.
!    If both are specified, this object takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    The filename of an {\tt ESMF\_Config} format file.  
!    If specified, this file is opened, an {\tt ESMF\_Config} configuration
!    object is created for the file 
!    and attached to the new {\tt ESMF\_CplComp}.  
!    The user can call
!    {\tt ESMF\_CplCompGet()} to get and use the object.
!    If both are specified, the {\tt config} object takes priority 
!    over this one.
!   \item[{[clock]}]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
!    This should
!    not be the parent component clock, which should be maintained and passed
!    down to the initialize/run/finalize routines separately.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                                ! local error localrc

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateConf%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
                                       ESMF_CONTEXT, rc)) return
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                clock=clock, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CplCompCreateConf%compp => compclass
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateConf
    

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreateVM"
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a new CplComp with VM enabled
!
! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateVM(vm, name, config, configFile, &
                                    clock, petList, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateVM
!
! !ARGUMENTS:
      type(ESMF_VM),     intent(in)            :: vm
      character(len=*),  intent(in),  optional :: name
      type(ESMF_Config), intent(in),  optional :: config
      character(len=*),  intent(in),  optional :: configFile
      type(ESMF_Clock),  intent(in),  optional :: clock
      integer,           intent(in),  optional :: petList(:)
      integer,           intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_CplComp}, setting the resources explicitly.
!
!  The return value is the new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[vm]
!    {\tt ESMF\_VM} object for the parent component out of which 
!    this {\tt ESMF\_CplCompCreate()} call is issued.
!    This will become the parent {\tt ESMF\_VM} 
!    of the newly created {\tt ESMF\_CplComp}.
!   \item[{[name]}]
!    Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!    from within the {\tt ESMF\_CplComp} code once the initialization routine
!    is called.
!   \item[{[config]}]
!    An already-created {\tt ESMF\_Config} configuration object 
!    from which the new component
!    can read in namelist-type information to set parameters for this run.
!    If both are specified, this object takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    The filename of an {\tt ESMF\_Config} format file.  
!    If specified, this file is opened, an {\tt ESMF\_Config} configuration
!    object is created for the file, and attached to the new component.  
!    The user can call {\tt ESMF\_CplCompGet()} to get and use the object.
!    If both are specified, the {\tt config} object takes priority 
!    over this one.
!   \item[{[clock]}]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
!    This should
!    not be the parent component clock, which should be maintained and passed
!    down to the initialize/run/finalize routines separately.
!   \item[{[petList]}]
!    List of {\tt PET}s in the given {\tt ESMF\_VM} that the parent 
!    component is giving to the created child 
!    component. If {\tt petList} is not specified all of the 
!    parents {\tt PET}s will be given to the child component.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                                ! local error localrc

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateVM%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
                                       ESMF_CONTEXT, rc)) return
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                clock=clock, vm=vm, petList=petList, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CplCompCreateVM%compp => compclass
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateVM


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreateGPar"
!BOPI
! !IROUTINE: ESMF_CplCompCreate - Create a new CplComp from a Parent Component
!
! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateGPar(parent, name, config, &
                                      configFile, clock, vm, petList, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateGPar
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(in) :: parent
      character(len=*), intent(in), optional :: name
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Clock), intent(in), optional :: clock
      type(ESMF_VM), intent(in), optional :: vm
      integer, intent(in),  optional :: petList(:)
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_CplComp}, specifying the resources of the parent
!  component as a default, with the option to select specific subsets of
!  those resources to give to the child component.
!
!  The return value is the new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[parent]
!    The parent component object.  The child {\tt ESMF_CplComp} 
!    will inherit all the {\tt PET}s from the parent.
!   \item[{[name]}]
!    Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!    from within the {\tt ESMF\_CplComp} code once the initialization routine
!    is called.
!   \item[{[config]}]
!    An already-created {\tt ESMF\_Config} configuration object 
!    from which the new component
!    can read in namelist-type information to set parameters for this run.
!    If both are specified, this object takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    The filename of an {\tt ESMF\_Config} format file.  
!    If specified, this file is opened, an {\tt ESMF\_Config} configuration
!    object is created for the file, and attached to the new component.  
!    The user can call {\tt ESMF\_CplCompGet()} to get and use the object.
!    If both are specified, the {\tt config} object takes priority 
!    over this one.
!   \item[{[clock]}]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
!    This should
!    not be the parent component clock, which should be maintained and passed
!    down to the initialize/run/finalize routines separately.
!   \item[{[vm]}]
!    {\tt ESMF\_VM} virtual machine object.  If unspecified, 
!    inherit parents' {\tt ESMF\_VM}.
!   \item[{[petlist]}]
!    List of {\tt PET}s in the given {\tt ESMF\_VM} that the parent 
!    component is giving to the created child 
!    component. If {\tt petList} is not specified all of the 
!    parents {\tt PET}s will be given to the child component.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                               ! local error status

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateGPar%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
                                       ESMF_CONTEXT, rc)) return
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                parent=parent%compp, &
                                vm=vm, petList=petList, clock=clock, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CplCompCreateGPar%compp => compclass
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateGPar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreateCPar"
!BOPI
! !IROUTINE: ESMF_CplCompCreate - Create a new CplComp from a Parent Component
!
! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateCPar(parent, name, config, &
                                      configFile, clock, vm, petList, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreateCPar
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(in) :: parent
      character(len=*), intent(in), optional :: name
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Clock), intent(in), optional :: clock
      type(ESMF_VM), intent(in), optional :: vm
      integer, intent(in),  optional :: petList(:)
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_CplComp}, specifying the resources of the parent
!  component as a default, with the option to select specific subsets of
!  those resources to give to the child component.
!
!  The return value is the new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[parent]}]
!    The parent component object.  The child {\tt ESFM\_CplComp} 
!    will inherit all the {\tt PET}s from the parent.
!   \item[{[name]}]
!    Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!    from within the {\tt ESMF\_CplComp} code once the initialization routine
!    is called.
!   \item[{[config]}]
!    An already-created {\tt ESMF\_Config} configuration object 
!    from which the new component
!    can read in namelist-type information to set parameters for this run.
!    If both are specified, this object takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    The filename of an {\tt ESMF\_Config} format file.  
!    If specified, this file is opened, an {\tt ESMF\_Config} configuration
!    object is created for the file, and attached to the new component.  
!    The user can call {\tt ESMF\_CplCompGet()} to get and use the object.
!    If both are specified, the {\tt config} object takes priority 
!    over this one.
!   \item[{[clock]}]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
!    This should
!    not be the parent component clock, which should be maintained and passed
!    down to the initialize/run/finalize routines separately.
!   \item[{[vm]}]
!    {\tt ESMF\_VM} virtual machine object.  If unspecified, 
!    inherit parents' {\tt ESMF\_VM}.
!   \item[{[petlist]}]
!    List of {\tt PET}s in the given {\tt ESMF\_VM} that the parent 
!    component is giving to the created child 
!    component. If {\tt petList} is not specified all of the 
!    parents {\tt PET}s will be given to the child component.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                                ! local error status

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateCPar%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
                                       ESMF_CONTEXT, rc)) return
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                parent=parent%compp, &
                                vm=vm, petList=petList, clock=clock, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CplCompCreateCPar%compp => compclass
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateCPar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompDestroy"
!BOP
! !IROUTINE: ESMF_CplCompDestroy - Release resources for a CplComp

! !INTERFACE:
      subroutine ESMF_CplCompDestroy(cplcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: cplcomp
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp]
!       Release all resources associated with this {\tt ESMF\_CplComp}
!       and mark the object as invalid.  It is an error to pass this
!       object into any other routines after being destroyed.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! local vars
        integer :: localrc                       ! local error localrc

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! Check to see if already destroyed
        if (.not.associated(cplcomp%compp)) then  
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                             "CplComp not initialized or already destroyed", &
                              ESMF_CONTEXT, rc)) return
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(cplcomp%compp, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Deallocate the cplcomp struct itself
        deallocate(cplcomp%compp, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating Component class", &
                                       ESMF_CONTEXT, rc)) return
        nullify(cplcomp%compp)
 
        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_CplCompDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompFinalize"
!BOP
! !IROUTINE: ESMF_CplCompFinalize - Call the CplComp's finalize routine
!
! !INTERFACE:
    recursive subroutine ESMF_CplCompFinalize(cplcomp, importState, &
                                  exportState, clock, phase, blockingflag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user-supplied finalization routine for 
!  an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    The {\tt ESMF\_CplComp} to call finalize routine for.
!   \item[{[importState]}]
!    {\tt ESMF\_State} containing import data for coupling.
!   \item[{[exportState]}]
!    {\tt ESMF\_State} containing export data for coupling.
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!   \item[{[phase]}]  
!      Component providers must document whether their each of their
!      routines are {\em single-phase} or {\em multi-phase}.  
!      Single-phase routines require only one invocation to complete
!      their work.  
!      Multi-phase routines provide multiple subroutines to accomplish
!      the work, accomodating components which must complete part of their
!      work, return to the caller and allow other processing to occur,
!      and then continue the original operation.
!      For single-phase child components this argument is optional, but   
!      if specified it must be {\tt ESMF\_SINGLEPHASE}.
!      For multiple-phase child components, this is the integer phase 
!      number to be invoked.
!   \item[{[blockingflag]}]
!    Use {\tt ESMF\_BLOCKING} (default) or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompFinalize(cplcomp%compp, importState=importState, &
                 exportState=exportState, clock=clock, phase=phase, &
                 blockingflag=blockingflag, rc=rc)

        end subroutine ESMF_CplCompFinalize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGet"
!BOP
! !IROUTINE: ESMF_CplCompGet - Query a CplComp for information
!
! !INTERFACE:
      subroutine ESMF_CplCompGet(cplcomp, name, config, &
                                                   configFile, clock, vm, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(in) :: cplcomp
      character(len=*), intent(out), optional :: name
      type(ESMF_Config), intent(out), optional :: config
      character(len=*), intent(out), optional :: configFile
      type(ESMF_Clock), intent(out), optional :: clock
      type(ESMF_VM), intent(out), optional :: vm
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Returns information about an {\tt ESMF\_CplComp}.
!  For queries where the caller
!  only wants a single value, specify the argument by name.
!  All the arguments after {\tt cplcomp} argument are optional 
!  to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to query.
!   \item[{[name]}]
!    Return the name of the {\tt ESMF\_CplComp}.
!   \item[{[config]}]
!    Return the {\tt ESMF\_Config} object for this {\tt ESMF\_CplComp}.
!   \item[{[configFile]}]
!    Return the configuration filename for this {\tt ESMF\_CplComp}.
!   \item[{[clock]}]
!    Return the private clock for this {\tt ESMF\_CplComp}.
!   \item[{[vm]}]
!    Return the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompGet(cplcomp%compp, name, vm=vm, clock=clock, &
                          configFile=configFile, config=config, rc=rc)

        end subroutine ESMF_CplCompGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompInitialize"
!BOP
! !IROUTINE: ESMF_CplCompInitialize - Call the CplComp's initialize routine
!
! !INTERFACE:
      recursive subroutine ESMF_CplCompInitialize(cplcomp, importState, &
                                   exportState, clock, phase, blockingflag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call initialize routine for.
!   \item[{[importState]}]  
!    {\tt ESMF\_State} containing import data for coupling.
!   \item[{[exportState]}]  
!    {\tt ESMF\_State} containing export data for coupling.
!   \item[{[clock]}]
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!   \item[{[phase]}] 
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.  
!    Single-phase routines require only one invocation to complete
!    their work.  
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but   
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase 
!    number to be invoked.
!   \item[{[blockingflag]}]
!    Valid values are {\tt ESMF\_BLOCKING} (the default) 
!    or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompInitialize(cplcomp%compp, importState, exportState, &
                    clock=clock, phase=phase, blockingflag=blockingflag, rc=rc)

        end subroutine ESMF_CplCompInitialize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompPrint"
!BOP
! !IROUTINE:  ESMF_CplCompPrint - Print the contents of a CplComp
!
! !INTERFACE:
      subroutine ESMF_CplCompPrint(cplcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: cplcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Prints information about an {\tt ESMF\_CplComp} to {\tt stdout}.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to print.
!   \item[{[options]}]
!    Print options are not yet supported.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

     !jw  call ESMF_LogWrite("Coupler Component:", ESMF_LOG_INFO)
       print *, "Coupler Component:"
       call ESMF_CompPrint(cplcomp%compp, options, rc)

       end subroutine ESMF_CplCompPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompReadRestart"
!BOPI
! !IROUTINE: ESMF_CplCompReadRestart -- Call the CplComp's restore routine
!
! !INTERFACE:
     recursive subroutine ESMF_CplCompReadRestart(cplcomp, iospec, clock, &
                                                  phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: cplcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call readrestart routine for.
!   \item[{[iospec]}]
!    {\tt ESMF\_IOSpec} object which describes I/O options.
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!    {\tt ESMF\_State} containing export data for coupling.
!   \item[{[phase]}]  
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.  
!    Single-phase routines require only one invocation to complete
!    their work.  
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but   
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase 
!    number to be invoked.
!    If multiple-phase restore, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingflag]}]
!    Valid values are {\tt ESMF\_BLOCKING} (the default) 
!    or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
	! Changed BOP to BOPI until implemented.
        call ESMF_CompReadRestart(cplcomp%compp, iospec, clock, phase, &
                                  blockingflag, rc)

        end subroutine ESMF_CplCompReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompRun"
!BOP
! !IROUTINE: ESMF_CplCompRun - Call the CplComp's run routine
!
! !INTERFACE:
    recursive subroutine ESMF_CplCompRun(cplcomp, importState, exportState, &
                                         clock, phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call run routine for.
!   \item[{[importState]}]
!    {\tt ESMF\_State} containing import data for coupling.
!   \item[{[exportState]}]
!    {\tt ESMF\_State} containing export data for coupling.
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!   \item[{[phase]}]  
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.  
!    Single-phase routines require only one invocation to complete
!    their work.  
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but   
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase 
!    number to be invoked.
!    If multiple-phase restore, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!    External clock for passing in time information.
!   \item[{[blockingflag]}]
!    Use {\tt ESMF\_BLOCKING} (default) or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompRun(cplcomp%compp, importState, exportState, &
                    clock=clock, phase=phase, blockingflag=blockingflag, rc=rc)

        end subroutine ESMF_CplCompRun


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSet"
!BOP
! !IROUTINE: ESMF_CplCompSet - Set or reset information about the CplComp
!
! !INTERFACE:
      subroutine ESMF_CplCompSet(cplcomp, name, config, configFile, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: cplcomp
      character(len=*), intent(in), optional :: name
      type(ESMF_Config), intent(in), optional :: config
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Clock), intent(in), optional :: clock
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Sets or resets information about an {\tt ESMF\_CplComp}.
!  The caller can set individual values by specifying
!  the arguments by name.
!  All the arguments except {\tt cplcomp} are optional 
!  to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to change.
!   \item[{[name]}]
!    Set the name of the {\tt ESMF\_CplComp}.
!   \item[{[config]}]
!    Set the configuration information for the {\tt ESMF\_CplComp} from
!    this already created {\tt ESMF\_Config} object.   
!    If specified, takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    Set the configuration filename for this {\tt ESMF\_CplComp}.
!    An {\tt ESMF\_Config} object will be created for this file
!    and attached to the {\tt ESMF\_CplComp}.  Superceeded by {\tt config}
!    if both are specified.
!   \item[{[clock]}]
!    Set the private clock for this {\tt ESMF\_CplComp}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompSet(cplcomp%compp, name, clock=clock, &
                          configFile=configFile, config=config, rc=rc)

        end subroutine ESMF_CplCompSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxThreads"
!BOPI
! !IROUTINE: ESMF_CplCompSetVMMaxThreads - Define a VM for this CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxThreads(cplcomp, max, &
                     pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: cplcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
!     \item[{[max]}] 
!      Maximum threading level.
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference.
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference.
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_FAILURE

    ! call CompClass method
    call ESMF_CompSetVMMaxThreads(cplcomp%compp, max, &
                   pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMaxThreads

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMinThreads"
!BOPI
! !IROUTINE: ESMF_CplCompSetVMMinThreads - Define a VM for this CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMinThreads(cplcomp, max, &
                        pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: cplcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
!     \item[{[max]}] 
!      Maximum number of PEs per PET.
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference.
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference.
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_FAILURE

    ! call CompClass method
    call ESMF_CompSetVMMinThreads(cplcomp%compp, max, &
                  pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxPEs"
!BOPI
! !IROUTINE: ESMF_CplCompSetVMMaxPEs - Define a VM for this CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxPEs(cplcomp, max, &
                       pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: cplcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
!     \item[{[max]}] 
!      Maximum number of PEs per PET.
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference.
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference.
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_FAILURE

    ! call CompClass method
    call ESMF_CompSetVMMaxPEs(cplcomp%compp, max, &
                   pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMaxPEs
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompValidate"
!BOP
! !IROUTINE: ESMF_CplCompValidate -- Ensure the CplComp is internally consistent
!
! !INTERFACE:
      subroutine ESMF_CplCompValidate(cplcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: cplcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!   Currently all this method does is to check that the 
!   {\tt cplcomp} exists.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to validate.
!   \item[{[options]}]
!    Validation options are not yet supported.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

       call ESMF_CompValidate(cplcomp%compp, options, rc)
 
       end subroutine ESMF_CplCompValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompWriteRestart"
!BOPI
! !IROUTINE: ESMF_CplCompWriteRestart -- Call the CplComp's checkpoint routine

! !INTERFACE:
    recursive subroutine ESMF_CplCompWriteRestart(cplcomp, iospec, clock, &
                                                  phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: cplcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call writerestart routine for.
!   \item[{[iospec]}]
!    {\tt ESMF\_IOSpec} object which describes I/O options.
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!   \item[{[phase]}]  
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.  
!    Single-phase routines require only one invocation to complete
!    their work.  
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but   
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase 
!    number to be invoked.
!   \item[{[blockingflag]}]
!    Valid values are {\tt ESMF\_BLOCKING} (the default) 
!    or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
	! Changed BOP to BOPI until implemented.
        call ESMF_CompWriteRestart(cplcomp%compp, iospec, clock, phase, &
                                   blockingflag, rc)

        end subroutine ESMF_CplCompWriteRestart



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompWait"
!BOP
! !IROUTINE: ESMF_CplCompWait - Wait for a CplComp to return
!
! !INTERFACE:
  subroutine ESMF_CplCompWait(cplcomp, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: cplcomp
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!     When executing asychronously, wait for an {\tt ESMF\_CplComp} to return.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to wait for. 
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_FAILURE

    ! call CompClass method
    call ESMF_CompWait(cplcomp%compp, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompWait
!------------------------------------------------------------------------------



end module ESMF_CplCompMod

