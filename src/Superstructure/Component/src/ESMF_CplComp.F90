! $Id: ESMF_CplComp.F90,v 1.32 2004/05/17 16:04:02 nscollins Exp $
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
! !MODULE: ESMF_CplCompMod - Coupler Cplcomp class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Coupler Cplcomp} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      !use ESMF_MachineMod
      use ESMF_VMMod
      use ESMF_ConfigMod
      use ESMF_ClockMod
      use ESMF_GridMod
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
      '$Id: ESMF_CplComp.F90,v 1.32 2004/05/17 16:04:02 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_CplCompCreate - Create a Coupler Cplcomp
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
!     This interface provides an entry point for methods that create a 
!     Coupler {\tt Cplcomp}.  The various varieties allow the resources
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
!  Create a new {\tt ESMF\_CplComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[name]
!    CplComp name.
!   \item[config]
!    CplComp-specific configuration object.  
!   \item[clock]
!    CplComp-specific clock object.  
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

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
          print *, "ERROR in ESMF_CplCompCreate: Allocate"
          return
        endif

        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                    config=config, clock=clock, rc=status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (status .ne. ESMF_SUCCESS) then
          print *, "CplComp construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateNew%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

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
!  Create a new {\tt ESMF\_CplComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[name]}]
!    CplComp name.
!   \item[{[config]}]
!    Already created {\tt ESMF\_Config} object.  If specified, takes
!    priority over config filename.
!   \item[{[configFile]}]
!    CplComp-specific configuration filename. 
!   \item[{[clock]}]
!    CplComp-specific clock.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

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
          print *, "ERROR in ESMF_CplCplCompCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                clock=clock, rc=status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (status .ne. ESMF_SUCCESS) then
          print *, "CplComp construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateConf%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreateConf
    

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreateVM"
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a new CplComp with VM enabled
!
! !INTERFACE:
      ! Private name; call using ESMF_CplCompCreate()      
      function ESMF_CplCompCreateVM(vm, &
                                  name, config, configFile, clock, petList, rc)
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
!  Create a new {\tt ESMF\_CplComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[vm]
!    VM of the component out of which this {\tt ESMF\_CplCompCreate()}
!    call is issued.
!    This will become the parent VM of the newly created Coupler Component.
!   \item[{[name]}]
!    Name of the new Coupler Component.
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
!   \item[{[configFile]}]
!    CplComp-specific configuration filename. 
!   \item[{[clock]}]
!    CplComp-specific clock.
!   \item[{[petList]}]
!    List of PET in vm that the parent component is giving to the created child 
!    component. If petList is not provided all of the parent's PETs will be 
!    given to the child component.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateVM%compp)
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
          print *, "ERROR in ESMF_CplCplCompCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                clock=clock, vm=vm, petList=petList, rc=status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (status .ne. ESMF_SUCCESS) then
          print *, "CplComp construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateVM%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

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
!  Create a new {\tt ESMF\_CplComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[parent]}]
!    Parent component.
!   \item[{[name]}]
!    CplComp name.
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
!   \item[{[configFile]}]
!    CplComp-specific configuration filename. 
!   \item[{[clock]}]
!    CplComp-specific clock.
!   \item[{[vm]}]
!    Virtual machine.  If unspecified, inherit parent VM.
!   \item[{[petlist]}]
!    List of {\tt PET}s assigned to this component.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateGPar%compp)
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
          print *, "ERROR in ESMF_CplCplCompCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                parent=parent%compp, &
                                vm=vm, petList=petList, clock=clock, rc=status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (status .ne. ESMF_SUCCESS) then
          print *, "CplComp construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateGPar%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

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
!  Create a new {\tt ESMF\_CplComp} and set the decomposition characteristics.
!
!  The return value is a new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[parent]}]
!    Parent component.
!   \item[{[name]}]
!    CplComp name.
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
!   \item[{[configFile]}]
!    CplComp-specific configuration filename. 
!   \item[{[clock]}]
!    CplComp-specific clock.
!   \item[{[vm]}]
!    Virtual machine.  If unspecified, inherit parent VM.
!   \item[{[petlist]}]
!    List of {\tt PET}s assigned to this component.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreateCPar%compp)
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
          print *, "ERROR in ESMF_CplCplCompCreate: Allocate"
          return
        endif
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                parent=parent%compp, &
                                vm=vm, petList=petList, clock=clock, rc=status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (status .ne. ESMF_SUCCESS) then
          print *, "CplComp construction error"
          return
        endif

        ! Set return values
        ESMF_CplCompCreateCPar%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

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
!       Destroy contents of this {\tt ESMF\_CplComp}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

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
        if (.not.associated(cplcomp%compp)) then  
          print *, "CplComp already destroyed"
          return
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(cplcomp%compp, status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (status .ne. ESMF_SUCCESS) then
          print *, "CplComp contents destruction error"
          return
        endif

        ! Deallocate the cplcomp struct itself
        deallocate(cplcomp%compp, stat=status)
        ! if (ESMF_LogFoundAllocError(status, "deallocating cplcomp", rc)) return
        if (status .ne. 0) then
          print *, "CplComp contents destruction error"
          return
        endif
        nullify(cplcomp%compp)
 
        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CplCompDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompFinalize"
!BOP
! !IROUTINE: ESMF_CplCompFinalize - Call the CplComp's finalize routine
!
! !INTERFACE:
    recursive subroutine ESMF_CplCompFinalize(cplcomp, importState, &
                                              exportState, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalize code for a cplcomp.
!
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    CplComp to call Finalize routine for.
!   \item[{[importState]}]
!       ESMF\_State containing import data for coupling.
!   \item[{[exportState]}]
!       ESMF\_State containing export data for coupling.
!   \item[{[clock]}]  External clock for passing in time information.
!   \item[{[phase]}]  If multiple-phase finalize, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompFinalize(cplcomp%compp, importState=importState, &
                      exportState=exportState, clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompFinalize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGet"
!BOP
! !IROUTINE: ESMF_CplCompGet - Query a CplComp for information
!
! !INTERFACE:
      subroutine ESMF_CplCompGet(cplcomp, name, clock, &
                                                   configFile, config, vm, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(in) :: cplcomp
      character(len=*), intent(out), optional :: name
      type(ESMF_Clock), intent(out), optional :: clock
      character(len=*), intent(out), optional :: configFile
      type(ESMF_Config), intent(out), optional :: config
      type(ESMF_VM), intent(out), optional :: vm
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the coupler component.  
!      For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the cplcomp input are optional 
!      to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    CplComp to query.
!   \item[{[name]}]
!    CplComp name.
!   \item[{[clock]}]
!    CplComp-specific clock.
!   \item[{[configFile]}]
!    CplComp-specific configuration filename.
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
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
                                                  exportState, clock, phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a cplcomp.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    CplComp to call Initialization routine for.
!   \item[{[importState]}]  
!       {\tt ESMF\_State} containing source data for coupling.
!   \item[{[exportState]}]  
!       {\tt ESMF\_State} containing destination data for coupling.
!   \item[{[clock]}]  External clock for passing in time information.
!   \item[{[phase]}]  If multiple-phase init, which phase number this is.
!      Pass in {\tt 0} or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompInitialize(cplcomp%compp, importState=importState, &
                                 exportState=exportState, clock=clock,     &
                                 phase=phase, rc=rc)

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
!      Routine to print information about a cplcomp.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    CplComp to print.
!   \item[{[options]}]
!    Options on print.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

       print *, "Coupler CplComp:"
       call ESMF_CompPrint(cplcomp%compp, options, rc)

       end subroutine ESMF_CplCompPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompReadRestart"
!BOP
! !IROUTINE: ESMF_CplCompReadRestart -- Call the CplComp's restore routine
!
! !INTERFACE:
     recursive subroutine ESMF_CplCompReadRestart(cplcomp, iospec, clock, &
                                                                    phase, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: cplcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for a cplcomp.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    CplComp to call ReadRestart routine for.
!   \item[{[iospec]}]
!    {\tt IOSpec} object which describes I/O options.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!      If multiple-phase restore, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompReadRestart(cplcomp%compp, iospec, clock, phase, rc)

        end subroutine ESMF_CplCompReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompRun"
!BOP
! !IROUTINE: ESMF_CplCompRun - Call CplComp run routine with two States
!
! !INTERFACE:
    recursive subroutine ESMF_CplCompRun(cplcomp, importState, exportState, &
                                                              clock, phase, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for a cplcomp.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    CplComp to call Run routine for.
!   \item[{[importState]}]
!     {\tt ESMF\_State} containing import data for coupling.
!   \item[{[exportState]}]
!     {\tt ESMF\_State} containing export data for coupling.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!      If multiple-phase run, which phase number this is.
!      Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompRun(cplcomp%compp, importState=importState,  &
                      exportState=exportState, clock=clock, phase=phase, rc=rc)

        end subroutine ESMF_CplCompRun


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSet"
!BOP
! !IROUTINE: ESMF_CplCompSet - Set or reset information about the CplComp
!
! !INTERFACE:
      subroutine ESMF_CplCompSet(cplcomp, name, clock, configFile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: cplcomp
      character(len=*), intent(in), optional :: name
      type(ESMF_Clock), intent(in), optional :: clock
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Config), intent(in), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets or resets information about the cplcomp.  When the caller
!      only wants to set a single value specify the argument by name.
!      All the arguments after the cplcomp input are optional 
!      to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    CplComp to set information for. 
!   \item[{[name]}]
!    CplComp name.
!   \item[{[clock]}]
!    CplComp-specific clock.
!   \item[{[configFile]}]
!    CplComp-specific configuration filename.
!   \item[{[config]}]
!    Already created {\tt Config} object.  If specified, takes
!    priority over config filename.
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
!      Routine to ensure a CplComp is valid.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    CplComp to validate.
!   \item[{[options]}]
!    Object to be validated.
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
!BOP
! !IROUTINE: ESMF_CplCompWriteRestart -- Call the CplComp's checkpoint routine

! !INTERFACE:
    recursive subroutine ESMF_CplCompWriteRestart(cplcomp, iospec, clock, &
                                                                     phase, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: cplcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for a cplcomp.
!    
!  The arguments are: 
!  \begin{description} 
!  
!   \item[cplcomp]
!    CplComp to call WriteRestart routine for.
!   \item[{[iospec]}]
!    {\tt ESMF\_IOSpec} object which describes I/O options.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase checkpoint, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        call ESMF_CompWriteRestart(cplcomp%compp, iospec, clock, phase, rc)

        end subroutine ESMF_CplCompWriteRestart


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
!     Set characteristics of the VM for this component.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      cplcomp object
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
    call ESMF_CompSetVMMaxThreads(cplcomp%compp, max, &
                   pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    ! if (ESMF_LogPassFoundError(status, rc)) return
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompSetVMMaxThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
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
!     Set characteristics of the VM for this component.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      cplcomp object
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
    call ESMF_CompSetVMMinThreads(cplcomp%compp, max, &
                  pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    ! if (ESMF_LogPassFoundError(status, rc)) return
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompSetVMMinThreads error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
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
!     Set characteristics of the VM for this component.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      cplcomp object
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
    call ESMF_CompSetVMMaxPEs(cplcomp%compp, max, &
                   pref_intra_process, pref_intra_ssi, pref_inter_ssi, status)
    ! if (ESMF_LogPassFoundError(status, rc)) return
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompSetVMMaxPEs error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMaxPEs
!------------------------------------------------------------------------------


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
!     Wait for an {\tt ESMF\_CplComp} to return.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      cplcomp object
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

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
    call ESMF_CompWait(cplcomp%compp, status)
    ! if (ESMF_LogPassFoundError(status, rc)) return
    if (status .ne. ESMF_SUCCESS) then
      print *, "ESMF_CompWait error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompWait
!------------------------------------------------------------------------------



end module ESMF_CplCompMod

