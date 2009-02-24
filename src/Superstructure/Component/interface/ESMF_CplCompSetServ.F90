! $Id: ESMF_CplCompSetServ.F90,v 1.19 2009/02/24 06:58:26 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
!==============================================================================
!
! This file contains ONLY the documentation for CplCompSetServices
! and related interfaces.  The actual code will be found in ESMC_Comp_F.C
! This file contains NO executable code.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_CplCompGetInternalState(cplcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: cplcomp
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Available to be called by an {\tt ESMF\_CplComp} at any time after 
!  {\tt ESMF\_CplCompSetInternalState} has been called.
!  Since init, run, and finalize must be separate subroutines data that
!  they need to share in common can either be module global data, or can
!  be allocated in a private data block and the address of that block
!  can be registered with the framework and retrieved by this call.
!  When running multiple instantiations of an {\tt ESMF\_CplComp}, 
!  for example during ensemble runs, 
!  it may be simpler to maintain private data specific to 
!  each run with private data blocks.  A corresponding 
!  {\tt ESMF\_CplCompSetInternalState} call sets the data pointer to 
!  this block, and this call retrieves the data pointer.   
!  Note that the {\tt dataPointer} argument needs to be a derived type
!  which contains only a pointer of the type of the data block defined
!  by the user.  When making this call the pointer needs to be unassociated.
!  When the call returns the pointer will now reference the original
!  data block which was set during the previous call to
!  {\tt ESMF\_CplCompSetInternalState}.
!    
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    An {\tt ESMF\_CplComp} object.
!   \item[dataPointer]
!    A derived type, containing only an unassociated pointer 
!    to the private data block.
!    The framework will fill in the pointer. When this call returns the
!    pointer is set to the same address set during 
!    {\tt ESMF\_CplCompSetInternalState}.
!    This level of indirection is needed to reliably set and retrieve 
!    the data block no matter which architecture or compiler is used.  
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetEntryPoint - Set user routine as entry point for standard Component method
!
! !INTERFACE:
!  subroutine ESMF_CplCompSetEntryPoint(cplcomp, stage, routine, phase, rc)
!
! !ARGUMENTS:
!    use ESMF_CompMod
!    type(ESMF_CplComp),  intent(in) :: cplcomp
!    character(*),        intent(in) :: stage
!    subroutine                      :: routine
!    integer, intent(in),  optional  :: phase
!    integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
! Registers a user-supplied {\tt routine} as the entry point for one of the
! predefined Component {\tt stage}s. After this call the {\tt routine} becomes
! accessible via the standard Component API method for this {\tt stage}.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[stage]
!   One of a set of predefined Component stages - e.g. {\tt ESMF\_SETINIT}, 
!   {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}. !!!need to reference here!!!
! \item[routine]
!   The user-supplied subroutine to be associated for this {\tt stage}.
!   This subroutine does not have to be public.
! \item[{[phase]}] 
!   The {\tt phase} number for multi-phase stages. For single phase 
!   stages the {\tt phase} argument can be omitted. The default setting
!   is {\tt ESMF\_SINGLEPHASE}.
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown below, including the {\tt intent}. Arguments must not be declared
! as optional, and the types and order must match.
!
! !INTERFACE:
!   interface
!     subroutine routine(cplcomp, importState, exportState, clock, rc)
!       type(ESMF_CplComp)   :: cplcomp      ! must not be optional
!       type(ESMF_State)     :: importState  ! must not be optional
!       type(ESMF_State)     :: exportState  ! must not be optional
!       type(ESMF_Clock)     :: clock        ! must not be optional
!       integer, intent(out) :: rc           ! must not be optional
!     end subroutine
!   end interface
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_CplCompSetInternalState(cplcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: cplcomp
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Available to be called by an {\tt ESMF\_CplComp} at any time, but 
!  expected to be
!  most useful when called during the registration process, or initialization.
!  Since init, run, and finalize must be separate subroutines data that
!  they need to share in common can either be module global data, or can
!  be allocated in a private data block and the address of that block
!  can be registered with the framework and retrieved by subsequent calls.
!  When running multiple instantiations of an {\tt ESMF\_CplComp}, 
!  for example during
!  ensemble runs, it may be simpler to maintain private data specific to 
!  each run with private data blocks.  A corresponding 
!  {\tt ESMF\_CplCompGetInternalState} call retrieves the data pointer.
!    
!  The arguments are:
!  \begin{description}
!   \item[cplcomp] 
!    An {\tt ESMF\_CplComp} object.
!   \item[dataPointer]
!    A pointer to the private data block, wrapped in a derived type which
!    contains only a pointer to the block.  This level of indirection is
!    needed to reliably set and retrieve the data block no matter which
!    architecture or compiler is used.  
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Call user routine to register CplComp methods
!
! !INTERFACE:
! recursive subroutine ESMF_CplCompSetServices(cplcomp, routine, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp)              :: cplcomp
!   subroutine                      :: routine
!   integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
! Call into user provided {\tt routine} which is responsible for
! for setting Component's Initialize(), Run() and Finalize() services.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[routine]
!   Routine to be called.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown below, including the {\tt intent}. Arguments must not be declared
! as optional, and the types and order must match.
!
! !INTERFACE:
!   interface
!     subroutine routine(cplcomp, rc)
!       type(ESMF_CplComp)   :: cplcomp    ! must not be optional
!       integer, intent(out) :: rc         ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The {\tt routine}, when called by the framework, must make successive calls to
! {\tt ESMF\_CplCompSetEntryPoint()} to preset callback routines for standard
! Component Initialize(), Run() and Finalize() methods.
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Call user routine, located in shared object, to register CplComp methods
!
! !INTERFACE:
! ! Private name; call using ESMF_CplCompSetServices()
!  recursive subroutine ESMF_CplCompSetServicesShObj(cplcomp, routine, sharedObj, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp),      intent(inout)         :: cplcomp
!   character(len=*),        intent(in)            :: routine
!   character(len=*),        intent(in),  optional :: sharedObj
!   integer,                 intent(out), optional :: rc 
!
! !DESCRIPTION:
! Call into user provided routine which is responsible for setting
! Component's Initialize(), Run() and Finalize() services. The named
! {\tt routine} must exist in the shared object file specified in the
! {\tt sharedObj} argument. All of the platform specific details about 
! dynamic linking and loading apply.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[routine]
!   Name of routine to be called.
! \item[{[sharedObj]}]
!   Name of shared object that contains {\tt routine}. If the {\tt sharedObj}
!   argument is not provided the executable itself will be searched for
!   {\tt routine}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! !INTERFACE:
!   interface
!     subroutine routine(cplcomp, rc)
!       type(ESMF_CplComp)   :: cplcomp    ! must not be optional
!       integer, intent(out) :: rc         ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The {\tt routine}, when called by the framework, must make successive calls to
! {\tt ESMF\_CplCompSetEntryPoint()} to preset callback routines for standard
! Component Initialize(), Run() and Finalize() methods.
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetVM - Call user routine to set CplComp VM properies
!
! !INTERFACE:
! recursive subroutine ESMF_CplCompSetVM(cplcomp, routine, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp)              :: cplcomp
!   subroutine                      :: routine
!   integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
! Optionally call into user provided {\tt routine} which is responsible for
! for setting Component's VM properties. 
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[routine]
!   Routine to be called.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown below, including the {\tt intent}. Arguments must not be declared
! as optional, and the types and order must match.
!
! !INTERFACE:
!   interface
!     subroutine routine(cplcomp, rc)
!       type(ESMF_CplComp)   :: cplcomp     ! must not be optional
!       integer, intent(out) :: rc          ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The subroutine, when called by the framework, is expected to use any of the
! {\tt ESMF\_CplCompSetVMxxx()} methods to set the properties of the VM
! associated with the Coupler Component.
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetVM - Call user routine, located in shared object, to set CplComp VM properies
!
! !INTERFACE:
! ! Private name; call using ESMF_CplCompSetVM()
!  recursive subroutine ESMF_CplCompSetVMLib(cplcomp, routine, sharedObj, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp),      intent(inout)         :: cplcomp
!   character(len=*),        intent(in)            :: routine
!   character(len=*),        intent(in),  optional :: sharedObj
!   integer,                 intent(out), optional :: rc 
!
! !DESCRIPTION:
! Optionally call into user provided {\tt routine} which is responsible for
! for setting Component's VM properties. The named {\tt routine} must exist
! in the shared object file specified in the {\tt sharedObj} argument. All of
! the platform specific details about dynamic linking and loading apply.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[routine]
!   Routine to be called.
! \item[{[sharedObj]}]
!   Name of shared object that contains {\tt routine}. If the {\tt sharedObj}
!   argument is not provided the executable itself will be searched for
!   {\tt routine}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown below, including the {\tt intent}. Arguments must not be declared
! as optional, and the types and order must match.
!
! !INTERFACE:
!   interface
!     subroutine routine(cplcomp, rc)
!       type(ESMF_CplComp)   :: cplcomp     ! must not be optional
!       integer, intent(out) :: rc          ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The subroutine, when called by the framework, is expected to use any of the
! {\tt ESMF\_CplCompSetVMxxx()} methods to set the properties of the VM
! associated with the Coupler Component.
!
!EOP
!------------------------------------------------------------------------------
