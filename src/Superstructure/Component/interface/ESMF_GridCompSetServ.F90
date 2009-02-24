! $Id: ESMF_GridCompSetServ.F90,v 1.18 2009/02/24 06:58:26 theurich Exp $
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
! This file contains ONLY the documentation for GridCompSetServices
! and related interfaces.  The actual code will be found in ESMC_Comp_F.C
! This file contains NO executable code.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_GridCompGetInternalState(gridcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp), intent(inout) :: gridcomp
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Available to be called by an {\tt ESMF\_GridComp} at any time after 
!  {\tt ESMF\_GridCompSetInternalState} has been called.
!  Since init, run, and finalize must be separate subroutines data that
!  they need to share in common can either be module global data, or can
!  be allocated in a private data block and the address of that block
!  can be registered with the framework and retrieved by this call.
!  When running multiple instantiations of an {\tt ESMF\_GridComp}, 
!  for example during ensemble runs, 
!  it may be simpler to maintain private data specific to 
!  each run with private data blocks.  A corresponding 
!  {\tt ESMF\_GridCompSetInternalState} call sets the data pointer to 
!  this block, and this call retrieves the data pointer.
!  Note that the {\tt dataPointer} argument needs to be a derived type
!  which contains only a pointer of the type of the data block defined
!  by the user.  When making this call the pointer needs to be unassociated.
!  When the call returns the pointer will now reference the original
!  data block which was set during the previous call to
!  {\tt ESMF\_GridCompSetInternalState}.

!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    An {\tt ESMF\_GridComp} object.
!   \item[dataPointer]
!    A derived type, containing only an unassociated pointer 
!    to the private data block.
!    The framework will fill in the pointer. When this call returns the
!    pointer is set to the same address set during 
!    {\tt ESMF\_GridCompSetInternalState}.
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
! !IROUTINE: ESMF_GridCompSetEntryPoint - Set user routine as entry point for standard Component method
!
! !INTERFACE:
!  subroutine ESMF_GridCompSetEntryPoint(gridcomp, stage, routine, phase, rc)
!
! !ARGUMENTS:
!    use ESMF_CompMod
!    type(ESMF_GridComp), intent(in) :: gridcomp
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
! \item[gridcomp]
!   An {\tt ESMF\_GridComp} object.
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
!     subroutine routine(gridcomp, importState, exportState, clock, rc)
!       type(ESMF_GridComp)  :: gridcomp     ! must not be optional
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
! !IROUTINE: ESMF_GridCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_GridCompSetInternalState(gridcomp, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp), intent(inout) :: gridcomp
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Available to be called by an {\tt ESMF\_GridComp} at any time, but 
!  expected to be
!  most useful when called during the registration process, or initialization.
!  Since init, run, and finalize must be separate subroutines data that
!  they need to share in common can either be module global data, or can
!  be allocated in a private data block and the address of that block
!  can be registered with the framework and retrieved by subsequent calls.
!  When running multiple instantiations of an {\tt ESMF\_GridComp}, 
!  for example during
!  ensemble runs, it may be simpler to maintain private data specific to 
!  each run with private data blocks.  A corresponding 
!  {\tt ESMF\_GridCompGetInternalState} call retrieves the data pointer.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    An {\tt ESMF\_GridComp} object.
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
! !IROUTINE: ESMF_GridCompSetServices - Call user routine to register GridComp methods
!
! !INTERFACE:
! recursive subroutine ESMF_GridCompSetServices(gridcomp, routine, rc)
!
! !ARGUMENTS:
!   type(ESMF_GridComp)             :: gridcomp
!   subroutine                      :: routine
!   integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
! Call into user provided {\tt routine} which is responsible for
! for setting Component's Initialize(), Run() and Finalize() services.
!    
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Gridded Component.
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
!     subroutine routine(gridcomp, rc)
!       type(ESMF_GridComp)  :: gridcomp   ! must not be optional
!       integer, intent(out) :: rc         ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The {\tt routine}, when called by the framework, must make successive calls to
! {\tt ESMF\_GridCompSetEntryPoint()} to preset callback routines for standard
! Component Initialize(), Run() and Finalize() methods.
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSetServices - Call user routine, located in shared object, to register GridComp methods
!
! !INTERFACE:
! ! Private name; call using ESMF_GridCompSetServices()
!  recursive subroutine ESMF_GridCompSetServicesShObj(gridcomp, routine, sharedObj, rc)
!
! !ARGUMENTS:
!   type(ESMF_GridComp),     intent(inout)         :: gridcomp
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
! \item[gridcomp]
!   Gridded Component.
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
!     subroutine routine(gridcomp, rc)
!       type(ESMF_GridComp)  :: gridcomp   ! must not be optional
!       integer, intent(out) :: rc         ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The {\tt routine}, when called by the framework, must make successive calls to
! {\tt ESMF\_GridCompSetEntryPoint()} to preset callback routines for standard
! Component Initialize(), Run() and Finalize() methods.
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSetVM - Call user routine to set GridComp VM properies
!
! !INTERFACE:
! recursive subroutine ESMF_GridCompSetVM(gridcomp, routine, rc)
!
! !ARGUMENTS:
!   type(ESMF_GridComp)             :: gridcomp
!   subroutine                      :: routine
!   integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
! Optionally call into user provided {\tt routine} which is responsible for
! for setting Component's VM properties. 
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Gridded Component.
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
!     subroutine routine(gridcomp, rc)
!       type(ESMF_GridComp)  :: gridcomp    ! must not be optional
!       integer, intent(out) :: rc          ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The subroutine, when called by the framework, is expected to use any of the
! {\tt ESMF\_GridCompSetVMxxx()} methods to set the properties of the VM
! associated with the Gridded Component.
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSetVM - Call user routine, located in shared object, to set GridComp VM properies
!
! !INTERFACE:
! ! Private name; call using ESMF_GridCompSetVM()
!  recursive subroutine ESMF_GridCompSetVMLib(gridcomp, routine, sharedObj, rc)
!
! !ARGUMENTS:
!   type(ESMF_GridComp),     intent(inout)         :: gridcomp
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
! \item[gridcomp]
!   Gridded Component.
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
!     subroutine routine(gridcomp, rc)
!       type(ESMF_GridComp)  :: gridcomp    ! must not be optional
!       integer, intent(out) :: rc          ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The subroutine, when called by the framework, is expected to use any of the
! {\tt ESMF\_GridCompSetVMxxx()} methods to set the properties of the VM
! associated with the Gridded Component.
!
!EOP
!------------------------------------------------------------------------------
