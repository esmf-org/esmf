! $Id: ESMF_GridCompSetServ.F90,v 1.15 2009/01/15 06:50:41 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
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

!BOP
! !IROUTINE: ESMF_GridCompSetEntryPoint - Set name of GridComp subroutines
!
! !INTERFACE:
!      subroutine ESMF_GridCompSetEntryPoint(comp, subroutineType, &
!                                            subroutineName, phase, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp), intent(inout) :: comp
!      character(len=*), intent(in) :: subroutineType
!      subroutine, intent(in) :: subroutineName
!      integer, intent(in) :: phase
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Intended to be called by an {\tt ESMF\_GridComp} during its SetServices
!  routine. An {\tt ESMF\_GridComp} calls {\tt ESMF\_GridCompSetEntryPoint}
!  for each of the predefined initialize, run, and finalize routines, 
!  to assocate the internal subroutine to be called for each function. 
!  If multiple phases for init, run, or finalize are needed, this can be 
!  called with phase numbers.
!
!  After this subroutine returns, the framework knows how to call the
!  initialize, run, and finalize routines for this component.
!    
!  The arguments are:
!  \begin{description}
!   \item[comp]
!    An {\tt ESMF\_GridComp} object.
!   \item[subroutineType]
!    One of a set of predefined subroutine types - e.g. {\tt ESMF\_SETINIT}, 
!    {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}.
!   \item[subroutineName]
!    The name of the user-supplied {\tt gridcomp} subroutine to be associated with the
!    {\tt subroutineType}.   This subroutine does not have to be
!    public to the module.
!   \item[phase] 
!    For {\tt ESMF\_GridComp}s which need to initialize or run or finalize 
!    with mutiple phases, the phase number which 
!    corresponds to this subroutine name.
!    For single phase subroutines use the
!    parameter {\tt ESMF\_SINGLEPHASE}.   The {\tt ESMF\_GridComp} writer 
!    must document
!    the requirements of the {\tt ESMF\_GridComp} for how and when 
!    the multiple phases are expected to be called.
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
! The component writer must supply a subroutine with the exact interface 
! shown below. Arguments must not be declared as optional, and the types and
! order must match.
!
! !INTERFACE:
!      interface
!        subroutine subroutineName (comp, importState, exportState, clock, rc)
!          type(ESMF\_GridComp) :: comp                     ! must not be optional
!          type(ESMF\_State)    :: importState, exportState ! must not be optional
!          type(ESMF\_Clock)    :: clock                    ! must not be optional
!          integer, intent(out) :: rc                       ! must not be optional
!        end subroutine
!      end interface
!
!EOP

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

!BOP
! !IROUTINE: ESMF_GridCompSetServices - Register GridComp interface routines
!
! !INTERFACE:
!      subroutine ESMF_GridCompSetServices(comp, subroutineName, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp) :: comp
!      subroutine :: subroutineName
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Call a gridded {\tt ESMF\_GridComp}'s setservices registration routine.  
!  The parent component must first create an {\tt ESMF\_GridComp}, then
!  call this routine.  The arguments are the object returned from the create
!  call, plus the user-supplied, public, well-known subroutine name
!  that is the registration routine for this {\tt ESMF\_GridComp}. 
!  This name must be documented by the {\tt ESMF\_GridComp} provider.
!
!  After this subroutine returns, the framework knows how to call
!  the initialize, run, and finalize routines for the {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    An {\tt ESMF\_GridComp} object.
!   \item[subroutineName]
!    The public name of the {\tt gridcomp}'s 
!    {\tt ESMF\_GridCompSetServices} call.  
!    An {\tt ESMF\_GridComp} writer must provide this information.
!    Note that this is the actual subroutine, not a character string.
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
! The component writer must supply a subroutine with the exact interface 
! shown below. Arguments must not be declared as optional, and the types and
! order must match.
!
! !INTERFACE:
!      interface
!        subroutine subroutineName (comp, rc)
!          type(ESMF\_GridComp) :: comp   ! must not be optional
!          integer, intent(out) :: rc     ! must not be optional
!        end subroutine
!      end interface
!
! !DESCRIPTION:
!  The subroutine, when called by the framework, must make successive calls to
!  {\tt ESMF\_GridCompSetEntryPoint} to preset callback routines for initialization,
!  run, and finalization for a coupler component.
!
!EOP

!BOP
! !IROUTINE: ESMF_GridCompSetServices - Register GridComp interface routines located in shared object
!
! !INTERFACE:
  ! Private name; call using ESMF_GridCompSetServices()
!  recursive subroutine ESMF_GridCompSetServicesLib(comp, sharedObj, routine, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp),     intent(inout)         :: comp
!      character(len=*),        intent(in)            :: sharedObj
!      character(len=*),        intent(in)            :: routine
!      integer,                 intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call into user provided routine which is responsible for setting
!  component's Initialize(), Run() and Finalize() services. The named
!  {\tt routine} must exist in the shared object file specified in the
!  {\tt sharedObj} argument.
!    
!  The arguments are:
!  \begin{description}
!  \item[comp]
!  Gridded component.
!  \item[sharedObj]
!  Name of shared object that contains {\tt routine}.
!  \item[routine]
!  Name of routine to be called.
! \item[{[rc]}]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

!BOP
! !IROUTINE: ESMF_GridCompSetVM - Set GridComp VM properties
!
! !INTERFACE:
!      subroutine ESMF_GridCompSetVM(comp, subroutineName, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp) :: comp
!      subroutine :: subroutineName
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Call a {\tt ESMF\_GridComp}'s setVM routine to give the child component
!  the opportunity to its VM properties.
!  The parent component must first create an {\tt ESMF\_GridComp}, then
!  optionally call this routine. The arguments are the object returned from
!  the create call, plus the user-supplied, public subroutine name
!  that is the setVM routine for this {\tt ESMF\_GridComp}. 
!  This name must be documented by the {\tt ESMF\_GridComp} provider.
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    An {\tt ESMF\_GridComp} object.
!   \item[subroutineName]
!    The public name of the {\tt gridcomp}'s setVM call.
!    An {\tt ESMF\_GridComp} writer must provide this information.
!    Note that this is the actual subroutine, not a character string.
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
! The component writer must supply a subroutine with the exact interface 
! shown below. Arguments must not be declared as optional, and the types and
! order must match.
!
! !INTERFACE:
!      interface
!        subroutine subroutineName (comp, rc)
!          type(ESMF\_GridComp) :: comp   ! must not be optional
!          integer, intent(out) :: rc     ! must not be optional
!        end subroutine
!      end interface
!
! !DESCRIPTION:
!  The subroutine, when called by the framework, is expected to use any of the
!  {\tt ESMF\_GridCompSetVMxxx()} methods to set the properties of the VM
!  associated with the gridded component.
!
!EOP

!BOP
! !IROUTINE: ESMF_GridCompSetVM - Set GridComp VM properties in routine located in shared object
!
! !INTERFACE:
  ! Private name; call using ESMF_GridCompSetVM()
!  recursive subroutine ESMF_GridCompSetVMLib(comp, sharedObj, routine, rc)
!
! !ARGUMENTS:
!      type(ESMF_GridComp),     intent(inout)         :: comp
!      character(len=*),        intent(in)            :: sharedObj
!      character(len=*),        intent(in)            :: routine
!      integer,                 intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call into user provided routine which is responsible for setting
!  component's VM properties. The named
!  {\tt routine} must exist in the shared object file specified in the
!  {\tt sharedObj} argument.
!    
!  The arguments are:
!  \begin{description}
!  \item[comp]
!  Gridded component.
!  \item[sharedObj]
!  Name of shared object that contains {\tt routine}.
!  \item[routine]
!  Name of routine to be called.
! \item[{[rc]}]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
