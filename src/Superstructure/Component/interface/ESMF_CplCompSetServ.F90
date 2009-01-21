! $Id: ESMF_CplCompSetServ.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
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

!BOP
! !IROUTINE: ESMF_CplCompSetEntryPoint - Set name of CplComp subroutines
!
! !INTERFACE:
!      subroutine ESMF_CplCompSetEntryPoint(comp, subroutineType, &
!                                            subroutineName, phase, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: comp
!      character(len=*), intent(in) :: subroutineType
!      subroutine, intent(in) :: subroutineName
!      integer, intent(in) :: phase
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Intended to be called by an {\tt ESMF\_CplComp} during the 
!  registration process.
!  An {\tt ESMF\_CplComp} calls {\tt ESMF\_CplCompSetEntryPoint} for each of 
!  the predefined init, run, and finalize routines, 
!  to assocate the internal subroutine to be
!  called for each function.  If multiple phases for init, run, or finalize
!  are needed, this can be called with phase numbers.
!
!  After this subroutine returns, the framework now knows how to call
!  the initialize, run, and finalize routines for this child 
!  {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[comp]
!    An {\tt ESMF\_CplComp} object.
!   \item[subroutineType]
!    One of a set of predefined subroutine types - e.g. {\tt ESMF\_SETINIT}, 
!    {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}.
!   \item[subroutineName]
!    The name of the user-supplied {\tt cplcomp} subroutine to be associated with the
!    {\tt subroutineType}.  This subroutine does not have to be
!    public to the module.
!   \item[{[phase]}] 
!    For {\tt ESMF\_CplComp}s which need to initialize, run, or finalize 
!    with mutiple phases, the phase number which 
!    corresponds to this subroutine name.
!    For single phase subroutines, either omit this argument, or use the
!    parameter {\tt ESMF\_SINGLEPHASE}.   The {\tt ESMF\_CplComp} writer 
!    must document
!    the requirements of the {\tt ESMF\_CplComp} for how and when 
!    the multiple phases are expected to be called.
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
!  The user-supplied routine must conform to the following interface:
!
! !INTERFACE:
!      interface
!        subroutine subroutineName (comp, importState, exportState, clock, rc)
!          type(ESMF\_CplComp) :: comp
!          type(ESMF\_State) :: importState, exportState
!          type(ESMF\_Clock) :: clock
!          integer, intent(out) :: rc
!        end subroutine
!      end interface
!
!EOP

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

!BOP
!
! !IROUTINE: ESMF_CplCompSetServices - Register CplComp interface routines
!
! !INTERFACE:
!      subroutine ESMF_CplCompSetServices(comp, subroutineName, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: comp
!      subroutine, intent(in) :: subroutineName
!      integer, intent(out) :: rc
!
! !DESCRIPTION:
!  Call an {\tt ESMF\_CplComp}'s setservices registration routine.  
!  The parent component must first create an {\tt ESMF\_CplComp}, then
!  call this routine. The arguments are the object returned from the create
!  call, plus the user-supplied, public, well-known, subroutine name
!  that is the registration routine for this {\tt ESMF\_CplComp}.  
!  This name must be documented by the {\tt ESMF\_CplComp} provider.
!
!  After this subroutine returns, the framework now knows how to call
!  the initialize, run, and finalize routines for the {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    An {\tt ESMF\_CplComp} object.
!   \item[subroutineName]
!    The public name of the user-supplied {\tt cplcomp} 
!    {\tt ESMF\_CplCompSetServices} routine.  
!    An {\tt ESMF\_CplComp} writer must provide this information.
!    Note this is the actual subroutine, not a character string.
!   \item[rc] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    Note: unlike most other ESMF routines, this argument is not optional
!    because of implementation considerations.
!   \end{description}
!
!  The user-supplied registration routine must conform to the following
!  interface:
!
! !INTERFACE:
!      interface
!        subroutine subroutineName (comp, rc)
!          type(ESMF\_CplComp) :: comp
!          integer, intent(out) :: rc
!        end subroutine
!      end interface
!
! !DESCRIPTION:
!  The subroutine, when called by the framework, must make successive calls to
!  {\tt ESMF\_CplCompSetEntryPoint} to preset callback routines for initialization,
!  run, and finalization for a coupler component.
!
!EOP
