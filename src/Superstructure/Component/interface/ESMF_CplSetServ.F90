! $Id: ESMF_CplSetServ.F90,v 1.1 2004/03/15 20:43:26 nscollins Exp $
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
!==============================================================================
!
! This file contains ONLY the documentation for CplCompSetServices
! and related interfaces.  The actual code will be found in ESMC_Comp_F.C
! This file contains NO executable code.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !DESCRIPTION:
!
! The subroutine described below is intended to be called right after
! a component is created.
!
!EOP
!
!
!BOP
!
! !IROUTINE: ESMF_CplCompSetServices - Register Component interface routines
!
! !INTERFACE:
!      subroutine ESMF_CplCompSetServices(component, subroutineName, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: component
!      subroutine, intent(in) :: subroutineName
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Call a Coupler {\tt ESMF\_Component}'s SetServices registration routine.  
!  The parent Component must first create a subcomponent and then
!  call this routine, giving it the component derived type
!  returned from the create, plus the public, well-known, subroutine name
!  that is the registration routine for that component.  This name must be
!  documented by the subcomponent provider.
!
!  After this subroutine returns, the framework now knows how to call
!  the Initialize, Run, and Finalize routines for the subcomponents.
!    
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component object returned from a {\tt ESMF\_CplCompCreate} call.
!   \item[subroutineName]
!    The public name of the Component's {\tt SetServices} call.  
!    A component writer must provide this information.
!   \item[{[rc]}] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

!BOP
! !DESCRIPTION:
!
! The subroutines described below are intended to be called from inside a
! Component's {\tt SetServices} routine.
!
!EOP

!BOP
! !IROUTINE: ESMF_CplCompSetEntryPoint - Set name of component subroutines
!
! !INTERFACE:
!      subroutine ESMF_CplCompSetEntryPoint(component, subroutineType, &
!                                            subroutineName, phase, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: component
!      character(len=*), intent(in) :: subroutineType
!      subroutine, intent(in) :: subroutineName
!      integer, intent(in) :: phase
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Intended to be called by a component during the Registration process.
!  A component calls SetEntryPoint for each of the predefined Init,
!  Run, and Finalize routines, to assocate the internal subroutine to be
!  called for each function.  If multiple phases for Init, Run, or Finalize
!  are needed, this can be called with phase numbers.
!
!  After this subroutine returns, the framework now knows how to call
!  the Initialize, Run, and Finalize routines for the subcomponents.
!    
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component object returned from a {\tt ESMF\_CplCompCreate} call.
!   \item[subroutineType]
!    One of a set of predefined subroutine types - e.g. {\tt ESMF\_SETINIT}, 
!    {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}.
!   \item[subroutineName]
!    The name of the Component's subroutine to be associated with the
!    subroutineType (e.g. Initialize).   This subroutine does not have to be
!    public to the module.
!   \item[{[phase]}] 
!    For Components which need to initialize or run or finalize in mutiple
!    phases, the phase number which corresponds to this subroutine name.
!    For single phase subroutines, either omit this argument, or use the
!    parameter {\tt ESMF\_SINGLEPHASE}.   The Component writer must document
!    the requirements of the Component for how and when the multiple phases
!    are expected to be called.
!   \item[{[rc]}] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP

!BOP
! !IROUTINE: ESMF_CplCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_CplCompSetInternalState(component, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: component
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Available to be called by a component at any time, but expected to be
!  most useful when called during the Registration process, or Initialization.
!  Since Init, Run, and Finalize must be separate subroutines, data that
!  they need to share in common can either be module global data, or can
!  be allocated in a private data block, and the address of that block
!  can be registered with the framework and retrieved by subsequent calls.
!  When running multiple instantiations of a component, for example during
!  ensemble runs, it may be simpler to maintain private data specific to 
!  each run with private data blocks.  A corresponding {\tt GetInternalState}
!  call retrieves the data pointer.
!    
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component object returned from a {\tt ESMF\_CplCompCreate} call.
!   \item[dataPointer]
!    A pointer to the private data block, wrapped in a derived type which
!    contains only a pointer to the block.  This level of indirection is
!    needed to reliably set and retrieve the data block no matter which
!    architecture or compiler is used.  
!   \item[{[rc]}] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

!BOP
! !IROUTINE: ESMF_CplCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
!      subroutine ESMF_CplCompGetInternalState(component, dataPointer, rc)
!
! !ARGUMENTS:
!      type(ESMF_CplComp), intent(inout) :: component
!      type(any), pointer, intent(in) :: dataPointer
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Available to be called by a component at any time after 
!  {\tt SetInternalState} has been called.
!  Since Init, Run, and Finalize must be separate subroutines, data that
!  they need to share in common can either be module global data, or can
!  be allocated in a private data block, and the address of that block
!  can be registered with the framework and retrieved by this call.
!  When running multiple instantiations of a component, for example during
!  ensemble runs, it may be simpler to maintain private data specific to 
!  each run with private data blocks.  A corresponding {\tt SetInternalState}
!  call sets the data pointer to this block, and this call retrieves the 
!  data pointer.
!    
!  The arguments are:
!  \begin{description}
!   \item[component]
!    Component object returned from a {\tt ESMF\_CplCompCreate} call.
!   \item[dataPointer]
!    A derived type, containing only a pointer to the private data block.
!    The framework will fill in the block and when this call returns the
!    pointer is set to the same address set during 
!    {\tt ESMF\_CplSetInternalState}.
!    This level of indirection is needed to reliably set and retrieve 
!    the data block no matter which architecture or compiler is used.  
!   \item[{[rc]}] 
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
