! $Id: ESMF_PhysGrid.F90,v 1.6 2002/11/07 20:37:19 jwolfe Exp $
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
!     ESMF PhysGrid Module
      module ESMF_PhysGridMod
!
!==============================================================================
!
! This file contains the PhysGrid class definition and all PhysGrid class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_PhysGrid.h>
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_PhysGridMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt PhysGrid> class ...
!
! < Insert a paragraph or two explaining the function of this class. >
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF base class
!     use ESMF_<XXX>Mod   ! any other dependencies
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_PhysGridConfig
!
!     ! Description of ESMF_PhysGridConfig

      type ESMF_PhysGridConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysGridType
!
!     !  Description of ESMF_PhysGrid. 

      type ESMF_PhysGridType
      sequence
      private
        type (ESMF_Base) :: base
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysGrid
!
!     !  The PhysGrid data structure that is passed between languages.

      type ESMF_PhysGrid
      sequence
      private
        type (ESMF_PhysGridType), pointer :: ptr   ! pointer to a physgrid type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_PhysGridConfig
      public ESMF_PhysGrid
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!  Pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_PhysGridCreate                 ! interface only, deep class
    public ESMF_PhysGridDestroy                ! interface only, deep class

    public ESMF_PhysGridGetConfig
    public ESMF_PhysGridSetConfig
    public ESMF_PhysGridGetValue               ! Get<Value>
    public ESMF_PhysGridSetValue               ! Set<Value>
 
    public ESMF_PhysGridValidate
    public ESMF_PhysGridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysGrid.F90,v 1.6 2002/11/07 20:37:19 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_PhysGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for PhysGrid create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_PhysGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridConstructNew

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt PhysGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------

!    < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the PhysGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridCreateNew - Create a new PhysGrid

! !INTERFACE:
      function ESMF_PhysGridCreateNew(name, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateNew
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

!     integer, intent(in) :: arg1                        
!     integer, intent(in) :: arg2                        
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt PhysGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt PhysGrid} name.
!     \item[arg2]
!          Argument 2.         
!     \item[[arg3]] 
!          Argument 3.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysGridType), pointer :: physgrid   ! Pointer to new physgrid
      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize pointers
      nullify(physgrid)
      nullify(ESMF_PhysGridCreateNew%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(physgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_PhysGridConstructNew(physgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateNew: PhysGrid construct"
        return
      endif

!     Set return values.
      ESMF_PhysGridCreateNew%ptr => physgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridDestroy - Free all resources associated with a PhysGrid 

! !INTERFACE:
      subroutine ESMF_PhysGridDestroy(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt PhysGrid} object previously allocated
!     via an {\tt ESMF_PhysGridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          The class to be destroyed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridConstructNew - Construct the internals of an allocated
!                                 PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridConstructNew(physgrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType), intent(in) :: physgrid  
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt PhysGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_PhysGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_PhysGridCreate}, which calls
!     {\tt ESMF\_PhysGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Pointer to a {\tt PhysGrid}.
!     \item[arg1]
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[[name]] 
!          Argument 3.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructNew: PhysGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridDestruct - Free any PhysGrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_PhysGridDestruct(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_PhysGridConstruct}, does any additional cleanup before the
!     original PhysGrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_PhysGridDestroy}, which calls
!     {\tt ESMF_PhysGridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          The class to be destructed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridGetConfig - Get configuration information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the PhysGrid object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be queried.
!     \item[config]
!          Configuration information.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridSetConfig - Set configuration information for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the PhysGrid object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be configured.
!     \item[config]
!          Configuration information.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridGetValue - Get <Value> for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetValue(physgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of PhysGrid attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be queried.
!     \item[value]
!          Value to be retrieved.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridSetValue - Set <Value> for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetValue(PhysGrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a PhysGrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be modified.
!     \item[value]
!          Value to be set.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridValidate - Check internal consistency of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridValidate(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a PhysGrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be queried.
!     \item[[opt]]
!          Validation options.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end subroutine ESMF_PhysGridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridPrint - Print the contents of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridPrint(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a PhysGrid.  
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be queried.
!     \item[[opt]]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_PhysGridPrint

!------------------------------------------------------------------------------

      end module ESMF_PhysGridMod
