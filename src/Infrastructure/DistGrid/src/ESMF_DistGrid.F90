! $Id: ESMF_DistGrid.F90,v 1.8 2002/12/04 21:29:25 jwolfe Exp $
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
!     ESMF DistGrid Module
      module ESMF_DistGridMod
!
!==============================================================================
!
! This file contains the DistGrid class definition and all DistGrid class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_DistGrid.h>
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_DistGridMod - contains Grid decompostion methods
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt DistGrid} class, which contains a
! collection of subgrids which constitute a single logical {\tt Grid}. The
! subgrids can be operated on in parallel on a multiprocessor machine. The
! {\tt DistGrid} class contains the mapping between the local grid 
! decompositions and the global logical {\tt Grid}. It contains methods to
! synchronize data values between the boundaries of subsets, and to collect
! and communicate global data values. It interacts closely with the
! {\tt PhysGrid} object.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_DistGridConfig
!
!     ! Description of ESMF_DistGridConfig

      type ESMF_DistGridConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGridType
!
!     !  Description of ESMF_DistGrid. 

      type ESMF_DistGridType
      sequence
      private
        type (ESMF_Base) :: base
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGrid
!
!     !  The DistGrid data structure that is passed between languages.

      type ESMF_DistGrid
      sequence
      private
        type (ESMF_DistGridType), pointer :: ptr     ! pointer to a distgrid
                                                     ! type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_DistGridConfig
      public ESMF_DistGrid
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
    public ESMF_DistGridCreate                 ! interface only, deep class
    public ESMF_DistGridDestroy                ! interface only, deep class

    public ESMF_DistGridGetConfig
    public ESMF_DistGridSetConfig
    public ESMF_DistGridGetValue               ! Get<Value>
    public ESMF_DistGridSetValue               ! Set<Value>
 
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistGrid.F90,v 1.8 2002/12/04 21:29:25 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_DistGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for DistGrid create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridConstructNew

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt DistGrid}.
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
! This section includes the DistGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridCreateNew - Create a new DistGrid

! !INTERFACE:
      function ESMF_DistGridCreateNew(name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateNew
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

!     integer, intent(in) :: arg1                        
!     integer, intent(in) :: arg2                        
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt DistGrid} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]] 
!          {\tt DistGrid} name.
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

      type(ESMF_DistGridType), pointer :: distgrid  ! Pointer to new distgrid
      integer :: status=ESMF_FAILURE                ! Error status
      logical :: rcpresent=.FALSE.                  ! Return code present

!     Initialize pointers
      nullify(distgrid)
      nullify(ESMF_DistGridCreateNew%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(distgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructNew(distgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateNew: DistGrid construct"
        return
      endif

!     Set return values.
      ESMF_DistGridCreateNew%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridDestroy - Free all resources associated with a DistGrid 

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt DistGrid} object previously allocated
!     via an {\tt ESMF_DistGridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridConstructNew - Construct the internals of an allocated
!                                 DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructNew(distgrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), intent(in) :: distgrid 
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_DistGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt DistGrid}.
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
        print *, "ERROR in ESMF_DistGridConstructNew: DistGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridDestruct - Free any DistGrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_DistGridDestruct(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_DistGridConstruct}, does any additional cleanup before the
!     original DistGrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_DistGridDestroy}, which calls
!     {\tt ESMF_DistGridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridGetConfig - Get configuration information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the DistGrid object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetConfig - Set configuration information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the DistGrid object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridGetValue - Get <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetValue(distgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of DistGrid attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetValue - Set <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetValue(DistGrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a DistGrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridValidate - Check internal consistency of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridValidate(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a DistGrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridPrint - Print the contents of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridPrint(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a DistGrid.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridPrint

!------------------------------------------------------------------------------

      end module ESMF_DistGridMod
