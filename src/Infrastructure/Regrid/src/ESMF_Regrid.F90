! $Id: ESMF_Regrid.F90,v 1.6 2002/11/07 18:31:07 jwolfe Exp $
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
!     ESMF Regrid Module
      module ESMF_RegridMod
!
!==============================================================================
!
! This file contains the Regrid class definition and all Regrid class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Regrid.h>
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_RegridMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the Regrid class ...
!
! < Insert a paragraph or two explaining the function of this class. >
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF base class
!      use ESMF_GridMod    ! ESMF grid class
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_RegridConfig
!
!     ! Description of ESMF_RegridConfig

      type ESMF_RegridConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RegridType
!
!     ! Description of ESMF_Regrid.

      type ESMF_RegridType
      sequence
      private
        type (ESMF_Base) :: base
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_Regrid
!
!     !  The Regrid data structure that is passed between languages. 

      type ESMF_Regrid
      sequence
      private
        type (ESMF_RegridType), pointer :: ptr     ! pointer to a regrid type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_RegridConfig
      public ESMF_Regrid
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
    public ESMF_RegridCreate                 ! interface only, deep class
    public ESMF_RegridDestroy                ! interface only, deep class

!   public ESMF_RegridGetConfig
!   public ESMF_RegridSetConfig
!   public ESMF_RegridGetValue               ! Get<Value>
!   public ESMF_RegridSetValue               ! Set<Value>
 
!   public ESMF_RegridValidate
!   public ESMF_RegridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Regrid.F90,v 1.6 2002/11/07 18:31:07 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RegridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for Regrid create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_RegridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridConstructNew

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct a
!     complete {\tt Regrid}.
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
! This section includes the Regrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridCreateNew - Create a new Regrid

! !INTERFACE:
      function ESMF_RegridCreateNew(name, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateNew
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

!     integer, intent(in) :: arg1                        
!     integer, intent(in) :: arg2                        
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Regrid} object and constructs its
!     internals.  Return a pointer to a new {\tt Regrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]] 
!          {\tt Regrid} name.
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

      type(ESMF_RegridType), pointer :: regrid    ! Pointer to new regrid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(regrid)
      nullify(ESMF_RegridCreateNew%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(regrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_RegridCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_RegridConstructNew(regrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_RegridCreateNew: Regrid construct"
        return
      endif

!     Set return values.
      ESMF_RegridCreateNew%ptr => regrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridDestroy - Free all resources associated with a Regrid 

! !INTERFACE:
      subroutine ESMF_RegridDestroy(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt Regrid} object previously allocated
!     via an {\tt ESMF_RegridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridConstructNew - Construct the internals of an allocated Regrid

! !INTERFACE:
      subroutine ESMF_RegridConstructNew(regrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(in) :: regrid
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt Regrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_RegridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_RegridCreate}, which calls
!     {\tt ESMF\_RegridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
!          Pointer to a {\tt Regrid}
!     \item[arg1]
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[[name]] 
!          {\tt Regrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_RegridConstructNew: Regrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridDestruct - Free any Regrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_RegridDestruct(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_RegridConstruct}, does any additional cleanup before the
!     original Regrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_RegridDestroy}, which calls
!     {\tt ESMF_RegridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridGetConfig - Get configuration information from a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGetConfig(regrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the Regrid object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridSetConfig - Set configuration information for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSetConfig(regrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the Regrid object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridGetValue - Get <Value> for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGetValue(regrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of Regrid attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridSetValue - Set <Value> for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSetValue(Regrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a Regrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridValidate - Check internal consistency of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridValidate(regrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a Regrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_RegridPrint - Print the contents of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridPrint(regrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a Regrid.  
!
!     The arguments are:
!     \begin{description}
!     \item[regrid] 
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
      end subroutine ESMF_RegridPrint

!------------------------------------------------------------------------------

      end module ESMF_RegridMod
