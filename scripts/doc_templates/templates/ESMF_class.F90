! $Id: ESMF_class.F90,v 1.12 2002/11/05 18:04:20 cdeluca Exp $
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
!     ESMF <Class> Module
      module ESMF_<Class>Mod
!
!==============================================================================
!
! This file contains the <Class> class definition and all <Class> class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_<Comp>.h>
!==============================================================================
!BOP
! !MODULE: ESMF_<Class>Mod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt <Class>} class ...
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
!     ! ESMF_<Class>Config
!
!     ! Description of ESMF_<Class>Config

      type ESMF_<Class>Config
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_<Class>
!
!     ! Description of ESMF_<Class>. 

      type ESMF_<Class>
      sequence
      private
!       type (ESMF_Base) :: base
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_<Class>Config
      public ESMF_<Class>
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
    public ESMF_<Class>Create                 ! interface only, deep class
    public ESMF_<Class>Destroy                ! interface only, deep class
    public ESMF_<Class>Construct              ! internal only, deep class
    public ESMF_<Class>Destruct               ! internal only, deep class

! the following routine applies to a shallow class
    public ESMF_<Class>Init                   ! shallow class

    public ESMF_<Class>GetConfig
    public ESMF_<Class>SetConfig
    public ESMF_<Class>GetValue               ! Get<Value>
    public ESMF_<Class>SetValue               ! Set<Value>
 
    public ESMF_<Class>Validate
    public ESMF_<Class>Print
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_class.F90,v 1.12 2002/11/05 18:04:20 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_<Class>Create 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_<Class>CreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for <Class> create
!     methods.
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
! This section includes the <Class> Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>CreateNew - Create a new <Class>

! !INTERFACE:
      function ESMF_<Class>CreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>CreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: arg3  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt <Class>} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[arg1] 
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[[arg3]] 
!          Argument 3.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

!
!  code goes here
!
      end function ESMF_<Class>CreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>Destroy - Free all resources associated with a <Class> 

! !INTERFACE:
      subroutine ESMF_<Class>Destroy(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt <Class>} object previously allocated
!     via an {\tt ESMF_<Class>Create routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>Destroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>Construct - Construct the internals of an allocated <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Construct(<class>, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   ! <class> to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt <Class>} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_<Class>Destruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_<Class>Create}, which calls
!     {\tt ESMF\_<Class>Construct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
!          The class to be constructed.
!     \item[arg1]
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[[arg3]] 
!          Argument 3.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_<Class>Construct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>Destruct - Free any <Class> memory allocated internally

! !INTERFACE:
      subroutine ESMF_<Class>Destruct(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_<Class>Construct}, does any additional cleanup before the
!     original <Class> object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_<Class>Destroy}, which calls
!     {\tt ESMF_<Class>Destruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>Destruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>Init - Initialize a <Class> 

! !INTERFACE:
      subroutine ESMF_<Class>Init(<class>, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   
      integer, intent(in) :: arg1                       
      integer, intent(in) :: arg2                       
      character (len = *), intent(in), optional :: arg3 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt <Class>} values; it does not
!     allocate any resources.  Define for shallow classes only, 
!     for deep classes define and use routines Create/Destroy and 
!     Construct/Destruct.  Can be overloaded like ESMF_<Class>Create
!     via interface blocks.
!
!  The arguments are:
!     \begin{description}
!     \item[<class>]
!          Class to be initialized.
!     \item[arg1] 
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[[arg3]] 
!          Argument 3.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_<Class>Init

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>GetConfig - Get configuration information from a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>GetConfig(<class>, config, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the <Class> object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>GetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>SetConfig - Set configuration information for a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>SetConfig(<class>, config, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the <Class> object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>SetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>GetValue - Get <Value> for a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>GetValue(<class>, value, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of <Class> attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>GetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>SetValue - Set <Value> for a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>SetValue(<Class>, value, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a <Class> attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>SetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>Validate - Check internal consistency of a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Validate(<class>, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a <Class> is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>Validate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_<Class>Print - Print the contents of a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Print(<class>, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a <Class>.  
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
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
      end subroutine ESMF_<Class>Print

!------------------------------------------------------------------------------

      end module ESMF_<Class>Mod
