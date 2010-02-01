! $Id: inter_ESMF_class.F90,v 1.4.2.4 2010/02/01 20:48:49 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
!     ESMF <Class> Module
      module ESMF_<Class>Mod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the <Class> class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_<Comp>.h"
!==============================================================================
!BOP
! !MODULE: ESMF_<Class>Mod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt <Class>} class ...
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
!       ! Pick 1 of the following 2 choices:
!       ! If this is a shallow class, this must match exactly the C++
!       ! class definition in size and order.
        integer :: dummy1
        integer :: dummy2
!       < insert other class members here >

!       ! If this is a deep class, the derived type contains only a 
!       ! place to hold the C++ 'this' pointer
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
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

! the following routine applies to a shallow class
      public ESMF_<Class>Init                   ! shallow class

      public ESMF_<Class>GetConfig
      public ESMF_<Class>SetConfig
      public ESMF_<Class>Get                    ! get and set values
      public ESMF_<Class>Set
 
      public ESMF_<Class>Validate
      public ESMF_<Class>Print
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: inter_ESMF_class.F90,v 1.4.2.4 2010/02/01 20:48:49 svasquez Exp $'

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

!     < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the <Class> Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>CreateNew - Create a new <Class>

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
!     \item[{[arg3]}] 
!          Argument 3.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

        ! local variables
        type (ESMF_<Class>) :: <class>     ! new C++ <Class>
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        <class>%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_<Class>Create(arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> create error"
          return  
        endif

        ! Set return values
        ESMF_<Class>CreateNew = <class>

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_<Class>CreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Destroy - Free all resources associated with a <Class> 

! !INTERFACE:
      subroutine ESMF_<Class>Destroy(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(inout) :: <class>   
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
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ destroy code
        call c_ESMC_<Class>Destroy(<class>, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> create error"
          return  
        endif

        ! nullify pointer
        <class>%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>Destroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Init - Initialize a <Class> 

! !INTERFACE:
      subroutine ESMF_<Class>Init(<class>, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(inout) :: <class>   
      integer, intent(in) :: arg1                       
      integer, intent(in) :: arg2                       
      character (len = *), intent(in), optional :: arg3 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt <Class>} values; it does not
!     allocate any resources.  Define for shallow classes only, 
!     for deep classes define and use routines Create/Destroy.
!     Can be overloaded like ESMF_<Class>Create via interface blocks.
!
!  The arguments are:
!     \begin{description}
!     \item[<class>]
!          Class to be initialized.
!     \item[arg1] 
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[{[arg3]}] 
!          Argument 3.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ Init code
        call c_ESMC_<Class>Init(<class>, arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> init error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>Init

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>GetConfig - Get configuration information from a <Class>

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
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_<Class>GetConfig(<class>, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> GetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>GetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>SetConfig - Set configuration information for a <Class>

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
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 


        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++  code
        call c_ESMC_<Class>SetConfig(<class>, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> SetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>SetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Get - Get values from a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Get(<class>, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of <Class> attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
!          Class to be queried.
!     \item[{[value1]}]
!          Value to be retrieved.         
!     \item[{[value2]}]
!          Value to be retrieved.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
        call c_ESMC_<Class>Get(<class>, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>Get

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Set - Set values in a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Set(<Class>, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      integer, intent(in), optional :: value1
      integer, intent(in), optional :: value2
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
!     \item[{[value1]}]
!          Value to be set.         
!     \item[{[value2]}]
!          Value to be set.         
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
        call c_ESMC_<Class>Set(<class>, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "<Class> Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>Set

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Validate - Check internal consistency of a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Validate(<class>, options, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a <Class> is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
!          Class to be queried.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n
!
       character (len=6) :: defaultopts      ! default validate options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_FAILURE
       endif

       defaultopts = "quick"

       if(present(options)) then
           call c_ESMC_<Class>Validate(<class>, options, status)   
       else
           call c_ESMC_<Class>Validate(<class>, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "<Class> validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_<Class>Validate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Print - Print the contents of a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Print(<class>, options, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print information about a <Class>.  
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
!          Class to be queried.
!     \item[{[options]}]
!          Print options that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

       character (len=6) :: defaultopts      ! default print options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

       if(present(options)) then
           call c_ESMC_<Class>Print(<class>, options, status)   
       else
           call c_ESMC_<Class>Print(<class>, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "<Class> print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_<Class>Print

!------------------------------------------------------------------------------

       end module ESMF_<Class>Mod
