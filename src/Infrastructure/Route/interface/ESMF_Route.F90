! $Id: ESMF_Route.F90,v 1.1 2003/03/05 17:04:53 nscollins Exp $
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
!     ESMF Route Module
      module ESMF_RouteMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the Route class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Route.h>
!==============================================================================
!BOP
! !MODULE: ESMF_RouteMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt Route} class ...
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
!     ! ESMF_RouteConfig
!
!     ! Description of ESMF_RouteConfig

      type ESMF_RouteConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_Route
!
!     ! Description of ESMF_Route. 

      type ESMF_Route
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
      public ESMF_RouteConfig
      public ESMF_Route
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
      public ESMF_RouteCreate                 ! interface only, deep class
      public ESMF_RouteDestroy                ! interface only, deep class

! the following routine applies to a shallow class
      public ESMF_RouteInit                   ! shallow class

      public ESMF_RouteGetConfig
      public ESMF_RouteSetConfig
      public ESMF_RouteGet                    ! get and set values
      public ESMF_RouteSet
 
      public ESMF_RouteValidate
      public ESMF_RoutePrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Route.F90,v 1.1 2003/03/05 17:04:53 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RouteCreate 

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_RouteCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for Route create
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
! This section includes the Route Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteCreateNew - Create a new Route

! !INTERFACE:
      function ESMF_RouteCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RouteCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: arg3  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Route} object and constructs its
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
        type (ESMF_Route) :: route     ! new C++ Route
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        route%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_RouteCreate(arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route create error"
          return  
        endif

        ! Set return values
        ESMF_RouteCreateNew = route

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RouteCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteDestroy - Free all resources associated with a Route 

! !INTERFACE:
      subroutine ESMF_RouteDestroy(route, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt Route} object previously allocated
!     via an {\tt ESMF_RouteCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          The class to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        type (ESMF_Route) :: route     ! new C++ Route
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
        call c_ESMC_RouteDestroy(route, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route create error"
          return  
        endif

        ! nullify pointer
        <class%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteInit - Initialize a Route 

! !INTERFACE:
      subroutine ESMF_RouteInit(route, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route   
      integer, intent(in) :: arg1                       
      integer, intent(in) :: arg2                       
      character (len = *), intent(in), optional :: arg3 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt Route} values; it does not
!     allocate any resources.  Define for shallow classes only, 
!     for deep classes define and use routines Create/Destroy.
!     Can be overloaded like ESMF_RouteCreate via interface blocks.
!
!  The arguments are:
!     \begin{description}
!     \item[route]
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
        call c_ESMC_RouteInit(route, arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route init error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteGetConfig - Get configuration information from a Route

! !INTERFACE:
      subroutine ESMF_RouteGetConfig(route, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the Route object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
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
        call c_ESMC_RouteGetConfig(route, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route GetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteSetConfig - Set configuration information for a Route

! !INTERFACE:
      subroutine ESMF_RouteSetConfig(route, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the Route object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
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
        call c_ESMC_RouteSetConfig(route, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route SetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteGet - Get values from a Route

! !INTERFACE:
      subroutine ESMF_RouteGet(route, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of Route attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
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
        call c_ESMC_RouteGet(route, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteSet - Set values in a Route

! !INTERFACE:
      subroutine ESMF_RouteSet(Route, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a Route attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
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
        call c_ESMC_RouteSet(route, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "Route Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RouteSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RouteValidate - Check internal consistency of a Route

! !INTERFACE:
      subroutine ESMF_RouteValidate(route, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a Route is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Class to be queried.
!     \item[{[opt]}]
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
           call c_ESMC_RouteValidate(route, options, status)   
       else
           call c_ESMC_RouteValidate(route, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Route validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_RouteValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RoutePrint - Print the contents of a Route

! !INTERFACE:
      subroutine ESMF_RoutePrint(route, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Route), intent(in) :: route      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a Route.  
!
!     The arguments are:
!     \begin{description}
!     \item[route] 
!          Class to be queried.
!     \item[{[opt]}]
!          Print ptions that control the type of information and level of 
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
           call c_ESMC_RoutePrint(route, options, status)   
       else
           call c_ESMC_RoutePrint(route, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Route print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RoutePrint

!------------------------------------------------------------------------------

       end module ESMF_RouteMod
