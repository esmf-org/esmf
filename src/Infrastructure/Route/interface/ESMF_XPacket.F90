! $Id: ESMF_XPacket.F90,v 1.1 2003/03/05 17:04:54 nscollins Exp $
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
!     ESMF XPacket Module
      module ESMF_XPacketMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the XPacket class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Route.h>
!==============================================================================
!BOP
! !MODULE: ESMF_XPacketMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt XPacket} class ...
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
!     ! ESMF_XPacketConfig
!
!     ! Description of ESMF_XPacketConfig

      type ESMF_XPacketConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_XPacket
!
!     ! Description of ESMF_XPacket. 

      type ESMF_XPacket
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
      public ESMF_XPacketConfig
      public ESMF_XPacket
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
      public ESMF_XPacketCreate                 ! interface only, deep class
      public ESMF_XPacketDestroy                ! interface only, deep class

! the following routine applies to a shallow class
      public ESMF_XPacketInit                   ! shallow class

      public ESMF_XPacketGetConfig
      public ESMF_XPacketSetConfig
      public ESMF_XPacketGet                    ! get and set values
      public ESMF_XPacketSet
 
      public ESMF_XPacketValidate
      public ESMF_XPacketPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_XPacket.F90,v 1.1 2003/03/05 17:04:54 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_XPacketCreate 

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_XPacketCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for XPacket create
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
! This section includes the XPacket Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketCreateNew - Create a new XPacket

! !INTERFACE:
      function ESMF_XPacketCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_XPacket) :: ESMF_XPacketCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: arg3  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt XPacket} object and constructs its
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
        type (ESMF_XPacket) :: xpacket     ! new C++ XPacket
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        xpacket%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_XPacketCreate(arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket create error"
          return  
        endif

        ! Set return values
        ESMF_XPacketCreateNew = xpacket

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_XPacketCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketDestroy - Free all resources associated with a XPacket 

! !INTERFACE:
      subroutine ESMF_XPacketDestroy(xpacket, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt XPacket} object previously allocated
!     via an {\tt ESMF_XPacketCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
!          The class to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        type (ESMF_XPacket) :: xpacket     ! new C++ XPacket
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
        call c_ESMC_XPacketDestroy(xpacket, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket create error"
          return  
        endif

        ! nullify pointer
        <class%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketInit - Initialize a XPacket 

! !INTERFACE:
      subroutine ESMF_XPacketInit(xpacket, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket   
      integer, intent(in) :: arg1                       
      integer, intent(in) :: arg2                       
      character (len = *), intent(in), optional :: arg3 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt XPacket} values; it does not
!     allocate any resources.  Define for shallow classes only, 
!     for deep classes define and use routines Create/Destroy.
!     Can be overloaded like ESMF_XPacketCreate via interface blocks.
!
!  The arguments are:
!     \begin{description}
!     \item[xpacket]
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
        call c_ESMC_XPacketInit(xpacket, arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket init error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketGetConfig - Get configuration information from a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketGetConfig(xpacket, config, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the XPacket object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
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
        call c_ESMC_XPacketGetConfig(xpacket, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket GetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketSetConfig - Set configuration information for a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketSetConfig(xpacket, config, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the XPacket object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
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
        call c_ESMC_XPacketSetConfig(xpacket, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket SetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketGet - Get values from a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketGet(xpacket, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of XPacket attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
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
        call c_ESMC_XPacketGet(xpacket, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketSet - Set values in a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketSet(XPacket, value, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a XPacket attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
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
        call c_ESMC_XPacketSet(xpacket, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "XPacket Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XPacketSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketValidate - Check internal consistency of a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketValidate(xpacket, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a XPacket is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
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
           call c_ESMC_XPacketValidate(xpacket, options, status)   
       else
           call c_ESMC_XPacketValidate(xpacket, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "XPacket validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_XPacketValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketPrint - Print the contents of a XPacket

! !INTERFACE:
      subroutine ESMF_XPacketPrint(xpacket, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a XPacket.  
!
!     The arguments are:
!     \begin{description}
!     \item[xpacket] 
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
           call c_ESMC_XPacketPrint(xpacket, options, status)   
       else
           call c_ESMC_XPacketPrint(xpacket, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "XPacket print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_XPacketPrint

!------------------------------------------------------------------------------

       end module ESMF_XPacketMod
