! $Id: ESMF_XPacket.F90,v 1.1 2003/03/04 14:59:21 nscollins Exp $
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
! This file contains the XPacket class definition and all XPacket class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOP
! !MODULE: ESMF_XPacketMod - Exchange packets used during Component coupling.
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt XPacket} class ...
!
! < Insert a paragraph or two explaining the function of this class. >
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF base class
      use ESMF_ArrayMod
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
!     !  ESMF_XPacketType
!
!     ! Description of ESMF_XPacketType. 

      type ESMF_XPacketType
      sequence
      private
        type (ESMF_Base) :: base
        type (ESMF_Array) :: tmpspace     ! is this a list?
        !type (ESMF_Route) :: route        ! is this a list?
        ! names?
      end type

!------------------------------------------------------------------------------
!     !  ESMF_XPacket
!
!     ! Description of ESMF_XPacket

      type ESMF_XPacket
      sequence
      private
        type (ESMF_XPacketType), pointer :: xtype
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_XPacketConfig
      public ESMF_XPacket
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

      public ESMF_XPacketCreate                 ! interface only, deep class
      public ESMF_XPacketDestroy                ! interface only, deep class

      public ESMF_XPacketGetConfig
      public ESMF_XPacketSetConfig
      public ESMF_XPacketGet                    ! Get multiple values
      public ESMF_XPacketSet                    ! Set multiple value
 
      public ESMF_XPacketValidate
      public ESMF_XPacketPrint
 
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_XPacket.F90,v 1.1 2003/03/04 14:59:21 nscollins Exp $'

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
      function ESMF_XPacketCreateNew(arg1, arg2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_XPacket) :: ESMF_XPacketCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: name  
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
!     \item[{[name]}] 
!          Exchange packet name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


        ! local variables
        type (ESMF_XPacketType), pointer :: xtype        ! new XPacket
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        nullify(ESMF_XPacketCreateNew%xtype)
        nullify(xtype)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif  

        allocate(xtype, stat=status)
        if (status .ne. 0) then             ! fortran rc, not esmf
          print *, "XPacket create error"
          return
        endif

        ! Call Construct method to fill in contents.
        call ESMF_XPacketConstruct(xtype, arg1, arg2, name, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "XPacket create error"
          return
        endif

        ! Set return value
        ESMF_XPacketCreateNew%xtype => xtype

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

      ! local vars
      integer :: status                       ! local error status
      logical :: rcpresent                    ! did user specify rc?

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Test for an already destroyed object
      if (.not. associated(xpacket%xtype)) then
        print *, "Calling XPacketDestroy on an empty object" 
        return
      endif

      ! Call Destruct to release resources
      call ESMF_XPacketDestruct(xpacket%xtype, status)
      if (status .ne. ESMF_SUCCESS) then
        return
      endif

      ! Deallocate the xpacketstruct itself
      deallocate(xpacket%xtype, stat=status)
      if (status .ne. 0) then
        print *, "XPacket contents destruction error"
        return
      endif
      nullify(xpacket%xtype)

      ! Set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS
 
      end subroutine ESMF_XPacketDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketConstruct - Construct the internals of an allocated XPacket

! !INTERFACE:
      subroutine ESMF_XPacketConstruct(xtype, arg1, arg2, name, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacketType), pointer :: xtype  ! xpacket to be filled in
      integer, intent(in), optional :: arg1                       
      integer, intent(in), optional :: arg2                      
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt XPacket} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_XPacketDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_XPacketCreate}, which calls
!     {\tt ESMF\_XPacketConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[xtype] 
!          The class to be constructed.
!     \item[{[arg1]}] 
!          Argument 1.
!     \item[{[arg2]}]
!          Argument 2.         
!     \item[{[name]}] 
!          Optional name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      ! local vars
      integer :: status                                ! local error status
      logical :: rcpresent                             ! did user specify rc?

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: fill in values here.
      call ESMF_SetName(xtype%base, name, "XPacket", status)

      !if (present(arg1)) then
      !  xtype%arg1 = arg1
      !else 
      !  xtype%arg1 = default_value
      !endif

      !
      !  code goes here
      !

      ! Set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_XPacketConstruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketDestruct - Free any XPacket memory allocated internally

! !INTERFACE:
      subroutine ESMF_XPacketDestruct(xtype, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacketType), pointer :: xtype    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!     {\tt  ESMF\_XPacketConstruct}, does any additional cleanup before the
!     original XPacket object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_XPacketDestroy}, which calls
!     {\tt ESMF_XPacketDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[xtype] 
!          The class to be destructed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      ! local vars
      integer :: status                       ! local error status
      logical :: rcpresent                    ! did user specify rc?

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! release any storage that was allocated
      !if (associated(xtype%pointer)) then
      !   deallocate(xtype%pointer, stat=status)
      !   if (status .ne. 0) then
      !     print *, "XPacket contents destruction error"
      !     return
      !   endif
      !   nullify(xtype%pointer)
      !endif

      !
      !  code goes here
      !

      ! Set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_XPacketDestruct

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

!
!  code goes here
!
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

!
!  code goes here
!
      end subroutine ESMF_XPacketSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketGet - Get information from a XPacket

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
!     Returns information from a XPacket object.
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

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.  
        rc = ESMF_FAILURE
      endif

      if (present(value1)) then
        !
        !  code goes here
        !
      endif

      if (present(value2)) then
        !
        !  code goes here
        !
      endif

      !
      !  code goes here
      !

      if(status .ne. ESMF_SUCCESS) then    
        print *, "ERROR in ESMF_XPacketGet" 
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_XPacketGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XPacketSet - Set information in a XPacket object.

! !INTERFACE:
      subroutine ESMF_XPacketSet(XPacket, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_XPacket), intent(in) :: xpacket
      integer, intent(in), optional :: value1
      integer, intent(in), optional :: value2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set XPacket values.
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


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.  
        rc = ESMF_FAILURE
      endif

      if (present(value1)) then
        !
        !  code goes here
        !
      endif

      if (present(value2)) then
        !
        !  code goes here
        !
      endif

      !
      !  code goes here
      !

      if(status .NE. ESMF_SUCCESS) then    
        print *, "ERROR in ESMF_XPacketSet"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

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

      integer :: status                       ! local error status
      logical :: rcpresent                    ! did user specify rc?
      character (len=6) :: defaultopts

      ! Initialize return code; assume failure until success is certain  
      status = ESMF_FAILURE       
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      defaultopts = "quick"

      ! Decide at what level of detail to verify.
      if(present(opt)) then
          ! TODO:  decide how much checking to do
      endif

      !
      !  TODO: code goes here
      !

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
      character (len=*), intent(in), optional :: opt      
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
!          Print options that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: status                       ! local error status
      logical :: rcpresent                    ! did user specify rc?
      character (len=6) :: defaultopts
      character (len=ESMF_MAXSTR) :: name

      ! Initialize return code; assume failure until success is certain  
      status = ESMF_FAILURE       
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      defaultopts = "brief"

      ! Decide what to print.
      if(present(opt)) then
          ! TODO:  decide what to print
      endif

      call ESMF_GetName(xpacket%xtype%base, name, status)
      print *, "XPacket print:"
      print *, "  name = ", trim(name)

      ! TODO: add more info here

      ! Set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_XPacketPrint

!------------------------------------------------------------------------------

      end module ESMF_XPacketMod
