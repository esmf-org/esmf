! $Id: ESMF_class.F90,v 1.19.2.4 2010/02/01 20:48:49 svasquez Exp $
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
! This file contains the <Class> class definition and all <Class> class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
! #include "ESMF_<Comp>.h"
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
!     !  ESMF_<Class>Type
!
!     ! Description of ESMF_<Class>Type. 
!     !  <this is used for a deep class only.>

      type ESMF_<Class>Type
      sequence
      private
        type (ESMF_Base) :: base
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_<Class>
!
!     ! Description of ESMF_<Class>

      type ESMF_<Class>
      sequence
      private
        ! < for a deep class, this is a pointer to the underlying type. >
        type (ESMF_<Class>Type), pointer :: <class>type
        ! < for a shallow class, the actual values go here. > 
        ! integer :: shallow
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
      public ESMF_<Class>Get                    ! Get multiple <Values>
      public ESMF_<Class>Set                    ! Set multiple <Values>
 
      public ESMF_<Class>Validate
      public ESMF_<Class>Print
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_class.F90,v 1.19.2.4 2010/02/01 20:48:49 svasquez Exp $'

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
      function ESMF_<Class>CreateNew(arg1, arg2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>CreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: name  
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
!     \item[{[name]}] 
!          Optional object name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


        ! local variables
        type (ESMF_<Class>Type), pointer :: <class>type  ! new <Class>
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        nullify(ESMF_<Class>CreateNew%<class>type)
        nullify(<class>type)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif  

        allocate(<class>type, stat=status)
        if (status .ne. 0) then             ! fortran rc, not esmf
          print *, "<Class> create error"
          return
        endif

        ! Call Construct method to fill in contents.
        call ESMF_<Class>Construct(<class>type, arg1, arg2, name, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "<Class> create error"
          return
        endif

        ! Set return value
        ESMF_<Class>CreateNew%<class>type => <class>type

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
      if (.not. associated(<class>%<class>type)) then
        print *, "Calling <Class>Destroy on an empty object" 
        return
      endif

      ! Call Destruct to release resources
      call ESMF_<Class>Destruct(<class>%<class>type, status)
      if (status .ne. ESMF_SUCCESS) then
        return
      endif

      ! Deallocate the <class>struct itself
      deallocate(<class>%<class>type, stat=status)
      if (status .ne. 0) then
        print *, "<Class> contents destruction error"
        return
      endif
      nullify(<class>%<class>type)

      ! Set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS
 
      end subroutine ESMF_<Class>Destroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Construct - Construct the internals of an allocated <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Construct(<class>type, arg1, arg2, name, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>Type), pointer :: <class>type
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
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
!     \item[<class>type] 
!          The class to be constructed.
!     \item[arg1]
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[{[name]}] 
!          Optional object name.
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
      call ESMF_SetName(<class>type%base, name, "<Class>", status)

      if (present(arg1)) then
        <class>type%arg1 = arg1
      else 
        <class>type%arg1 = default_value
      endif

      !
      !  code goes here
      !

      ! Set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_<Class>Construct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Destruct - Free any <Class> memory allocated internally

! !INTERFACE:
      subroutine ESMF_<Class>Destruct(<class>type, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>Type), pointer :: <class>type    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!     {\tt  ESMF\_<Class>Construct}, does any additional cleanup before the
!     original <Class> object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_<Class>Destroy}, which calls
!     {\tt ESMF_<Class>Destruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[<class>type] 
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
      if (associated(<class>type%pointer)) then
         deallocate(<class>type%pointer, stat=status)
         if (status .ne. 0) then
           print *, "<Class> contents destruction error"
           return
         endif
         nullify(<class>type%pointer)
      endif

      !
      !  code goes here
      !

      ! Set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_<Class>Destruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Init - Initialize a <Class> 

! !INTERFACE:
      subroutine ESMF_<Class>Init(<class>, arg1, arg2, name, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(inout) :: <class>   
      integer, intent(in), optional :: arg1                       
      integer, intent(in), optional :: arg2                       
      character (len = *), intent(in), optional :: name 
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
!     \item[{[arg1]}]
!          Argument 1.
!     \item[{[arg2]}]
!          Argument 2.         
!     \item[{[name]}] 
!          Optional object name.
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

      if (present(arg1)) then
        <class>%arg1 = arg1
      else
        <class>%arg1 = default_value
      endif

      !
      !  code goes here
      !

      ! Set return values
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

!
!  code goes here
!
      end subroutine ESMF_<Class>GetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>SetConfig - Set configuration information for a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>SetConfig(<class>, config, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(inout) :: <class>
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

!
!  code goes here
!
      end subroutine ESMF_<Class>SetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Get - Get information from a <Class>

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
!     Returns information from a <Class> object.
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
        print *, "ERROR in ESMF_<Class>Get"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_<Class>Get

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Set - Set information in a <Class> object.

! !INTERFACE:
      subroutine ESMF_<Class>Set(<Class>, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(inout) :: <class>
      integer, intent(in), optional :: value1
      integer, intent(in), optional :: value2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set <Class> values.
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
        print *, "ERROR in ESMF_<Class>Set"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_<Class>Set

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Validate - Check internal consistency of a <Class>

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

      end subroutine ESMF_<Class>Validate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Print - Print the contents of a <Class>

! !INTERFACE:
      subroutine ESMF_<Class>Print(<class>, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>      
      character (len=*), intent(in), optional :: opt      
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

      call ESMF_GetName(<class>%<class>type%base, name, status)
      print *, "<Class> print:"
      print *, "  name = ", trim(name)

      ! TODO: add more info here

      ! Set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_<Class>Print

!------------------------------------------------------------------------------

      end module ESMF_<Class>Mod
