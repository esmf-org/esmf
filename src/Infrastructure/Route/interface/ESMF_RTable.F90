! $Id: ESMF_RTable.F90,v 1.1 2003/03/10 23:20:28 nscollins Exp $
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
!     ESMF RTable Module
      module ESMF_RTableMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the RTable class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Route.h>
!==============================================================================
!BOP
! !MODULE: ESMF_RTableMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt RTable} class ...
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
!     ! ESMF_RTableConfig
!
!     ! Description of ESMF_RTableConfig

      type ESMF_RTableConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RTable
!
!     ! Description of ESMF_RTable. 

      type ESMF_RTable
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
      public ESMF_RTableConfig
      public ESMF_RTable
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
      public ESMF_RTableCreate                 ! interface only, deep class
      public ESMF_RTableDestroy                ! interface only, deep class

! the following routine applies to a shallow class
      public ESMF_RTableInit                   ! shallow class

      public ESMF_RTableGetConfig
      public ESMF_RTableSetConfig
      public ESMF_RTableGet                    ! get and set values
      public ESMF_RTableSet
 
      public ESMF_RTableValidate
      public ESMF_RTablePrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RTable.F90,v 1.1 2003/03/10 23:20:28 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_RTableCreate 

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_RTableCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for RTable create
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
! This section includes the RTable Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableCreateNew - Create a new RTable

! !INTERFACE:
      function ESMF_RTableCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_RTable) :: ESMF_RTableCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: arg3  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt RTable} object and constructs its
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
        type (ESMF_RTable) :: rtable     ! new C++ RTable
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        rtable%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_RTableCreate(arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable create error"
          return  
        endif

        ! Set return values
        ESMF_RTableCreateNew = rtable

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RTableCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableDestroy - Free all resources associated with a RTable 

! !INTERFACE:
      subroutine ESMF_RTableDestroy(rtable, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt RTable} object previously allocated
!     via an {\tt ESMF_RTableCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          The class to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        type (ESMF_RTable) :: rtable     ! new C++ RTable
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
        call c_ESMC_RTableDestroy(rtable, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable create error"
          return  
        endif

        ! nullify pointer
        <class%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableInit - Initialize a RTable 

! !INTERFACE:
      subroutine ESMF_RTableInit(rtable, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable   
      integer, intent(in) :: arg1                       
      integer, intent(in) :: arg2                       
      character (len = *), intent(in), optional :: arg3 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt RTable} values; it does not
!     allocate any resources.  Define for shallow classes only, 
!     for deep classes define and use routines Create/Destroy.
!     Can be overloaded like ESMF_RTableCreate via interface blocks.
!
!  The arguments are:
!     \begin{description}
!     \item[rtable]
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
        call c_ESMC_RTableInit(rtable, arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable init error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableGetConfig - Get configuration information from a RTable

! !INTERFACE:
      subroutine ESMF_RTableGetConfig(rtable, config, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the RTable object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
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
        call c_ESMC_RTableGetConfig(rtable, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable GetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableSetConfig - Set configuration information for a RTable

! !INTERFACE:
      subroutine ESMF_RTableSetConfig(rtable, config, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the RTable object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
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
        call c_ESMC_RTableSetConfig(rtable, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable SetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableGet - Get values from a RTable

! !INTERFACE:
      subroutine ESMF_RTableGet(rtable, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of RTable attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
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
        call c_ESMC_RTableGet(rtable, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableSet - Set values in a RTable

! !INTERFACE:
      subroutine ESMF_RTableSet(RTable, value, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a RTable attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
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
        call c_ESMC_RTableSet(rtable, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTableValidate - Check internal consistency of a RTable

! !INTERFACE:
      subroutine ESMF_RTableValidate(rtable, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a RTable is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
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
           call c_ESMC_RTableValidate(rtable, options, status)   
       else
           call c_ESMC_RTableValidate(rtable, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "RTable validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_RTableValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RTablePrint - Print the contents of a RTable

! !INTERFACE:
      subroutine ESMF_RTablePrint(rtable, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a RTable.  
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
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
           call c_ESMC_RTablePrint(rtable, options, status)   
       else
           call c_ESMC_RTablePrint(rtable, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "RTable print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RTablePrint

!------------------------------------------------------------------------------

       end module ESMF_RTableMod
