! $Id: ESMF_CommTable.F90,v 1.1 2003/03/10 23:20:24 nscollins Exp $
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
!     ESMF CommTable Module
      module ESMF_CommTableMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the CommTable class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_Route.h>
!==============================================================================
!BOP
! !MODULE: ESMF_CommTableMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt CommTable} class ...
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
!     ! ESMF_CommTableConfig
!
!     ! Description of ESMF_CommTableConfig

      type ESMF_CommTableConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_CommTable
!
!     ! Description of ESMF_CommTable. 

      type ESMF_CommTable
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
      public ESMF_CommTableConfig
      public ESMF_CommTable
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
      public ESMF_CommTableCreate                 ! interface only, deep class
      public ESMF_CommTableDestroy                ! interface only, deep class

! the following routine applies to a shallow class
      public ESMF_CommTableInit                   ! shallow class

      public ESMF_CommTableGetConfig
      public ESMF_CommTableSetConfig
      public ESMF_CommTableGet                    ! get and set values
      public ESMF_CommTableSet
 
      public ESMF_CommTableValidate
      public ESMF_CommTablePrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_CommTable.F90,v 1.1 2003/03/10 23:20:24 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_CommTableCreate 

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CommTableCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for CommTable create
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
! This section includes the CommTable Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableCreateNew - Create a new CommTable

! !INTERFACE:
      function ESMF_CommTableCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_CommTable) :: ESMF_CommTableCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        
      integer, intent(in) :: arg2                        
      character (len = *), intent(in), optional :: arg3  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt CommTable} object and constructs its
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
        type (ESMF_CommTable) :: commtable     ! new C++ CommTable
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_FAILURE
        rcpresent = .FALSE.   
        commtable%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call C++ create code
        call c_ESMC_CommTableCreate(arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable create error"
          return  
        endif

        ! Set return values
        ESMF_CommTableCreateNew = commtable

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CommTableCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableDestroy - Free all resources associated with a CommTable 

! !INTERFACE:
      subroutine ESMF_CommTableDestroy(commtable, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt CommTable} object previously allocated
!     via an {\tt ESMF_CommTableCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
!          The class to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

        ! local variables
        type (ESMF_CommTable) :: commtable     ! new C++ CommTable
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
        call c_ESMC_CommTableDestroy(commtable, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable create error"
          return  
        endif

        ! nullify pointer
        <class%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableInit - Initialize a CommTable 

! !INTERFACE:
      subroutine ESMF_CommTableInit(commtable, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable   
      integer, intent(in) :: arg1                       
      integer, intent(in) :: arg2                       
      character (len = *), intent(in), optional :: arg3 
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     ESMF routine which only initializes {\tt CommTable} values; it does not
!     allocate any resources.  Define for shallow classes only, 
!     for deep classes define and use routines Create/Destroy.
!     Can be overloaded like ESMF_CommTableCreate via interface blocks.
!
!  The arguments are:
!     \begin{description}
!     \item[commtable]
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
        call c_ESMC_CommTableInit(commtable, arg1, arg2, arg3, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable init error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableGetConfig - Get configuration information from a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableGetConfig(commtable, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the CommTable object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
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
        call c_ESMC_CommTableGetConfig(commtable, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable GetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableSetConfig - Set configuration information for a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableSetConfig(commtable, config, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the CommTable object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
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
        call c_ESMC_CommTableSetConfig(commtable, config, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable SetConfig error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableGet - Get values from a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableGet(commtable, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of CommTable attribute <Value>.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
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
        call c_ESMC_CommTableGet(commtable, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable Get error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableSet - Set values in a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableSet(CommTable, value, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a CommTable attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
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
        call c_ESMC_CommTableSet(commtable, value1, value2, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "CommTable Set error"
          return  
        endif

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTableValidate - Check internal consistency of a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableValidate(commtable, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a CommTable is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
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
           call c_ESMC_CommTableValidate(commtable, options, status)   
       else
           call c_ESMC_CommTableValidate(commtable, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "CommTable validate error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CommTableValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommTablePrint - Print the contents of a CommTable

! !INTERFACE:
      subroutine ESMF_CommTablePrint(commtable, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in) :: commtable      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a CommTable.  
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
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
           call c_ESMC_CommTablePrint(commtable, options, status)   
       else
           call c_ESMC_CommTablePrint(commtable, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "CommTable print error"
         return
       endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_CommTablePrint

!------------------------------------------------------------------------------

       end module ESMF_CommTableMod
