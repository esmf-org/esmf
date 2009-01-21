! $Id: ESMF_CommTable.F90,v 1.14.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_CommTable.F90"
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
#include "ESMF.h"
!==============================================================================
!BOPI
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
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_BaseMod    ! ESMF base class
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!
!     !  ESMF_CommTable
!
!     ! Description of ESMF_CommTable. 

      type ESMF_CommTable
      sequence
      private
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
	ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
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
      public ESMF_CommTableGetInit

      public ESMF_CommTableCreate                 ! interface only, deep class
      public ESMF_CommTableDestroy                ! interface only, deep class

      public ESMF_CommTableGet                    ! get and set values
      public ESMF_CommTableSet
 
      public ESMF_CommTableValidate
      public ESMF_CommTablePrint
 
! < list the rest of the public interfaces here >
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_CommTable.F90,v 1.14.2.3 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_CommTableCreate 

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_CommTableCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for CommTable create
!     methods.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------

!     < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! Route Initialiation function
!
!------------------------------------------------------------------------------!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTableGetInit"
!BOPI
! !IROUTINE: ESMF_CommTableGetInit - Get the Init status 

! !INTERFACE:
      function ESMF_CommTableGetInit(d)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_CommTableGetInit
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(in),optional :: d
!
! !DESCRIPTION:
!     Get the init status
!
!     The arguments are:
!     \begin{description}
!     \item[d] 
!          The class to be queried 
!     \end{description}
!
!EOPI
  if (present(d)) then
     ESMF_CommTableGetInit=ESMF_INIT_GET(d)
  else
     ESMF_CommTableGetInit=ESMF_INIT_CREATED
  endif 
end function ESMF_CommTableGetInit
!==============================================================================
!
! This section includes the CommTable Create and Destroy methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTableCreateNew"
!BOPI
! !IROUTINE: ESMF_CommTableCreateNew - Create a new CommTable

! !INTERFACE:
      function ESMF_CommTableCreateNew(mypet, petcount, rc)
!
! !RETURN VALUE:
      type(ESMF_CommTable) :: ESMF_CommTableCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: mypet
      integer, intent(in) :: petcount
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt CommTable} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[mypet] 
!          The local PET number.
!     \item[petcount]
!          The total PET count in this VM.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        type (ESMF_CommTable) :: commtable     ! new C++ CommTable
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        commtable%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ! Call C++ create code
        call c_ESMC_CommTableCreate(commtable, mypet, petcount, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CommTableCreateNew = commtable

        if (rcpresent) rc = ESMF_SUCCESS

	ESMF_INIT_SET_CREATED(ESMF_CommTableCreateNew)

        end function ESMF_CommTableCreateNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTableDestroy"
!BOPI
! !IROUTINE: ESMF_CommTableDestroy - Free all resources associated with a CommTable 

! !INTERFACE:
      subroutine ESMF_CommTableDestroy(commtable, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(inout) :: commtable   
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
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ! Call C++ destroy code
        call c_ESMC_CommTableDestroy(commtable, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        commtable%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

	ESMF_INIT_SET_DELETED(commtable)

        end subroutine ESMF_CommTableDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTableGet"
!BOPI
! !IROUTINE: ESMF_CommTableGet - Get values from a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableGet(commtable, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(inout) :: commtable
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
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_CommTableGetInit,commtable,rc)

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
        call c_ESMC_CommTableGet(commtable, value1, value2, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTableSet"
!BOPI
! !IROUTINE: ESMF_CommTableSet - Set values in a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableSet(CommTable, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(inout) :: commtable
      integer, intent(in), optional :: value1
      integer, intent(in), optional :: value2
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
!EOPI

        ! local variables
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ESMF_INIT_CHECK_DEEP(ESMF_CommTableGetInit,commtable,rc)

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
        call c_ESMC_CommTableSet(commtable, value1, value2, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CommTableSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTableValidate"
!BOPI
! !IROUTINE: ESMF_CommTableValidate - Check internal consistency of a CommTable

! !INTERFACE:
      subroutine ESMF_CommTableValidate(commtable, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(inout) :: commtable       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a CommTable is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[commtable] 
!          Class to be queried.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

       character (len=6) :: defaultopts      ! default validate options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_RC_NOT_IMPL
       endif

       ESMF_INIT_CHECK_DEEP(ESMF_CommTableGetInit,commtable,rc)

       defaultopts = "quick"

       if(present(options)) then
           call c_ESMC_CommTableValidate(commtable, options, status)   
       else
           call c_ESMC_CommTableValidate(commtable, defaultopts, status)
       endif

        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CommTableValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommTablePrint"
!BOPI
! !IROUTINE: ESMF_CommTablePrint - Print the contents of a CommTable

! !INTERFACE:
      subroutine ESMF_CommTablePrint(commtable, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CommTable), intent(inout) :: commtable      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print information about a CommTable.  
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
!     \item[commtable] 
!          Class to be queried.
!     \item[{[options]}]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

       character (len=6) :: defaultopts      ! default print options
       integer :: status                     ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain       
       status = ESMF_RC_NOT_IMPL
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.  
         rc = ESMF_RC_NOT_IMPL
       endif

       ESMF_INIT_CHECK_DEEP(ESMF_CommTableGetInit,commtable,rc)

       defaultopts = "brief"

       if(present(options)) then
           call c_ESMC_CommTablePrint(commtable, options, status)   
       else
           call c_ESMC_CommTablePrint(commtable, defaultopts, status)
       endif

       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_CommTablePrint

!------------------------------------------------------------------------------

       end module ESMF_CommTableMod
