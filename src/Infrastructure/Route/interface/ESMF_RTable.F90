! $Id: ESMF_RTable.F90,v 1.15.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_RTable.F90"
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
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RTableMod - Route table containing operations to perform
!
! !DESCRIPTION:
!
! The code in this file implements the F90 wrapper code for the C++
!  implementation of the {\tt ESMF\_RTable} class, which describes how to move
!  or send data from one address space to another to accomplish a predefined
!  data movement operation.  This is precomputed for speed, and at execution
!  time the only input needed are the base address pointers of the data blocks.
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

!------------------------------------------------------------------------------
!     !  ESMF_RTable
!
!     ! Description of ESMF_RTable. 

      type ESMF_RTable
      sequence
      private
        type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
	ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_RTable
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

      public ESMF_RTableGetInit

      public ESMF_RTableCreate                 ! interface only, deep class
      public ESMF_RTableDestroy                ! interface only, deep class

      public ESMF_RTableGet                    ! get and set values
      public ESMF_RTableSet
 
      public ESMF_RTableValidate
      public ESMF_RTablePrint
 
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RTable.F90,v 1.15.2.3 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_RTableCreate 

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_RTableCreateNew

! !DESCRIPTION:
!     This interface provides a single entry point for RTable create
!     methods.
!
!EOPI
      end interface 
!
!==============================================================================

      contains

!==============================================================================
!
! RTable Initialiation and Validation functions
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTableGetInit"
!BOPI
! !IROUTINE: ESMF_RTableGetInit - Get the Init status 

! !INTERFACE:
      function ESMF_RTableGetInit(d)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_RTableGetInit
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in),optional :: d
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
     ESMF_RTableGetInit=ESMF_INIT_GET(d)
  else
     ESMF_RTableGetInit=ESMF_INIT_CREATED
  endif 
end function ESMF_RTableGetInit

!==============================================================================
!
! This section includes the RTable Create and Destroy methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTableCreateNew"
!BOPI
! !IROUTINE: ESMF_RTableCreateNew - Create a new RTable

! !INTERFACE:
      function ESMF_RTableCreateNew(myvmid, decount, rc)
!
! !RETURN VALUE:
      type(ESMF_RTable) :: ESMF_RTableCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: myvmid
      integer, intent(in) :: decount
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_RTable} object and constructs its
!     internals.
!
!     The arguments are:
!     \begin{description}
!     \item[myvmid] 
!         The local DE number.
!     \item[decount]
!         Total number of DE slots to create in the table.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        type (ESMF_RTable) :: rtable     ! new C++ RTable
        integer :: status                  ! local error status
        logical :: rcpresent               ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.   
        rtable%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ! Call C++ create code
        call c_ESMC_RTableCreate(myvmid, decount, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_RTableCreateNew = rtable

        if (rcpresent) rc = ESMF_SUCCESS

	ESMF_INIT_SET_CREATED(ESMF_RTableCreateNew)

        end function ESMF_RTableCreateNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTableDestroy"
!BOPI
! !IROUTINE: ESMF_RTableDestroy - Free all resources associated with a RTable 

! !INTERFACE:
      subroutine ESMF_RTableDestroy(rtable, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(inout) :: rtable   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt ESMF\_RTable} object previously allocated
!     via an {\tt ESMF_RTableCreate()} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          The {\tt ESMF\_RTable} to be destroyed.
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
        call c_ESMC_RTableDestroy(rtable, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! nullify pointer
        rtable%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

	ESMF_INIT_SET_DELETED(rtable)

        end subroutine ESMF_RTableDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTableGet"
!BOPI
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
!     Returns information from an {\tt ESMF\_RTable}.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          {\tt ESMF\_RTable} to be queried.
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

        ESMF_INIT_CHECK_DEEP(ESMF_RTableGetInit,rtable,rc)

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
	! This function is not implemented yet -- P.Li 11/27/06
        call c_ESMC_RTableGet(rtable, value1, value2, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTablePrint"
!BOPI
! !IROUTINE: ESMF_RTablePrint - Print the contents of a RTable

! !INTERFACE:
      subroutine ESMF_RTablePrint(rtable, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(inout) :: rtable      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print information about an {\tt ESMF\_RTable}.  
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
!     \item[rtable] 
!          {\tt ESMF\_RTable} to be queried.
!     \item[{[options]}]
!          Print options that control the type of information and level of 
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

       ! Check initialization
       ESMF_INIT_CHECK_DEEP(ESMF_RTableGetInit,rtable,rc)

       defaultopts = "brief"

       if(present(options)) then
           call c_ESMC_RTablePrint(rtable, options, status)   
       else
           call c_ESMC_RTablePrint(rtable, defaultopts, status)
       endif

       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS
 
       end subroutine ESMF_RTablePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTableSet"
!BOPI
! !IROUTINE: ESMF_RTableSet - Set values in a RTable

! !INTERFACE:
      subroutine ESMF_RTableSet(RTable, value1, value2, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable
      integer, intent(out), optional :: value1
      integer, intent(out), optional :: value2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Modify an {\tt ESMF\_RTable}.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          {\tt ESMF\_RTable} to be modified.
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

        ESMF_INIT_CHECK_DEEP(ESMF_RTableGetInit,rtable,rc)

        if (present(value1)) then
          ! code to be added here
        endif

        if (present(value2)) then
          ! code to be added here
        endif

        ! Call C++  code
	! This function is not implemented yet -- P.Li 11/27/06
        call c_ESMC_RTableSet(rtable, value1, value2, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RTableValidate"
!BOPI
! !IROUTINE: ESMF_RTableValidate - Check internal consistency of a RTable

! !INTERFACE:
      subroutine ESMF_RTableValidate(rtable, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(inout) :: rtable       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Verifies that an {\tt ESMF\_RTable} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          {\tt ESMF\_RTable} to be checked.
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

       ! Check initialization
       ESMF_INIT_CHECK_DEEP(ESMF_RTableGetInit,rtable,rc)

       defaultopts = "quick"

       if(present(options)) then
           call c_ESMC_RTableValidate(rtable, options, status)   
       else
           call c_ESMC_RTableValidate(rtable, defaultopts, status)
       endif

       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_RTableValidate

!------------------------------------------------------------------------------

       end module ESMF_RTableMod
