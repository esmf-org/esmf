! $Id: ESMF_RTable.F90,v 1.4 2004/04/20 22:54:19 nscollins Exp $
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
#include "ESMF.h"
!!#include "ESMF_Route.h"
!==============================================================================
!BOPI
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
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
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

      public ESMF_RTableGet                    ! get and set values
      public ESMF_RTableSet
 
      public ESMF_RTableValidate
      public ESMF_RTablePrint
 
! < list the rest of the public interfaces here >
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RTable.F90,v 1.4 2004/04/20 22:54:19 nscollins Exp $'

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
!------------------------------------------------------------------------------

!     < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the RTable Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RTableCreateNew - Create a new RTable

! !INTERFACE:
      function ESMF_RTableCreateNew(mydeid, decount, rc)
!
! !RETURN VALUE:
      type(ESMF_RTable) :: ESMF_RTableCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: mydeid
      integer, intent(in) :: decount
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
!EOPI
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
        call c_ESMC_RTableCreate(mydeid, decount, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable create error"
          return  
        endif

        ! Set return values
        ESMF_RTableCreateNew = rtable

        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_RTableCreateNew

!------------------------------------------------------------------------------
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
!EOPI
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
        call c_ESMC_RTableDestroy(rtable, status)
        if (status .ne. ESMF_SUCCESS) then  
          print *, "RTable create error"
          return  
        endif

        ! nullify pointer
        rtable%this = ESMF_NULL_POINTER

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_RTableDestroy


!------------------------------------------------------------------------------
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
!EOPI
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
!EOPI
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
!BOPI
! !IROUTINE: ESMF_RTableValidate - Check internal consistency of a RTable

! !INTERFACE:
      subroutine ESMF_RTableValidate(rtable, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable       
      character (len=*), intent(in), optional :: options    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a RTable is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          Class to be queried.
!     \item[{[options]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
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
!BOPI
! !IROUTINE: ESMF_RTablePrint - Print the contents of a RTable

! !INTERFACE:
      subroutine ESMF_RTablePrint(rtable, options, rc)
!
! !ARGUMENTS:
      type(ESMF_RTable), intent(in) :: rtable      
      character (len=*), intent(in), optional :: options      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a RTable.  
!
!     The arguments are:
!     \begin{description}
!     \item[rtable] 
!          Class to be queried.
!     \item[{[options]}]
!          Print options that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
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
