! $Id: ESMF_Cpl.F90,v 1.1 2003/01/07 21:38:19 nscollins Exp $
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
!     ESMF Cpl module
      module ESMF_CplMod
!
!==============================================================================
!
! This file contains the Coupler class definition and all Coupler component
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_CplMod - Manage data coupling uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Coupler Component} class and associated functions and subroutines.  
!
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      !use ESMF_ComponentMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Cpl
!
!     ! Cpl data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Cpl
      sequence
      private
        type(ESMF_Cpl), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Cpl
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_CplCreate
      public ESMF_CplDestroy
 
      !public ESMF_CplInit
      !public ESMF_CplRun
      !public ESMF_CplFinalize
 
      public ESMF_CplCheckpoint
      public ESMF_CplRestore
 
      public ESMF_CplPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Cpl.F90,v 1.1 2003/01/07 21:38:19 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_CplCreate -- Generic interface to create a Coupler

! !INTERFACE:
     interface ESMF_CplCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_CplCreateNew

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_CplCreate} functions.   
!
!  \begin{description}
!  \item[xxx]
!    Description of xxx.
!  \item[yyy]
!    Description of yyy.
!  \item[[zzz]]
!    Description of optional arg zzz.
!  \item[rc]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!  
!EOP 
end interface

!------------------------------------------------------------------------------


!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Cpl Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCreateNew -- Create a new Cpl specifying all options.

! !INTERFACE:
      function ESMF_CplCreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_Cpl) :: ESMF_CplCreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Cpl and set the decomposition characteristics.
!
!  The return value is a new Cpl.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_Cpl), pointer :: ptr   ! opaque pointer to new C++ Cpl
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        nullify(ptr)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        call c_ESMC_CplCreate(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Cpl construction error"
          return
        endif

!       set return values
        ESMF_CplCreateNew%this => ptr 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CplCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCreateNoData

! !INTERFACE:
      function ESMF_CplCreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_Cpl) :: ESMF_CplCreateNoData
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt Cpl} object.
!
!  The arguments are:
!  \begin{description}
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       ! Local variables
        type (ESMF_Cpl), pointer :: a    ! pointer to new Cpl
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       ! Initialize pointer
        nullify(a)

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! C routine which interfaces to the C++ routine which does actual work
        !call c_ESMC_CplCreateNoData(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Cpl construction error"
        !  return
        !endif

!       set return values
        ESMF_CplCreateNoData%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CplCreateNoData


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CplDestroy(cpl, rc)
!
! !ARGUMENTS:
      type(ESMF_Cpl) :: cpl
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Cpl}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[cpl]
!       Destroy contents of this {\tt Cpl}.
!
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       call Destroy to release resources on the C++ side
        call c_ESMC_CplDestroy(cpl%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Cpl contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CplDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CplSetData(cpl, rc)
!
! !ARGUMENTS:
      type(ESMF_Cpl) :: cpl 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of CplCreate which creates an empty 
!      Cpl and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_CplSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the cpl.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CplGet(cpl, rc)
!
! !ARGUMENTS:
      type(ESMF_Cpl) :: cpl
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the cpl.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the cpl input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_CplGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Cpls
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CplCheckpoint(cpl, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Cpl):: cpl 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_CplCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_CplRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Cpl) :: ESMF_CplRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! cpl name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Cpl from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Cpl) :: a 

!       this is just to shut the compiler up
        type (ESMF_Cpl), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_CplRestore = a 
 
        end function ESMF_CplRestore


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_CplPrint(cpl, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Cpl) :: cpl
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a cpl.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts="brief"
       integer :: status=ESMF_FAILURE      ! local error status
       logical :: rcpresent=.FALSE.

!      Initialize return code; assume failure until success is certain
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

!      ! Interface to call the C++ print code
       if(present(options)) then
           call c_ESMC_CplPrint(cpl%this, options, status) 
       else
           call c_ESMC_CplPrint(cpl%this, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Cpl print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CplPrint


       end module ESMF_CplMod

