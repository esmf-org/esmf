! $Id: ESMF_State.F90,v 1.2 2003/01/07 23:14:33 nscollins Exp $
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
!     ESMF State module
      module ESMF_StateMod
!
!==============================================================================
!
! This file contains the State class definition and all State
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_StateMod - Manage data states uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt State} class and associated functions and subroutines.  
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
!     ! ESMF_State
!
!     ! State data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_State
      sequence
      private
        type(ESMF_State), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateCreate
      public ESMF_StateDestroy

      !public ESMF_StateInit
      !public ESMF_StateRun
      !public ESMF_StateFinalize
 
      public ESMF_StateCheckpoint
      public ESMF_StateRestore
 
      public ESMF_StatePrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_State.F90,v 1.2 2003/01/07 23:14:33 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_StateCreate -- Generic interface to create an State

! !INTERFACE:
     interface ESMF_StateCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_StateCreateNew
        !module procedure ESMF_StateCreateSPMD
        !module procedure ESMF_StateCreateMPMD

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateCreate} functions.   
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
! This section includes the State Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateCreateNew -- Create a new State specifying all options.

! !INTERFACE:
      function ESMF_StateCreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new State and set the decomposition characteristics.
!
!  The return value is a new State.
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
        type (ESMF_State), pointer :: ptr   ! opaque pointer to new C++ State
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
        call c_ESMC_StateCreate(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State construction error"
          return
        endif

!       set return values
        ESMF_StateCreateNew%this => ptr 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_StateCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateCreateSPMD

! !INTERFACE:
      function ESMF_StateCreateSPMD(rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreateSPMD
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt State} object.
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
        type (ESMF_State), pointer :: a    ! pointer to new State
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
        !call c_ESMC_StateCreateSPMD(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "State construction error"
        !  return
        !endif

!       set return values
        ESMF_StateCreateSPMD%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_StateCreateSPMD


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateCreateMPMD

! !INTERFACE:
      function ESMF_StateCreateMPMD(rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateCreateMPMD
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt State} object.
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
        type (ESMF_State), pointer :: a    ! pointer to new State
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
        !call c_ESMC_StateCreateMPMD(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "State construction error"
        !  return
        !endif

!       set return values
        ESMF_StateCreateMPMD%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_StateCreateMPMD


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_StateDestroy(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt State}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[state]
!       Destroy contents of this {\tt State}.
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
        call c_ESMC_StateDestroy(state%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "State contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_StateDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_StateSetData(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of StateCreate which creates an empty 
!      State and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_StateSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the state.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_StateGet(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the state.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the state input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_StateGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for States
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_StateCheckpoint(state, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_State):: state 
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
        end subroutine ESMF_StateCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_StateRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_State) :: ESMF_StateRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! state name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a State from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_State) :: a 

!       this is just to shut the compiler up
        type (ESMF_State), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_StateRestore = a 
 
        end function ESMF_StateRestore


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_StatePrint(state, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about an state.
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
           call c_ESMC_StatePrint(state%this, options, status) 
       else
           call c_ESMC_StatePrint(state%this, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "State print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_StatePrint


       end module ESMF_StateMod

