! $Id: ESMF_Component.F90,v 1.1 2003/01/07 21:38:18 nscollins Exp $
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
!     ESMF Component module
      module ESMF_ComponentMod
!
!==============================================================================
!
! This file contains the Component class definition and all Component
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_ComponentMod - Manage data components uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Component} class and associated functions and subroutines.  
!
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Component
!
!     ! Component data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Component
      sequence
      private
        type(ESMF_Component), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Component
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ComponentCreate
      public ESMF_ComponentDestroy
 
      !public ESMF_ComponentInit
      !public ESMF_ComponentRun
      !public ESMF_ComponentFinalize
 
      public ESMF_ComponentCheckpoint
      public ESMF_ComponentRestore
      public ESMF_ComponentWrite
      public ESMF_ComponentRead
 
      public ESMF_ComponentPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Component.F90,v 1.1 2003/01/07 21:38:18 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_ComponentCreate -- Generic interface to create an Component

! !INTERFACE:
     interface ESMF_ComponentCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_ComponentCreateNew
!        !module procedure ESMF_ComponentCreateNoData

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ComponentCreate} functions.   
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
! This section includes the Component Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ComponentCreateNew -- Create a new Component specifying all options.

! !INTERFACE:
      function ESMF_ComponentCreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_Component) :: ESMF_ComponentCreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Component and set the decomposition characteristics.
!
!  The return value is a new Component.
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
        type (ESMF_Component), pointer :: ptr   ! opaque pointer to new C++ Component
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
        call c_ESMC_ComponentCreate(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

!       set return values
        ESMF_ComponentCreateNew%this => ptr 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ComponentCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ComponentCreateNoData

! !INTERFACE:
      function ESMF_ComponentCreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_Component) :: ESMF_ComponentCreateNoData
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt Component} object.
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
        type (ESMF_Component), pointer :: a    ! pointer to new Component
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
        !call c_ESMC_ComponentCreateNoData(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Component construction error"
        !  return
        !endif

!       set return values
        ESMF_ComponentCreateNoData%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ComponentCreateNoData


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ComponentDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Component) :: component
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Component}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[component]
!       Destroy contents of this {\tt Component}.
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
        call c_ESMC_ComponentDestroy(component%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ComponentDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ComponentSetData(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Component) :: component 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of ComponentCreate which creates an empty 
!      Component and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_ComponentSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the component.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ComponentGet(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Component) :: component
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the component.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the component input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_ComponentGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Components
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ComponentCheckpoint(component, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Component):: component 
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
        end subroutine ESMF_ComponentCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ComponentRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Component) :: ESMF_ComponentRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! component name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Component from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Component) :: a 

!       this is just to shut the compiler up
        type (ESMF_Component), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_ComponentRestore = a 
 
        end function ESMF_ComponentRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ComponentWrite(component, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Component) :: component
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_ComponentWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ComponentRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Component) :: ESMF_ComponentRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! component name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Component) :: a

!       this is just to shut the compiler up
        type (ESMF_Component), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_ComponentRead = a 
 
        end function ESMF_ComponentRead


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_ComponentPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Component) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
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
           call c_ESMC_ComponentPrint(component%this, options, status) 
       else
           call c_ESMC_ComponentPrint(component%this, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Component print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_ComponentPrint


       end module ESMF_ComponentMod

