! $Id: ESMF_App.F90,v 1.1 2003/01/29 00:00:16 nscollins Exp $
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
!     ESMF App module
      module ESMF_AppMod
!
!==============================================================================
!
! This file contains the App class definition and all App
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_AppMod - Manage data apps uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Application Component} class and associated functions and subroutines.  
!
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_CompMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_App
!
!     ! App data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_App
      sequence
      private
        type(ESMF_App), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_App
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_AppCreate
      public ESMF_AppDestroy
 
      !public ESMF_AppInit
      !public ESMF_AppRun
      !public ESMF_AppFinalize
 
      public ESMF_AppCheckpoint
      public ESMF_AppRestore
 
      public ESMF_AppPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_App.F90,v 1.1 2003/01/29 00:00:16 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_AppCreate -- Generic interface to create an Application

! !INTERFACE:
     interface ESMF_AppCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_AppCreateNew
        !module procedure ESMF_AppCreateSPMD
        !module procedure ESMF_AppCreateMPMD

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_AppCreate} functions.   
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
! This section includes the Application Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCreateNew -- Create a new App specifying all options.

! !INTERFACE:
      function ESMF_AppCreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_App) :: ESMF_AppCreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new App and set the decomposition characteristics.
!
!  The return value is a new App.
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
        type (ESMF_App), pointer :: ptr   ! opaque pointer to new C++ App
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
        call c_ESMC_AppCreate(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "App construction error"
          return
        endif

!       set return values
        ESMF_AppCreateNew%this => ptr 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_AppCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCreateSPMD

! !INTERFACE:
      function ESMF_AppCreateSPMD(rc)
!
! !RETURN VALUE:
      type(ESMF_App) :: ESMF_AppCreateSPMD
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt App} object.
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
        type (ESMF_App), pointer :: a    ! pointer to new App
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
        !call c_ESMC_AppCreateSPMD(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "App construction error"
        !  return
        !endif

!       set return values
        ESMF_AppCreateSPMD%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_AppCreateSPMD


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCreateMPMD

! !INTERFACE:
      function ESMF_AppCreateMPMD(rc)
!
! !RETURN VALUE:
      type(ESMF_App) :: ESMF_AppCreateMPMD
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt App} object.
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
        type (ESMF_App), pointer :: a    ! pointer to new App
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
        !call c_ESMC_AppCreateMPMD(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "App construction error"
        !  return
        !endif

!       set return values
        ESMF_AppCreateMPMD%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_AppCreateMPMD


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_AppDestroy(app, rc)
!
! !ARGUMENTS:
      type(ESMF_App) :: app
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt App}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[app]
!       Destroy contents of this {\tt App}.
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
        call c_ESMC_AppDestroy(app%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "App contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_AppDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_AppSetData(app, rc)
!
! !ARGUMENTS:
      type(ESMF_App) :: app 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of AppCreate which creates an empty 
!      App and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_AppSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the app.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_AppGet(app, rc)
!
! !ARGUMENTS:
      type(ESMF_App) :: app
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the app.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the app input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_AppGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Apps
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_AppCheckpoint(app, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_App):: app 
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
        end subroutine ESMF_AppCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_AppRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_App) :: ESMF_AppRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! app name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a App from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_App) :: a 

!       this is just to shut the compiler up
        type (ESMF_App), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_AppRestore = a 
 
        end function ESMF_AppRestore


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AppPrint(app, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_App) :: app
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about an application.
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
           call c_ESMC_AppPrint(app%this, options, status) 
       else
           call c_ESMC_AppPrint(app%this, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "App print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_AppPrint


       end module ESMF_AppMod

