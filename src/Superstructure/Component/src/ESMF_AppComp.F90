! $Id: ESMF_AppComp.F90,v 1.5 2003/09/09 21:08:12 nscollins Exp $
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
!     ESMF Application Component module
      module ESMF_AppCompMod
!
!==============================================================================
!
! This file contains the Application Component class definition and 
!  all Application Component class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_AppCompMod - Application Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Application Component} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_MachineMod
      use ESMF_ConfigMod
      use ESMF_DELayoutMod
      use ESMF_ClockMod
      use ESMF_GridMod
      use ESMF_StateMod
      use ESMF_CompMod
      implicit none


!------------------------------------------------------------------------------
!     ! ESMF_AppComp
!
!     ! Application Component wrapper

      type ESMF_AppComp
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_CompClass), pointer :: compp => NULL()
#else
         type(ESMF_CompClass), pointer :: compp
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_AppComp

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_AppCompCreate
      public ESMF_AppCompDestroy

      public ESMF_AppCompGet
      public ESMF_AppCompSet
 
      public ESMF_AppCompValidate
      public ESMF_AppCompPrint
 
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_AppComp.F90,v 1.5 2003/09/09 21:08:12 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes Component Create/Destroy, Construct/Destruct methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompCreate -- Create a new Component.

! !INTERFACE:
      function ESMF_AppCompCreate(name, layout, dirpath, configfile, rc)
!
! !RETURN VALUE:
      type(ESMF_AppComp) :: ESMF_AppCompCreate
!
! !ARGUMENTS:
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      character(len=*), intent(in), optional :: dirpath
      character(len=*), intent(in), optional :: configfile
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
!   \item[{[name]}]
!    Component name.
!
!   \item[{[layout]}]
!    Component layout.
!
!   \item[{[dirpath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!
!   \item[{[configfile]}]
!    File containing configuration information, either absolute filename
!    or relative to {\tt dirpath}.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic component
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Initialize the pointer to null.
        nullify(ESMF_AppCompCreate%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Allocate a new comp class
        allocate(compclass, stat=status)
        if(status .NE. 0) then
          print *, "ERROR in ESMF_AppComponentCreate: Allocate"
          return
        endif

        ! Call construction method to initialize component internals
        call ESMF_CompConstruct(compclass, ESMF_APPCOMPTYPE, name, layout, &
                             dirpath=dirpath, configfile=configfile, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif
  

        ! Set return values
        ESMF_AppCompCreate%compp => compclass
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_AppCompCreate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompGet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_AppCompGet(component, name, layout, &
                                           dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp), intent(in) :: component
      character(len=*), intent(out), optional :: name
      type(ESMF_DELayout), intent(out), optional :: layout
      character(len=*), intent(out), optional :: dirpath
      character(len=*), intent(out), optional :: configfile
      type(ESMF_Config), intent(out), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the component.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompGet(component%compp, name, layout, &
                          dirpath=dirpath, configfile=configfile, &
                          config=config, rc=rc)

        end subroutine ESMF_AppCompGet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompSet -- Query a component for various information
!
! !INTERFACE:
      subroutine ESMF_AppCompSet(component, name, layout, &
                                           dirpath, configfile, config, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp), intent(inout) :: component
      character(len=*), intent(in), optional :: name
      type(ESMF_DELayout), intent(in), optional :: layout
      character(len=*), intent(in), optional :: dirpath
      character(len=*), intent(in), optional :: configfile
      type(ESMF_Config), intent(in), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets or resets information about the component.  When the caller
!      only wants to set a single value specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOP
! !REQUIREMENTS:

        call ESMF_CompSet(component%compp, name, layout, &
                          dirpath=dirpath, configfile=configfile, &
                          config=config, rc=rc)

        end subroutine ESMF_AppCompSet


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
      subroutine ESMF_AppCompValidate(component, options, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOP
! !REQUIREMENTS:

       call ESMF_CompValidate(component%compp, options, rc)
 
       end subroutine ESMF_AppCompValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AppCompPrint -- Print the contents of a Component
!
! !INTERFACE:
      subroutine ESMF_AppCompPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_AppComp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a component.
!
!EOP
! !REQUIREMENTS:

       print *, "Application Component:"
       call ESMF_CompPrint(component%compp, options, rc)

       end subroutine ESMF_AppCompPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AppCompDestroy -- Release resources for a Component

! !INTERFACE:
      subroutine ESMF_AppCompDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_AppComp) :: component
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
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
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

        ! Check to see if already destroyed
        if (.not.associated(component%compp)) then  
          print *, "Component already destroyed"
          return
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(component%compp, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component contents destruction error"
          return
        endif

        ! Deallocate the component struct itself
        deallocate(component%compp, stat=status)
        if (status .ne. 0) then
          print *, "Component contents destruction error"
          return
        endif
        nullify(component%compp)
 
        ! When destroying App component, finalize framework
        call ESMF_FrameworkFinalize(status)

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_AppCompDestroy

end module ESMF_AppCompMod

