! $Id: ESMF_Comp.F90,v 1.3 2003/01/30 23:42:38 nscollins Exp $
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
      module ESMF_CompMod
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
! !MODULE: ESMF_CompMod - Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Component} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_LayoutMod
      use ESMF_StateMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_CompType
!
!     ! Component type: Application, Gridded Component, or Coupler
!
      type ESMF_CompType
      sequence
      private
        integer :: ctype
      end type

      type(ESMF_CompType), parameter :: &
                  ESMF_APPCOMP = ESMF_CompType(1), &
                  ESMF_GRIDCOMP = ESMF_CompType(2), &
                  ESMF_CPLCOMP = ESMF_CompType(3)

!------------------------------------------------------------------------------
!     ! ESMF_ModelType
!
!     ! Model type: Atmosphere, Land, Ocean, SeaIce, River runoff.
!
      type ESMF_ModelType
      sequence
      private
        integer :: mtype
      end type

      type(ESMF_ModelType), parameter :: &
                  ESMF_ATM = ESMF_ModelType(1), &
                  ESMF_LAND = ESMF_ModelType(2), &
                  ESMF_OCEAN = ESMF_ModelType(3), &
                  ESMF_SEAICE = ESMF_ModelType(4), &
                  ESMF_RIVER = ESMF_ModelType(5)

!------------------------------------------------------------------------------
!     ! ESMF_CompClass
!
!     ! Component class data. 

      type ESMF_CompClass
      sequence
      private
         type(ESMF_State) :: importstate
         type(ESMF_State) :: exportstate
         type(ESMF_State), dimension(:), pointer :: statelist
         integer :: instance_id
         ! these will be allocatable arrays of variable len
         integer :: function_count
         character(len=ESMF_MAXSTR), dimension(5) :: function_name
         type(ESMF_Pointer), dimension(5) :: function_list
      end type

!------------------------------------------------------------------------------
!     ! ESMF_Comp
!
!     ! Component wrapper

      type ESMF_Comp
      sequence
      private
         type(ESMF_CompClass), pointer :: compp
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Comp
      public ESMF_CompType, ESMF_APPCOMP, ESMF_GRIDCOMP, ESMF_CPLCOMP
      public ESMF_ModelType, ESMF_ATM, ESMF_LAND, ESMF_OCEAN, &
                             ESMF_SEAICE, ESMF_RIVER
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_CompCreate
      public ESMF_CompDestroy
 
      public ESMF_CompInit  !  (comptype, modeltype, ...)
      !public ESMF_CompSetRoutine  ! (component, "init", My_Init)
      public ESMF_CompRun   ! (component, time) or (coupler, statelist?, time)
      public ESMF_CompFinalize   ! (component)
      !public ESMF_CompGetState  ! (component, "import"/"export"/"list", state)
      !public ESMF_CompSetState  ! (component, "import"/"export"/"list", state)
      !public ESMF_CompQueryState 
 
      !public ESMF_CompCheckpoint
      !public ESMF_CompRestore
      !public ESMF_CompWrite
      !public ESMF_CompRead
 
      public ESMF_CompValidate
      public ESMF_CompPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comp.F90,v 1.3 2003/01/30 23:42:38 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_CompCreate -- Generic interface to create an Component

! !INTERFACE:
     interface ESMF_CompCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_CompCreateNew
!        !module procedure ESMF_CompCreateNoData

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
! !IROUTINE: ESMF_CompCreateNew -- Create a new Component specifying all options.

! !INTERFACE:
      function ESMF_CompCreateNew(name, layout, ctype, mtype, filepath, rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompCreateNew
!
! !ARGUMENTS:
      character(len=*), intent(in) :: name
      type(ESMF_Layout), intent(in) :: layout
      type(ESMF_CompType), intent(in) :: ctype
      type(ESMF_ModelType), intent(in) :: mtype 
      character(len=*), intent(in), optional :: filepath
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
!   \item[name]
!    Component name.
!
!   \item[layout]
!    Component layout.
!
!   \item[ctype]
!    Component type, where valid types include ESMF\_APPCOMP, ESMF\_GRIDCOMP, 
!    and ESMF\_CPLCOMP for Applications, Gridded Components, and Couplers,
!    respectively.
!
!   \item[mtype]
!    Component Model Type, where model includes ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER.  
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_CompClass) :: comptype       ! the new Component
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        nullify(ESMF_CompCreateNew%compp)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        !call ESMF_CompConstruct(comp, name, layout, ctype, mtype, &
        !                            filepath, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component construction error"
          return
        endif

!       set return values
        ESMF_CompCreateNew%compp = comptype
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CompCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompCreateNoData

! !INTERFACE:
      function ESMF_CompCreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompCreateNoData
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
        type (ESMF_CompClass) :: comptype       ! the new Component
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       ! Initialize pointer
        nullify(ESMF_CompCreateNoData%compp)

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! construct routine
        !call ESMF_CompConstructNoData(comp, arglist, status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Component construction error"
        !  return
        !endif

!       set return values
        ESMF_CompCreateNoData%compp = comptype
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CompCreateNoData


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CompDestroy(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
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
        call c_ESMC_CompDestroy(component, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Component Init, Run, and Finalize methods
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompInit -- Call the Component's init routine

! !INTERFACE:
      subroutine ESMF_CompInit(component, rc)
!
!
! !ARGUMENTS:
      type (ESMF_Comp) :: component
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to call Initialization routine for.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        call c_ESMC_CompInit(component, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component initialization error"
          return
        endif

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompInit


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompRun -- Call the Component's run routine

! !INTERFACE:
      subroutine ESMF_CompRun(component, timesteps, rc)
!
!
! !ARGUMENTS:
      type (ESMF_Comp) :: component 
      integer, intent(in) :: timesteps
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component for which to call Run routine.
!
!   \item[timesteps]
!    How long the Run interval is.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Runialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        call c_ESMC_CompRun(component, timesteps, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component run error"
          return
        endif

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompRun


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CompFinalize -- Call the Component's finalization routine

! !INTERFACE:
      subroutine ESMF_CompFinalize(component, rc)
!
!
! !ARGUMENTS:
      type (ESMF_Comp) :: component 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalization code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[component]
!    Component to call finalization routine for.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Finalize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        call c_ESMC_CompFinalize(component, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Component finalization error"
          return
        endif

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompFinalize


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CompSetData(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component 
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
        end subroutine ESMF_CompSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the component.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CompGet(component, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
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
        end subroutine ESMF_CompGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Components
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CompCheckpoint(component, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp):: component 
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
        end subroutine ESMF_CompCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_CompRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompRestore
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
        type (ESMF_Comp) :: a 

!       this is just to shut the compiler up
        nullify(a%compp)
!
! TODO: add code here
!

        ESMF_CompRestore = a 
 
        end function ESMF_CompRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_CompWrite(component, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
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
        end subroutine ESMF_CompWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_CompRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Comp) :: ESMF_CompRead
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
        type (ESMF_Comp) :: a

!       this is just to shut the compiler up
        nullify(a%compp)

!
! TODO: add code here
!

        ESMF_CompRead = a 
 
        end function ESMF_CompRead


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_CompValidate(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
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

!      ! Interface to call the C++ validate code
       if(present(options)) then
           call c_ESMC_CompValidate(component, options, status) 
       else
           call c_ESMC_CompValidate(component, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Component validate error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompValidate

!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_CompPrint(component, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Comp) :: component
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
           call c_ESMC_CompPrint(component, options, status) 
       else
           call c_ESMC_CompPrint(component, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Component print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompPrint


       end module ESMF_CompMod

