! $Id: ESMF_GComp.F90,v 1.1 2003/01/07 21:38:21 nscollins Exp $
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
!     ESMF GComp module
      module ESMF_GCompMod
!
!==============================================================================
!
! This file contains the Gridded Component class definition and all GComp
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_GCompMod - Manage data gridded comps uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt GComp} class and associated functions and subroutines.  
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
!     ! ESMF_GComp
!
!     ! GComp data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_GComp
      sequence
      private
        type(ESMF_GComp), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_GComp
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_GCompCreate
      public ESMF_GCompDestroy
 
      !public ESMF_GCompInit
      !public ESMF_GCompRun
      !public ESMF_GCompFinalize
 
      public ESMF_GCompCheckpoint
      public ESMF_GCompRestore
      public ESMF_GCompWrite
      public ESMF_GCompRead
 
      public ESMF_GCompPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GComp.F90,v 1.1 2003/01/07 21:38:21 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_GCompCreate -- Generic interface to create an GComp

! !INTERFACE:
     interface ESMF_GCompCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_GCompCreateNew
!        !module procedure ESMF_GCompCreateNoData

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_GCompCreate} functions.   
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
! This section includes the GComp Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GCompCreateNew -- Create a new GComp specifying all options.

! !INTERFACE:
      function ESMF_GCompCreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_GComp) :: ESMF_GCompCreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new GComp and set the decomposition characteristics.
!
!  The return value is a new GComp.
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
        type (ESMF_GComp), pointer :: ptr   ! opaque pointer to new C++ GComp
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
        call c_ESMC_GCompCreate(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "GComp construction error"
          return
        endif

!       set return values
        ESMF_GCompCreateNew%this => ptr 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GCompCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GCompCreateNoData

! !INTERFACE:
      function ESMF_GCompCreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_GComp) :: ESMF_GCompCreateNoData
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt GComp} object.
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
        type (ESMF_GComp), pointer :: a    ! pointer to new GComp
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
        !call c_ESMC_GCompCreateNoData(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "GComp construction error"
        !  return
        !endif

!       set return values
        ESMF_GCompCreateNoData%this => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_GCompCreateNoData


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GCompDestroy(gcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_GComp) :: gcomp
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt GComp}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[gcomp]
!       Destroy contents of this {\tt GComp}.
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
        call c_ESMC_GCompDestroy(gcomp%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "GComp contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_GCompDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GCompSetData(gcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_GComp) :: gcomp 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of GCompCreate which creates an empty 
!      GComp and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_GCompSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the gcomp.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GCompGet(gcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_GComp) :: gcomp
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the gcomp.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the gcomp input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_GCompGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for GComps
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GCompCheckpoint(gcomp, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_GComp):: gcomp 
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
        end subroutine ESMF_GCompCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_GCompRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_GComp) :: ESMF_GCompRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! gcomp name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a GComp from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_GComp) :: a 

!       this is just to shut the compiler up
        type (ESMF_GComp), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_GCompRestore = a 
 
        end function ESMF_GCompRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GCompWrite(gcomp, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_GComp) :: gcomp
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
        end subroutine ESMF_GCompWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_GCompRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_GComp) :: ESMF_GCompRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! gcomp name to read
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
        type (ESMF_GComp) :: a

!       this is just to shut the compiler up
        type (ESMF_GComp), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_GCompRead = a 
 
        end function ESMF_GCompRead


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_GCompPrint(gcomp, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_GComp) :: gcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a gcomp.
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
           call c_ESMC_GCompPrint(gcomp%this, options, status) 
       else
           call c_ESMC_GCompPrint(gcomp%this, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "GComp print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_GCompPrint


       end module ESMF_GCompMod

