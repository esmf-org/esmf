! $Id: ESMF_Xform.F90,v 1.4 2003/02/03 21:45:50 nscollins Exp $
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
!     ESMF Xform module
      module ESMF_XformMod
!
!==============================================================================
!
! This file contains the Transform class definition and all Transform
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_XformMod - Object to encapsulate lists of transformations
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Transform} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      !use ESMF_ComponentMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!     ! ESMF_Xform
!
!     ! Xform data type returned to the caller.  

      type ESMF_Xform
      sequence
      private
        character(len=ESMF_MAXSTR) :: name
        type(ESMF_Pointer) :: funcptr
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Xform
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_XformCreate
      public ESMF_XformDestroy
 
      !public ESMF_XformInit
      !public ESMF_XformRun
      !public ESMF_XformFinalize
 
      public ESMF_XformCheckpoint
      public ESMF_XformRestore
 
      public ESMF_XformPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Xform.F90,v 1.4 2003/02/03 21:45:50 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_XformCreate -- Generic interface to create a Transform.

! !INTERFACE:
     interface ESMF_XformCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_XformCreateNew

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_XformCreate} functions.   
!
!  \begin{description}
!  \item[xxx]
!    Description of xxx.
!  \item[yyy]
!    Description of yyy.
!  \item[{[zzz]}]
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
! This section includes the Transform Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformCreateNew -- Create a new Xform specifying all options.

! !INTERFACE:
      function ESMF_XformCreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_Xform) :: ESMF_XformCreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Xform and set the decomposition characteristics.
!
!  The return value is a new Xform.
!    
!  The arguments are:
!  \begin{description}
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        !type (ESMF_XformType), target :: newxform 
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        !nullify(ESMF_XformCreateNew%xformp)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       !TODO : insert code here

!       set return values
        !ESMF_XformCreateNew%xformp => newxform
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_XformCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_XformDestroy - Destroy a Transform object
!
! !INTERFACE:
      subroutine ESMF_XformDestroy(xform, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform) :: xform
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Xform}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[xform]
!       Destroy contents of this {\tt Xform}.
!
!     \item[{[rc]}]
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

!       ! TODO: insert code here
        !nullify(xform%xformp)

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XformDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformSetData - Set information in a Transform
!
! !INTERFACE:
      subroutine ESMF_XformSetData(xform, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform) :: xform 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of XformCreate which creates an empty 
!      Xform and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_XformSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the xform.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformGetData - Get information from a Transform
!
! !INTERFACE:
      subroutine ESMF_XformGet(xform, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform) :: xform
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the xform.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the xform input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_XformGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Xforms
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformCheckpoint - Save Transform state
!
! !INTERFACE:
      subroutine ESMF_XformCheckpoint(xform, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform):: xform 
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
        end subroutine ESMF_XformCheckpoint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformRestore - Restore Transform state
!
! !INTERFACE:
      function ESMF_XformRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Xform) :: ESMF_XformRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! xform name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Xform from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Xform) :: a 

!       this is just to shut the compiler up
        !type (ESMF_XformType), target :: b 
        !a%xformp => b
        !nullify(a%xformp)

!
! TODO: add code here
!

        ESMF_XformRestore = a 
 
        end function ESMF_XformRestore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformPrint - Print information about a Transform object
!
! !INTERFACE:
      subroutine ESMF_XformPrint(xform, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Xform) :: xform
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about an xform.
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

!      ! TODO: Add Print code  here
       if(present(options)) then
           ! decode options - long, short, whatever
           print *, "Transform object" !!, xform%xformp%xformcount
 	   status = ESMF_SUCCESS
       else
           print *, "Transform object" !!, xform%xformp%xformcount
 	   status = ESMF_SUCCESS
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Xform print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_XformPrint


       end module ESMF_XformMod

