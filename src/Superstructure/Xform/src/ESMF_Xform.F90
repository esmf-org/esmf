! $Id: ESMF_Xform.F90,v 1.4 2004/04/29 17:30:03 nscollins Exp $
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
      use ESMF_IOSpecMod
      !use ESMF_CompMod
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
        type(ESMF_Pointer) :: subrptr
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Xform
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_XformInit
      public ESMF_XformGet, ESMF_XformSet
 
      public ESMF_XformWriteRestart
      public ESMF_XformReadRestart
 
      public ESMF_XformPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Xform.F90,v 1.4 2004/04/29 17:30:03 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================



!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Transform Init method
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformInit -- Fill in an Xform's data

! !INTERFACE:
      subroutine ESMF_XformInit(xform, name, subr, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform), intent(inout) :: xform
      character(len=*), intent(in) :: name
      integer, intent(in) :: subr 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Fill in the values for an Xform.
!
!      The arguments are:
!      \begin{description}
!       \item[xform]
!        The {\tt Transform} object to be initialized.
!       \item[name]
!     The name of the {\tt Transform}.
!       \item[subr]
!         The subroutine to be called when executing this {\tt Transform}.
!         It must be declared as returning an integer return code value.
!       \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

!       Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       !TODO : figure out how to coerce an integer into a pointer
        xform%name = name
        !xform%subrptr = subr

!       set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_XformInit



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformSet - Set information in a Transform
!
! !INTERFACE:
      subroutine ESMF_XformSet(xform, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform) :: xform 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Update or overwrite data inside a {\tt Transform}.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_XformSet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the xform.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformGet - Get information from a Transform
!
! !INTERFACE:
      subroutine ESMF_XformGet(xform, name, subr, rc)
!
! !ARGUMENTS:
      type(ESMF_Xform), intent(in) :: xform
      character(len=*), intent(out), optional :: name 
      integer, intent(out), optional :: subr
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the xform.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the xform input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

        name = xform%name
        if (present(subr)) subr = 0
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_XformGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Xforms
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformWriteRestart - Save Transform state
!
! !INTERFACE:
      subroutine ESMF_XformWriteRestart(xform, iospec, rc)
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
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_XformWriteRestart


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_XformReadRestart - ReadRestart Transform state
!
! !INTERFACE:
      function ESMF_XformReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Xform) :: ESMF_XformReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Xform from the last call to WriteRestart.
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

        ESMF_XformReadRestart = a 
        if (present(rc)) rc = ESMF_FAILURE
 
        end function ESMF_XformReadRestart


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
       character (len=6) :: defaultopts
       integer :: status                   ! local error status
       logical :: rcpresent                ! did user specify rc?

!      Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

!      ! TODO: Add Print code  here
       if(present(options)) then
           ! decode options - long, short, whatever
           print *, "Transform object", trim(xform%name)
 	   status = ESMF_SUCCESS
       else
           print *, "Transform object", trim(xform%name)
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

