! $Id: inter_ESMF_Class.F90,v 1.3 2003/01/10 15:15:01 nscollins Exp $
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
!     ESMF <Class> module
      module ESMF_<Class>Mod
!
!==============================================================================
!
! This file contains the <Class> class definition and all <Class>
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_<Class>Mod - Manage data <class>s uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt <Class>} class and associated functions and subroutines.  
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
!     ! ESMF_<Class>
!
!     ! <Class> data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_<Class>
      sequence
      private
        type(ESMF_Pointer) :: this    ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_<Class>
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_<Class>Create
      public ESMF_<Class>Destroy
 
      !public ESMF_<Class>SetData
      !public ESMF_<Class>GetData
      !public ESMF_<Class>Get
 
      public ESMF_<Class>Checkpoint
      public ESMF_<Class>Restore
      public ESMF_<Class>Write
      public ESMF_<Class>Read
 
      public ESMF_<Class>Print
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: inter_ESMF_Class.F90,v 1.3 2003/01/10 15:15:01 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_<Class>Create -- Generic interface to create an <Class>

! !INTERFACE:
     interface ESMF_<Class>Create

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_<Class>CreateNew
!        !module procedure ESMF_<Class>CreateNoData

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_<Class>Create} functions.   
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
! This section includes the <Class> Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>CreateNew -- Create a new <Class> specifying all options.

! !INTERFACE:
      function ESMF_<Class>CreateNew(rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>CreateNew
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new <Class> and set the decomposition characteristics.
!
!  The return value is a new <Class>.
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
        type (ESMF_<Class>) :: class        ! new thing being created
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       ! Initialize the contents to Null in case of failure
        class%this = ESMF_NULL_POINTER

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! Routine which interfaces to the C++ creation routine.
        call c_ESMC_<Class>Create(class, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "<Class> construction error"
          return
        endif

!       ! Set return values
        ESMF_<Class>CreateNew = class 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_<Class>CreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>CreateNoData

! !INTERFACE:
      function ESMF_<Class>CreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>CreateNoData
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new empty {\tt <Class>} object.
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
        type (ESMF_<Class>) :: class        ! new class being created
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       ! Initialize the contents to Null in case of failure
        class%this = ESMF_NULL_POINTER

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! C routine which interfaces to the C++ routine which does actual work
        call c_ESMC_<Class>CreateNoData(class, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "<Class> construction error"
          return
        endif

!       ! Set return values
        ESMF_<Class>CreateNoData = class
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_<Class>CreateNoData


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Destroy(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>) :: <class>
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt <Class>}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[<class>]
!       Destroy contents of this {\tt <Class>}.
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

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! Call Destroy to release resources on the C++ side
        call c_ESMC_<Class>Destroy(<class>, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "<Class> contents destruction error"
          return
        endif

!       ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_<Class>Destroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>SetData(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>) :: <class> 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of <Class>Create which creates an empty 
!      <Class> and allows the Data to be specified later. 
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_<Class>SetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the <class>.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Get(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>) :: <class>
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the <class>.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the <class> input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_<Class>Get

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for <Class>s
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Checkpoint(<class>, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>):: <class> 
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
        end subroutine ESMF_<Class>Checkpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_<Class>Restore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>Restore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! <class> name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a <Class> from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_<Class>) :: a 

!       ! add code here

        ESMF_<Class>Restore = a 
 
        end function ESMF_<Class>Restore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Write(<class>, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>) :: <class>
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
        end subroutine ESMF_<Class>Write


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_<Class>Read(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>Read
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! <class> name to read
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
        type (ESMF_<Class>) :: a

!
! TODO: add code here
!

        ESMF_<Class>Read = a 
 
        end function ESMF_<Class>Read


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_<Class>Print(<class>, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_<Class>) :: <class>
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a <class>.
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
           call c_ESMC_<Class>Print(<class>, options, status) 
       else
           call c_ESMC_<Class>Print(<class>, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "<Class> print error"
         return
       endif

!      ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_<Class>Print


       end module ESMF_<Class>Mod

