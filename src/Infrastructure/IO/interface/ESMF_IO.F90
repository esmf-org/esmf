! $Id: ESMF_IO.F90,v 1.2 2002/10/16 20:37:50 nscollins Exp $
!-------------------------------------------------------------------------
!
! ESMF IO module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the IO defined type
!  and functions which operate on it.  This is an
!  interface to the actual IO class object in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------

! module definition

      module ESMF_IOMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_IOmod
!
! !USES:
      use ESMF_BaseMod

! !PUBLIC TYPES:
      implicit none

      integer, parameter :: UNKNOWN=0, NETCDF=1

      type ESMF_IOType
      sequence
      private
         integer :: iotype
      end type

      type ESMF_IOSpec
      sequence
      private
          type (ESMF_Status) :: iostatus
          type (ESMF_IOType) :: iotype
          integer :: current_status
          type (ESMF_Flag) :: async_io
      end type

      private ESMF_IOSpecCreateNew, ESMF_IOSpecCreateCopy

! !DESCRIPTION:
!     The following routines apply to general I/O characteristics.

! !PUBLIC MEMBER FUNCTIONS:
!
!     function ESMF_IOSpecCreate() (interface only)
!     function ESMF_IOSpecCreateNew()
!     function ESMF_IOSpecCreateCopy()
! or
!     function ESMF_IOSpecInit()
!
!     subroutine ESMF_IOSpecConstruct()
!     subroutine ESMF_IOSpecDestruct()
!
!     subroutine ESMF_IOSpecSet()
!     subroutine ESMF_IOSpecGet()
! 
!     subroutine ESMF_IOSpecSetName()  
!     subroutine ESMF_IOSpecGetName()
!     subroutine ESMF_IOSpecSetDestName()
!     subroutine ESMF_IOSpecGetDestName()
!     subroutine ESMF_IOSpecSetSrcName()
!     subroutine ESMF_IOSpecGetSrcName()
!
!     subroutine ESMF_IOSpecSetType()  
!     subroutine ESMF_IOSpecGetType()
!     subroutine ESMF_IOSpecSetDestType()
!     subroutine ESMF_IOSpecGetDestType()
!     subroutine ESMF_IOSpecSetSrcType()
!     subroutine ESMF_IOSpecGetSrcType()
!
!     subroutine ESMF_IOSpecSetOptions()  
!     subroutine ESMF_IOSpecGetOptions()
!     subroutine ESMF_IOSpecSetDestOptions()
!     subroutine ESMF_IOSpecGetDestOptions()
!     subroutine ESMF_IOSpecSetSrcOptions()
!     subroutine ESMF_IOSpecGetSrcOptions()
!
!     subroutine ESMF_IOSpecPrint()
!     subroutine ESMF_IOSpecValidate()
!
!     subroutine ESMF_IOSpecxxx
!

!EOP

!BOP
!  !INTERFACE:
      interface ESMF_IOSpecCreate
!
      end interface

! add interface block for Create here

!-------------------------------------------------------------------------

      contains

!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      function ESMF_IOSpecCreateNew(rc)
!
! !RETURN VALUE:
      type (ESMF_IOSpec), pointer :: ESMF_IOSpecCreateNew
!
! !PARAMETERS:
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:

!
! !REQUIREMENTS: 

!EOP
      type (ESMF_IOSpec), pointer :: ios

      ALLOCATE(ios)

      ESMF_IOSpecCreateNew = ios

      end function ESMF_IOSpecCreateNew


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      function ESMF_IOSpecCreateCopy(iospec, rc)
!
! !RETURN VALUE:
      type (ESMF_IOSpec) :: ESMF_IOSpecCreateCopy
!
! !PARAMETERS:
      type (ESMF_IOSpec), intent(in) :: iospec           ! thing to copy
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:

!
! !REQUIREMENTS: 

!EOP
      type (ESMF_IOSpec) ios !! = ESMF_Allocate(47)    ! how does this work?

      ESMF_IOSpecCreateCopy = ios

      end function ESMF_IOSpecCreateCopy




      end module ESMF_IOMod

